"""
TuoKit RAG Manager - Main orchestrator
Combines all components for unified knowledge management
"""
import os
from pathlib import Path
from typing import List, Dict, Optional
import logging
import yaml
from concurrent.futures import ThreadPoolExecutor
import hashlib
import psycopg2

# Add Ollama integration
import sys
sys.path.append(str(Path(__file__).parent.parent.parent))
from utils.ollama import safe_ollama_generate
from utils.model_manager import ModelManager

from .postgres_store import PostgresVectorStore
from .document_processor import DocumentProcessor
from .web_scraper import KnowledgeBaseScraper

logger = logging.getLogger(__name__)

class TuoKitRAGManager:
    """Main RAG system orchestrator"""
    
    def __init__(self, config_path: str = None):
        self.config = self._load_config(config_path)
        
        # Initialize components
        self.vector_store = PostgresVectorStore(
            self.config.get('postgres_connection')
        )
        self.doc_processor = DocumentProcessor(
            self.config.get('embedding_model', 'all-MiniLM-L6-v2')
        )
        self.web_scraper = KnowledgeBaseScraper(
            self.config.get('scraper_output_dir', 'C:/Projects/TuoKit/scraped_kb')
        )
        
        # Track indexed files
        self.indexed_files = self._load_index_state()
    
    def _load_config(self, config_path: str) -> Dict:
        """Load or create default configuration"""
        if config_path and Path(config_path).exists():
            with open(config_path, 'r') as f:
                return yaml.safe_load(f)
        
        # Default configuration
        return {
            'postgres_connection': os.getenv('TUOKIT_DB_URL', 
                'postgresql://localhost:5432/tuokit'),
            'embedding_model': 'all-MiniLM-L6-v2',
            'chunk_size': 1000,
            'chunk_overlap': 200,
            'scraper_output_dir': 'C:/Projects/TuoKit/scraped_kb',
            'index_state_file': 'C:/Projects/TuoKit/.rag_index_state.json'
        }
    
    def _load_index_state(self) -> Dict:
        """Load index state for incremental updates"""
        state_file = Path(self.config['index_state_file'])
        if state_file.exists():
            import json
            with open(state_file, 'r') as f:
                return json.load(f)
        return {'indexed_files': {}, 'indexed_urls': []}
    
    def _save_index_state(self):
        """Save index state"""
        import json
        state_file = Path(self.config['index_state_file'])
        state_file.parent.mkdir(exist_ok=True)
        with open(state_file, 'w') as f:
            json.dump(self.indexed_files, f, indent=2)
    
    def index_directory(self, directory: str, 
                       pattern: str = "**/*",
                       force_reindex: bool = False) -> Dict:
        """Index all files in directory"""
        logger.info(f"Indexing directory: {directory}")
        
        path = Path(directory)
        if not path.exists():
            raise ValueError(f"Directory not found: {directory}")
        
        files = list(path.glob(pattern))
        
        # Filter already indexed files
        if not force_reindex:
            files = [f for f in files if self._should_index_file(f)]
        
        results = {
            'total_files': len(files),
            'processed': 0,
            'chunks_created': 0,
            'errors': []
        }
        
        # Process files in parallel
        with ThreadPoolExecutor(max_workers=4) as executor:
            futures = []
            for file_path in files:
                future = executor.submit(self._index_file, file_path)
                futures.append((file_path, future))
            
            for file_path, future in futures:
                try:
                    chunks_added = future.result()
                    results['processed'] += 1
                    results['chunks_created'] += chunks_added
                except Exception as e:
                    logger.error(f"Error indexing {file_path}: {e}")
                    results['errors'].append(str(file_path))
        
        self._save_index_state()
        return results
    
    def _should_index_file(self, file_path: Path) -> bool:
        """Check if file needs indexing"""
        str_path = str(file_path)
        
        # Get file hash
        try:
            with open(file_path, 'rb') as f:
                file_hash = hashlib.sha256(f.read()).hexdigest()
        except:
            return False
        
        # Check if already indexed with same hash
        if str_path in self.indexed_files.get('indexed_files', {}):
            if self.indexed_files['indexed_files'][str_path] == file_hash:
                return False
        
        return True
    
    def _index_file(self, file_path: Path) -> int:
        """Index a single file"""
        chunks = self.doc_processor.process_file(str(file_path))
        
        if chunks:
            self.vector_store.add_chunks(chunks)
            
            # Update index state
            with open(file_path, 'rb') as f:
                file_hash = hashlib.sha256(f.read()).hexdigest()
            
            if 'indexed_files' not in self.indexed_files:
                self.indexed_files['indexed_files'] = {}
            
            self.indexed_files['indexed_files'][str(file_path)] = file_hash
        
        return len(chunks)
    
    def index_web_docs(self, url: str, max_pages: int = 50) -> Dict:
        """Index online documentation"""
        logger.info(f"Indexing web docs from: {url}")
        
        # Scrape pages
        scraped_files = self.web_scraper.scrape_site(url, max_pages)
        
        # Index scraped files
        results = {
            'pages_scraped': len(scraped_files),
            'chunks_created': 0
        }
        
        for file_path in scraped_files:
            chunks = self.doc_processor.process_file(file_path)
            if chunks:
                self.vector_store.add_chunks(chunks)
                results['chunks_created'] += len(chunks)
        
        # Update index state
        if 'indexed_urls' not in self.indexed_files:
            self.indexed_files['indexed_urls'] = []
        
        self.indexed_files['indexed_urls'].append({
            'url': url,
            'pages': len(scraped_files),
            'timestamp': str(Path.ctime(Path(scraped_files[0])))
        })
        
        self._save_index_state()
        return results
    
    
    def generate_answer(self, query: str, chunks: List[Dict], 
                       max_context_length: int = 4000) -> Dict:
        """Generate answer using Ollama and retrieved chunks"""
        if not chunks:
            return {
                'answer': 'No relevant information found.',
                'chunks_used': 0
            }
        
        # Build context from top chunks
        context_parts = []
        total_length = 0
        chunks_used = 0
        
        for chunk in chunks:
            chunk_text = f"[Source: {chunk.get('source_path', 'Unknown')}]\n{chunk['content']}\n"
            chunk_length = len(chunk_text)
            
            if total_length + chunk_length > max_context_length:
                break
                
            context_parts.append(chunk_text)
            total_length += chunk_length
            chunks_used += 1
        
        context = "\n---\n".join(context_parts)
        
        # Generate answer
        prompt = f"""Based on the following context from the knowledge base, answer the question.
Only use information from the provided context. If the context doesn't contain relevant information, say so.

Context:
{context}

Question: {query}

Answer:"""
        
        try:
            response = safe_ollama_generate(
                model=ModelManager.get_default_model(),
                prompt=prompt,
                system="You are a helpful assistant. Answer questions based only on the provided context. Be concise and accurate."
            )
            
            return {
                'answer': response.get('response', 'Unable to generate answer'),
                'chunks_used': chunks_used,
                'error': response.get('error')
            }
        except Exception as e:
            logger.error(f"Error generating answer: {e}")
            return {
                'answer': 'Error generating answer',
                'chunks_used': chunks_used,
                'error': str(e)
            }
    
    def search(self, query: str, 
              top_k: int = 10,
              source_filter: Optional[str] = None,
              use_llm: bool = True) -> List[Dict]:
        """Search knowledge base"""
        # Create query embedding
        query_embedding = self.doc_processor.embedder.encode(query)
        
        # Search vector store
        results = self.vector_store.search(
            query_embedding,
            top_k=top_k,
            source_filter=source_filter
        )
        
        if use_llm and results:
            # Generate answer using Ollama
            answer_data = self.generate_answer(query, results)
            
            # Add answer to results
            for result in results:
                result['generated_answer'] = answer_data.get('answer')
                result['answer_metadata'] = {
                    'chunks_used': answer_data.get('chunks_used'),
                    'error': answer_data.get('error')
                }
        
        return results
    
    def get_stats(self) -> Dict:
        """Get system statistics"""
        stats = self.vector_store.get_stats()
        stats['indexed_files'] = len(self.indexed_files.get('indexed_files', {}))
        stats['indexed_urls'] = len(self.indexed_files.get('indexed_urls', []))
        return stats
    
    def clear_index(self, confirm: bool = False):
        """Clear all indexed data"""
        if not confirm:
            raise ValueError("Set confirm=True to clear index")
        
        # Clear vector store
        with psycopg2.connect(self.vector_store.conn_string) as conn:
            with conn.cursor() as cur:
                cur.execute("TRUNCATE TABLE knowledge_chunks")
                conn.commit()
        
        # Clear index state
        self.indexed_files = {'indexed_files': {}, 'indexed_urls': []}
        self._save_index_state()
        
        logger.info("Index cleared")
