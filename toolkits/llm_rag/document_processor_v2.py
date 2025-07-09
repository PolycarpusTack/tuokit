"""
Enhanced Document Processor v2 for TuoKit RAG
Uses code-aware chunking and better embedding models
"""
import os
from pathlib import Path
from typing import List, Dict, Optional, Generator
import hashlib
from datetime import datetime
import logging
import json
import yaml
from functools import lru_cache
import numpy as np

# Document parsing libraries
from sentence_transformers import SentenceTransformer
import pypdf
from docx import Document

# Import our code-aware chunker
from .code_chunker import CodeAwareChunker

logger = logging.getLogger(__name__)

class EnhancedDocumentProcessor:
    """Improved document processor with code-aware chunking and caching"""
    
    def __init__(self, 
                 embedding_model: str = None,
                 chunk_size: int = 1200,
                 chunk_overlap: int = 200,
                 use_cache: bool = True):
        
        # Use better embedding model for code
        if embedding_model is None:
            # Options in order of preference:
            # 1. Code-specific model (if available)
            # 2. General purpose high-quality model
            # 3. Fallback to lightweight model
            try:
                self.embedder = SentenceTransformer('Salesforce/codet5p-110m-embedding')
                logger.info("Using CodeT5+ embedding model")
            except:
                try:
                    self.embedder = SentenceTransformer('BAAI/bge-base-en-v1.5')
                    logger.info("Using BGE embedding model")
                except:
                    self.embedder = SentenceTransformer('all-MiniLM-L6-v2')
                    logger.warning("Falling back to MiniLM embedding model")
        else:
            self.embedder = SentenceTransformer(embedding_model)
        
        # Initialize code-aware chunker
        self.code_chunker = CodeAwareChunker(
            max_chunk_size=chunk_size,
            chunk_overlap=chunk_overlap
        )
        
        # Cache settings
        self.use_cache = use_cache
        self.cache_dir = Path(".embedding_cache")
        if use_cache:
            self.cache_dir.mkdir(exist_ok=True)
        
        # Processing stats
        self.stats = {
            'files_processed': 0,
            'chunks_created': 0,
            'cache_hits': 0,
            'errors': 0
        }
    
    def process_file(self, file_path: str, force_reprocess: bool = False) -> List[Dict]:
        """Process a single file into chunks with embeddings"""
        path = Path(file_path)
        
        if not path.exists():
            logger.error(f"File not found: {file_path}")
            self.stats['errors'] += 1
            return []
        
        # Check cache first
        if self.use_cache and not force_reprocess:
            cached_chunks = self._get_cached_chunks(file_path)
            if cached_chunks:
                self.stats['cache_hits'] += 1
                return cached_chunks
        
        # Get chunks based on file type
        try:
            chunks = self._get_file_chunks(file_path)
            
            # Process chunks
            processed_chunks = []
            for chunk in chunks:
                # Generate embedding
                embedding = self._get_embedding(chunk['content'])
                
                # Add metadata
                chunk['embedding'] = embedding
                chunk['metadata'] = {
                    'file_name': path.name,
                    'file_size': path.stat().st_size,
                    'modified_at': datetime.fromtimestamp(path.stat().st_mtime).isoformat(),
                    'chunk_type': chunk.get('type', 'unknown'),
                    'chunk_name': chunk.get('name', ''),
                    'language': chunk.get('language', ''),
                    'line_start': chunk.get('line_start', 0)
                }
                
                # Add content hash for deduplication
                content_hash = hashlib.sha256(chunk['content'].encode()).hexdigest()
                chunk['content_hash'] = content_hash
                
                # Determine source type
                chunk['source_type'] = self._determine_source_type(path)
                chunk['source_path'] = str(path)
                
                processed_chunks.append(chunk)
            
            # Cache the processed chunks
            if self.use_cache:
                self._cache_chunks(file_path, processed_chunks)
            
            self.stats['files_processed'] += 1
            self.stats['chunks_created'] += len(processed_chunks)
            
            logger.info(f"Processed {len(processed_chunks)} chunks from {file_path}")
            return processed_chunks
            
        except Exception as e:
            logger.error(f"Error processing {file_path}: {e}")
            self.stats['errors'] += 1
            return []
    
    def _get_file_chunks(self, file_path: str) -> List[Dict]:
        """Get chunks from file using appropriate method"""
        path = Path(file_path)
        ext = path.suffix.lower()
        
        # Use code chunker for code files
        if ext in ['.py', '.js', '.ts', '.jsx', '.tsx', '.java', '.cpp', '.c', '.h', 
                   '.cs', '.rb', '.go', '.rs', '.swift', '.kt', '.scala']:
            return self.code_chunker.chunk_file(file_path)
        
        # Use code chunker for markdown and config files too
        elif ext in ['.md', '.yaml', '.yml', '.json', '.toml', '.ini']:
            return self.code_chunker.chunk_file(file_path)
        
        # Special handling for other file types
        elif ext == '.pdf':
            return self._process_pdf(file_path)
        
        elif ext in ['.docx', '.doc']:
            return self._process_docx(file_path)
        
        elif ext in ['.txt', '.log']:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            return self.code_chunker.chunk_generic_text(content, file_path)
        
        else:
            logger.warning(f"Unsupported file type: {ext}")
            return []
    
    def _process_pdf(self, file_path: str) -> List[Dict]:
        """Process PDF with page awareness"""
        chunks = []
        
        try:
            with open(file_path, 'rb') as f:
                pdf_reader = pypdf.PdfReader(f)
                
                for page_num, page in enumerate(pdf_reader.pages):
                    text = page.extract_text()
                    if text.strip():
                        # Chunk each page separately
                        page_chunks = self.code_chunker.chunk_generic_text(
                            text, 
                            file_path
                        )
                        
                        # Add page metadata
                        for chunk in page_chunks:
                            chunk['page_number'] = page_num + 1
                            chunk['type'] = 'pdf_page'
                            chunks.append(chunk)
                            
        except Exception as e:
            logger.error(f"Error processing PDF {file_path}: {e}")
        
        return chunks
    
    def _process_docx(self, file_path: str) -> List[Dict]:
        """Process DOCX documents"""
        chunks = []
        
        try:
            doc = Document(file_path)
            
            # Extract all text with paragraph awareness
            full_text = []
            for para in doc.paragraphs:
                if para.text.strip():
                    full_text.append(para.text)
            
            # Chunk the combined text
            combined_text = '\n\n'.join(full_text)
            raw_chunks = self.code_chunker.chunk_generic_text(combined_text, file_path)
            
            for chunk in raw_chunks:
                chunk['type'] = 'docx_section'
                chunks.append(chunk)
                
        except Exception as e:
            logger.error(f"Error processing DOCX {file_path}: {e}")
        
        return chunks
    
    @lru_cache(maxsize=10000)
    def _get_embedding(self, text: str) -> np.ndarray:
        """Get embedding with caching"""
        # For very long text, truncate to model's max length
        max_length = 512  # Most models have this limit
        if len(text) > max_length * 4:  # Rough character to token ratio
            text = text[:max_length * 4]
        
        try:
            embedding = self.embedder.encode(text, convert_to_numpy=True)
            return embedding
        except Exception as e:
            logger.error(f"Error generating embedding: {e}")
            # Return zero vector as fallback
            return np.zeros(self.embedder.get_sentence_embedding_dimension())
    
    def _determine_source_type(self, path: Path) -> str:
        """Determine source type from file path and extension"""
        ext = path.suffix.lower()
        
        # Check if it's in specific directories
        path_str = str(path).lower()
        if 'doc' in path_str or 'readme' in path_str:
            return 'documentation'
        elif 'test' in path_str:
            return 'test'
        elif 'example' in path_str:
            return 'example'
        
        # By extension
        if ext in ['.py', '.js', '.java', '.cpp', '.c', '.go', '.rs']:
            return 'code'
        elif ext in ['.md', '.rst', '.txt']:
            return 'documentation'
        elif ext in ['.yaml', '.yml', '.json', '.toml', '.ini']:
            return 'config'
        elif ext in ['.pdf', '.docx']:
            return 'document'
        else:
            return 'other'
    
    def _get_cached_chunks(self, file_path: str) -> Optional[List[Dict]]:
        """Retrieve chunks from cache"""
        if not self.use_cache:
            return None
        
        # Generate cache key
        cache_key = self._get_cache_key(file_path)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        if not cache_file.exists():
            return None
        
        try:
            # Check if file has been modified since cache
            file_mtime = Path(file_path).stat().st_mtime
            cache_mtime = cache_file.stat().st_mtime
            
            if file_mtime > cache_mtime:
                return None  # File is newer than cache
            
            # Load from cache
            with open(cache_file, 'r') as f:
                cached_data = json.load(f)
            
            # Convert embedding lists back to numpy arrays
            chunks = cached_data['chunks']
            for chunk in chunks:
                chunk['embedding'] = np.array(chunk['embedding'])
            
            return chunks
            
        except Exception as e:
            logger.error(f"Error loading cache: {e}")
            return None
    
    def _cache_chunks(self, file_path: str, chunks: List[Dict]):
        """Save chunks to cache"""
        if not self.use_cache:
            return
        
        cache_key = self._get_cache_key(file_path)
        cache_file = self.cache_dir / f"{cache_key}.json"
        
        try:
            # Convert numpy arrays to lists for JSON serialization
            chunks_to_cache = []
            for chunk in chunks:
                chunk_copy = chunk.copy()
                chunk_copy['embedding'] = chunk['embedding'].tolist()
                chunks_to_cache.append(chunk_copy)
            
            cache_data = {
                'file_path': file_path,
                'chunks': chunks_to_cache,
                'model': self.embedder.get_config_dict()['model_name_or_path'],
                'timestamp': datetime.now().isoformat()
            }
            
            with open(cache_file, 'w') as f:
                json.dump(cache_data, f)
                
        except Exception as e:
            logger.error(f"Error saving to cache: {e}")
    
    def _get_cache_key(self, file_path: str) -> str:
        """Generate cache key for file"""
        # Include file path and modification time in key
        file_stat = Path(file_path).stat()
        key_string = f"{file_path}_{file_stat.st_mtime}_{file_stat.st_size}"
        return hashlib.sha256(key_string.encode()).hexdigest()
    
    def get_stats(self) -> Dict:
        """Get processing statistics"""
        return self.stats.copy()
    
    def clear_cache(self):
        """Clear the embedding cache"""
        if self.cache_dir.exists():
            for cache_file in self.cache_dir.glob("*.json"):
                cache_file.unlink()
        
        logger.info("Embedding cache cleared")


# Example usage
if __name__ == "__main__":
    # Test the enhanced processor
    processor = EnhancedDocumentProcessor()
    
    # Process a Python file
    chunks = processor.process_file("app.py")
    
    print(f"Processing stats: {processor.get_stats()}")
    
    for i, chunk in enumerate(chunks[:3]):  # Show first 3 chunks
        print(f"\n--- Chunk {i+1} ---")
        print(f"Type: {chunk['metadata']['chunk_type']}")
        print(f"Name: {chunk['metadata']['chunk_name']}")
        print(f"Lines: {chunk['metadata']['line_start']}+")
        print(f"Content preview: {chunk['content'][:150]}...")
        print(f"Embedding shape: {chunk['embedding'].shape}")