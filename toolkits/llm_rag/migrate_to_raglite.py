#!/usr/bin/env python3
"""
Migration script from current RAG implementation to RAGLite
Simple, practical, and gets the job done
"""
import os
import sys
from pathlib import Path
import logging
import psycopg2
from typing import List, Dict

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def check_requirements():
    """Check if we can proceed with migration"""
    issues = []
    
    # Check for RAGLite
    try:
        import raglite
        logger.info("✅ RAGLite is installed")
    except ImportError:
        issues.append("RAGLite not installed. Run: pip install raglite flashrank")
    
    # Check for database
    db_url = os.getenv('TUOKIT_DB_URL')
    if not db_url:
        issues.append("TUOKIT_DB_URL environment variable not set")
    else:
        try:
            conn = psycopg2.connect(db_url)
            conn.close()
            logger.info("✅ Database connection successful")
        except:
            issues.append("Cannot connect to database")
    
    # Check for Ollama
    try:
        import ollama
        ollama.list()
        logger.info("✅ Ollama is running")
    except:
        logger.warning("⚠️ Ollama not detected - answer generation will fail")
    
    if issues:
        logger.error("❌ Migration cannot proceed:")
        for issue in issues:
            logger.error(f"  - {issue}")
        return False
    
    return True

def setup_raglite():
    """Initialize RAGLite with TuoKit configuration"""
    from raglite import RAGLiteConfig
    from rerankers import Reranker
    
    config = RAGLiteConfig(
        # Use existing PostgreSQL
        db_url=os.getenv('TUOKIT_DB_URL'),
        
        # Use local models via Ollama
        llm="ollama/deepseek-coder:6.7b",
        
        # Better embeddings - you can change this
        embedder="BAAI/bge-base-en-v1.5",
        
        # Enable reranking for better results
        reranker=Reranker("ms-marco-MiniLM-L-12-v2", model_type="flashrank"),
        
        # Larger chunks for code
        chunk_max_size=1200,
        
        # Store in different table to preserve existing data
        collection_name="raglite_chunks"
    )
    
    logger.info("✅ RAGLite configured")
    return config

def index_codebase(config, base_path: str = "."):
    """Index the TuoKit codebase"""
    from raglite import insert_document
    
    logger.info(f"Indexing codebase from: {base_path}")
    
    # File patterns to index
    patterns = [
        "**/*.py",
        "**/*.md", 
        "**/*.yaml",
        "**/*.yml",
        "**/README*"
    ]
    
    # Directories to skip
    skip_dirs = {
        "__pycache__", ".git", "node_modules", 
        ".pytest_cache", "venv", ".env"
    }
    
    indexed_count = 0
    
    for pattern in patterns:
        for file_path in Path(base_path).rglob(pattern):
            # Skip if in excluded directory
            if any(skip_dir in file_path.parts for skip_dir in skip_dirs):
                continue
            
            # Skip if file is too large (>1MB)
            if file_path.stat().st_size > 1_000_000:
                logger.warning(f"Skipping large file: {file_path}")
                continue
            
            try:
                insert_document(file_path, config=config)
                indexed_count += 1
                
                if indexed_count % 10 == 0:
                    logger.info(f"Indexed {indexed_count} files...")
                    
            except Exception as e:
                logger.error(f"Error indexing {file_path}: {e}")
    
    logger.info(f"✅ Indexed {indexed_count} files")
    return indexed_count

def create_search_wrapper():
    """Create a simple search wrapper for TuoKit integration"""
    wrapper_code = '''"""
RAGLite wrapper for TuoKit integration
Drop-in replacement for the existing RAG manager
"""
from raglite import RAGLiteConfig, hybrid_search, retrieve_chunks, rerank_chunks, rag
from rerankers import Reranker
import os
from typing import List, Dict, Optional
from pathlib import Path

class TuoKitRAG:
    """Simple wrapper around RAGLite for TuoKit"""
    
    def __init__(self):
        self.config = RAGLiteConfig(
            db_url=os.getenv('TUOKIT_DB_URL'),
            llm="ollama/deepseek-coder:6.7b",
            embedder="BAAI/bge-base-en-v1.5",
            reranker=Reranker("ms-marco-MiniLM-L-12-v2", model_type="flashrank"),
            chunk_max_size=1200,
            collection_name="raglite_chunks"
        )
    
    def search(self, query: str, top_k: int = 10) -> List[Dict]:
        """Search using RAGLite hybrid search"""
        chunk_ids, scores = hybrid_search(query, num_results=top_k*2, config=self.config)
        
        if not chunk_ids:
            return []
        
        chunks = retrieve_chunks(chunk_ids, config=self.config)
        reranked = rerank_chunks(query, chunks, config=self.config)
        
        # Format results to match existing interface
        results = []
        for i, (chunk, score) in enumerate(reranked[:top_k]):
            results.append({
                'content': chunk['text'],
                'source_path': chunk.get('document_path', 'unknown'),
                'similarity': float(score),
                'chunk_index': i
            })
        
        return results
    
    def search_with_answer(self, query: str, top_k: int = 5) -> Dict:
        """Search and generate answer using Ollama"""
        # RAGLite handles everything!
        response = rag(
            prompt=query,
            config=self.config,
            search=hybrid_search,
            top_k=top_k
        )
        
        # Format response
        return {
            'answer': response,
            'chunks': self.search(query, top_k)
        }
    
    def index_file(self, file_path: str):
        """Index a single file"""
        from raglite import insert_document
        insert_document(Path(file_path), config=self.config)
    
    def get_stats(self) -> Dict:
        """Get basic statistics"""
        # RAGLite doesn't expose stats directly, so we approximate
        return {
            'status': 'active',
            'backend': 'raglite',
            'ready': True
        }

# Usage example
if __name__ == "__main__":
    rag = TuoKitRAG()
    
    # Test search
    results = rag.search("crash analyzer")
    print(f"Found {len(results)} results")
    
    # Test with answer generation
    response = rag.search_with_answer("How does crash analyzer work?")
    print(f"Answer: {response['answer'][:200]}...")
'''
    
    # Save wrapper
    wrapper_path = Path("toolkits/llm_rag/raglite_wrapper.py")
    wrapper_path.write_text(wrapper_code)
    logger.info(f"✅ Created wrapper at: {wrapper_path}")

def update_imports():
    """Update TuoKit to use the new RAG implementation"""
    logger.info("Updating imports in TuoKit pages...")
    
    # Files that might import RAG
    files_to_check = [
        "pages/rag_search.py",
        "toolkits/llm_rag/app.py"
    ]
    
    for file_path in files_to_check:
        path = Path(file_path)
        if path.exists():
            content = path.read_text()
            
            # Replace imports
            content = content.replace(
                "from rag_manager import TuoKitRAGManager",
                "from toolkits.llm_rag.raglite_wrapper import TuoKitRAG as TuoKitRAGManager"
            )
            
            path.write_text(content)
            logger.info(f"✅ Updated imports in: {file_path}")

def main():
    """Run the migration"""
    logger.info("🚀 Starting RAGLite migration...")
    
    # Step 1: Check requirements
    if not check_requirements():
        return
    
    # Step 2: Setup RAGLite
    config = setup_raglite()
    
    # Step 3: Index codebase
    logger.info("\n📚 Indexing TuoKit codebase...")
    indexed = index_codebase(config, ".")
    
    if indexed == 0:
        logger.error("No files were indexed!")
        return
    
    # Step 4: Create wrapper
    logger.info("\n🔧 Creating integration wrapper...")
    create_search_wrapper()
    
    # Step 5: Update imports
    logger.info("\n🔄 Updating TuoKit imports...")
    update_imports()
    
    # Done!
    logger.info("\n✅ Migration complete!")
    logger.info("\nNext steps:")
    logger.info("1. Test the RAG search in TuoKit")
    logger.info("2. Remove old RAG implementation files")
    logger.info("3. Celebrate - you just saved weeks of work! 🎉")

if __name__ == "__main__":
    main()