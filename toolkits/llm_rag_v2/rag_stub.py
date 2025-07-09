"""
Temporary RAG stub for testing without full dependencies
This allows the page to load and show the interface even without RAGLite installed
"""

import logging
from pathlib import Path
from typing import List, Dict, Optional, Union
from datetime import datetime
from .learning_tracker import get_learning_tracker
from .token_manager import TokenManager
from .streaming_handler import StreamingRAGHandler

logger = logging.getLogger(__name__)

class TuoKitRAGStub:
    """
    Stub implementation of TuoKitRAG for testing without dependencies
    """
    
    def __init__(self, config=None):
        """Initialize stub RAG"""
        self.config = config or {}
        self._indexed_files = set()
        self.learning_tracker = get_learning_tracker()
        self._last_query = None
        self._last_results = []
        
        # Initialize new components
        self.token_manager = TokenManager(model='stub-model')
        self.streaming_handler = StreamingRAGHandler()
        
        logger.info("Using RAG stub implementation (dependencies not installed)")
        
    def index_directory(self, 
                       directory: Union[str, Path],
                       force_reindex: bool = False,
                       show_progress: bool = True) -> Dict[str, int]:
        """Stub: Pretend to index directory"""
        directory = Path(directory)
        
        # Simulate indexing
        stats = {
            'total_files': 42,
            'indexed': 38,
            'skipped': 3,
            'errors': 1
        }
        
        if show_progress:
            logger.info(f"[STUB] Would index {directory}")
            logger.info(f"[STUB] Simulated stats: {stats}")
            
        return stats
    
    def index_file(self, file_path: Union[str, Path]) -> bool:
        """Stub: Pretend to index file"""
        logger.info(f"[STUB] Would index file: {file_path}")
        return True
    
    def update_file(self, file_path: Union[str, Path]) -> bool:
        """Stub: Pretend to update file"""
        logger.info(f"[STUB] Would update file: {file_path}")
        return True
    
    def delete_file(self, file_path: Union[str, Path]) -> bool:
        """Stub: Pretend to delete file"""
        logger.info(f"[STUB] Would delete file: {file_path}")
        return True
    
    def search(self, 
              query: str, 
              top_k: int = 10,
              include_scores: bool = True) -> List[Dict]:
        """Stub: Return sample search results"""
        # Log query for learning
        self.learning_tracker.log_query(query)
        self._last_query = query
        
        # Return some dummy results
        results = [
            {
                'content': f'# Sample Result 1\\nThis is a sample search result for "{query}"',
                'source_path': 'pages/crash_analyzer.py',
                'chunk_index': 0,
                'metadata': {},
                'result_id': 'pages/crash_analyzer.py:0',
                'score': 0.85
            },
            {
                'content': f'def process_{query.replace(" ", "_")}():\\n    """Process function"""\\n    pass',
                'source_path': 'utils/tool_base.py',
                'chunk_index': 1,
                'metadata': {},
                'result_id': 'utils/tool_base.py:1',
                'score': 0.72
            }
        ]
        
        self._last_results = results[:top_k]
        return self._last_results
    
    def generate_answer(self,
                       query: str,
                       query_type: str = "general",
                       top_k: int = 5,
                       include_sources: bool = True,
                       stream: bool = False) -> Dict:
        """Stub: Generate a sample answer with streaming support"""
        answer = f"""[RAG Stub Response]

I would search for information about "{query}" in the TuoKit codebase.

Since RAGLite dependencies are not installed, this is a demonstration response. 
To get actual results:

1. Install dependencies: pip install -r toolkits/llm_rag_v2/requirements.txt
2. Ensure Ollama is running
3. Index your codebase using the Index Management tab

Query type: {query_type}
"""
        
        # Calculate token usage (stub)
        token_usage = {
            'query': len(query) // 4,
            'contexts': 100,
            'system': 50,
            'total': 150 + len(query) // 4,
            'available': 4096,
            'remaining': 3946 - len(query) // 4
        }
        
        if stream:
            # Return streaming stub
            def stub_stream():
                words = answer.split()
                for i, word in enumerate(words):
                    yield {
                        'text': word + ' ',
                        'token_count': i + 1,
                        'done': i == len(words) - 1
                    }
            
            return {
                'stream': stub_stream(),
                'query': query,
                'token_usage': token_usage,
                'sources': self.search(query, top_k=top_k) if include_sources else []
            }
        
        result = {
            'answer': answer,
            'query': query,
            'timestamp': datetime.now().isoformat(),
            'token_usage': token_usage
        }
        
        if include_sources:
            result['sources'] = self.search(query, top_k=top_k)
        
        return result
    
    def create_declarative_pipeline(self, pipeline_type: str = "general"):
        """Create a stub declarative pipeline"""
        # Return a mock pipeline object
        class StubPipeline:
            def forward(self, **kwargs):
                return {
                    'answer': f"[Stub Pipeline] Would process: {kwargs.get('query', 'no query')}",
                    'documents': self.search(kwargs.get('query', ''), top_k=3),
                    'pipeline_type': pipeline_type
                }
            
            def optimize(self, examples, metric=None):
                return {
                    'baseline': 0.5,
                    'optimized': 0.7,
                    'improvement': 40.0
                }
        
        return StubPipeline()
    
    def get_stats(self) -> Dict:
        """Get stub statistics"""
        stats = {
            'indexed_files': 42,
            'config': {
                'llm': 'ollama/deepseek-coder (stub)',
                'embedder': 'BAAI/bge-base-en-v1.5 (stub)',
                'chunk_size': 1200
            },
            'status': 'Dependencies not installed - using stub'
        }
        
        # Add learning stats
        learning_stats = self.learning_tracker.get_stats()
        stats['learning'] = learning_stats
        
        return stats
    
    def mark_helpful(self, result_id: str):
        """Mark a result as helpful"""
        if self._last_query:
            self.learning_tracker.log_feedback(
                self._last_query, 
                result_id, 
                helpful=True
            )
    
    def mark_not_helpful(self, result_id: str):
        """Mark a result as not helpful"""
        if self._last_query:
            self.learning_tracker.log_feedback(
                self._last_query, 
                result_id, 
                helpful=False
            )
    
    def track_interaction(self, result_id: str, action: str):
        """Track user interaction with a result"""
        if self._last_query:
            self.learning_tracker.log_interaction(
                self._last_query,
                result_id,
                action
            )
    
    def get_popular_queries(self, limit: int = 10) -> List[str]:
        """Get most popular queries"""
        return self.learning_tracker.get_popular_queries(limit)
    
    def get_query_suggestions(self, partial: str, limit: int = 5) -> List[str]:
        """Get query suggestions based on partial input"""
        return self.learning_tracker.get_query_suggestions(partial, limit)


def get_default_config(**kwargs):
    """Stub config function"""
    return {
        'stub': True,
        'message': 'Using stub configuration',
        **kwargs
    }