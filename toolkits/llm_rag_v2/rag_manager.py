"""
TuoKit RAG Manager v2 - Clean implementation using RAGLite
"""
import os
import logging
from pathlib import Path
from typing import List, Dict, Optional, Union
from datetime import datetime

logger = logging.getLogger(__name__)

# Try to import RAGLite, fall back to stub if not available
try:
    from raglite import (
        RAGLiteConfig, 
        insert_document, 
        update_document,
        delete_document,
        hybrid_search, 
        retrieve_chunks, 
        rerank_chunks, 
        rag
    )
    RAGLITE_AVAILABLE = True
except ImportError:
    logger.warning("RAGLite not installed. Using stub implementation.")
    logger.warning("Install with: pip install -r toolkits/llm_rag_v2/requirements.txt")
    RAGLITE_AVAILABLE = False
    
    # Import stub instead
    from .rag_stub import TuoKitRAGStub
    RAGLiteConfig = dict  # Use dict as config type

# Local imports
from .config import get_default_config, get_indexing_config, get_prompt_template
from .learning_tracker import get_learning_tracker
from .error_handling import (
    handle_rag_errors, TokenLimitError, RetrievalError, 
    ModelNotFoundError, validate_token_limit
)
from .token_manager import TokenManager
from .streaming_handler import StreamingRAGHandler

class TuoKitRAG:
    """
    Simple, clean RAG implementation for TuoKit using RAGLite
    """
    
    def __init__(self, config: Optional[RAGLiteConfig] = None):
        """
        Initialize TuoKit RAG
        
        Args:
            config: Optional RAGLiteConfig. If not provided, uses defaults.
        """
        # If RAGLite not available, return stub instance
        if not RAGLITE_AVAILABLE:
            self.__class__ = TuoKitRAGStub
            TuoKitRAGStub.__init__(self, config)
            return
            
        self.config = config or get_default_config()
        self.indexing_config = get_indexing_config()
        self._indexed_files = set()
        self.learning_tracker = get_learning_tracker()
        self._last_query = None
        self._last_results = []
        
        # Initialize new components
        self.token_manager = TokenManager(
            model=getattr(self.config, 'llm', 'deepseek-coder')
        )
        self.streaming_handler = StreamingRAGHandler()
        
    def index_directory(self, 
                       directory: Union[str, Path],
                       force_reindex: bool = False,
                       show_progress: bool = True) -> Dict[str, int]:
        """
        Index all eligible files in a directory
        
        Args:
            directory: Directory to index
            force_reindex: If True, reindex even if file hasn't changed
            show_progress: If True, log progress
            
        Returns:
            Dict with indexing statistics
        """
        directory = Path(directory)
        if not directory.exists():
            raise ValueError(f"Directory not found: {directory}")
        
        stats = {
            'total_files': 0,
            'indexed': 0,
            'skipped': 0,
            'errors': 0
        }
        
        # Get all eligible files
        eligible_files = self._get_eligible_files(directory)
        stats['total_files'] = len(eligible_files)
        
        if show_progress:
            logger.info(f"Found {len(eligible_files)} files to process")
        
        # Index each file
        for i, file_path in enumerate(eligible_files):
            try:
                if self._should_index_file(file_path, force_reindex):
                    insert_document(file_path, config=self.config)
                    self._indexed_files.add(str(file_path))
                    stats['indexed'] += 1
                    
                    if show_progress and (i + 1) % 10 == 0:
                        logger.info(f"Progress: {i + 1}/{len(eligible_files)} files")
                else:
                    stats['skipped'] += 1
                    
            except Exception as e:
                logger.error(f"Error indexing {file_path}: {e}")
                stats['errors'] += 1
        
        if show_progress:
            logger.info(f"Indexing complete: {stats}")
            
        return stats
    
    def index_file(self, file_path: Union[str, Path]) -> bool:
        """
        Index a single file
        
        Args:
            file_path: Path to file
            
        Returns:
            True if successful, False otherwise
        """
        try:
            file_path = Path(file_path)
            insert_document(file_path, config=self.config)
            self._indexed_files.add(str(file_path))
            return True
        except Exception as e:
            logger.error(f"Error indexing {file_path}: {e}")
            return False
    
    def update_file(self, file_path: Union[str, Path]) -> bool:
        """
        Update an already indexed file
        
        Args:
            file_path: Path to file
            
        Returns:
            True if successful, False otherwise
        """
        try:
            file_path = Path(file_path)
            update_document(file_path, config=self.config)
            return True
        except Exception as e:
            logger.error(f"Error updating {file_path}: {e}")
            return False
    
    def delete_file(self, file_path: Union[str, Path]) -> bool:
        """
        Remove a file from the index
        
        Args:
            file_path: Path to file
            
        Returns:
            True if successful, False otherwise
        """
        try:
            file_path = Path(file_path)
            delete_document(file_path, config=self.config)
            self._indexed_files.discard(str(file_path))
            return True
        except Exception as e:
            logger.error(f"Error deleting {file_path}: {e}")
            return False
    
    @handle_rag_errors
    def search(self, 
              query: str, 
              top_k: int = 10,
              include_scores: bool = True) -> List[Dict]:
        """
        Search for relevant documents using hybrid search
        
        Args:
            query: Search query
            top_k: Number of results to return
            include_scores: If True, include similarity scores
            
        Returns:
            List of search results
        """
        # Validate token limit
        validate_token_limit(query, limit=2048)
        
        # Log query for learning
        self.learning_tracker.log_query(query)
        self._last_query = query
        
        try:
            # Perform hybrid search
            chunk_ids, scores = hybrid_search(
                query, 
                num_results=top_k * 2,  # Get more for reranking
                config=self.config
            )
            
            if not chunk_ids:
                return []
            
            # Retrieve chunks
            chunks = retrieve_chunks(chunk_ids, config=self.config)
            
            # Rerank for better results
            reranked = rerank_chunks(query, chunks, config=self.config)
            
            # Format results
            results = []
            for i, (chunk, score) in enumerate(reranked[:top_k]):
                # Generate unique ID for tracking
                result_id = f"{chunk.get('document_path', 'unknown')}:{chunk.get('chunk_id', i)}"
                
                result = {
                    'content': chunk.get('text', ''),
                    'source_path': chunk.get('document_path', 'unknown'),
                    'chunk_index': i,
                    'metadata': chunk.get('metadata', {}),
                    'result_id': result_id
                }
                
                if include_scores:
                    # Apply learning boost
                    base_score = float(score)
                    learning_boost = self.learning_tracker.get_learning_boost(query, result_id)
                    result['score'] = base_score * (1 + learning_boost)
                    result['base_score'] = base_score
                    result['learning_boost'] = learning_boost
                
                results.append(result)
            
            # Re-sort by boosted scores
            if include_scores:
                results.sort(key=lambda x: x['score'], reverse=True)
            
            self._last_results = results
            return results
            
        except Exception as e:
            logger.error(f"Search error: {e}")
            raise RetrievalError(query, str(e))
    
    @handle_rag_errors
    def generate_answer(self,
                       query: str,
                       query_type: str = "general",
                       top_k: int = 5,
                       include_sources: bool = True,
                       stream: bool = False) -> Dict:
        """
        Search and generate an answer using the LLM
        
        Args:
            query: The question to answer
            query_type: Type of query (general, code_explanation, debugging, implementation)
            top_k: Number of chunks to use for context
            include_sources: If True, include source documents in response
            stream: If True, return a streaming response
            
        Returns:
            Dict with answer and optional sources
        """
        try:
            # Get appropriate prompt template
            system_prompt = get_prompt_template(query_type)
            
            # First search for relevant documents
            search_results = self.search(query, top_k=top_k * 2)
            
            if not search_results:
                raise RetrievalError(query, "No relevant documents found")
            
            # Optimize context selection based on token limits
            optimized_contexts = self.token_manager.optimize_context_selection(
                query=query,
                contexts=search_results,
                system_prompt=system_prompt
            )
            
            # Calculate token usage
            token_usage = self.token_manager.calculate_rag_tokens(
                query=query,
                contexts=[ctx['content'] for ctx in optimized_contexts],
                system_prompt=system_prompt
            )
            
            # Log token usage
            logger.info(self.token_manager.format_token_usage(token_usage))
            
            # If streaming is requested, return a generator
            if stream:
                return {
                    'stream': self._stream_answer(query, optimized_contexts, system_prompt, query_type),
                    'query': query,
                    'token_usage': token_usage,
                    'sources': optimized_contexts if include_sources else []
                }
            
            # Use RAGLite's built-in RAG function with optimized contexts
            answer = rag(
                prompt=query,
                config=self.config,
                search=hybrid_search,
                top_k=len(optimized_contexts),
                system_prompt=system_prompt
            )
            
            result = {
                'answer': answer,
                'query': query,
                'timestamp': datetime.now().isoformat(),
                'token_usage': token_usage
            }
            
            # Optionally include source documents
            if include_sources:
                result['sources'] = optimized_contexts
            
            # Track learning
            self.learning_tracker.log_generation(query, answer, query_type)
            
            return result
            
        except TuoKitRAGError:
            raise  # Re-raise our custom errors
        except Exception as e:
            logger.error(f"Error generating answer: {e}")
            raise RetrievalError(query, str(e))
    
    def _stream_answer(self, query: str, contexts: List[Dict], 
                      system_prompt: str, query_type: str):
        """Internal method to stream answer generation"""
        # Build context string
        context_str = "\n---\n".join([
            f"[{i+1}] {ctx['source_path']}:\n{ctx['content']}" 
            for i, ctx in enumerate(contexts)
        ])
        
        # Set up streaming handler
        def on_token(chunk):
            yield {
                'text': chunk.get('text', ''),
                'token_count': chunk.get('token_count', 0),
                'done': chunk.get('done', False)
            }
        
        self.streaming_handler.on('on_token', on_token)
        
        # Stream the answer
        full_prompt = f"{system_prompt}\n\nContext:\n{context_str}\n\nQuestion: {query}"
        
        # Use streaming handler (sync wrapper for Streamlit compatibility)
        from .streaming_handler import StreamingRAGSync
        sync_streamer = StreamingRAGSync(self)
        
        for chunk in sync_streamer.query_stream_sync(query, top_k=len(contexts)):
            yield chunk
    
    def create_declarative_pipeline(self, pipeline_type: str = "general"):
        """Create a declarative RAG pipeline"""
        from .declarative_rag import SmallTalkRAG, DebugRAG, DeclarativeRAGPipeline
        
        if pipeline_type == "smalltalk":
            return SmallTalkRAG(self)
        elif pipeline_type == "debug":
            return DebugRAG(self)
        else:
            # Create general pipeline
            from .declarative_rag import Retrieve, Rerank, Generate
            pipeline = DeclarativeRAGPipeline(self)
            pipeline.add_module(Retrieve(k=10))
            pipeline.add_module(Rerank(top_k=5))
            pipeline.add_module(Generate())
            return pipeline.compile()
    
    def _get_eligible_files(self, directory: Path) -> List[Path]:
        """Get all files eligible for indexing"""
        eligible_files = []
        
        for pattern in self.indexing_config['include_patterns']:
            for file_path in directory.rglob(pattern):
                # Skip if in excluded directory
                if any(excluded in file_path.parts 
                      for excluded in self.indexing_config['exclude_dirs']):
                    continue
                
                # Skip if file is too large or too small
                try:
                    file_size = file_path.stat().st_size
                    if (file_size > self.indexing_config['max_file_size'] or
                        file_size < self.indexing_config['min_file_size']):
                        continue
                except:
                    continue
                
                # Skip binary files
                if self.indexing_config['skip_binary']:
                    try:
                        with open(file_path, 'r', encoding='utf-8') as f:
                            f.read(100)  # Try reading first 100 chars
                    except:
                        continue  # Skip if can't read as text
                
                eligible_files.append(file_path)
        
        return eligible_files
    
    def _should_index_file(self, file_path: Path, force: bool) -> bool:
        """Check if file should be indexed"""
        if force:
            return True
            
        # Skip if already indexed (basic check)
        if str(file_path) in self._indexed_files:
            return False
            
        return True
    
    def get_stats(self) -> Dict:
        """Get basic statistics about the RAG system"""
        stats = {
            'indexed_files': len(self._indexed_files),
            'config': {
                'llm': self.config.llm,
                'embedder': self.config.embedder,
                'chunk_size': self.config.chunk_max_size
            }
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


# Convenience functions for common operations
def quick_setup(base_path: str = ".") -> TuoKitRAG:
    """
    Quick setup for getting started
    
    Args:
        base_path: Path to index
        
    Returns:
        Configured TuoKitRAG instance
    """
    rag = TuoKitRAG()
    
    logger.info("Starting quick setup...")
    stats = rag.index_directory(base_path)
    logger.info(f"Setup complete: {stats}")
    
    return rag


# Example usage
if __name__ == "__main__":
    # Setup logging
    logging.basicConfig(level=logging.INFO)
    
    # Quick test
    rag = TuoKitRAG()
    
    # Test search
    results = rag.search("crash analyzer", top_k=3)
    print(f"\nFound {len(results)} results")
    for i, result in enumerate(results):
        print(f"\n{i+1}. {result['source_path']}")
        print(f"   Score: {result.get('score', 0):.3f}")
        print(f"   Preview: {result['content'][:100]}...")
    
    # Test answer generation
    response = rag.generate_answer(
        "How does the crash analyzer work?",
        query_type="code_explanation"
    )
    print(f"\nAnswer: {response['answer'][:200]}...")