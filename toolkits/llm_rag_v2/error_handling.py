"""
Enhanced error handling for RAG system
Inspired by Anthropic SDK's error hierarchy
"""

import logging
from typing import Optional, Dict, Any
from functools import wraps
import streamlit as st

logger = logging.getLogger(__name__)

# Base RAG Errors
class TuoKitRAGError(Exception):
    """Base exception for all RAG-related errors"""
    def __init__(self, message: str, details: Optional[Dict[str, Any]] = None):
        super().__init__(message)
        self.message = message
        self.details = details or {}
        logger.error(f"{self.__class__.__name__}: {message}", extra=self.details)
    
    def user_friendly_message(self) -> str:
        """Return a message suitable for displaying to users"""
        return self.message

# Specific Error Types
class TokenLimitError(TuoKitRAGError):
    """Raised when input exceeds token limits"""
    def __init__(self, token_count: int, limit: int):
        super().__init__(
            f"Input too long: {token_count} tokens (limit: {limit})",
            {"token_count": token_count, "limit": limit}
        )
    
    def user_friendly_message(self) -> str:
        return f"Your input is too long. Please reduce it by {self.details['token_count'] - self.details['limit']} tokens."

class RetrievalError(TuoKitRAGError):
    """Raised when document retrieval fails"""
    def __init__(self, query: str, reason: str):
        super().__init__(
            f"Failed to retrieve documents for query: {query}",
            {"query": query, "reason": reason}
        )
    
    def user_friendly_message(self) -> str:
        return "Unable to search the knowledge base. Please try different keywords or check your connection."

class IndexingError(TuoKitRAGError):
    """Raised when document indexing fails"""
    def __init__(self, file_path: str, reason: str):
        super().__init__(
            f"Failed to index file: {file_path}",
            {"file_path": file_path, "reason": reason}
        )
    
    def user_friendly_message(self) -> str:
        return f"Unable to index the file. Reason: {self.details['reason']}"

class EmbeddingError(TuoKitRAGError):
    """Raised when embedding generation fails"""
    def __init__(self, text_length: int, model: str):
        super().__init__(
            f"Failed to generate embeddings for text of length {text_length}",
            {"text_length": text_length, "model": model}
        )
    
    def user_friendly_message(self) -> str:
        return "Unable to process the text for search. The text may be too long or in an unsupported format."

class ModelNotFoundError(TuoKitRAGError):
    """Raised when the specified model is not available"""
    def __init__(self, model_name: str):
        super().__init__(
            f"Model not found: {model_name}",
            {"model_name": model_name}
        )
    
    def user_friendly_message(self) -> str:
        return f"The AI model '{self.details['model_name']}' is not available. Please check your Ollama installation."

class DatabaseConnectionError(TuoKitRAGError):
    """Raised when database connection fails"""
    def __init__(self, db_url: str, original_error: str):
        super().__init__(
            f"Failed to connect to database",
            {"db_url": db_url, "error": original_error}
        )
    
    def user_friendly_message(self) -> str:
        return "Unable to connect to the knowledge database. Please check your database configuration."

class NoResultsError(TuoKitRAGError):
    """Raised when search returns no results"""
    def __init__(self, query: str):
        super().__init__(
            f"No results found for query: {query}",
            {"query": query}
        )
    
    def user_friendly_message(self) -> str:
        return f"No results found for '{self.details['query']}'. Try different keywords or broaden your search."

# Error Handlers
def handle_rag_errors(func):
    """Decorator to handle RAG errors gracefully"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except TuoKitRAGError as e:
            # Log the technical details
            logger.error(f"RAG Error in {func.__name__}: {e.message}", exc_info=True)
            
            # Show user-friendly message in Streamlit
            if 'st' in globals():
                st.error(f"❌ {e.user_friendly_message()}")
            
            # Re-raise for debugging if needed
            if logger.isEnabledFor(logging.DEBUG):
                raise
            
            return None
        except Exception as e:
            # Handle unexpected errors
            logger.error(f"Unexpected error in {func.__name__}: {str(e)}", exc_info=True)
            
            if 'st' in globals():
                st.error("❌ An unexpected error occurred. Please try again or contact support.")
            
            if logger.isEnabledFor(logging.DEBUG):
                raise
            
            return None
    
    return wrapper

# Validation Functions
def validate_token_limit(text: str, limit: int = 8192, model: str = "deepseek-coder"):
    """Validate that text doesn't exceed token limit"""
    # Simple approximation: 1 token ≈ 4 characters
    estimated_tokens = len(text) // 4
    
    if estimated_tokens > limit:
        raise TokenLimitError(estimated_tokens, limit)
    
    return True

def validate_file_type(file_path: str, allowed_extensions: list):
    """Validate file type for indexing"""
    from pathlib import Path
    
    ext = Path(file_path).suffix.lower()
    if ext not in allowed_extensions:
        raise IndexingError(
            file_path, 
            f"File type '{ext}' not supported. Allowed: {', '.join(allowed_extensions)}"
        )
    
    return True

# Error Recovery Strategies
class ErrorRecovery:
    """Strategies for recovering from errors"""
    
    @staticmethod
    def retry_with_backoff(func, max_retries: int = 3, backoff_factor: float = 2.0):
        """Retry a function with exponential backoff"""
        import time
        
        for attempt in range(max_retries):
            try:
                return func()
            except TuoKitRAGError as e:
                if attempt == max_retries - 1:
                    raise
                
                wait_time = backoff_factor ** attempt
                logger.warning(f"Attempt {attempt + 1} failed, retrying in {wait_time}s...")
                time.sleep(wait_time)
    
    @staticmethod
    def fallback_search(primary_func, fallback_func):
        """Try primary search, fall back to simpler search on failure"""
        try:
            return primary_func()
        except RetrievalError:
            logger.warning("Primary search failed, trying fallback...")
            return fallback_func()
    
    @staticmethod
    def chunk_large_input(text: str, chunk_size: int = 4096) -> list:
        """Split large input into smaller chunks"""
        chunks = []
        for i in range(0, len(text), chunk_size):
            chunks.append(text[i:i + chunk_size])
        return chunks

# Streamlit Error Display Component
def create_error_display():
    """Create a nice error display component for Streamlit"""
    return '''
# In your Streamlit page:
import streamlit as st
from toolkits.llm_rag_v2.error_handling import handle_rag_errors, TokenLimitError

@handle_rag_errors
def search_with_error_handling(query):
    # Validate input
    validate_token_limit(query, limit=2048)
    
    # Perform search
    results = rag.search(query)
    
    if not results:
        raise NoResultsError(query)
    
    return results

# Use in UI
try:
    results = search_with_error_handling(user_query)
    display_results(results)
except TokenLimitError as e:
    st.warning(f"⚠️ {e.user_friendly_message()}")
    # Offer to truncate
    if st.button("Truncate and search"):
        truncated = user_query[:2048]
        results = search_with_error_handling(truncated)
'''

# Example Error Monitoring
class RAGErrorMonitor:
    """Monitor and analyze RAG errors"""
    
    def __init__(self):
        self.error_counts = {}
        self.error_patterns = []
    
    def log_error(self, error: TuoKitRAGError):
        """Log error for analysis"""
        error_type = type(error).__name__
        self.error_counts[error_type] = self.error_counts.get(error_type, 0) + 1
        
        self.error_patterns.append({
            'type': error_type,
            'message': error.message,
            'details': error.details,
            'timestamp': datetime.now()
        })
    
    def get_error_summary(self) -> Dict:
        """Get summary of errors"""
        return {
            'total_errors': sum(self.error_counts.values()),
            'error_counts': self.error_counts,
            'most_common': max(self.error_counts.items(), key=lambda x: x[1])[0] if self.error_counts else None,
            'recent_errors': self.error_patterns[-10:]  # Last 10 errors
        }