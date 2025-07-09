"""
RAGLite configuration for TuoKit
Sensible defaults that work out of the box
"""
import os
from pathlib import Path
from typing import Optional

# Try to import RAGLite components
try:
    from raglite import RAGLiteConfig
    from rerankers import Reranker
    DEPS_AVAILABLE = True
except ImportError:
    # Use stub types if not available
    RAGLiteConfig = dict
    Reranker = None
    DEPS_AVAILABLE = False

def get_default_config(
    db_url: Optional[str] = None,
    llm_model: Optional[str] = None,
    embedding_model: Optional[str] = None,
    collection_name: str = "tuokit_docs"
) -> RAGLiteConfig:
    """
    Get default RAGLite configuration for TuoKit
    
    Args:
        db_url: Database URL (defaults to TUOKIT_DB_URL env var)
        llm_model: LLM to use (defaults to Ollama deepseek-coder)
        embedding_model: Embedding model (defaults to BGE)
        collection_name: Name for the document collection
    
    Returns:
        Configured RAGLiteConfig instance
    """
    
    # Database configuration
    if db_url is None:
        db_url = os.getenv('TUOKIT_DB_URL')
        if not db_url:
            # Fallback to SQLite for testing
            db_path = Path.home() / ".tuokit" / "rag.db"
            db_path.parent.mkdir(exist_ok=True)
            db_url = f"sqlite:///{db_path}"
    
    # LLM configuration
    if llm_model is None:
        # Check if user has a selected model in session
        # Otherwise use deepseek-coder as default
        llm_model = "ollama/deepseek-coder:6.7b"
    
    # Embedding model configuration
    if embedding_model is None:
        # Use BGE for better code understanding
        # This runs locally via sentence-transformers
        embedding_model = "BAAI/bge-base-en-v1.5"
    
    # Create configuration
    if DEPS_AVAILABLE:
        config = RAGLiteConfig(
            # Database settings
            db_url=db_url,
            collection_name=collection_name,
            
            # Model settings
            llm=llm_model,
            embedder=embedding_model,
            embedder_normalize=True,  # Important for cosine similarity
            
            # Chunking settings optimized for code
            chunk_max_size=1200,  # Larger chunks for code context
            chunk_overlap=200,    # Overlap to maintain context
            
            # Enable reranking for better results
            reranker=Reranker(
                "ms-marco-MiniLM-L-12-v2", 
                model_type="flashrank"
            ),
            
            # Search settings
            vector_search_k=20,   # Get more candidates for reranking
            rerank_k=10,         # Return top 10 after reranking
        )
    else:
        # Return stub config
        from .rag_stub import get_default_config as get_stub_config
        config = get_stub_config(
            db_url=db_url,
            llm_model=llm_model,
            embedding_model=embedding_model,
            collection_name=collection_name
        )
    
    return config

def get_indexing_config() -> dict:
    """
    Get configuration for indexing documents
    
    Returns:
        Dict with indexing settings
    """
    return {
        # File patterns to index
        "include_patterns": [
            "**/*.py",      # Python files
            "**/*.md",      # Markdown docs
            "**/*.rst",     # ReStructuredText
            "**/*.yaml",    # YAML configs
            "**/*.yml",     # YAML configs
            "**/*.json",    # JSON configs
            "**/*.txt",     # Text files
            "**/README*",   # README files
        ],
        
        # Directories to exclude
        "exclude_dirs": {
            "__pycache__",
            ".git",
            ".github",
            "node_modules",
            ".pytest_cache",
            "venv",
            ".venv",
            "env",
            ".env",
            "dist",
            "build",
            ".egg-info",
            "__pypackages__",
            ".tox",
            ".mypy_cache",
            ".ruff_cache",
            "htmlcov",
            ".coverage",
            "*.egg-info"
        },
        
        # File size limits
        "max_file_size": 1_000_000,  # 1MB
        "min_file_size": 10,         # 10 bytes
        
        # Processing options
        "skip_binary": True,
        "respect_gitignore": True,
    }

# Prompt templates for different query types
PROMPT_TEMPLATES = {
    "code_explanation": """
You are a helpful coding assistant. Based on the provided context, explain the code clearly and concisely.
Focus on what the code does, how it works, and any important details.
If the context doesn't contain relevant information, say so.
""",
    
    "debugging": """
You are an expert debugger. Based on the provided context, help debug the issue.
Identify potential causes, suggest fixes, and explain your reasoning.
If you need more information, specify what would be helpful.
""",
    
    "implementation": """
You are a senior developer. Based on the provided context and existing code patterns,
provide implementation guidance. Follow the coding style and patterns shown in the context.
Be specific and provide code examples where appropriate.
""",
    
    "general": """
You are a knowledgeable assistant for the TuoKit project. Answer questions based on the provided context.
Be accurate, helpful, and concise. If the context doesn't contain the answer, acknowledge this.
"""
}

def get_prompt_template(query_type: str = "general") -> str:
    """Get appropriate prompt template based on query type"""
    return PROMPT_TEMPLATES.get(query_type, PROMPT_TEMPLATES["general"])