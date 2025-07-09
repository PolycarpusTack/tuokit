"""
TuoKit LLM RAG Toolkit
======================
A practical RAG implementation that integrates llm-search with PostgreSQL
for unified knowledge management.

Features:
- Document ingestion (code, PDFs, DOCX, web scraping)
- PostgreSQL + pgvector for embeddings storage
- Incremental indexing
- Hybrid search (dense + sparse)
- Source attribution and filtering
"""

from .rag_manager import TuoKitRAGManager
from .document_processor import DocumentProcessor
from .web_scraper import KnowledgeBaseScraper
from .postgres_store import PostgresVectorStore

__version__ = "0.1.0"
__all__ = [
    "TuoKitRAGManager",
    "DocumentProcessor", 
    "KnowledgeBaseScraper",
    "PostgresVectorStore"
]
