# TuoKit LLM RAG Toolkit

A practical RAG (Retrieval-Augmented Generation) implementation that integrates with PostgreSQL and supports multiple document types including code, PDFs, DOCX files, and web scraping.

## 🚀 Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Initialize the database
python -m llm_rag.cli init

# Index your codebase
python -m llm_rag.cli index-dir C:/Projects/TuoKit/src --pattern "**/*.py"

# Start the web interface
streamlit run llm_rag/app.py
```

## 📁 Project Structure

```
toolkits/llm_rag/
├── __init__.py          # Package initialization
├── postgres_store.py    # PostgreSQL + pgvector integration
├── document_processor.py # Multi-format document processing
├── web_scraper.py       # Online knowledge base scraping
├── rag_manager.py       # Main orchestrator
├── config.yaml          # Configuration file
├── app.py              # Streamlit web interface
├── cli.py              # Command-line interface
└── README.md           # This file
```

## 🔧 Configuration

Edit `config.yaml` to customize:

```yaml
# PostgreSQL connection
postgres_connection: "postgresql://user:password@localhost:5432/tuokit"

# Embedding model
embedding_model: "all-MiniLM-L6-v2"

# Document sources
sources:
  - name: "codebase"
    path: "C:/Projects/TuoKit/src"
    pattern: "**/*.{py,js,yaml}"
```

## 📚 Supported Document Types

- **Code**: `.py`, `.js`, `.yaml`, `.json`
- **Documents**: `.md`, `.txt`, `.pdf`, `.docx`
- **Web**: HTML pages, documentation sites
- **Config**: YAML, JSON files

## 🎯 Features

### 1. Incremental Indexing
Only processes changed files, saving time on large codebases.

### 2. PostgreSQL Integration
Uses your existing PostgreSQL instance with pgvector for efficient similarity search.

### 3. Web Scraping
Automatically scrapes and indexes online documentation:

```bash
python -m llm_rag.cli index-web https://docs.example.com --max-pages 50
```

### 4. Source Filtering
Search within specific document types:

```python
results = rag.search("authentication", source_filter="code")
```

## 💻 CLI Commands

```bash
# Initialize system
python -m llm_rag.cli init

# Index directory
python -m llm_rag.cli index-dir /path/to/docs --pattern "**/*.md"

# Index web documentation
python -m llm_rag.cli index-web https://docs.example.com

# Search from command line
python -m llm_rag.cli search "how does authentication work"

# Show statistics
python -m llm_rag.cli stats

# Index all configured sources
python -m llm_rag.cli index-all

# Clear index (careful!)
python -m llm_rag.cli clear
```

## 🌐 Web Interface

The Streamlit interface provides:
- Visual search interface
- Real-time indexing progress
- Statistics dashboard
- Source management
- Result previews with similarity scores

## 🔌 Integration Examples

### Python Script
```python
from llm_rag import TuoKitRAGManager

# Initialize
rag = TuoKitRAGManager(config_path="config.yaml")

# Index a directory
results = rag.index_directory("/path/to/docs")

# Search
results = rag.search("database connection", top_k=5)
```

### With Ollama (Coming Soon)
```python
# Search with LLM-generated answers
results = rag.search("explain the auth system", use_llm=True)
```

## 🚧 Roadmap

- [ ] Ollama integration for answer generation
- [ ] Async indexing with Celery
- [ ] Redis caching layer
- [ ] API endpoint for external tools
- [ ] Hybrid search (dense + sparse)
- [ ] Document update detection
- [ ] Export/import index snapshots

## 🐛 Troubleshooting

### PostgreSQL Connection Error
```bash
# Check if pgvector extension is installed
psql -d tuokit -c "CREATE EXTENSION IF NOT EXISTS vector;"
```

### Memory Issues with Large Files
Adjust chunk size in `config.yaml`:
```yaml
chunk_size: 500  # Smaller chunks
chunk_overlap: 100
```

### Slow Embedding Generation
Use GPU acceleration:
```bash
pip install torch==2.0.0+cu118 -f https://download.pytorch.org/whl/torch_stable.html
```

## 📄 License

Part of the TuoKit project. See main project license.
