# TuoKit RAG v2 - RAGLite Implementation

A clean, modern RAG (Retrieval-Augmented Generation) implementation for TuoKit using RAGLite.

## 🚀 Features

- **Hybrid Search**: Combines vector similarity and BM25 keyword search
- **Smart Reranking**: Uses FlashRank for better result ordering
- **Code-Aware**: Optimized chunking for source code
- **Local LLMs**: Integrates with Ollama for privacy-first AI
- **PostgreSQL Support**: Scalable vector storage with pgvector
- **Fallback Mode**: Works without dependencies using stub implementation

## 📦 Installation

### Quick Start (Stub Mode)
The RAG system works out-of-the-box with a stub implementation for testing:

```bash
# No installation needed - just run TuoKit
streamlit run app.py
# Navigate to RAG Knowledge Base in the UI
```

### Full Installation
For the complete RAG experience with actual search and AI capabilities:

```bash
# Install RAGLite and dependencies
pip install -r toolkits/llm_rag_v2/requirements.txt

# Ensure Ollama is running
ollama serve

# Ensure PostgreSQL is available (optional, will use SQLite otherwise)
```

## 🔧 Configuration

The system uses sensible defaults that work immediately:

- **LLM**: Ollama with your selected model (defaults to deepseek-coder)
- **Embeddings**: BAAI/bge-base-en-v1.5 (runs locally)
- **Database**: PostgreSQL with pgvector or SQLite fallback
- **Chunk Size**: 1200 tokens (optimized for code)

## 💡 Usage

### Basic Search
```python
from toolkits.llm_rag_v2 import TuoKitRAG

# Initialize
rag = TuoKitRAG()

# Search your codebase
results = rag.search("crash analyzer implementation", top_k=5)
for result in results:
    print(f"{result['source_path']}: {result['score']:.3f}")
```

### AI-Powered Answers
```python
# Get an AI-generated answer with sources
response = rag.generate_answer(
    "How does the crash analyzer work?",
    query_type="code_explanation"
)
print(response['answer'])
```

### Index Your Codebase
```python
# Index a directory
stats = rag.index_directory("./my_project")
print(f"Indexed {stats['indexed']} files")

# Index a single file
rag.index_file("./important_doc.md")
```

## 🏗️ Architecture

```
toolkits/llm_rag_v2/
├── __init__.py          # Package exports
├── rag_manager.py       # Main RAG implementation
├── config.py            # Configuration and prompts
├── rag_stub.py          # Fallback stub for testing
└── requirements.txt     # Dependencies
```

## 🎯 Design Principles

1. **Works Immediately**: Stub mode allows testing without setup
2. **Production Ready**: Full implementation scales to large codebases
3. **Clean Code**: Simple, maintainable implementation
4. **TuoKit Integration**: Uses existing model manager and database utils

## 🚦 Status Indicators

The UI shows clear status about the RAG system:
- 🟢 **Full Mode**: All dependencies installed, using RAGLite
- 🟡 **Stub Mode**: Running without dependencies for testing
- 🔴 **Error**: Check logs for configuration issues

## 🔍 Query Types

Optimize your results with specialized query types:
- **general**: Default, balanced search
- **code_explanation**: Focus on understanding code
- **debugging**: Help with error analysis
- **implementation**: Get coding guidance

## 🛠️ Troubleshooting

### "No module named 'raglite'"
Install dependencies: `pip install -r toolkits/llm_rag_v2/requirements.txt`

### "Cannot connect to Ollama"
Start Ollama: `ollama serve`

### "Database connection failed"
The system will automatically use SQLite. For PostgreSQL, set:
```bash
export TUOKIT_DB_URL="postgresql://user:pass@localhost/tuokit"
```

## 🔄 Migration from v1

The old llm-search based implementation in `toolkits/llm_rag/` is deprecated.
This v2 implementation:
- ✅ Fixes all architectural issues
- ✅ Uses proven RAGLite framework
- ✅ Includes proper error handling
- ✅ Works without full setup

## 📚 Resources

- [RAGLite Documentation](https://github.com/superlinear-ai/raglite)
- [TuoKit Documentation](../../../docs/)
- [Ollama Models](https://ollama.ai/library)