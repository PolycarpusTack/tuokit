#!/usr/bin/env python3
"""
Test script to check RAGLite setup without all dependencies
"""

import sys
import os
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

print("=== TuoKit RAGLite v2 Setup Test ===\n")

# Check if basic imports work
print("1. Checking Python environment...")
print(f"   Python version: {sys.version}")
print(f"   Current directory: {os.getcwd()}")

# Check if RAGLite structure exists
print("\n2. Checking RAGLite v2 structure...")
rag_v2_path = Path("toolkits/llm_rag_v2")
if rag_v2_path.exists():
    print(f"   ✓ RAGLite v2 directory exists: {rag_v2_path}")
    files = list(rag_v2_path.glob("*.py"))
    for f in files:
        print(f"     - {f.name}")
else:
    print(f"   ✗ RAGLite v2 directory not found!")

# Check if page exists
print("\n3. Checking Streamlit page...")
page_path = Path("pages/rag_knowledge_base.py")
if page_path.exists():
    print(f"   ✓ RAG Knowledge Base page exists: {page_path}")
else:
    print(f"   ✗ RAG Knowledge Base page not found!")

# Check for required packages
print("\n4. Checking installed packages...")
packages = {
    "raglite": "RAGLite (core engine)",
    "sentence_transformers": "Sentence Transformers (embeddings)",
    "flashrank": "FlashRank (reranking)",
    "psycopg2": "PostgreSQL adapter",
    "streamlit": "Streamlit (UI)",
    "ollama": "Ollama (LLM integration)"
}

for pkg, desc in packages.items():
    try:
        __import__(pkg)
        print(f"   ✓ {desc}: Installed")
    except ImportError:
        print(f"   ✗ {desc}: Not installed")

# Check Ollama connection
print("\n5. Checking Ollama connection...")
try:
    import requests
    response = requests.get("http://localhost:11434/api/tags", timeout=2)
    if response.status_code == 200:
        models = response.json().get('models', [])
        print(f"   ✓ Ollama is running with {len(models)} models")
        if models:
            print("   Available models:")
            for m in models[:5]:  # Show first 5
                print(f"     - {m['name']}")
    else:
        print(f"   ✗ Ollama responded with status {response.status_code}")
except Exception as e:
    print(f"   ✗ Cannot connect to Ollama: {e}")

# Check database
print("\n6. Checking database configuration...")
db_url = os.getenv('TUOKIT_DB_URL')
if db_url:
    print(f"   ✓ TUOKIT_DB_URL is set: {db_url[:30]}...")
else:
    print("   ℹ TUOKIT_DB_URL not set, will use SQLite fallback")
    fallback_db = Path.home() / ".tuokit" / "rag.db"
    print(f"   SQLite path: {fallback_db}")
    if fallback_db.parent.exists():
        print(f"   ✓ Directory exists: {fallback_db.parent}")

# Summary
print("\n=== Summary ===")
print("\nTo complete RAGLite setup:")
print("1. Install dependencies: pip install -r toolkits/llm_rag_v2/requirements.txt")
print("2. Ensure Ollama is running: ollama serve")
print("3. Run the app: streamlit run app.py")
print("4. Navigate to the RAG Knowledge Base page")
print("\nThe implementation is ready but needs dependencies installed.")