#!/usr/bin/env python3
"""
Test if the RAG Knowledge Base page can load
"""

import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

print("Testing RAG Knowledge Base page load...\n")

try:
    # Import the page module
    from pages.rag_knowledge_base import show, initialize_rag
    print("✓ Page module imported successfully")
    
    # Test RAG initialization
    print("\nTesting RAG initialization...")
    from toolkits.llm_rag_v2 import TuoKitRAG, get_default_config
    
    rag = TuoKitRAG()
    print("✓ RAG initialized (using stub)")
    
    # Test basic operations
    results = rag.search("test query", top_k=2)
    print(f"✓ Search works: {len(results)} results")
    
    answer = rag.generate_answer("test question")
    print(f"✓ Answer generation works: {len(answer['answer'])} chars")
    
    stats = rag.get_stats()
    print(f"✓ Stats work: {stats['status']}")
    
    print("\n✅ All tests passed!")
    print("\nThe RAG Knowledge Base page should work with the stub implementation.")
    print("To use the full implementation, install dependencies:")
    print("  pip install -r toolkits/llm_rag_v2/requirements.txt")
    
except Exception as e:
    print(f"\n❌ Error: {e}")
    import traceback
    traceback.print_exc()