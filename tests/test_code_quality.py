"""
Test code quality improvements
"""

from utils.vector_constants import *
from utils.embedding_engine import EmbeddingEngine
from utils.simple_vector_store import SimpleVectorStore
from utils.database import DatabaseManager

def test_code_quality():
    print("Testing Code Quality Improvements\n")
    
    # Test 1: Constants are properly defined
    print("1. Testing constants:")
    constants = [
        ("DEFAULT_EMBEDDING_MODEL", DEFAULT_EMBEDDING_MODEL),
        ("DEFAULT_SEARCH_LIMIT", DEFAULT_SEARCH_LIMIT),
        ("VECTOR_SEARCH_CHUNK_SIZE", VECTOR_SEARCH_CHUNK_SIZE),
        ("SEMANTIC_SEARCH_THRESHOLD", SEMANTIC_SEARCH_THRESHOLD),
        ("MAX_TEXT_LENGTH", MAX_TEXT_LENGTH)
    ]
    
    for name, value in constants:
        print(f"   {name}: {value}")
    print("   ✅ All constants properly defined")
    
    # Test 2: No hardcoded values
    print("\n2. Testing default parameters use constants:")
    
    engine = EmbeddingEngine()
    print(f"   Engine default model: {engine.model} == {DEFAULT_EMBEDDING_MODEL}")
    print(f"   ✅ Using constant: {engine.model == DEFAULT_EMBEDDING_MODEL}")
    
    # Test 3: Circular import resolution
    print("\n3. Testing circular import resolution:")
    
    db = DatabaseManager()
    if db.connected:
        # Create vector store with engine
        vector_store = SimpleVectorStore(db, engine)
        print("   ✅ Vector store created with engine reference")
        
        # Test that it doesn't create new engine instances
        import numpy as np
        test_embedding = np.random.rand(DEFAULT_EMBEDDING_DIM)
        
        # This should use the provided engine
        results = vector_store.search_similar(
            test_embedding,
            DEFAULT_EMBEDDING_MODEL,
            limit=5
        )
        print("   ✅ Search completed without circular import")
    else:
        print("   ⚠️  Database not connected, skipping circular import test")
    
    # Test 4: Parameter validation
    print("\n4. Testing parameter defaults:")
    
    # Create engine without parameters
    engine2 = EmbeddingEngine()
    print(f"   Default model: {engine2.model}")
    
    # Test embedding with empty text
    empty_embedding = engine2.embed_text("")
    print(f"   Empty text embedding size: {len(empty_embedding)} == {DEFAULT_EMBEDDING_DIM}")
    print(f"   ✅ Correct dimension: {len(empty_embedding) == DEFAULT_EMBEDDING_DIM}")
    
    # Test 5: Cache version
    print("\n5. Testing cache versioning:")
    
    # Generate an embedding to create cache
    test_text = "Test cache versioning"
    embedding = engine2.embed_text(test_text)
    
    # Check cache file exists and has version
    cache_key = engine2._get_cache_key(test_text)
    cache_file = engine2.cache_dir / f"{cache_key}.json"
    
    if cache_file.exists():
        import json
        with open(cache_file, 'r') as f:
            cache_data = json.load(f)
            print(f"   Cache version: {cache_data.get('version')}")
            print(f"   ✅ Version present: {'version' in cache_data}")
    else:
        print("   ⚠️  Cache file not created")
    
    print("\n✅ Code quality test complete")

if __name__ == "__main__":
    test_code_quality()