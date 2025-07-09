"""
Test secure JSON cache implementation
"""

import json
import os
from pathlib import Path
from utils.embedding_engine import EmbeddingEngine

def test_secure_cache():
    print("Testing Secure JSON Cache Implementation\n")
    
    engine = EmbeddingEngine()
    
    # Test 1: Clean up old pickle files
    print("1. Cleaning up old pickle cache files:")
    cleaned = engine.cleanup_old_pickle_cache()
    print(f"   Cleaned {cleaned} pickle files")
    
    # Test 2: Test new JSON cache
    print("\n2. Testing JSON cache:")
    test_text = "Test embedding for security validation"
    
    # Generate embedding (will cache)
    embedding1 = engine.embed_text(test_text)
    print(f"   Generated embedding with dimension: {len(embedding1)}")
    
    # Load from cache
    embedding2 = engine.embed_text(test_text)
    print(f"   Loaded from cache: {all(embedding1 == embedding2)}")
    
    # Test 3: Verify cache file format
    print("\n3. Verifying cache file format:")
    cache_key = engine._get_cache_key(test_text)
    cache_file = engine.cache_dir / f"{cache_key}.json"
    
    if cache_file.exists():
        with open(cache_file, 'r') as f:
            data = json.load(f)
            print(f"   Cache format valid: {isinstance(data, dict)}")
            print(f"   Contains embedding: {'embedding' in data}")
            print(f"   Contains model: {'model' in data}")
            print(f"   Contains version: {'version' in data}")
            print(f"   Model matches: {data.get('model') == engine.model}")
    else:
        print("   ❌ Cache file not found")
    
    # Test 4: Test corrupted cache handling
    print("\n4. Testing corrupted cache handling:")
    
    # Create a corrupted cache file
    test_text2 = "Test corrupted cache"
    cache_key2 = engine._get_cache_key(test_text2)
    corrupted_file = engine.cache_dir / f"{cache_key2}.json"
    
    with open(corrupted_file, 'w') as f:
        f.write("corrupted data")
    
    # Should handle gracefully
    embedding3 = engine.embed_text(test_text2)
    print(f"   Handled corrupted cache: {len(embedding3) > 0}")
    print(f"   Corrupted file removed: {not corrupted_file.exists()}")
    
    # Test 5: Security check - no pickle files created
    print("\n5. Security verification:")
    pickle_files = list(engine.cache_dir.glob("*.pkl"))
    print(f"   No pickle files created: {len(pickle_files) == 0}")
    
    print("\n✅ Secure cache test complete")

if __name__ == "__main__":
    test_secure_cache()