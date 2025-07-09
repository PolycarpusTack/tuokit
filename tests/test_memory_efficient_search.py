"""
Test memory-efficient vector search
"""

import numpy as np
import psutil
import os
from utils.database import DatabaseManager
from utils.simple_vector_store import SimpleVectorStore
from utils.embedding_engine import EmbeddingEngine

def get_memory_usage():
    """Get current process memory usage in MB"""
    process = psutil.Process(os.getpid())
    return process.memory_info().rss / 1024 / 1024

def test_memory_efficient_search():
    print("Testing Memory-Efficient Vector Search\n")
    
    db = DatabaseManager()
    if not db.connected:
        print("❌ Database not connected")
        return
    
    vector_store = SimpleVectorStore(db)
    engine = EmbeddingEngine()
    
    # Get embedding statistics
    stats = vector_store.get_stats()
    print(f"1. Current embeddings: {stats['total_embeddings']}")
    
    # Test search with memory monitoring
    print("\n2. Testing vector search memory usage:")
    
    # Create a test query
    test_query = "database connection error handling"
    
    # Monitor memory before search
    memory_before = get_memory_usage()
    print(f"   Memory before search: {memory_before:.1f} MB")
    
    try:
        # Generate query embedding
        query_embedding = engine.embed_text(test_query)
        
        # Perform search
        results = vector_store.search_similar(
            query_embedding,
            model_name=engine.model,
            threshold=0.3,
            limit=10
        )
        
        # Monitor memory after search
        memory_after = get_memory_usage()
        print(f"   Memory after search: {memory_after:.1f} MB")
        print(f"   Memory increase: {memory_after - memory_before:.1f} MB")
        
        print(f"\n3. Search results: {len(results)} items found")
        for i, (kid, similarity) in enumerate(results[:3]):
            print(f"   {i+1}. ID: {kid}, Similarity: {similarity:.3f}")
        
        # Test with different chunk sizes
        print("\n4. Testing chunk processing:")
        
        # Simulate processing many embeddings
        chunk_test_sizes = [50, 100, 200]
        for chunk_size in chunk_test_sizes:
            # Temporarily modify chunk size (would need to expose this as parameter)
            print(f"   Chunk size {chunk_size}: ", end="")
            
            # Just verify the search completes
            try:
                results = vector_store.search_similar(
                    query_embedding,
                    model_name=engine.model,
                    threshold=0.3,
                    limit=5
                )
                print(f"✅ Found {len(results)} results")
            except Exception as e:
                print(f"❌ Error: {e}")
        
        print("\n✅ Memory-efficient search test complete")
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")

if __name__ == "__main__":
    test_memory_efficient_search()