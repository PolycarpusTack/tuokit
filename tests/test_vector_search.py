"""
Test Vector Search Implementation
Quick test to verify embedding and search functionality
"""

from utils.database import DatabaseManager
from utils.simple_vector_store import SimpleVectorStore
from utils.embedding_engine import EmbeddingEngine

def test_vector_search():
    print("🔍 Testing Vector Search Implementation\n")
    
    # Initialize components
    print("1. Initializing components...")
    db = DatabaseManager()
    vector_store = SimpleVectorStore(db)
    embedding_engine = EmbeddingEngine()
    
    # Get stats
    print("\n2. Current statistics:")
    stats = vector_store.get_stats()
    print(f"   - Total embeddings: {stats['total_embeddings']}")
    print(f"   - Knowledge without embeddings: {stats['knowledge_without_embeddings']}")
    print(f"   - Coverage: {stats['coverage_percentage']:.1f}%")
    
    # Test embedding generation
    print("\n3. Testing embedding generation...")
    test_text = "How to handle database connection errors in Python"
    try:
        embedding = embedding_engine.embed_text(test_text)
        print(f"   ✅ Generated embedding with dimension: {len(embedding)}")
    except Exception as e:
        print(f"   ❌ Embedding generation failed: {e}")
        return
    
    # Generate embeddings for a few knowledge items
    print("\n4. Generating embeddings for knowledge items...")
    try:
        count = vector_store.update_all_embeddings(batch_size=5)
        print(f"   ✅ Generated {count} new embeddings")
    except Exception as e:
        print(f"   ❌ Batch embedding failed: {e}")
    
    # Test search
    print("\n5. Testing vector search...")
    queries = [
        "database connection error",
        "Python debugging",
        "SQL optimization"
    ]
    
    for query in queries:
        print(f"\n   Query: '{query}'")
        try:
            # Generate query embedding
            query_embedding = embedding_engine.embed_text(query)
            
            # Search for similar items
            results = vector_store.search_similar(
                query_embedding,
                model_name=embedding_engine.model,
                threshold=0.3,
                limit=3
            )
            
            if results:
                print("   Results:")
                for knowledge_id, similarity in results:
                    item = db.get_knowledge_by_id(knowledge_id)
                    if item:
                        _, tool, prompt, _, _, _, _ = item
                        print(f"     - [{similarity:.2%}] {tool}: {prompt[:60]}...")
            else:
                print("   No results found")
                
        except Exception as e:
            print(f"   Search failed: {e}")
    
    # Final stats
    print("\n6. Final statistics:")
    stats = vector_store.get_stats()
    print(f"   - Total embeddings: {stats['total_embeddings']}")
    print(f"   - Coverage: {stats['coverage_percentage']:.1f}%")
    
    print("\n✅ Test complete!")

if __name__ == "__main__":
    test_vector_search()