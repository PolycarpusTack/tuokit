"""
Test batch fetching to verify N+1 query fix
"""

from utils.database import DatabaseManager
import time

def test_batch_fetch():
    print("Testing Batch Fetch Implementation\n")
    
    db = DatabaseManager()
    if not db.connected:
        print("❌ Database not connected")
        return
    
    # Test 1: Single fetch (old way)
    print("1. Testing single fetch performance:")
    knowledge_ids = []
    
    # Get some knowledge IDs
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            cur.execute("SELECT id FROM knowledge_units LIMIT 20")
            knowledge_ids = [row[0] for row in cur.fetchall()]
    
    if not knowledge_ids:
        print("   No knowledge items found for testing")
        return
    
    # Time single fetches
    start = time.time()
    results_single = []
    for kid in knowledge_ids:
        item = db.get_knowledge_by_id(kid)
        if item:
            results_single.append(item)
    single_time = time.time() - start
    print(f"   Single fetch time: {single_time:.3f}s for {len(knowledge_ids)} items")
    
    # Test 2: Batch fetch (new way)
    print("\n2. Testing batch fetch performance:")
    start = time.time()
    results_batch = db.get_knowledge_by_ids(knowledge_ids)
    batch_time = time.time() - start
    print(f"   Batch fetch time: {batch_time:.3f}s for {len(knowledge_ids)} items")
    
    # Verify results match
    print("\n3. Verifying results:")
    print(f"   Single fetch returned: {len(results_single)} items")
    print(f"   Batch fetch returned: {len(results_batch)} items")
    
    if len(results_single) == len(results_batch):
        print("   ✅ Result counts match")
    else:
        print("   ❌ Result counts don't match!")
    
    # Performance improvement
    if single_time > 0:
        improvement = (single_time - batch_time) / single_time * 100
        print(f"\n4. Performance improvement: {improvement:.1f}%")
        print(f"   Batch fetch is {single_time/batch_time:.1f}x faster")
    
    print("\n✅ Batch fetch test complete")

if __name__ == "__main__":
    test_batch_fetch()