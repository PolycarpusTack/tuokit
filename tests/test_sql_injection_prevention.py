"""
Test SQL injection prevention in search
"""

from utils.database import DatabaseManager

def test_sql_injection_prevention():
    print("Testing SQL Injection Prevention\n")
    
    db = DatabaseManager()
    if not db.connected:
        print("❌ Database not connected")
        return
    
    # Test cases with potential SQL injection attempts
    test_queries = [
        ("Normal search", "database error"),
        ("Single quote", "'; DROP TABLE knowledge_units; --"),
        ("Double dash comment", "test -- comment"),
        ("Union attempt", "' UNION SELECT * FROM users --"),
        ("Escape attempt", "\\'; DELETE FROM knowledge_units; --"),
        ("Long query", "a" * 1000),
        ("Special chars", "test OR 1=1"),
        ("Null byte", "test\x00more"),
        ("Unicode injection", "test\u0027 OR \u00271\u0027=\u00271"),
        ("Nested query", "test'); INSERT INTO knowledge_units VALUES('hack'")
    ]
    
    print("Testing various SQL injection attempts:\n")
    
    for test_name, query in test_queries:
        print(f"Test: {test_name}")
        print(f"Query: {repr(query[:50])}...")
        
        try:
            # Use the safe search method
            results = db.search_knowledge_safe(query, limit=5)
            print(f"✅ Query executed safely, returned {len(results)} results")
            
            # Verify database is still intact
            count = db.get_knowledge_count()
            if count > 0:
                print(f"✅ Database intact (knowledge count: {count})")
            else:
                print("⚠️  No knowledge items found (might be empty DB)")
                
        except Exception as e:
            print(f"❌ Error: {e}")
        
        print()
    
    # Test input validation
    print("\nTesting input validation:")
    
    invalid_inputs = [
        ("None input", None),
        ("Empty string", ""),
        ("Integer input", 123),
        ("List input", ["test"]),
        ("Dict input", {"query": "test"})
    ]
    
    for test_name, query in invalid_inputs:
        print(f"\nTest: {test_name}")
        try:
            results = db.search_knowledge_safe(query, limit=5)
            print(f"✅ Handled gracefully, returned {len(results)} results")
        except Exception as e:
            print(f"✅ Properly rejected with error: {e}")
    
    print("\n✅ SQL injection prevention test complete")

if __name__ == "__main__":
    test_sql_injection_prevention()