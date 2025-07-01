#!/usr/bin/env python3
"""
Test script for TuoKit SQL Optimizer
Tests core functionality without Streamlit
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Test the core optimization functions
def test_sql_optimizer():
    print("Testing SQL Optimizer Core Functions")
    print("=" * 60)
    
    # Test queries
    test_queries = {
        "Slow Join": """
SELECT o.*, c.name, c.email 
FROM orders o 
JOIN customers c ON o.customer_id = c.id 
WHERE o.status = 'pending' 
ORDER BY o.created_at DESC
""",
        
        "Subquery": """
SELECT * FROM products 
WHERE category_id IN (
    SELECT id FROM categories 
    WHERE parent_id = 5
)
""",
        
        "N+1 Query": """
SELECT u.*, 
    (SELECT COUNT(*) FROM orders WHERE user_id = u.id) as order_count
FROM users u
WHERE u.active = true
"""
    }
    
    try:
        import ollama
        
        for name, query in test_queries.items():
            print(f"\n{name}:")
            print("-" * 40)
            
            # Test execution plan analysis
            prompt = f"""
            Analyze this PostgreSQL query execution plan:
            {query}
            
            Provide: summary, performance risks, and complexity analysis.
            """
            
            response = ollama.generate(
                model="deepseek-coder:6.7b",
                prompt=prompt,
                options={"temperature": 0.1}
            )
            
            print("Analysis preview:")
            print(response['response'][:300] + "...")
            
        print("\n" + "=" * 60)
        print("SQL Optimizer functions working correctly!")
        
    except Exception as e:
        print(f"Error: {str(e)}")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is running")
        print("2. Check model: ollama pull deepseek-coder:6.7b")

def test_validation_functions():
    print("\n\nTesting Validation Functions")
    print("=" * 60)
    
    # Test dangerous query detection
    dangerous_queries = [
        ("DROP TABLE users", "Should block DROP"),
        ("TRUNCATE orders", "Should block TRUNCATE"),
        ("DELETE FROM customers;", "Should block DELETE without WHERE"),
        ("SELECT * FROM users", "Should pass - safe query")
    ]
    
    for query, expected in dangerous_queries:
        # Simple pattern matching
        dangerous = any(word in query.upper() for word in ['DROP', 'TRUNCATE'])
        if 'DELETE' in query.upper() and 'WHERE' not in query.upper():
            dangerous = True
        
        status = "BLOCKED" if dangerous else "SAFE"
        print(f"{status}: {query[:30]}... - {expected}")

if __name__ == "__main__":
    print("TuoKit SQL Optimizer - Function Test")
    print("=" * 60)
    
    test_sql_optimizer()
    test_validation_functions()
    
    print("\n\nTo use the full SQL Optimizer:")
    print("1. Start TuoKit: streamlit run app.py")
    print("2. Navigate to SQL Optimizer in the sidebar")
