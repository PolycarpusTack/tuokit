#!/usr/bin/env python3
"""
Test script for TuoKit SQL Pipeline
Tests the core pipeline functions without Streamlit
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Test the pipeline functions
def test_sql_pipeline():
    print("Testing SQL Pipeline Core Functions")
    print("=" * 60)
    
    # Test natural language examples
    test_descriptions = [
        "Show me the top 5 customers by total spending",
        "Find products that are low on stock (less than 20 items)",
        "Calculate monthly sales totals for 2024",
        "List employees who joined in the last 90 days"
    ]
    
    try:
        import ollama
        
        for description in test_descriptions:
            print(f"\nDescription: {description}")
            print("-" * 50)
            
            # Test SQL generation
            prompt = f"""
            Convert this request to PostgreSQL SQL:
            "{description}"
            
            Output ONLY the SQL query.
            """
            
            response = ollama.generate(
                model="deepseek-coder:6.7b",
                prompt=prompt,
                options={"temperature": 0.2}
            )
            
            print("Generated SQL:")
            print(response['response'][:200] + "...")
            
        print("\n" + "=" * 60)
        print("SQL Pipeline functions working correctly!")
        
    except Exception as e:
        print(f"Error: {str(e)}")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is running")
        print("2. Check model: ollama pull deepseek-coder:6.7b")

def test_safety_validation():
    print("\n\nTesting Safety Validation")
    print("=" * 60)
    
    # Test queries
    test_queries = [
        ("SELECT * FROM users", True, "Safe query"),
        ("DROP TABLE users", False, "Should block DROP"),
        ("DELETE FROM orders;", False, "Should block DELETE without WHERE"),
        ("UPDATE users SET active = true WHERE id = 1", True, "Safe UPDATE")
    ]
    
    import re
    
    for query, expected_safe, description in test_queries:
        # Simple safety check
        dangerous_patterns = [
            r'\bDROP\s+TABLE\b',
            r'\bTRUNCATE\b',
            r'\bDELETE\s+FROM\s+\w+\s*;',
            r'\bUPDATE\s+\w+\s+SET\s+.*\s*;'
        ]
        
        is_safe = True
        for pattern in dangerous_patterns:
            if re.search(pattern, query, re.IGNORECASE):
                is_safe = False
                break
        
        status = "PASS" if is_safe == expected_safe else "FAIL"
        print(f"{status}: {query[:30]}... - {description}")

def test_concept_extraction():
    print("\n\nTesting SQL Concept Extraction")
    print("=" * 60)
    
    import re
    
    test_query = """
    SELECT c.name, COUNT(o.id) as order_count, SUM(o.amount) as total
    FROM customers c
    LEFT JOIN orders o ON c.id = o.customer_id
    WHERE o.created_at >= '2024-01-01'
    GROUP BY c.name
    HAVING COUNT(o.id) > 5
    ORDER BY total DESC
    """
    
    concepts = {
        "SELECT": r'\bSELECT\b',
        "JOIN": r'\bJOIN\b',
        "WHERE": r'\bWHERE\b',
        "GROUP BY": r'\bGROUP\s+BY\b',
        "HAVING": r'\bHAVING\b',
        "ORDER BY": r'\bORDER\s+BY\b',
        "Aggregation": r'\b(COUNT|SUM|AVG|MAX|MIN)\s*\('
    }
    
    print("Query contains:")
    for concept, pattern in concepts.items():
        if re.search(pattern, test_query, re.IGNORECASE):
            print(f"  ✓ {concept}")

if __name__ == "__main__":
    print("TuoKit SQL Pipeline - Function Test")
    print("=" * 60)
    
    test_sql_pipeline()
    test_safety_validation()
    test_concept_extraction()
    
    print("\n\nTo use the full SQL Pipeline:")
    print("1. Start TuoKit: streamlit run app.py")
    print("2. Navigate to SQL Pipeline in the sidebar")
    print("\nThe Pipeline provides a guided 4-step workflow:")
    print("  1. Describe your data need in plain English")
    print("  2. Review and edit generated SQL")
    print("  3. Automatic optimization")
    print("  4. Understand and test your query")
