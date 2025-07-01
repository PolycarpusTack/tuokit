#!/usr/bin/env python3
"""
Test script for TuoKit SQL Generator
Tests basic functionality without full Streamlit app
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pages.sql_generator import generate_sql, explain_sql

def test_sql_generator():
    """Test SQL generation functionality"""
    
    print("🧪 Testing TuoKit SQL Generator")
    print("-" * 50)
    
    # Test PostgreSQL query
    print("\n1. Testing PostgreSQL Generation:")
    test_query = "Find top 5 customers by total order amount in 2023"
    schema_hint = "customers(id, name, email)\norders(id, customer_id, amount, order_date)"
    
    result = generate_sql(test_query, "PostgreSQL", schema_hint)
    
    if result['error']:
        print(f"❌ Error: {result['raw_response']}")
    else:
        print(f"✅ Generated SQL:")
        print(result['sql'])
        print("\n📝 Full Response:")
        print(result['raw_response'][:200] + "...")
    
    # Test Oracle query
    print("\n\n2. Testing Oracle Generation:")
    oracle_result = generate_sql(test_query, "Oracle", schema_hint)
    
    if oracle_result['error']:
        print(f"❌ Error: {oracle_result['raw_response']}")
    else:
        print(f"✅ Generated SQL:")
        print(oracle_result['sql'])
    
    # Test SQL explanation
    if not result['error'] and result['sql']:
        print("\n\n3. Testing SQL Analysis:")
        analysis = explain_sql(result['sql'], "PostgreSQL")
        print("📊 Analysis:")
        print(analysis[:300] + "...")
    
    print("\n" + "-" * 50)
    print("✨ SQL Generator test complete!")

if __name__ == "__main__":
    try:
        test_sql_generator()
    except Exception as e:
        print(f"❌ Test failed: {str(e)}")
        print("\nMake sure:")
        print("1. Ollama is running")
        print("2. deepseek-coder:6.7b model is installed")
        print("3. You're in the TuoKit directory")
