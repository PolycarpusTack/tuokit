#!/usr/bin/env python3
"""
Enhanced test script for TuoKit SQL Generator
Tests all major features: generation, optimization, translation, and security
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pages.sql_generator import (
    generate_sql, explain_sql, optimize_sql, 
    translate_sql, detect_vulnerabilities
)

def test_sql_generation():
    """Test basic SQL generation"""
    print("🧪 1. Testing SQL Generation")
    print("-" * 50)
    
    test_cases = [
        ("Find top 5 customers by total orders", "PostgreSQL"),
        ("Create employee hierarchy report", "Oracle"),
    ]
    
    for query, db_type in test_cases:
        print(f"\n📝 Query: {query} ({db_type})")
        result = generate_sql(query, db_type)
        
        if result['error']:
            print(f"❌ Error: {result['raw_response']}")
        else:
            print(f"✅ Generated SQL:\n{result['sql'][:200]}...")

def test_optimization():
    """Test SQL optimization"""
    print("\n\n🧪 2. Testing SQL Optimization")
    print("-" * 50)
    
    test_sql = """
    SELECT * FROM orders o
    WHERE o.customer_id IN (
        SELECT customer_id FROM customers 
        WHERE country = 'USA'
    )
    """
    
    print(f"📝 Original SQL:\n{test_sql}")
    optimization = optimize_sql(test_sql, "PostgreSQL")
    print(f"\n✅ Optimization suggestions:\n{optimization[:300]}...")

def test_translation():
    """Test SQL translation between dialects"""
    print("\n\n🧪 3. Testing SQL Translation")
    print("-" * 50)
    
    oracle_sql = """
    SELECT * FROM (
        SELECT employee_id, name, salary,
               ROW_NUMBER() OVER (ORDER BY salary DESC) rn
        FROM employees
    ) WHERE rn <= 10
    """
    
    print(f"📝 Oracle SQL:\n{oracle_sql}")
    translated = translate_sql(oracle_sql, "Oracle", "PostgreSQL")
    print(f"\n✅ PostgreSQL translation:\n{translated[:300]}...")

def test_security():
    """Test SQL security scanning"""
    print("\n\n🧪 4. Testing Security Scanner")
    print("-" * 50)
    
    vulnerable_sql = """
    SELECT * FROM users 
    WHERE username = '" + username + "' 
    AND password = '" + password + "'
    """
    
    print(f"📝 SQL to audit:\n{vulnerable_sql}")
    vulnerabilities = detect_vulnerabilities(vulnerable_sql)
    print(f"\n✅ Security analysis:\n{vulnerabilities[:400]}...")

def run_all_tests():
    """Run all SQL Generator tests"""
    print("🚀 TuoKit SQL Generator - Enhanced Test Suite")
    print("=" * 60)
    
    try:
        test_sql_generation()
        test_optimization()
        test_translation()
        test_security()
        
        print("\n\n" + "=" * 60)
        print("✨ All tests completed successfully!")
        print("\nNote: Results depend on Ollama model responses.")
        
    except Exception as e:
        print(f"\n❌ Test failed: {str(e)}")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is running: 'ollama serve'")
        print("2. Ensure model is installed: 'ollama pull deepseek-coder:6.7b'")
        print("3. Check if you're in the TuoKit directory")

if __name__ == "__main__":
    run_all_tests()
