#!/usr/bin/env python3
"""
Enhanced test script for TuoKit Enterprise SQL Generator
Tests all features with and without database connectivity
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pages.sql_generator import (
    generate_sql, explain_sql, optimize_sql, 
    translate_sql, detect_vulnerabilities
)

def test_basic_features():
    """Test features that work without database connection"""
    print("🧪 Testing Basic SQL Generator Features")
    print("=" * 60)
    
    # Test 1: SQL Generation
    print("\n1. SQL Generation Test")
    print("-" * 30)
    query = "Find top 5 customers by total order amount in 2023"
    result = generate_sql(query, "PostgreSQL")
    if not result['error']:
        print(f"✅ Generated SQL:\n{result['sql'][:200]}...")
    else:
        print(f"❌ Error: {result['raw_response']}")
    
    # Test 2: With advanced options
    print("\n2. Stored Procedure Generation")
    print("-" * 30)
    result = generate_sql(
        query, 
        "Oracle", 
        advanced_options={'stored_procedure': True, 'security_hardened': True}
    )
    if not result['error']:
        print(f"✅ Generated Procedure:\n{result['sql'][:200]}...")
    else:
        print(f"❌ Error: {result['raw_response']}")

def test_optimization():
    """Test SQL optimization"""
    print("\n3. SQL Optimization Test")
    print("-" * 30)
    
    test_sql = """
    SELECT * FROM orders o
    JOIN customers c ON o.customer_id = c.id
    WHERE c.country = 'USA' 
    AND o.order_date >= '2023-01-01'
    """
    
    optimization = optimize_sql(test_sql, "PostgreSQL")
    print(f"✅ Optimization suggestions:\n{optimization[:300]}...")

def test_translation():
    """Test SQL translation"""
    print("\n4. SQL Translation Test")
    print("-" * 30)
    
    oracle_sql = """
    SELECT * FROM (
        SELECT employee_id, name, salary,
               ROW_NUMBER() OVER (ORDER BY salary DESC) rn
        FROM employees
    ) WHERE rn <= 10
    """
    
    translated = translate_sql(oracle_sql, "Oracle", "PostgreSQL")
    print(f"✅ Translated to PostgreSQL:\n{translated[:200]}...")

def test_security():
    """Test security scanning"""
    print("\n5. Security Audit Test")
    print("-" * 30)
    
    vulnerable_sql = """
    CREATE PROCEDURE GetUser
    @Username NVARCHAR(50)
    AS
    BEGIN
        EXEC('SELECT * FROM users WHERE username = ''' + @Username + '''')
    END
    """
    
    audit = detect_vulnerabilities(vulnerable_sql)
    print(f"✅ Risk Level: {audit['risk_level']}")
    print(f"Details:\n{audit['details'][:300]}...")

def test_db_connectivity():
    """Test database connectivity features (if available)"""
    print("\n6. Database Connectivity Test")
    print("-" * 30)
    
    try:
        from sqlalchemy import create_engine
        print("✅ SQLAlchemy available - database connections supported")
    except ImportError:
        print("ℹ️ SQLAlchemy not installed - database features disabled")
        print("   To enable: pip install sqlalchemy")
    
    try:
        import cx_Oracle
        print("✅ cx_Oracle available - Oracle connections supported")
    except ImportError:
        print("ℹ️ cx_Oracle not installed - Oracle features disabled")
        print("   To enable: pip install cx_Oracle")

def run_all_tests():
    """Run comprehensive test suite"""
    print("🚀 TuoKit Enterprise SQL Generator - Test Suite")
    print("=" * 60)
    
    try:
        test_basic_features()
        test_optimization()
        test_translation()
        test_security()
        test_db_connectivity()
        
        print("\n" + "=" * 60)
        print("✨ All tests completed!")
        print("\nNotes:")
        print("- Basic features work without database connectivity")
        print("- Install sqlalchemy for live database features")
        print("- Install cx_Oracle for Oracle database support")
        
    except Exception as e:
        print(f"\n❌ Test failed: {str(e)}")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is running")
        print("2. Check model is installed: ollama pull deepseek-coder:6.7b")
        print("3. Verify you're in the TuoKit directory")

if __name__ == "__main__":
    run_all_tests()
