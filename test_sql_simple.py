#!/usr/bin/env python3
"""
Simple test script for SQL Generator core functions
Tests without Streamlit dependency
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Direct function tests without importing the full module
def test_sql_functions():
    """Test SQL generation functions directly"""
    print("Testing SQL Generator Core Functions")
    print("=" * 60)
    
    # Test with direct ollama calls
    try:
        import ollama
        
        # Test 1: Basic SQL generation
        print("\n1. Testing SQL Generation")
        print("-" * 30)
        
        prompt = """
        Create a PostgreSQL SQL query for: "Find top 5 customers by total orders"
        
        Requirements:
        1. Use PostgreSQL syntax and functions
        2. Include comprehensive comments
        3. Optimize for performance
        
        Output in markdown with:
        ```sql
        -- Generated SQL
        ```
        """
        
        response = ollama.generate(
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={"temperature": 0.2}
        )
        
        print("OK - SQL Generation working!")
        print(f"Response preview: {response['response'][:200]}...")
        
    except Exception as e:
        print(f"ERROR: {str(e)}")
        print("\nMake sure:")
        print("1. Ollama is running")
        print("2. Model is installed: ollama pull deepseek-coder:6.7b")

def test_dependencies():
    """Test optional dependencies"""
    print("\n2. Testing Dependencies")
    print("-" * 30)
    
    # Check core dependencies
    try:
        import pandas
        print("OK - pandas - installed")
    except ImportError:
        print("MISSING - pandas - not installed (required)")
    
    try:
        import sqlparse
        print("OK - sqlparse - installed")
    except ImportError:
        print("MISSING - sqlparse - not installed (required)")
    
    # Check optional dependencies
    print("\nOptional dependencies:")
    try:
        import sqlalchemy
        print("OK - SQLAlchemy - installed (database connections enabled)")
    except ImportError:
        print("INFO - SQLAlchemy - not installed (database features disabled)")
    
    try:
        import cx_Oracle
        print("OK - cx_Oracle - installed (Oracle support enabled)")
    except ImportError:
        print("INFO - cx_Oracle - not installed (Oracle features disabled)")

def run_tests():
    """Run all tests"""
    print("TuoKit SQL Generator - Function Test")
    print("=" * 60)
    
    test_dependencies()
    test_sql_functions()
    
    print("\n" + "=" * 60)
    print("Test complete!")
    print("\nTo run the full SQL Generator:")
    print("1. Start TuoKit: streamlit run app.py")
    print("2. Navigate to SQL Generator in the sidebar")

if __name__ == "__main__":
    run_tests()
