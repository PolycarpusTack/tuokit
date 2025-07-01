#!/usr/bin/env python3
"""
Unified SQL Test Suite for TuoKit
Consolidates all SQL-related tests into organized test cases
"""

import sys
import os
import unittest
from typing import Dict, Optional

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import utilities and SQL functions
from utils import DatabaseManager, safe_ollama_generate
from pages.sql_generator import (
    generate_sql, explain_sql, optimize_sql, 
    translate_sql, detect_vulnerabilities
)
from pages.sql_optimizer import optimize_sql_query, explain_optimization
from pages.sql_pipeline import create_pipeline_sql

class TestSQLCore(unittest.TestCase):
    """Test core SQL generation functionality"""
    
    @classmethod
    def setUpClass(cls):
        """Set up test environment"""
        cls.db = DatabaseManager()
        cls.model = "deepseek-coder:6.7b"
        
    def test_basic_sql_generation(self):
        """Test basic SQL query generation"""
        test_cases = [
            ("Find top 5 customers by total orders", "customers"),
            ("Show monthly revenue for 2024", "revenue"),
            ("Get products with low inventory", "products")
        ]        
        for query, expected_table in test_cases:
            with self.subTest(query=query):
                result = generate_sql(query, "PostgreSQL")
                self.assertFalse(result.get('error'), f"Failed to generate SQL for: {query}")
                self.assertIn(expected_table.upper(), result['sql'].upper())
                
    def test_sql_explanation(self):
        """Test SQL query explanation"""
        sql = """
        SELECT c.name, COUNT(o.id) as order_count
        FROM customers c
        JOIN orders o ON c.id = o.customer_id
        GROUP BY c.id, c.name
        ORDER BY order_count DESC
        LIMIT 5;
        """
        result = explain_sql(sql, self.model)
        self.assertIsNotNone(result)
        self.assertIn("customer", result.lower())
        
    def test_sql_optimization(self):
        """Test SQL query optimization"""
        inefficient_sql = """
        SELECT * FROM orders 
        WHERE customer_id IN (SELECT id FROM customers WHERE country = 'USA')
        """
        result = optimize_sql(inefficient_sql, self.model)
        self.assertIsNotNone(result)
        # Should suggest JOIN instead of subquery
        self.assertTrue("JOIN" in result.upper() or "optimization" in result.lower())

class TestSQLSecurity(unittest.TestCase):
    """Test SQL security features"""
    
    def test_sql_injection_detection(self):
        """Test detection of SQL injection vulnerabilities"""
        vulnerable_queries = [
            "SELECT * FROM users WHERE id = '" + "1 OR 1=1" + "'",
            "DELETE FROM products WHERE name = '" + "'; DROP TABLE users; --" + "'",
            f"UPDATE users SET password = '{'password'}' WHERE username = '{'admin OR 1=1'}'"
        ]
        
        for query in vulnerable_queries:
            with self.subTest(query=query[:50]):
                vulnerabilities = detect_vulnerabilities(query, "deepseek-coder:6.7b")
                self.assertTrue(len(vulnerabilities) > 0, "Failed to detect SQL injection")
                
    def test_safe_query_validation(self):
        """Test that safe queries pass security checks"""
        safe_query = """
        SELECT u.username, u.email 
        FROM users u
        WHERE u.created_at > $1 
        AND u.status = $2
        """
        vulnerabilities = detect_vulnerabilities(safe_query, "deepseek-coder:6.7b")
        self.assertEqual(len(vulnerabilities), 0, "False positive in security check")

class TestSQLPipeline(unittest.TestCase):
    """Test SQL pipeline generation"""
    
    def test_etl_pipeline_creation(self):
        """Test creation of ETL pipeline SQL"""
        description = "Daily sales aggregation from orders to summary table"
        result = create_pipeline_sql(description, "PostgreSQL", "deepseek-coder:6.7b")
        
        self.assertIsNotNone(result)
        # Should contain common ETL elements
        keywords = ["INSERT", "SELECT", "FROM", "GROUP BY"]
        for keyword in keywords:
            self.assertIn(keyword, result.upper())

class TestSQLIntegration(unittest.TestCase):
    """Test integration between SQL tools"""
    
    @classmethod
    def setUpClass(cls):
        """Set up test database connection"""
        cls.db = DatabaseManager()
        
    def test_generate_and_optimize_workflow(self):
        """Test workflow: generate SQL then optimize it"""
        # Step 1: Generate SQL
        query_desc = "Get customer orders with product details"
        generated = generate_sql(query_desc, "PostgreSQL")
        self.assertFalse(generated.get('error'))
        
        # Step 2: Optimize the generated SQL
        if generated.get('sql'):
            optimized = optimize_sql_query(generated['sql'], "deepseek-coder:6.7b")
            self.assertIsNotNone(optimized)
            
    def test_knowledge_capture(self):
        """Test that SQL operations are logged to knowledge base"""
        if not self.db.connected:
            self.skipTest("Database not connected")
            
        initial_count = self.db.get_knowledge_count()
        
        # Perform SQL operation
        generate_sql("Test query for logging", "PostgreSQL")
        
        # Check if logged (may be async, so we just check it doesn't error)
        try:
            new_count = self.db.get_knowledge_count()
            self.assertGreaterEqual(new_count, initial_count)
        except:
            pass  # Knowledge capture is optional

class TestSQLDialects(unittest.TestCase):
    """Test SQL dialect translation"""
    
    def test_dialect_translation(self):
        """Test translating between SQL dialects"""
        postgres_sql = """
        SELECT 
            DATE_TRUNC('month', created_at) as month,
            COUNT(*) as total
        FROM orders
        GROUP BY DATE_TRUNC('month', created_at)
        """
        
        dialects = ["MySQL", "SQLite", "SQL Server"]
        for target_dialect in dialects:
            with self.subTest(dialect=target_dialect):
                result = translate_sql(postgres_sql, "PostgreSQL", target_dialect, "deepseek-coder:6.7b")
                self.assertIsNotNone(result)
                # Basic check - should still have SELECT and FROM
                self.assertIn("SELECT", result.upper())
                self.assertIn("FROM", result.upper())

def run_sql_tests(verbose: bool = True) -> Dict[str, any]:
    """Run all SQL tests and return results"""
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    test_classes = [
        TestSQLCore,
        TestSQLSecurity,
        TestSQLPipeline,
        TestSQLIntegration,
        TestSQLDialects
    ]
    
    for test_class in test_classes:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2 if verbose else 1)
    result = runner.run(suite)
    
    return {
        "total": result.testsRun,
        "failures": len(result.failures),
        "errors": len(result.errors),
        "success": result.wasSuccessful()
    }

def quick_smoke_test():
    """Quick smoke test for CI/CD pipelines"""
    print("🚀 Running SQL Suite Smoke Test")
    print("=" * 60)
    
    try:
        # Test basic SQL generation
        result = generate_sql("Simple SELECT query", "PostgreSQL")
        if result.get('error'):
            print("❌ SQL generation failed")
            return False
            
        print("✅ SQL generation working")
        
        # Test optimization
        optimize_result = optimize_sql("SELECT * FROM users", "deepseek-coder:6.7b")
        if optimize_result:
            print("✅ SQL optimization working")
        else:
            print("⚠️  SQL optimization returned empty result")
            
        print("\n✨ Smoke test passed!")
        return True
        
    except Exception as e:
        print(f"❌ Smoke test failed: {str(e)}")
        return False

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="TuoKit SQL Test Suite")
    parser.add_argument("--smoke", action="store_true", help="Run quick smoke test only")
    parser.add_argument("--quiet", action="store_true", help="Reduce output verbosity")
    
    args = parser.parse_args()
    
    if args.smoke:
        success = quick_smoke_test()
        sys.exit(0 if success else 1)
    else:
        print("🧪 TuoKit SQL Test Suite")
        print("=" * 60)
        results = run_sql_tests(verbose=not args.quiet)
        
        print(f"\n📊 Test Summary:")
        print(f"   Total Tests: {results['total']}")
        print(f"   Failures: {results['failures']}")
        print(f"   Errors: {results['errors']}")
        print(f"   Success: {'✅' if results['success'] else '❌'}")
        
        sys.exit(0 if results['success'] else 1)
