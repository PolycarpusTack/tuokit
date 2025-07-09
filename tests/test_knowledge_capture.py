#!/usr/bin/env python3
"""
Test suite for the Unified Knowledge Capture System
Tests quality gates, categorization, and capture functionality
"""

import unittest
import sys
import os
from datetime import datetime

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.knowledge_capture import (
    QualityGate, QualityMetrics, KnowledgeCategorizer, 
    UnifiedKnowledgeCapture, KNOWLEDGE_CATEGORIES
)

class TestQualityMetrics(unittest.TestCase):
    """Test quality scoring calculations"""
    
    def test_short_content_score(self):
        """Test scoring for short content"""
        metrics = QualityMetrics(
            content_length=60,
            code_block_count=0,
            has_explanation=False,
            has_example=False,
            specificity_score=0.2,
            completeness_score=0.3
        )
        score = metrics.overall_score
        self.assertLess(score, 30, "Short content should have low score")
        
    def test_high_quality_content_score(self):
        """Test scoring for high-quality content"""
        metrics = QualityMetrics(
            content_length=800,
            code_block_count=2,
            has_explanation=True,
            has_example=True,
            specificity_score=0.8,
            completeness_score=0.9
        )
        score = metrics.overall_score
        self.assertGreater(score, 70, "High-quality content should have high score")
        
    def test_code_focused_content(self):
        """Test scoring for code-heavy content"""
        metrics = QualityMetrics(
            content_length=300,
            code_block_count=3,
            has_explanation=False,
            has_example=True,
            specificity_score=0.9,
            completeness_score=0.7
        )
        score = metrics.overall_score
        self.assertGreater(score, 50, "Code-focused content should have decent score")

class TestQualityGate(unittest.TestCase):
    """Test quality validation"""
    
    def setUp(self):
        self.gate = QualityGate()
        
    def test_reject_short_content(self):
        """Test rejection of too-short content"""
        is_valid, reason, metrics = self.gate.validate(
            "This is too short", 
            {"tool": "test"}
        )
        self.assertFalse(is_valid)
        self.assertIn("too short", reason)
        
    def test_reject_long_content(self):
        """Test rejection of too-long content"""
        long_content = "x" * 15000
        is_valid, reason, metrics = self.gate.validate(
            long_content,
            {"tool": "test"}
        )
        self.assertFalse(is_valid)
        self.assertIn("too long", reason)
        
    def test_accept_good_content(self):
        """Test acceptance of quality content"""
        good_content = '''
        Here's how to implement a binary search in Python:
        
        ```python
        def binary_search(arr, target):
            left, right = 0, len(arr) - 1
            
            while left <= right:
                mid = (left + right) // 2
                if arr[mid] == target:
                    return mid
                elif arr[mid] < target:
                    left = mid + 1
                else:
                    right = mid - 1
                    
            return -1
        ```
        
        This algorithm works by repeatedly dividing the search space in half.
        For example, searching for 5 in [1, 3, 5, 7, 9] would check index 2 first.
        '''
        
        is_valid, reason, metrics = self.gate.validate(
            good_content,
            {"tool": "code_explainer"}
        )
        self.assertTrue(is_valid)
        self.assertIsNotNone(metrics)
        self.assertGreater(metrics.overall_score, 50)
        
    def test_metrics_calculation(self):
        """Test quality metrics calculation"""
        content = '''
        def calculate_fibonacci(n):
            """Calculate nth Fibonacci number"""
            if n <= 1:
                return n
            return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)
            
        This recursive implementation has exponential time complexity.
        For example, calculate_fibonacci(5) returns 5.
        '''
        
        is_valid, reason, metrics = self.gate._calculate_metrics(
            content, 
            {"tool": "test"}
        )
        
        self.assertEqual(metrics.code_block_count, 0)  # No ``` blocks
        self.assertTrue(metrics.has_explanation)  # Contains "This..."
        self.assertTrue(metrics.has_example)  # Contains "For example"
        self.assertGreater(metrics.specificity_score, 0)  # Has specific terms

class TestKnowledgeCategorizer(unittest.TestCase):
    """Test automatic categorization"""
    
    def setUp(self):
        self.categorizer = KnowledgeCategorizer()
        
    def test_categorize_code_pattern(self):
        """Test categorization of code patterns"""
        content = '''
        def merge_sort(arr):
            if len(arr) <= 1:
                return arr
            mid = len(arr) // 2
            left = merge_sort(arr[:mid])
            right = merge_sort(arr[mid:])
            return merge(left, right)
        '''
        
        category, tags = self.categorizer.categorize(
            content,
            {"tool": "algorithm_generator"}
        )
        
        self.assertEqual(category, "code_pattern")
        self.assertIn("python", tags)
        
    def test_categorize_error_solution(self):
        """Test categorization of error solutions"""
        content = '''
        To fix the ImportError: No module named 'requests', you need to install it:
        
        pip install requests
        
        This error occurs when the requests library is not installed in your environment.
        '''
        
        category, tags = self.categorizer.categorize(
            content,
            {"tool": "error_resolver"}
        )
        
        self.assertEqual(category, "error_solution")
        self.assertIn("import", tags)
        
    def test_categorize_sql_query(self):
        """Test categorization of SQL queries"""
        content = '''
        SELECT u.name, COUNT(o.id) as order_count
        FROM users u
        LEFT JOIN orders o ON u.id = o.user_id
        WHERE u.created_at > '2024-01-01'
        GROUP BY u.id, u.name
        HAVING COUNT(o.id) > 5
        ORDER BY order_count DESC;
        '''
        
        category, tags = self.categorizer.categorize(
            content,
            {"tool": "sql_generator"}
        )
        
        self.assertEqual(category, "sql_query")
        self.assertIn("select", tags)
        self.assertIn("join", tags)
        
    def test_extract_framework_tags(self):
        """Test framework detection in tags"""
        content = '''
        import streamlit as st
        
        st.title("My App")
        if st.button("Click me"):
            st.write("Button clicked!")
        '''
        
        category, tags = self.categorizer.categorize(
            content,
            {"tool": "code_generator"}
        )
        
        self.assertIn("streamlit", tags)
        self.assertIn("python", tags)

class TestUnifiedKnowledgeCapture(unittest.TestCase):
    """Test the main capture manager"""
    
    def setUp(self):
        # Create capture manager without database
        self.capture = UnifiedKnowledgeCapture(db_manager=None)
        
    def test_capture_without_db(self):
        """Test that capture gracefully handles no database"""
        result = self.capture.capture(
            tool_name="test_tool",
            model="test_model",
            prompt="How to sort a list?",
            response="Use the sorted() function in Python",
            metadata={}
        )
        
        self.assertIsNone(result, "Should return None without database")
        
    def test_capture_validation(self):
        """Test that capture validates content"""
        # Create a mock database manager
        class MockDB:
            def get_connection(self):
                return self
            def __enter__(self):
                return self
            def __exit__(self, *args):
                pass
            def cursor(self):
                return self
            def execute(self, *args):
                pass
            def fetchone(self):
                return [1]  # Mock knowledge ID
            def fetchall(self):
                return []
                
        self.capture.db = MockDB()
        
        # Test with too-short content
        result = self.capture.capture(
            tool_name="test",
            model="test",
            prompt="Test",
            response="Too short",
            metadata={}
        )
        
        self.assertIsNone(result, "Should reject short content")
        
    def test_metrics_collection(self):
        """Test metrics without database"""
        metrics = self.capture.get_metrics()
        self.assertEqual(metrics, {}, "Should return empty metrics without DB")
        
    def test_search_without_db(self):
        """Test search without database"""
        results = self.capture.search("test")
        self.assertEqual(results, [], "Should return empty results without DB")

class TestIntegrationScenarios(unittest.TestCase):
    """Test realistic usage scenarios"""
    
    def test_code_explanation_scenario(self):
        """Test capturing a code explanation"""
        gate = QualityGate()
        categorizer = KnowledgeCategorizer()
        
        content = '''
        The Observer pattern in Python can be implemented like this:
        
        ```python
        class Subject:
            def __init__(self):
                self._observers = []
                
            def attach(self, observer):
                self._observers.append(observer)
                
            def notify(self, message):
                for observer in self._observers:
                    observer.update(message)
                    
        class Observer:
            def update(self, message):
                print(f"Received: {message}")
        ```
        
        This pattern is useful for implementing event systems where multiple
        objects need to be notified of state changes. For example, in a GUI
        application, multiple widgets might need to update when data changes.
        '''
        
        # Test quality validation
        is_valid, reason, metrics = gate.validate(content, {"tool": "pattern_explainer"})
        self.assertTrue(is_valid)
        self.assertGreater(metrics.overall_score, 70)
        
        # Test categorization
        category, tags = categorizer.categorize(content, {"tool": "pattern_explainer"})
        self.assertEqual(category, "code_pattern")
        self.assertIn("python", tags)
        
    def test_error_solution_scenario(self):
        """Test capturing an error solution"""
        gate = QualityGate()
        categorizer = KnowledgeCategorizer()
        
        content = '''
        If you're getting "RuntimeError: CUDA out of memory", try these solutions:
        
        1. Reduce batch size in your training loop
        2. Clear the GPU cache: torch.cuda.empty_cache()
        3. Use gradient accumulation for larger effective batch sizes
        4. Enable mixed precision training with torch.cuda.amp
        
        Example of gradient accumulation:
        ```python
        accumulation_steps = 4
        for i, batch in enumerate(dataloader):
            loss = model(batch) / accumulation_steps
            loss.backward()
            
            if (i + 1) % accumulation_steps == 0:
                optimizer.step()
                optimizer.zero_grad()
        ```
        '''
        
        # Test quality validation
        is_valid, reason, metrics = gate.validate(content, {"tool": "error_advisor"})
        self.assertTrue(is_valid)
        self.assertGreater(metrics.overall_score, 60)
        
        # Test categorization
        category, tags = categorizer.categorize(content, {"tool": "error_advisor"})
        self.assertEqual(category, "error_solution")
        self.assertIn("runtime", tags)

def run_all_tests():
    """Run all tests and print summary"""
    print("🧪 Running Knowledge Capture System Tests")
    print("=" * 60)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add all test classes
    test_classes = [
        TestQualityMetrics,
        TestQualityGate,
        TestKnowledgeCategorizer,
        TestUnifiedKnowledgeCapture,
        TestIntegrationScenarios
    ]
    
    for test_class in test_classes:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 60)
    print("📊 Test Summary:")
    print(f"   Tests run: {result.testsRun}")
    print(f"   Failures: {len(result.failures)}")
    print(f"   Errors: {len(result.errors)}")
    
    if result.wasSuccessful():
        print("\n✅ All tests passed! Knowledge Capture System is working correctly.")
    else:
        print("\n❌ Some tests failed. Please review the errors above.")
        
    return result.wasSuccessful()

if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)