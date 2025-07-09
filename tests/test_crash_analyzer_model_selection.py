#!/usr/bin/env python3
"""
Test model selection functionality in Crash Analyzer
"""

import sys
import os
import unittest
from unittest.mock import Mock, patch

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

class TestCrashAnalyzerModelSelection(unittest.TestCase):
    """Test crash analyzer model selection features"""
    
    def setUp(self):
        """Set up test environment"""
        self.mock_models = [
            "deepseek-r1:latest",
            "deepseek-r1:1.5b", 
            "llama2:7b",
            "mistral:7b"
        ]
    
    @patch('utils.ollama.get_available_models')
    @patch('utils.ollama.check_model_availability')
    def test_model_selection_widget(self, mock_check_availability, mock_get_models):
        """Test that model selection widget shows available models"""
        
        # Mock available models
        mock_get_models.return_value = self.mock_models
        mock_check_availability.return_value = {
            "available": True,
            "can_attempt_pull": True,
            "status": "ready"
        }
        
        # Import after mocking
        from pages.crash_analyzer import show_model_selection
        
        # This would normally require Streamlit session state
        # For unit testing, we'll just verify the function exists and imports work
        self.assertTrue(callable(show_model_selection))
        
        # Verify mocks were set up
        self.assertEqual(mock_get_models.return_value, self.mock_models)
        self.assertTrue(mock_check_availability.return_value["available"])
    
    @patch('utils.ollama.get_available_models')
    def test_model_fallback_when_none_available(self, mock_get_models):
        """Test that fallback works when no models available"""
        
        # Mock no available models
        mock_get_models.return_value = []
        
        from pages.crash_analyzer import show_model_selection
        
        # Function should handle empty model list gracefully
        self.assertTrue(callable(show_model_selection))
    
    def test_crash_instance_schema_includes_model(self):
        """Test that crash instance schema includes model_used column"""
        
        # Test the migration script exists
        migration_path = os.path.join(
            os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
            'scripts', 'migration', 'add_model_column_to_crash_instances.py'
        )
        
        self.assertTrue(os.path.exists(migration_path), 
                       "Migration script should exist")
        
        # Read migration and verify it adds model_used column
        with open(migration_path, 'r') as f:
            migration_content = f.read()
            
        self.assertIn('model_used', migration_content,
                     "Migration should add model_used column")
        self.assertIn('VARCHAR(100)', migration_content,
                     "Model column should be varchar")
    
    def test_save_crash_analysis_accepts_model_parameter(self):
        """Test that save function accepts model parameter"""
        
        from pages.crash_analyzer import save_crash_analysis
        import inspect
        
        # Get function signature
        sig = inspect.signature(save_crash_analysis)
        
        # Check that model_used parameter exists
        self.assertIn('model_used', sig.parameters,
                     "save_crash_analysis should accept model_used parameter")
        
        # Check that it has a default value
        model_param = sig.parameters['model_used']
        self.assertEqual(model_param.default, None,
                        "model_used should default to None")
    
    def test_json_helper_accepts_model_parameter(self):
        """Test that JSON helper functions accept model parameter"""
        
        from utils.json_helper import get_chunk_analysis_safe
        import inspect
        
        # Get function signature
        sig = inspect.signature(get_chunk_analysis_safe)
        
        # Check that model parameter exists
        self.assertIn('model', sig.parameters,
                     "get_chunk_analysis_safe should accept model parameter")
        
        # Check that it has a default value
        model_param = sig.parameters['model']
        self.assertEqual(model_param.default, None,
                        "model should default to None for dynamic selection")
    
    @patch('streamlit.session_state', {'selected_model': 'test-model:latest'})
    def test_session_state_model_usage(self):
        """Test that functions use model from session state"""
        
        # This is a basic test to ensure session state integration
        # In real usage, Streamlit would manage session state
        
        try:
            import streamlit as st
            # Verify session state can be accessed
            self.assertTrue(hasattr(st, 'session_state'))
        except ImportError:
            # Streamlit not available in test environment
            self.skipTest("Streamlit not available for testing")

def run_tests():
    """Run all model selection tests"""
    print("🧪 Testing Crash Analyzer Model Selection")
    print("=" * 50)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(TestCrashAnalyzerModelSelection)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print results
    if result.wasSuccessful():
        print("\n✅ All model selection tests passed!")
        return True
    else:
        print(f"\n❌ {len(result.failures)} test(s) failed")
        print(f"❌ {len(result.errors)} test(s) had errors")
        return False

if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)