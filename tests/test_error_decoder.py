# tests/test_error_decoder.py
"""Tests for the refactored error decoder toolkit"""

import pytest
from toolkits.error_decoder import ErrorDecoder
from toolkits.error_decoder.parsers import parse_error_message
from toolkits.error_decoder.processors import analyze_error
from toolkits.error_decoder.config import ERROR_PATTERNS, EXAMPLE_ERRORS

class TestErrorDecoder:
    """Test the error decoder functionality"""
    
    def test_error_decoder_init(self):
        """Test ErrorDecoder initialization"""
        decoder = ErrorDecoder()
        assert decoder.tool_name == "Advanced Error Decoder"
        assert decoder.tool_description == "Professional debugging with deep educational insights and code solutions"
    
    def test_parse_python_error(self):
        """Test parsing Python errors"""
        error = '''Traceback (most recent call last):
  File "app.py", line 42, in <module>
    result = calculate(10, 0)
ZeroDivisionError: division by zero'''
        
        parsed = parse_error_message(error)
        assert parsed['language'] == 'python'
        assert parsed['error_type'] == 'ZeroDivisionError'
        assert parsed['message'] == 'division by zero'
        assert parsed['file'] == 'app.py'
        assert parsed['line'] == 42
    
    def test_parse_ruby_error(self):
        """Test parsing Ruby errors"""
        error = "app.rb:10:in `divide': divided by 0 (ZeroDivisionError)"
        
        parsed = parse_error_message(error)
        assert parsed['language'] == 'ruby'
        assert parsed['error_type'] == 'ZeroDivisionError'
        assert parsed['file'] == 'app.rb'
        assert parsed['line'] == 10
        assert parsed['context'] == 'divide'
    
    def test_parse_smalltalk_error(self):
        """Test parsing SmallTalk errors"""
        error = '''[Process 1234]: 2024-01-15 10:30:00 MessageNotUnderstood Error: Array>>doesNotExist
Receiver: #(1 2 3)
Arguments: ()
Stack trace here'''
        
        parsed = parse_error_message(error)
        assert parsed['language'] == 'smalltalk'
        assert parsed['error_type'] == 'Array>>doesNotExist'
        assert parsed['process'] == 'Process 1234'
        assert parsed['receiver'] == '#(1 2 3)'
    
    def test_parse_generic_error(self):
        """Test parsing generic errors"""
        error = "ValueError: invalid literal for int()"
        
        parsed = parse_error_message(error)
        assert parsed['error_type'] == 'ValueError'
        assert parsed['message'] == 'invalid literal for int()'
    
    def test_error_patterns_exist(self):
        """Test that error patterns are defined"""
        assert len(ERROR_PATTERNS) > 0
        assert 'python' in ERROR_PATTERNS
        assert 'ruby' in ERROR_PATTERNS
        assert 'smalltalk' in ERROR_PATTERNS
    
    def test_example_errors_exist(self):
        """Test that example errors are defined"""
        assert len(EXAMPLE_ERRORS) > 0
        assert 'SmallTalk' in EXAMPLE_ERRORS
        assert 'Ruby' in EXAMPLE_ERRORS
        assert 'default' in EXAMPLE_ERRORS
    
    def test_analyze_error_mock(self):
        """Test analyze_error with mocked response"""
        # This would normally call Ollama, but we can test the function exists
        error_data = {
            'language': 'python',
            'error_type': 'ValueError',
            'message': 'invalid literal',
            'file': 'test.py',
            'line': 10
        }
        
        # Just verify the function can be called
        # In a real test, we'd mock the Ollama response
        assert callable(analyze_error)