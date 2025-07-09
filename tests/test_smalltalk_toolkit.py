# tests/test_smalltalk_toolkit.py
"""Tests for the refactored SmallTalk toolkit"""

import pytest
from toolkits.smalltalk import SmallTalkToolkit
from toolkits.smalltalk.processors import extract_class_info
from toolkits.smalltalk.config import SNIPPET_CATEGORIES, REFACTORING_TECHNIQUES

class TestSmallTalkToolkit:
    """Test the SmallTalk toolkit functionality"""
    
    def test_toolkit_init(self):
        """Test toolkit initialization"""
        toolkit = SmallTalkToolkit()
        # OllamaToolBase sets tool_name to "smalltalk_toolkit"
        # TuoKitToolBase attributes should be checked differently
        assert hasattr(toolkit, 'tool_name')
        assert hasattr(toolkit, 'tool_description')
        assert toolkit.tool_description == "Comprehensive suite for SmallTalk development"
    
    def test_extract_class_info(self):
        """Test class info extraction"""
        code = """
Object subclass: #Point
    instanceVariableNames: 'x y'
    classVariableNames: 'Origin'
    poolDictionaries: ''
    category: 'Kernel-BasicObjects'

Point >> + aPoint
    "Add two points"
    ^(x + aPoint x) @ (y + aPoint y)

Point >> x
    ^x
"""
        
        info = extract_class_info(code)
        
        # Check class extraction
        assert len(info['classes']) == 1
        assert info['classes'][0]['name'] == 'Point'
        assert info['classes'][0]['superclass'] == 'Object'
        
        # Check instance variables
        assert 'x' in info['instance_vars']
        assert 'y' in info['instance_vars']
        
        # Check class variables
        assert 'Origin' in info['class_vars']
        
        # Check methods
        assert len(info['methods']) == 2
        method_selectors = [m['selector'] for m in info['methods']]
        assert '+ aPoint' in method_selectors
        assert 'x' in method_selectors
    
    def test_snippet_categories(self):
        """Test snippet categories are defined"""
        assert len(SNIPPET_CATEGORIES) > 0
        assert "Collections & Iteration" in SNIPPET_CATEGORIES
        assert "GUI Development (MVC)" in SNIPPET_CATEGORIES
        assert "Testing & Debugging" in SNIPPET_CATEGORIES
        
        # Check structure
        for category, info in SNIPPET_CATEGORIES.items():
            assert 'icon' in info
            assert 'description' in info
            assert 'subcategories' in info
    
    def test_refactoring_techniques(self):
        """Test refactoring techniques are defined"""
        assert len(REFACTORING_TECHNIQUES) > 0
        assert "Extract Method" in REFACTORING_TECHNIQUES
        assert "Rename Variable" in REFACTORING_TECHNIQUES
        
        # Check structure
        for technique, info in REFACTORING_TECHNIQUES.items():
            assert 'description' in info
            assert 'example' in info
    
    def test_f_string_fix(self):
        """Test that the f-string syntax error is fixed"""
        # Import should not raise SyntaxError
        from toolkits.smalltalk.generators import generate_metaprogramming
        
        # Function should be callable
        assert callable(generate_metaprogramming)
    
    def test_tool_categories(self):
        """Test tool categories configuration"""
        from toolkits.smalltalk.config import TOOL_CATEGORIES
        
        expected_tools = [
            "Class Generator",
            "Code Explainer", 
            "Snippet Generator",
            "Code Refactorer",
            "Ruby Converter",
            "Seaside Generator",
            "Metaprogramming"
        ]
        
        for tool in expected_tools:
            assert tool in TOOL_CATEGORIES
            assert 'icon' in TOOL_CATEGORIES[tool]
            assert 'description' in TOOL_CATEGORIES[tool]
            assert 'key' in TOOL_CATEGORIES[tool]