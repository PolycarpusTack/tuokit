"""
Test script for Study Guide Generator
Tests basic functionality and error handling
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.file_handler import extract_text, extract_text_from_url, validate_file_size
from pages.study_guide_generator import parse_study_materials, generate_export_text

def test_parse_study_materials():
    """Test the parsing function with sample Ollama response"""
    sample_response = """
SUMMARY
- Key concept 1: Understanding variables
- Key concept 2: Control flow structures
- Key concept 3: Functions and modularity

FLASHCARDS
What is a variable? | A named storage location in memory
What is a loop? | A control structure that repeats code
What is a function? | A reusable block of code

QUIZ
1. What is the purpose of variables in programming?
A) To store data
B) To create loops
C) To define functions
D) To import modules
Correct: A

KEY TERMS
Variable: A named container for storing data values
Function: A self-contained block of code that performs a specific task
Loop: A programming construct that repeats a block of code
"""
    
    result = parse_study_materials(sample_response)
    
    print("Parsed Materials Test:")
    print(f"Summary items: {len(result['summary'])}")
    print(f"Flashcards: {len(result['flashcards'])}")
    print(f"Quiz questions: {len(result['quiz'])}")
    print(f"Key terms: {len(result['key_terms'])}")
    
    assert len(result['summary']) > 0, "Should parse summary items"
    assert len(result['flashcards']) > 0, "Should parse flashcards"
    assert len(result['quiz']) > 0, "Should parse quiz questions"
    assert len(result['key_terms']) > 0, "Should parse key terms"
    
    print("✅ Parsing test passed!")
    return result

def test_export_text():
    """Test the export text generation"""
    materials = {
        'summary': ['Test point 1', 'Test point 2'],
        'flashcards': [
            {'question': 'Q1?', 'answer': 'A1'},
            {'question': 'Q2?', 'answer': 'A2'}
        ],
        'quiz': [{
            'question': 'Test question?',
            'options': ['A) Option 1', 'B) Option 2'],
            'correct': 'A'
        }],
        'key_terms': [
            {'term': 'Term1', 'definition': 'Definition 1'}
        ]
    }
    
    export = generate_export_text(materials)
    print("\nExport Test:")
    print(f"Export length: {len(export)} characters")
    print("First 200 chars:")
    print(export[:200])
    
    assert 'STUDY GUIDE' in export, "Should contain header"
    assert 'SUMMARY' in export, "Should contain summary section"
    assert 'FLASHCARDS' in export, "Should contain flashcards section"
    
    print("✅ Export test passed!")

if __name__ == "__main__":
    print("Testing Study Guide Generator Components...\n")
    
    # Test parsing
    parsed = test_parse_study_materials()
    
    # Test export
    test_export_text()
    
    print("\n✅ All tests passed!")
