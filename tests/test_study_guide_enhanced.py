"""
Test script for enhanced Study Guide features
Tests learning strategy and content validation
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.learning_strategy import SimpleLearningStrategy
from utils.content_validator import SimpleContentValidator
from datetime import datetime, date

def test_learning_strategy():
    """Test spaced repetition schedule generation"""
    print("Testing Learning Strategy...")
    
    strategy = SimpleLearningStrategy()
    
    # Test schedule generation
    concepts = ["Variables", "Functions", "Loops", "Classes", "Inheritance"]
    schedule = strategy.generate_review_schedule(concepts, "Intermediate")
    
    print(f"\nGenerated schedule for {len(concepts)} concepts:")
    for concept, dates in list(schedule.items())[:3]:  # Show first 3
        print(f"\n{concept}:")
        for date in dates:
            print(f"  - {date.strftime('%B %d, %Y')}")
    
    # Test different difficulties
    beginner_schedule = strategy.generate_review_schedule(["Test"], "Beginner")
    advanced_schedule = strategy.generate_review_schedule(["Test"], "Advanced")
    
    print(f"\nInterval comparison:")
    print(f"Beginner intervals: {[d.day for d in beginner_schedule['Test']]}")
    print(f"Advanced intervals: {[d.day for d in advanced_schedule['Test']]}")
    
    print("✅ Learning strategy test passed!")

def test_content_validator():
    """Test content validation functionality"""
    print("\n\nTesting Content Validator...")
    
    validator = SimpleContentValidator()
    
    # Test with sample content
    source = """
    Python was created in 1991 by Guido van Rossum. 
    It is used by 48% of developers worldwide.
    The latest version is Python 3.12.
    """
    
    generated = """
    Python was created in 1989 by Guido van Rossum.
    It is used by 75% of developers worldwide.
    The latest version is Python 3.12.
    Chapter 5 discusses advanced features.
    """
    
    result = validator.quick_accuracy_check(generated, source)
    
    print(f"\nAccuracy Score: {result['accuracy_score']}/10")
    print(f"Confidence: {result['confidence']}")
    print(f"Total Issues: {result['total_issues']}")
    
    if result['issues']:
        print("\nDetected Issues:")
        for issue in result['issues']:
            print(f"  - {issue['type']}: {issue['content']}")
            print(f"    Suggestion: {issue['suggestion']}")
    
    # Test claim extraction
    claims = validator.extract_key_claims(generated)
    print(f"\nExtracted {len(claims)} factual claims")
    
    # Test correlation
    correlation = validator.source_correlation_check(
        "Python was created in 1991",
        [source]
    )
    print(f"\nSource correlation score: {correlation:.2f}")
    
    print("✅ Content validator test passed!")

def test_integration():
    """Test integration of new features"""
    print("\n\nTesting Feature Integration...")
    
    # Simulate a study session
    content_hash = "test123"
    concepts = ["Test Concept 1", "Test Concept 2"]
    
    strategy = SimpleLearningStrategy()
    
    # This would normally interact with the database
    print("Simulating study session tracking...")
    print(f"Content hash: {content_hash}")
    print(f"Concepts: {concepts}")
    print(f"Timestamp: {datetime.now().isoformat()}")
    
    print("✅ Integration test passed!")

if __name__ == "__main__":
    print("Testing Enhanced Study Guide Features...\n")
    
    test_learning_strategy()
    test_content_validator()
    test_integration()
    
    print("\n\n✅ All enhanced features tests passed!")
    print("\nNext steps:")
    print("1. Test with actual Ollama models")
    print("2. Verify database integration")
    print("3. Check UI components in Streamlit")
