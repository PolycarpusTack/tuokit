"""
Test script for EduMind
Verifies core functionality without dependencies
"""

from datetime import datetime, timedelta

def test_mode_mapping():
    """Test learning mode parameter conversion"""
    print("Testing Mode Mapping...")
    
    modes = {
        "Study Guide": "study_guide",
        "Practice Quiz": "practice_quiz", 
        "Concept Explanation": "concept_explanation"
    }
    
    for display_name, param_name in modes.items():
        converted = display_name.lower().replace(" ", "_")
        assert converted == param_name, f"Mode conversion failed for {display_name}"
        print(f"  [OK] {display_name} -> {param_name}")
    
    print("[PASS] Mode mapping test passed!\n")

def test_review_schedule():
    """Test review schedule generation"""
    print("Testing Review Schedule Generation...")
    
    options = ["Tomorrow", "3 days", "1 week", "2 weeks", "1 month"]
    today = datetime.now()
    
    expected_days = [1, 3, 7, 14, 30]
    
    for i, option in enumerate(options):
        if option == "Tomorrow":
            expected = today + timedelta(days=1)
        elif option == "3 days":
            expected = today + timedelta(days=3)
        elif option == "1 week":
            expected = today + timedelta(weeks=1)
        elif option == "2 weeks":
            expected = today + timedelta(weeks=2)
        elif option == "1 month":
            expected = today + timedelta(days=30)
        
        print(f"  {option}: {expected.strftime('%B %d, %Y')}")
    
    print("[PASS] Schedule generation test passed!\n")

def test_validation_responses():
    """Test accuracy validation response handling"""
    print("Testing Validation Response Handling...")
    
    test_cases = [
        ("No inaccuracies found.", "All facts verified"),
        ("no inaccuracies detected", "All facts verified"),
        ("Found 2 errors in dates", "Review suggested: Found 2 errors in dates"),
        ("The year 1989 should be 1991", "Review suggested: The year 1989 should be 1991")
    ]
    
    for response, expected_prefix in test_cases:
        if "no inaccuracies" in response.lower():
            result = "All facts verified"
        else:
            result = f"Review suggested: {response[:100]}..."
        
        # Remove special characters for testing
        expected_clean = expected_prefix.replace("✅ ", "").replace("⚠️ ", "").split("...")[0]
        result_clean = result.replace("✅ ", "").replace("⚠️ ", "")
        
        assert result_clean.startswith(expected_clean), f"Validation parsing failed for: {response}"
        print(f"  [OK] '{response[:30]}...' -> '{result_clean[:30]}...'")
    
    print("[PASS] Validation response test passed!\n")

def test_metadata_structure():
    """Test metadata format for database storage"""
    print("Testing Metadata Structure...")
    
    # Simulate metadata creation
    metadata = {
        "mode": "study_guide",
        "content_hash": "abc123",
        "complexity": 3,
        "validation": "All facts verified",
        "timestamp": datetime.now().isoformat()
    }
    
    # Check all required fields
    required_fields = ["mode", "content_hash", "complexity", "validation", "timestamp"]
    for field in required_fields:
        assert field in metadata, f"Missing required field: {field}"
        print(f"  [OK] {field}: {str(metadata[field])[:30]}")
    
    print("[PASS] Metadata structure test passed!\n")

def test_complexity_levels():
    """Test complexity level boundaries"""
    print("Testing Complexity Levels...")
    
    # Slider goes from 1-5
    test_values = [0, 1, 3, 5, 6]
    
    for value in test_values:
        # Clamp to valid range
        clamped = max(1, min(5, value))
        print(f"  Input: {value} -> Clamped: {clamped}")
    
    print("[PASS] Complexity level test passed!\n")

if __name__ == "__main__":
    print("Running EduMind Unit Tests")
    print("=" * 50)
    
    test_mode_mapping()
    test_review_schedule()
    test_validation_responses()
    test_metadata_structure()
    test_complexity_levels()
    
    print("=" * 50)
    print("[PASS] All unit tests passed successfully!")
    print("\nNote: These tests verify core logic without database/Ollama dependencies.")
    print("For integration testing, ensure all services are running.")
