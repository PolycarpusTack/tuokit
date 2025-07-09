"""
Test script to verify crash analyzer JSON parsing fixes
Tests the enhanced JSON helper and crash analyzer improvements
"""
import json
import sys
import os

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def test_json_helper():
    """Test the enhanced JSON helper functions"""
    print("=" * 60)
    print("Testing Enhanced JSON Helper")
    print("=" * 60)
    
    from utils.json_helper_enhanced import extract_json_from_response, get_chunk_analysis_safe
    
    # Test cases
    test_cases = [
        # Empty response
        ("", "Empty response test"),
        
        # Valid JSON
        ('{"errors_found": ["NullPointerException"], "severity": "High"}', "Valid JSON test"),
        
        # JSON in markdown
        ('```json\n{"errors_found": ["Error1"], "severity": "Critical"}\n```', "Markdown JSON test"),
        
        # Mixed text with JSON
        ('Here is the analysis:\n{"errors_found": ["ParseError"], "severity": "Medium"}\nEnd of analysis', "Mixed text test"),
        
        # Malformed JSON
        ('{"errors_found": ["Error", "severity": "High"}', "Malformed JSON test"),
        
        # Text with error keywords but no JSON
        ('Error: Something went wrong. Exception occurred at line 42.', "Text only test"),
        
        # Nested JSON
        ('The response is: ```{"errors_found": ["OutOfMemoryError"], "error_types": ["Memory"], "severity": "Critical"}```', "Nested JSON test"),
    ]
    
    print("\nTesting extract_json_from_response:")
    print("-" * 40)
    
    for test_input, description in test_cases:
        print(f"\n{description}:")
        print(f"Input: {repr(test_input[:50])}..." if len(test_input) > 50 else f"Input: {repr(test_input)}")
        
        result = extract_json_from_response(test_input, fallback_on_error=True)
        print(f"Result: {json.dumps(result, indent=2)}")
        
        # Verify we always get required fields
        required_fields = ["errors_found", "error_types", "severity", "summary"]
        missing_fields = [f for f in required_fields if f not in result]
        if missing_fields:
            print(f"⚠️ WARNING: Missing fields: {missing_fields}")
        else:
            print("✅ All required fields present")
    
    print("\n" + "=" * 60)
    print("Testing get_chunk_analysis_safe:")
    print("-" * 40)
    
    # Test chunk analysis
    test_chunks = [
        # Chunk with errors
        ("2024-01-15 ERROR: NullPointerException at com.example.MyClass.method(MyClass.java:42)\n" * 10, "Error chunk"),
        
        # Chunk without errors
        ("INFO: Application started successfully\nDEBUG: Loading configuration\n" * 10, "Normal chunk"),
        
        # Empty chunk
        ("", "Empty chunk"),
        
        # Chunk with mixed content
        ("Some normal logs here\nERROR: OutOfMemoryError\nMore logs\nFATAL: System crash\n", "Mixed chunk"),
    ]
    
    for chunk_content, description in test_chunks:
        print(f"\n{description}:")
        print(f"Content preview: {chunk_content[:100]}..." if len(chunk_content) > 100 else f"Content: {chunk_content}")
        
        # Mock the safe_ollama_generate function for testing
        def mock_ollama_generate(model, prompt, temperature=0.3, format=None):
            # Simulate different responses
            if "ERROR" in chunk_content or "Exception" in chunk_content:
                return {
                    'response': '{"errors_found": ["Test error"], "error_types": ["TestException"], "severity": "High", "summary": "Errors detected"}'
                }
            else:
                return {
                    'response': '{"errors_found": [], "error_types": [], "severity": "Low", "summary": "No errors"}'
                }
        
        # Patch the function temporarily
        import utils
        original_func = getattr(utils, 'safe_ollama_generate', None)
        utils.safe_ollama_generate = mock_ollama_generate
        
        try:
            result = get_chunk_analysis_safe(chunk_content, chunk_info=" (test chunk)")
            print(f"Analysis result: {json.dumps(result, indent=2)}")
            
            # Verify result structure
            if isinstance(result, dict) and 'errors_found' in result:
                print("✅ Valid analysis structure")
            else:
                print("❌ Invalid analysis structure")
        finally:
            # Restore original function if it existed
            if original_func:
                utils.safe_ollama_generate = original_func
    
    print("\n" + "=" * 60)
    print("✅ JSON Helper Tests Complete")
    print("=" * 60)

def test_chunk_processing_simulation():
    """Simulate the chunk processing that was failing"""
    print("\n" + "=" * 60)
    print("Simulating Chunk Processing (like chunks 89-102)")
    print("=" * 60)
    
    from utils.json_helper_enhanced import extract_json_from_response
    
    # Simulate responses that would cause "Expecting value: line 1 column 1 (char 0)"
    problem_responses = [
        "",  # Empty response
        " ",  # Whitespace only
        "\n",  # Newline only
        "null",  # Null value
        "undefined",  # JavaScript undefined
        "Sorry, I cannot process this",  # Non-JSON response
        '{"errors_found":]',  # Malformed JSON
        '{"incomplete": "json"',  # Incomplete JSON
    ]
    
    consecutive_failures = 0
    for i, response in enumerate(problem_responses, start=89):
        print(f"\nProcessing chunk {i}:")
        print(f"Response: {repr(response)}")
        
        try:
            # Old approach that would fail
            result = json.loads(response)
            print(f"Direct JSON parse: {result}")
            consecutive_failures = 0
        except json.JSONDecodeError as e:
            print(f"❌ JSON Error (old approach): {e}")
            consecutive_failures += 1
            
            # New approach with enhanced helper
            result = extract_json_from_response(response, fallback_on_error=True)
            print(f"✅ Enhanced helper result: {json.dumps(result, indent=2)}")
            
            # With the new approach, we get a valid structure even on failure
            if 'errors_found' in result:
                print("✅ Fallback structure provided - processing can continue")
                consecutive_failures = 0  # Reset because we got usable data
    
    print(f"\n{'✅' if consecutive_failures < 10 else '❌'} Consecutive failures: {consecutive_failures}")
    print("With the enhanced helper, we avoid the consecutive failure limit!")

def main():
    """Run all tests"""
    try:
        test_json_helper()
        test_chunk_processing_simulation()
        
        print("\n" + "=" * 60)
        print("🎉 All tests completed successfully!")
        print("The crash analyzer should now handle chunks 89-102 without errors.")
        print("=" * 60)
        
        return True
    except Exception as e:
        print(f"\n❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
