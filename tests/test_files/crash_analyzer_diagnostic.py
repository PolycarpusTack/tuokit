"""
Quick diagnostic script for crash analyzer issues
Run this if you encounter any problems
"""
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def check_imports():
    """Check if all required imports work"""
    print("Checking imports...")
    try:
        import streamlit as st
        print("✅ Streamlit imported")
    except ImportError as e:
        print(f"❌ Streamlit import failed: {e}")
        return False
    
    try:
        from utils import safe_ollama_generate
        print("✅ safe_ollama_generate imported")
    except ImportError as e:
        print(f"❌ safe_ollama_generate import failed: {e}")
        return False
    
    try:
        from utils.json_helper import extract_json_from_response
        print("✅ json_helper imported")
    except ImportError as e:
        print(f"❌ json_helper import failed: {e}")
        return False
    
    return True

def check_ollama_format():
    """Check if ollama supports format parameter"""
    print("\nChecking Ollama format support...")
    try:
        from utils import safe_ollama_generate
        
        # Test with a simple prompt
        result = safe_ollama_generate(
            model="deepseek-r1:8b",
            prompt='Respond with JSON: {"test": "value"}',
            format="json"
        )
        
        if result.get('error'):
            print(f"⚠️ Ollama returned error: {result.get('response')}")
            print("   The model might not support JSON format")
        else:
            print("✅ Ollama format parameter works")
            
    except Exception as e:
        print(f"❌ Error testing format: {e}")

def check_json_parsing():
    """Test JSON parsing helpers"""
    print("\nChecking JSON parsing...")
    try:
        from utils.json_helper import extract_json_from_response
        
        # Test cases
        test_responses = [
            '{"valid": "json"}',
            'Some text before {"embedded": "json"} and after',
            'No JSON here at all',
            ''
        ]
        
        for i, response in enumerate(test_responses):
            result = extract_json_from_response(response)
            if "error" not in result or i < 2:
                print(f"✅ Test {i+1}: Extracted {result}")
            else:
                print(f"✅ Test {i+1}: Correctly handled non-JSON")
                
    except Exception as e:
        print(f"❌ JSON parsing test failed: {e}")

def main():
    print("=" * 50)
    print("Crash Analyzer Diagnostic Tool")
    print("=" * 50)
    
    # Run checks
    if not check_imports():
        print("\n❌ Import issues detected. Check your environment.")
        return
    
    check_ollama_format()
    check_json_parsing()
    
    print("\n" + "=" * 50)
    print("Diagnostic complete!")
    print("\nIf you see warnings:")
    print("1. The model might not support JSON format")
    print("2. Try using a different model")
    print("3. The fallback mechanisms will handle it")
    print("=" * 50)

if __name__ == "__main__":
    main()
