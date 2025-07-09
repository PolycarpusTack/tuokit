"""
Test script to verify the crash analyzer format parameter fix
"""
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils import safe_ollama_generate

def test_json_format():
    """Test that safe_ollama_generate works with format parameter"""
    print("Testing safe_ollama_generate with format='json'...")
    
    prompt = """
    Analyze this error and respond in JSON format:
    {
        "error_type": "the type of error",
        "severity": "High/Medium/Low"
    }
    
    Error: NullPointerException at line 42
    """
    
    result = safe_ollama_generate(
        model="deepseek-r1:8b",  # Use a model that's available
        prompt=prompt,
        format="json"
    )
    
    print(f"Response received: {result.get('error', 'Success')}")
    if not result.get('error'):
        print(f"Response text: {result.get('response', '')[:200]}...")
    
    return not result.get('error')

if __name__ == "__main__":
    try:
        success = test_json_format()
        if success:
            print("\n✅ Format parameter fix successful!")
        else:
            print("\n❌ Format parameter fix failed!")
    except Exception as e:
        print(f"\n❌ Error: {e}")
