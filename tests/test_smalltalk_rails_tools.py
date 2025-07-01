"""
Test script for SmallTalk & Rails development tools
Run this to verify all tools are working correctly
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.ollama import OllamaManager, safe_ollama_generate
from utils.database import DatabaseManager

def test_ollama_connection():
    """Test Ollama service connection"""
    print("Testing Ollama connection...")
    status = OllamaManager.get_status()
    if status["running"]:
        print("✅ Ollama is running")
        print(f"   Models available: {status['model_count']}")
        return True
    else:
        print("❌ Ollama is not running")
        print("   Run: ollama serve")
        return False

def test_models():
    """Test required models"""
    print("\nChecking required models...")
    required_models = ["deepseek-coder:6.7b", "deepseek-r1:6.7b"]
    available_models = OllamaManager.list_models()
    
    for model in required_models:
        if model in available_models:
            print(f"✅ {model} is available")
        else:
            print(f"❌ {model} is missing")
            print(f"   Run: ollama pull {model}")
    
    return all(model in available_models for model in required_models)

def test_generation():
    """Test AI generation"""
    print("\nTesting AI generation...")
    
    test_prompt = "Write a simple SmallTalk method that returns 'Hello World'"
    result = safe_ollama_generate(
        model="deepseek-coder:6.7b",
        prompt=test_prompt,
        temperature=0.1
    )
    
    if not result["error"]:
        print("✅ Generation successful")
        print(f"   Response length: {len(result['response'])} chars")
        return True
    else:
        print("❌ Generation failed")
        print(f"   Error: {result['response']}")
        return False

def test_database():
    """Test database connection"""
    print("\nTesting database connection...")
    
    try:
        db = DatabaseManager()
        if db.connected:
            print("✅ Database connected")
            count = db.get_knowledge_count()
            print(f"   Knowledge units: {count}")
            return True
        else:
            print("❌ Database connection failed")
            return False
    except Exception as e:
        print(f"❌ Database error: {e}")
        return False

def test_smalltalk_tools():
    """Quick test of SmallTalk tools"""
    print("\nTesting SmallTalk tools...")
    
    # Import and test each tool
    try:
        from pages.smalltalk_explainer import SmallTalkExplainer
        explainer = SmallTalkExplainer()
        print("✅ SmallTalk Explainer loaded")
        
        from pages.smalltalk_snippets import SmallTalkSnippetFinder
        finder = SmallTalkSnippetFinder()
        print("✅ SmallTalk Snippet Finder loaded")
        
        from pages.smalltalk_ruby_converter import CodeConverter
        converter = CodeConverter()
        print("✅ SmallTalk-Ruby Converter loaded")
        
        return True
    except Exception as e:
        print(f"❌ Error loading tools: {e}")
        return False

def test_rails_tools():
    """Quick test of Rails tools"""
    print("\nTesting Rails tools...")
    
    try:
        from pages.rails_scaffold import RailsScaffoldGenerator
        generator = RailsScaffoldGenerator()
        print("✅ Rails Scaffold Generator loaded")
        
        from pages.rails_debugger import RailsDebugger
        debugger = RailsDebugger()
        print("✅ Rails Debugger loaded")
        
        return True
    except Exception as e:
        print(f"❌ Error loading tools: {e}")
        return False

def main():
    """Run all tests"""
    print("=== SmallTalk & Rails Tools Test Suite ===\n")
    
    tests = [
        ("Ollama Connection", test_ollama_connection),
        ("Required Models", test_models),
        ("AI Generation", test_generation),
        ("Database Connection", test_database),
        ("SmallTalk Tools", test_smalltalk_tools),
        ("Rails Tools", test_rails_tools)
    ]
    
    results = []
    for name, test_func in tests:
        try:
            result = test_func()
            results.append((name, result))
        except Exception as e:
            print(f"❌ {name} test crashed: {e}")
            results.append((name, False))
    
    print("\n=== Test Summary ===")
    passed = sum(1 for _, result in results if result)
    total = len(results)
    
    for name, result in results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{status} - {name}")
    
    print(f"\nTotal: {passed}/{total} tests passed")
    
    if passed == total:
        print("\n🎉 All tests passed! The SmallTalk & Rails tools are ready to use.")
    else:
        print("\n⚠️  Some tests failed. Please fix the issues above before using the tools.")

if __name__ == "__main__":
    main()
