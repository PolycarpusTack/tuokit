"""
Test script for new SmallTalk development tools
Verifies all 6 new tools are working correctly
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.ollama import OllamaManager

def test_new_smalltalk_tools():
    """Test all 6 new SmallTalk tools"""
    print("=== Testing New SmallTalk Tools ===\n")
    
    tools_to_test = [
        ("SmallTalk Class Generator", "smalltalk_class_gen", "SmallTalkClassGenerator"),
        ("Morphic UI Builder", "morphic_builder", "MorphicUIBuilder"),
        ("Seaside Component Generator", "seaside_generator", "SeasideComponentGenerator"),
        ("SmallTalk Refactoring Assistant", "smalltalk_refactorer", "SmallTalkRefactorer"),
        ("SmallTalk Metaprogramming Helper", "smalltalk_meta", "SmallTalkMetaprogrammingHelper"),
        ("SmallTalk Image Browser", "image_browser", "SmallTalkImageBrowser")
    ]
    
    results = {}
    
    # Test 1: Import all tools
    print("1. Testing tool imports...")
    for tool_name, module_name, class_name in tools_to_test:
        try:
            module = __import__(f"pages.{module_name}", fromlist=[class_name])
            tool_class = getattr(module, class_name)
            print(f"✅ {tool_name}: Import successful")
            results[tool_name] = {"import": True}
            
            # Test instantiation
            try:
                instance = tool_class()
                results[tool_name]["instantiate"] = True
                print(f"   ✓ Instantiation successful")
            except Exception as e:
                results[tool_name]["instantiate"] = False
                print(f"   ✗ Instantiation failed: {e}")
                
        except Exception as e:
            print(f"❌ {tool_name}: Import failed - {e}")
            results[tool_name] = {"import": False}
    
    # Test 2: Check page functions
    print("\n2. Testing page display functions...")
    for tool_name, module_name, _ in tools_to_test:
        try:
            module = __import__(f"pages.{module_name}", fromlist=["show"])
            if hasattr(module, "show"):
                print(f"✅ {tool_name}: show() function found")
                results[tool_name]["show_function"] = True
            else:
                print(f"❌ {tool_name}: show() function missing")
                results[tool_name]["show_function"] = False
        except Exception as e:
            print(f"❌ {tool_name}: Error checking show() - {e}")
            results[tool_name]["show_function"] = False
    
    # Test 3: Verify key methods
    print("\n3. Testing key methods...")
    method_tests = {
        "SmallTalk Class Generator": ("generate_class", ["description"]),
        "Morphic UI Builder": ("generate_morphic_ui", ["description"]),
        "Seaside Component Generator": ("generate_seaside_component", ["description"]),
        "SmallTalk Refactoring Assistant": ("refactor_code", ["code", "technique"]),
        "SmallTalk Metaprogramming Helper": ("generate_metaprogramming", ["code", "task"]),
        "SmallTalk Image Browser": ("query_image", ["query"])
    }
    
    for tool_name, (method_name, expected_params) in method_tests.items():
        if tool_name in results and results[tool_name].get("instantiate"):
            module_name = next(m for n, m, _ in tools_to_test if n == tool_name)
            class_name = next(c for n, _, c in tools_to_test if n == tool_name)
            
            try:
                module = __import__(f"pages.{module_name}", fromlist=[class_name])
                tool_class = getattr(module, class_name)
                instance = tool_class()
                
                if hasattr(instance, method_name):
                    method = getattr(instance, method_name)
                    # Check method parameters
                    import inspect
                    sig = inspect.signature(method)
                    params = list(sig.parameters.keys())
                    params.remove('self')  # Remove self parameter
                    
                    # Check if expected params are in the method signature
                    has_params = all(p in params for p in expected_params)
                    
                    if has_params:
                        print(f"✅ {tool_name}: {method_name}() verified")
                        results[tool_name]["key_method"] = True
                    else:
                        print(f"❌ {tool_name}: {method_name}() missing parameters")
                        results[tool_name]["key_method"] = False
                else:
                    print(f"❌ {tool_name}: {method_name}() not found")
                    results[tool_name]["key_method"] = False
                    
            except Exception as e:
                print(f"❌ {tool_name}: Method test failed - {e}")
                results[tool_name]["key_method"] = False
    
    # Test 4: Check Ollama integration
    print("\n4. Checking Ollama integration...")
    ollama_status = OllamaManager.get_status()
    if ollama_status["running"]:
        print("✅ Ollama is running")
        print(f"   Models available: {ollama_status['model_count']}")
    else:
        print("⚠️ Ollama not running - generation features will fail")
    
    # Summary
    print("\n=== Test Summary ===")
    total_tools = len(tools_to_test)
    passed_tools = 0
    
    for tool_name, test_results in results.items():
        all_passed = all(test_results.values())
        if all_passed:
            passed_tools += 1
            print(f"✅ {tool_name}: All tests passed")
        else:
            print(f"❌ {tool_name}: Some tests failed")
            for test, passed in test_results.items():
                if not passed:
                    print(f"   - {test} failed")
    
    print(f"\nTotal: {passed_tools}/{total_tools} tools fully functional")
    
    if passed_tools == total_tools:
        print("\n🎉 All new SmallTalk tools are ready to use!")
    else:
        print("\n⚠️ Some tools need attention. Check the errors above.")
    
    return results

def test_tool_features():
    """Test specific features of each tool"""
    print("\n=== Testing Tool Features ===\n")
    
    # Test Class Generator features
    try:
        from pages.smalltalk_class_gen import SmallTalkClassGenerator
        gen = SmallTalkClassGenerator()
        
        # Test extract_class_info
        sample_code = """Object subclass: #TestClass
    instanceVariableNames: 'name age'
    classVariableNames: 'Population'"""
        
        info = gen.extract_class_info(sample_code)
        if info["class_name"] == "TestClass":
            print("✅ Class Generator: extract_class_info() works")
        else:
            print("❌ Class Generator: extract_class_info() failed")
    except Exception as e:
        print(f"❌ Class Generator feature test failed: {e}")
    
    # Test Refactorer techniques
    try:
        from pages.smalltalk_refactorer import SmallTalkRefactorer
        ref = SmallTalkRefactorer()
        
        if len(ref.refactoring_techniques) >= 10:
            print(f"✅ Refactorer: {len(ref.refactoring_techniques)} techniques available")
        else:
            print("❌ Refactorer: Missing refactoring techniques")
    except Exception as e:
        print(f"❌ Refactorer feature test failed: {e}")
    
    # Test Metaprogramming tasks
    try:
        from pages.smalltalk_meta import SmallTalkMetaprogrammingHelper
        meta = SmallTalkMetaprogrammingHelper()
        
        if len(meta.meta_tasks) >= 10:
            print(f"✅ Metaprogramming: {len(meta.meta_tasks)} tasks available")
        else:
            print("❌ Metaprogramming: Missing tasks")
    except Exception as e:
        print(f"❌ Metaprogramming feature test failed: {e}")
    
    # Test Image Browser queries
    try:
        from pages.image_browser import SmallTalkImageBrowser
        browser = SmallTalkImageBrowser()
        
        if len(browser.common_queries) >= 10:
            print(f"✅ Image Browser: {len(browser.common_queries)} query types available")
        else:
            print("❌ Image Browser: Missing query types")
    except Exception as e:
        print(f"❌ Image Browser feature test failed: {e}")

def main():
    """Run all tests"""
    print("SmallTalk Development Tools - Test Suite\n")
    print("This script tests the 6 new SmallTalk tools added to TuoKit.\n")
    
    # Run basic tests
    results = test_new_smalltalk_tools()
    
    # Run feature tests
    test_tool_features()
    
    print("\n" + "="*50)
    print("\nTest suite complete!")
    print("\nTo use these tools:")
    print("1. Ensure Ollama is running: ollama serve")
    print("2. Pull required models: ollama pull deepseek-coder:6.7b")
    print("3. Start TuoKit: streamlit run app.py")
    print("4. Navigate to any SmallTalk tool from the sidebar")

if __name__ == "__main__":
    main()
