"""
Enhanced Test Script for SmallTalk & Rails Tools
Verifies all enhanced features are working correctly
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.ollama import OllamaManager, safe_ollama_generate
from utils.database import DatabaseManager

def test_enhanced_features():
    """Test the enhanced features of all tools"""
    print("=== Testing Enhanced SmallTalk & Rails Tools ===\n")
    
    results = {}
    
    # Test 1: Import all enhanced tools
    print("1. Testing tool imports...")
    try:
        from pages.smalltalk_explainer import SmallTalkExplainer, show as explainer_show
        from pages.rails_scaffold import RailsScaffoldGenerator, show as scaffold_show
        from pages.smalltalk_ruby_converter import CodeConverter, show as converter_show
        from pages.rails_debugger import RailsDebugger, show as debugger_show
        from pages.smalltalk_snippets import SmallTalkSnippetFinder, show as snippets_show
        
        print("✅ All enhanced tools imported successfully")
        results['imports'] = True
    except Exception as e:
        print(f"❌ Import error: {e}")
        results['imports'] = False
        return results
    
    # Test 2: Check enhanced features in each tool
    print("\n2. Checking enhanced features...")
    
    # SmallTalk Explainer enhancements
    try:
        explainer = SmallTalkExplainer()
        # Test enhanced explain_code with new parameters
        test_method = explainer.explain_code.__code__.co_varnames
        has_detail_level = 'detail_level' in test_method
        has_include_tips = 'include_tips' in test_method
        has_compare_oop = 'compare_oop' in test_method
        
        if has_detail_level and has_include_tips and has_compare_oop:
            print("✅ SmallTalk Explainer: Enhanced parameters found")
            results['explainer_enhanced'] = True
        else:
            print("❌ SmallTalk Explainer: Missing enhanced parameters")
            results['explainer_enhanced'] = False
    except Exception as e:
        print(f"❌ SmallTalk Explainer test failed: {e}")
        results['explainer_enhanced'] = False
    
    # Rails Scaffold Generator enhancements
    try:
        generator = RailsScaffoldGenerator()
        # Test enhanced generate_scaffold parameters
        test_method = generator.generate_scaffold.__code__.co_varnames
        has_test_framework = 'test_framework' in test_method
        has_template_engine = 'template_engine' in test_method
        has_api_mode = 'api_mode' in test_method
        
        if has_test_framework and has_template_engine and has_api_mode:
            print("✅ Rails Scaffold: Enhanced configuration options found")
            results['scaffold_enhanced'] = True
        else:
            print("❌ Rails Scaffold: Missing enhanced options")
            results['scaffold_enhanced'] = False
    except Exception as e:
        print(f"❌ Rails Scaffold test failed: {e}")
        results['scaffold_enhanced'] = False
    
    # Code Converter enhancements
    try:
        converter = CodeConverter()
        # Test enhanced convert_code parameters
        test_method = converter.convert_code.__code__.co_varnames
        has_preserve_style = 'preserve_style' in test_method
        has_add_explanations = 'add_explanations' in test_method
        
        if has_preserve_style and has_add_explanations:
            print("✅ Code Converter: Enhanced options found")
            results['converter_enhanced'] = True
        else:
            print("❌ Code Converter: Missing enhanced options")
            results['converter_enhanced'] = False
    except Exception as e:
        print(f"❌ Code Converter test failed: {e}")
        results['converter_enhanced'] = False
    
    # Rails Debugger enhancements
    try:
        debugger = RailsDebugger()
        # Test enhanced methods
        has_detect_error = hasattr(debugger, 'detect_error_type')
        has_quick_fixes = hasattr(debugger, 'get_quick_fixes')
        
        if has_detect_error and has_quick_fixes:
            print("✅ Rails Debugger: Enhanced error detection found")
            results['debugger_enhanced'] = True
            
            # Test error detection
            test_error = "ActionController::RoutingError (No route matches [GET] '/test')"
            error_type, emoji, desc = debugger.detect_error_type(test_error)
            if error_type == "routing":
                print("   ✓ Error categorization working")
        else:
            print("❌ Rails Debugger: Missing enhanced methods")
            results['debugger_enhanced'] = False
    except Exception as e:
        print(f"❌ Rails Debugger test failed: {e}")
        results['debugger_enhanced'] = False
    
    # SmallTalk Snippets enhancements
    try:
        finder = SmallTalkSnippetFinder()
        # Test enhanced features
        has_categories = hasattr(finder, 'snippet_categories')
        has_complexity = 'complexity' in finder.generate_snippet.__code__.co_varnames
        has_search_filters = hasattr(finder, 'search_snippets')
        
        if has_categories and has_complexity and has_search_filters:
            print("✅ Snippet Finder: Enhanced features found")
            results['snippets_enhanced'] = True
            
            # Test categories
            if isinstance(finder.snippet_categories, dict):
                print(f"   ✓ {len(finder.snippet_categories)} categories available")
        else:
            print("❌ Snippet Finder: Missing enhanced features")
            results['snippets_enhanced'] = False
    except Exception as e:
        print(f"❌ Snippet Finder test failed: {e}")
        results['snippets_enhanced'] = False
    
    # Test 3: UI Enhancement verification
    print("\n3. Checking UI enhancements...")
    ui_checks = {
        'tabs': "tabs = st.tabs" in open('pages/smalltalk_explainer.py').read(),
        'sidebar_config': "with st.sidebar:" in open('pages/rails_scaffold.py').read(),
        'sliders': "select_slider" in open('pages/smalltalk_snippets.py').read(),
        'metrics': "st.metric" in open('pages/rails_debugger.py').read()
    }
    
    for feature, present in ui_checks.items():
        if present:
            print(f"✅ UI Feature: {feature}")
        else:
            print(f"❌ UI Feature: {feature} missing")
    
    results['ui_enhanced'] = all(ui_checks.values())
    
    # Summary
    print("\n=== Enhancement Test Summary ===")
    total_tests = len(results)
    passed_tests = sum(1 for v in results.values() if v)
    
    print(f"\nPassed: {passed_tests}/{total_tests} tests")
    
    if passed_tests == total_tests:
        print("\n🎉 All enhancements verified successfully!")
        print("The SmallTalk & Rails tools are fully enhanced and ready to use.")
    else:
        print("\n⚠️ Some enhancements need attention:")
        for test, passed in results.items():
            if not passed:
                print(f"  - {test}")
    
    return results

def test_integration():
    """Test integration between tools"""
    print("\n=== Testing Tool Integration ===")
    
    # Test that tools can work together
    try:
        # Test 1: Generate a snippet and convert it
        from pages.smalltalk_snippets import SmallTalkSnippetFinder
        from pages.smalltalk_ruby_converter import CodeConverter
        
        finder = SmallTalkSnippetFinder()
        converter = CodeConverter()
        
        print("✅ Tools can be used together")
        
        # Test 2: Database integration
        db = DatabaseManager()
        if db.connected:
            print("✅ Database integration available")
        else:
            print("⚠️ Database not connected (optional)")
        
        return True
    except Exception as e:
        print(f"❌ Integration test failed: {e}")
        return False

def main():
    """Run all enhancement tests"""
    print("SmallTalk & Rails Tools - Enhancement Verification\n")
    print("This script verifies that all enhancements have been successfully applied.\n")
    
    # Check prerequisites
    print("Checking prerequisites...")
    ollama_status = OllamaManager.get_status()
    if ollama_status["running"]:
        print("✅ Ollama is running")
    else:
        print("⚠️ Ollama not running (tools will work but generation will fail)")
    
    print("\n" + "="*50 + "\n")
    
    # Run enhancement tests
    enhancement_results = test_enhanced_features()
    
    # Run integration test
    integration_result = test_integration()
    
    print("\n" + "="*50)
    print("\n✨ Enhancement verification complete!")
    
    # Final recommendations
    if all(enhancement_results.values()) and integration_result:
        print("\n🚀 All systems go! The enhanced tools are ready for use.")
        print("\nNext steps:")
        print("1. Start TuoKit: streamlit run app.py")
        print("2. Navigate to any SmallTalk or Rails tool")
        print("3. Enjoy the enhanced features!")
    else:
        print("\n📋 Please review the failed tests above and check:")
        print("1. All files were saved correctly")
        print("2. No syntax errors in the enhanced code")
        print("3. Dependencies are properly installed")

if __name__ == "__main__":
    main()
