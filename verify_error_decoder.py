#!/usr/bin/env python3
"""
Verify Error Decoder and Exception Advisor Implementation
"""

import os
import sys
import importlib.util

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def verify_file_exists(filepath, description):
    """Check if a file exists"""
    if os.path.exists(filepath):
        print(f"OK - {description}: {filepath}")
        return True
    else:
        print(f"FAIL - {description} NOT FOUND: {filepath}")
        return False

def verify_imports(module_path, imports, description):
    """Verify that a module can be imported and has required functions"""
    try:
        spec = importlib.util.spec_from_file_location("module", module_path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        
        missing = []
        for imp in imports:
            if not hasattr(module, imp):
                missing.append(imp)
        
        if missing:
            print(f"WARN - {description} - Missing functions: {missing}")
            return False
        else:
            print(f"OK - {description} - All functions found")
            return True
    except Exception as e:
        print(f"FAIL - {description} - Import error: {e}")
        return False

def main():
    """Run verification tests"""
    print("[VERIFY] Error Decoder Enhancement Implementation\n")
    
    success = True
    
    # Check files exist
    print("[FILES] Checking Files:")
    success &= verify_file_exists("pages/error_tool.py", "Error Decoder")
    success &= verify_file_exists("pages/exception_advisor.py", "Exception Advisor")
    success &= verify_file_exists("ERROR_DECODER_ENHANCED.md", "Documentation")
    print()
    
    # Check error_tool.py functions
    print("[FUNCTIONS] Checking Error Decoder Functions:")
    error_functions = [
        "parse_error_message",
        "analyze_error",
        "generate_fix_patch",
        "get_educational_content",
        "show_educational_layer",
        "get_error_statistics"
    ]
    success &= verify_imports("pages/error_tool.py", error_functions, "error_tool.py")
    print()
    
    # Check exception_advisor.py functions
    print("[FUNCTIONS] Checking Exception Advisor Functions:")
    advisor_functions = [
        "analyze_exception_handling",
        "generate_handling_strategy"
    ]
    success &= verify_imports("pages/exception_advisor.py", advisor_functions, "exception_advisor.py")
    print()
    
    # Check navigation updates
    print("[NAVIGATION] Checking Navigation Updates:")
    try:
        with open("app.py", "r", encoding="utf-8") as f:
            app_content = f.read()
            
        if "pages/exception_advisor.py" in app_content:
            print("OK - Exception Advisor added to navigation")
        else:
            print("FAIL - Exception Advisor NOT in navigation")
            success = False
            
        if "TuoKit v1.4.0" in app_content:
            print("OK - Version updated to 1.4.0")
        else:
            print("FAIL - Version NOT updated")
            success = False
    except Exception as e:
        print(f"FAIL - Error checking app.py: {e}")
        success = False
    print()
    
    # Summary
    print("[SUMMARY]")
    if success:
        print("OK - All verifications passed! Error Decoder enhancements are properly implemented.")
        print("\n[NEXT STEPS]")
        print("1. Start TuoKit: ./start_tuokit.bat")
        print("2. Navigate to Error Decoder")
        print("3. Try SmallTalk/Ruby examples")
        print("4. Test code fix generation")
        print("5. Explore Exception Advisor")
    else:
        print("FAIL - Some verifications failed. Please check the implementation.")
    
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())
