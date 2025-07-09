#!/usr/bin/env python3
"""
TuoKit Cleanup Verification Script
Tests that the cleanup was successful
"""

def verify_cleanup():
    """Verify all cleanup steps completed successfully"""
    print("TuoKit Cleanup Verification")
    print("=" * 50)
    
    results = []
    
    # Test 1: Check new test directory exists
    import os
    if os.path.exists("tests/test_sql_suite.py"):
        results.append(("SQL tests consolidated", True))
    else:
        results.append(("SQL tests consolidated", False))
    
    # Test 2: Check utils package structure
    if os.path.isdir("utils") and os.path.exists("utils/__init__.py"):
        results.append(("Utils modularized", True))
    else:
        results.append(("Utils modularized", False))
    
    # Test 3: Test imports
    try:
        from utils import DatabaseManager, safe_ollama_generate, get_system_stats
        from utils import OllamaToolBase, capture_knowledge
        results.append(("Utils imports working", True))
    except ImportError as e:
        results.append(("Utils imports working", False))
        print(f"Import error: {e}")
    
    # Test 4: Check old utils backed up
    if os.path.exists("utils_old.py"):
        results.append(("Original utils backed up", True))
    else:
        results.append(("Original utils backed up", False))
    
    # Print results
    print("\nCleanup Verification Results:")
    print("-" * 50)
    
    all_passed = True
    for test, passed in results:
        status = "[PASS]" if passed else "[FAIL]"
        print(f"{status} {test}")
        if not passed:
            all_passed = False
    
    print("\n" + "=" * 50)
    if all_passed:
        print("SUCCESS: All cleanup steps verified!")
        print("\nNext steps:")
        print("1. Run SQL test suite: python tests/test_sql_suite.py")
        print("2. Delete old test files: test_sql_*.py")
        print("3. Update any custom imports in your code (optional)")
    else:
        print("WARNING: Some cleanup steps need attention")
        
    return all_passed

if __name__ == "__main__":
    import sys
    success = verify_cleanup()
    sys.exit(0 if success else 1)
