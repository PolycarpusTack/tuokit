"""
Simple test to verify SmallTalk toolkit consolidation
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def test_smalltalk_toolkit():
    """Test that the consolidated SmallTalk toolkit works"""
    print("Testing SmallTalk Toolkit Consolidation...")
    
    try:
        # Test 1: Import the toolkit
        print("\nTest 1: Importing smalltalk_toolkit...")
        from pages.smalltalk_toolkit import SmallTalkToolkit, show
        print("  PASS - Import successful!")
        
        # Test 2: Create toolkit instance
        print("\nTest 2: Creating toolkit instance...")
        toolkit = SmallTalkToolkit()
        print("  PASS - Toolkit instance created!")
        
        # Test 3: Check all methods exist
        print("\nTest 3: Checking all tool methods...")
        methods = [
            'generate_class',
            'explain_code', 
            'generate_snippet',
            'refactor_code',
            'convert_code',
            'generate_seaside_component',
            'generate_metaprogramming'
        ]
        
        all_exist = True
        for method in methods:
            if hasattr(toolkit, method):
                print(f"  PASS - {method} exists")
            else:
                print(f"  FAIL - {method} missing!")
                all_exist = False
        
        # Test 4: Check navigation update
        print("\nTest 4: Checking navigation update...")
        from utils.navigation import NAVIGATION_CATEGORIES
        
        smalltalk_category = NAVIGATION_CATEGORIES.get("🎈 Smalltalk Tools", {})
        tools = smalltalk_category.get("tools", {})
        
        if "smalltalk_toolkit" in tools:
            print("  PASS - Navigation updated correctly!")
            print(f"       Tool: {tools['smalltalk_toolkit']['name']}")
            print(f"       File: {tools['smalltalk_toolkit']['file']}")
        else:
            print("  FAIL - Navigation not updated!")
            
        # Test 5: Check old tools removed
        print("\nTest 5: Checking old tools removed from navigation...")
        old_tools = [
            'seaside_generator',
            'smalltalk_class_gen',
            'smalltalk_explainer',
            'smalltalk_meta',
            'smalltalk_refactorer',
            'smalltalk_ruby_converter',
            'smalltalk_snippets'
        ]
        
        removed_count = 0
        for old_tool in old_tools:
            if old_tool not in tools:
                removed_count += 1
            else:
                print(f"  WARNING - {old_tool} still in navigation!")
        
        print(f"  RESULT - {removed_count}/{len(old_tools)} old tools removed from navigation")
        
        # Test 6: Check archived files
        print("\nTest 6: Checking archived files...")
        archive_dirs = [d for d in os.listdir('archive') if d.startswith('smalltalk_tools_')]
        if archive_dirs:
            latest_archive = sorted(archive_dirs)[-1]
            archive_path = os.path.join('archive', latest_archive)
            archived_files = os.listdir(archive_path)
            print(f"  PASS - Found archive: {latest_archive}")
            print(f"       Contains {len(archived_files)} files")
        else:
            print("  INFO - No archive directory found")
        
        print("\n" + "="*50)
        if all_exist and "smalltalk_toolkit" in tools and removed_count == len(old_tools):
            print("ALL TESTS PASSED! SmallTalk consolidation successful!")
        else:
            print("Some tests failed. Please check the results above.")
        print("="*50)
        
    except Exception as e:
        print(f"\nERROR - Test failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_smalltalk_toolkit()
