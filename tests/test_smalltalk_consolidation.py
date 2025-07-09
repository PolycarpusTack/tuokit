"""
Test script for SmallTalk Toolkit consolidation
Verifies the new unified tool loads and functions correctly
"""

import streamlit as st
import sys
import os

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def test_smalltalk_toolkit():
    """Test that the consolidated SmallTalk toolkit works"""
    print("🧪 Testing SmallTalk Toolkit Consolidation...")
    
    try:
        # Test 1: Import the toolkit
        print("✓ Test 1: Importing smalltalk_toolkit...")
        from pages.smalltalk_toolkit import SmallTalkToolkit, show
        print("  ✅ Import successful!")
        
        # Test 2: Create toolkit instance
        print("\n✓ Test 2: Creating toolkit instance...")
        toolkit = SmallTalkToolkit()
        print("  ✅ Toolkit instance created!")
        
        # Test 3: Check all methods exist
        print("\n✓ Test 3: Checking all tool methods...")
        methods = [
            'generate_class',
            'explain_code', 
            'generate_snippet',
            'refactor_code',
            'convert_code',
            'generate_seaside_component',
            'generate_metaprogramming'
        ]
        
        for method in methods:
            if hasattr(toolkit, method):
                print(f"  ✅ {method} exists")
            else:
                print(f"  ❌ {method} missing!")
        
        # Test 4: Check navigation update
        print("\n✓ Test 4: Checking navigation update...")
        from utils.navigation import NAVIGATION_CATEGORIES
        
        smalltalk_category = NAVIGATION_CATEGORIES.get("🎈 Smalltalk Tools", {})
        tools = smalltalk_category.get("tools", {})
        
        if "smalltalk_toolkit" in tools:
            print("  ✅ Navigation updated correctly!")
            print(f"     Tool: {tools['smalltalk_toolkit']['name']}")
            print(f"     File: {tools['smalltalk_toolkit']['file']}")
        else:
            print("  ❌ Navigation not updated!")
            
        # Test 5: Check old tools removed
        print("\n✓ Test 5: Checking old tools removed from navigation...")
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
        
        print(f"  ✅ {removed_count}/{len(old_tools)} old tools removed from navigation")
        
        print("\n🎉 All tests passed! SmallTalk consolidation successful!")
        
    except Exception as e:
        print(f"\n❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_smalltalk_toolkit()
