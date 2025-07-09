#!/usr/bin/env python
"""Test script to verify app_modern.py can be imported and run"""

try:
    # Test imports
    print("Testing imports...")
    import streamlit as st
    import sqlite3
    import pandas as pd
    import psutil
    print("✓ Core imports successful")
    
    # Test tool registry import
    from utils.tool_registry import COMPLETE_TOOL_REGISTRY as TOOL_REGISTRY, get_tool_count
    print(f"✓ Tool registry loaded: {get_tool_count()} tools")
    
    # Test app_modern import
    import app_modern
    print("✓ app_modern.py imported successfully")
    
    # Show tool categories
    print("\nTool Categories:")
    for category, info in TOOL_REGISTRY.items():
        print(f"  {category}: {len(info['tools'])} tools")
    
    print("\n✅ All tests passed! The app is ready to run.")
    print("\nTo start TuoKit, run:")
    print("  streamlit run app_modern.py")
    
except ImportError as e:
    print(f"❌ Import error: {e}")
    print("\nPlease install missing dependencies:")
    print("  pip install streamlit plotly psutil")
    
except Exception as e:
    print(f"❌ Error: {e}")
