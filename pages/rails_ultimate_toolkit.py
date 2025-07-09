"""
Rails Ultimate Toolkit Page
Thin wrapper for the Rails toolkit
"""

import streamlit as st
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))

from toolkits.rails.analyzer import RailsUltimateToolkit

def main():
    """Main entry point for Rails Ultimate Toolkit page"""
    st.set_page_config(
        page_title="Rails Ultimate Toolkit - TuoKit",
        page_icon="🛤️",
        layout="wide",
        initial_sidebar_state="expanded"
    )
    
    # Create and run the Rails toolkit
    toolkit = RailsUltimateToolkit()
    toolkit.run()

if __name__ == "__main__":
    main()