"""
Health Monitor Page - Unified health monitoring dashboard
Thin wrapper for the health toolkit
"""

import streamlit as st
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))

from toolkits.health.analyzer import HealthAnalyzer

def main():
    """Main entry point for health monitoring page"""
    st.set_page_config(
        page_title="TuoKit Health Monitor",
        page_icon="🏥",
        layout="wide"
    )
    
    # Create and run the health analyzer
    analyzer = HealthAnalyzer()
    analyzer.run()

if __name__ == "__main__":
    main()