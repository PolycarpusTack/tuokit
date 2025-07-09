"""
TuoKit - Crash Analyzer V2 Page
Test page for the redesigned crash analyzer
"""
import streamlit as st
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))

# Page configuration
st.set_page_config(
    page_title="Crash Analyzer V2 - TuoKit",
    page_icon="🚨",
    layout="wide"
)

# Import and run the new crash analyzer
from toolkits.crash_analyzer_v2 import CrashAnalyzerV2

def show():
    """Main entry point for Crash Analyzer V2 page"""
    analyzer = CrashAnalyzerV2()
    analyzer.run()

# Run the analyzer
if __name__ == "__main__":
    show()