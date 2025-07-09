"""
TuoKit - Crash Analyzer Pro
Refactored modular version
"""
import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Crash Analyzer - TuoKit",
    page_icon="🚨",
    layout="wide"
)

# Import and run the refactored crash analyzer
from toolkits.crash_analyzer import CrashAnalyzer

def show():
    """Main entry point for Crash Analyzer page"""
    analyzer = CrashAnalyzer()
    analyzer.run()

# Run the analyzer
if __name__ == "__main__":
    show()