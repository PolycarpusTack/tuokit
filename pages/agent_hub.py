"""
TuoKit - Agent Hub Page
Uses the modularized agent hub toolkit
"""
import streamlit as st
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))

# Page configuration
st.set_page_config(
    page_title="Agent Hub - TuoKit",
    page_icon="🤖",
    layout="wide"
)

# Import and run the agent hub toolkit
from toolkits.agent_hub import AgentHubUI

def show():
    """Main entry point for Agent Hub page"""
    ui = AgentHubUI()
    ui.run()

# Run the agent hub
if __name__ == "__main__":
    show()
