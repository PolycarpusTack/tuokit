"""
TuoKit Agent Hub - Consolidated Version
Uses the modular agent_hub toolkit
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Agent Hub - TuoKit",
    page_icon="🤖",
    layout="wide"
)

# Import the new modular agent hub
from toolkits.agent_hub import AgentHubUI


def show():
    """Main entry point for Agent Hub page"""
    agent_hub = AgentHubUI()
    agent_hub.run()


# Run the agent hub
if __name__ == "__main__":
    show()
