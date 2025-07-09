"""
Ruby Toolkit Page - Thin wrapper for Ruby toolkit
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Ruby Toolkit - TuoKit",
    page_icon="💎",
    layout="wide"
)

# Import sidebar navigation
from utils.sidebar_nav import render_sidebar_navigation

# Render sidebar
render_sidebar_navigation(current_page="ruby_toolkit")

# Import and run the Ruby toolkit
from toolkits.ruby import RubyToolkit

def show():
    """Run the Ruby toolkit"""
    toolkit = RubyToolkit()
    toolkit.run()

if __name__ == "__main__":
    show()