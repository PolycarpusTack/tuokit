"""
TuoKit - SQL Toolkit Page
Complete SQL workflow tools with UI
"""

import streamlit as st
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))

# Page configuration
st.set_page_config(
    page_title="SQL Toolkit - TuoKit",
    page_icon="🗄️",
    layout="wide"
)

# Import sidebar navigation
from utils.sidebar_nav import render_sidebar_navigation

# Render sidebar
render_sidebar_navigation(current_page="sql_toolkit")

# Import and run the SQL toolkit
from toolkits.sql import SQLToolkit

def show():
    """Main entry point for SQL Toolkit page"""
    toolkit = SQLToolkit()
    toolkit.run()

# Run the toolkit
if __name__ == "__main__":
    show()

# Keep the original functions for backward compatibility with agent_hub
from toolkits.sql.processors import (
    generate_sql_query,
    optimize_sql_query,
    explain_sql_query,
    validate_sql_syntax,
    generate_schema_from_description,
    convert_sql_dialect,
    analyze_query_performance,
    format_sql_query,
    generate_table_documentation,
    sql_to_code,
    sql_to_smalltalk_visualworks,
    generate_sample_data_query,
    analyze_query_dependencies
)

# Export all functions
__all__ = [
    'generate_sql_query',
    'optimize_sql_query',
    'explain_sql_query',
    'validate_sql_syntax',
    'generate_schema_from_description',
    'convert_sql_dialect',
    'analyze_query_performance',
    'format_sql_query',
    'generate_table_documentation',
    'sql_to_code',
    'sql_to_smalltalk_visualworks',
    'generate_sample_data_query',
    'analyze_query_dependencies'
]