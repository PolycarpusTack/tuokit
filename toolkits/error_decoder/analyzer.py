# toolkits/error_decoder/analyzer.py
"""Main Error Decoder analyzer class"""

import streamlit as st
from utils.tool_base import TuoKitToolBase
from utils import DatabaseManager, get_available_models
from .config import DEFAULT_MODEL, FALLBACK_MODELS
from .parsers import parse_error_message
from .processors import analyze_error
from .educational import show_error_statistics, show_learning_center
from .ui_components import (
    render_header, render_controls, render_main_tabs, render_error_input,
    render_code_context, render_analysis_results, render_sidebar_tools,
    render_recent_errors, render_pro_tips, render_navigation, render_related_errors
)

class ErrorDecoder(TuoKitToolBase):
    """Advanced error decoder with educational insights"""
    
    def __init__(self):
        super().__init__(
            tool_name="Advanced Error Decoder",
            tool_description="Professional debugging with deep educational insights and code solutions"
        )
        self.initialize_session_state()
    
    def initialize_session_state(self):
        """Initialize session state variables"""
        if "db" not in st.session_state:
            try:
                st.session_state.db = DatabaseManager()
            except Exception as e:
                st.error(f"Database connection failed: {e}")
                st.session_state.db = None
        
        if "user_code" not in st.session_state:
            st.session_state.user_code = ""
        
        if "error_data" not in st.session_state:
            st.session_state.error_data = {}
        
        if "educational_mode" not in st.session_state:
            st.session_state.educational_mode = True
        
        if "selected_model" not in st.session_state:
            st.session_state.selected_model = DEFAULT_MODEL
    
    def run(self):
        """Main entry point for the error decoder"""
        # Page configuration
        st.set_page_config(
            page_title="TuoKit - Advanced Error Decoder",
            page_icon="🐞",
            layout="wide"
        )
        
        # Render header
        render_header()
        
        # Sidebar model selection
        with st.sidebar:
            self.render_model_selection()
        
        # Main controls
        language, analysis_depth, st.session_state.educational_mode = render_controls()
        
        # Main interface tabs
        tab1, tab2, tab3 = render_main_tabs()
        
        with tab1:
            error_input = render_error_input(language)
            
            # Decode button
            if st.button("🔍 Analyze Error", type="primary", use_container_width=True):
                self.analyze_error(error_input, language, analysis_depth)
        
        with tab2:
            st.session_state.user_code = render_code_context(language)
        
        with tab3:
            show_error_statistics(st.session_state.db)
        
        # Sidebar extras
        with st.sidebar:
            render_sidebar_tools(st.session_state.error_data, st.session_state.selected_model)
            st.divider()
            show_learning_center(language)
            st.divider()
            render_recent_errors(st.session_state.db)
            st.divider()
            self.render_settings()
            render_pro_tips()
        
        # Navigation
        render_navigation()
        
        # Clear loaded error
        if 'loaded_error' in st.session_state:
            del st.session_state.loaded_error
    
    def render_model_selection(self):
        """Render model selection in sidebar"""
        st.subheader("🤖 AI Model")
        available_models = get_available_models(FALLBACK_MODELS)
        st.session_state.selected_model = st.selectbox(
            "Error Analysis Model", 
            available_models,
            index=0,
            help="Models currently available in Ollama"
        )
    
    def render_settings(self):
        """Render settings section"""
        st.subheader("⚙️ Settings")
        st.session_state.selected_model = st.selectbox(
            "AI Model",
            options=FALLBACK_MODELS,
            index=0
        )
    
    def analyze_error(self, error_input, language, analysis_depth):
        """Analyze the error and display results"""
        if not error_input.strip():
            st.warning("Please paste an error message")
            return
        
        with st.spinner("Performing deep analysis..."):
            # Parse error
            parsed = parse_error_message(error_input)
            if language != "Auto-detect":
                parsed["language"] = language.lower()
            st.session_state.error_data = parsed
            
            # Get analysis based on depth
            if analysis_depth == "Quick":
                analysis = analyze_error(parsed, "", st.session_state.selected_model)
            else:
                analysis = analyze_error(parsed, st.session_state.user_code, st.session_state.selected_model)
            
            # Save to knowledge base
            if st.session_state.db:
                try:
                    query_id = st.session_state.db.log_query(
                        tool="error_decoder",
                        model=st.session_state.selected_model,
                        prompt=error_input,
                        response=analysis
                    )
                    st.session_state.last_query_id = query_id
                    
                    # Auto-capture knowledge
                    self.auto_capture_knowledge(
                        prompt=error_input,
                        response=analysis,
                        metadata={
                            "error_type": parsed['error_type'],
                            "language": parsed['language'],
                            "analysis_depth": analysis_depth
                        }
                    )
                except Exception as e:
                    st.error(f"Error logging: {e}")
        
        # Display results
        render_analysis_results(
            error_input, analysis, parsed, language,
            st.session_state.user_code, analysis_depth,
            st.session_state.educational_mode, st.session_state.selected_model
        )
        
        # Related errors
        render_related_errors(st.session_state.db, parsed, error_input)