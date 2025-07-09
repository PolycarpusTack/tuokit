# toolkits/error_decoder/ui_components.py
"""UI components for error decoder interface"""

import streamlit as st
from .config import ERROR_INPUT_HEIGHT, CODE_INPUT_HEIGHT, LANG_MAP, LANGUAGES, ANALYSIS_DEPTHS, DEFAULT_DEPTH
from .parsers import parse_error_message
from .processors import (
    generate_fix_patch, generate_prevention_strategies,
    get_common_misconceptions, get_historical_context, analyze_error_patterns
)
from .educational import show_error_examples
from utils.annotated_text_helpers import (
    annotated_text, parse_error_message as parse_annotated_error,
    highlight_severity, display_annotated_error, ANNOTATION_COLORS
)

def render_header():
    """Render main header and description"""
    st.title("🎓 Advanced Error Decoder")
    st.caption("Professional debugging with deep educational insights and code solutions")

def render_controls():
    """Render language and analysis controls"""
    col1, col2, col3 = st.columns([2, 2, 1])
    
    with col1:
        language = st.selectbox(
            "Focus Language:",
            options=LANGUAGES,
            index=0
        )
    
    with col2:
        analysis_depth = st.select_slider(
            "Analysis Depth",
            options=ANALYSIS_DEPTHS,
            value=DEFAULT_DEPTH
        )
    
    with col3:
        educational_mode = st.checkbox(
            "Educational Mode", 
            value=True,
            help="Get detailed explanations and learning resources"
        )
    
    return language, analysis_depth, educational_mode

def render_main_tabs():
    """Render main interface tabs"""
    return st.tabs(["Error Analysis", "Code Context", "Error Statistics"])

def render_error_input(language):
    """Render error input area with examples"""
    placeholder = get_placeholder_for_language(language)
    
    error_input = st.text_area(
        f"Paste error message or traceback:", 
        placeholder=placeholder,
        height=ERROR_INPUT_HEIGHT,
        value=st.session_state.get('loaded_error', '')
    )
    
    # Show examples gallery
    show_error_examples(language)
    
    return error_input

def render_code_context(language):
    """Render code context input area"""
    st.write("Provide code context for better analysis (optional but recommended):")
    
    user_code = st.text_area(
        "Code context:",
        value=st.session_state.get('user_code', ''),
        height=CODE_INPUT_HEIGHT,
        placeholder="Paste the code that caused the error..."
    )
    
    return user_code

def render_analysis_results(error_input, analysis, parsed, language, user_code, analysis_depth, educational_mode, model):
    """Render complete analysis results"""
    st.success("✅ Error Analysis Complete!")
    
    # Error card with annotated text
    st.subheader(f"🔍 {parsed['error_type']} Analysis")
    
    # Display error with annotations
    st.markdown("**Error Message:**")
    error_components = parse_annotated_error(error_input)
    annotated_text(*error_components)
    
    # Error metadata
    render_error_metadata(parsed)
    
    # Main analysis
    st.markdown("### 📋 Analysis")
    st.markdown(analysis)
    
    # Educational layer
    if educational_mode and analysis_depth != "Quick":
        from .educational import show_educational_layer
        show_educational_layer(parsed['error_type'], parsed['language'], model)
    
    # Code fix if context available
    if user_code.strip() and analysis_depth != "Quick":
        render_code_fix_solution(parsed, user_code, language, model)
    
    # Prevention checklist
    render_prevention_checklist(parsed, model)
    
    # Deep analysis extras
    if analysis_depth == "Deep":
        render_deep_analysis_extras(parsed, model)

def render_error_metadata(parsed):
    """Render error metadata in columns"""
    if parsed.get('file') or parsed.get('line'):
        cols = st.columns(4)
        if parsed.get('file'):
            cols[0].metric("File", parsed['file'])
        if parsed.get('line'):
            cols[1].metric("Line", parsed['line'])
        if parsed.get('language'):
            cols[2].metric("Language", parsed['language'].title())
        if parsed.get('context'):
            cols[3].metric("Context", parsed['context'][:20] + "...")

def render_code_fix_solution(parsed, user_code, language, model):
    """Render code fix solution section"""
    with st.expander("🛠️ Code Fix Solution"):
        with st.spinner("Generating fix..."):
            fixed_code = generate_fix_patch(parsed, user_code, model)
            
            if fixed_code:
                st.code(fixed_code, language=LANG_MAP[language])
                
                col1, col2 = st.columns([1, 3])
                with col1:
                    if st.button("Apply Fix"):
                        st.session_state.user_code = fixed_code
                        st.success("Code updated! Check the Code Context tab.")
                with col2:
                    st.download_button(
                        "Download Fixed Code",
                        data=fixed_code,
                        file_name=f"fixed_{parsed.get('file', 'code')}.{parsed['language']}",
                        mime="text/plain"
                    )
            else:
                st.warning("Unable to generate automatic fix")

def render_prevention_checklist(parsed, model):
    """Render prevention checklist"""
    with st.expander("✅ Prevention Checklist"):
        with st.spinner("Generating prevention strategies..."):
            prevention = generate_prevention_strategies(
                parsed['error_type'], 
                parsed['language'],
                model
            )
            st.markdown(prevention)

def render_deep_analysis_extras(parsed, model):
    """Render additional deep analysis components"""
    # Community insights
    with st.expander("🌐 Community Insights"):
        misconceptions = get_common_misconceptions(
            parsed['error_type'],
            parsed['language'],
            model
        )
        st.markdown(misconceptions)
    
    # Historical context
    with st.expander("🕰️ Historical Context"):
        history = get_historical_context(
            parsed['error_type'],
            parsed['language'],
            model
        )
        st.markdown(history)

def render_related_errors(db, parsed, error_input):
    """Render related historical errors"""
    if db:
        with st.expander("📚 Related Historical Errors"):
            recent = db.get_recent_queries(limit=20)
            similar_errors = [
                q for q in recent 
                if q[1] == "error_decoder" and 
                parsed['error_type'] in q[3] and 
                q[3] != error_input
            ][:3]
            
            if similar_errors:
                for err in similar_errors:
                    st.caption(f"• {err[3][:100]}...")
            else:
                st.info("No similar errors found in knowledge base")

def render_sidebar_tools(error_data, model):
    """Render sidebar advanced tools"""
    st.subheader("🔬 Advanced Tools")
    
    # Pattern detection
    if st.button("🔍 Detect Error Pattern"):
        if error_data:
            pattern = analyze_error_patterns(error_data['error_type'], model)
            st.info(pattern)

def render_recent_errors(db):
    """Render recent errors in sidebar"""
    st.subheader("📚 Recent Errors")
    if db:
        recent = db.get_recent_queries(limit=10)
        error_queries = [q for q in recent if q[1] == "error_decoder"][:5]
        
        if error_queries:
            for query in error_queries:
                parsed = parse_error_message(query[3])
                error_type = parsed["error_type"]
                with st.expander(f"🔖 {error_type[:20]}..."):
                    st.caption(query[3][:100] + "...")
                    if st.button("Load", key=f"load_{query[0]}"):
                        st.session_state.loaded_error = query[3]
                        st.rerun()
        else:
            st.info("No recent errors decoded")

def render_pro_tips():
    """Render pro tips section"""
    st.caption("💡 Pro Tips:")
    st.info(
        "• Paste full tracebacks for best results\n"
        "• Include code context for fix generation\n"
        "• Use Deep analysis for educational insights"
    )

def render_navigation():
    """Render navigation buttons"""
    st.divider()
    col1, col2, col3 = st.columns(3)
    with col1:
        if st.button("← Back to Dashboard", use_container_width=True):
            st.switch_page("app.py")
    with col2:
        if st.button("🛡️ Exception Advisor", use_container_width=True):
            st.switch_page("pages/exception_advisor.py")
    with col3:
        if st.button("📚 Knowledge Library", use_container_width=True):
            st.switch_page("pages/knowledge_lib.py")

def get_placeholder_for_language(language):
    """Get appropriate placeholder text for language"""
    placeholders = {
        "SmallTalk": "MessageNotUnderstood: MyClass>>someMethod",
        "Ruby": "NoMethodError: undefined method 'name' for nil:NilClass",
        "Rails": "NoMethodError: undefined method 'name' for nil:NilClass"
    }
    
    return placeholders.get(language, 
        "Traceback (most recent call last):\n  File \"app.py\", line 42, in <module>\n    result = calculate(10, 0)\nZeroDivisionError: division by zero"
    )