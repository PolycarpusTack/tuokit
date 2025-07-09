"""
TuoKit Sidebar Navigation Component
Provides consistent navigation across all pages
"""

import streamlit as st
from .navigation import NAVIGATION_CATEGORIES, get_tool_count

def render_sidebar_navigation(current_page=None):
    """
    Render a consistent sidebar navigation for all TuoKit pages
    
    Args:
        current_page: The current page/tool identifier
    """
    with st.sidebar:
        # TuoKit Logo and Home
        st.markdown("""
        <div style="text-align: center; padding: 1rem 0;">
            <h2 style="margin: 0;">🚀 TuoKit</h2>
            <p style="color: #666; font-size: 0.8rem; margin-top: 0.5rem;">
                AI Development Companion
            </p>
        </div>
        """, unsafe_allow_html=True)
        
        if st.button("🏠 Home", use_container_width=True):
            st.switch_page("app.py")
        
        st.divider()
        
        # Quick Stats
        col1, col2 = st.columns(2)
        with col1:
            st.metric("Tools", get_tool_count(), label_visibility="collapsed")
        with col2:
            st.metric("Categories", len(NAVIGATION_CATEGORIES), label_visibility="collapsed")
        
        st.divider()
        
        # Navigation by Category
        st.subheader("🧭 Navigation")
        
        for category_name, category_data in NAVIGATION_CATEGORIES.items():
            with st.expander(f"{category_data.get('icon', '📁')} {category_name}", expanded=False):
                st.caption(category_data['description'])
                
                for tool_id, tool_info in category_data['tools'].items():
                    # Highlight current page
                    if current_page == tool_id:
                        st.markdown(f"""
                        <div style="background: rgba(25, 118, 210, 0.2); 
                             border-left: 3px solid #1976d2; 
                             padding: 0.5rem; margin: 0.25rem -0.5rem;">
                            {tool_info['icon']} **{tool_info['name']}** (current)
                        </div>
                        """, unsafe_allow_html=True)
                    else:
                        if st.button(
                            f"{tool_info['icon']} {tool_info['name']}", 
                            key=f"nav_{tool_id}",
                            use_container_width=True,
                            help=tool_info['description']
                        ):
                            st.switch_page(f"pages/{tool_info['file']}")
        
        st.divider()
        
        # Quick Links
        st.subheader("⚡ Quick Links")
        
        col1, col2 = st.columns(2)
        with col1:
            if st.button("📚 Knowledge", use_container_width=True):
                st.switch_page("pages/knowledge_lib.py")
        
        with col2:
            if st.button("❓ Help", use_container_width=True):
                st.switch_page("pages/help_guide.py")
        
        # Featured: SQL Toolkit
        st.divider()
        st.subheader("✨ Featured")
        st.info("""
        **🛢️ SQL Toolkit Pro**
        
        Now with 11 powerful tools:
        • Generate SQL from natural language
        • Format messy queries
        • Convert to Python/Java/Smalltalk
        • Generate documentation
        • Create sample data
        • And much more!
        """)
        
        if st.button("🛢️ Open SQL Toolkit", use_container_width=True, type="primary"):
            st.switch_page("pages/sql_toolkit_modern.py")
        
        # System Health (if connected)
        if st.session_state.get('db'):
            st.divider()
            st.caption("🏥 System Status")
            
            try:
                # Simple health check
                count = st.session_state.db.get_knowledge_count()
                st.success(f"✅ Database: Connected ({count} items)")
            except:
                st.error("❌ Database: Disconnected")