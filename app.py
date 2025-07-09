import streamlit as st
from utils import (
    DatabaseManager, 
    get_system_stats, 
    get_available_models,
    apply_modern_theme,
    render_hero_section
)
from utils.navigation import NAVIGATION_CATEGORIES, get_tool_count, get_category_count, search_tools
from utils.model_manager import ModelManager
import json

# Initialize session state with dynamic model selection
if "selected_model" not in st.session_state:
    st.session_state.selected_model = ModelManager.get_default_model()
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed. Please check your configuration: {e}")
        st.session_state.db = None
if "show_command_palette" not in st.session_state:
    st.session_state.show_command_palette = False

# Check for first run
if "first_run_checked" not in st.session_state:
    st.session_state.first_run_checked = True
    if st.session_state.db:
        try:
            count = st.session_state.db.get_knowledge_count()
            recent = st.session_state.db.get_recent_queries(limit=1)
            if count == 0 and len(recent) == 0:
                st.switch_page("pages/onboarding_wizard.py")
        except:
            pass

# Page configuration
st.set_page_config(
    page_title="TuoKit - AI Development Companion",
    page_icon="🚀",
    layout="wide",
    initial_sidebar_state="collapsed"
)

# Apply modern theme
apply_modern_theme()

# Add keyboard shortcuts
st.markdown("""
<script>
document.addEventListener('DOMContentLoaded', function() {
    document.addEventListener('keydown', function(e) {
        // Press / to focus search
        if (e.key === '/' && !e.ctrlKey && !e.metaKey) {
            e.preventDefault();
            const searchInput = document.querySelector('[data-testid="stTextInput"] input');
            if (searchInput) searchInput.focus();
        }
    });
});
</script>
""", unsafe_allow_html=True)

# Modern Navigation Bar
def render_nav_bar():
    """Render modern navigation bar with model selector"""
    # Fixed position nav with model selector
    nav_placeholder = st.container()
    with nav_placeholder:
        st.markdown("""
        <div style="position: fixed; top: 0; left: 0; right: 0; height: 80px; 
             background: rgba(20, 20, 30, 0.95); backdrop-filter: blur(20px); 
             border-bottom: 1px solid rgba(255, 255, 255, 0.1); z-index: 100;">
        </div>
        """, unsafe_allow_html=True)
        
        # Create columns for layout
        col1, col2, col3 = st.columns([2, 2, 3])
        
        with col1:
            st.markdown("""
            <div style="margin-top: 15px; display: flex; align-items: center; gap: 1rem;">
                <span style="font-size: 2rem;">🚀</span>
                <span class="gradient-text" style="font-size: 1.5rem; font-weight: 700;">TuoKit</span>
            </div>
            """, unsafe_allow_html=True)
        
        with col2:
            st.markdown(f"""
            <div style="margin-top: 25px;">
                <span style="color: #9e9e9e;">{get_tool_count()} tools • {get_category_count()} categories</span>
            </div>
            """, unsafe_allow_html=True)
        
        with col3:
            # Dynamic model selector
            with st.container():
                ModelManager.render_model_selector(
                    key="main_nav_model",
                    help_text="AI model for all tools"
                )
    
    # Add spacer for fixed nav
    st.markdown("<div style='height: 100px;'></div>", unsafe_allow_html=True)

# Command Palette
def render_command_palette():
    """Render command palette overlay"""
    if st.session_state.show_command_palette:
        with st.container():
            col1, col2, col3 = st.columns([1, 2, 1])
            with col2:
                st.markdown("""
                <div class="command-palette" style="margin-top: 2rem;">
                    <h3 style="color: white; margin-bottom: 1rem;">🔍 Quick Search</h3>
                </div>
                """, unsafe_allow_html=True)
                
                search = st.text_input(
                    "Search tools",
                    placeholder="Type to search tools or commands...",
                    key="command_search",
                    label_visibility="collapsed"
                )
                
                if search:
                    results = search_tools(search)
                    if results:
                        for result in results[:5]:
                            if st.button(
                                f"{result['icon']} {result['name']} - {result['description'][:50]}...",
                                key=f"cmd_{result['id']}",
                                use_container_width=True
                            ):
                                st.session_state.show_command_palette = False
                                st.switch_page(f"pages/{result['file']}")
                    else:
                        st.info("No tools found matching your search")
                
                if st.button("Close (Esc)", key="close_cmd"):
                    st.session_state.show_command_palette = False
                    st.rerun()

# Tool Card Component
def render_tool_card(tool_id, tool_info, category_color="#1976d2"):
    """Render a modern tool card"""
    # Get usage stats if database is available
    uses_today = 0
    if st.session_state.db:
        try:
            uses_today = st.session_state.db.get_tool_usage_count(tool_id)
        except:
            pass
    
    card_html = f"""
    <div class="tool-card" style="height: 100%;">
        <div style="font-size: 2.5rem; margin-bottom: 1rem;">{tool_info['icon']}</div>
        <h3 style="color: white; margin-bottom: 0.5rem;">{tool_info['name']}</h3>
        <p style="color: #9e9e9e; margin-bottom: 1rem; min-height: 3rem;">{tool_info['description']}</p>
        <div style="display: flex; gap: 1rem; color: #666; font-size: 0.875rem;">
            <span>⚡ {uses_today} uses</span>
        </div>
    </div>
    """
    st.markdown(card_html, unsafe_allow_html=True)
    
    if st.button("Open Tool", key=f"open_{tool_id}", use_container_width=True):
        st.switch_page(f"pages/{tool_info['file']}")

# Main App
render_nav_bar()

# Hero Section
render_hero_section()

# Featured: SQL Toolkit Pro
st.markdown("""
<div style="background: linear-gradient(135deg, #1976d2, #1565c0); border-radius: 12px; 
     padding: 2rem; margin: 2rem 0; text-align: center;">
    <h2 style="color: white; margin-bottom: 1rem;">✨ Featured: SQL Toolkit Pro</h2>
    <p style="color: rgba(255, 255, 255, 0.9); font-size: 1.1rem; margin-bottom: 1.5rem;">
        Now with 11 powerful SQL tools - Generate, Format, Optimize, Convert to Code & more!
    </p>
    <div style="display: flex; gap: 2rem; justify-content: center; flex-wrap: wrap;">
        <div style="text-align: center;">
            <div style="font-size: 2rem; margin-bottom: 0.5rem;">🛢️</div>
            <span style="color: rgba(255, 255, 255, 0.8);">SQL Generation</span>
        </div>
        <div style="text-align: center;">
            <div style="font-size: 2rem; margin-bottom: 0.5rem;">🔧</div>
            <span style="color: rgba(255, 255, 255, 0.8);">Query Optimization</span>
        </div>
        <div style="text-align: center;">
            <div style="font-size: 2rem; margin-bottom: 0.5rem;">💻</div>
            <span style="color: rgba(255, 255, 255, 0.8);">Code Conversion</span>
        </div>
        <div style="text-align: center;">
            <div style="font-size: 2rem; margin-bottom: 0.5rem;">📚</div>
            <span style="color: rgba(255, 255, 255, 0.8);">Documentation</span>
        </div>
    </div>
</div>
""", unsafe_allow_html=True)

if st.button("🚀 Open SQL Toolkit", key="featured_sql", use_container_width=False, type="primary"):
    st.switch_page("pages/sql_toolkit_modern.py")

# Command Bar
col1, col2, col3 = st.columns([1, 3, 1])
with col2:
    st.markdown("""
    <div class="command-bar" style="background: rgba(30, 30, 40, 0.6); 
         backdrop-filter: blur(10px); border: 1px solid rgba(255, 255, 255, 0.1); 
         border-radius: 12px; padding: 1.5rem; margin-bottom: 3rem; 
         display: flex; align-items: center; gap: 1rem;">
        <span style="color: #666; font-size: 1.2rem;">⌘</span>
    </div>
    """, unsafe_allow_html=True)
    
    # Place input over the command bar
    search_query = st.text_input(
        "Quick search",
        placeholder="Quick search: Try 'sql optimize' or 'ruby debug'...",
        key="quick_search",
        label_visibility="collapsed"
    )
    
    st.caption("💡 Press `/` to focus • Press `Ctrl+K` for command palette")

# Handle search
if search_query:
    results = search_tools(search_query)
    if results:
        st.subheader(f"Search Results for '{search_query}'")
        cols = st.columns(3)
        for idx, result in enumerate(results[:6]):
            with cols[idx % 3]:
                render_tool_card(result['id'], result)
    else:
        st.info("No tools found matching your search")
else:
    # Show categories with tool cards
    for category_name, category_data in NAVIGATION_CATEGORIES.items():
        st.markdown(f"""
        <h2 style="font-size: 1.5rem; margin: 2rem 0 1rem 0; color: #ffffff;">
            {category_data.get('icon', '📁')} {category_name}
        </h2>
        <p style="color: #9e9e9e; margin-bottom: 2rem;">{category_data['description']}</p>
        """, unsafe_allow_html=True)
        
        # Display tools in grid
        tools = list(category_data["tools"].items())
        cols = st.columns(3)
        
        for idx, (tool_id, tool_info) in enumerate(tools):
            with cols[idx % 3]:
                render_tool_card(tool_id, tool_info, category_data.get('color', '#1976d2'))

# Analytics Section
if st.session_state.db:
    st.markdown("""
    <h2 style="font-size: 1.5rem; margin: 3rem 0 2rem 0; color: #ffffff;">
        📈 Today's Activity
    </h2>
    """, unsafe_allow_html=True)
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        total_queries = len(st.session_state.db.get_recent_queries(100))
        st.metric("Total Queries", total_queries)
    
    with col2:
        knowledge_count = st.session_state.db.get_knowledge_count()
        st.metric("Knowledge Items", knowledge_count)
    
    with col3:
        active_tools = get_tool_count()
        st.metric("Active Tools", active_tools)
    
    with col4:
        categories = get_category_count()
        st.metric("Categories", categories)

# Floating Action Button
st.markdown("""
<div class="fab" style="position: fixed; bottom: 2rem; right: 2rem; width: 60px; height: 60px;
     background: linear-gradient(135deg, #1976d2, #4caf50); border-radius: 50%;
     display: flex; align-items: center; justify-content: center; color: white;
     font-size: 24px; cursor: pointer; box-shadow: 0 4px 20px rgba(25, 118, 210, 0.4);
     z-index: 999;" onclick="alert('New tool creation coming soon!')">
    +
</div>
""", unsafe_allow_html=True)

# Handle command palette toggle
if st.button("Open Command Palette (Ctrl+K)", key="cmd_palette_btn", type="secondary"):
    st.session_state.show_command_palette = True
    st.rerun()

# Render command palette if active
render_command_palette()

# Footer
st.markdown("---")
st.markdown("""
<div style="text-align: center; color: #666; padding: 2rem 0;">
    <p>Built with ❤️ using Streamlit and AI • TuoKit v1.0</p>
</div>
""", unsafe_allow_html=True)
