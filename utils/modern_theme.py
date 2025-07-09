"""
TuoKit Modern UI Theme
Quick implementation of the new design system
Add this to your app.py or create as utils/theme.py
"""

import streamlit as st

def apply_modern_theme():
    """Apply the modern dark theme with animations"""
    st.markdown("""
    <style>
        /* Import Inter font */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        
        /* Dark theme base */
        .stApp {
            background: linear-gradient(135deg, #0a0a0a 0%, #1a1a2e 50%, #16213e 100%);
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
        }
        
        /* Hide default Streamlit branding */
        #MainMenu {visibility: hidden;}
        footer {visibility: hidden;}
        header {visibility: hidden;}
        
        /* Modern navigation bar */
        .main-nav {
            position: fixed;
            top: 0;
            left: 0;
            right: 0;
            height: 60px;
            background: rgba(20, 20, 30, 0.8);
            backdrop-filter: blur(20px);
            border-bottom: 1px solid rgba(255, 255, 255, 0.1);
            z-index: 999;
            display: flex;
            align-items: center;
            padding: 0 2rem;
        }
        
        /* Content padding to account for fixed nav */
        .main > div {
            padding-top: 80px;
        }
        
        /* Tool cards */
        .tool-card {
            background: rgba(30, 30, 40, 0.4);
            backdrop-filter: blur(10px);
            border: 1px solid rgba(255, 255, 255, 0.1);
            border-radius: 16px;
            padding: 2rem;
            transition: all 0.3s ease;
            cursor: pointer;
            margin-bottom: 1rem;
        }
        
        .tool-card:hover {
            transform: translateY(-4px);
            border-color: rgba(25, 118, 210, 0.5);
            box-shadow: 0 8px 32px rgba(25, 118, 210, 0.2);
        }
        
        /* Gradient text */
        .gradient-text {
            background: linear-gradient(135deg, #1976d2, #4caf50);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            font-weight: 700;
        }
        
        /* Modern buttons */
        .stButton > button {
            background: linear-gradient(135deg, #1976d2, #4caf50);
            color: white;
            border: none;
            border-radius: 8px;
            padding: 0.5rem 1.5rem;
            font-weight: 600;
            transition: all 0.3s ease;
        }
        
        .stButton > button:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 20px rgba(25, 118, 210, 0.4);
        }
        
        /* Input fields */
        .stTextInput > div > div > input {
            background: rgba(30, 30, 40, 0.6);
            border: 1px solid rgba(255, 255, 255, 0.1);
            border-radius: 8px;
            color: white;
            padding: 0.75rem 1rem;
        }
        
        .stTextInput > div > div > input:focus {
            border-color: rgba(25, 118, 210, 0.5);
            box-shadow: 0 0 0 3px rgba(25, 118, 210, 0.2);
        }
        
        /* Metrics */
        [data-testid="metric-container"] {
            background: rgba(30, 30, 40, 0.4);
            border: 1px solid rgba(255, 255, 255, 0.1);
            border-radius: 12px;
            padding: 1rem;
        }
        
        /* Animations */
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(20px); }
            to { opacity: 1; transform: translateY(0); }
        }
        
        .element-container {
            animation: fadeIn 0.6s ease-out;
        }
        
        /* Command palette styles */
        .command-palette {
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            background: rgba(20, 20, 30, 0.95);
            backdrop-filter: blur(20px);
            border: 1px solid rgba(255, 255, 255, 0.2);
            border-radius: 12px;
            padding: 1rem;
            width: 600px;
            max-width: 90vw;
            box-shadow: 0 20px 60px rgba(0, 0, 0, 0.5);
            z-index: 1000;
        }
        
        /* Loading animation */
        .loading {
            background: linear-gradient(90deg, #1a1a2e 0%, #2a2a3e 50%, #1a1a2e 100%);
            background-size: 200% 100%;
            animation: shimmer 2s infinite;
        }
        
        @keyframes shimmer {
            0% { background-position: -200% 0; }
            100% { background-position: 200% 0; }
        }
    </style>
    """, unsafe_allow_html=True)

def render_hero_section():
    """Render the hero section with gradient text"""
    st.markdown("""
    <div style="text-align: center; padding: 4rem 0;">
        <h1 class="gradient-text" style="font-size: 4rem; margin-bottom: 1rem;">
            Your AI Development Companion
        </h1>
        <p style="font-size: 1.5rem; color: #9e9e9e;">
            42 specialized tools to accelerate your coding workflow
        </p>
    </div>
    """, unsafe_allow_html=True)

def render_tool_card(icon, title, description, uses_today=0):
    """Render a modern tool card"""
    st.markdown(f"""
    <div class="tool-card">
        <div style="font-size: 2.5rem; margin-bottom: 1rem;">{icon}</div>
        <h3 style="color: white; margin-bottom: 0.5rem;">{title}</h3>
        <p style="color: #9e9e9e; margin-bottom: 1rem;">{description}</p>
        <div style="display: flex; gap: 1rem; color: #666; font-size: 0.875rem;">
            <span>⚡ {uses_today} uses today</span>
        </div>
    </div>
    """, unsafe_allow_html=True)

def render_command_palette_trigger():
    """Render the command palette trigger"""
    if st.session_state.get('show_command_palette', False):
        search = st.text_input(
            "",
            placeholder="Type to search tools or commands...",
            key="command_search",
            label_visibility="collapsed"
        )
        
        if search:
            # Implement fuzzy search here
            st.info(f"Searching for: {search}")
    
    # Keyboard shortcut hint
    st.caption("Press `/` to open command palette")

# Example usage
if __name__ == "__main__":
    st.set_page_config(
        page_title="TuoKit - Modern UI Demo",
        page_icon="🚀",
        layout="wide"
    )
    
    # Apply theme
    apply_modern_theme()
    
    # Render hero
    render_hero_section()
    
    # Command palette
    render_command_palette_trigger()
    
    # Example tool cards
    col1, col2, col3 = st.columns(3)
    
    with col1:
        render_tool_card(
            "🗄️",
            "SQL Toolkit",
            "Generate, optimize, and analyze SQL queries with AI",
            523
        )
    
    with col2:
        render_tool_card(
            "💎",
            "Ruby Toolkit",
            "Ruby development tools and performance optimization",
            287
        )
    
    with col3:
        render_tool_card(
            "📚",
            "Knowledge Base",
            "Capture and visualize your AI-generated insights",
            156
        )
