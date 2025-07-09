import streamlit as st

# Initialize session state
from utils import DatabaseManager, apply_modern_theme
from utils.navigation import NAVIGATION_CATEGORIES, get_tool_count, get_category_count
import random
from datetime import datetime

# Page config
st.set_page_config(
    page_title="TuoKit Home",
    page_icon="🏠",
    layout="wide",
    initial_sidebar_state="collapsed"
)

# Apply modern theme
apply_modern_theme()

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🏠 Welcome to TuoKit
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Your personal AI development companion
    </p>
</div>
""", unsafe_allow_html=True)

# Initialize database
db = st.session_state.get('db')

# Quick Stats
col1, col2, col3, col4 = st.columns(4)

with col1:
    st.metric(
        "Active Tools",
        get_tool_count(),
        help="Total number of available tools"
    )

with col2:
    st.metric(
        "Categories",
        get_category_count(),
        help="Tool categories"
    )

with col3:
    if db:
        knowledge_count = db.get_knowledge_count()
        st.metric(
            "Knowledge Items",
            knowledge_count,
            help="Saved insights and patterns"
        )
    else:
        st.metric("Knowledge Items", "N/A")

with col4:
    current_hour = datetime.now().hour
    if current_hour < 12:
        greeting = "Good Morning! ☀️"
    elif current_hour < 18:
        greeting = "Good Afternoon! 🌤️"
    else:
        greeting = "Good Evening! 🌙"
    st.metric("Time", greeting)

# Divider
st.markdown("---")

# Featured Tools Section
st.markdown("""
<h2 style="font-size: 1.8rem; margin-bottom: 1.5rem; color: #ffffff;">
    ⭐ Featured Tools
</h2>
""", unsafe_allow_html=True)

# Get some featured tools
featured_tools = [
    ("sql_toolkit", "SQL Toolkit", "🛢️", "Complete SQL workflow - generate, optimize, explain"),
    ("ruby_toolkit", "Ruby Toolkit", "💎", "Ruby code optimization and best practices"),
    ("code_tools", "Code Explainer", "💡", "Explain and understand code in any language"),
    ("knowledge_lib", "Knowledge Library", "📚", "Browse AI-generated knowledge base")
]

cols = st.columns(len(featured_tools))
for idx, (tool_id, name, icon, desc) in enumerate(featured_tools):
    with cols[idx]:
        st.markdown(f"""
        <div class="tool-card" style="text-align: center; padding: 1.5rem;">
            <div style="font-size: 3rem; margin-bottom: 0.5rem;">{icon}</div>
            <h4 style="color: white; margin-bottom: 0.5rem;">{name}</h4>
            <p style="color: #9e9e9e; font-size: 0.875rem;">{desc}</p>
        </div>
        """, unsafe_allow_html=True)
        
        if st.button("Open", key=f"featured_{tool_id}", use_container_width=True):
            # Find the correct file
            for cat_data in NAVIGATION_CATEGORIES.values():
                if tool_id in cat_data["tools"]:
                    st.switch_page(f"pages/{cat_data['tools'][tool_id]['file']}")
                    break

# Recent Activity
if db:
    st.markdown("""
    <h2 style="font-size: 1.8rem; margin: 2rem 0 1.5rem 0; color: #ffffff;">
        📈 Recent Activity
    </h2>
    """, unsafe_allow_html=True)
    
    recent_queries = db.get_recent_queries(limit=5)
    if recent_queries:
        for query in recent_queries:
            query_id, tool, prompt, created_at = query
            
            # Format time
            time_str = created_at.strftime("%I:%M %p") if created_at.date() == datetime.now().date() else created_at.strftime("%b %d")
            
            # Create activity card
            st.markdown(f"""
            <div style="background: rgba(30, 30, 40, 0.4); border-radius: 8px; 
                 padding: 1rem; margin-bottom: 0.5rem; border-left: 3px solid #1976d2;">
                <div style="display: flex; justify-content: space-between; align-items: center;">
                    <div>
                        <strong style="color: #1976d2;">{tool}</strong>
                        <span style="color: #666; margin-left: 1rem;">{time_str}</span>
                    </div>
                </div>
                <div style="color: #9e9e9e; margin-top: 0.5rem; font-size: 0.875rem;">
                    {prompt[:100]}{'...' if len(prompt) > 100 else ''}
                </div>
            </div>
            """, unsafe_allow_html=True)
    else:
        st.info("No recent activity yet. Start using tools to see your history here!")

# Quick Tips
st.markdown("""
<h2 style="font-size: 1.8rem; margin: 2rem 0 1.5rem 0; color: #ffffff;">
    💡 Quick Tips
</h2>
""", unsafe_allow_html=True)

tips = [
    "Press `/` to quickly search for tools",
    "Use `Ctrl+K` to open the command palette",
    "Your knowledge is automatically saved and searchable",
    "Check out SQL Toolkit for database query optimization",
    "Ruby Toolkit includes Rails-specific helpers"
]

# Random tip
selected_tip = random.choice(tips)
st.info(f"💡 **Tip**: {selected_tip}")

# Categories Overview
st.markdown("""
<h2 style="font-size: 1.8rem; margin: 2rem 0 1.5rem 0; color: #ffffff;">
    📂 Browse by Category
</h2>
""", unsafe_allow_html=True)

# Display categories in a grid
cols = st.columns(3)
for idx, (category_name, category_data) in enumerate(NAVIGATION_CATEGORIES.items()):
    with cols[idx % 3]:
        # Category card
        st.markdown(f"""
        <div class="tool-card" style="min-height: 150px;">
            <div style="font-size: 2rem; margin-bottom: 0.5rem;">{category_data.get('icon', '📁')}</div>
            <h4 style="color: white; margin-bottom: 0.5rem;">{category_name}</h4>
            <p style="color: #9e9e9e; font-size: 0.875rem;">
                {len(category_data['tools'])} tools
            </p>
            <p style="color: #666; font-size: 0.8rem;">
                {category_data['description']}
            </p>
        </div>
        """, unsafe_allow_html=True)
        
        if st.button(f"Explore", key=f"cat_{idx}", use_container_width=True):
            st.info(f"Selected: {category_name} - Navigation temporarily disabled for testing")

# Footer
st.markdown("---")
st.markdown("""
<div style="text-align: center; color: #666; padding: 1rem 0;">
    <p>Need help? Check out the <a href="#" style="color: #1976d2;">Help Guide</a> or press `?` anytime</p>
</div>
""", unsafe_allow_html=True)
