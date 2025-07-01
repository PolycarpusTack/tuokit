#!/usr/bin/env python3
"""
Quick script to show how to integrate Agent Lite into TuoKit
Run this to see the required changes (doesn't modify files)
"""

def show_integration_steps():
    print("🔧 TuoKit Agent Lite Integration Guide")
    print("=" * 50)
    
    print("\n1. Add to app.py navigation:")
    print("-" * 30)
    print("""
# In app.py, add to the navigation section:

# After the existing page list
pages = {
    "Dashboard": "📊",
    "Code Tools": "💻",
    "SQL Tools": "🗄️",
    "Document Tools": "📄",
    "Knowledge Library": "📚",
    "Agent Portal": "🤖",      # Existing robust agents
    "Agent Lite": "🚀",        # NEW: Lite agent system
    "Help": "❓"
}

# Or in st.sidebar navigation:
page = st.sidebar.radio(
    "Navigation",
    ["Dashboard", "Code Tools", "SQL Tools", "Document Tools", 
     "Knowledge Library", "Agent Portal", "Agent Lite", "Help"]
)

# Add page routing:
elif page == "Agent Lite":
    from pages import agent_lite
    agent_lite.show()  # The page has its own show() function
""")

    print("\n2. Run database migration:")
    print("-" * 30)
    print("""
psql -U ollama_user -d ollama_knowledge -f database_migration_lite_agents.sql

# This adds:
# - pipelines table
# - pipeline_templates table with 3 starter templates
# - Analytics views
""")

    print("\n3. Install any missing dependencies:")
    print("-" * 30)
    print("""
pip install -r requirements.txt

# Note: psutil was added for better system stats (optional)
""")

    print("\n4. Test the integration:")
    print("-" * 30)
    print("""
# Run the test suite
python test_agent_lite.py

# Or test manually:
1. Start TuoKit: streamlit run app.py
2. Navigate to "Agent Lite" page
3. Try the example pipelines
""")

    print("\n5. Features available:")
    print("-" * 30)
    print("""
✅ Pipeline Automator
   - Visual pipeline builder
   - Tool chaining with parameters
   - One-click example workflows
   - Auto-save successful pipelines

✅ Educational Companion
   - Real-time guidance
   - Context-aware tips
   - Common mistake warnings
   - Learning history export
""")

    print("\n📚 Documentation:")
    print("-" * 30)
    print("""
- AGENT_LITE_README.md - Full documentation
- AGENT_SYSTEMS_COMPARISON.md - Robust vs Lite comparison
- test_agent_lite.py - Test suite with examples
""")

    print("\n✨ That's it! The Lite Agent System is ready to use.")
    print("\nKey benefit: Users can create multi-tool workflows visually")
    print("without understanding complex agent concepts.\n")

if __name__ == "__main__":
    show_integration_steps()
