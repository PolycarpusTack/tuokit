# Add this to your app.py sidebar section

# Experimental Features Section
st.sidebar.divider()
st.sidebar.subheader("🧪 Experimental Features")

# Feature toggle with info
use_next_gen = st.sidebar.toggle(
    "Use Next-Gen Tools",
    key="use_next_gen_tools",
    help="Try unified tools with all features preserved"
)

if use_next_gen:
    st.sidebar.success("✅ Using next-gen tools")
    st.sidebar.caption("Report issues to improve!")
else:
    st.sidebar.info("Using stable tools")

# In your navigation section, conditionally show tools:
if st.session_state.get('use_next_gen_tools', False):
    # Next-gen tools
    st.page_link("pages/sql_toolkit_next.py", label="🚀 SQL Toolkit", icon="🚀")
    st.page_link("pages/agent_next.py", label="🚀 AI Agent", icon="🚀")
else:
    # Current tools
    st.page_link("pages/sql_generator.py", label="🛢️ SQL Generator", icon="🛢️")
    st.page_link("pages/sql_optimizer.py", label="⚡ SQL Optimizer", icon="⚡")
    st.page_link("pages/sql_pipeline.py", label="🔄 SQL Pipeline", icon="🔄")
    st.page_link("pages/agent_lite.py", label="🤖 Agent Lite", icon="🤖")
