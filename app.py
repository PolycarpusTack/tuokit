import streamlit as st
from utils import OllamaManager, DatabaseManager, get_system_stats

# Initialize session state
if "selected_model" not in st.session_state:
    st.session_state.selected_model = "deepseek-coder:6.7b"
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed. Please check your configuration: {e}")
        st.session_state.db = None

# Check for first run
if "first_run_checked" not in st.session_state:
    st.session_state.first_run_checked = True
    # Check if user should see onboarding
    if st.session_state.db:
        try:
            # Simple check: if no queries exist, likely first run
            count = st.session_state.db.get_knowledge_count()
            recent = st.session_state.db.get_recent_queries(limit=1)
            if count == 0 and len(recent) == 0:
                st.switch_page("pages/onboarding_wizard.py")
        except:
            pass

# Page configuration
st.set_page_config(
    page_title="TuoKit Dashboard",
    page_icon="🧠",
    layout="wide"
)

# --- Sidebar ---
with st.sidebar:
    st.title("TuoKit Control Panel")
    
    # Model selection
    st.subheader("AI Engine")
    model_options = ["deepseek-coder:6.7b", "deepseek-r1:6.7b", "deepseek-r1:1.5b"]
    st.selectbox("Active Model", model_options, key="selected_model")    
    # Quick actions
    st.divider()
    st.subheader("Quick Actions")
    if st.button("🔄 Check Ollama Status", use_container_width=True):
        st.session_state.ollama_status = OllamaManager.get_status()
    
    if st.button("📊 Update Stats", use_container_width=True):
        st.session_state.system_stats = get_system_stats()
        if st.session_state.db:
            st.session_state.recent_queries = st.session_state.db.get_recent_queries()
        else:
            st.session_state.recent_queries = []
    
    # System info
    st.divider()
    st.subheader("System Info")
    stats = get_system_stats() if "system_stats" not in st.session_state else st.session_state.system_stats
    st.metric("CPU Usage", stats["cpu"])
    st.metric("Memory Usage", stats["memory"])
    
    # Navigation
    st.divider()
    st.subheader("Tools Navigation")
    st.page_link("app.py", label="📊 Dashboard", icon="📊")
    st.page_link("pages/code_tools.py", label="💻 Code Tools", icon="💻")
    st.page_link("pages/doc_tools.py", label="📄 Document Tools", icon="📄")
    st.page_link("pages/study_guide_generator.py", label="📚 Study Guide", icon="📚")
    st.page_link("pages/edu_mind.py", label="🎓 EduMind", icon="🎓")
    st.page_link("pages/sql_generator.py", label="🛢️ SQL Generator", icon="🛢️")
    st.page_link("pages/sql_optimizer.py", label="🔍 SQL Optimizer", icon="🔍")
    st.page_link("pages/sql_pipeline.py", label="🔄 SQL Pipeline", icon="🔄")
    
    # SmallTalk & Rails Development Tools
    st.caption("SmallTalk & Rails")
    st.page_link("pages/smalltalk_explainer.py", label="🧑‍🏫 SmallTalk Explainer", icon="🧑‍🏫")
    st.page_link("pages/smalltalk_class_gen.py", label="🏗️ ST Class Generator", icon="🏗️")
    st.page_link("pages/morphic_builder.py", label="🎨 Morphic UI Builder", icon="🎨")
    st.page_link("pages/seaside_generator.py", label="🌊 Seaside Generator", icon="🌊")
    st.page_link("pages/smalltalk_refactorer.py", label="🔧 ST Refactorer", icon="🔧")
    st.page_link("pages/smalltalk_meta.py", label="✨ ST Metaprogramming", icon="✨")
    st.page_link("pages/image_browser.py", label="🔍 Image Browser", icon="🔍")
    st.page_link("pages/smalltalk_snippets.py", label="📚 ST Snippets", icon="📚")
    st.page_link("pages/smalltalk_ruby_converter.py", label="🔄 ST↔Ruby Converter", icon="🔄")
    
    st.caption("Rails Tools")
    st.page_link("pages/rails_scaffold.py", label="⚡ Rails Scaffold", icon="⚡")
    st.page_link("pages/rails_debugger.py", label="🐞 Rails Debugger", icon="🐞")
    st.page_link("pages/ruby_profiler.py", label="⚡ Ruby Performance Profiler", icon="⚡")
    st.page_link("pages/rails_system_tests.py", label="🧪 Rails System Tests", icon="🧪")
    st.page_link("pages/ruby_pattern_matching.py", label="🎯 Pattern Matching Explorer", icon="🎯")
    st.page_link("pages/ruby_ractors.py", label="⚡ Concurrency Advisor", icon="⚡")
    st.page_link("pages/rails_graphql.py", label="🚀 GraphQL API Builder", icon="🚀")
    st.page_link("pages/ruby_memory_optimizer.py", label="🧠 Memory Optimizer", icon="🧠")
    st.page_link("pages/view_components.py", label="🧩 View Components", icon="🧩")
    st.page_link("pages/ruby_c_extensions.py", label="🛠️ C Extensions", icon="🛠️")
    st.page_link("pages/rails_upgrader.py", label="🆙 Rails Upgrader", icon="🆙")
    st.page_link("pages/ruby_katas.py", label="🥋 Ruby Katas", icon="🥋")
    
    st.divider()
    st.page_link("pages/regex_tool.py", label="🔍 Regex Generator", icon="🔍")
    st.page_link("pages/error_tool.py", label="🐞 Error Decoder", icon="🐞")
    st.page_link("pages/exception_advisor.py", label="🛡️ Exception Advisor", icon="🛡️")
    st.page_link("pages/crash_analyzer.py", label="🚨 Crash Analyzer", icon="🚨")
    st.page_link("pages/knowledge_lib.py", label="📚 Knowledge Library", icon="📚")
    st.page_link("pages/help_guide.py", label="❓ Help Guide", icon="❓")
    
    st.divider()
    if st.button("🧙‍♂️ Tutorial", use_container_width=True):
        st.switch_page("pages/onboarding_wizard.py")
# --- Main Dashboard ---
st.title("🧠 TuoKit - AI Developer Portal")
st.caption("Central hub for your AI-powered development tools")

# Status Cards
col1, col2, col3 = st.columns(3)
with col1:
    status = OllamaManager.get_status() if "ollama_status" not in st.session_state else st.session_state.ollama_status
    status_icon = "✅" if status["running"] else "❌"
    st.metric("Ollama Status", f"{status_icon} {'Running' if status['running'] else 'Stopped'}")
    
with col2:
    st.metric("Loaded Models", status["model_count"])
    
with col3:
    knowledge_count = st.session_state.db.get_knowledge_count() if st.session_state.db else 0
    st.metric("Knowledge Units", knowledge_count)

# System Stats
st.subheader("Resource Utilization")
if "system_stats" in st.session_state:
    stats = st.session_state.system_stats
    col1, col2 = st.columns(2)
    with col1:
        st.progress(float(stats["cpu"].rstrip('%'))/100, text=f"CPU: {stats['cpu']}")    with col2:
        st.progress(float(stats["memory"].rstrip('%'))/100, text=f"Memory: {stats['memory']}")
else:
    st.button("Load System Stats")

# Recent Activity
st.subheader("Recent Activity")
if "recent_queries" not in st.session_state:
    if st.session_state.db:
        st.session_state.recent_queries = st.session_state.db.get_recent_queries()
    else:
        st.session_state.recent_queries = []

if st.session_state.recent_queries:
    for qid, tool, prompt, timestamp in st.session_state.recent_queries:
        tool_icon = "💻" if "coder" in tool else "📄"
        with st.expander(f"{tool_icon} {timestamp} - {tool}"):
            st.caption(f"Query ID: #{qid}")
            st.code(prompt[:200] + ("..." if len(prompt) > 200 else ""))
else:
    st.info("No recent activity yet")

# Quick Start Tools
st.divider()
st.subheader("Quick Start Tools")

# First row of tools
tt_col1, tt_col2, tt_col3 = st.columns(3)
with tt_col1:
    if st.button("💡 Explain Code", use_container_width=True):
        st.switch_page("pages/code_tools.py")

with tt_col2:
    if st.button("📄 Analyze Document", use_container_width=True):
        st.switch_page("pages/doc_tools.py")

with tt_col3:
    if st.button("🔍 Regex Generator", use_container_width=True):
        st.switch_page("pages/regex_tool.py")

# Second row of tools
tt2_col1, tt2_col2, tt2_col3 = st.columns(3)
with tt2_col1:
    if st.button("📚 Knowledge Library", use_container_width=True):
        st.switch_page("pages/knowledge_lib.py")

with tt2_col2:
    if st.button("📚 Study Guide", use_container_width=True):
        st.switch_page("pages/study_guide_generator.py")

with tt2_col3:
    if st.button("🐞 Error Decoder", use_container_width=True):
        st.switch_page("pages/error_tool.py")

# Third row of tools
tt3_col1, tt3_col2, tt3_col3 = st.columns(3)
with tt3_col1:
    if st.button("🎓 EduMind", use_container_width=True):
        st.switch_page("pages/edu_mind.py")

with tt3_col2:
    if st.button("🛢️ SQL Tools", use_container_width=True):
        st.switch_page("pages/sql_generator.py")

with tt3_col3:
    if st.button("🧙‍♂️ Tutorial", use_container_width=True):
        st.switch_page("pages/onboarding_wizard.py")

# New row for debugging tools
st.caption("**Debugging & Analysis Tools**")
dbg_col1, dbg_col2, dbg_col3 = st.columns(3)
with dbg_col1:
    if st.button("🚨 Crash Analyzer", use_container_width=True):
        st.switch_page("pages/crash_analyzer.py")

with dbg_col2:
    if st.button("🐞 Rails Debugger", use_container_width=True):
        st.switch_page("pages/rails_debugger.py")

with dbg_col3:
    pass  # Space for future debugging tool

# Fourth row - SmallTalk & Rails tools
st.caption("**SmallTalk & Rails Development**")
tt4_col1, tt4_col2, tt4_col3 = st.columns(3)
with tt4_col1:
    if st.button("🧑‍🏫 SmallTalk Explainer", use_container_width=True):
        st.switch_page("pages/smalltalk_explainer.py")

with tt4_col2:
    if st.button("⚡ Rails Scaffold", use_container_width=True):
        st.switch_page("pages/rails_scaffold.py")

with tt4_col3:
    if st.button("🔄 Code Converter", use_container_width=True):
        st.switch_page("pages/smalltalk_ruby_converter.py")

# Fifth row - New SmallTalk tools
tt5_col1, tt5_col2, tt5_col3 = st.columns(3)
with tt5_col1:
    if st.button("🏗️ Class Generator", use_container_width=True):
        st.switch_page("pages/smalltalk_class_gen.py")

with tt5_col2:
    if st.button("🎨 Morphic UI", use_container_width=True):
        st.switch_page("pages/morphic_builder.py")

with tt5_col3:
    if st.button("✨ Metaprogramming", use_container_width=True):
        st.switch_page("pages/smalltalk_meta.py")

# Sixth row - Ruby Performance & Testing tools
st.caption("**Ruby Performance & Testing**")
tt6_col1, tt6_col2, tt6_col3 = st.columns(3)
with tt6_col1:
    if st.button("⚡ Ruby Profiler", use_container_width=True):
        st.switch_page("pages/ruby_profiler.py")

with tt6_col2:
    if st.button("🧪 System Tests", use_container_width=True):
        st.switch_page("pages/rails_system_tests.py")

with tt6_col3:
    if st.button("🎯 Pattern Matching", use_container_width=True):
        st.switch_page("pages/ruby_pattern_matching.py")

# Seventh row - Advanced Ruby tools
st.caption("**Advanced Ruby Features**")
tt7_col1, tt7_col2, tt7_col3 = st.columns(3)
with tt7_col1:
    if st.button("⚡ Ractors & Concurrency", use_container_width=True):
        st.switch_page("pages/ruby_ractors.py")

with tt7_col2:
    if st.button("🚀 GraphQL Builder", use_container_width=True):
        st.switch_page("pages/rails_graphql.py")

with tt7_col3:
    if st.button("🧠 Memory Optimizer", use_container_width=True):
        st.switch_page("pages/ruby_memory_optimizer.py")

# Eighth row - Professional Ruby Development
st.caption("**Professional Ruby Development**")
tt8_col1, tt8_col2, tt8_col3 = st.columns(3)
with tt8_col1:
    if st.button("🧩 View Components", use_container_width=True):
        st.switch_page("pages/view_components.py")

with tt8_col2:
    if st.button("🛠️ C Extensions", use_container_width=True):
        st.switch_page("pages/ruby_c_extensions.py")

with tt8_col3:
    if st.button("🆙 Rails Upgrader", use_container_width=True):
        st.switch_page("pages/rails_upgrader.py")

# Ninth row - Learning & Training
st.caption("**Learning & Training**")
tt9_col1, tt9_col2, tt9_col3 = st.columns(3)
with tt9_col1:
    if st.button("🥋 Ruby Katas", use_container_width=True):
        st.switch_page("pages/ruby_katas.py")

with tt9_col2:
    pass  # Space for future tool

with tt9_col3:
    pass  # Space for future tool

# Footer
st.divider()
footer_col1, footer_col2 = st.columns([4, 1])
with footer_col1:
    st.caption("TuoKit v1.4.0 | Local AI Development Suite")
with footer_col2:
    if st.button("❓ Help", use_container_width=True):
        st.switch_page("pages/help_guide.py")