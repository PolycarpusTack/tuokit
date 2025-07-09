"""
System Health Dashboard - Quick access to health check tools
"""

import streamlit as st
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.tool_base import TuoKitToolBase

class SystemHealth(TuoKitToolBase):
    """System health dashboard for testing"""
    
    def __init__(self):
        super().__init__(
            tool_name="System Health",
            tool_description="Health checks and testing dashboard"
        )

def main():
    st.set_page_config(
        page_title="System Health",
        page_icon="🏥",
        layout="wide"
    )
    
    tool = SystemHealth()
    
    st.title("🏥 System Health Dashboard")
    st.markdown("Quick access to health check and testing tools")
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        st.subheader("🗄️ Database Health")
        st.markdown("Check PostgreSQL connection and migration status")
        if st.button("🔗 Open Database Health Check", key="db_health"):
            st.markdown("**Run this command in terminal:**")
            st.code("streamlit run pages/database_health_check.py", language="bash")
            st.info("This will open in a new browser tab")
    
    with col2:
        st.subheader("🤖 Ollama Health")
        st.markdown("Check Ollama service and models")
        if st.button("🔗 Open Ollama Health Check", key="ollama_health"):
            st.markdown("**Run this command in terminal:**")
            st.code("streamlit run pages/ollama_health_check.py", language="bash")
            st.info("This will open in a new browser tab")
    
    with col3:
        st.subheader("🧠 Knowledge Capture Demo")
        st.markdown("Test the knowledge relationships system")
        if st.button("🔗 Open Knowledge Demo", key="knowledge_demo"):
            st.markdown("**Run this command in terminal:**")
            st.code("streamlit run pages/knowledge_capture_demo.py", language="bash")
            st.info("This will open in a new browser tab")
    
    st.divider()
    
    st.subheader("🧪 Testing Instructions")
    st.markdown("""
    **Phase 1: Database Setup**
    1. Click "Open Database Health Check" above
    2. Run the command in a new terminal
    3. Look for 95-100% completion status
    
    **Phase 2: Knowledge Capture**
    1. Click "Open Knowledge Demo" above
    2. Run the command in a new terminal  
    3. Try the demo scenarios to capture knowledge
    
    **Phase 3: Relationship Discovery**
    ```bash
    python3 scripts/discover_knowledge_relationships.py
    ```
    """)
    
    # Show current system status
    st.subheader("📊 Quick Status Check")
    
    # Test database connection
    if st.session_state.get('db'):
        try:
            with st.session_state.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT COUNT(*) FROM knowledge_units")
                    knowledge_count = cur.fetchone()[0]
                    
                    cur.execute("SELECT COUNT(*) FROM knowledge_links")
                    relationships_count = cur.fetchone()[0]
                    
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Database", "Connected ✅")
            with col2:
                st.metric("Knowledge Units", knowledge_count)
            with col3:
                st.metric("Relationships", relationships_count)
                
        except Exception as e:
            st.error(f"Database error: {e}")
    else:
        st.warning("Database not connected")

if __name__ == "__main__":
    main()