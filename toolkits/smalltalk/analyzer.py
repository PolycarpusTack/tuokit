# toolkits/smalltalk/analyzer.py
"""Main SmallTalk Toolkit analyzer class"""

import streamlit as st
from datetime import datetime
from utils.tool_base import TuoKitToolBase
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
from .config import TOOL_CATEGORIES, DEFAULT_MODEL
from .ui_components import (
    show_class_generator, show_code_explainer, show_snippet_generator,
    show_refactorer, show_ruby_converter, show_seaside_generator,
    show_metaprogramming
)

class SmallTalkToolkit(TuoKitToolBase, OllamaToolBase):
    """Unified SmallTalk development toolkit"""
    
    def __init__(self):
        # Initialize TuoKitToolBase
        TuoKitToolBase.__init__(
            self,
            tool_name="SmallTalk Development Toolkit",
            tool_description="Comprehensive suite for SmallTalk development"
        )
        
        # Initialize OllamaToolBase  
        OllamaToolBase.__init__(
            self,
            tool_name="smalltalk_toolkit",
            default_model=st.session_state.get("selected_model", DEFAULT_MODEL)
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
        
        if "selected_tool" not in st.session_state:
            st.session_state.selected_tool = None
        
        if "selected_model" not in st.session_state:
            st.session_state.selected_model = DEFAULT_MODEL
    
    def run(self):
        """Main entry point for SmallTalk toolkit"""
        # Page configuration
        st.set_page_config(
            page_title="SmallTalk Toolkit - TuoKit",
            page_icon="🚀",
            layout="wide"
        )
        
        # Header
        st.title("🚀 SmallTalk Development Toolkit")
        st.caption("Comprehensive suite combining all SmallTalk tools into one unified interface")
        
        # Tool selection
        self.render_tool_selection()
        
        # Display selected tool
        if st.session_state.selected_tool:
            self.render_selected_tool()
        
        # Sidebar
        with st.sidebar:
            self.render_sidebar()
    
    def render_tool_selection(self):
        """Render tool selection grid"""
        st.subheader("🎯 Select a Tool")
        
        # Create grid layout
        cols = st.columns(3)
        
        for idx, (tool_name, tool_info) in enumerate(TOOL_CATEGORIES.items()):
            with cols[idx % 3]:
                if st.button(
                    f"{tool_info['icon']} {tool_name}",
                    help=tool_info['description'],
                    use_container_width=True,
                    key=f"tool_{tool_info['key']}"
                ):
                    st.session_state.selected_tool = tool_info['key']
                    st.rerun()
        
        # Show description for selected tool
        if st.session_state.selected_tool:
            for tool_name, tool_info in TOOL_CATEGORIES.items():
                if tool_info['key'] == st.session_state.selected_tool:
                    st.info(f"**{tool_name}**: {tool_info['description']}")
                    break
    
    def render_selected_tool(self):
        """Render the selected tool interface"""
        st.divider()
        
        # Map tool keys to display functions
        tool_map = {
            "class_generator": show_class_generator,
            "code_explainer": show_code_explainer,
            "snippet_generator": show_snippet_generator,
            "refactorer": show_refactorer,
            "ruby_converter": show_ruby_converter,
            "seaside_generator": show_seaside_generator,
            "metaprogramming": show_metaprogramming
        }
        
        # Display the selected tool
        if st.session_state.selected_tool in tool_map:
            tool_func = tool_map[st.session_state.selected_tool]
            tool_func(self, st.session_state.db)
            
            # Auto-capture knowledge for any generated content
            if hasattr(self, 'last_prompt') and hasattr(self, 'last_response'):
                self.auto_capture_knowledge(
                    prompt=self.last_prompt,
                    response=self.last_response,
                    metadata={
                        "tool": st.session_state.selected_tool,
                        "timestamp": datetime.now().isoformat()
                    }
                )
    
    def render_sidebar(self):
        """Render sidebar content"""
        st.subheader("⚙️ Settings")
        
        # Model selection
        self.model_selector()
        
        st.divider()
        
        # Quick links
        st.subheader("🔗 Quick Links")
        st.markdown("""
        - [SmallTalk Documentation](https://wiki.squeak.org/squeak)
        - [Pharo By Example](https://pharo.org/documentation)
        - [Seaside Framework](https://seaside.st/)
        - [VisualWorks Guide](https://www.cincomsmalltalk.com/)
        """)
        
        st.divider()
        
        # Recent activity
        if st.session_state.db:
            st.subheader("📚 Recent Activity")
            recent = self.get_recent_queries(limit=5)
            if recent:
                for query in recent[:5]:
                    with st.expander(f"{query['timestamp'][:10]} - {query['tool'][:20]}..."):
                        st.code(query['prompt'][:200] + "...", language="text")
            else:
                st.info("No recent activity")
        
        st.divider()
        
        # Tips
        st.subheader("💡 Pro Tips")
        tips = [
            "Use Class Generator for quick prototypes",
            "Explain code before refactoring",
            "Test Seaside components locally",
            "Metaprogramming can solve complex problems elegantly"
        ]
        
        import random
        st.info(random.choice(tips))
    
    def save_to_knowledge_base(self, db, prompt, response, metadata=None):
        """Save to knowledge base with metadata"""
        if db:
            try:
                # Log the query
                query_id = db.log_query(
                    tool="smalltalk_toolkit",
                    model=self.model,
                    prompt=prompt,
                    response=response
                )
                
                # Store for auto-capture
                self.last_prompt = prompt
                self.last_response = response
                
                return query_id
            except Exception as e:
                st.error(f"Error saving to knowledge base: {e}")
                return None
    
    def log_to_database(self, db, input_text, output_text):
        """Log interaction to database"""
        return self.save_to_knowledge_base(db, input_text, output_text)
    
    def get_recent_queries(self, limit=10):
        """Get recent queries from database"""
        if st.session_state.db:
            try:
                recent = st.session_state.db.get_recent_queries(limit=limit)
                return [
                    {
                        'id': q[0],
                        'tool': q[1],
                        'timestamp': q[2],
                        'prompt': q[3],
                        'response': q[4]
                    }
                    for q in recent if q[1] == 'smalltalk_toolkit'
                ]
            except:
                return []
        return []