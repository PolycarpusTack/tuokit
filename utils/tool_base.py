"""
TuoKit Base Tool Class
Standardizes knowledge capture across all tools
"""

import streamlit as st
import os
from typing import Dict, Any, Optional
from .database import DatabaseManager
from .ollama import safe_ollama_generate
from .knowledge_capture import get_capture_manager

class TuoKitToolBase:
    """
    Base class for all TuoKit tools with automatic knowledge capture
    
    Features:
    - Automatic knowledge capture for all AI interactions
    - Standardized error handling
    - Consistent UI patterns
    - Built-in metrics tracking
    """
    
    def __init__(self, tool_name: str, tool_description: str):
        self.tool_name = tool_name
        self.tool_description = tool_description
        self._init_session_state()
        self._init_database()
        self._init_knowledge_capture()
        
    def _init_session_state(self):
        """Initialize session state variables"""
        if "last_query_id" not in st.session_state:
            st.session_state.last_query_id = None
        if "query_count" not in st.session_state:
            st.session_state.query_count = 0
            
    def _init_database(self):
        """Initialize database connection"""
        if "db" not in st.session_state:
            try:
                st.session_state.db = DatabaseManager()
            except Exception as e:
                st.error(f"Database connection failed: {e}")
                st.session_state.db = None
    
    def _init_knowledge_capture(self):
        """Initialize knowledge capture manager"""
        if "capture_manager" not in st.session_state:
            db = st.session_state.get("db")
            st.session_state.capture_manager = get_capture_manager(db)
            
        # Check if knowledge capture is enabled (default: True)
        self.knowledge_capture_enabled = os.getenv("ENABLE_KNOWLEDGE_CAPTURE", "true").lower() == "true"
                
    def generate_with_capture(self, 
                            prompt: str, 
                            model: str = None,
                            temperature: float = 0.7,
                            **kwargs) -> Dict[str, Any]:
        """
        Generate AI response with automatic knowledge capture
        
        Args:
            prompt: The prompt to send to the AI
            model: Model to use (defaults to session state)
            temperature: Generation temperature
            **kwargs: Additional parameters for ollama
            
        Returns:
            Dict with 'response' and 'query_id' keys
        """
        # Use model from session state or dynamic default if not provided
        if model is None:
            from utils.model_manager import ModelManager
            model = ModelManager.get_default_model()
            
        # Generate response - put temperature in options for Ollama
        options = kwargs.pop('options', {})
        if temperature != 0.7:  # Only set if not default
            options['temperature'] = temperature
            
        response = safe_ollama_generate(
            model=model,
            prompt=prompt,
            options=options,
            **kwargs
        )
        
        # Basic logging to queries table
        query_id = None
        if st.session_state.db and not response.get('error'):
            try:
                query_id = st.session_state.db.log_query(
                    tool=self.tool_name,
                    model=model,
                    prompt=prompt,
                    response=response.get('response', ''),
                    metadata=kwargs
                )
                st.session_state.last_query_id = query_id
                st.session_state.query_count += 1
            except Exception as e:
                # Log error but don't fail the generation
                print(f"Basic logging failed: {e}")
        
        # Enhanced knowledge capture using the new system
        knowledge_id = None
        if (self.knowledge_capture_enabled and 
            st.session_state.get('capture_manager') and 
            not response.get('error')):
            try:
                knowledge_id = st.session_state.capture_manager.capture(
                    tool_name=self.tool_name,
                    model=model,
                    prompt=prompt,
                    response=response.get('response', ''),
                    metadata={
                        'query_id': query_id,
                        **kwargs
                    }
                )
                if knowledge_id:
                    st.session_state.last_knowledge_id = knowledge_id
                    # Trigger relationship discovery for new knowledge
                    self._discover_relationships_for_new_knowledge(knowledge_id)
            except Exception as e:
                # Silent failure - don't disrupt user experience
                print(f"Enhanced knowledge capture failed: {e}")
                
        return {
            'response': response.get('response', ''),
            'error': response.get('error', False),
            'query_id': query_id,
            'knowledge_id': knowledge_id
        }
    
    def save_to_knowledge_base(self, 
                              title: str, 
                              content: str, 
                              category: str,
                              query_id: int = None) -> bool:
        """
        Save content to knowledge base
        
        Args:
            title: Title for the knowledge entry
            content: Content to save
            category: Category for organization
            query_id: Optional query ID to link to
            
        Returns:
            True if saved successfully
        """
        if not st.session_state.db:
            return False
            
        try:
            query_id = query_id or st.session_state.get('last_query_id')
            return st.session_state.db.save_knowledge_unit(
                query_id=query_id,
                title=title,
                content=content,
                category=category
            )
        except Exception as e:
            st.error(f"Failed to save to knowledge base: {e}")
            return False
    
    def show_knowledge_save_widget(self, 
                                  default_title: str = None,
                                  default_category: str = None,
                                  content: str = None):
        """
        Display a widget for saving to knowledge base
        
        Args:
            default_title: Default title to show
            default_category: Default category
            content: Content to save (uses last response if not provided)
        """
        if not st.session_state.get('last_query_id'):
            return
            
        with st.expander("💾 Save to Knowledge Base"):
            title = st.text_input(
                "Title", 
                value=default_title or f"{self.tool_name} - Generated Content"
            )
            
            # Get existing categories
            categories = []
            if st.session_state.db:
                categories = st.session_state.db.get_knowledge_categories() or []
            
            # Default categories
            default_categories = [
                "Code Snippet", "Algorithm", "Documentation", 
                "Error Solution", "Best Practice", "Tutorial"
            ]
            all_categories = sorted(list(set(categories + default_categories)))
            
            category = st.selectbox(
                "Category",
                all_categories,
                index=all_categories.index(default_category) if default_category in all_categories else 0
            )
            
            if st.button("💾 Save", type="primary"):
                if self.save_to_knowledge_base(title, content or "", category):
                    st.success("✅ Saved to knowledge base!")
                    st.balloons()
                else:
                    st.error("Failed to save to knowledge base")
    
    def search_knowledge(self, 
                        query: str, 
                        category: Optional[str] = None, 
                        limit: int = 10) -> list:
        """
        Search the knowledge base
        
        Args:
            query: Search query
            category: Optional category filter
            limit: Maximum results to return
            
        Returns:
            List of knowledge units matching the query
        """
        if not st.session_state.get('capture_manager'):
            return []
            
        return st.session_state.capture_manager.search(
            query=query,
            category=category,
            min_quality=30,  # Only show decent quality
            limit=limit
        )
    
    def show_knowledge_search_widget(self):
        """Display a widget for searching knowledge base"""
        if not st.session_state.get('capture_manager'):
            return
            
        with st.expander("🔍 Search Knowledge Base"):
            col1, col2 = st.columns([3, 1])
            
            with col1:
                search_query = st.text_input(
                    "Search for:", 
                    placeholder="e.g., 'python sorting', 'SQL join', 'error fix'"
                )
                
            with col2:
                categories = ["All"] + list(self._get_available_categories())
                category = st.selectbox("Category:", categories)
                category = None if category == "All" else category
                
            if search_query:
                results = self.search_knowledge(
                    query=search_query,
                    category=category,
                    limit=5
                )
                
                if results:
                    st.subheader("📚 Related Knowledge:")
                    for result in results:
                        with st.container():
                            st.markdown(f"**{result['title']}**")
                            st.caption(f"Category: {result['category']} | Quality: {result['quality_score']}/100")
                            
                            # Show content preview
                            preview = result['content'][:300]
                            if len(result['content']) > 300:
                                preview += "..."
                            st.text_area("", preview, height=100, disabled=True, key=f"preview_{result['id']}")
                            
                            if st.button(f"📋 Copy", key=f"copy_{result['id']}"):
                                st.code(result['content'])
                else:
                    st.info("No related knowledge found. Your interaction might be the first of its kind!")
    
    def show_related_knowledge_widget(self, knowledge_id: int = None):
        """Display widget showing knowledge related to the current or specified unit"""
        if not st.session_state.get('db') or not knowledge_id:
            knowledge_id = st.session_state.get('last_knowledge_id')
            
        if not knowledge_id:
            return
            
        try:
            from .knowledge_relationships import RelationshipManager
            
            relationship_manager = RelationshipManager(st.session_state.db)
            related_knowledge = relationship_manager.get_related_knowledge(knowledge_id)
            
            if related_knowledge:
                with st.expander("🔗 Related Knowledge", expanded=False):
                    st.markdown("*Knowledge connected to your recent interaction:*")
                    
                    # Group by relationship type
                    by_type = {}
                    for item in related_knowledge:
                        rel_type = item['relationship_type']
                        if rel_type not in by_type:
                            by_type[rel_type] = []
                        by_type[rel_type].append(item)
                    
                    for rel_type, items in by_type.items():
                        from .knowledge_relationships import RELATIONSHIP_TYPES
                        type_info = RELATIONSHIP_TYPES.get(rel_type, {})
                        type_name = type_info.get('name', rel_type.title())
                        
                        st.markdown(f"**{type_name}s:**")
                        
                        for item in items[:3]:  # Show top 3 per type
                            knowledge = item['knowledge']
                            strength = item['strength']
                            
                            with st.container():
                                col1, col2 = st.columns([4, 1])
                                
                                with col1:
                                    st.markdown(f"• **{knowledge['title']}**")
                                    st.caption(f"{knowledge['content_preview']}...")
                                    
                                with col2:
                                    st.metric("", f"{strength:.1%}", f"Q:{knowledge['quality_score']}")
                                
                                if st.button(f"📖 View", key=f"view_related_{knowledge['id']}"):
                                    st.info(f"**{knowledge['title']}**\n\n{knowledge['content_preview']}")
                        
                        if len(items) > 3:
                            st.caption(f"... and {len(items) - 3} more {type_name.lower()}s")
                    
        except Exception as e:
            # Silent failure - relationships are a nice-to-have feature
            pass
    
    def _discover_relationships_for_new_knowledge(self, knowledge_id: int):
        """FIXED: Incremental discovery for newly captured knowledge"""
        try:
            from .knowledge_relationships import discover_relationships_for_new_knowledge
            
            # FIXED: Use incremental discovery instead of full re-scan
            # This only compares the new unit against existing ones (O(n) vs O(n²))
            discover_relationships_for_new_knowledge(st.session_state.db, knowledge_id)
            
        except Exception as e:
            # Silent failure - don't disrupt the user experience
            pass
    
    def _get_available_categories(self) -> list:
        """Get available knowledge categories"""
        from .knowledge_capture import KNOWLEDGE_CATEGORIES
        return [info["name"] for info in KNOWLEDGE_CATEGORIES.values()]
    
    def show_enhanced_metrics(self):
        """Display enhanced metrics including knowledge capture stats"""
        # Basic metrics
        self.show_metrics()
        
        # Knowledge capture metrics
        if st.session_state.get('capture_manager'):
            st.subheader("📊 Knowledge Capture Metrics")
            
            metrics = st.session_state.capture_manager.get_metrics()
            if metrics:
                col1, col2, col3, col4 = st.columns(4)
                
                with col1:
                    st.metric("Total Knowledge", metrics.get('total_units', 0))
                    
                with col2:
                    st.metric("Avg Quality", f"{metrics.get('avg_quality', 0):.0f}/100")
                    
                with col3:
                    st.metric("Today's Captures", metrics.get('captures_24h', 0))
                    
                with col4:
                    categories = metrics.get('by_category', {})
                    top_category = max(categories.keys(), key=lambda k: categories[k]) if categories else "N/A"
                    st.metric("Top Category", top_category)
                
                # Most used knowledge
                most_used = metrics.get('most_used', [])
                if most_used:
                    st.subheader("🏆 Most Used Knowledge")
                    for item in most_used[:3]:
                        st.markdown(f"• **{item['title']}** (used {item['usage_count']} times)")
    
    def show_knowledge_status_indicator(self):
        """Show a small indicator of knowledge capture status"""
        if not self.knowledge_capture_enabled:
            return
            
        capture_manager = st.session_state.get('capture_manager')
        if capture_manager:
            last_knowledge_id = st.session_state.get('last_knowledge_id')
            if last_knowledge_id:
                st.success("💡 Knowledge captured automatically!")
            else:
                st.info("💭 Quality knowledge will be captured automatically")
        else:
            st.warning("⚠️ Knowledge capture unavailable")
    
    def show_metrics(self):
        """Display tool usage metrics"""
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.metric("Queries This Session", st.session_state.query_count)
            
        with col2:
            if st.session_state.db:
                total = len(st.session_state.db.get_recent_queries(limit=1000))
                st.metric("Total Queries", total)
            else:
                st.metric("Total Queries", "N/A")
                
        with col3:
            if st.session_state.db:
                kb_count = st.session_state.db.get_knowledge_count()
                st.metric("Knowledge Units", kb_count)
            else:
                st.metric("Knowledge Units", "N/A")
    
    def handle_error(self, error: Exception, context: str = ""):
        """
        Standardized error handling with logging
        
        Args:
            error: The exception that occurred
            context: Additional context about what was happening
        """
        error_msg = f"Error in {self.tool_name}"
        if context:
            error_msg += f" ({context})"
        error_msg += f": {str(error)}"
        
        st.error(error_msg)
        
        # Log to system logs if available
        if st.session_state.db:
            try:
                st.session_state.db.execute_write(
                    """INSERT INTO system_logs (level, message, context, created_at) 
                       VALUES (?, ?, ?, datetime('now'))""",
                    ("ERROR", str(error), f"{self.tool_name}: {context}")
                )
            except:
                pass  # Don't fail on logging errors


# Example usage in a tool:
"""
from utils.tool_base import TuoKitToolBase

class MyTool(TuoKitToolBase):
    def __init__(self):
        super().__init__(
            tool_name="My Awesome Tool",
            tool_description="Does awesome things with AI"
        )
    
    def process_request(self, user_input: str):
        st.title("🚀 My Awesome Tool")
        
        # Show knowledge search widget for related content
        self.show_knowledge_search_widget()
        
        # This automatically captures high-quality knowledge!
        result = self.generate_with_capture(
            prompt=f"Process this: {user_input}",
            temperature=0.7
        )
        
        if not result['error']:
            st.write(result['response'])
            
            # Show knowledge capture status
            self.show_knowledge_status_indicator()
            
            # Optional: Manual save widget for special cases
            if st.checkbox("📝 Manual Save Options"):
                self.show_knowledge_save_widget(
                    default_title=f"Processing: {user_input[:30]}...",
                    default_category="Processed Output",
                    content=result['response']
                )
        
        # Show enhanced metrics in sidebar
        with st.sidebar:
            self.show_enhanced_metrics()

# New Features Available to All Tools:
# 1. Automatic knowledge capture with quality filtering
# 2. Knowledge search widget to find related content  
# 3. Enhanced metrics showing capture statistics
# 4. Status indicators for capture success
# 5. Feature flag support (ENABLE_KNOWLEDGE_CAPTURE)
"""
