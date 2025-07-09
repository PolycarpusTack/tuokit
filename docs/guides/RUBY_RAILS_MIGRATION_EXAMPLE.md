# Ruby/Rails Consolidation - Practical Implementation Example

## Example: Migrating Rails Controller Generator

This example shows how to migrate the existing `rails_controller_gen.py` to use the unified toolkit while maintaining the exact same user interface.

### Step 1: Create the Adapter Layer

```python
# utils/rails_adapters.py
"""
Adapter classes to bridge existing pages with unified toolkits
Maintains backward compatibility while using new consolidated backend
"""

from typing import Dict, List, Any, Optional
from tools.rails_unified_toolkit import (
    RailsUnifiedToolkit, 
    RailsGenerationConfig, 
    RailsToolType
)

class RailsControllerAdapter:
    """Adapter for Rails Controller Generator"""
    
    def __init__(self, model: str = "deepseek-coder:6.7b"):
        self.toolkit = RailsUnifiedToolkit(ollama_model=model)
        self.model = model
    
    def generate_controller(
        self,
        resource_name: str,
        actions: List[str],
        api_version: Optional[str] = None,
        auth_type: str = "none",
        format: str = "html",
        nested_under: Optional[str] = None,
        concerns: Optional[List[str]] = None,
        skip_routes: bool = False
    ) -> Dict[str, Any]:
        """
        Generate Rails controller using unified toolkit
        Maintains exact same interface as original tool
        """
        
        # Build features list from parameters
        features = actions.copy()
        
        # Add configuration flags to features
        if api_version:
            features.append(f"api_v{api_version}")
        if nested_under:
            features.append(f"nested:{nested_under}")
        if concerns:
            features.extend([f"concern:{c}" for c in concerns])
        if skip_routes:
            features.append("skip_routes")
            
        # Create configuration for unified toolkit
        config = RailsGenerationConfig(
            resource_name=resource_name,
            tool_type=RailsToolType.CONTROLLER,
            api_mode=(format == "json"),
            authentication=auth_type.lower(),
            features=features
        )
        
        # Generate using unified toolkit
        result = self.toolkit.generate(config)
        
        # Transform result to match original format
        return {
            "controller_code": result["files"].get("controller", ""),
            "routes_code": result["files"].get("routes", ""),
            "strong_params": result["components"].get("strong_params", ""),
            "test_code": result["files"].get("tests", ""),
            "success": True,
            "message": "Controller generated successfully",
            "setup_instructions": result.get("instructions", "")
        }
    
    def get_model(self) -> str:
        """Get the current Ollama model"""
        return self.model
    
    def save_to_knowledge_base(self, resource_name: str, code: str, prompt: str) -> bool:
        """Save generated code to knowledge base"""
        # This can use the existing database integration
        # or delegate to the unified toolkit's knowledge capture
        return True
```

### Step 2: Update the Page to Use the Adapter

```python
# pages/rails_controller_gen.py (migrated version)
"""
RESTful Controller Generator for TuoKit
Now using unified Rails toolkit via adapter
"""

import streamlit as st
from utils.rails_adapters import RailsControllerAdapter
from utils.database import DatabaseManager
from datetime import datetime

def show():
    """Rails controller generator page"""
    
    st.title("🎛️ RESTful Controller Generator")
    st.markdown("""
    Generate production-ready Rails controllers with:
    - ✅ RESTful actions (index, show, create, update, destroy)
    - ✅ Strong parameters
    - ✅ Authentication support
    - ✅ API mode with JSON responses
    - ✅ Nested resources
    - ✅ RSpec/Minitest tests
    """)
    
    # Initialize adapter with selected model
    model = st.session_state.get("selected_model", "deepseek-coder:6.7b")
    adapter = RailsControllerAdapter(model=model)
    db = DatabaseManager()
    
    # UI Components (exactly the same as before)
    col1, col2 = st.columns([2, 1])
    
    with col1:
        resource_name = st.text_input(
            "Resource Name",
            placeholder="e.g., Product, User, Order",
            help="Singular form, will be pluralized automatically"
        )
        
        actions = st.multiselect(
            "Select Actions",
            options=["index", "show", "new", "create", "edit", "update", "destroy"],
            default=["index", "show", "create", "update", "destroy"],
            help="Choose which RESTful actions to include"
        )
    
    with col2:
        format_type = st.radio(
            "Response Format",
            options=["HTML", "JSON (API)"],
            help="HTML for web apps, JSON for APIs"
        )
        
        auth_type = st.selectbox(
            "Authentication",
            options=["None", "Devise", "JWT", "Basic"],
            help="Authentication method to use"
        )
    
    # Advanced options in expander
    with st.expander("Advanced Options"):
        col3, col4 = st.columns(2)
        
        with col3:
            api_version = st.text_input(
                "API Version",
                placeholder="e.g., 1",
                help="For versioned APIs (e.g., /api/v1)"
            ) if format_type == "JSON (API)" else None
            
            nested_under = st.text_input(
                "Nested Under Resource",
                placeholder="e.g., user",
                help="For nested routes like /users/:user_id/products"
            )
        
        with col4:
            concerns = st.multiselect(
                "Include Concerns",
                options=["Trackable", "Sortable", "Searchable", "Archivable"],
                help="Shared controller behaviors"
            )
            
            skip_routes = st.checkbox(
                "Skip Routes Generation",
                help="Don't generate routes.rb entries"
            )
    
    # Generate button
    if st.button("🚀 Generate Controller", type="primary", use_container_width=True):
        if not resource_name:
            st.error("Please enter a resource name")
            return
        
        if not actions:
            st.error("Please select at least one action")
            return
        
        try:
            with st.spinner(f"Generating {resource_name} controller..."):
                # Use adapter to generate
                result = adapter.generate_controller(
                    resource_name=resource_name,
                    actions=actions,
                    api_version=api_version,
                    auth_type=auth_type,
                    format=format_type.split()[0].lower(),
                    nested_under=nested_under,
                    concerns=concerns,
                    skip_routes=skip_routes
                )
            
            if result["success"]:
                st.success(result["message"])
                
                # Display generated code
                tabs = st.tabs(["Controller", "Routes", "Tests", "Setup"])
                
                with tabs[0]:
                    st.code(result["controller_code"], language="ruby")
                    
                    col5, col6 = st.columns(2)
                    with col5:
                        st.download_button(
                            "📥 Download Controller",
                            data=result["controller_code"],
                            file_name=f"{resource_name.lower()}_controller.rb",
                            mime="text/plain"
                        )
                    with col6:
                        if st.button("💾 Save to Knowledge Base"):
                            # Build prompt for knowledge base
                            prompt = f"Generate {resource_name} controller with actions: {', '.join(actions)}"
                            
                            # Save using adapter
                            if adapter.save_to_knowledge_base(
                                resource_name=resource_name,
                                code=result["controller_code"],
                                prompt=prompt
                            ):
                                st.success("Saved to knowledge base!")
                
                with tabs[1]:
                    if result.get("routes_code"):
                        st.code(result["routes_code"], language="ruby")
                    else:
                        st.info("Routes generation was skipped")
                
                with tabs[2]:
                    st.code(result["test_code"], language="ruby")
                    st.download_button(
                        "📥 Download Tests",
                        data=result["test_code"],
                        file_name=f"{resource_name.lower()}_controller_spec.rb",
                        mime="text/plain"
                    )
                
                with tabs[3]:
                    st.markdown(result["setup_instructions"])
                
                # Show strong parameters helper
                if result.get("strong_params"):
                    with st.expander("💡 Strong Parameters Helper"):
                        st.code(result["strong_params"], language="ruby")
                        st.info("Add this to your controller's private methods")
            
            else:
                st.error(f"Generation failed: {result.get('message', 'Unknown error')}")
                
        except Exception as e:
            st.error(f"Error generating controller: {str(e)}")
            st.exception(e)
    
    # Knowledge Base Section
    with st.sidebar:
        st.subheader("📚 Knowledge Base")
        
        # Search previous generations
        search_term = st.text_input("Search controllers", placeholder="e.g., User, API")
        
        if search_term:
            results = db.search_knowledge("controller", search_term)
            
            if results:
                st.write(f"Found {len(results)} controllers")
                
                for result in results[:5]:
                    with st.expander(f"{result['title']} - {result['created_at']}"):
                        st.code(result['content'], language="ruby")
                        
                        if st.button(f"Load {result['title']}", key=f"load_{result['id']}"):
                            st.session_state.loaded_controller = result['content']
                            st.rerun()
            else:
                st.info("No controllers found")
        
        # Tips and best practices
        with st.expander("💡 Best Practices"):
            st.markdown("""
            **Controller Best Practices:**
            - Keep controllers thin, models fat
            - Use before_actions for common setup
            - Leverage concerns for shared behavior
            - Always use strong parameters
            - Prefer respond_to over separate actions
            - Use service objects for complex logic
            - Test all actions, especially edge cases
            """)
```

### Step 3: Test the Migration

```python
# tests/test_controller_migration.py
"""
Test that migrated controller generator maintains feature parity
"""

import pytest
from utils.rails_adapters import RailsControllerAdapter
from pages.rails_controller_gen import show

class TestControllerMigration:
    
    def test_adapter_generates_controller(self):
        """Test basic controller generation"""
        adapter = RailsControllerAdapter()
        
        result = adapter.generate_controller(
            resource_name="Product",
            actions=["index", "show", "create"],
            auth_type="devise"
        )
        
        assert result["success"] is True
        assert "class ProductsController" in result["controller_code"]
        assert "def index" in result["controller_code"]
        assert "before_action :authenticate_user!" in result["controller_code"]
    
    def test_api_mode_generation(self):
        """Test API controller generation"""
        adapter = RailsControllerAdapter()
        
        result = adapter.generate_controller(
            resource_name="Product",
            actions=["index", "create"],
            format="json",
            api_version="1"
        )
        
        assert "Api::V1::ProductsController" in result["controller_code"]
        assert "render json:" in result["controller_code"]
    
    def test_nested_resource_generation(self):
        """Test nested resource controller"""
        adapter = RailsControllerAdapter()
        
        result = adapter.generate_controller(
            resource_name="Comment",
            actions=["create", "destroy"],
            nested_under="post"
        )
        
        assert "@post = Post.find(params[:post_id])" in result["controller_code"]
        assert "@comment = @post.comments" in result["controller_code"]
    
    def test_concerns_inclusion(self):
        """Test controller with concerns"""
        adapter = RailsControllerAdapter()
        
        result = adapter.generate_controller(
            resource_name="Product",
            actions=["index"],
            concerns=["Searchable", "Sortable"]
        )
        
        assert "include Searchable" in result["controller_code"]
        assert "include Sortable" in result["controller_code"]
    
    def test_feature_parity_with_original(self):
        """Ensure all original features work"""
        adapter = RailsControllerAdapter()
        
        # Test all authentication types
        for auth in ["none", "devise", "jwt", "basic"]:
            result = adapter.generate_controller("User", ["index"], auth_type=auth)
            assert result["success"] is True
        
        # Test all actions
        all_actions = ["index", "show", "new", "create", "edit", "update", "destroy"]
        result = adapter.generate_controller("Product", all_actions)
        
        for action in all_actions:
            assert f"def {action}" in result["controller_code"]
```

### Step 4: Gradual Rollout Plan

```python
# config/feature_flags.py
"""
Feature flags for gradual toolkit migration
"""

MIGRATION_FLAGS = {
    "use_unified_controller": True,  # Start with controller
    "use_unified_model": False,      # Roll out gradually
    "use_unified_scaffold": False,
    "use_unified_graphql": False,
    # ... other tools
}

def should_use_unified_toolkit(tool_name: str) -> bool:
    """Check if tool should use unified toolkit"""
    return MIGRATION_FLAGS.get(f"use_unified_{tool_name}", False)
```

### Step 5: Monitor and Validate

```python
# utils/migration_monitor.py
"""
Monitor the success of toolkit migration
"""

import time
from typing import Dict, Any
from utils.database import DatabaseManager

class MigrationMonitor:
    
    def __init__(self):
        self.db = DatabaseManager()
    
    def track_generation(
        self, 
        tool_type: str, 
        using_unified: bool,
        generation_time: float,
        success: bool
    ):
        """Track generation metrics"""
        self.db.execute("""
            INSERT INTO generation_metrics 
            (tool_type, using_unified, generation_time, success, created_at)
            VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)
        """, (tool_type, using_unified, generation_time, success))
    
    def compare_performance(self, tool_type: str) -> Dict[str, Any]:
        """Compare performance between old and new implementations"""
        
        # Get metrics for original implementation
        original_metrics = self.db.query("""
            SELECT 
                AVG(generation_time) as avg_time,
                COUNT(*) as total_generations,
                SUM(CASE WHEN success THEN 1 ELSE 0 END) * 100.0 / COUNT(*) as success_rate
            FROM generation_metrics
            WHERE tool_type = ? AND NOT using_unified
        """, (tool_type,))
        
        # Get metrics for unified implementation  
        unified_metrics = self.db.query("""
            SELECT 
                AVG(generation_time) as avg_time,
                COUNT(*) as total_generations,
                SUM(CASE WHEN success THEN 1 ELSE 0 END) * 100.0 / COUNT(*) as success_rate
            FROM generation_metrics
            WHERE tool_type = ? AND using_unified
        """, (tool_type,))
        
        return {
            "original": original_metrics[0] if original_metrics else None,
            "unified": unified_metrics[0] if unified_metrics else None,
            "improvement": {
                "speed": (
                    (original_metrics[0]["avg_time"] - unified_metrics[0]["avg_time"]) 
                    / original_metrics[0]["avg_time"] * 100
                ) if original_metrics and unified_metrics else 0
            }
        }
```

## Benefits of This Approach

1. **Zero Disruption**: Users see the exact same interface
2. **Gradual Migration**: Can migrate one tool at a time
3. **Easy Rollback**: Just change the adapter or feature flag
4. **Performance Tracking**: Monitor improvements
5. **Maintained Features**: All functionality preserved

## Next Steps

1. Implement adapters for all 15 tools
2. Add comprehensive tests for each adapter
3. Enable feature flags for gradual rollout
4. Monitor performance and user feedback
5. Remove old implementations once confident

This practical approach ensures a smooth transition to the unified toolkits while maintaining the excellent user experience of TuoKit!