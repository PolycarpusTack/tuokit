"""
Rails Ultimate Toolkit Analyzer
Comprehensive Rails development suite with organized categories
"""

import streamlit as st
from typing import Dict, Any, List, Optional
from datetime import datetime

from utils.tool_base import TuoKitToolBase
from utils.ollama import get_ollama_manager, safe_ollama_generate

class RailsUltimateToolkit(TuoKitToolBase):
    """Comprehensive Rails development toolkit with 25+ tools"""
    
    def __init__(self):
        super().__init__(
            tool_name="Rails Ultimate Toolkit",
            tool_description="Complete Rails development suite - models, controllers, APIs, testing, debugging, and more"
        )
        
        # Import tool modules
        from .generators import ModelGenerator, ControllerGenerator, ServiceObjectGenerator
        from .testing import RSpecGenerator, SystemTestGenerator
        from .debugging import ErrorAnalyzer, PerformanceAnalyzer
        from .api import GraphQLGenerator, RestApiGenerator
        from .config import RAILS_CONFIG
        
        # Initialize tools
        self.model_gen = ModelGenerator()
        self.controller_gen = ControllerGenerator()
        self.service_gen = ServiceObjectGenerator()
        self.rspec_gen = RSpecGenerator()
        self.system_test_gen = SystemTestGenerator()
        self.error_analyzer = ErrorAnalyzer()
        self.perf_analyzer = PerformanceAnalyzer()
        self.graphql_gen = GraphQLGenerator()
        self.api_gen = RestApiGenerator()
        
        self.config = RAILS_CONFIG
    
    def run(self):
        """Main entry point for Rails toolkit"""
        st.title("🛤️ Rails Ultimate Toolkit")
        st.markdown("Complete Rails development suite with 25+ specialized tools")
        
        # Sidebar for tool categories
        with st.sidebar:
            st.markdown("## 🗂️ Tool Categories")
            
            category = st.radio(
                "Select Category",
                [
                    "🏗️ Generators",
                    "🧪 Testing",
                    "🐛 Debugging",
                    "🌐 API Tools",
                    "🎨 Frontend",
                    "🚀 DevOps",
                    "📚 Documentation"
                ],
                key="rails_category"
            )
            
            st.divider()
            
            # Model selector if available
            models = self._get_available_models()
            if models:
                selected_model = st.selectbox(
                    "🤖 AI Model",
                    models,
                    help="Select model for code generation"
                )
                st.session_state['selected_model'] = selected_model
        
        # Main content area
        if category == "🏗️ Generators":
            self._show_generators()
        elif category == "🧪 Testing":
            self._show_testing_tools()
        elif category == "🐛 Debugging":
            self._show_debugging_tools()
        elif category == "🌐 API Tools":
            self._show_api_tools()
        elif category == "🎨 Frontend":
            self._show_frontend_tools()
        elif category == "🚀 DevOps":
            self._show_devops_tools()
        elif category == "📚 Documentation":
            self._show_documentation_tools()
    
    def _show_generators(self):
        """Show code generation tools"""
        st.subheader("🏗️ Code Generators")
        
        tool_tabs = st.tabs([
            "Model",
            "Controller", 
            "Service Object",
            "Form Object",
            "Scaffold",
            "Migration"
        ])
        
        with tool_tabs[0]:
            self._show_model_generator()
        
        with tool_tabs[1]:
            self._show_controller_generator()
        
        with tool_tabs[2]:
            self._show_service_generator()
        
        with tool_tabs[3]:
            self._show_form_generator()
        
        with tool_tabs[4]:
            self._show_scaffold_generator()
        
        with tool_tabs[5]:
            self._show_migration_generator()
    
    def _show_model_generator(self):
        """Model generation interface"""
        st.markdown("### 📦 Model Generator")
        
        col1, col2 = st.columns([2, 1])
        
        with col1:
            model_desc = st.text_area(
                "Model Description",
                placeholder="User with email, name, admin flag, has many posts",
                height=100
            )
        
        with col2:
            st.markdown("**Options**")
            use_uuid = st.checkbox("Use UUID", value=False)
            add_devise = st.checkbox("Add Devise", value=False)
            add_factory = st.checkbox("Add Factory", value=True)
            add_tests = st.checkbox("Add Tests", value=True)
        
        if st.button("🚀 Generate Model", type="primary", use_container_width=True):
            if model_desc:
                with st.spinner("Generating model..."):
                    result = self.model_gen.generate(
                        description=model_desc,
                        use_uuid=use_uuid,
                        add_devise=add_devise,
                        add_factory=add_factory,
                        add_tests=add_tests
                    )
                    
                    if result.get('success'):
                        # Display generated code
                        st.success("✅ Model generated successfully!")
                        
                        # Show code in tabs
                        code_tabs = st.tabs(["Migration", "Model", "Factory", "Tests"])
                        
                        with code_tabs[0]:
                            st.code(result.get('migration', ''), language='ruby')
                        
                        with code_tabs[1]:
                            st.code(result.get('model', ''), language='ruby')
                        
                        with code_tabs[2]:
                            if add_factory:
                                st.code(result.get('factory', ''), language='ruby')
                        
                        with code_tabs[3]:
                            if add_tests:
                                st.code(result.get('tests', ''), language='ruby')
                        
                        # Save to knowledge base
                        self._save_to_knowledge(
                            title=f"Rails Model: {result.get('model_name', 'Model')}",
                            content=result.get('full_code', ''),
                            category="rails_model"
                        )
                    else:
                        st.error(f"Generation failed: {result.get('error', 'Unknown error')}")
            else:
                st.warning("Please provide a model description")
    
    def _show_controller_generator(self):
        """Controller generation interface"""
        st.markdown("### 🎮 Controller Generator")
        
        col1, col2 = st.columns([2, 1])
        
        with col1:
            resource_name = st.text_input(
                "Resource Name",
                placeholder="posts"
            )
            
            # Action selector
            st.markdown("**Select Actions**")
            actions_cols = st.columns(4)
            
            actions = []
            action_list = ["index", "show", "new", "create", "edit", "update", "destroy"]
            
            for i, action in enumerate(action_list):
                with actions_cols[i % 4]:
                    if st.checkbox(action, value=action in ["index", "show", "create", "update", "destroy"]):
                        actions.append(action)
        
        with col2:
            st.markdown("**Options**")
            api_only = st.checkbox("API Only", value=False)
            add_auth = st.checkbox("Add Authentication", value=True)
            nested_under = st.text_input("Nested Under", placeholder="admin")
            api_version = st.number_input("API Version", min_value=1, value=1) if api_only else None
        
        if st.button("🚀 Generate Controller", type="primary", use_container_width=True):
            if resource_name and actions:
                with st.spinner("Generating controller..."):
                    result = self.controller_gen.generate(
                        resource=resource_name,
                        actions=actions,
                        api_only=api_only,
                        add_auth=add_auth,
                        nested_under=nested_under,
                        api_version=api_version
                    )
                    
                    if result.get('success'):
                        st.success("✅ Controller generated successfully!")
                        
                        # Show generated code
                        code_tabs = st.tabs(["Controller", "Routes", "Tests"])
                        
                        with code_tabs[0]:
                            st.code(result.get('controller', ''), language='ruby')
                        
                        with code_tabs[1]:
                            st.code(result.get('routes', ''), language='ruby')
                        
                        with code_tabs[2]:
                            st.code(result.get('tests', ''), language='ruby')
                    else:
                        st.error(f"Generation failed: {result.get('error', 'Unknown error')}")
            else:
                st.warning("Please provide resource name and select at least one action")
    
    def _show_testing_tools(self):
        """Show testing tools"""
        st.subheader("🧪 Testing Tools")
        
        tool_tabs = st.tabs([
            "RSpec Generator",
            "System Tests",
            "Factory Builder",
            "Test Coverage",
            "Performance Tests"
        ])
        
        with tool_tabs[0]:
            self._show_rspec_generator()
        
        with tool_tabs[1]:
            self._show_system_test_generator()
        
        with tool_tabs[2]:
            self._show_factory_builder()
        
        with tool_tabs[3]:
            self._show_test_coverage()
        
        with tool_tabs[4]:
            self._show_performance_tests()
    
    def _show_debugging_tools(self):
        """Show debugging and analysis tools"""
        st.subheader("🐛 Debugging & Analysis")
        
        tool_tabs = st.tabs([
            "Error Analyzer",
            "Performance Profiler",
            "Query Optimizer",
            "Memory Analyzer",
            "N+1 Detector"
        ])
        
        with tool_tabs[0]:
            self._show_error_analyzer()
        
        with tool_tabs[1]:
            self._show_performance_profiler()
        
        with tool_tabs[2]:
            self._show_query_optimizer()
        
        with tool_tabs[3]:
            self._show_memory_analyzer()
        
        with tool_tabs[4]:
            self._show_n_plus_one_detector()
    
    def _show_api_tools(self):
        """Show API development tools"""
        st.subheader("🌐 API Development")
        
        tool_tabs = st.tabs([
            "GraphQL",
            "REST API",
            "Serializers",
            "API Docs",
            "Webhooks"
        ])
        
        with tool_tabs[0]:
            self._show_graphql_generator()
        
        with tool_tabs[1]:
            self._show_rest_api_builder()
        
        with tool_tabs[2]:
            self._show_serializer_generator()
        
        with tool_tabs[3]:
            self._show_api_docs_generator()
        
        with tool_tabs[4]:
            self._show_webhook_builder()
    
    def _show_frontend_tools(self):
        """Show frontend integration tools"""
        st.subheader("🎨 Frontend Integration")
        
        tool_tabs = st.tabs([
            "ViewComponent",
            "Stimulus",
            "React Integration",
            "Hotwire",
            "Asset Pipeline"
        ])
        
        with tool_tabs[0]:
            st.info("ViewComponent generator - Build reusable view components")
        
        with tool_tabs[1]:
            st.info("Stimulus controller generator - Create interactive JavaScript")
        
        with tool_tabs[2]:
            st.info("React component integration - Rails + React setup")
        
        with tool_tabs[3]:
            st.info("Hotwire/Turbo tools - Modern Rails frontend")
        
        with tool_tabs[4]:
            st.info("Asset pipeline optimizer - Manage CSS/JS assets")
    
    def _show_devops_tools(self):
        """Show DevOps and deployment tools"""
        st.subheader("🚀 DevOps & Deployment")
        
        tool_tabs = st.tabs([
            "Dockerfile",
            "CI/CD Pipeline",
            "Database Tasks",
            "Deployment Config",
            "Monitoring"
        ])
        
        with tool_tabs[0]:
            st.info("Dockerfile generator for Rails apps")
        
        with tool_tabs[1]:
            st.info("CI/CD pipeline generator (GitHub Actions, CircleCI)")
        
        with tool_tabs[2]:
            st.info("Database migration and seed tools")
        
        with tool_tabs[3]:
            st.info("Deployment configuration (Heroku, AWS, etc)")
        
        with tool_tabs[4]:
            st.info("Monitoring setup (New Relic, Datadog)")
    
    def _show_documentation_tools(self):
        """Show documentation tools"""
        st.subheader("📚 Documentation")
        
        tool_tabs = st.tabs([
            "API Docs",
            "README Generator",
            "YARD Docs",
            "Diagram Generator",
            "Changelog"
        ])
        
        with tool_tabs[0]:
            st.info("OpenAPI/Swagger documentation generator")
        
        with tool_tabs[1]:
            st.info("README.md generator with badges and sections")
        
        with tool_tabs[2]:
            st.info("YARD documentation for Ruby code")
        
        with tool_tabs[3]:
            st.info("ERD and architecture diagram generator")
        
        with tool_tabs[4]:
            st.info("CHANGELOG.md generator from git history")
    
    # Placeholder methods for tools (to be implemented)
    def _show_service_generator(self):
        st.info("Service Object generator - Coming soon!")
    
    def _show_form_generator(self):
        st.info("Form Object generator - Coming soon!")
    
    def _show_scaffold_generator(self):
        st.info("Full scaffold generator - Coming soon!")
    
    def _show_migration_generator(self):
        st.info("Migration generator - Coming soon!")
    
    def _show_rspec_generator(self):
        self.rspec_gen.show_interface()
    
    def _show_system_test_generator(self):
        self.system_test_gen.show_interface()
    
    def _show_factory_builder(self):
        st.info("Factory builder - Coming soon!")
    
    def _show_test_coverage(self):
        st.info("Test coverage analyzer - Coming soon!")
    
    def _show_performance_tests(self):
        st.info("Performance test generator - Coming soon!")
    
    def _show_error_analyzer(self):
        self.error_analyzer.show_interface()
    
    def _show_performance_profiler(self):
        self.perf_analyzer.show_interface()
    
    def _show_query_optimizer(self):
        st.info("Query optimizer - Coming soon!")
    
    def _show_memory_analyzer(self):
        st.info("Memory analyzer - Coming soon!")
    
    def _show_n_plus_one_detector(self):
        st.info("N+1 query detector - Coming soon!")
    
    def _show_graphql_generator(self):
        self.graphql_gen.show_interface()
    
    def _show_rest_api_builder(self):
        self.api_gen.show_interface()
    
    def _show_serializer_generator(self):
        st.info("Serializer generator - Coming soon!")
    
    def _show_api_docs_generator(self):
        st.info("API documentation generator - Coming soon!")
    
    def _show_webhook_builder(self):
        st.info("Webhook builder - Coming soon!")
    
    def _get_available_models(self) -> List[str]:
        """Get list of available Ollama models"""
        try:
            from utils.ollama import get_available_models
            return get_available_models()
        except:
            return []
    
    def _save_to_knowledge(self, title: str, content: str, category: str):
        """Save generated code to knowledge base"""
        try:
            if hasattr(st.session_state, 'db') and st.session_state.db:
                # Log the query
                query_id = st.session_state.db.log_query(
                    tool="rails_ultimate_toolkit",
                    model=st.session_state.get('selected_model', 'unknown'),
                    prompt=title,
                    response=content,
                    metadata={"category": category}
                )
                
                # Capture as knowledge
                from utils.knowledge_capture import capture_knowledge
                capture_knowledge(
                    st.session_state.db,
                    query_id,
                    title=title,
                    content=content,
                    category=category,
                    tags=[category, "rails", "generated_code"]
                )
        except Exception as e:
            st.error(f"Failed to save to knowledge base: {e}")

# Create a page wrapper for Streamlit
def show():
    """Streamlit page entry point"""
    toolkit = RailsUltimateToolkit()
    toolkit.run()

if __name__ == "__main__":
    show()