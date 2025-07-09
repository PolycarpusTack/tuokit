"""
Rails API Development Tools
GraphQL, REST API, Serializers, and API documentation
"""

import streamlit as st
from typing import Dict, Any, List, Optional

from utils.ollama import safe_ollama_generate
from .config import API_FORMATS

class GraphQLGenerator:
    """Generate GraphQL schemas and resolvers for Rails"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_graphql_type(self, model_description: str, **options) -> Dict[str, Any]:
        """Generate GraphQL type from model description"""
        include_mutations = options.get('include_mutations', True)
        include_subscriptions = options.get('include_subscriptions', False)
        
        prompt = f"""Generate GraphQL implementation for Rails model: {model_description}

Include:
1. GraphQL Type definition with all fields
2. Query resolvers with filtering and pagination
3. {"Mutations for CRUD operations" if include_mutations else ""}
4. {"Subscriptions for real-time updates" if include_subscriptions else ""}
5. Input types for mutations
6. Proper error handling
7. Authorization checks

Use graphql-ruby gem conventions."""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="Generate production-ready GraphQL code for Rails using graphql-ruby.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        return {
            "success": True,
            "graphql_code": response.get("response", "")
        }
    
    def show_interface(self):
        """Show GraphQL generator interface"""
        st.markdown("### 🌐 GraphQL Generator")
        
        model_desc = st.text_area(
            "Model Description",
            placeholder="User with posts and comments, including authentication",
            height=100
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            include_queries = st.checkbox("Include Queries", value=True)
            include_mutations = st.checkbox("Include Mutations", value=True)
            include_subscriptions = st.checkbox("Include Subscriptions", value=False)
        
        with col2:
            include_auth = st.checkbox("Add Authorization", value=True)
            include_pagination = st.checkbox("Add Pagination", value=True)
            include_filtering = st.checkbox("Add Filtering", value=True)
        
        if st.button("🚀 Generate GraphQL", type="primary", use_container_width=True):
            if model_desc:
                with st.spinner("Generating GraphQL schema..."):
                    result = self.generate_graphql_type(
                        model_desc,
                        include_mutations=include_mutations,
                        include_subscriptions=include_subscriptions,
                        include_auth=include_auth,
                        include_pagination=include_pagination,
                        include_filtering=include_filtering
                    )
                    
                    if result.get('success'):
                        st.success("✅ GraphQL schema generated!")
                        
                        # Show in tabs
                        tabs = st.tabs(["Type Definitions", "Resolvers", "Mutations", "Schema"])
                        
                        # Parse and display different sections
                        code = result.get('graphql_code', '')
                        sections = self._parse_graphql_sections(code)
                        
                        with tabs[0]:
                            st.code(sections.get('types', ''), language='ruby')
                        
                        with tabs[1]:
                            st.code(sections.get('resolvers', ''), language='ruby')
                        
                        with tabs[2]:
                            if include_mutations:
                                st.code(sections.get('mutations', ''), language='ruby')
                        
                        with tabs[3]:
                            st.code(sections.get('schema', ''), language='ruby')
                    else:
                        st.error(f"Generation failed: {result.get('error')}")
            else:
                st.warning("Please provide a model description")
    
    def _parse_graphql_sections(self, code: str) -> Dict[str, str]:
        """Parse GraphQL code into sections"""
        # Simple parsing - in production would be more sophisticated
        sections = {
            'types': '',
            'resolvers': '',
            'mutations': '',
            'schema': ''
        }
        
        current_section = 'types'
        current_content = []
        
        for line in code.split('\n'):
            if 'class.*Type' in line:
                current_section = 'types'
            elif 'class.*Resolver' in line:
                current_section = 'resolvers'
            elif 'class.*Mutation' in line:
                current_section = 'mutations'
            elif 'class.*Schema' in line:
                current_section = 'schema'
            
            current_content.append(line)
            sections[current_section] = '\n'.join(current_content)
        
        return sections


class RestApiGenerator:
    """Generate RESTful API endpoints"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_api_controller(self, resource: str, **options) -> Dict[str, Any]:
        """Generate REST API controller"""
        api_version = options.get('version', 1)
        format_type = options.get('format', 'JSON')
        include_auth = options.get('auth', True)
        
        prompt = f"""Generate Rails API controller for resource: {resource}
API Version: v{api_version}
Format: {format_type}

Include:
1. All RESTful actions (index, show, create, update, destroy)
2. {"JWT/Token authentication" if include_auth else "No authentication"}
3. Proper status codes and error responses
4. Pagination for index action
5. Filtering and sorting support
6. Strong parameters
7. API versioning
8. {"JSON:API specification compliance" if format_type == "JSON:API" else "Standard JSON responses"}

Follow Rails API best practices."""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="Generate production-ready Rails API controller code.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        return {
            "success": True,
            "api_code": response.get("response", "")
        }
    
    def show_interface(self):
        """Show REST API generator interface"""
        st.markdown("### 🔌 REST API Generator")
        
        col1, col2 = st.columns(2)
        
        with col1:
            resource_name = st.text_input(
                "Resource Name",
                placeholder="posts"
            )
            
            api_version = st.number_input(
                "API Version",
                min_value=1,
                value=1
            )
        
        with col2:
            format_type = st.selectbox(
                "Response Format",
                API_FORMATS
            )
            
            auth_type = st.selectbox(
                "Authentication",
                ["JWT", "API Key", "OAuth", "None"]
            )
        
        st.markdown("**Features to Include:**")
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            include_pagination = st.checkbox("Pagination", value=True)
            include_filtering = st.checkbox("Filtering", value=True)
            include_sorting = st.checkbox("Sorting", value=True)
        
        with col2:
            include_versioning = st.checkbox("API Versioning", value=True)
            include_rate_limit = st.checkbox("Rate Limiting", value=False)
            include_caching = st.checkbox("Caching", value=True)
        
        with col3:
            include_docs = st.checkbox("API Documentation", value=True)
            include_tests = st.checkbox("Request Specs", value=True)
            include_serializer = st.checkbox("Serializers", value=True)
        
        if st.button("🚀 Generate API", type="primary", use_container_width=True):
            if resource_name:
                with st.spinner("Generating REST API..."):
                    result = self.generate_api_controller(
                        resource_name,
                        version=api_version,
                        format=format_type,
                        auth=(auth_type != "None")
                    )
                    
                    if result.get('success'):
                        st.success("✅ API generated successfully!")
                        
                        # Show different components
                        tabs = st.tabs(["Controller", "Routes", "Serializer", "Tests", "Documentation"])
                        
                        # For now, show all in first tab
                        with tabs[0]:
                            st.code(result.get('api_code', ''), language='ruby')
                        
                        # Placeholder for other tabs
                        with tabs[1]:
                            st.info("Routes configuration will be shown here")
                        
                        with tabs[2]:
                            if include_serializer:
                                st.info("Serializer code will be shown here")
                        
                        with tabs[3]:
                            if include_tests:
                                st.info("Request specs will be shown here")
                        
                        with tabs[4]:
                            if include_docs:
                                st.info("API documentation will be shown here")
                    else:
                        st.error(f"Generation failed: {result.get('error')}")
            else:
                st.warning("Please provide a resource name")


class SerializerGenerator:
    """Generate API serializers"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_serializer(self, model_code: str, serializer_type: str = "ActiveModel") -> Dict[str, Any]:
        """Generate serializer from model"""
        prompt = f"""Generate {serializer_type} serializer for this Rails model:

{model_code}

Include:
1. All model attributes with proper types
2. Associations with nested serialization
3. Custom methods and computed fields
4. Conditional attributes
5. Different serialization contexts (summary, detailed)
6. Performance optimizations (avoiding N+1)

Use {serializer_type} best practices."""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system=f"Generate efficient {serializer_type} serializer for Rails API.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        return {
            "success": True,
            "serializer": response.get("response", "")
        }