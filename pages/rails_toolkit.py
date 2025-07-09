"""
Consolidated Rails Toolkit for TuoKit
Combines all Rails development functionality into a unified interface
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rails Toolkit - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
import re
from datetime import datetime

class UnifiedRailsToolkit(OllamaToolBase):
    """Comprehensive Rails development toolkit combining all Rails tools"""
    
    def __init__(self):
        super().__init__(
            tool_name="rails_toolkit",
            default_model=ModelManager.get_default_model()
        )
        self.coder_model = ModelManager.get_default_model()
        self.reasoning_model = ModelManager.get_default_model()
        
        # Controller actions
        self.standard_actions = {
            "index": "GET /resources - List all resources",
            "show": "GET /resources/:id - Show specific resource",
            "new": "GET /resources/new - Form for new resource",
            "create": "POST /resources - Create new resource",
            "edit": "GET /resources/:id/edit - Form to edit resource",
            "update": "PUT/PATCH /resources/:id - Update resource",
            "destroy": "DELETE /resources/:id - Delete resource"
        }
        
        # RSpec test types
        self.test_types = {
            "Model": {"description": "Unit tests for ActiveRecord models", "focus": "validations, associations, scopes, methods"},
            "Controller": {"description": "Controller specs (legacy style)", "focus": "actions, params, responses, filters"},
            "Request": {"description": "Integration tests for API endpoints", "focus": "HTTP requests, responses, side effects"},
            "System": {"description": "End-to-end browser tests", "focus": "user interactions, JavaScript, full stack"},
            "Helper": {"description": "Tests for view helpers", "focus": "helper methods, formatting, view logic"},
            "Service": {"description": "Tests for service objects", "focus": "business logic, external APIs, complex operations"},
            "Job": {"description": "Tests for background jobs", "focus": "job execution, arguments, side effects"},
            "Mailer": {"description": "Tests for ActionMailer", "focus": "email content, recipients, attachments"}
        }
        
        # Error patterns for debugging
        self.error_patterns = {
            "routing": {"pattern": r"(No route matches|ActionController::RoutingError|Route)", "emoji": "🛣️", "description": "Routing Configuration Error"},
            "database": {"pattern": r"(ActiveRecord::|PG::|Mysql2::|SQLite3::|migration)", "emoji": "🗄️", "description": "Database/ActiveRecord Error"},
            "validation": {"pattern": r"(Validation failed|ActiveRecord::RecordInvalid|RecordNotFound)", "emoji": "✅", "description": "Model Validation Error"},
            "authentication": {"pattern": r"(Unauthorized|CanCan|Pundit|Devise|authenticate)", "emoji": "🔐", "description": "Authentication/Authorization Error"},
            "view": {"pattern": r"(ActionView::|undefined method.*for nil|Missing template|partial)", "emoji": "🎨", "description": "View/Template Error"},
            "asset": {"pattern": r"(Asset.*not found|Sprockets::|Webpacker|pipeline)", "emoji": "📦", "description": "Asset Pipeline Error"},
            "configuration": {"pattern": r"(uninitialized constant|NameError|LoadError|require)", "emoji": "⚙️", "description": "Configuration/Loading Error"},
            "syntax": {"pattern": r"(SyntaxError|unexpected|syntax)", "emoji": "📝", "description": "Ruby Syntax Error"}
        }
    
    # Controller Generation
    def generate_controller(self, resource_name: str, actions: list, api_version: str = None,
                          auth_type: str = "None", format: str = "html", nested_under: str = None) -> dict:
        """Generate RESTful controller with specified actions"""
        context_parts = []
        if api_version:
            context_parts.append(f"API version: {api_version}")
        if auth_type != "None":
            context_parts.append(f"Authentication: {auth_type}")
        if format == "json":
            context_parts.append("JSON API format")
        if nested_under:
            context_parts.append(f"Nested under: {nested_under}")
            
        context = "\n".join(context_parts) if context_parts else ""
        
        prompt = f"""Generate a Rails 7 controller for resource: {resource_name}

Actions to include: {', '.join(actions)}
{context}

Create a complete controller with:
1. All specified RESTful actions
2. Strong parameters for {resource_name.lower()}
3. Before filters for common functionality
4. Proper error handling and responses
5. {"JSON responses for API" if format == "json" else "HTML responses with flash messages"}
6. {"Authentication filters" if auth_type != "None" else ""}
7. {"Nested resource handling" if nested_under else ""}
8. RSpec request specs for all actions

Follow Rails conventions and RESTful best practices."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a Rails expert. Generate clean, secure, RESTful controllers."
        )
        
        return {"code": result["response"], "error": result["error"]}
    
    # Model Generation
    def generate_model(self, description: str, test_framework: str = "RSpec",
                      orm: str = "ActiveRecord", add_devise: bool = False,
                      add_factory: bool = True) -> dict:
        """Generate complete Rails model with migration and tests"""
        enhancements = []
        if add_devise:
            enhancements.append("Include Devise authentication modules")
        if add_factory and test_framework == "RSpec":
            enhancements.append("Include FactoryBot factory")
        if test_framework != "None":
            enhancements.append(f"Include {test_framework} tests")
            
        prompt = f"""Generate a complete Rails 7 model for: {description}

ORM: {orm}
Test Framework: {test_framework}
{chr(10).join(enhancements) if enhancements else ''}

Provide complete code including:
1. Migration file with proper indexes and foreign keys
2. Model class with:
   - Validations (presence, uniqueness, format, etc.)
   - Associations if mentioned
   - Scopes for common queries
   - Callbacks if appropriate
   - Custom methods if needed
3. {"FactoryBot factory for testing" if add_factory else ""}
4. {f"{test_framework} model specs with full coverage" if test_framework != "None" else ""}

Use modern Rails conventions and best practices. Include helpful comments."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a Rails expert. Generate production-ready model code following Rails conventions."
        )
        
        return {"code": result["response"], "error": result["error"]}
    
    # Scaffold Generation
    def generate_scaffold(self, description: str, rails_version: str = "7.0",
                         test_framework: str = "RSpec", template_engine: str = "ERB",
                         add_auth: bool = False, api_mode: bool = False) -> dict:
        """Generate complete Rails scaffold code with advanced options"""
        enhancements = []
        if add_auth:
            enhancements.append("Include Devise authentication integration")
        if api_mode:
            enhancements.append("Generate API-only controllers with JSON responses")
        if test_framework != "None":
            enhancements.append(f"Include {test_framework} test examples")
        if template_engine != "ERB":
            enhancements.append(f"Use {template_engine} for view templates")
            
        system_prompt = f"""You are a Rails {rails_version} expert. Generate a complete scaffold.

Include:
1. Model with validations and associations
2. Migration file with proper indexes
3. Controller with all RESTful actions{' (API mode)' if api_mode else ''}
4. Views using {template_engine} (index, show, new, edit, _form partial){' - skip for API mode' if api_mode else ''}
5. Routes entry
6. {test_framework} tests{' - skip' if test_framework == 'None' else ''}

{chr(10).join(enhancements) if enhancements else ''}

Use modern Rails best practices and conventions. Output clean, production-ready code."""

        prompt = f"Generate a complete Rails {rails_version} scaffold for: {description}"
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system=system_prompt
        )
        
        return {"code": result["response"], "error": result["error"]}
    
    # GraphQL API Generation
    def generate_graphql_api(self, resource: str, operations: list, auth_method: str = "None",
                           pagination: str = "Cursor", features: list = None) -> dict:
        """Generate GraphQL API implementation"""
        features = features or ["Introspection"]
        
        prompt = f"""Create GraphQL API for {resource} with operations: {', '.join(operations)}

Configuration:
- Authentication: {auth_method}
- Pagination: {pagination}
- Features: {', '.join(features)}

Implement complete solution using graphql-ruby gem:
- Type definitions
- Query/Mutation resolvers
- N+1 prevention (BatchLoader)
- Authentication
- Error handling
Include tests and example queries"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a GraphQL expert. Generate production-ready GraphQL API code."
        )
        
        return {"code": result["response"], "error": result["error"]}
    
    # System Test Generation
    def generate_system_test(self, feature_description: str, test_framework: str = "RSpec",
                           include_accessibility: bool = True) -> dict:
        """Generate comprehensive system tests with accessibility checks"""
        enhancements = []
        if include_accessibility:
            enhancements.append("Include accessibility checks (a11y)")
            
        prompt = f"""Generate {test_framework} system test for: {feature_description}

Include:
- Page object model
- User journey scenarios
{"- Accessibility checks (a11y)" if include_accessibility else ""}
- JavaScript interaction
- Screenshot on failure
- Database cleaning
Use modern best practices."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a testing expert. Generate comprehensive system tests."
        )
        
        return {"code": result["response"], "error": result["error"]}
    
    # Rails Upgrade Planning
    def generate_upgrade_path(self, from_ver: str, to_ver: str, project_details: dict) -> dict:
        """Generate Rails upgrade roadmap"""
        prompt = f"""Upgrade from Rails {from_ver} to {to_ver}
Project: {project_details}

Provide detailed roadmap:
1. Breaking Changes: Key differences
2. Gem Compatibility: Required updates
3. Deprecation Guide: Changes needed
4. Performance Considerations
5. Recommended Tools: dual-boot, appraisal
Include estimated effort level"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            model=self.reasoning_model,
            temperature=0.2,
            system="You are a Rails upgrade expert. Provide comprehensive upgrade guidance."
        )
        
        return {"plan": result["response"], "error": result["error"]}
    
    # Rails Debugging
    def analyze_error(self, error_message: str, context: dict) -> dict:
        """Analyze Rails error with comprehensive context"""
        context_parts = []
        if context.get("code"):
            context_parts.append(f"Code Context:\n{context['code']}")
        if context.get("rails_version"):
            context_parts.append(f"Rails Version: {context['rails_version']}")
        if context.get("environment"):
            context_parts.append(f"Environment: {context['environment']}")
        if context.get("stack_trace"):
            context_parts.append(f"Stack Trace:\n{context['stack_trace'][:500]}...")
            
        context_str = "\n\n".join(context_parts)
        
        prompt = f"""Debug this Rails error:

Error Message:
{error_message}

{context_str if context_str else "No additional context provided"}

Provide:
1) Root Cause Analysis - What's causing this error?
2) Step-by-Step Solution - How to fix it (be specific)
3) Prevention Tips - How to avoid this in the future
4) Related Rails Concepts - Understanding the underlying issue
5) Common Variations - Similar errors to watch for"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            model=self.reasoning_model,
            temperature=0.2,
            system="""You are a Rails debugging expert with deep knowledge of the framework.
Provide clear, actionable solutions with specific code examples where relevant.
Consider Rails version differences and best practices."""
        )
        
        return {"analysis": result["response"], "error": result["error"]}
    
    def detect_error_type(self, error_message: str) -> tuple:
        """Detect and categorize the type of Rails error"""
        for error_type, info in self.error_patterns.items():
            if re.search(info["pattern"], error_message, re.IGNORECASE):
                return error_type, info["emoji"], info["description"]
        return "general", "🔧", "General Rails Error"
    
    # RSpec Test Generation
    def generate_specs(self, code: str, spec_type: str, use_factories: bool = True,
                      use_shoulda: bool = True, coverage_level: str = "comprehensive") -> dict:
        """Generate RSpec tests for Ruby/Rails code"""
        test_info = self.test_types.get(spec_type, {})
        
        enhancements = []
        if use_factories:
            enhancements.append("Use FactoryBot for test data")
        if use_shoulda and spec_type == "Model":
            enhancements.append("Use Shoulda Matchers for validations/associations")
        
        prompt = f"""Generate {coverage_level} RSpec {spec_type} tests for this Ruby/Rails code:

```ruby
{code}
```

Test Type: {spec_type}
Focus Areas: {test_info.get('focus', 'all aspects')}
{chr(10).join(enhancements) if enhancements else ''}

Create comprehensive tests covering:
1. Happy path scenarios - normal expected behavior
2. Edge cases - boundary conditions, empty inputs
3. Error conditions - invalid inputs, exceptions
4. Security considerations - authorization, injection
5. Performance considerations - N+1 queries, caching

Use RSpec best practices:
- Descriptive test names
- Proper test structure (describe/context/it)
- DRY with shared examples
- Appropriate use of let/let!/before blocks
- Clear expectations with proper matchers

Include comments explaining complex test scenarios."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are an RSpec testing expert. Generate thorough, well-structured tests."
        )
        
        return {"specs": result["response"], "error": result["error"]}
    
    # ViewComponent Generation
    def generate_view_component(self, description: str, template_lang: str = "ERB",
                              js_framework: str = "Stimulus", features: list = None) -> dict:
        """Generate ViewComponent with tests and stimulus integration"""
        features = features or ["Variants"]
        
        config = {
            "template": template_lang,
            "js": js_framework,
            "features": features
        }
        
        prompt = f"""Create ViewComponent for: {description}
Config: {config}

Include:
- Component class (Ruby)
- Template ({template_lang})
- Stimulus controller (JavaScript) if using Stimulus
- RSpec tests
- Preview/Storybook integration
Follow ViewComponent best practices"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a ViewComponent expert. Generate reusable, testable components."
        )
        
        return {"code": result["response"], "error": result["error"]}

def render_controller_generator(toolkit):
    """Render RESTful Controller Generator interface"""
    st.subheader("🎛️ RESTful Controller Generator")
    
    col1, col2 = st.columns([2, 1])
    with col1:
        resource_name = st.text_input("Resource Name", placeholder="e.g., Article, User, Product")
    with col2:
        nested_under = st.text_input("Nested Under (Optional)", placeholder="e.g., User")
    
    # Action selection
    st.markdown("### Select Actions")
    action_cols = st.columns(4)
    selected_actions = []
    
    for i, (action, description) in enumerate(toolkit.standard_actions.items()):
        with action_cols[i % 4]:
            if st.checkbox(action.capitalize(), value=action in ["index", "show", "create", "update", "destroy"],
                         help=description, key=f"ctrl_action_{action}"):
                selected_actions.append(action)
    
    # Configuration
    with st.expander("Controller Options"):
        col1, col2 = st.columns(2)
        with col1:
            api_mode = st.toggle("API Mode", value=False)
            api_version = st.selectbox("API Version", ["None", "v1", "v2", "v3"]) if api_mode else None
        with col2:
            auth_type = st.radio("Authentication", ["None", "Devise", "JWT", "API Key"])
            response_format = st.radio("Response Format", ["html", "json", "both"], index=1 if api_mode else 0)
    
    if st.button("Generate Controller", type="primary", disabled=not resource_name or not selected_actions):
        with st.spinner("Generating RESTful controller..."):
            result = toolkit.generate_controller(
                resource_name, selected_actions,
                api_version=api_version if api_version != "None" else None,
                auth_type=auth_type, format=response_format, nested_under=nested_under
            )
            
            if not result["error"]:
                st.success("✅ Controller generated successfully!")
                st.code(result["code"], language="ruby")
                
                controller_name = f"{resource_name.lower()}s_controller.rb"
                if api_version and api_version != "None":
                    controller_name = f"api/{api_version}/{controller_name}"
                
                st.download_button("📥 Download Controller", data=result["code"],
                                 file_name=controller_name, mime="text/plain")

def render_model_generator(toolkit):
    """Render Model & Migration Generator interface"""
    st.subheader("🏗️ Rails Model Generator")
    
    description = st.text_area("Describe your model",
        placeholder="e.g., User with email:string(unique, indexed), password_digest:string, first_name:string, last_name:string, admin:boolean(default: false)",
        help="Use Rails migration syntax or natural language")
    
    # Quick templates
    st.markdown("### Model Templates")
    template_cols = st.columns(4)
    templates = {
        "👤 User": "User with email:string(unique), password_digest:string, first_name:string, last_name:string, admin:boolean",
        "📝 Article": "Article with title:string, content:text, published:boolean, author:references, slug:string(unique)",
        "🛒 Product": "Product with name:string, description:text, price:decimal, stock:integer, category:references",
        "💬 Comment": "Comment with body:text, author:references, commentable:references{polymorphic}"
    }
    
    for i, (name, desc) in enumerate(templates.items()):
        with template_cols[i % 4]:
            if st.button(name, key=f"model_template_{i}"):
                st.session_state['model_description'] = desc
                st.rerun()
    
    # Use session state for description if template was clicked
    if 'model_description' in st.session_state:
        description = st.session_state['model_description']
    
    # Configuration
    with st.expander("Model Options"):
        col1, col2 = st.columns(2)
        with col1:
            test_framework = st.radio("Testing Framework", ["RSpec", "Minitest", "None"])
            orm = st.radio("ORM", ["ActiveRecord", "Mongoid"])
        with col2:
            add_devise = st.toggle("Add Devise Authentication", value=False)
            add_factory = st.toggle("Include FactoryBot", value=True)
            add_uuid = st.toggle("Use UUID Primary Keys", value=False)
    
    if st.button("Generate Model", type="primary", disabled=not description.strip()):
        with st.spinner("Generating Rails model..."):
            result = toolkit.generate_model(
                description + (" (with UUID primary key)" if add_uuid else ""),
                test_framework=test_framework,
                orm=orm,
                add_devise=add_devise,
                add_factory=add_factory
            )
            
            if not result["error"]:
                st.success("✅ Model generated successfully!")
                st.code(result["code"], language="ruby")
                
                # Extract model name
                model_name = description.split()[0].capitalize()
                st.download_button("📥 Download Model Files", data=result["code"],
                                 file_name=f"{model_name.lower()}_model.rb", mime="text/plain")

def render_scaffold_generator(toolkit):
    """Render Scaffold Generator interface"""
    st.subheader("⚡ Rails Scaffold Generator")
    
    description = st.text_input("Describe your resource",
        placeholder="e.g., Blog post with title:string content:text published:boolean author:references",
        help="Use Rails field type syntax or natural language")
    
    # Configuration
    with st.expander("Scaffold Options"):
        col1, col2 = st.columns(2)
        with col1:
            rails_version = st.selectbox("Rails Version", ["7.0", "6.1", "6.0", "5.2"])
            test_framework = st.radio("Testing Framework", ["RSpec", "Minitest", "None"])
        with col2:
            template_engine = st.radio("Template Engine", ["ERB", "Haml", "Slim"])
            add_auth = st.toggle("Add Authentication", value=False)
            api_mode = st.toggle("API Mode", value=False)
    
    if st.button("Generate Scaffold", type="primary", disabled=not description.strip()):
        with st.spinner("Generating Rails scaffold..."):
            result = toolkit.generate_scaffold(
                description,
                rails_version=rails_version,
                test_framework=test_framework,
                template_engine=template_engine,
                add_auth=add_auth,
                api_mode=api_mode
            )
            
            if not result["error"]:
                st.success("✅ Scaffold generated successfully!")
                st.code(result["code"], language="ruby")
                
                resource_name = description.split()[0].lower()
                st.download_button("📥 Download Scaffold", data=result["code"],
                                 file_name=f"{resource_name}_scaffold.rb", mime="text/plain")

def render_graphql_generator(toolkit):
    """Render GraphQL API Builder interface"""
    st.subheader("🚀 Rails GraphQL API Builder")
    
    resource = st.text_input("Resource Name", "Post")
    fields = st.text_area("Resource Fields", 
        "title: String!\ncontent: String!\nauthor: User!\ncomments: [Comment!]!",
        height=150)
    
    operations = st.multiselect("Supported Operations", 
        ["Query", "Mutation", "Subscription"],
        default=["Query", "Mutation"])
    
    # Configuration
    with st.expander("API Options"):
        col1, col2 = st.columns(2)
        with col1:
            auth_method = st.selectbox("Authentication", ["None", "JWT", "Devise", "API Key"])
            pagination = st.radio("Pagination", ["Cursor", "Offset", "Relay"])
        with col2:
            features = st.multiselect("Include Features",
                ["File Uploads", "Subscriptions", "Introspection", "Field-level Auth", "Query Complexity"],
                default=["Introspection"])
    
    if st.button("Generate API", type="primary"):
        with st.spinner("Building GraphQL schema..."):
            result = toolkit.generate_graphql_api(
                f"{resource} with fields: {fields}",
                operations,
                auth_method=auth_method,
                pagination=pagination,
                features=features
            )
            
            if not result["error"]:
                st.success("✅ GraphQL API generated successfully!")
                st.code(result["code"], language="ruby")
                st.download_button("📥 Download Schema", result["code"], "graphql_schema.rb")

def render_system_test_generator(toolkit):
    """Render System Test Generator interface"""
    st.subheader("🧪 Rails System Test Generator")
    
    feature = st.text_area("Describe Feature", 
        height=150,
        placeholder="e.g., User registration flow with email confirmation")
    
    # Configuration
    col1, col2 = st.columns(2)
    with col1:
        test_framework = st.radio("Framework", ["RSpec", "Minitest"])
        browser = st.selectbox("Browser", ["Chrome", "Firefox", "Safari"])
    with col2:
        js_driver = st.radio("JavaScript Driver", ["Selenium", "Cuprite", "Apparition"])
        include_a11y = st.toggle("Include Accessibility Checks", True)
    
    if st.button("Generate Tests", type="primary") and feature:
        with st.spinner("Creating test scenarios..."):
            result = toolkit.generate_system_test(
                feature,
                test_framework=test_framework,
                include_accessibility=include_a11y
            )
            
            if not result["error"]:
                st.success("✅ System tests generated successfully!")
                st.code(result["code"], language="ruby")
                st.download_button("📥 Download Test", result["code"], "system_test.rb")

def render_upgrade_advisor(toolkit):
    """Render Rails Upgrade Advisor interface"""
    st.subheader("🆙 Rails Upgrade Advisor")
    
    col1, col2 = st.columns(2)
    with col1:
        from_ver = st.selectbox("Current Rails Version", 
            ["5.0", "5.1", "5.2", "6.0", "6.1", "7.0", "7.1"], index=3)
    with col2:
        all_versions = ["5.0", "5.1", "5.2", "6.0", "6.1", "7.0", "7.1", "7.2"]
        available_targets = [v for v in all_versions if v > from_ver]
        to_ver = st.selectbox("Target Rails Version", available_targets)
    
    # Project details
    project_size = st.radio("Project Size", 
        ["Small (<10k LOC)", "Medium (10-50k LOC)", "Large (>50k LOC)"])
    
    critical_gems = st.text_area("Critical Gems (one per line)", 
        "devise\nsidekiq\npg\nredis\nrspec-rails", height=100)
    
    # Additional configuration
    with st.expander("Project Configuration"):
        col1, col2 = st.columns(2)
        with col1:
            database = st.selectbox("Database", ["PostgreSQL", "MySQL", "SQLite"])
            deployment = st.selectbox("Deployment", ["Heroku", "AWS", "Docker", "Traditional"])
        with col2:
            api_only = st.toggle("API-only Application", False)
            test_framework = st.radio("Test Suite", ["RSpec", "Minitest", "Both"])
    
    if st.button("Generate Upgrade Plan", type="primary"):
        with st.spinner("Analyzing upgrade path..."):
            project_details = {
                "size": project_size,
                "gems": critical_gems.split('\n'),
                "database": database,
                "deployment": deployment,
                "api_only": api_only,
                "test_framework": test_framework
            }
            
            result = toolkit.generate_upgrade_path(from_ver, to_ver, project_details)
            
            if not result["error"]:
                st.success(f"✅ Rails {from_ver} → {to_ver} Upgrade Plan Generated!")
                st.markdown(result["plan"])
                st.download_button("📥 Download Plan", result["plan"], 
                                 f"rails_{from_ver}_to_{to_ver}_upgrade.md")

def render_debugger(toolkit):
    """Render Rails Debugging Assistant interface"""
    st.subheader("🐞 Rails Debugging Assistant")
    
    error_message = st.text_area("📋 Paste Rails Error Message",
        height=150,
        placeholder="""Example:
ActionController::RoutingError (No route matches [GET] "/api/users"):
app/controllers/application_controller.rb:12:in `rescue_from'

Or any Rails error message...""")
    
    # Detect error type
    if error_message:
        error_type, emoji, description = toolkit.detect_error_type(error_message)
        col1, col2 = st.columns([1, 3])
        with col1:
            st.metric("Error Type", f"{emoji} {error_type.title()}")
        with col2:
            st.info(f"**Detected**: {description}")
    
    # Context inputs
    with st.expander("➕ Add Context (Recommended)", expanded=True):
        code_snippet = st.text_area("Related Code", height=100,
            placeholder="Paste the code causing the error (controller, model, view, etc.)")
        
        col1, col2 = st.columns(2)
        with col1:
            rails_version = st.selectbox("Rails Version", ["7.0", "6.1", "6.0", "5.2"])
            environment = st.selectbox("Environment", ["Development", "Production", "Test", "Staging"])
        with col2:
            gems_context = st.text_input("Relevant Gems", placeholder="e.g., devise, pundit, rspec")
            db_type = st.selectbox("Database", ["PostgreSQL", "MySQL", "SQLite", "Other"])
    
    if st.button("🔍 Debug Error", type="primary", disabled=not error_message.strip()):
        context = {
            "rails_version": rails_version,
            "environment": environment,
            "code": code_snippet,
            "gems": gems_context,
            "database": db_type
        }
        
        with st.spinner("Analyzing error..."):
            result = toolkit.analyze_error(error_message, context)
            
            if not result["error"]:
                st.success("✅ Analysis complete!")
                st.markdown(result["analysis"])

def render_rspec_generator(toolkit):
    """Render RSpec Test Generator interface"""
    st.subheader("🧪 RSpec Test Generator")
    
    code = st.text_area("Paste Ruby/Rails Code to Test",
        height=300,
        placeholder="""Example:
class User < ApplicationRecord
  validates :email, presence: true, uniqueness: true
  validates :age, numericality: { greater_than_or_equal_to: 18 }
  
  has_many :posts
  
  def full_name
    "#{first_name} #{last_name}".strip
  end
  
  def adult?
    age >= 18
  end
end""")
    
    col1, col2 = st.columns([3, 1])
    with col1:
        spec_type = st.selectbox("Test Type", list(toolkit.test_types.keys()))
    with col2:
        if spec_type in toolkit.test_types:
            st.info(toolkit.test_types[spec_type]["description"])
    
    # Configuration
    with st.expander("Test Options"):
        col1, col2 = st.columns(2)
        with col1:
            use_factories = st.toggle("Use FactoryBot", value=True)
            use_shoulda = st.toggle("Use Shoulda Matchers", value=True)
        with col2:
            coverage_level = st.select_slider("Coverage Level",
                options=["basic", "standard", "comprehensive", "exhaustive"],
                value="comprehensive")
    
    if st.button("Generate Tests", type="primary", disabled=not code.strip()):
        with st.spinner("Creating comprehensive test suite..."):
            result = toolkit.generate_specs(
                code, spec_type,
                use_factories=use_factories,
                use_shoulda=use_shoulda,
                coverage_level=coverage_level
            )
            
            if not result["error"]:
                st.success("✅ Tests generated successfully!")
                st.code(result["specs"], language="ruby")
                
                # Extract class name for filename
                match = re.search(r'class\s+(\w+)', code)
                class_name = match.group(1).lower() if match else "subject"
                st.download_button("📥 Download Spec File", result["specs"],
                                 f"{class_name}_spec.rb", mime="text/plain")

def render_view_component_generator(toolkit):
    """Render ViewComponent Generator interface"""
    st.subheader("🧩 Rails View Component Generator")
    
    description = st.text_area("Describe Component", 
        height=150,
        placeholder="e.g., Notification badge with count, styles, and animation")
    
    # Configuration
    with st.expander("Component Options"):
        col1, col2 = st.columns(2)
        with col1:
            template_lang = st.radio("Template Language", ["ERB", "HAML", "SLIM"])
            js_framework = st.selectbox("JavaScript Framework", ["Stimulus", "Turbo Streams", "Vanilla JS", "Alpine.js"])
        with col2:
            features = st.multiselect("Additional Features",
                ["Slots", "Variants", "I18n Support", "Form Integration", "Animation", "Loading States"],
                default=["Variants"])
    
    if st.button("Generate Component", type="primary") and description:
        with st.spinner("Building component..."):
            result = toolkit.generate_view_component(
                description,
                template_lang=template_lang,
                js_framework=js_framework,
                features=features
            )
            
            if not result["error"]:
                st.success("✅ Component generated successfully!")
                st.code(result["code"], language="ruby")
                st.download_button("📥 Download Component", result["code"], "component.rb")

def show():
    """Main page display function"""
    st.title("🛤️ Unified Rails Toolkit")
    st.markdown("Comprehensive Rails development suite with all tools in one place")
    
    # Initialize toolkit
    toolkit = UnifiedRailsToolkit()
    db = DatabaseManager()
    
    # Tool selection
    tool_category = st.selectbox("Select Tool Category", [
        "Controller Generator",
        "Model Generator",
        "Scaffold Generator",
        "GraphQL API Builder",
        "System Test Generator",
        "Upgrade Advisor",
        "Debugging Assistant",
        "RSpec Test Generator",
        "ViewComponent Generator"
    ])
    
    st.divider()
    
    # Render selected tool
    if tool_category == "Controller Generator":
        render_controller_generator(toolkit)
    elif tool_category == "Model Generator":
        render_model_generator(toolkit)
    elif tool_category == "Scaffold Generator":
        render_scaffold_generator(toolkit)
    elif tool_category == "GraphQL API Builder":
        render_graphql_generator(toolkit)
    elif tool_category == "System Test Generator":
        render_system_test_generator(toolkit)
    elif tool_category == "Upgrade Advisor":
        render_upgrade_advisor(toolkit)
    elif tool_category == "Debugging Assistant":
        render_debugger(toolkit)
    elif tool_category == "RSpec Test Generator":
        render_rspec_generator(toolkit)
    elif tool_category == "ViewComponent Generator":
        render_view_component_generator(toolkit)
    
    # Rails Resources
    with st.sidebar:
        st.divider()
        st.subheader("📚 Rails Resources")
        
        st.markdown("""
        **Official Documentation**
        - [Rails Guides](https://guides.rubyonrails.org/)
        - [Rails API Docs](https://api.rubyonrails.org/)
        - [Rails Forum](https://discuss.rubyonrails.org/)
        
        **Learning Resources**
        - [GoRails](https://gorails.com/)
        - [RailsCasts](http://railscasts.com/)
        - [Drifting Ruby](https://www.driftingruby.com/)
        
        **Best Practices**
        - [Rails Style Guide](https://rails.rubystyle.guide/)
        - [Better Specs](https://www.betterspecs.org/)
        - [Rails Best Practices](https://rails-bestpractices.com/)
        """)
        
        st.divider()
        st.caption("💡 **Pro Tip**: Use keyboard shortcuts for faster navigation")

# Entry point
if __name__ == "__main__":
    show()