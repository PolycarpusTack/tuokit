"""
RESTful Controller Generator for TuoKit
Generates Rails controllers with RESTful actions and API support
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rails Controller Gen - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class RailsControllerGenerator(OllamaToolBase):
    """Rails RESTful controller generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="rails_controller_gen",
            default_model=ModelManager.get_default_model()
        )
        
        self.standard_actions = {
            "index": "GET /resources - List all resources",
            "show": "GET /resources/:id - Show specific resource",
            "new": "GET /resources/new - Form for new resource",
            "create": "POST /resources - Create new resource",
            "edit": "GET /resources/:id/edit - Form to edit resource",
            "update": "PUT/PATCH /resources/:id - Update resource",
            "destroy": "DELETE /resources/:id - Delete resource"
        }
    
    def generate_controller(self, resource_name: str, actions: list,
                          api_version: str = None, auth_type: str = "None",
                          format: str = "html", nested_under: str = None) -> dict:
        """Generate RESTful controller with specified actions"""
        
        # Build controller context
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
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def generate_routes(self, resource_name: str, actions: list,
                       api_version: str = None, nested_under: str = None) -> str:
        """Generate routes configuration"""
        prompt = f"""Generate Rails routes for {resource_name} with actions: {', '.join(actions)}
{"API namespace: " + api_version if api_version else ""}
{"Nested under: " + nested_under if nested_under else ""}

Show the routes.rb configuration."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🎛️ RESTful Controller Generator")
    st.markdown("Generate Rails controllers following RESTful conventions")
    
    # Initialize generator
    generator = RailsControllerGenerator()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Controller Options")
        
        # API configuration
        api_mode = st.toggle("API Mode", value=False)
        
        if api_mode:
            api_version = st.selectbox(
                "API Version",
                ["v1", "v2", "v3"],
                help="API versioning namespace"
            )
        else:
            api_version = None
        
        # Authentication
        auth_type = st.radio(
            "Authentication",
            ["None", "Devise", "JWT", "API Key"],
            help="Authentication method"
        )
        
        # Response format
        response_format = st.radio(
            "Response Format",
            ["html", "json", "both"],
            index=1 if api_mode else 0
        )
        
        st.divider()
        
        # Additional options
        include_specs = st.toggle(
            "Include RSpec Tests",
            value=True,
            help="Generate request specs"
        )
        
        include_swagger = st.toggle(
            "Include Swagger Docs",
            value=api_mode,
            help="Add API documentation"
        )
        
        st.divider()
        st.caption("💡 **Tip**: Follow RESTful conventions for better APIs")
    
    # Main input
    col1, col2 = st.columns([2, 1])
    
    with col1:
        resource_name = st.text_input(
            "Resource Name",
            placeholder="e.g., Article, User, Product",
            help="Singular form of the resource"
        )
    
    with col2:
        nested_under = st.text_input(
            "Nested Under (Optional)",
            placeholder="e.g., User",
            help="Parent resource for nesting"
        )
    
    # Action selection
    st.markdown("### 🎯 Select Actions")
    
    action_cols = st.columns(4)
    selected_actions = []
    
    for i, (action, description) in enumerate(generator.standard_actions.items()):
        with action_cols[i % 4]:
            if st.checkbox(
                action.capitalize(),
                value=action in ["index", "show", "create", "update", "destroy"],
                help=description,
                key=f"action_{action}"
            ):
                selected_actions.append(action)
    
    # Custom actions
    with st.expander("➕ Custom Actions"):
        custom_actions = st.text_input(
            "Additional Actions",
            placeholder="e.g., publish, archive, duplicate (comma-separated)",
            help="Non-RESTful actions to include"
        )
        if custom_actions:
            selected_actions.extend([a.strip() for a in custom_actions.split(",")])
    
    # Generate button
    if st.button("🎛️ Generate Controller", type="primary", 
                 disabled=not resource_name or not selected_actions):
        with st.spinner("Generating RESTful controller..."):
            result = generator.generate_controller(
                resource_name,
                selected_actions,
                api_version=api_version,
                auth_type=auth_type,
                format=response_format,
                nested_under=nested_under
            )
            
            if not result["error"]:
                st.success("✅ Controller generated successfully!")
                
                # Display metrics
                col1, col2, col3, col4 = st.columns(4)
                with col1:
                    st.metric("Resource", resource_name)
                with col2:
                    st.metric("Actions", len(selected_actions))
                with col3:
                    st.metric("Format", response_format.upper())
                with col4:
                    st.metric("Auth", auth_type)
                
                # Display in tabs
                tabs = st.tabs([
                    "📝 Controller Code",
                    "🛤️ Routes",
                    "📚 REST Guide",
                    "🧪 Testing",
                    "💾 Save"
                ])
                
                with tabs[0]:
                    st.code(result["code"], language="ruby")
                    
                    # Download button
                    controller_name = f"{resource_name.lower()}s_controller.rb"
                    if api_version:
                        controller_name = f"api/{api_version}/{controller_name}"
                    
                    st.download_button(
                        "📥 Download Controller",
                        data=result["code"],
                        file_name=controller_name,
                        mime="text/plain"
                    )
                    
                    # Controller location
                    st.info(f"""
                    **File Location:**
                    ```
                    app/controllers/{controller_name}
                    ```
                    """)
                
                with tabs[1]:
                    st.subheader("🛤️ Routes Configuration")
                    
                    with st.spinner("Generating routes..."):
                        routes = generator.generate_routes(
                            resource_name,
                            selected_actions,
                            api_version=api_version,
                            nested_under=nested_under
                        )
                        st.code(routes, language="ruby")
                    
                    # Route helpers
                    st.subheader("Route Helpers")
                    resource_plural = resource_name.lower() + "s"
                    
                    if nested_under:
                        parent_singular = nested_under.lower()
                        parent_plural = parent_singular + "s"
                        st.code(f"""
# Nested route helpers
{parent_singular}_{resource_plural}_path(@{parent_singular})              # /{parent_plural}/:id/{resource_plural}
{parent_singular}_{resource_name.lower()}_path(@{parent_singular}, @{resource_name.lower()})  # /{parent_plural}/:id/{resource_plural}/:id
new_{parent_singular}_{resource_name.lower()}_path(@{parent_singular})     # /{parent_plural}/:id/{resource_plural}/new
edit_{parent_singular}_{resource_name.lower()}_path(@{parent_singular}, @{resource_name.lower()}) # /{parent_plural}/:id/{resource_plural}/:id/edit
                        """, language="ruby")
                    else:
                        st.code(f"""
# Route helpers
{resource_plural}_path              # /{resource_plural}
{resource_name.lower()}_path(@{resource_name.lower()})        # /{resource_plural}/:id
new_{resource_name.lower()}_path         # /{resource_plural}/new
edit_{resource_name.lower()}_path(@{resource_name.lower()})   # /{resource_plural}/:id/edit
                        """, language="ruby")
                
                with tabs[2]:
                    st.subheader("📚 RESTful Design Guide")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### REST Principles
                        
                        **1. Resource-Based**
                        - URLs identify resources
                        - Use nouns, not verbs
                        - Collections and items
                        
                        **2. HTTP Methods**
                        - GET: Read data
                        - POST: Create new
                        - PUT/PATCH: Update
                        - DELETE: Remove
                        
                        **3. Stateless**
                        - No client context
                        - Each request complete
                        - Authentication in each
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Rails Conventions
                        
                        **URL Patterns**
                        ```
                        GET    /articles          # index
                        GET    /articles/new      # new
                        POST   /articles          # create
                        GET    /articles/:id      # show
                        GET    /articles/:id/edit # edit
                        PATCH  /articles/:id      # update
                        DELETE /articles/:id      # destroy
                        ```
                        
                        **Response Codes**
                        - 200: Success
                        - 201: Created
                        - 204: No Content
                        - 404: Not Found
                        - 422: Unprocessable
                        """)
                    
                    # Best practices
                    st.divider()
                    st.subheader("Best Practices")
                    
                    practice_tabs = st.tabs(["Security", "Performance", "API Design"])
                    
                    with practice_tabs[0]:
                        st.markdown("""
                        **Security Best Practices**
                        - Always use strong parameters
                        - Implement proper authentication
                        - Use CSRF protection for HTML
                        - Validate user permissions
                        - Sanitize user input
                        """)
                    
                    with practice_tabs[1]:
                        st.markdown("""
                        **Performance Tips**
                        - Use includes to avoid N+1
                        - Implement pagination
                        - Cache expensive operations
                        - Use database indexes
                        - Background jobs for slow tasks
                        """)
                    
                    with practice_tabs[2]:
                        st.markdown("""
                        **API Design Guidelines**
                        - Version your APIs
                        - Use consistent naming
                        - Return appropriate status codes
                        - Include helpful error messages
                        - Document with Swagger/OpenAPI
                        """)
                
                with tabs[3]:
                    st.subheader("🧪 Testing the Controller")
                    
                    if include_specs:
                        st.markdown("### RSpec Request Specs")
                        
                        st.code(f"""# Run controller specs
bundle exec rspec spec/requests/{resource_plural}_spec.rb

# Run with documentation format
bundle exec rspec spec/requests/{resource_plural}_spec.rb --format documentation

# Run specific example
bundle exec rspec spec/requests/{resource_plural}_spec.rb:42""", language="bash")
                        
                        # Example test
                        st.markdown("### Example Request Spec")
                        st.code(f"""
require 'rails_helper'

RSpec.describe "/{resource_plural}", type: :request do
  describe "GET /index" do
    it "returns a successful response" do
      get {resource_plural}_path
      expect(response).to be_successful
    end
  end
  
  describe "POST /create" do
    it "creates a new {resource_name}" do
      expect {{
        post {resource_plural}_path, params: {{ {resource_name.lower()}: valid_attributes }}
      }}.to change({resource_name}, :count).by(1)
    end
  end
end
                        """, language="ruby")
                    
                    # Postman/cURL examples
                    st.markdown("### API Testing")
                    
                    test_tabs = st.tabs(["cURL", "HTTPie", "Postman"])
                    
                    with test_tabs[0]:
                        st.code(f"""# GET request
curl http://localhost:3000/{resource_plural}

# POST request
curl -X POST http://localhost:3000/{resource_plural} \\
  -H "Content-Type: application/json" \\
  -d '{{"name":"Test"}}'

# PUT request
curl -X PUT http://localhost:3000/{resource_plural}/1 \\
  -H "Content-Type: application/json" \\
  -d '{{"name":"Updated"}}'""", language="bash")
                    
                    with test_tabs[1]:
                        st.code(f"""# GET request
http localhost:3000/{resource_plural}

# POST request
http POST localhost:3000/{resource_plural} name="Test"

# PUT request
http PUT localhost:3000/{resource_plural}/1 name="Updated" """, language="bash")
                    
                    with test_tabs[2]:
                        st.markdown("""
                        **Postman Collection**
                        1. Create new collection
                        2. Add requests for each endpoint
                        3. Set up environment variables
                        4. Add tests for responses
                        5. Export and share collection
                        """)
                
                with tabs[4]:
                    st.subheader("💾 Save Controller")
                    
                    title = st.text_input(
                        "Title",
                        value=f"Rails Controller: {resource_name}sController"
                    )
                    
                    project = st.text_input(
                        "Project Name",
                        placeholder="MyRailsAPI"
                    )
                    
                    notes = st.text_area(
                        "Implementation Notes",
                        placeholder="Add notes about this controller..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"rails, controller, rest, {resource_name.lower()}"
                    )
                    
                    if st.button("💾 Save Controller", type="primary"):
                        if db.connected:
                            # Compile content
                            content = f"""## Rails Controller: {resource_name}sController

## Configuration
- Actions: {', '.join(selected_actions)}
- API Version: {api_version or 'None'}
- Authentication: {auth_type}
- Format: {response_format}
- Nested Under: {nested_under or 'None'}

## Generated Code
{result['code']}

## Routes
{routes if 'routes' in locals() else 'Not generated'}

## Project: {project}
## Notes: {notes}"""
                            
                            metadata = {
                                "resource_name": resource_name,
                                "actions": selected_actions,
                                "api_version": api_version,
                                "auth_type": auth_type,
                                "format": response_format,
                                "nested_under": nested_under,
                                "include_specs": include_specs
                            }
                            
                            query_id = generator.db.log_query(
                                tool="rails_controller_gen",
                                model=generator.default_model,
                                prompt=f"{resource_name} controller with {', '.join(selected_actions)}",
                                response=result["code"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=content,
                                    category="Rails Controllers",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Controller saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
