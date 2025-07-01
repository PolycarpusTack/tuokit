"""
Rails Scaffold Generator for TuoKit
Generates complete Rails scaffolds using DeepSeek Coder model
Enhanced with framework options and advanced configurations
"""

import streamlit as st
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class RailsScaffoldGenerator(OllamaToolBase):
    """Rails scaffold generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="rails_scaffold",
            default_model="deepseek-coder:6.7b"
        )
        
    def generate_scaffold(self, description: str, rails_version: str = "7.0",
                         test_framework: str = "RSpec", template_engine: str = "ERB",
                         add_auth: bool = False, api_mode: bool = False) -> dict:
        """Generate complete Rails scaffold code with advanced options"""
        
        # Build enhanced prompt based on options
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
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def parse_resource_name(self, description: str) -> str:
        """Extract resource name from description"""
        words = description.split()
        if words:
            return words[0].lower()
        return "resource"

def show():
    """Main page display function"""
    st.title("⚡ Rails Scaffold Generator")
    st.markdown("Generate complete Rails scaffolds with models, controllers, and views")
    
    # Initialize generator
    generator = RailsScaffoldGenerator()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Scaffold Configuration")
        
        rails_version = st.selectbox(
            "Rails Version",
            ["7.0", "6.1", "6.0", "5.2"],
            index=0,
            help="Select your Rails framework version"
        )
        
        st.divider()
        
        test_framework = st.radio(
            "Testing Framework",
            ["RSpec", "Minitest", "None"],
            help="Choose your preferred testing framework"
        )
        
        template_engine = st.radio(
            "Template Engine",
            ["ERB", "Haml", "Slim"],
            help="Select view template format"
        )
        
        st.divider()
        
        add_auth = st.toggle(
            "Add Authentication",
            value=False,
            help="Include Devise authentication setup"
        )
        
        api_mode = st.toggle(
            "API Mode",
            value=False,
            help="Generate API-only controllers"
        )
        
        use_uuid = st.toggle(
            "Use UUID Primary Keys",
            value=False,
            help="Use UUIDs instead of integer IDs"
        )
        
        st.divider()
        st.caption("💡 **Pro Tip**: Use API mode for React/Vue frontends")
    
    # Main content area
    description = st.text_input(
        "Describe your resource",
        placeholder="e.g., Blog post with title:string content:text published:boolean author:references",
        help="Use Rails field type syntax or natural language"
    )
    
    # Quick templates
    st.markdown("### 🚀 Quick Start Templates")
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        if st.button("📝 Blog System"):
            description = "Post title:string content:text published:boolean author:references view_count:integer"
    
    with col2:
        if st.button("🛒 E-commerce"):
            description = "Product name:string description:text price:decimal inventory:integer category:references featured:boolean"
    
    with col3:
        if st.button("📋 Task Manager"):
            description = "Task title:string description:text due_date:datetime completed:boolean priority:integer assignee:references"
    
    with col4:
        if st.button("👥 User Profile"):
            description = "Profile user:references bio:text avatar_url:string location:string website:string public:boolean"
    
    # Generate button with loading state
    if st.button("🚀 Generate Scaffold", type="primary", disabled=not description.strip()):
        with st.spinner("Generating Rails scaffold..."):
            # Build enhanced description
            enhanced_desc = description
            if use_uuid:
                enhanced_desc += " (use UUID primary keys)"
                
            result = generator.generate_scaffold(
                enhanced_desc,
                rails_version=rails_version,
                test_framework=test_framework,
                template_engine=template_engine,
                add_auth=add_auth,
                api_mode=api_mode
            )
            
            if not result["error"]:
                st.success("✅ Scaffold generated successfully!")
                
                # Display in tabs
                tab1, tab2, tab3, tab4 = st.tabs([
                    "📁 Generated Code", 
                    "🛠️ Implementation", 
                    "📚 Documentation",
                    "💾 Save"
                ])
                
                with tab1:
                    st.code(result["code"], language="ruby")
                    
                    # Download button
                    resource_name = generator.parse_resource_name(description)
                    st.download_button(
                        "📥 Download Scaffold",
                        data=result["code"],
                        file_name=f"{resource_name}_scaffold.rb",
                        mime="text/plain"
                    )
                
                with tab2:
                    st.subheader("🛠️ Implementation Steps")
                    
                    # Generate commands
                    resource_name = generator.parse_resource_name(description)
                    
                    st.code(f"""# 1. Generate the scaffold
rails generate scaffold {description}

# 2. Run database migration
rails db:migrate

# 3. (Optional) Seed test data
rails db:seed

# 4. Start Rails server
rails server

# 5. Access your resource
# http://localhost:3000/{resource_name}s""", language="bash")
                    
                    if test_framework != "None":
                        st.subheader(f"🧪 Run {test_framework} Tests")
                        if test_framework == "RSpec":
                            st.code("bundle exec rspec spec/models/\nbundle exec rspec spec/controllers/", language="bash")
                        else:
                            st.code("rails test", language="bash")
                    
                    if add_auth:
                        st.subheader("🔐 Authentication Setup")
                        st.code("""# Add to Gemfile
gem 'devise'

# Install and configure
bundle install
rails generate devise:install
rails generate devise User
rails db:migrate""", language="bash")
                
                with tab3:
                    st.subheader("📚 Rails Concepts")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### Scaffold Components
                        
                        **Model** (`app/models/`)
                        - ActiveRecord ORM
                        - Validations & associations
                        - Business logic
                        
                        **Controller** (`app/controllers/`)
                        - RESTful actions
                        - Strong parameters
                        - Before/after filters
                        
                        **Views** (`app/views/`)
                        - HTML templates
                        - Partials for DRY
                        - Form helpers
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Rails Best Practices
                        
                        **Convention over Configuration**
                        - Follow naming conventions
                        - Use Rails generators
                        - Leverage defaults
                        
                        **RESTful Design**
                        - Standard HTTP verbs
                        - Resource-based URLs
                        - Predictable patterns
                        
                        **Testing**
                        - Test-driven development
                        - Model & request specs
                        - Fixtures or factories
                        """)
                    
                    st.info("📖 **Recommended**: Check out the [Rails Guides](https://guides.rubyonrails.org/) for in-depth documentation")
                
                with tab4:
                    st.subheader("💾 Save to Knowledge Library")
                    
                    # Save form
                    title = st.text_input(
                        "Title",
                        value=f"Rails Scaffold: {description[:50]}..."
                    )
                    
                    notes = st.text_area(
                        "Additional Notes",
                        placeholder="Add any implementation notes or context..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"rails, scaffold, {rails_version}, {resource_name}"
                    )
                    
                    if st.button("💾 Save Scaffold", type="primary"):
                        if db.connected:
                            metadata = {
                                "rails_version": rails_version,
                                "test_framework": test_framework,
                                "template_engine": template_engine,
                                "add_auth": add_auth,
                                "api_mode": api_mode,
                                "use_uuid": use_uuid,
                                "notes": notes
                            }
                            
                            query_id = generator.db.log_query(
                                tool="rails_scaffold",
                                model=generator.default_model,
                                prompt=description,
                                response=result["code"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=result["code"],
                                    category="Rails Scaffolds",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Saved to knowledge library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")
    
    # Rails field types reference
    with st.expander("📖 Rails Field Types Reference"):
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.markdown("""
            **Text Types**
            - `string` - Short text (255 chars)
            - `text` - Long text
            - `json` - JSON data
            - `jsonb` - Binary JSON
            """)
        
        with col2:
            st.markdown("""
            **Numeric Types**
            - `integer` - Whole numbers
            - `decimal` - Precise decimals
            - `float` - Floating point
            - `bigint` - Large integers
            """)
        
        with col3:
            st.markdown("""
            **Other Types**
            - `boolean` - True/false
            - `date` - Date only
            - `datetime` - Date and time
            - `references` - Foreign key
            - `uuid` - UUID type
            """)

# Entry point
if __name__ == "__main__":
    show()
