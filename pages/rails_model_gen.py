"""
Rails Model & Migration Generator for TuoKit
Generates complete Rails models with migrations, validations, and tests
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rails Model Gen - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class RailsModelGenerator(OllamaToolBase):
    """Rails model and migration generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="rails_model_gen",
            default_model=ModelManager.get_default_model()
        )
    
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
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def parse_model_info(self, description: str) -> dict:
        """Extract model information from description"""
        info = {
            "model_name": "Model",
            "attributes": [],
            "associations": []
        }
        
        # Basic parsing - in production, this would be more sophisticated
        words = description.split()
        if words:
            info["model_name"] = words[0].capitalize()
        
        # Look for common attribute patterns
        if "email" in description.lower():
            info["attributes"].append("email:string")
        if "name" in description.lower():
            info["attributes"].append("name:string")
        if "user" in description.lower() and "belongs_to" in description.lower():
            info["associations"].append("belongs_to :user")
            
        return info

def show():
    """Main page display function"""
    st.title("🏗️ Rails Model Generator")
    st.markdown("Generate complete Rails models with migrations, validations, and tests")
    
    # Initialize generator
    generator = RailsModelGenerator()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Model Configuration")
        
        test_framework = st.radio(
            "Testing Framework",
            ["RSpec", "Minitest", "None"],
            help="Choose your testing framework"
        )
        
        orm = st.radio(
            "ORM",
            ["ActiveRecord", "Mongoid"],
            help="Object-Relational Mapper"
        )
        
        st.divider()
        
        add_devise = st.toggle(
            "Add Devise Authentication",
            value=False,
            help="Include Devise modules for user authentication"
        )
        
        add_factory = st.toggle(
            "Include FactoryBot",
            value=True,
            help="Generate factory for testing"
        )
        
        add_uuid = st.toggle(
            "Use UUID Primary Keys",
            value=False,
            help="Use UUIDs instead of integer IDs"
        )
        
        st.divider()
        st.caption("💡 **Pro Tip**: Include associations in your description")
    
    # Main input area
    description = st.text_input(
        "Describe your model",
        placeholder="e.g., User with email:string(unique, indexed), password_digest:string, first_name:string, last_name:string, admin:boolean(default: false)",
        help="Use Rails migration syntax or natural language"
    )
    
    # Quick templates
    st.markdown("### 🚀 Model Templates")
    
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
                description = desc
    
    # Add UUID note to description if enabled
    if add_uuid:
        uuid_note = " (with UUID primary key)"
        if uuid_note not in description:
            description += uuid_note
    
    # Generate button
    if st.button("🏗️ Generate Model", type="primary", disabled=not description.strip()):
        with st.spinner("Generating Rails model..."):
            result = generator.generate_model(
                description,
                test_framework=test_framework,
                orm=orm,
                add_devise=add_devise,
                add_factory=add_factory
            )
            
            if not result["error"]:
                st.success("✅ Model generated successfully!")
                
                # Parse model info
                model_info = generator.parse_model_info(description)
                
                # Display metrics
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("Model", model_info["model_name"])
                with col2:
                    st.metric("Attributes", len(model_info["attributes"]))
                with col3:
                    st.metric("Test Framework", test_framework)
                
                # Display in tabs
                tabs = st.tabs([
                    "📝 Model Code",
                    "🗄️ Migration",
                    "🧪 Tests",
                    "📚 ActiveRecord Guide",
                    "💾 Save"
                ])
                
                with tabs[0]:
                    st.code(result["code"], language="ruby")
                    
                    # Download button
                    st.download_button(
                        "📥 Download All Files",
                        data=result["code"],
                        file_name=f"{model_info['model_name'].lower()}_model.rb",
                        mime="text/plain"
                    )
                
                with tabs[1]:
                    st.subheader("🗄️ Migration Commands")
                    
                    model_name_lower = model_info["model_name"].lower()
                    st.code(f"""# Generate migration
rails generate migration Create{model_info['model_name']}s

# Or with attributes
rails generate model {model_info['model_name']} {' '.join(model_info['attributes'])}

# Run migration
rails db:migrate

# Rollback if needed
rails db:rollback

# Check migration status
rails db:migrate:status""", language="bash")
                    
                    # Migration best practices
                    st.info("""
                    **Migration Best Practices:**
                    - Always add indexes for foreign keys
                    - Use `null: false` for required fields
                    - Set sensible defaults
                    - Add database constraints when possible
                    """)
                
                with tabs[2]:
                    st.subheader(f"🧪 {test_framework} Testing")
                    
                    if test_framework == "RSpec":
                        st.code("""# Run model specs
bundle exec rspec spec/models/

# Run with coverage
COVERAGE=true bundle exec rspec

# Run specific test
bundle exec rspec spec/models/user_spec.rb:42""", language="bash")
                        
                        st.markdown("""
                        **RSpec Best Practices:**
                        - Use FactoryBot for test data
                        - Test validations thoroughly
                        - Use shared examples for common behavior
                        - Keep tests focused and fast
                        """)
                    elif test_framework == "Minitest":
                        st.code("""# Run model tests
rails test test/models/

# Run specific test
rails test test/models/user_test.rb""", language="bash")
                
                with tabs[3]:
                    st.subheader("📚 ActiveRecord Patterns")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### Validations
                        
                        **Common Validations**
                        ```ruby
                        validates :email, presence: true, uniqueness: true
                        validates :age, numericality: { greater_than: 0 }
                        validates :website, format: { with: URI.regexp }
                        validates :terms, acceptance: true
                        ```
                        
                        **Custom Validations**
                        ```ruby
                        validate :custom_validation
                        
                        private
                        def custom_validation
                          errors.add(:base, "Message") if condition
                        end
                        ```
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Associations
                        
                        **Types**
                        ```ruby
                        belongs_to :user
                        has_many :comments
                        has_one :profile
                        has_many :through
                        has_and_belongs_to_many :tags
                        ```
                        
                        **Options**
                        ```ruby
                        has_many :comments, dependent: :destroy
                        belongs_to :user, optional: true
                        has_one :profile, inverse_of: :user
                        ```
                        """)
                    
                    st.divider()
                    
                    # Links to documentation
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.link_button(
                            "ActiveRecord Basics",
                            "https://guides.rubyonrails.org/active_record_basics.html",
                            use_container_width=True
                        )
                    with col2:
                        st.link_button(
                            "Validations Guide",
                            "https://guides.rubyonrails.org/active_record_validations.html",
                            use_container_width=True
                        )
                    with col3:
                        st.link_button(
                            "Associations Guide",
                            "https://guides.rubyonrails.org/association_basics.html",
                            use_container_width=True
                        )
                
                with tabs[4]:
                    st.subheader("💾 Save Model to Library")
                    
                    title = st.text_input(
                        "Title",
                        value=f"Rails Model: {model_info['model_name']}"
                    )
                    
                    project_name = st.text_input(
                        "Project Name",
                        placeholder="MyRailsApp"
                    )
                    
                    notes = st.text_area(
                        "Implementation Notes",
                        placeholder="Add any notes about this model design..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"rails, model, {orm.lower()}, {model_info['model_name'].lower()}"
                    )
                    
                    if st.button("💾 Save Model", type="primary"):
                        if db.connected:
                            # Compile full content
                            full_content = f"""## Rails Model: {model_info['model_name']}

## Description
{description}

## Configuration
- ORM: {orm}
- Test Framework: {test_framework}
- Devise: {'Yes' if add_devise else 'No'}
- UUID Keys: {'Yes' if add_uuid else 'No'}

## Generated Code
{result['code']}

## Project: {project_name}
## Notes: {notes}"""
                            
                            metadata = {
                                "model_name": model_info["model_name"],
                                "orm": orm,
                                "test_framework": test_framework,
                                "add_devise": add_devise,
                                "add_factory": add_factory,
                                "add_uuid": add_uuid,
                                "project": project_name
                            }
                            
                            query_id = generator.db.log_query(
                                tool="rails_model_gen",
                                model=generator.default_model,
                                prompt=description,
                                response=result["code"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=full_content,
                                    category="Rails Models",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Model saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")
    
    # ActiveRecord examples
    with st.expander("📖 ActiveRecord Examples"):
        example_tabs = st.tabs(["Scopes", "Callbacks", "Concerns"])
        
        with example_tabs[0]:
            st.markdown("**Scopes**")
            st.code("""
class Article < ApplicationRecord
  scope :published, -> { where(published: true) }
  scope :recent, -> { order(created_at: :desc) }
  scope :by_author, ->(author) { where(author: author) }
  
  # Combining scopes
  scope :recent_published, -> { published.recent }
end

# Usage
Article.published.recent.limit(10)
            """, language="ruby")
        
        with example_tabs[1]:
            st.markdown("**Callbacks**")
            st.code("""
class User < ApplicationRecord
  before_save :normalize_email
  after_create :send_welcome_email
  before_destroy :check_for_orders
  
  private
  
  def normalize_email
    self.email = email.downcase.strip
  end
  
  def send_welcome_email
    UserMailer.welcome(self).deliver_later
  end
end
            """, language="ruby")
        
        with example_tabs[2]:
            st.markdown("**Concerns**")
            st.code("""
# app/models/concerns/searchable.rb
module Searchable
  extend ActiveSupport::Concern
  
  included do
    scope :search, ->(query) {
      where("name LIKE ?", "%#{query}%")
    }
  end
  
  class_methods do
    def searchable_fields
      [:name, :description]
    end
  end
end

# Usage
class Product < ApplicationRecord
  include Searchable
end
            """, language="ruby")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
