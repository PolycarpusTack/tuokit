# pages/view_components.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="View Components - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate

def generate_component(description, config):
    """Generate ViewComponent with tests and stimulus integration"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Create ViewComponent for: {description} | Config: {config}",
        system=(
            "Include:\n"
            "- Component class (Ruby)\n"
            "- Template (ERB/HAML/SLIM)\n"
            "- Stimulus controller (JavaScript)\n"
            "- RSpec tests\n"
            "- Preview/Storybook integration\n"
            "Follow ViewComponent best practices"
        )
    )['response']

def generate_usage_example(description):
    """Generate usage examples for the component"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Show usage example for component: {description}",
        system="Render component in ERB view with different props and states"
    )['response']

def show():
    st.title("🧩 Rails View Component Generator")
    st.caption("Create reusable, testable view components with Stimulus integration")
    
    # Component description
    description = st.text_area("Describe Component", 
                              height=150,
                              placeholder="e.g., Notification badge with count, styles, and animation",
                              key="component_desc")
    
    # Configuration
    with st.sidebar:
        st.subheader("Component Options")
        template_lang = st.radio("Template Language", ["ERB", "HAML", "SLIM"])
        js_framework = st.selectbox("JavaScript Framework", ["Stimulus", "Turbo Streams", "Vanilla JS", "Alpine.js"])
        accessibility = st.toggle("Add Accessibility Features", True)
        dark_mode = st.toggle("Include Dark Mode Support", False)
        responsive = st.toggle("Responsive Design", True)
        
        st.subheader("Component Features")
        features = st.multiselect("Additional Features",
                                ["Slots", "Variants", "I18n Support", "Form Integration", 
                                 "Animation", "Loading States"],
                                default=["Variants"])
    
    if st.button("Generate Component", type="primary") and description:
        with st.spinner("Building component..."):
            config = {
                "template": template_lang,
                "js": js_framework,
                "accessibility": accessibility,
                "dark_mode": dark_mode,
                "responsive": responsive,
                "features": features
            }
            
            component = generate_component(description, config)
            
            # Display results in tabs
            tab1, tab2, tab3, tab4, tab5 = st.tabs(["Component", "Usage", "Tests", "Styling", "Setup"])
            
            with tab1:
                st.subheader("Component Implementation")
                st.code(component, language="ruby")
                st.download_button("Download Component", component, "component.rb")
                
            with tab2:
                st.subheader("Usage Examples")
                example = generate_usage_example(description)
                st.code(example, language="erb")
                
                # Show different states
                st.caption("**Component States:**")
                states = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Show component in different states: {description}",
                    system="Show default, loading, error, and success states"
                )['response']
                st.code(states, language="erb")
                
            with tab3:
                st.subheader("Component Tests")
                tests = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Generate RSpec tests for ViewComponent: {description}",
                    system="Include unit tests, rendering tests, and accessibility tests"
                )['response']
                st.code(tests, language="ruby")
                st.download_button("Download Tests", tests, "component_spec.rb")
                
            with tab4:
                st.subheader("Component Styling")
                if js_framework == "Stimulus":
                    stimulus_controller = safe_ollama_generate(
                        model=ModelManager.get_default_model(),
                        prompt=f"Generate Stimulus controller for: {description}",
                        system="Modern JavaScript with event handling and state management"
                    )['response']
                    st.code(stimulus_controller, language="javascript")
                    
                styles = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Generate CSS for component: {description}",
                    system=f"Include {'dark mode' if dark_mode else ''} {'responsive' if responsive else ''} styles"
                )['response']
                st.code(styles, language="css")
                
            with tab5:
                st.markdown("""
                ### Setup Instructions
                
                1. **Install ViewComponent:**
                ```bash
                bundle add view_component
                rails generate component Example
                ```
                
                2. **Directory Structure:**
                ```
                app/components/
                ├── application_component.rb
                ├── example_component.rb
                ├── example_component.html.erb
                └── example_component.js
                
                spec/components/
                └── example_component_spec.rb
                ```
                
                3. **Preview Setup:**
                ```ruby
                # config/routes.rb
                mount Lookbook::Engine, at: '/lookbook' if Rails.env.development?
                ```
                
                4. **Stimulus Setup:**
                ```javascript
                // app/javascript/controllers/index.js
                import ExampleController from "./example_controller"
                application.register("example", ExampleController)
                ```
                """)
            
            # Component anatomy
            with st.expander("⚙️ ViewComponent Best Practices", expanded=True):
                col1, col2 = st.columns(2)
                
                with col1:
                    st.markdown("""
                    **Component Design:**
                    - Single responsibility
                    - Props over instance variables
                    - Composition over inheritance
                    - Test-driven development
                    
                    **Performance:**
                    - Avoid N+1 queries
                    - Cache expensive operations
                    - Use lazy loading
                    - Minimize JavaScript
                    """)
                
                with col2:
                    st.markdown("""
                    **Accessibility:**
                    - Semantic HTML
                    - ARIA labels
                    - Keyboard navigation
                    - Screen reader support
                    
                    **Testing:**
                    - Unit test logic
                    - Render test output
                    - Accessibility tests
                    - Visual regression tests
                    """)
                
                st.link_button("ViewComponent Documentation", "https://viewcomponent.org/")
            
            # Component patterns
            with st.expander("💡 Common Component Patterns"):
                st.markdown("""
                **Slots Pattern:**
                ```ruby
                class CardComponent < ViewComponent::Base
                  renders_one :header
                  renders_one :footer
                  renders_many :actions
                end
                ```
                
                **Variants Pattern:**
                ```ruby
                class ButtonComponent < ViewComponent::Base
                  def initialize(variant: :primary)
                    @variant = variant
                  end
                  
                  def variant_classes
                    {
                      primary: "btn-primary",
                      secondary: "btn-secondary",
                      danger: "btn-danger"
                    }[@variant]
                  end
                end
                ```
                
                **Conditional Rendering:**
                ```ruby
                def render?
                  @items.present?
                end
                ```
                """)
            
            # Save to knowledge base
            if st.button("💾 Add to Component Library"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="view_components",
                        model=ModelManager.get_default_model(),
                        prompt=description,
                        response=component,
                        metadata={
                            "tags": ["rails", "frontend", "components"],
                            "template": template_lang,
                            "js_framework": js_framework
                        }
                    )
                    if query_id:
                        st.success("Component saved to library!")
                else:
                    st.error("Could not connect to database")

if __name__ == "__main__":
    show()
