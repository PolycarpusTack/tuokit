"""
Seaside Component Generator for TuoKit
Creates Seaside web components for SmallTalk web applications
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Seaside Generator - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SeasideComponentGenerator(OllamaToolBase):
    """Seaside web component generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="seaside_generator",
            default_model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
        )
    
    def generate_seaside_component(self, description: str, 
                                  component_type: str = "Basic",
                                  include_ajax: bool = False,
                                  include_css: bool = True) -> dict:
        """Generate Seaside web component"""
        
        enhancements = []
        if include_ajax:
            enhancements.append("Include AJAX/jQuery callbacks")
        if include_css:
            enhancements.append("Include CSS styling methods")
        if component_type == "Form":
            enhancements.append("Include form validation")
        elif component_type == "Report":
            enhancements.append("Include data table rendering")
            
        prompt = f"""Create a Seaside web component for: {description}

Component Type: {component_type}
{chr(10).join(enhancements) if enhancements else ''}

Generate SmallTalk code with:
1. WAComponent subclass definition
2. renderContentOn: method
3. Callback methods for user interactions
4. State management (instance variables)
5. Initialize method
6. updateRoot: for adding CSS/JS if needed
7. States method if component has multiple states
8. Validation methods if applicable

Follow Seaside best practices and conventions."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a Seaside framework expert. Generate clean, working Seaside components."
        )
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def generate_magritte_description(self, component_desc: str) -> str:
        """Generate Magritte descriptions for the component"""
        prompt = f"""Generate Magritte descriptions for this Seaside component: {component_desc}

Include:
- Field descriptions with proper types
- Validation rules
- Display hints
- Accessor methods"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🌊 Seaside Component Generator")
    st.markdown("Create web components for Seaside SmallTalk applications")
    
    # Initialize generator
    generator = SeasideComponentGenerator()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Component Configuration")
        
        component_type = st.selectbox(
            "Component Type",
            ["Basic", "Form", "Report", "Navigation", "Dashboard", "Custom"],
            help="Type of Seaside component to generate"
        )
        
        st.divider()
        
        include_ajax = st.toggle(
            "Include AJAX/jQuery",
            value=False,
            help="Add asynchronous callbacks"
        )
        
        include_css = st.toggle(
            "Include CSS Styling",
            value=True,
            help="Add CSS methods"
        )
        
        include_magritte = st.toggle(
            "Include Magritte",
            value=False,
            help="Add Magritte descriptions"
        )
        
        st.divider()
        
        # Component features
        st.subheader("🔧 Features")
        features = []
        
        if st.checkbox("Authentication"):
            features.append("authentication")
        if st.checkbox("Pagination"):
            features.append("pagination")
        if st.checkbox("File Upload"):
            features.append("file upload")
        if st.checkbox("Export (CSV/PDF)"):
            features.append("export")
        
        st.caption("💡 **Tip**: Seaside uses continuations for flow control")
    
    # Main input
    description = st.text_input(
        "Describe the web component",
        placeholder="e.g., User registration form with email validation and terms acceptance",
        help="What should this Seaside component do?"
    )
    
    # Quick templates
    st.markdown("### 🌐 Component Templates")
    
    template_cols = st.columns(3)
    templates = {
        "👤 User Registration": "Registration form with name, email, password fields, validation, and email confirmation",
        "📝 Blog Editor": "Blog post editor with title, content (rich text), tags, publish date, and preview",
        "🛒 Product Listing": "Product grid with images, prices, filters, sorting, and add to cart buttons",
        "📊 Data Report": "Tabular report with sorting, filtering, pagination, and CSV export",
        "🔍 Search Interface": "Advanced search form with multiple criteria, autocomplete, and result display",
        "📱 Contact Form": "Contact form with name, email, subject, message, and CAPTCHA"
    }
    
    for i, (name, desc) in enumerate(templates.items()):
        with template_cols[i % 3]:
            if st.button(name, key=f"seaside_template_{i}"):
                description = desc
    
    # Generate button
    if st.button("🌊 Generate Component", type="primary", disabled=not description.strip()):
        with st.spinner("Creating Seaside component..."):
            result = generator.generate_seaside_component(
                description,
                component_type=component_type,
                include_ajax=include_ajax,
                include_css=include_css
            )
            
            if not result["error"]:
                st.success("✅ Seaside component generated!")
                
                # Display in tabs
                tabs = ["📝 Component Code", "🎨 Styling", "⚓ Seaside Guide", "💾 Save"]
                if include_magritte:
                    tabs.insert(2, "🔮 Magritte")
                
                tab_objects = st.tabs(tabs)
                
                with tab_objects[0]:
                    st.code(result["code"], language="smalltalk")
                    
                    # Download button
                    st.download_button(
                        "📥 Download Component",
                        data=result["code"],
                        file_name="seaside_component.st",
                        mime="text/plain"
                    )
                    
                    # Registration code
                    st.subheader("📋 Component Registration")
                    st.code("""
"Register the component:"
WAAdmin register: MyComponent asApplicationAt: 'myapp'.

"Or with configuration:"
(WAAdmin register: MyComponent asApplicationAt: 'myapp')
    preferenceAt: #sessionClass put: MySession;
    preferenceAt: #renderPhaseContinuationClass put: WARenderPhaseContinuation.

"Access at: http://localhost:8080/myapp"
                    """, language="smalltalk")
                
                with tab_objects[1]:
                    st.subheader("🎨 CSS Styling")
                    
                    st.code("""
"Add CSS in updateRoot: method:"
MyComponent>>updateRoot: anHtmlRoot
    super updateRoot: anHtmlRoot.
    anHtmlRoot stylesheet url: '/css/myapp.css'.
    
    "Or inline CSS:"
    anHtmlRoot style: '
        .my-component { padding: 20px; }
        .my-button { background: #007bff; color: white; }
    '

"Using CSS in rendering:"
MyComponent>>renderContentOn: html
    html div
        class: 'my-component';
        with: [
            html heading: 'My Component'.
            html anchor
                class: 'my-button';
                callback: [ self doSomething ];
                with: 'Click Me' ]
                    """, language="smalltalk")
                    
                    # CSS framework integration
                    st.subheader("🎨 CSS Frameworks")
                    
                    framework_col1, framework_col2 = st.columns(2)
                    
                    with framework_col1:
                        st.markdown("""
                        **Bootstrap Integration**
                        ```smalltalk
                        updateRoot: anHtmlRoot
                            super updateRoot: anHtmlRoot.
                            anHtmlRoot stylesheet url: 
                                'https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css'
                        ```
                        """)
                    
                    with framework_col2:
                        st.markdown("""
                        **Tailwind CSS**
                        ```smalltalk
                        html div
                            class: 'bg-blue-500 text-white p-4 rounded';
                            with: 'Styled with Tailwind'
                        ```
                        """)
                
                # Magritte tab (conditional)
                if include_magritte and len(tab_objects) > 4:
                    with tab_objects[2]:
                        st.subheader("🔮 Magritte Descriptions")
                        
                        with st.spinner("Generating Magritte descriptions..."):
                            magritte_code = generator.generate_magritte_description(description)
                            st.code(magritte_code, language="smalltalk")
                        
                        st.info("💡 Magritte provides meta-descriptions for automatic form generation")
                        
                        # Magritte example
                        st.markdown("### Example Usage")
                        st.code("""
"Automatic form rendering with Magritte:"
renderContentOn: html
    html render: (self asComponent
        addValidatedForm;
        yourself)
                        """, language="smalltalk")
                
                # Seaside Guide tab
                guide_tab_index = 3 if include_magritte else 2
                with tab_objects[guide_tab_index]:
                    st.subheader("⚓ Seaside Framework Guide")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### Core Concepts
                        
                        **Component-Based**
                        - Reusable UI elements
                        - Composable hierarchy
                        - Stateful by default
                        
                        **Callbacks**
                        - No URL routing
                        - Direct method callbacks
                        - Type-safe parameters
                        
                        **Session Management**
                        - Automatic state tracking
                        - Back button support
                        - Continuations
                        
                        **Canvas API**
                        - `html div: 'content'`
                        - `html anchor callback: []`
                        - `html form: []`
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Best Practices
                        
                        **Component Design**
                        - Small, focused components
                        - Clear responsibilities
                        - Proper initialization
                        
                        **State Management**
                        - Instance variables for state
                        - Announcements for events
                        - Proper cleanup
                        
                        **Performance**
                        - Use AJAX for updates
                        - Minimize session state
                        - Cache expensive operations
                        
                        **Security**
                        - Validate all inputs
                        - Use CSRF protection
                        - Escape output properly
                        """)
                    
                    # Common patterns
                    st.divider()
                    st.subheader("📚 Common Patterns")
                    
                    pattern_tabs = st.tabs(["Forms", "AJAX", "Navigation", "Tasks"])
                    
                    with pattern_tabs[0]:
                        st.code("""
"Form with validation:"
renderFormOn: html
    html form: [
        html label: 'Email:'.
        html textInput
            on: #email of: self;
            placeholder: 'user@example.com'.
        
        html submitButton
            callback: [ self validateAndSave ];
            with: 'Submit' ]
                        """, language="smalltalk")
                    
                    with pattern_tabs[1]:
                        st.code("""
"AJAX update:"
html div
    id: 'result';
    with: [ self renderResultOn: html ].

html anchor
    onClick: (html jQuery id: 'result') 
        load html: [ :h | self renderResultOn: h ];
    with: 'Refresh'
                        """, language="smalltalk")
                    
                    with pattern_tabs[2]:
                        st.code("""
"Navigation:"
self call: (AnotherComponent new
    onAnswer: [ :value | 
        self processAnswer: value ])
                        """, language="smalltalk")
                    
                    with pattern_tabs[3]:
                        st.code("""
"Background task:"
self session 
    addBackgroundTask: [
        [ self updateData ] 
            ensure: [ self announce: DataUpdated new ]]
                        """, language="smalltalk")
                
                # Save tab
                save_tab_index = 4 if include_magritte else 3
                with tab_objects[save_tab_index]:
                    st.subheader("💾 Save Component")
                    
                    title = st.text_input(
                        "Title",
                        value=f"Seaside: {description[:40]}..."
                    )
                    
                    app_name = st.text_input(
                        "Application Name",
                        placeholder="myapp"
                    )
                    
                    notes = st.text_area(
                        "Implementation Notes",
                        placeholder="Add notes about this component..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"seaside, web, {component_type.lower()}"
                    )
                    
                    if st.button("💾 Save Component", type="primary"):
                        if db.connected:
                            # Compile content
                            full_content = f"""## Seaside Component Code
{result['code']}

## Component Type: {component_type}
## Features: AJAX={'Yes' if include_ajax else 'No'}, CSS={'Yes' if include_css else 'No'}
## Application: {app_name}

{f'## Magritte Descriptions\n{magritte_code}' if include_magritte and 'magritte_code' in locals() else ''}

## Notes
{notes}"""
                            
                            metadata = {
                                "component_type": component_type,
                                "include_ajax": include_ajax,
                                "include_css": include_css,
                                "include_magritte": include_magritte,
                                "app_name": app_name,
                                "features": features if 'features' in locals() else []
                            }
                            
                            query_id = generator.db.log_query(
                                tool="seaside_generator",
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
                                    category="Seaside Components",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Component saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
