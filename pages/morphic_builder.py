"""
Morphic UI Builder for TuoKit
Creates Morphic UI interfaces for VisualWorks SmallTalk
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Morphic Builder - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class MorphicUIBuilder(OllamaToolBase):
    """Morphic UI generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="morphic_builder",
            default_model=ModelManager.get_default_model()
        )
    
    def generate_morphic_ui(self, description: str, theme: str = "System",
                           layout: str = "Vertical", components: list = None) -> dict:
        """Generate Morphic UI code"""
        
        components_str = ""
        if components:
            components_str = f"\nRequired components: {', '.join(components)}"
        
        prompt = f"""Create a Morphic UI for VisualWorks SmallTalk: {description}

Theme: {theme}
Layout: {layout}{components_str}

Requirements:
1. Complete Morph subclass definition
2. Initialize method setting up UI
3. Layout management ({layout.lower()} arrangement)
4. Event handlers for user interaction
5. Proper opening/closing methods
6. Comments explaining the structure

Include:
- Buttons with action blocks
- Text fields/areas as needed
- Labels and formatting
- Proper morphic hierarchy
- Example usage code"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="Generate clean Morphic UI code following VisualWorks conventions. Use proper morph composition."
        )
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def generate_event_handlers(self, ui_description: str) -> str:
        """Generate event handler methods"""
        prompt = f"""Generate SmallTalk event handler methods for this UI: {ui_description}

Include handlers for:
- Button clicks
- Text field changes
- Mouse events if needed
- Keyboard shortcuts if appropriate"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🎨 Morphic UI Builder")
    st.markdown("Create Morphic user interfaces for VisualWorks SmallTalk")
    
    # Initialize builder
    builder = MorphicUIBuilder()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ UI Configuration")
        
        theme = st.selectbox(
            "Theme",
            ["System", "Light", "Dark", "Custom"],
            help="Visual theme for the UI"
        )
        
        layout = st.radio(
            "Layout",
            ["Vertical", "Horizontal", "Grid", "Flow"],
            help="How components are arranged"
        )
        
        st.divider()
        
        # Component selection
        st.subheader("🧩 Components")
        components = []
        
        if st.checkbox("Buttons", value=True):
            components.append("Buttons")
        if st.checkbox("Text Fields", value=True):
            components.append("Text Fields")
        if st.checkbox("Lists/Tables"):
            components.append("Lists")
        if st.checkbox("Menus"):
            components.append("Menus")
        if st.checkbox("Progress Bars"):
            components.append("Progress Indicators")
        if st.checkbox("Images"):
            components.append("Image Morphs")
        
        st.divider()
        st.caption("💡 **Tip**: Morphic supports drag-and-drop UI building")
    
    # Main input
    description = st.text_input(
        "Describe the UI",
        placeholder="e.g., Login form with username/password fields and submit/cancel buttons",
        help="Natural language description of your UI needs"
    )
    
    # Quick templates
    st.markdown("### 🎯 UI Templates")
    
    template_cols = st.columns(3)
    templates = {
        "🔐 Login Form": "Login dialog with username and password fields, remember me checkbox, submit and cancel buttons",
        "📝 Data Entry": "Form with multiple text fields, dropdown selections, date picker, save and reset buttons",
        "📊 Dashboard": "Dashboard with status indicators, charts area, refresh button, and navigation menu",
        "🔍 Search Interface": "Search bar with filters panel, results list, pagination controls",
        "⚙️ Settings Panel": "Tabbed settings interface with various options, apply and cancel buttons",
        "💬 Chat Window": "Chat interface with message list, input field, send button, user status"
    }
    
    for i, (name, desc) in enumerate(templates.items()):
        with template_cols[i % 3]:
            if st.button(name, key=f"ui_template_{i}"):
                description = desc
    
    # Generate button
    if st.button("🎨 Generate UI Code", type="primary", disabled=not description.strip()):
        with st.spinner("Building Morphic interface..."):
            result = builder.generate_morphic_ui(
                description,
                theme=theme,
                layout=layout,
                components=components
            )
            
            if not result["error"]:
                st.success("✅ Morphic UI generated successfully!")
                
                # Display in tabs
                tabs = st.tabs([
                    "📝 UI Code",
                    "🎯 Event Handlers",
                    "📚 Morphic Guide",
                    "🖼️ Layout Preview",
                    "💾 Save"
                ])
                
                with tabs[0]:
                    st.code(result["code"], language="smalltalk")
                    
                    # Download button
                    st.download_button(
                        "📥 Download UI Code",
                        data=result["code"],
                        file_name="morphic_ui.st",
                        mime="text/plain"
                    )
                    
                    # Usage instructions
                    st.subheader("🚀 Usage")
                    st.code("""
"Open the UI:"
MyMorphicUI new openInWorld.

"Or in a window:"
MyMorphicUI new openInWindowLabeled: 'My Application'.

"Or as a dialog:"
MyMorphicUI new openCenteredInWorld.
                    """, language="smalltalk")
                
                with tabs[1]:
                    st.subheader("🎯 Event Handlers")
                    
                    with st.spinner("Generating event handlers..."):
                        handlers = builder.generate_event_handlers(description)
                        st.code(handlers, language="smalltalk")
                    
                    st.info("💡 Add these methods to your Morph class for handling user interactions")
                
                with tabs[2]:
                    st.subheader("📚 Morphic Framework Guide")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### Core Concepts
                        
                        **Morphs**
                        - Visual objects
                        - Composable hierarchy
                        - Direct manipulation
                        
                        **Common Morphs**
                        - `TextMorph` - Text display
                        - `ButtonMorph` - Clickable buttons
                        - `PluggableTextMorph` - Text input
                        - `SystemWindow` - Windows
                        - `PanelMorph` - Containers
                        
                        **Properties**
                        - `color:` - Background color
                        - `extent:` - Size
                        - `position:` - Location
                        - `borderWidth:` - Border
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Layout Management
                        
                        **Automatic Layout**
                        ```smalltalk
                        aPanel layoutPolicy: TableLayout new.
                        aPanel listDirection: #topToBottom.
                        aPanel cellSpacing: 5.
                        ```
                        
                        **Manual Positioning**
                        ```smalltalk
                        aMorph position: 10@10.
                        aMorph extent: 200@100.
                        ```
                        
                        **Relative Layout**
                        ```smalltalk
                        aMorph layoutFrame: (LayoutFrame
                            fractions: (0@0 corner: 1@1)
                            offsets: (10@10 corner: -10@-10))
                        ```
                        """)
                    
                    # Morphic tips
                    st.divider()
                    st.markdown("### 🛠️ Morphic Development Tips")
                    
                    tips_col1, tips_col2 = st.columns(2)
                    
                    with tips_col1:
                        st.markdown("""
                        **Interactive Development**
                        - Alt-click to get halo
                        - Drag morphs around
                        - Inspect via halo menu
                        - Debug live
                        """)
                    
                    with tips_col2:
                        st.markdown("""
                        **Best Practices**
                        - Use SystemWindow for apps
                        - Handle window closing
                        - Cleanup in delete method
                        - Use announcements for events
                        """)
                
                with tabs[3]:
                    st.subheader("🖼️ Layout Preview")
                    
                    # ASCII art representation of layout
                    if layout == "Vertical":
                        st.code("""
┌─────────────────────────┐
│      Title/Header       │
├─────────────────────────┤
│    Component 1          │
├─────────────────────────┤
│    Component 2          │
├─────────────────────────┤
│    Component 3          │
├─────────────────────────┤
│    Buttons Row          │
└─────────────────────────┘
                        """, language="text")
                    elif layout == "Horizontal":
                        st.code("""
┌───────┬───────┬───────┬───────┐
│       │       │       │       │
│ Comp1 │ Comp2 │ Comp3 │ Comp4 │
│       │       │       │       │
└───────┴───────┴───────┴───────┘
                        """, language="text")
                    elif layout == "Grid":
                        st.code("""
┌───────┬───────┬───────┐
│   1   │   2   │   3   │
├───────┼───────┼───────┤
│   4   │   5   │   6   │
├───────┼───────┼───────┤
│   7   │   8   │   9   │
└───────┴───────┴───────┘
                        """, language="text")
                    else:  # Flow
                        st.code("""
┌────┬──────┬───┬────────┐
│ C1 │  C2  │C3 │   C4   │
├────┴───┬──┴───┼────────┤
│   C5   │  C6  │   C7   │
└────────┴──────┴────────┘
                        """, language="text")
                    
                    st.caption(f"Preview of {layout} layout arrangement")
                
                with tabs[4]:
                    st.subheader("💾 Save UI Design")
                    
                    title = st.text_input(
                        "Title",
                        value=f"Morphic UI: {description[:30]}..."
                    )
                    
                    project_name = st.text_input(
                        "Project Name",
                        placeholder="MyApplication"
                    )
                    
                    notes = st.text_area(
                        "Design Notes",
                        placeholder="Add notes about this UI design..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"morphic, ui, {layout.lower()}"
                    )
                    
                    if st.button("💾 Save UI Design", type="primary"):
                        if db.connected:
                            # Compile full content
                            full_content = f"""## Morphic UI Code
{result['code']}

## Event Handlers
{handlers if 'handlers' in locals() else 'Not generated'}

## Layout: {layout}
## Theme: {theme}
## Components: {', '.join(components)}

## Project: {project_name}
## Notes: {notes}"""
                            
                            metadata = {
                                "theme": theme,
                                "layout": layout,
                                "components": components,
                                "project": project_name,
                                "notes": notes
                            }
                            
                            query_id = builder.db.log_query(
                                tool="morphic_builder",
                                model=builder.default_model,
                                prompt=description,
                                response=result["code"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=full_content,
                                    category="Morphic UI",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ UI design saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")
    
    # Morphic examples
    with st.expander("📖 Morphic Code Examples"):
        example_col1, example_col2 = st.columns(2)
        
        with example_col1:
            st.markdown("**Simple Button**")
            st.code("""
button := SimpleButtonMorph new.
button label: 'Click Me'.
button target: self.
button actionSelector: #buttonClicked.
button openInWorld.
            """, language="smalltalk")
        
        with example_col2:
            st.markdown("**Text Input**")
            st.code("""
textMorph := PluggableTextMorph new.
textMorph setText: 'Enter text here'.
textMorph extent: 200@50.
textMorph openInWorld.
            """, language="smalltalk")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
