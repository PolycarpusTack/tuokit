"""
SmallTalk Class Generator for TuoKit
Generates complete VisualWorks SmallTalk class definitions using DeepSeek models
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Smalltalk Class Gen - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SmallTalkClassGenerator(OllamaToolBase):
    """SmallTalk class generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_class_gen",
            default_model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
        )
    
    def generate_class(self, description: str, include_tests: bool = False,
                      include_examples: bool = True) -> dict:
        """Generate complete SmallTalk class definition"""
        
        enhancements = []
        if include_tests:
            enhancements.append("Include SUnit test class")
        if include_examples:
            enhancements.append("Include usage examples")
            
        prompt = f"""Generate a complete VisualWorks SmallTalk class for: {description}

Requirements:
1. Proper subclassing with correct superclass
2. Instance and class variables as needed
3. Initialize method
4. Accessor methods (getters/setters)
5. Core business methods
6. Class-side creation methods if appropriate
7. Comments explaining the class purpose

{chr(10).join(enhancements) if enhancements else ''}

Follow VisualWorks naming conventions and best practices."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a SmallTalk expert. Generate clean, idiomatic VisualWorks SmallTalk code."
        )
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def extract_class_info(self, code: str) -> dict:
        """Extract class name and structure from generated code"""
        info = {
            "class_name": "UnknownClass",
            "superclass": "Object",
            "instance_vars": [],
            "class_vars": []
        }
        
        for line in code.splitlines():
            if "subclass: #" in line:
                parts = line.split("#")
                if len(parts) > 1:
                    info["class_name"] = parts[1].split()[0]
                if "Object" in line:
                    info["superclass"] = "Object"
                elif line.strip().startswith("'"):
                    info["superclass"] = line.split()[0].strip("'")
            elif "instanceVariableNames:" in line:
                vars_str = line.split("'")[1] if "'" in line else ""
                info["instance_vars"] = vars_str.split() if vars_str else []
            elif "classVariableNames:" in line:
                vars_str = line.split("'")[1] if "'" in line else ""
                info["class_vars"] = vars_str.split() if vars_str else []
        
        return info

def show():
    """Main page display function"""
    st.title("🏗️ SmallTalk Class Generator")
    st.markdown("Generate complete VisualWorks SmallTalk class definitions with AI assistance")
    
    # Initialize generator
    generator = SmallTalkClassGenerator()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Generation Options")
        
        include_tests = st.toggle(
            "Include Test Class",
            value=False,
            help="Generate corresponding SUnit test class"
        )
        
        include_examples = st.toggle(
            "Include Usage Examples",
            value=True,
            help="Add example code showing how to use the class"
        )
        
        design_pattern = st.selectbox(
            "Design Pattern",
            ["None", "Singleton", "Factory", "Observer", "Strategy", "Composite"],
            help="Apply a specific design pattern"
        )
        
        st.divider()
        st.caption("💡 **Pro Tip**: Be specific about methods and behaviors you need")
    
    # Main input area
    description = st.text_input(
        "Describe your class",
        placeholder="e.g., BankAccount with balance, deposit/withdraw methods, transaction history",
        help="Natural language description of the class you need"
    )
    
    # Quick templates
    st.markdown("### 🚀 Quick Templates")
    
    template_cols = st.columns(4)
    templates = {
        "💰 Domain Model": "Customer entity with name, email, orders collection, validation",
        "🎮 Game Object": "Player with health, position, inventory, movement methods",
        "📊 Data Structure": "Stack collection with push, pop, peek, isEmpty operations",
        "🔧 Service": "EmailService with send, queue, retry functionality"
    }
    
    for i, (name, desc) in enumerate(templates.items()):
        with template_cols[i % 4]:
            if st.button(name, key=f"template_{i}"):
                description = desc
    
    # Add design pattern to description if selected
    if design_pattern != "None":
        pattern_desc = f" (implement as {design_pattern} pattern)"
        if pattern_desc not in description:
            description += pattern_desc
    
    # Generate button
    if st.button("🏗️ Generate Class", type="primary", disabled=not description.strip()):
        with st.spinner("Generating SmallTalk class..."):
            result = generator.generate_class(
                description,
                include_tests=include_tests,
                include_examples=include_examples
            )
            
            if not result["error"]:
                st.success("✅ Class generated successfully!")
                
                # Extract class information
                class_info = generator.extract_class_info(result["code"])
                
                # Display metrics
                col1, col2, col3, col4 = st.columns(4)
                with col1:
                    st.metric("Class", class_info["class_name"])
                with col2:
                    st.metric("Superclass", class_info["superclass"])
                with col3:
                    st.metric("Instance Vars", len(class_info["instance_vars"]))
                with col4:
                    st.metric("Class Vars", len(class_info["class_vars"]))
                
                # Display in tabs
                tabs = st.tabs(["📝 Class Code", "🏛️ Structure", "📚 Concepts", "💾 Save"])
                
                with tabs[0]:
                    st.code(result["code"], language="smalltalk")
                    
                    # Download button
                    st.download_button(
                        "📥 Download Class",
                        data=result["code"],
                        file_name=f"{class_info['class_name']}.st",
                        mime="text/plain"
                    )
                
                with tabs[1]:
                    st.subheader("Class Structure")
                    
                    # Visual representation
                    st.markdown(f"""
                    ```
                    {class_info['superclass']}
                        ↑
                        |
                    {class_info['class_name']}
                    ```
                    """)
                    
                    if class_info["instance_vars"]:
                        st.markdown("**Instance Variables:**")
                        for var in class_info["instance_vars"]:
                            st.markdown(f"- `{var}`")
                    
                    if class_info["class_vars"]:
                        st.markdown("**Class Variables:**")
                        for var in class_info["class_vars"]:
                            st.markdown(f"- `{var}`")
                    
                    # Filing instructions
                    st.subheader("📁 Filing Into Image")
                    st.code(f"""
"File this into your VisualWorks image:"
"1. Copy the class definition"
"2. Open System Browser"
"3. Select target category"
"4. Paste in code pane"
"5. Accept (Ctrl+S)"

"Or file in from workspace:"
'{class_info['class_name']}.st' asFilename fileIn.
                    """, language="smalltalk")
                
                with tabs[2]:
                    st.subheader("🧠 SmallTalk OOP Principles")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### Core Concepts
                        
                        **Everything is an Object**
                        - Classes are objects too
                        - Methods are objects
                        - Even nil is an object
                        
                        **Message Passing**
                        - Not method calls
                        - Dynamic dispatch
                        - Messages can be intercepted
                        
                        **Live System**
                        - Modify classes at runtime
                        - Inspect any object
                        - Debug running code
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Class Design
                        
                        **Single Responsibility**
                        - One class, one purpose
                        - Cohesive methods
                        - Clear abstractions
                        
                        **Encapsulation**
                        - Private instance variables
                        - Public message interface
                        - Protected through protocols
                        
                        **Inheritance**
                        - Single inheritance only
                        - Favor composition
                        - Abstract superclasses
                        """)
                    
                    st.info("💡 **Best Practice**: Start with simple classes and refactor as needed")
                
                with tabs[3]:
                    st.subheader("💾 Save to Knowledge Library")
                    
                    title = st.text_input(
                        "Title",
                        value=f"SmallTalk Class: {class_info['class_name']}"
                    )
                    
                    notes = st.text_area(
                        "Implementation Notes",
                        placeholder="Add any notes about this class design..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"smalltalk, class, {class_info['class_name'].lower()}"
                    )
                    
                    if st.button("💾 Save Class", type="primary"):
                        if db.connected:
                            metadata = {
                                "class_name": class_info["class_name"],
                                "superclass": class_info["superclass"],
                                "instance_vars": class_info["instance_vars"],
                                "class_vars": class_info["class_vars"],
                                "include_tests": include_tests,
                                "include_examples": include_examples,
                                "design_pattern": design_pattern,
                                "notes": notes
                            }
                            
                            query_id = generator.db.log_query(
                                tool="smalltalk_class_gen",
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
                                    category="SmallTalk Classes",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Class saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Generation failed. Please check your Ollama connection.")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
