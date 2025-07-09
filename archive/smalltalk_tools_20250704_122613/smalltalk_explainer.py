"""
SmallTalk Code Explainer for TuoKit
Analyzes and explains VisualWorks SmallTalk code using DeepSeek models
Enhanced with complexity levels and detailed analysis options
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Smalltalk Explainer - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SmallTalkExplainer(OllamaToolBase):
    """SmallTalk code analysis and explanation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_explainer",
            default_model=st.session_state.get("selected_model", "deepseek-r1:6.7b")
        )
    
    def explain_code(self, code: str, detail_level: str = "Detailed", 
                     include_tips: bool = True, compare_oop: bool = False) -> str:
        """Generate detailed explanation of SmallTalk code with configurable options"""
        
        # Build system prompt based on options
        sections = [
            "1) Overall Purpose - What does this code accomplish?",
            "2) Key SmallTalk Concepts - Important language features used",
            "3) Execution Flow - Step-by-step walkthrough"
        ]
        
        if detail_level == "Advanced":
            sections.append("4) Implementation Details - Deep dive into internals")
        
        if include_tips:
            sections.append("5) Potential Improvements - Optimization suggestions")
            
        if compare_oop:
            sections.append("6) OOP Paradigm Comparison - How this differs from Java/C++")
        
        system_prompt = f"""You are an expert SmallTalk developer. 
Provide a {detail_level.lower()}-level explanation with these sections:
{chr(10).join(sections)}

Use clear language appropriate for the detail level selected."""

        prompt = f"""Explain this VisualWorks SmallTalk code:

```smalltalk
{code}
```"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2,
            system=system_prompt
        )
        
        return result["response"] if not result["error"] else result["response"]

def show():
    """Main page display function"""
    st.title("🧑‍🏫 SmallTalk Code Explainer")
    st.markdown("Analyze and understand VisualWorks SmallTalk code with AI-powered explanations")
    
    # Initialize explainer
    explainer = SmallTalkExplainer()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Analysis Options")
        
        detail_level = st.select_slider(
            "Detail Level",
            options=["Basic", "Detailed", "Advanced"],
            value="Detailed",
            help="Basic: Quick overview | Detailed: Standard analysis | Advanced: Deep dive"
        )
        
        st.divider()
        
        include_tips = st.toggle(
            "Include Optimization Tips",
            value=True,
            help="Get suggestions for improving the code"
        )
        
        compare_oop = st.toggle(
            "Compare to Other OOP Languages",
            value=False,
            help="See how SmallTalk differs from Java/C++"
        )
        
        st.divider()
        st.caption("💡 **Pro Tip**: Use Advanced mode for learning SmallTalk paradigms in depth")
    
    # Main content area
    code = st.text_area(
        "Paste SmallTalk Code",
        height=300,
        placeholder="""Example:
OrderedCollection new
    add: 'first';
    add: 'second';
    yourself

Or paste a class definition:
Object subclass: #Person
    instanceVariableNames: 'name age'
    classVariableNames: ''
    poolDictionaries: ''"""
    )
    
    # Quick examples
    col1, col2, col3 = st.columns(3)
    with col1:
        if st.button("📚 Load Collection Example"):
            code = """numbers := OrderedCollection new.
1 to: 10 do: [:i | numbers add: i squared].
sum := numbers inject: 0 into: [:total :each | total + each]"""
            
    with col2:
        if st.button("🏗️ Load Class Example"):
            code = """Object subclass: #BankAccount
    instanceVariableNames: 'balance owner'
    classVariableNames: 'InterestRate'
    poolDictionaries: ''
    
BankAccount >> deposit: amount
    balance := balance + amount"""
    
    with col3:
        if st.button("🔄 Load Block Example"):
            code = """| factorial |
factorial := [:n | 
    n <= 1 
        ifTrue: [1]
        ifFalse: [n * (factorial value: n - 1)]].
factorial value: 5"""
    
    # Analyze button
    if st.button("🔍 Explain Code", type="primary", disabled=not code.strip()):
        with st.spinner(f"Analyzing SmallTalk code ({detail_level} mode)..."):
            explanation = explainer.explain_code(
                code, 
                detail_level=detail_level,
                include_tips=include_tips,
                compare_oop=compare_oop
            )
            
            # Display explanation in tabs
            tab1, tab2, tab3 = st.tabs(["📖 Explanation", "🧠 Learning Resources", "💾 Knowledge Base"])
            
            with tab1:
                st.markdown(explanation)
                
                # Feedback section
                col1, col2 = st.columns([4, 1])
                with col2:
                    if st.button("📋 Copy", help="Copy explanation to clipboard"):
                        st.write("Copy functionality requires JavaScript")
            
            with tab2:
                st.subheader("SmallTalk Learning Resources")
                
                col1, col2 = st.columns(2)
                
                with col1:
                    st.markdown("""
                    ### Core Concepts
                    
                    **Message Passing**
                    - Everything is done by sending messages
                    - Syntax: `receiver message` or `receiver message: arg`
                    - Example: `3 + 4` sends `+` message to `3`
                    
                    **Blocks (Closures)**
                    - Anonymous functions: `[:arg | expression]`
                    - Can capture variables from enclosing scope
                    - Evaluated with `value` message
                    
                    **Method Cascading**
                    - Send multiple messages using `;`
                    - Example: `object msg1; msg2; msg3`
                    """)
                
                with col2:
                    st.markdown("""
                    ### Advanced Topics
                    
                    **Metaclasses**
                    - Classes are objects too
                    - Class methods defined on metaclass
                    - `MyClass class` accesses metaclass
                    
                    **Image-based Development**
                    - Live coding environment
                    - Persistent object memory
                    - No file-based compilation
                    
                    **Reflection**
                    - Inspect any object: `anObject inspect`
                    - Browse methods: `anObject class browse`
                    """)
                
                st.info("📚 **Recommended Reading**: 'SmallTalk Best Practice Patterns' by Kent Beck")
                
                # External resources
                st.markdown("### 🔗 External Resources")
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.link_button("Pharo MOOC", "https://mooc.pharo.org", use_container_width=True)
                with col2:
                    st.link_button("Squeak Wiki", "https://wiki.squeak.org", use_container_width=True)
                with col3:
                    st.link_button("GNU SmallTalk", "https://www.gnu.org/software/smalltalk/", use_container_width=True)
            
            with tab3:
                st.subheader("Save to Knowledge Base")
                
                # Knowledge unit form
                title = st.text_input(
                    "Title for this analysis",
                    value=f"SmallTalk: {code[:30]}..." if len(code) > 30 else f"SmallTalk: {code}"
                )
                
                category = st.selectbox(
                    "Category",
                    ["SmallTalk Basics", "OOP Patterns", "Collections", "GUI/MVC", "Advanced Topics"]
                )
                
                tags = st.text_input(
                    "Tags (comma-separated)",
                    placeholder="smalltalk, collections, blocks"
                )
                
                if st.button("💾 Save Analysis", type="primary"):
                    db = DatabaseManager()
                    if db.connected:
                        # Save the query first
                        query_id = explainer.db.log_query(
                            tool="smalltalk_explainer",
                            model=explainer.default_model,
                            prompt=code,
                            response=explanation,
                            metadata={
                                "detail_level": detail_level,
                                "include_tips": include_tips,
                                "compare_oop": compare_oop
                            }
                        )
                        
                        # Save as knowledge unit
                        if query_id and title:
                            success = db.save_knowledge_unit(
                                query_id=query_id,
                                title=title,
                                content=explanation,
                                category=category,
                                tags=[tag.strip() for tag in tags.split(",")] if tags else []
                            )
                            if success:
                                st.success(f"✅ Saved to knowledge library!")
                                st.balloons()
                            else:
                                st.error("Failed to save as knowledge unit")
                    else:
                        st.warning("Database not connected")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
