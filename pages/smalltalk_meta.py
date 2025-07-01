"""
SmallTalk Metaprogramming Helper for TuoKit
Assists with runtime code generation and reflection in SmallTalk
"""

import streamlit as st
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SmallTalkMetaprogrammingHelper(OllamaToolBase):
    """SmallTalk metaprogramming assistance tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_meta",
            default_model="deepseek-coder:6.7b"
        )
        
        self.meta_tasks = {
            "Add Logging": "Add logging to all methods in a class",
            "Create Accessors": "Generate getter/setter methods",
            "Method Wrappers": "Wrap methods with before/after behavior",
            "Dynamic Methods": "Create methods at runtime",
            "Class Creation": "Create classes programmatically",
            "Method Analysis": "Analyze and report on methods",
            "Performance Profiling": "Add performance measurement",
            "Deprecation Handling": "Mark methods as deprecated",
            "Proxy Objects": "Create proxy/decorator objects",
            "DSL Creation": "Build domain-specific languages"
        }
    
    def generate_metaprogramming(self, code: str, task: str, 
                               target_class: str = "") -> dict:
        """Generate metaprogramming solution"""
        
        prompt = f"""Perform this SmallTalk metaprogramming task: {task}

{f'Target class/code:\n```smalltalk\n{code}\n```' if code else ''}
{f'Target class name: {target_class}' if target_class and not code else ''}

Use SmallTalk metaprogramming capabilities:
- Runtime class/method creation
- Method compilation with Compiler
- Reflection APIs (thisContext, etc.)
- Method wrappers and proxies
- Dynamic method dispatch

Provide:
1. Complete working code
2. Clear comments explaining the metaprogramming
3. Example usage
4. Important considerations

Follow VisualWorks SmallTalk conventions."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a SmallTalk metaprogramming expert. Generate advanced but practical metaprogramming solutions."
        )
        
        return {
            "code": result["response"],
            "error": result["error"]
        }
    
    def explain_reflection_api(self, api_name: str) -> str:
        """Explain specific SmallTalk reflection API"""
        prompt = f"""Explain the SmallTalk reflection API: {api_name}

Include:
1. What it does
2. Common use cases
3. Code examples
4. Important caveats"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]
    
    def generate_dsl_example(self, domain: str) -> str:
        """Generate a domain-specific language example"""
        prompt = f"""Create a SmallTalk DSL (Domain-Specific Language) for: {domain}

Show:
1. DSL syntax design
2. Implementation using SmallTalk metaprogramming
3. Usage examples
4. How to extend it"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("✨ SmallTalk Metaprogramming Helper")
    st.markdown("Master runtime code generation and reflection in SmallTalk")
    
    # Initialize helper
    helper = SmallTalkMetaprogrammingHelper()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Metaprogramming Options")
        
        include_safety = st.toggle(
            "Include Safety Checks",
            value=True,
            help="Add error handling and validation"
        )
        
        include_tests = st.toggle(
            "Generate Tests",
            value=False,
            help="Create tests for metaprogramming code"
        )
        
        st.divider()
        
        # Quick references
        st.subheader("🔍 Quick Reference")
        
        if st.button("Compiler API"):
            st.session_state.show_api = "Compiler"
        if st.button("thisContext"):
            st.session_state.show_api = "thisContext"
        if st.button("Method Dictionary"):
            st.session_state.show_api = "MethodDictionary"
        if st.button("Behavior"):
            st.session_state.show_api = "Behavior"
        
        st.divider()
        st.caption("💡 **Warning**: Metaprogramming can break encapsulation")
    
    # Main content area
    tabs = st.tabs(["🎯 Task-Based", "📝 Custom Code", "🏗️ DSL Builder", "📚 Learning"])
    
    with tabs[0]:
        st.subheader("🎯 Common Metaprogramming Tasks")
        
        # Task selection
        task_cols = st.columns(2)
        selected_task = None
        
        for i, (task, description) in enumerate(helper.meta_tasks.items()):
            with task_cols[i % 2]:
                if st.button(
                    task,
                    key=f"meta_task_{i}",
                    help=description,
                    use_container_width=True
                ):
                    selected_task = task
                    st.session_state.selected_meta_task = task
        
        # Get selected task from session
        if "selected_meta_task" in st.session_state:
            selected_task = st.session_state.selected_meta_task
            
            if selected_task:
                st.info(f"**Selected Task**: {selected_task} - {helper.meta_tasks[selected_task]}")
                
                # Input based on task
                if selected_task in ["Add Logging", "Method Wrappers", "Performance Profiling"]:
                    code_input = st.text_area(
                        "Target Class Code (Optional)",
                        height=200,
                        placeholder="Paste the class to modify, or just provide class name below"
                    )
                    
                    target_class = st.text_input(
                        "Or just class name",
                        placeholder="e.g., MyDomainClass"
                    )
                else:
                    code_input = st.text_area(
                        "Base Code (Optional)",
                        height=200,
                        placeholder="Any relevant code context"
                    )
                    target_class = ""
                
                # Additional parameters based on task
                if selected_task == "Create Accessors":
                    st.markdown("**Variables to create accessors for:**")
                    variables = st.text_input(
                        "Instance variables",
                        placeholder="name age email (space-separated)"
                    )
                    if variables:
                        code_input = f"Instance variables: {variables}"
                
                elif selected_task == "DSL Creation":
                    domain = st.text_input(
                        "Domain for DSL",
                        placeholder="e.g., Testing, Configuration, Workflow"
                    )
                    if domain:
                        code_input = f"Domain: {domain}"
                
                # Generate button
                if st.button("✨ Generate Metaprogramming Code", type="primary"):
                    with st.spinner(f"Generating {selected_task} code..."):
                        result = helper.generate_metaprogramming(
                            code_input,
                            selected_task,
                            target_class
                        )
                        
                        if not result["error"]:
                            st.success(f"✅ {selected_task} code generated!")
                            
                            # Display result
                            st.subheader("Generated Metaprogramming Code")
                            st.code(result["code"], language="smalltalk")
                            
                            # Download button
                            st.download_button(
                                "📥 Download Code",
                                data=result["code"],
                                file_name=f"meta_{selected_task.lower().replace(' ', '_')}.st",
                                mime="text/plain"
                            )
                            
                            # Save option
                            if st.button("💾 Save to Library"):
                                st.session_state.save_meta = {
                                    "code": result["code"],
                                    "task": selected_task,
                                    "input": code_input
                                }
                        else:
                            st.error("Generation failed. Please try again.")
    
    with tabs[1]:
        st.subheader("📝 Custom Metaprogramming")
        
        custom_task = st.text_input(
            "Describe your metaprogramming task",
            placeholder="e.g., Add caching to all database query methods"
        )
        
        custom_code = st.text_area(
            "Code Context",
            height=300,
            placeholder="Paste relevant code or class definitions"
        )
        
        # Metaprogramming techniques to use
        st.markdown("**Techniques to use:**")
        technique_cols = st.columns(3)
        
        techniques = []
        with technique_cols[0]:
            if st.checkbox("Runtime Compilation"):
                techniques.append("Runtime compilation")
            if st.checkbox("Method Wrappers"):
                techniques.append("Method wrappers")
        
        with technique_cols[1]:
            if st.checkbox("Reflection"):
                techniques.append("Reflection")
            if st.checkbox("Dynamic Classes"):
                techniques.append("Dynamic classes")
        
        with technique_cols[2]:
            if st.checkbox("Proxies"):
                techniques.append("Proxy objects")
            if st.checkbox("Method Missing"):
                techniques.append("doesNotUnderstand:")
        
        if st.button("🔮 Generate Custom Solution", type="primary", 
                    disabled=not custom_task):
            enhanced_task = f"{custom_task}. Use these techniques: {', '.join(techniques)}" if techniques else custom_task
            
            with st.spinner("Creating custom metaprogramming solution..."):
                result = helper.generate_metaprogramming(
                    custom_code,
                    enhanced_task,
                    ""
                )
                
                if not result["error"]:
                    st.success("✅ Custom solution generated!")
                    st.code(result["code"], language="smalltalk")
    
    with tabs[2]:
        st.subheader("🏗️ DSL (Domain-Specific Language) Builder")
        
        dsl_domain = st.text_input(
            "DSL Domain",
            placeholder="e.g., Unit Testing, Configuration, State Machines, Business Rules"
        )
        
        # DSL characteristics
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("**DSL Style:**")
            dsl_style = st.radio(
                "Style",
                ["Fluent/Chain", "Block-based", "Message-based", "Declarative"],
                label_visibility="collapsed"
            )
        
        with col2:
            st.markdown("**Features:**")
            dsl_features = []
            if st.checkbox("Type checking"):
                dsl_features.append("type checking")
            if st.checkbox("Error handling"):
                dsl_features.append("error handling")
            if st.checkbox("Extensible"):
                dsl_features.append("extensibility")
        
        # Example DSL usage
        st.markdown("**Example usage (how you want it to look):**")
        example_usage = st.text_area(
            "DSL Usage Example",
            height=150,
            placeholder="""e.g., for a testing DSL:
TestCase new
    describe: 'Calculator'
    it: 'should add numbers' do: [
        self expect: (calc add: 2 to: 3) toBe: 5
    ]"""
        )
        
        if st.button("🏗️ Build DSL", type="primary", disabled=not dsl_domain):
            with st.spinner(f"Building {dsl_domain} DSL..."):
                enhanced_domain = f"{dsl_domain} with {dsl_style} style"
                if dsl_features:
                    enhanced_domain += f" including {', '.join(dsl_features)}"
                if example_usage:
                    enhanced_domain += f". Example usage: {example_usage}"
                
                dsl_code = helper.generate_dsl_example(enhanced_domain)
                
                st.success(f"✅ {dsl_domain} DSL generated!")
                st.code(dsl_code, language="smalltalk")
                
                # DSL documentation
                with st.expander("📚 DSL Usage Guide"):
                    st.markdown("""
                    ### Using Your DSL
                    
                    1. **File in the code** to your SmallTalk image
                    2. **Create instances** using the DSL syntax
                    3. **Extend** by adding new methods
                    4. **Document** the DSL for team members
                    
                    ### Best Practices
                    - Keep DSL syntax simple and intuitive
                    - Provide good error messages
                    - Document all DSL methods
                    - Version your DSL definitions
                    """)
    
    with tabs[3]:
        st.subheader("📚 Metaprogramming Learning Center")
        
        # Check if API explanation requested
        if "show_api" in st.session_state:
            api_name = st.session_state.show_api
            with st.expander(f"📖 {api_name} API Explanation", expanded=True):
                explanation = helper.explain_reflection_api(api_name)
                st.markdown(explanation)
            del st.session_state.show_api
        
        # Metaprogramming concepts
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("""
            ### Core APIs
            
            **Compiler**
            ```smalltalk
            Compiler evaluate: 'code string'
            MyClass compile: 'methodSource'
            ```
            
            **Reflection**
            ```smalltalk
            thisContext
            anObject class
            aClass methodDictionary
            aClass allInstances
            ```
            
            **Method Manipulation**
            ```smalltalk
            aClass>>addMethod: sourceCode
            aClass removeSelector: #methodName
            aMethod sourceCode
            ```
            """)
        
        with col2:
            st.markdown("""
            ### Techniques
            
            **Method Wrapper**
            ```smalltalk
            original := aClass>>methodName.
            aClass compile: 'methodName
                Transcript show: \'Before\'.
                result := self performWrapped: #methodName.
                Transcript show: \'After\'.
                ^result'
            ```
            
            **Dynamic Dispatch**
            ```smalltalk
            doesNotUnderstand: aMessage
                "Handle unknown messages"
                ^self handleDynamic: aMessage
            ```
            """)
        
        # Common patterns
        st.divider()
        st.subheader("🎯 Common Metaprogramming Patterns")
        
        pattern_tabs = st.tabs(["Logging", "Caching", "Proxies", "Builders"])
        
        with pattern_tabs[0]:
            st.code("""
"Add logging to all methods:"
aClass methodDictionary keysAndValuesDo: [:selector :method |
    | source |
    source := method sourceCode.
    aClass compile: selector, '
        Transcript show: \\'Entering ', selector, '\\'.
        ', source
]
            """, language="smalltalk")
        
        with pattern_tabs[1]:
            st.code("""
"Method caching:"
cache := Dictionary new.

aClass compile: 'cachedMethod: arg
    ^cache at: arg ifAbsentPut: [
        "expensive computation"
        self originalMethod: arg
    ]'
            """, language="smalltalk")
        
        with pattern_tabs[2]:
            st.code("""
"Proxy object:"
ProxyClass>>doesNotUnderstand: aMessage
    Transcript show: 'Intercepted: ', aMessage selector.
    ^target perform: aMessage selector 
        withArguments: aMessage arguments
            """, language="smalltalk")
        
        with pattern_tabs[3]:
            st.code("""
"Fluent builder pattern:"
builder := Builder new
    withName: 'Product';
    withPrice: 99.99;
    withCategory: 'Electronics';
    build
            """, language="smalltalk")
    
    # Save dialog (if triggered)
    if "save_meta" in st.session_state:
        with st.expander("💾 Save Metaprogramming Code", expanded=True):
            save_data = st.session_state.save_meta
            
            title = st.text_input(
                "Title",
                value=f"Metaprogramming: {save_data['task']}"
            )
            
            notes = st.text_area(
                "Implementation Notes",
                placeholder="Add notes about this metaprogramming solution..."
            )
            
            tags = st.text_input(
                "Tags",
                value=f"metaprogramming, {save_data['task'].lower().replace(' ', '-')}, smalltalk"
            )
            
            if st.button("💾 Confirm Save", type="primary"):
                if db.connected:
                    content = f"""## Metaprogramming Task: {save_data['task']}

## Generated Code
```smalltalk
{save_data['code']}
```

## Input Context
{save_data['input']}

## Notes
{notes}"""
                    
                    metadata = {
                        "task": save_data['task'],
                        "include_safety": include_safety,
                        "include_tests": include_tests
                    }
                    
                    query_id = helper.db.log_query(
                        tool="smalltalk_meta",
                        model=helper.default_model,
                        prompt=f"{save_data['task']}: {save_data['input']}",
                        response=save_data['code'],
                        metadata=metadata
                    )
                    
                    if query_id and title:
                        success = db.save_knowledge_unit(
                            query_id=query_id,
                            title=title,
                            content=content,
                            category="SmallTalk Metaprogramming",
                            tags=[tag.strip() for tag in tags.split(",")]
                        )
                        if success:
                            st.success("✅ Metaprogramming code saved!")
                            del st.session_state.save_meta
                            st.balloons()
                else:
                    st.warning("Database not connected")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
