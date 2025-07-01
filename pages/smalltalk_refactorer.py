"""
SmallTalk Refactoring Assistant for TuoKit
Helps refactor SmallTalk code using various techniques
"""

import streamlit as st
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SmallTalkRefactorer(OllamaToolBase):
    """SmallTalk code refactoring tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_refactorer",
            default_model="deepseek-coder:6.7b"
        )
        
        self.refactoring_techniques = {
            "Extract Method": {
                "description": "Extract code into a new method",
                "example": "Turn repeated code into a reusable method"
            },
            "Rename Variable": {
                "description": "Give variables more meaningful names",
                "example": "Change 'x' to 'customerAge'"
            },
            "Introduce Parameter": {
                "description": "Replace hardcoded values with parameters",
                "example": "Make methods more flexible"
            },
            "Replace Conditional with Polymorphism": {
                "description": "Use object polymorphism instead of if/case",
                "example": "Replace type checks with method dispatch"
            },
            "Simplify Expressions": {
                "description": "Make complex expressions clearer",
                "example": "Break down nested conditions"
            },
            "Extract Class": {
                "description": "Move related methods to a new class",
                "example": "Separate concerns into cohesive classes"
            },
            "Inline Method": {
                "description": "Replace method call with method body",
                "example": "Remove unnecessary indirection"
            },
            "Move Method": {
                "description": "Move method to more appropriate class",
                "example": "Put behavior where data lives"
            },
            "Replace Temp with Query": {
                "description": "Replace temporary variable with method",
                "example": "Make calculations explicit"
            },
            "Introduce Null Object": {
                "description": "Replace nil checks with null object",
                "example": "Eliminate conditional nil handling"
            }
        }
    
    def refactor_code(self, code: str, technique: str, 
                     preserve_behavior: bool = True) -> dict:
        """Apply refactoring technique to code"""
        
        technique_info = self.refactoring_techniques.get(technique, {})
        
        prompt = f"""Apply the '{technique}' refactoring technique to this SmallTalk code:

```smalltalk
{code}
```

Refactoring: {technique}
Description: {technique_info.get('description', '')}

Requirements:
1. Apply the refactoring correctly
2. Preserve original behavior
3. Add comments explaining what changed
4. Show the specific improvements made
5. Follow SmallTalk best practices

Provide:
- The refactored code
- Clear comments on changes
- Brief explanation of benefits"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are a SmallTalk refactoring expert. Apply refactorings that improve code quality while preserving behavior."
        )
        
        return {
            "refactored": result["response"],
            "error": result["error"]
        }
    
    def analyze_code_smells(self, code: str) -> str:
        """Identify code smells and improvement opportunities"""
        prompt = f"""Analyze this SmallTalk code for code smells and refactoring opportunities:

```smalltalk
{code}
```

Identify:
1. Code smells present
2. Suggested refactorings
3. Priority of improvements
4. Specific examples from the code"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]
    
    def suggest_refactoring_plan(self, code: str) -> str:
        """Create a refactoring plan for the code"""
        prompt = f"""Create a step-by-step refactoring plan for this SmallTalk code:

```smalltalk
{code}
```

Provide:
1. Ordered list of refactorings to apply
2. Why each refactoring is beneficial
3. Dependencies between refactorings
4. Expected outcome"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🔧 SmallTalk Refactoring Assistant")
    st.markdown("Improve your SmallTalk code structure with automated refactoring")
    
    # Initialize refactorer
    refactorer = SmallTalkRefactorer()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Refactoring Options")
        
        preserve_behavior = st.toggle(
            "Preserve Behavior",
            value=True,
            help="Ensure refactoring doesn't change functionality"
        )
        
        show_diff = st.toggle(
            "Show Differences",
            value=True,
            help="Highlight what changed"
        )
        
        st.divider()
        
        # Analysis options
        st.subheader("📊 Analysis")
        
        analyze_smells = st.checkbox(
            "Detect Code Smells",
            value=True,
            help="Identify improvement opportunities"
        )
        
        suggest_plan = st.checkbox(
            "Suggest Refactoring Plan",
            value=False,
            help="Get ordered refactoring steps"
        )
        
        st.divider()
        st.caption("💡 **Tip**: Start with simple refactorings")
    
    # Main content area
    code = st.text_area(
        "SmallTalk Code to Refactor",
        height=300,
        placeholder="""Example:
MyClass>>processData: aCollection
    | result temp |
    result := OrderedCollection new.
    aCollection do: [:each |
        temp := each * 2.
        temp > 10 ifTrue: [
            result add: temp.
        ].
    ].
    ^result""",
        help="Paste the SmallTalk code you want to refactor"
    )
    
    # Refactoring technique selection
    st.markdown("### 🛠️ Select Refactoring Technique")
    
    # Display techniques in a grid
    technique_cols = st.columns(2)
    selected_technique = None
    
    for i, (technique, info) in enumerate(refactorer.refactoring_techniques.items()):
        with technique_cols[i % 2]:
            if st.button(
                f"{technique}",
                key=f"technique_{i}",
                help=info["description"],
                use_container_width=True
            ):
                selected_technique = technique
                st.session_state.selected_technique = technique
    
    # Get selected technique from session state
    if "selected_technique" in st.session_state:
        selected_technique = st.session_state.selected_technique
        
        # Show selected technique info
        if selected_technique:
            info = refactorer.refactoring_techniques[selected_technique]
            st.info(f"""
            **Selected: {selected_technique}**
            
            {info["description"]}
            
            *Example: {info["example"]}*
            """)
    
    # Refactor button
    if st.button("🔧 Apply Refactoring", type="primary", 
                 disabled=not code.strip() or not selected_technique):
        
        # Analysis first (if enabled)
        if analyze_smells:
            with st.expander("🔍 Code Analysis", expanded=True):
                with st.spinner("Analyzing code smells..."):
                    analysis = refactorer.analyze_code_smells(code)
                    st.markdown(analysis)
        
        # Refactoring plan (if enabled)
        if suggest_plan:
            with st.expander("📋 Refactoring Plan", expanded=True):
                with st.spinner("Creating refactoring plan..."):
                    plan = refactorer.suggest_refactoring_plan(code)
                    st.markdown(plan)
        
        # Apply refactoring
        with st.spinner(f"Applying {selected_technique} refactoring..."):
            result = refactorer.refactor_code(
                code,
                selected_technique,
                preserve_behavior=preserve_behavior
            )
            
            if not result["error"]:
                st.success(f"✅ {selected_technique} applied successfully!")
                
                # Display results in tabs
                tabs = st.tabs([
                    "✨ Refactored Code",
                    "📊 Before/After",
                    "📚 Refactoring Guide",
                    "💾 Save"
                ])
                
                with tabs[0]:
                    st.code(result["refactored"], language="smalltalk")
                    
                    # Download button
                    st.download_button(
                        "📥 Download Refactored Code",
                        data=result["refactored"],
                        file_name="refactored_code.st",
                        mime="text/plain"
                    )
                    
                    # Metrics
                    col1, col2 = st.columns(2)
                    with col1:
                        original_lines = len(code.splitlines())
                        refactored_lines = len(result["refactored"].splitlines())
                        st.metric(
                            "Line Count",
                            refactored_lines,
                            delta=refactored_lines - original_lines
                        )
                    with col2:
                        st.metric(
                            "Refactoring",
                            selected_technique
                        )
                
                with tabs[1]:
                    st.subheader("📊 Before/After Comparison")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("**Original Code**")
                        st.code(code, language="smalltalk")
                    
                    with col2:
                        st.markdown("**Refactored Code**")
                        st.code(result["refactored"], language="smalltalk")
                    
                    if show_diff:
                        st.info("💡 Review the changes carefully to ensure behavior is preserved")
                
                with tabs[2]:
                    st.subheader("📚 Refactoring Best Practices")
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("""
                        ### When to Refactor
                        
                        **Code Smells**
                        - Long methods
                        - Large classes
                        - Duplicate code
                        - Complex conditionals
                        - Feature envy
                        
                        **Timing**
                        - Before adding features
                        - After getting tests passing
                        - During code reviews
                        - When fixing bugs
                        """)
                    
                    with col2:
                        st.markdown("""
                        ### Refactoring Process
                        
                        **Steps**
                        1. Identify smell
                        2. Write/verify tests
                        3. Apply refactoring
                        4. Run tests
                        5. Commit changes
                        
                        **Safety**
                        - Small steps
                        - Test constantly
                        - Use version control
                        - Preserve behavior
                        """)
                    
                    # Common refactorings reference
                    st.divider()
                    st.subheader("🔧 Common SmallTalk Refactorings")
                    
                    refactoring_examples = {
                        "Extract Method": """
"Before:"
processOrder
    "validate"
    order items isEmpty ifTrue: [^self].
    order total < 0 ifTrue: [^self].
    "process"
    ...

"After:"
processOrder
    self validateOrder ifFalse: [^self].
    "process"
    ...

validateOrder
    order items isEmpty ifTrue: [^false].
    order total < 0 ifTrue: [^false].
    ^true
                        """,
                        "Replace Conditional": """
"Before:"
displayShape: aShape
    aShape type = #circle ifTrue: [^self displayCircle: aShape].
    aShape type = #square ifTrue: [^self displaySquare: aShape].

"After:"
displayShape: aShape
    ^aShape displayOn: self
                        """,
                        "Introduce Parameter": """
"Before:"
calculateTax
    ^amount * 0.15

"After:"
calculateTaxWithRate: rate
    ^amount * rate
                        """
                    }
                    
                    example_tabs = st.tabs(list(refactoring_examples.keys()))
                    for i, (name, example) in enumerate(refactoring_examples.items()):
                        with example_tabs[i]:
                            st.code(example, language="smalltalk")
                
                with tabs[3]:
                    st.subheader("💾 Save Refactoring")
                    
                    title = st.text_input(
                        "Title",
                        value=f"{selected_technique}: {code[:30]}..."
                    )
                    
                    description = st.text_area(
                        "Description",
                        placeholder="Describe what this refactoring accomplishes..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"refactoring, {selected_technique.lower().replace(' ', '-')}, smalltalk"
                    )
                    
                    save_original = st.checkbox("Save original code too", value=True)
                    
                    if st.button("💾 Save Refactoring", type="primary"):
                        if db.connected:
                            # Prepare content
                            content = f"""## Refactoring: {selected_technique}

## Description
{description}

## Refactored Code
```smalltalk
{result['refactored']}
```"""
                            
                            if save_original:
                                content = f"""## Original Code
```smalltalk
{code}
```

""" + content
                            
                            if analyze_smells and 'analysis' in locals():
                                content += f"\n\n## Code Analysis\n{analysis}"
                            
                            metadata = {
                                "technique": selected_technique,
                                "preserve_behavior": preserve_behavior,
                                "original_lines": len(code.splitlines()),
                                "refactored_lines": len(result["refactored"].splitlines())
                            }
                            
                            query_id = refactorer.db.log_query(
                                tool="smalltalk_refactorer",
                                model=refactorer.default_model,
                                prompt=f"{selected_technique}: {code}",
                                response=result["refactored"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=content,
                                    category="SmallTalk Refactoring",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Refactoring saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Refactoring failed. Please check your code and try again.")
    
    # Quick examples
    with st.expander("📖 Example Code for Practice"):
        st.markdown("Try refactoring these examples:")
        
        example_col1, example_col2 = st.columns(2)
        
        with example_col1:
            st.markdown("**Long Method Example**")
            if st.button("Load Example 1"):
                code = """Customer>>calculateBill
    | total discount tax finalAmount |
    total := 0.
    items do: [:item | total := total + item price].
    
    "Apply discount"
    membershipLevel = #gold ifTrue: [discount := total * 0.2].
    membershipLevel = #silver ifTrue: [discount := total * 0.1].
    membershipLevel = #bronze ifTrue: [discount := total * 0.05].
    discount isNil ifTrue: [discount := 0].
    
    "Calculate tax"
    tax := (total - discount) * 0.15.
    
    "Final amount"
    finalAmount := total - discount + tax.
    ^finalAmount"""
        
        with example_col2:
            st.markdown("**Code Duplication Example**")
            if st.button("Load Example 2"):
                code = """ReportGenerator>>generateDailyReport
    | data |
    data := OrderedCollection new.
    orders do: [:order |
        order date = Date today ifTrue: [
            data add: order
        ]
    ].
    ^self formatReport: data

ReportGenerator>>generateWeeklyReport
    | data |
    data := OrderedCollection new.
    orders do: [:order |
        (order date between: Date today - 7 and: Date today) ifTrue: [
            data add: order
        ]
    ].
    ^self formatReport: data"""

# Entry point for Streamlit
if __name__ == "__main__":
    show()
