import streamlit as st
from utils import DatabaseManager, safe_ollama_generate, get_contextual_help
import re

# Page configuration
st.set_page_config(
    page_title="TuoKit - Code Tools",
    page_icon="💻",
    layout="wide"
)

def extract_code(response: str) -> str:
    """Extract code blocks from model response"""
    code_blocks = re.findall(r'```[a-z]*\n(.*?)\n```', response, re.DOTALL)
    return "\n\n".join(code_blocks) if code_blocks else response

def explain_code(code: str, model: str) -> str:
    """Get code explanation from DeepSeek-Coder"""
    prompt = f"""
    Explain this code concisely in 3-5 bullet points. Focus on:
    1. Core functionality
    2. Key algorithms
    3. Potential edge cases
    
    Code:
    ```python
    {code}
    ```
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return response['response']
def debug_code(code: str, error: str, model: str) -> str:
    """Debug code based on error message"""
    prompt = f"""
    Fix this code that produces error: {error}
    Provide:
    1. Explanation of error
    2. Fixed code
    3. One-sentence summary of fix
    
    Code:
    ```python
    {code}
    ```
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return response['response']

def generate_code(description: str, model: str) -> str:
    """Generate code from natural language description"""
    prompt = f"""
    Create production-ready Python code based on this description:
    {description}
    
    Requirements:
    - Include type hints
    - Add error handling
    - Include brief docstring
    - Output ONLY code (no explanations)
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return extract_code(response['response'])
# Initialize session state for database
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

# Main content
st.title("💻 Code Tools")
st.caption("AI-powered code explanation, debugging, and generation")

# Tool selection
tool = st.radio("Select Tool:", ["Explain Code", "Debug Code", "Generate Code"], horizontal=True)

# Shared model selection
model = st.selectbox("AI Model", 
                    ["deepseek-coder:6.7b", "deepseek-r1:6.7b"],
                    index=0)

if tool == "Explain Code":
    st.subheader("Code Explanation")
    
    # Contextual help
    with st.expander("💡 Tips for this tool"):
        context = "security" if "#security" in st.session_state.get("code_input", "") else "default"
        help_text = get_contextual_help("code_explainer", context)
        st.markdown(help_text)
    
    code = st.text_area("Paste your code", height=250, 
                       placeholder="def calculate_tax(income):\n    return income * 0.2",
                       key="code_input")
    
    if st.button("Analyze Code", type="primary"):
        if not code.strip():
            st.warning("Please enter some code")
        else:            try:
                with st.spinner("Analyzing code structure..."):
                    explanation = explain_code(code, model)
                
                st.subheader("Explanation")
                st.markdown(explanation)
                
                # Log to database if available
                if st.session_state.db:
                    query_id = st.session_state.db.log_query(
                        tool="code_explainer",
                        model=model,
                        prompt=code,
                        response=explanation
                    )
                    st.session_state.last_query_id = query_id
                    st.success("✅ Analysis saved to knowledge base")
            except Exception as e:
                st.error(f"Error: {e}")

elif tool == "Debug Code":
    st.subheader("Code Debugging")
    col1, col2 = st.columns(2)
    with col1:
        code = st.text_area("Problematic Code", height=200,
                           placeholder="def divide(a, b):\n    return a / b")
    with col2:
        error = st.text_input("Error Message", 
                             placeholder="ZeroDivisionError: division by zero")    
    if st.button("Diagnose Issue", type="primary"):
        if not code.strip() or not error.strip():
            st.warning("Both code and error required")
        else:
            try:
                with st.spinner("Diagnosing problem..."):
                    debug_response = debug_code(code, error, model)
                
                st.subheader("Solution")
                st.markdown(debug_response)
                
                # Extract fixed code if available
                fixed_code = extract_code(debug_response)
                if fixed_code and fixed_code != debug_response:
                    st.subheader("Fixed Code")
                    st.code(fixed_code)
                
                # Log to database if available
                if st.session_state.db:
                    query_id = st.session_state.db.log_query(
                        tool="code_debugger",
                        model=model,
                        prompt=f"Code:\n{code}\n\nError:\n{error}",
                        response=debug_response
                    )
                    st.session_state.last_query_id = query_id
                    st.success("✅ Debug solution saved")
            except Exception as e:
                st.error(f"Error: {e}")
elif tool == "Generate Code":
    st.subheader("Code Generation")
    description = st.text_area("Describe what you need", height=150,
                             placeholder="Create a Python function to calculate Fibonacci sequence up to n numbers")
    lang = st.selectbox("Language", ["Python", "JavaScript", "SQL", "Bash"])
    
    if st.button("Generate Code", type="primary"):
        if not description.strip():
            st.warning("Please describe your requirements")
        else:
            try:
                with st.spinner("Generating solution..."):
                    full_prompt = f"{lang} code for: {description}"
                    generated = generate_code(full_prompt, model)
                
                st.subheader("Implementation")
                st.code(generated, language=lang.lower())
                
                # Copy button
                st.button("📋 Copy Code", key="copy_code", 
                         on_click=lambda: st.write("Code copied to clipboard!"))
                
                # Log to database if available
                if st.session_state.db:
                    query_id = st.session_state.db.log_query(
                        tool="code_generator",
                        model=model,
                        prompt=description,
                        response=generated
                    )
                    st.session_state.last_query_id = query_id                    st.success("✅ Code generated and saved")
            except Exception as e:
                st.error(f"Error: {e}")

# Knowledge saving component (reusable)
if 'last_query_id' in st.session_state and st.session_state.db:
    st.divider()
    with st.expander("💾 Save to Knowledge Base"):
        col1, col2 = st.columns(2)
        with col1:
            title = st.text_input("Title", value=f"{tool} - {model}")
        with col2:
            category = st.selectbox("Category", 
                                   ["Code Snippet", "Algorithm", "Error Solution", "Utility Function"])
        
        if st.button("Save to Library"):
            try:
                # Get last response
                last_query = st.session_state.db.get_query_by_id(st.session_state.last_query_id)
                if last_query:
                    content = last_query[3]  # ai_response field
                    
                    st.session_state.db.save_knowledge_unit(
                        query_id=st.session_state.last_query_id,
                        title=title,
                        content=content,
                        category=category
                    )
                    st.success(f"Saved to knowledge base as {category}!")
            except Exception as e:
                st.error(f"Error saving: {e}")

# Back to dashboard
st.divider()
col1, col2 = st.columns([1, 1])
with col1:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col2:
    if st.button("❓ Help", use_container_width=True):
        st.switch_page("pages/help_guide.py")