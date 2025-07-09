# pages/regex_tool.py
import streamlit as st

# Initialize session state
import re
from utils.model_manager import ModelManager
from utils import DatabaseManager, safe_ollama_generate, get_available_models

# Page configuration
st.set_page_config(
    page_title="TuoKit - Advanced Regex Generator",
    page_icon="🔍",
    layout="wide"
)

def validate_regex(pattern, test_string, flags=0):
    """Test regex pattern against sample text with detailed results"""
    try:
        compiled = re.compile(pattern, flags)
        matches = list(compiled.finditer(test_string))
        return {
            "valid": True,
            "match_count": len(matches),
            "matches": [{"text": m.group(), "start": m.start(), "end": m.end()} for m in matches]
        }
    except re.error as e:
        return {
            "valid": False,
            "error": str(e)
        }

def extract_regex_from_response(response):
    """Extract regex pattern from AI response"""
    # Try to find pattern in code blocks
    if match := re.search(r"```(?:regex|python)?\n(.+?)\n```", response, re.DOTALL):
        return match.group(1).strip()
    
    # Try to find pattern between backticks
    if match := re.search(r"`([^`]+)`", response):
        return match.group(1).strip()
    
    # Return cleaned response
    return response.strip()

def generate_regex_pattern(description, model):
    """Generate regex using Ollama with strict output formatting"""
    prompt = f"""Generate a regex pattern for: "{description}"

CRITICAL INSTRUCTIONS:
1. Output ONLY the regex pattern
2. Use Python regex syntax
3. Do NOT include any explanation
4. Put the pattern in a code block with ```

Example format:
```
^[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z]+$
```"""
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt
    )
    
    if 'error' in response:
        return None, response['response']
    
    # Extract regex from response
    pattern = extract_regex_from_response(response['response'])
    return pattern, None

# Initialize session state
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

# Model selection will be in sidebar

# Tutorial function
def show_tutorial():
    """Display interactive tutorial"""
    with st.expander("🧙‍♂️ Quick Tutorial", expanded=False):
        st.markdown("""
        ### How to use the Regex Generator:
        
        1. **Describe your pattern** in natural language
           - Example: "Email addresses"
           - Example: "Phone numbers with area codes"
           
        2. **Test your pattern** with sample text
           - See live highlighting of matches
           - Get match counts and positions
           
        3. **Use regex flags** for advanced matching:
           - 🅰️ Ignore Case: Case-insensitive matching
           - 🔠 Multiline: ^/$ match line boundaries
           - 🔘 Dotall: . matches newlines
           
        4. **Export to any language**:
           - Python, JavaScript, Java, Golang, C#
           - Copy-ready code snippets
           
        5. **Save patterns** for future use
           - Build your personal regex library
           - Access from sidebar history
        """)

# Main UI
st.title("🔍 Advanced Regex Generator")
st.caption("Create, test, and export regex patterns using natural language")

# Sidebar
with st.sidebar:
    st.subheader("📚 Pattern Library")
    if st.session_state.db:
        recent = st.session_state.db.get_recent_queries(limit=10)
        regex_queries = [q for q in recent if q[1] == "regex_generator"]  # Filter by tool
        
        if regex_queries:
            for query in regex_queries[:5]:
                with st.expander(f"🔖 {query[3][:30]}..."):  # user_prompt
                    st.code(query[4], language="regex")  # ai_response
                    if st.button("Load", key=f"load_{query[0]}"):
                        st.session_state.loaded_pattern = query[4]
                        st.session_state.loaded_description = query[3]
                        st.rerun()
        else:
            st.info("No saved patterns yet")
    
    st.divider()
    
    # Model selection
    st.subheader("⚙️ Settings")
    # Model selection - dynamically load from Ollama
    available_models = get_available_models()
    model = st.selectbox("AI Model", 
                        available_models,
                        index=0,
                        key="regex_model",
                        help="Models currently available in Ollama")
    
    # Show tutorial button
    if st.button("📖 Show Tutorial"):
        st.session_state.show_tutorial = True

# Show tutorial if requested
if st.session_state.get('show_tutorial', False):
    show_tutorial()
    st.session_state.show_tutorial = False

# Main content columns
col1, col2 = st.columns([3, 1])

with col1:
    st.subheader("Pattern Generator")

with col2:
    st.caption("💡 **Common patterns:**")
    st.caption("• Email validation\n• Phone numbers\n• URLs/domains\n• Dates & times")

# Input form
with st.form("regex_form"):
    description = st.text_area("Describe what you need to match:", 
                             placeholder="Email addresses\nPhone numbers in format (123) 456-7890\nURLs starting with https",
                             height=100,
                             value=st.session_state.get('loaded_description', ''))
    
    col1, col2 = st.columns(2)
    with col1:
        test_input = st.text_input("Test string:", 
                                  placeholder="test@example.com, contact@company.org")
    
    with col2:
        # Flags selector
        flags = st.multiselect(
            "Regex flags:",
            options=[
                ("Ignore Case", re.IGNORECASE), 
                ("Multiline", re.MULTILINE),
                ("Dotall", re.DOTALL)
            ],
            format_func=lambda x: x[0]
        )
        selected_flags = sum(flag[1] for flag in flags)
    
    submitted = st.form_submit_button("✨ Generate Regex", type="primary", use_container_width=True)

# Handle form submission
if submitted or st.session_state.get('loaded_pattern'):
    pattern = st.session_state.get('loaded_pattern')
    
    if submitted and description.strip():
        with st.spinner("🤖 Generating regex pattern..."):
            pattern, error = generate_regex_pattern(description.strip(), model)
        
        if error:
            st.error(f"Error generating pattern: {error}")
        elif pattern:
            # Log to database
            if st.session_state.db:
                try:
                    query_id = st.session_state.db.log_query(
                        tool="regex_generator",
                        model=model,
                        prompt=description,
                        response=pattern
                    )
                    st.session_state.last_query_id = query_id
                except Exception as e:
                    st.error(f"Error logging: {e}")
    
    if pattern:
        st.success("✅ Regex pattern generated!")
        
        # Display pattern with copy button
        col1, col2 = st.columns([5, 1])
        with col1:
            st.code(pattern, language="regex")
        
        # Test validation
        if test_input:
            st.subheader("🔬 Test Results")
            result = validate_regex(pattern, test_input, selected_flags)
            
            if result["valid"]:
                st.success(f"✅ Found {result['match_count']} matches")
                
                # Visual highlighting
                if result["matches"]:
                    colored_text = test_input
                    for match in reversed(result["matches"]):
                        colored_text = (
                            colored_text[:match["start"]] +
                            f"<mark style='background: #a8e6cf;'>{colored_text[match['start']:match['end']]}</mark>" +
                            colored_text[match["end"]:]
                        )
                    st.markdown("**Highlighted matches:**")
                    st.markdown(colored_text, unsafe_allow_html=True)
                    
                    # Show match details
                    with st.expander("Match Details"):
                        for i, match in enumerate(result["matches"]):
                            st.write(f"Match {i+1}: `{match['text']}` (position {match['start']}-{match['end']})")
            else:
                st.error(f"❌ Invalid pattern: {result['error']}")
        
        # Pattern explanation
        with st.expander("🔍 Pattern Explanation"):
            with st.spinner("Generating explanation..."):
                explain_prompt = f"""Explain this regex pattern component by component:
Pattern: {pattern}

Break down each part and explain what it matches. Use bullet points."""
                
                explanation = safe_ollama_generate(
                    model=model,
                    prompt=explain_prompt
                )
                
                if 'error' not in explanation:
                    st.markdown(explanation['response'])
        
        # Language export
        st.subheader("🚀 Export Pattern")
        export_format = st.selectbox("Select language:", 
                                    ["Python", "JavaScript", "Java", "Golang", "C#"])
        
        lang_map = {
            "Python": f"import re\npattern = re.compile(r'{pattern}')",
            "JavaScript": f"const pattern = /{pattern}/;",
            "Java": f"import java.util.regex.*;\nPattern pattern = Pattern.compile(\"{pattern}\");",
            "Golang": f"import \"regexp\"\npattern := regexp.MustCompile(`{pattern}`)",
            "C#": f"using System.Text.RegularExpressions;\nvar pattern = new Regex(@\"{pattern}\");"
        }
        st.code(lang_map[export_format], language=export_format.lower())
        
        # Save to knowledge base
        if st.session_state.db and st.session_state.get('last_query_id'):
            col1, col2 = st.columns(2)
            with col1:
                title = st.text_input("Save with title:", value=f"Regex: {description[:30]}" if description else "Regex Pattern")
            with col2:
                if st.button("💾 Save to Knowledge Base", key="save_regex"):
                    try:
                        saved = st.session_state.db.save_knowledge_unit(
                            query_id=st.session_state.last_query_id,
                            title=title,
                            content=f"Pattern: {pattern}\nDescription: {description}\nTest: {test_input or 'N/A'}",
                            category="Regex Pattern"
                        )
                        if saved:
                            st.success("✅ Saved to knowledge base!")
                    except Exception as e:
                        st.error(f"Error saving: {e}")
    
    # Clear loaded pattern
    if 'loaded_pattern' in st.session_state:
        del st.session_state.loaded_pattern
        del st.session_state.loaded_description

# Regex reference section
st.divider()
with st.expander("📚 Regex Quick Reference"):
    col1, col2 = st.columns(2)
    with col1:
        st.markdown("""
        **Common Patterns:**
        - `.` - Any character
        - `*` - Zero or more
        - `+` - One or more  
        - `?` - Zero or one
        - `^` - Start of string
        - `$` - End of string
        - `[]` - Character class
        - `()` - Capture group
        """)
    with col2:
        st.markdown("""
        **Character Classes:**
        - `\\d` - Digit [0-9]
        - `\\w` - Word character [a-zA-Z0-9_]
        - `\\s` - Whitespace
        - `\\b` - Word boundary
        - `[A-Z]` - Uppercase letters
        - `[a-z]` - Lowercase letters
        - `[0-9]` - Digits
        """)

# Navigation
st.divider()
col1, col2, col3 = st.columns(3)
with col1:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col2:
    if st.button("📚 Knowledge Library", use_container_width=True):
        st.switch_page("pages/knowledge_lib.py")
with col3:
    if st.button("❓ Help", use_container_width=True):
        st.switch_page("pages/help_guide.py")