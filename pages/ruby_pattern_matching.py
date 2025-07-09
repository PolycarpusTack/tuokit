# pages/ruby_pattern_matching.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Ruby Pattern Matching - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate

def explain_pattern_matching(code):
    """Explain pattern matching with real-world examples"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Explain pattern matching in:\n```ruby\n{code}\n```",
        system=(
            "Provide 3 sections:\n"
            "1. Pattern Deconstruction: How values are matched\n"
            "2. Real-World Use Cases: Practical applications\n"
            "3. Alternative Approaches: Equivalent non-pattern matching code\n"
            "Use Ruby 3.1+ syntax with examples"
        )
    )['response']

def generate_pattern_example(description):
    """Generate pattern matching examples"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Create pattern matching example for: {description}",
        system=(
            "Output working Ruby code with:\n"
            "- Multiple case/in scenarios\n"
            "- Variable binding\n"
            "- Guard clauses\n"
            "- Nested patterns\n"
            "Include comments explaining each pattern"
        )
    )['response']

def show():
    st.title("🎯 Ruby Pattern Matching Explorer")
    st.caption("Master Ruby 3's pattern matching with practical examples")
    
    # Input options
    analysis_type = st.radio("Explore Pattern Matching", 
                           ["Analyze Existing Code", "Generate New Example"])
    
    if analysis_type == "Analyze Existing Code":
        code = st.text_area("Paste Ruby Code with Pattern Matching", 
                           height=250,
                           placeholder="case user\nin {name:, age: 18..}\n  # ...\nend")
        
        if st.button("Analyze Pattern", type="primary") and code:
            with st.spinner("Deconstructing patterns..."):
                explanation = explain_pattern_matching(code)
                st.subheader("Pattern Matching Breakdown")
                st.markdown(explanation)
                
                # Pattern matching concepts
                with st.expander("🧩 Pattern Matching Fundamentals", expanded=True):
                    st.markdown("""
                    **Key Features:**
                    - Value Deconstruction: `in [a, b, c]`
                    - Variable Binding: `in {name: n}`
                    - Guard Clauses: `in [x, y] if x > y`
                    - As Patterns: `in [x, y] => point`
                    - Alternative Patterns: `in 0 | 1 | 2`
                    
                    **Pattern Types:**
                    - **Array Patterns**: Match array structure and values
                    - **Hash Patterns**: Extract specific keys
                    - **Object Patterns**: Match by class and attributes
                    - **Find Patterns**: `in [*, x, *]` to find elements
                    """)
                    st.link_button("Ruby Pattern Matching Docs", 
                                 "https://docs.ruby-lang.org/en/3.0/syntax/pattern_matching_rdoc.html")
    
    else:
        description = st.text_input("Describe Use Case", 
                                   placeholder="e.g., Process API responses, handle different error types")
        complexity = st.select_slider("Complexity Level", ["Simple", "Intermediate", "Advanced"])
        
        # Pattern type selection
        pattern_types = st.multiselect("Include Pattern Types",
                                     ["Array Patterns", "Hash Patterns", "Object Patterns", 
                                      "Guard Clauses", "Alternative Patterns"],
                                     default=["Hash Patterns"])
        
        if st.button("Generate Example", type="primary") and description:
            with st.spinner("Creating pattern matching example..."):
                full_desc = f"{description} | Complexity: {complexity} | Types: {', '.join(pattern_types)}"
                example = generate_pattern_example(full_desc)
                st.subheader("Pattern Matching Implementation")
                st.code(example, language="ruby")
                
                # Pattern complexity analysis
                st.info(f"Generated {complexity.lower()} pattern matching example with {len(pattern_types)} pattern types")
                
                # Save to knowledge base
                if st.button("💾 Save Example"):
                    db = DatabaseManager()
                    if db.connected:
                        query_id = db.log_query(
                            tool="pattern_matching",
                            model=ModelManager.get_default_model(),
                            prompt=description,
                            response=example,
                            metadata={"tags": ["ruby", "pattern_matching"], "complexity": complexity}
                        )
                        if query_id:
                            st.success("Example saved to library!")
                    else:
                        st.error("Could not connect to database")
    
    # Common patterns reference
    with st.expander("📚 Common Pattern Matching Examples"):
        st.markdown("""
        **API Response Handling:**
        ```ruby
        case response
        in {status: 200, data: {users: [*users]}}
          process_users(users)
        in {status: 404}
          handle_not_found
        in {status: 400..499, error: message}
          handle_client_error(message)
        end
        ```
        
        **Data Validation:**
        ```ruby
        case user_data
        in {email: /\\A[\\w+\\-.]+@[a-z\\d\\-]+(\\.[a-z\\d\\-]+)*\\.[a-z]+\\z/i, age: 18..}
          create_user(user_data)
        else
          reject_invalid_data
        end
        ```
        """)

if __name__ == "__main__":
    show()
