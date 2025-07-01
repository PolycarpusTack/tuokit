# pages/error_tool.py
import streamlit as st
import re
import json
from utils import DatabaseManager, safe_ollama_generate

# Page configuration
st.set_page_config(
    page_title="TuoKit - Advanced Error Decoder",
    page_icon="🐞",
    layout="wide"
)

# Predefined knowledge base for common errors
EDUCATIONAL_CONTENT = {
    # SmallTalk Errors
    "MessageNotUnderstood": {
        "title": "Message Not Understood Error",
        "categories": ["SmallTalk", "Object-Oriented"],
        "explanation": "Occurs when an object receives a message it doesn't implement",
        "analogy": "Like asking a fish to climb a tree - the receiver lacks the capability",
        "causes": [
            "Method not implemented in receiver's class",
            "Typo in method name",
            "Incorrect receiver type"
        ],
        "fixes": [
            "Implement the missing method",
            "Check spelling of message",
            "Verify receiver class hierarchy"
        ],
        "best_practices": [
            "Use #respondsTo: before sending messages",
            "Implement #doesNotUnderstand: for custom handling",
            "Use protocols to organize methods"
        ],
        "case_study": "In the SmallTalk image, all objects inherit from Object which implements #doesNotUnderstand:. This default implementation signals a MessageNotUnderstood exception.",
        "resources": [
            {"title": "SmallTalk Message Handling", "url": "https://wiki.squeak.org/squeak/194"},
            {"title": "Object Protocol Design", "url": "https://wiki.squeak.org/squeak/195"}
        ]
    },
    "SubscriptOutOfBounds": {
        "title": "Subscript Out of Bounds",
        "categories": ["SmallTalk", "Collections"],
        "explanation": "Occurs when accessing an index beyond collection size",
        "analogy": "Like trying to read page 100 in a 50-page book",
        "causes": [
            "Off-by-one errors",
            "Invalid index calculation",
            "Empty collection access"
        ],
        "fixes": [
            "Check collection size before access",
            "Use #ifEmpty: block",
            "Validate index ranges"
        ],
        "best_practices": [
            "Use #at:ifAbsent: instead of #at:",
            "Prefer iterators over index access",
            "Use #first, #last instead of indexes"
        ],
        "case_study": "The SmallTalk collection hierarchy implements #errorSubscriptBounds: to handle this error consistently across all collection types.",
        "resources": [
            {"title": "Collection Protocols", "url": "https://wiki.squeak.org/squeak/220"},
            {"title": "Error Handling Patterns", "url": "https://wiki.squeak.org/squeak/223"}
        ]
    },
    
    # Ruby/Rails Errors
    "NoMethodError": {
        "title": "No Method Error",
        "categories": ["Ruby", "Rails"],
        "explanation": "Occurs when calling undefined method on object",
        "analogy": "Like trying to drive a car with a steering wheel that doesn't exist",
        "causes": [
            "Method name typo",
            "Missing require/include",
            "Wrong object type"
        ],
        "fixes": [
            "Check method spelling",
            "Verify required files are loaded",
            "Use respond_to? before calling"
        ],
        "best_practices": [
            "Use method_missing for dynamic handling",
            "Implement null object pattern",
            "Use safe navigation operator (&.)"
        ],
        "case_study": "In Rails, this often occurs in views when referencing undefined helper methods. The solution is to define the method in the appropriate helper module.",
        "resources": [
            {"title": "Ruby Method Lookup", "url": "https://ruby-doc.org/core-3.1.2/doc/method_lookup_rdoc.html"},
            {"title": "Rails Helper Patterns", "url": "https://guides.rubyonrails.org/action_view_overview.html"}
        ]
    },
    "ActiveRecord::RecordNotFound": {
        "title": "Record Not Found",
        "categories": ["Rails", "Database"],
        "explanation": "Occurs when database query returns no results",
        "analogy": "Like looking for a book in an empty library",
        "causes": [
            "Invalid ID parameter",
            "Deleted record",
            "Scoped query with no matches"
        ],
        "fixes": [
            "Validate params before query",
            "Use find_by instead of find",
            "Implement rescue_from handler"
        ],
        "best_practices": [
            "Use find_or_initialize_by for safe access",
            "Implement null object pattern",
            "Use policy objects for authorization"
        ],
        "case_study": "Rails controllers rescue this error by default, returning a 404 response. This can be customized in ApplicationController.",
        "resources": [
            {"title": "ActiveRecord Querying", "url": "https://guides.rubyonrails.org/active_record_querying.html"},
            {"title": "Error Handling in Controllers", "url": "https://guides.rubyonrails.org/action_controller_overview.html#rescue-from"}
        ]
    }
}

def parse_error_message(error):
    """Enhanced error parsing with SmallTalk and Ruby support"""
    # Language-specific patterns
    patterns = {
        "python": r"File \"(.+?)\", line (\d+).*\n(\w+Error): (.+)",
        "javascript": r"at (.+?) \((.+?):(\d+):\d+\)\n(\w+Error): (.+)",
        "java": r"at (.+?)\((.+?):(\d+)\)\n(\w+Exception): (.+)",
        "c++": r"\((.+?):(\d+)\): error (.+?): (.+)",
        "ruby": r"(.+?):(\d+):in `(.+)': (.+) \((.+)\)",
        "rails": r"Completed (\d{3}) .* in (\d+ms).*\n\n(.+?) \(([\w:]+)\):\n\n(.+?):(\d+):in `(.+)'",
        "smalltalk": r"\[(.*?)\]: (.*?) (Error|Exception): (.*?)\nReceiver: (.*?)\nArguments: (.*?)\n(.*)",
        "generic": r"(\w+Error|\w+Exception): (.+)"
    }
    
    result = {
        "language": "unknown",
        "error_type": "Unknown",
        "message": "",
        "file": "",
        "line": 0,
        "context": ""
    }
    
    # Try language-specific patterns first
    for lang, pattern in patterns.items():
        match = re.search(pattern, error, re.DOTALL)
        if match:
            result["language"] = lang
            try:
                if lang == "python":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["error_type"] = match.group(3)
                    result["message"] = match.group(4)
                elif lang == "javascript":
                    result["context"] = match.group(1)
                    result["file"] = match.group(2)
                    result["line"] = int(match.group(3))
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(5)
                elif lang == "java":
                    result["context"] = match.group(1)
                    result["file"] = match.group(2)
                    result["line"] = int(match.group(3))
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(5)
                elif lang == "c++":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["error_type"] = match.group(3)
                    result["message"] = match.group(4)
                elif lang == "ruby":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["context"] = match.group(3)
                    result["error_type"] = match.group(5)
                    result["message"] = match.group(4)
                elif lang == "rails":
                    result["status"] = match.group(1)
                    result["time"] = match.group(2)
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(3)
                    result["file"] = match.group(5)
                    result["line"] = int(match.group(6))
                    result["context"] = match.group(7)
                elif lang == "smalltalk":
                    result["process"] = match.group(1)
                    result["timestamp"] = match.group(2)
                    result["error_type"] = match.group(4)
                    result["receiver"] = match.group(5)
                    result["arguments"] = match.group(6)
                    result["stack"] = match.group(7)
                    result["message"] = f"{match.group(4)}: {match.group(4)}"
                else:  # generic
                    result["error_type"] = match.group(1)
                    result["message"] = match.group(2)
            except (IndexError, ValueError):
                # Fallback to generic if pattern doesn't match fully
                result["error_type"] = "ParseError"
                result["message"] = error
            break
    
    return result

def analyze_error(error_data, user_code="", model="deepseek-coder:6.7b"):
    """Comprehensive error analysis with language-specific guidance"""
    # System prompt for detailed analysis
    system_prompt = (
        "You are a senior software engineer. Analyze errors with:\n"
        "1. Plain English explanation\n"
        "2. Root cause analysis\n"
        "3. Step-by-step fix instructions\n"
        "4. Prevention strategies\n"
        "5. Code solution (if applicable)\n"
        "Format response in markdown with clear sections.\n"
        "Provide language-specific best practices."
    )
    
    # Language-specific guidance additions
    lang_guidance = {
        "ruby": "Consider Ruby idioms and Rails conventions",
        "rails": "Focus on MVC structure, database interactions, and Rails conventions",
        "smalltalk": "Consider Smalltalk's object-oriented nature and image-based environment",
    }
    
    # Build context-aware prompt
    prompt = f"Language: {error_data['language'].capitalize()}\n"
    if error_data['language'] in lang_guidance:
        prompt += f"Note: {lang_guidance[error_data['language']]}\n"
    prompt += f"Error Type: {error_data['error_type']}\n"
    prompt += f"Message: {error_data['message']}\n"
    
    # Add language-specific context
    if error_data['language'] == "smalltalk":
        prompt += f"Receiver: {error_data.get('receiver', '')}\n"
        prompt += f"Arguments: {error_data.get('arguments', '')}\n"
        prompt += f"Process: {error_data.get('process', '')}\n"
    
    if error_data.get('file'):
        prompt += f"File: {error_data['file']}\n"
    if error_data.get('line') > 0:
        prompt += f"Line: {error_data['line']}\n"
    if user_code:
        prompt += f"\nCode Context:\n```{error_data['language']}\n{user_code}\n```"
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system=system_prompt
    )
    
    if 'error' in response:
        return f"Error generating analysis: {response['response']}"
    
    return response.get('response', 'Unable to generate analysis')

def generate_fix_patch(error_data, user_code, model="deepseek-coder:6.7b"):
    """Generate code patch to fix the error"""
    prompt = (
        f"Fix this {error_data['language']} error in the code below:\n"
        f"Error: {error_data['error_type']}: {error_data['message']}\n\n"
        f"Code:\n```{error_data['language']}\n{user_code}\n```\n\n"
        "Output ONLY the fixed code in a markdown code block."
    )
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="Output ONLY fixed code with minimal changes"
    )
    
    if 'error' in response:
        return None
    
    # Extract code from markdown block
    if match := re.search(r"```(?:[a-z]+)?\n(.*?)\n```", response['response'], re.DOTALL):
        return match.group(1)
    return response.get('response', '')

def get_educational_content(error_type, language, model="deepseek-coder:6.7b"):
    """Get structured educational content for error"""
    # Check predefined content
    if error_type in EDUCATIONAL_CONTENT:
        return EDUCATIONAL_CONTENT[error_type]
    
    # Generate with AI if not found
    prompt = (
        f"Create educational content for {error_type} error in {language} with: "
        "1. Detailed explanation\n"
        "2. Real-world analogy\n"
        "3. Common causes\n"
        "4. Fix strategies\n"
        "5. Best practices\n"
        "6. Case study example\n"
        "7. Learning resources\n"
        "Format as JSON with those keys"
    )
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="Output valid JSON only"
    )
    
    if 'error' not in response:
        try:
            return json.loads(response['response'])
        except:
            pass
    
    return {
        "title": error_type,
        "explanation": f"Error occurs in {language} applications",
        "resources": []
    }

def show_educational_layer(error_type, language, model="deepseek-coder:6.7b"):
    """Interactive educational experience"""
    content = get_educational_content(error_type, language, model)
    
    with st.expander("🎓 Educational Insights", expanded=True):
        st.subheader(f"Deep Dive: {content['title']}")
        
        # Explanation section
        st.markdown(f"### 📖 Explanation")
        st.info(content['explanation'])
        st.caption(f"**Analogy:** {content.get('analogy', '')}")
        
        # Causes & Fixes
        col1, col2 = st.columns(2)
        with col1:
            st.markdown("### 🚨 Common Causes")
            for cause in content.get('causes', []):
                st.markdown(f"- {cause}")
        with col2:
            st.markdown("### 🔧 Fix Strategies")
            for fix in content.get('fixes', []):
                st.markdown(f"- {fix}")
        
        # Best Practices
        st.markdown("### 🏆 Best Practices")
        for practice in content.get('best_practices', []):
            st.markdown(f"- `{practice}`")
        
        # Interactive case study
        st.markdown("### 🧪 Case Study")
        st.markdown(content.get('case_study', ''))
        
        if language == "smalltalk":
            st.code("""
            "Example of handling MessageNotUnderstood"
            MyClass >> doesNotUnderstand: aMessage
                aMessage selector = #someMissingMethod
                    ifTrue: [self initializeMissingMethod]
                    ifFalse: [super doesNotUnderstand: aMessage]
            """, language="smalltalk")
        elif language == "ruby":
            st.code("""
            # Example of handling NoMethodError in Ruby
            class MyClass
              def method_missing(method_name, *args, &block)
                if method_name.to_s.start_with?('find_by_')
                  # Handle dynamic finders
                  attribute = method_name.to_s.sub('find_by_', '')
                  find_by_attribute(attribute, args.first)
                else
                  super
                end
              end
            
              def respond_to_missing?(method_name, include_private = false)
                method_name.to_s.start_with?('find_by_') || super
              end
            end
            """, language="ruby")
        
        # Learning path
        st.markdown("### 📚 Learning Resources")
        for resource in content.get('resources', []):
            st.markdown(f"- [{resource['title']}]({resource.get('url', '#')})")

def get_error_statistics(db):
    """Get error frequency statistics from database"""
    try:
        query = """
        SELECT error_type, COUNT(*) as count 
        FROM (
            SELECT regexp_matches(user_prompt, '(\\w+Error|\\w+Exception):', 'g') as error_type
            FROM query_history 
            WHERE tool_name = 'error_decoder'
        ) t
        WHERE error_type IS NOT NULL
        GROUP BY error_type
        ORDER BY count DESC
        LIMIT 5
        """
        # Note: This is a simplified version. Actual implementation would need proper SQL
        return []
    except:
        return []

# Initialize session state
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

if "selected_model" not in st.session_state:
    st.session_state.selected_model = "deepseek-coder:6.7b"

if "user_code" not in st.session_state:
    st.session_state.user_code = ""

if "error_data" not in st.session_state:
    st.session_state.error_data = {}

if "educational_mode" not in st.session_state:
    st.session_state.educational_mode = True

# Main UI
st.title("🎓 Advanced Error Decoder")
st.caption("Professional debugging with deep educational insights and code solutions")

# Language selector and educational mode
col1, col2, col3 = st.columns([2, 2, 1])
with col1:
    language = st.selectbox(
        "Focus Language:",
        options=["Auto-detect", "Python", "JavaScript", "Java", "C++", "Ruby", "Rails", "SmallTalk"],
        index=0
    )
with col2:
    analysis_depth = st.select_slider(
        "Analysis Depth",
        options=["Quick", "Standard", "Deep"],
        value="Standard"
    )
with col3:
    st.session_state.educational_mode = st.checkbox(
        "Educational Mode", 
        value=True,
        help="Get detailed explanations and learning resources"
    )

# Main interface tabs
tab1, tab2, tab3 = st.tabs(["Error Analysis", "Code Context", "Error Statistics"])

with tab1:
    error_input = st.text_area(
        f"Paste error message or traceback:", 
        placeholder="MessageNotUnderstood: MyClass>>someMethod" if language=="SmallTalk" 
        else "NoMethodError: undefined method 'name' for nil:NilClass" if language in ["Ruby", "Rails"]
        else "Traceback (most recent call last):\n  File \"app.py\", line 42, in <module>\n    result = calculate(10, 0)\nZeroDivisionError: division by zero",
        height=200,
        value=st.session_state.get('loaded_error', '')
    )
    
    # Example gallery
    with st.expander("📚 Example Gallery"):
        if language == "SmallTalk":
            examples = [
                "MessageNotUnderstood: Array>>doesNotExist",
                "SubscriptOutOfBounds: 'Accessing index 5 of 3-element array'",
                "ZeroDivide: '5 / 0'",
                "ObjectNotFound: 'Non-existent object reference'"
            ]
        elif language in ["Ruby", "Rails"]:
            examples = [
                "NoMethodError: undefined method 'name' for nil:NilClass",
                "ActiveRecord::RecordNotFound: Couldn't find User with 'id'=999",
                "SyntaxError: unexpected end-of-input",
                "NameError: uninitialized constant MyController"
            ]
        else:
            examples = [
                "ValueError: invalid literal for int() with base 10: 'abc'",
                "TypeError: Cannot read properties of undefined (reading 'name')",
                "NullPointerException: Attempt to invoke virtual method on null object",
                "IndexError: list index out of range"
            ]
        
        cols = st.columns(3)
        for i, example in enumerate(examples):
            with cols[i % 3]:
                if st.button(example, use_container_width=True, key=f"ex_{i}"):
                    st.session_state.loaded_error = example
                    st.rerun()

with tab2:
    st.write("Provide code context for better analysis (optional but recommended):")
    
    # Language mapping for code editor
    lang_map = {
        "Auto-detect": "text",
        "Python": "python",
        "Ruby": "ruby",
        "Rails": "ruby",
        "SmallTalk": "smalltalk",
        "JavaScript": "javascript",
        "Java": "java",
        "C++": "cpp"
    }
    
    st.session_state.user_code = st.text_area(
        "Code context:",
        value=st.session_state.user_code,
        height=300,
        placeholder="Paste the code that caused the error..."
    )

with tab3:
    st.subheader("📊 Error Frequency Analysis")
    
    if st.session_state.db:
        # Get recent errors
        recent = st.session_state.db.get_recent_queries(limit=100)
        error_queries = [q for q in recent if q[1] == "error_decoder"]
        
        if error_queries:
            # Count error types
            error_counts = {}
            for query in error_queries:
                parsed = parse_error_message(query[3])
                error_type = parsed["error_type"]
                if error_type != "Unknown":
                    error_counts[error_type] = error_counts.get(error_type, 0) + 1
            
            # Display top errors
            if error_counts:
                st.markdown("### 📈 Most Common Errors")
                sorted_errors = sorted(error_counts.items(), key=lambda x: x[1], reverse=True)[:5]
                
                for error_type, count in sorted_errors:
                    progress = count / sorted_errors[0][1] if sorted_errors else 0
                    st.progress(progress, text=f"{error_type} ({count} occurrences)")
            else:
                st.info("No error statistics available yet")
        else:
            st.info("No errors analyzed yet")
    else:
        st.warning("Database not connected")

# Decode button
if st.button("🔍 Analyze Error", type="primary", use_container_width=True):
    if not error_input.strip():
        st.warning("Please paste an error message")
    else:
        with st.spinner("Performing deep analysis..."):
            # Parse error
            parsed = parse_error_message(error_input)
            if language != "Auto-detect":
                parsed["language"] = language.lower()
            st.session_state.error_data = parsed
            
            # Get analysis based on depth
            if analysis_depth == "Quick":
                analysis = analyze_error(parsed, "", st.session_state.selected_model)
            else:
                analysis = analyze_error(parsed, st.session_state.user_code, st.session_state.selected_model)
            
            # Save to knowledge base
            if st.session_state.db:
                try:
                    query_id = st.session_state.db.log_query(
                        tool="error_decoder",
                        model=st.session_state.selected_model,
                        prompt=error_input,
                        response=analysis
                    )
                    st.session_state.last_query_id = query_id
                except Exception as e:
                    st.error(f"Error logging: {e}")
        
        # Display results
        st.success("✅ Error Analysis Complete!")
        
        # Error card
        st.subheader(f"🔍 {parsed['error_type']} Analysis")
        st.error(f"```\n{error_input}\n```")
        
        # Error metadata
        if parsed.get('file') or parsed.get('line'):
            cols = st.columns(4)
            if parsed.get('file'):
                cols[0].metric("File", parsed['file'])
            if parsed.get('line'):
                cols[1].metric("Line", parsed['line'])
            if parsed.get('language'):
                cols[2].metric("Language", parsed['language'].title())
            if parsed.get('context'):
                cols[3].metric("Context", parsed['context'][:20] + "...")
        
        # Main analysis
        st.markdown("### 📋 Analysis")
        st.markdown(analysis)
        
        # Educational layer
        if st.session_state.educational_mode and analysis_depth != "Quick":
            show_educational_layer(parsed['error_type'], parsed['language'], st.session_state.selected_model)
        
        # Code fix if context available
        if st.session_state.user_code.strip() and analysis_depth != "Quick":
            with st.expander("🛠️ Code Fix Solution"):
                with st.spinner("Generating fix..."):
                    fixed_code = generate_fix_patch(parsed, st.session_state.user_code, st.session_state.selected_model)
                    
                    if fixed_code:
                        st.code(fixed_code, language=lang_map[language])
                        
                        col1, col2 = st.columns([1, 3])
                        with col1:
                            if st.button("Apply Fix"):
                                st.session_state.user_code = fixed_code
                                st.success("Code updated! Check the Code Context tab.")
                        with col2:
                            st.download_button(
                                "Download Fixed Code",
                                data=fixed_code,
                                file_name=f"fixed_{parsed.get('file', 'code')}.{parsed['language']}",
                                mime="text/plain"
                            )
                    else:
                        st.warning("Unable to generate automatic fix")
        
        # Prevention checklist
        with st.expander("✅ Prevention Checklist"):
            with st.spinner("Generating prevention strategies..."):
                prevent_prompt = (
                    f"Create a prevention checklist for {parsed['error_type']} errors in {parsed['language']}"
                )
                prevention = safe_ollama_generate(
                    model=st.session_state.selected_model,
                    prompt=prevent_prompt,
                    system="Output as markdown checklist with 5-7 items"
                )
                if 'error' not in prevention:
                    st.markdown(prevention['response'])
        
        # Community insights (for deep analysis)
        if analysis_depth == "Deep":
            with st.expander("🌐 Community Insights"):
                prompt = f"What are common misconceptions about {parsed['error_type']} in {parsed['language']}?"
                misconceptions = safe_ollama_generate(
                    model=st.session_state.selected_model,
                    prompt=prompt,
                    system="List 3-5 common misconceptions with explanations"
                )
                if 'error' not in misconceptions:
                    st.markdown(misconceptions['response'])
            
            # Historical context
            with st.expander("🕰️ Historical Context"):
                prompt = f"Explain the historical origin of {parsed['error_type']} in {parsed['language']}"
                history = safe_ollama_generate(
                    model=st.session_state.selected_model,
                    prompt=prompt,
                    system="Provide historical context in 2-3 paragraphs"
                )
                if 'error' not in history:
                    st.markdown(history['response'])
        
        # Related errors from knowledge base
        if st.session_state.db:
            with st.expander("📚 Related Historical Errors"):
                recent = st.session_state.db.get_recent_queries(limit=20)
                similar_errors = [
                    q for q in recent 
                    if q[1] == "error_decoder" and 
                    parsed['error_type'] in q[3] and 
                    q[3] != error_input
                ][:3]
                
                if similar_errors:
                    for err in similar_errors:
                        st.caption(f"• {err[3][:100]}...")
                else:
                    st.info("No similar errors found in knowledge base")

# Sidebar
with st.sidebar:
    st.subheader("🔬 Advanced Tools")
    
    # Pattern detection
    if st.button("🔍 Detect Error Pattern"):
        if st.session_state.error_data:
            pattern_prompt = f"Identify patterns in {st.session_state.error_data['error_type']} errors"
            pattern = safe_ollama_generate(
                model=st.session_state.selected_model,
                prompt=pattern_prompt,
                system="List common patterns and triggers"
            )
            if 'error' not in pattern:
                st.info(pattern['response'])
    
    st.divider()
    
    # Learning Center
    st.subheader("📖 Learning Center")
    
    if language == "SmallTalk":
        st.markdown("""
        **SmallTalk Resources:**
        - [Message Handling](https://wiki.squeak.org/squeak/194)
        - [Exception Hierarchy](https://wiki.squeak.org/squeak/223)
        - [doesNotUnderstand:](https://wiki.squeak.org/squeak/195)
        """)
    elif language in ["Ruby", "Rails"]:
        st.markdown("""
        **Ruby/Rails Resources:**
        - [Exception Handling](https://ruby-doc.org/core-3.1.2/Exception.html)
        - [Rails Error Handling](https://guides.rubyonrails.org/action_controller_overview.html#rescue-from)
        - [Metaprogramming](https://ruby-doc.org/core-3.1.2/doc/metaprogramming_rdoc.html)
        """)
    else:
        st.markdown("""
        **General Resources:**
        - Language documentation
        - Stack Overflow
        - Official tutorials
        """)
    
    st.divider()
    
    # Recent errors
    st.subheader("📚 Recent Errors")
    if st.session_state.db:
        recent = st.session_state.db.get_recent_queries(limit=10)
        error_queries = [q for q in recent if q[1] == "error_decoder"][:5]
        
        if error_queries:
            for query in error_queries:
                parsed = parse_error_message(query[3])
                error_type = parsed["error_type"]
                with st.expander(f"🔖 {error_type[:20]}..."):
                    st.caption(query[3][:100] + "...")
                    if st.button("Load", key=f"load_{query[0]}"):
                        st.session_state.loaded_error = query[3]
                        st.rerun()
        else:
            st.info("No recent errors decoded")
    
    st.divider()
    
    # Settings
    st.subheader("⚙️ Settings")
    st.session_state.selected_model = st.selectbox(
        "AI Model",
        options=["deepseek-coder:6.7b", "deepseek-r1:6.7b"],
        index=0
    )
    
    st.caption("💡 Pro Tips:")
    st.info(
        "• Paste full tracebacks for best results\n"
        "• Include code context for fix generation\n"
        "• Use Deep analysis for educational insights"
    )

# Navigation
st.divider()
col1, col2, col3 = st.columns(3)
with col1:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col2:
    if st.button("🛡️ Exception Advisor", use_container_width=True):
        st.switch_page("pages/exception_advisor.py")
with col3:
    if st.button("📚 Knowledge Library", use_container_width=True):
        st.switch_page("pages/knowledge_lib.py")

# Clear loaded error
if 'loaded_error' in st.session_state:
    del st.session_state.loaded_error
