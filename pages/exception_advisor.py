# pages/exception_advisor.py
import streamlit as st
from utils.model_manager import ModelManager
from utils.annotated_text_helpers import (
    annotated_text, parse_error_message, highlight_severity,
    ANNOTATION_COLORS
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate

# Page configuration
st.set_page_config(
    page_title="TuoKit - Exception Handling Advisor",
    page_icon="🛡️",
    layout="wide"
)

def analyze_exception_handling(code, language, model=ModelManager.get_default_model()):
    """Analyze exception handling patterns in code"""
    system_prompt = (
        f"You are a senior {language} engineer. Analyze exception handling:\n"
        "1. Identify exception handling approaches\n"
        "2. Evaluate error recovery strategies\n"
        "3. Assess logging and monitoring\n"
        "4. Provide improvement suggestions\n"
        "Format in markdown with sections: Approaches, Strengths, Weaknesses, Recommendations."
    )
    
    response = safe_ollama_generate(
        model=model,
        prompt=f"Code:\n```{language}\n{code}\n```",
        system=system_prompt
    )
    
    if 'error' in response:
        return f"Error analyzing code: {response['response']}"
    
    return response.get('response', 'Unable to analyze exception handling')

def generate_handling_strategy(language, system_type, model=ModelManager.get_default_model()):
    """Generate exception handling strategy template"""
    system_prompt = (
        f"Create a comprehensive {language} exception handling strategy for {system_type} systems.\n"
        "Include:\n"
        "- Error classification\n"
        "- Handling approaches\n"
        "- Logging standards\n"
        "- Monitoring integration\n"
        "- Fallback mechanisms\n"
        "Format in markdown with code examples."
    )
    
    response = safe_ollama_generate(
        model=model,
        prompt=f"Create {language} exception strategy for {system_type}",
        system=system_prompt
    )
    
    if 'error' in response:
        return f"Error generating strategy: {response['response']}"
    
    return response.get('response', 'Unable to generate strategy')

# Initialize session state
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

if "selected_model" not in st.session_state:
    st.session_state.selected_model = ModelManager.get_default_model()

# Main UI
st.title("🛡️ Exception Handling Advisor")
st.caption("Design robust error handling strategies for your systems")

# Main tabs
tab1, tab2, tab3 = st.tabs(["Code Analysis", "Strategy Builder", "Language Guide"])

with tab1:
    st.subheader("Analyze Exception Handling")
    
    col1, col2 = st.columns([1, 3])
    with col1:
        language = st.selectbox(
            "Language:",
            options=["Python", "Java", "JavaScript", "Ruby", "SmallTalk", "C++"],
            index=0,
            key="analyze_lang"
        )
    
    code = st.text_area(
        f"Paste {language} code to analyze:",
        height=300,
        placeholder="""def read_file(path):
    try:
        with open(path) as f:
            return f.read()
    except FileNotFoundError:
        logger.error("File not found")
        raise"""
    )
    
    if st.button("🔍 Analyze Handling", type="primary", use_container_width=True):
        if not code.strip():
            st.warning("Please paste code to analyze")
        else:
            with st.spinner("Analyzing exception patterns..."):
                analysis = analyze_exception_handling(code, language, st.session_state.selected_model)
                st.subheader("Exception Handling Analysis")
                
                # Check if the analysis contains exception examples and highlight them
                if "exception" in analysis.lower() or "error" in analysis.lower():
                    # Display a sample annotated exception
                    st.markdown("#### Exception Pattern Detection:")
                    sample_exceptions = [
                        ("FileNotFoundError", "error_type", ANNOTATION_COLORS["error_type"]),
                        (": ", "text", "#000000"),
                        ("No such file or directory", "error", ANNOTATION_COLORS["error"]),
                        (" at line ", "text", "#000000"),
                        ("42", "line_number", ANNOTATION_COLORS["line_number"])
                    ]
                    annotated_text(*sample_exceptions)
                
                st.markdown(analysis)
                
                # Save to knowledge base
                if st.session_state.db:
                    try:
                        st.session_state.db.log_query(
                            tool="exception_advisor",
                            model=st.session_state.selected_model,
                            prompt=code[:500] + "...",
                            response=analysis
                        )
                    except Exception as e:
                        st.error(f"Error logging: {e}")

with tab2:
    st.subheader("Build Custom Strategy")
    
    col1, col2 = st.columns(2)
    with col1:
        language = st.selectbox(
            "Target Language:",
            options=["Python", "Java", "JavaScript", "Ruby", "SmallTalk", "C++"],
            index=0,
            key="strategy_lang"
        )
    with col2:
        system_type = st.selectbox(
            "System Type:",
            options=["Web Application", "Microservice", "Desktop App", 
                     "Embedded System", "Batch Processor", "API Service"],
            index=0
        )
    
    # Additional strategy options
    with st.expander("🎯 Advanced Options"):
        col1, col2 = st.columns(2)
        with col1:
            include_logging = st.checkbox("Include Logging Strategy", value=True)
            include_monitoring = st.checkbox("Include Monitoring Integration", value=True)
        with col2:
            include_testing = st.checkbox("Include Testing Patterns", value=True)
            include_recovery = st.checkbox("Include Recovery Mechanisms", value=True)
    
    if st.button("🚀 Generate Strategy", type="primary", use_container_width=True):
        with st.spinner("Creating custom strategy..."):
            # Build enhanced prompt based on options
            enhanced_prompt = f"Create {language} exception strategy for {system_type}"
            if include_logging:
                enhanced_prompt += " with comprehensive logging"
            if include_monitoring:
                enhanced_prompt += " and monitoring integration"
            if include_testing:
                enhanced_prompt += " including unit test patterns"
            if include_recovery:
                enhanced_prompt += " with automatic recovery mechanisms"
            
            strategy = generate_handling_strategy(language, system_type, st.session_state.selected_model)
            
            st.success("✅ Strategy Generated!")
            st.subheader(f"{language} Exception Strategy for {system_type}")
            st.markdown(strategy)
            
            # Save to knowledge base
            if st.session_state.db:
                try:
                    st.session_state.db.log_query(
                        tool="exception_advisor",
                        model=st.session_state.selected_model,
                        prompt=enhanced_prompt,
                        response=strategy
                    )
                except Exception as e:
                    st.error(f"Error logging: {e}")
            
            # Download option
            st.download_button(
                "📥 Download Strategy",
                data=strategy,
                file_name=f"{language.lower()}_exception_strategy_{system_type.lower().replace(' ', '_')}.md",
                mime="text/markdown"
            )

with tab3:
    st.subheader("Language-Specific Guides")
    
    lang = st.selectbox(
        "Select Language:",
        options=["Python", "Java", "JavaScript", "Ruby", "SmallTalk", "C++"],
        index=0,
        key="guide_lang"
    )
    
    # Display language-specific guide
    with st.expander(f"📚 {lang} Exception Handling Best Practices", expanded=True):
        if lang == "SmallTalk":
            st.markdown("""
            ### VisualWorks SmallTalk Exception Handling
            
            **Unique Characteristics:**
            - Exception objects are normal Smalltalk objects
            - Handler blocks use `on:do:` syntax
            - Resumable exceptions with `resume:`, `resume`, `pass`
            
            **Best Practices:**
            
            1. **Use specific exception classes:**
            ```smalltalk
            [ someOperation ] 
                on: FileNotFound 
                do: [:ex | self handleFileNotFound: ex]
            ```
            
            2. **Create custom exceptions:**
            ```smalltalk
            MyCustomError class >> signal: aString
                ^ self new
                    messageText: aString;
                    signal
            ```
            
            3. **Use resumable exceptions for recoverable errors:**
            ```smalltalk
            [ self validateInput ] 
                on: InvalidInput 
                do: [:ex | ex resume: false]
            ```
            
            4. **Centralize exception handling in application supervisor**
            
            5. **Use ensure: for cleanup:**
            ```smalltalk
            [file := FileStream open: 'data.txt'.
             self processFile: file]
                ensure: [file ifNotNil: [file close]]
            ```
            """)
        
        elif lang == "Ruby":
            st.markdown("""
            ### Ruby & Rails Exception Handling
            
            **Key Patterns:**
            - Use `begin/rescue/ensure/end` blocks
            - Rails: Controller-level `rescue_from`
            - Custom exceptions inheriting from StandardError
            
            **Best Practices:**
            
            1. **Rescue specific exceptions:**
            ```ruby
            begin
              # risky code
            rescue ActiveRecord::RecordNotFound => e
              # handle not found
            rescue StandardError => e
              # fallback
            end
            ```
            
            2. **Create custom exceptions:**
            ```ruby
            class PaymentError < StandardError
              attr_reader :payment_id
              
              def initialize(message, payment_id)
                super(message)
                @payment_id = payment_id
              end
            end
            ```
            
            3. **Rails controller handling:**
            ```ruby
            class ApplicationController < ActionController::Base
              rescue_from PaymentError, with: :handle_payment_error
            
              private
              def handle_payment_error(exception)
                @error = exception
                render 'payment_error'
              end
            end
            ```
            
            4. **Use retry for transient errors:**
            ```ruby
            retries = 0
            begin
              fetch_external_data
            rescue Net::ReadTimeout => e
              retries += 1
              retry if retries < 3
              raise
            end
            ```
            
            5. **Log with context:**
            ```ruby
            Rails.logger.error("Error: #{e.message}")
            Rails.logger.error(e.backtrace.join("\n"))
            ```
            """)
        
        elif lang == "Python":
            st.markdown("""
            ### Python Exception Handling
            
            **Core Concepts:**
            - try/except/else/finally blocks
            - Exception hierarchy
            - Context managers
            
            **Best Practices:**
            
            1. **Catch specific exceptions:**
            ```python
            try:
                result = risky_operation()
            except ValueError as e:
                logger.error(f"Invalid value: {e}")
            except (IOError, OSError) as e:
                logger.error(f"IO error: {e}")
            ```
            
            2. **Create custom exceptions:**
            ```python
            class ValidationError(Exception):
                def __init__(self, field, value):
                    self.field = field
                    self.value = value
                    super().__init__(f"Invalid {field}: {value}")
            ```
            
            3. **Use context managers:**
            ```python
            from contextlib import contextmanager
            
            @contextmanager
            def managed_resource():
                resource = acquire_resource()
                try:
                    yield resource
                finally:
                    release_resource(resource)
            ```
            """)
        
        elif lang == "Java":
            st.markdown("""
            ### Java Exception Handling
            
            **Key Concepts:**
            - Checked vs Unchecked exceptions
            - try-with-resources
            - Exception chaining
            
            **Best Practices:**
            
            1. **Use try-with-resources:**
            ```java
            try (FileInputStream file = new FileInputStream("data.txt")) {
                // process file
            } catch (IOException e) {
                logger.error("File error", e);
            }
            ```
            
            2. **Create domain-specific exceptions:**
            ```java
            public class BusinessException extends Exception {
                private final ErrorCode errorCode;
                
                public BusinessException(String message, ErrorCode code) {
                    super(message);
                    this.errorCode = code;
                }
            }
            ```
            """)
        
        else:
            st.info(f"Guide for {lang} coming soon!")
    
    # Common anti-patterns
    st.divider()
    st.subheader("⚠️ Common Anti-Patterns")
    
    anti_patterns = [
        {
            "pattern": "Empty Catch Blocks",
            "consequence": "Silent failures, hidden bugs",
            "solution": "Always log errors at minimum"
        },
        {
            "pattern": "Catching Throwable/Exception",
            "consequence": "Catches unrecoverable errors",
            "solution": "Catch specific exceptions"
        },
        {
            "pattern": "Exception Swallowing",
            "consequence": "Hidden errors, debugging nightmare",
            "solution": "Always log/report/re-throw"
        },
        {
            "pattern": "Overly Broad Catches",
            "consequence": "Masking real issues",
            "solution": "Catch most specific exceptions"
        },
        {
            "pattern": "Exception Control Flow",
            "consequence": "Performance issues, unclear code",
            "solution": "Use exceptions for exceptional cases only"
        }
    ]
    
    for pattern in anti_patterns:
        with st.expander(f"❌ {pattern['pattern']}"):
            st.error(f"**Consequence:** {pattern['consequence']}")
            st.success(f"**Solution:** {pattern['solution']}")

# Sidebar
with st.sidebar:
    st.subheader("📚 Exception Resources")
    
    st.markdown("""
    **Documentation:**
    - [SmallTalk Exceptions](https://wiki.squeak.org/squeak/194)
    - [Ruby Error Handling](https://www.honeybadger.io/guides/ruby-exception-handling/)
    - [Java Exception Guide](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
    - [Python Exception Docs](https://docs.python.org/3/tutorial/errors.html)
    
    **Books:**
    - Exceptional Ruby (Avdi Grimm)
    - Effective Java (Joshua Bloch)
    - Clean Code (Robert Martin)
    """)
    
    st.divider()
    
    # Settings
    st.subheader("⚙️ Settings")
    st.session_state.selected_model = st.selectbox(
        "AI Model",
        options=get_available_models(),
        index=0
    )
    
    st.divider()
    
    # Quick tips
    st.subheader("💡 Quick Tips")
    st.info(
        "• Fail fast, fail clearly\n"
        "• Log errors with context\n"
        "• Use specific exception types\n"
        "• Consider recovery strategies\n"
        "• Test error paths"
    )

# Navigation
st.divider()
col1, col2, col3 = st.columns(3)
with col1:
    if st.button("← Error Decoder", use_container_width=True):
        st.switch_page("pages/error_tool.py")
with col2:
    if st.button("🏠 Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col3:
    if st.button("📚 Knowledge Library", use_container_width=True):
        st.switch_page("pages/knowledge_lib.py")
