# pages/ruby_c_extensions.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Ruby C Extensions - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate

def create_extension(description, config):
    """Generate safe Ruby C extension"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Create C extension for: {description} | Config: {config}",
        system=(
            "Output complete implementation:\n"
            "- extconf.rb\n"
            "- C source file\n"
            "- Ruby binding code\n"
            "- Safety precautions\n"
            "- Rakefile tasks\n"
            "Include memory management and error handling"
        )
    )['response']

def generate_benchmarks(description):
    """Generate performance benchmarks for the extension"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Generate benchmarks comparing Ruby vs C extension for: {description}",
        system="Use benchmark-ips to show performance improvement"
    )['response']

def show():
    st.title("🛠️ Ruby C Extension Assistant")
    st.caption("Build high-performance native extensions for Ruby")
    
    # Task description
    description = st.text_area("Describe Native Functionality", 
                              height=150,
                              placeholder="e.g., Fast matrix multiplication, image processing, cryptographic operations",
                              key="c_ext_desc")
    
    # Safety and configuration options
    with st.sidebar:
        st.subheader("Extension Configuration")
        memory_model = st.radio("Memory Management", ["Manual", "TypedData (Recommended)"])
        exception_handling = st.toggle("Ruby Exception Handling", True)
        thread_safety = st.toggle("Thread-Safe Implementation", False)
        
        safety_level = st.select_slider("Safety Level", 
                                      ["Performance Focus", "Balanced", "Safety First"],
                                      value="Balanced")
        
        st.subheader("Features")
        features = st.multiselect("Include Features",
                                ["GC Marking", "Object Allocation", "Type Checking", 
                                 "Debugging Support", "Profiling Hooks"],
                                default=["GC Marking", "Type Checking"])
    
    # C library integration
    uses_external = st.checkbox("Integrates with External C Library")
    if uses_external:
        library_name = st.text_input("Library Name", placeholder="e.g., libpng, openssl")
    else:
        library_name = None
    
    if st.button("Generate Extension", type="primary") and description:
        with st.spinner("Compiling native bridge..."):
            config = {
                "memory": memory_model,
                "exceptions": exception_handling,
                "thread_safe": thread_safety,
                "safety_level": safety_level,
                "features": features,
                "external_lib": library_name
            }
            
            extension = create_extension(description, config)
            
            # Display results in tabs
            tab1, tab2, tab3, tab4, tab5 = st.tabs(["C Source", "Build Files", "Tests", "Benchmarks", "Documentation"])
            
            with tab1:
                st.subheader("C Extension Implementation")
                st.code(extension, language="c")
                st.download_button("Download extension.c", extension, "extension.c")
                
                # Show memory management pattern
                if memory_model == "TypedData (Recommended)":
                    with st.expander("TypedData Pattern Explained"):
                        st.markdown("""
                        **TypedData Benefits:**
                        - Automatic GC integration
                        - Type safety
                        - Memory leak prevention
                        - Better debugging
                        
                        ```c
                        static const rb_data_type_t my_type = {
                            "MyExtension",
                            {mark_func, free_func, size_func},
                            0, 0, RUBY_TYPED_FREE_IMMEDIATELY
                        };
                        ```
                        """)
                
            with tab2:
                st.subheader("Build Configuration")
                
                # extconf.rb
                extconf = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Generate extconf.rb for C extension: {description}",
                    system=f"Include library checks for: {library_name if library_name else 'standard libs'}"
                )['response']
                st.code(extconf, language="ruby")
                st.download_button("Download extconf.rb", extconf, "extconf.rb")
                
                # Rakefile
                rakefile = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Generate Rakefile for C extension with compile and test tasks",
                    system="Include rake-compiler tasks"
                )['response']
                st.code(rakefile, language="ruby")
                
            with tab3:
                st.subheader("Extension Tests")
                tests = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=f"Generate tests for C extension: {description}",
                    system="Include unit tests, edge cases, and memory leak tests"
                )['response']
                st.code(tests, language="ruby")
                st.download_button("Download tests.rb", tests, "test_extension.rb")
                
            with tab4:
                st.subheader("Performance Benchmarks")
                benchmarks = generate_benchmarks(description)
                st.code(benchmarks, language="ruby")
                
                # Expected performance
                st.info("Typical C extension performance improvements:")
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("Numeric Operations", "10-100x faster")
                with col2:
                    st.metric("String Processing", "5-50x faster")
                with col3:
                    st.metric("Memory Usage", "50-90% less")
                
            with tab5:
                st.markdown(f"""
                ### Build & Installation
                
                **1. Compile the extension:**
                ```bash
                ruby extconf.rb
                make
                ```
                
                **2. Run tests:**
                ```bash
                ruby -Ilib:test test_extension.rb
                ```
                
                **3. Install:**
                ```bash
                rake install
                ```
                
                **4. Use in Ruby:**
                ```ruby
                require '{description.lower().replace(' ', '_')}'
                # Use your extension
                ```
                
                ### Debugging
                
                **GDB debugging:**
                ```bash
                gdb ruby
                (gdb) run -Ilib script.rb
                ```
                
                **Valgrind memory check:**
                ```bash
                valgrind --leak-check=full ruby script.rb
                ```
                """)
            
            # Safety considerations
            with st.expander("⚠️ C Extension Safety Guide", expanded=True):
                col1, col2 = st.columns(2)
                
                with col1:
                    st.markdown("""
                    **Memory Management:**
                    - Always use TypedData_Wrap_Struct
                    - Mark all Ruby objects in GC
                    - Free allocated memory
                    - Check for NULL pointers
                    
                    **Type Safety:**
                    - Use Check_Type macros
                    - Validate array bounds
                    - Handle numeric overflow
                    - String encoding checks
                    """)
                
                with col2:
                    st.markdown("""
                    **Error Handling:**
                    - Use rb_raise for errors
                    - Protect allocations
                    - Clean up on exceptions
                    - Document error conditions
                    
                    **Thread Safety:**
                    - Avoid global state
                    - Use rb_mutex if needed
                    - Document thread safety
                    - Test concurrent access
                    """)
                
                st.link_button("Ruby C API Documentation", 
                             "https://docs.ruby-lang.org/en/master/doc/extension_rdoc.html")
            
            # Common patterns
            with st.expander("💡 Common C Extension Patterns"):
                st.markdown("""
                **String Processing:**
                ```c
                VALUE process_string(VALUE self, VALUE str) {
                    Check_Type(str, T_STRING);
                    char *c_str = StringValueCStr(str);
                    // Process string
                    return rb_str_new_cstr(result);
                }
                ```
                
                **Array Iteration:**
                ```c
                VALUE process_array(VALUE self, VALUE arr) {
                    Check_Type(arr, T_ARRAY);
                    long len = RARRAY_LEN(arr);
                    for (long i = 0; i < len; i++) {
                        VALUE elem = rb_ary_entry(arr, i);
                        // Process element
                    }
                    return result;
                }
                ```
                
                **Numeric Operations:**
                ```c
                VALUE fast_calc(VALUE self, VALUE num) {
                    double n = NUM2DBL(num);
                    double result = perform_calculation(n);
                    return DBL2NUM(result);
                }
                ```
                """)
            
            # Save to knowledge base
            if st.button("💾 Save to Toolkit"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="c_extensions",
                        model=ModelManager.get_default_model(),
                        prompt=description,
                        response=extension,
                        metadata={
                            "tags": ["ruby", "c", "performance"],
                            "memory_model": memory_model,
                            "thread_safe": thread_safety
                        }
                    )
                    if query_id:
                        st.success("Extension saved!")
                else:
                    st.error("Could not connect to database")

if __name__ == "__main__":
    show()
