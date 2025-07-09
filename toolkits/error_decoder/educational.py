# toolkits/error_decoder/educational.py
"""Educational layer components for error learning"""

import streamlit as st
from .processors import get_educational_content
from .knowledge_base import LEARNING_RESOURCES

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
        
        # Language-specific examples
        show_language_specific_examples(language)
        
        # Learning path
        st.markdown("### 📚 Learning Resources")
        for resource in content.get('resources', []):
            st.markdown(f"- [{resource['title']}]({resource.get('url', '#')})")

def show_language_specific_examples(language):
    """Display language-specific code examples"""
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
    elif language == "python":
        st.code("""
        # Example of handling AttributeError in Python
        class SafeDict(dict):
            def __getattr__(self, name):
                try:
                    return self[name]
                except KeyError:
                    raise AttributeError(f"'{type(self).__name__}' object has no attribute '{name}'")
            
            def get_safe(self, key, default=None):
                return self.get(key, default)
        """, language="python")
    elif language == "javascript":
        st.code("""
        // Example of handling TypeError in JavaScript
        function safeAccess(obj, path, defaultValue) {
            try {
                return path.split('.').reduce((o, p) => o[p], obj) ?? defaultValue;
            } catch (e) {
                if (e instanceof TypeError) {
                    return defaultValue;
                }
                throw e;
            }
        }
        
        // Usage: safeAccess(user, 'profile.name', 'Unknown')
        """, language="javascript")

def show_learning_center(language):
    """Display learning center in sidebar"""
    st.subheader("📖 Learning Center")
    
    # Get appropriate resources
    resources = LEARNING_RESOURCES.get(language, LEARNING_RESOURCES.get("default"))
    st.markdown(resources)

def show_error_examples(language):
    """Display error examples gallery"""
    from .config import EXAMPLE_ERRORS
    
    examples = EXAMPLE_ERRORS.get(language, EXAMPLE_ERRORS.get("default"))
    
    with st.expander("📚 Example Gallery"):
        cols = st.columns(3)
        for i, example in enumerate(examples):
            with cols[i % 3]:
                if st.button(example, use_container_width=True, key=f"ex_{i}"):
                    st.session_state.loaded_error = example
                    st.rerun()

def show_error_statistics(db):
    """Display error frequency statistics"""
    from .parsers import get_error_statistics
    
    st.subheader("📊 Error Frequency Analysis")
    
    if db:
        error_stats = get_error_statistics(db)
        
        if error_stats:
            st.markdown("### 📈 Most Common Errors")
            
            # Display as progress bars
            max_count = error_stats[0][1] if error_stats else 1
            for error_type, count in error_stats:
                progress = count / max_count
                st.progress(progress, text=f"{error_type} ({count} occurrences)")
        else:
            st.info("No error statistics available yet")
    else:
        st.warning("Database not connected")