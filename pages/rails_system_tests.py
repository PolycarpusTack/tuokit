# pages/rails_system_tests.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rails System Tests - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
if "a11y" not in st.session_state:
    st.session_state.a11y = None
from utils import DatabaseManager, safe_ollama_generate

def generate_system_test(feature_description, test_framework="RSpec"):
    """Generate comprehensive system tests with accessibility checks"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Generate {test_framework} system test for: {feature_description}",
        system=(
            "Include:\n"
            "- Page object model\n"
            "- User journey scenarios\n"
            "- Accessibility checks (a11y)\n"
            "- JavaScript interaction\n"
            "- Screenshot on failure\n"
            "- Database cleaning\n"
            "Use modern best practices."
        )
    )['response']

def show():
    st.title("🧪 Rails System Test Generator")
    st.caption("Create production-ready system tests with accessibility checks")
    
    # Inputs
    feature = st.text_area("Describe Feature", 
                          height=150,
                          placeholder="e.g., User registration flow with email confirmation")
    
    # Configuration
    with st.sidebar:
        st.subheader("Test Configuration")
        test_framework = st.radio("Framework", ["RSpec", "Minitest"])
        browser = st.selectbox("Browser", ["Chrome", "Firefox", "Safari"])
        js_driver = st.radio("JavaScript Driver", ["Selenium", "Cuprite", "Apparition"])
        st.toggle("Include Accessibility Checks", True, key="a11y")
        st.toggle("Add Visual Testing", False, key="visual")
    
    if st.button("Generate Tests", type="primary") and feature:
        with st.spinner("Creating test scenarios..."):
            test_code = generate_system_test(feature, test_framework)
            
            # Display results
            st.subheader("System Test Implementation")
            st.code(test_code, language="ruby")
            
            # Test structure explanation
            with st.expander("🧩 Test Anatomy", expanded=True):
                st.markdown("""
                **Key Components:**
                1. **Setup**: Test data preparation
                2. **Exercise**: User interactions
                3. **Verify**: Assertions
                4. **Teardown**: Cleanup
                
                **Best Practices:**
                - One assertion per test
                - Independent test cases
                - Realistic user journeys
                """)
            
            # Download options
            col1, col2 = st.columns(2)
            with col1:
                st.download_button("Download Test", test_code, "system_test.rb")
            with col2:
                if st.button("Save to Test Suite"):
                    db = DatabaseManager()
                    if db.connected:
                        query_id = db.log_query(
                            tool="system_tests",
                            model=ModelManager.get_default_model(),
                            prompt=feature,
                            response=test_code,
                            metadata={"tags": ["rails", "testing", test_framework.lower()]}
                        )
                        if query_id:
                            st.success("Test saved to knowledge library!")
                    else:
                        st.error("Could not connect to database")
            
            # Accessibility reference
            if st.session_state.a11y:
                with st.expander("♿ Accessibility Standards", expanded=True):
                    st.markdown("""
                    **WCAG 2.1 Key Checks:**
                    - Keyboard navigation
                    - Color contrast
                    - ARIA landmarks
                    - Form labels
                    - Image alt text
                    
                    **Tools:**
                    - `axe-core` for automated checks
                    - `pa11y` for CLI testing
                    """)
                    st.link_button("WebAIM Checklist", "https://webaim.org/standards/wcag/checklist")

if __name__ == "__main__":
    show()
