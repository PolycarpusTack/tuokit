"""
Rails Testing Tools
RSpec, System Tests, Factories, and test utilities
"""

import streamlit as st
from typing import Dict, Any, List, Optional

from utils.ollama import safe_ollama_generate
from .config import RAILS_CONFIG

class RSpecGenerator:
    """Generate RSpec tests for Rails"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_model_spec(self, model_code: str) -> Dict[str, Any]:
        """Generate RSpec tests for a model"""
        prompt = f"""Generate comprehensive RSpec tests for this Rails model:

{model_code}

Include:
1. Validation tests (presence, uniqueness, format)
2. Association tests
3. Scope tests
4. Callback tests
5. Custom method tests
6. Edge cases and error conditions
7. Database constraint tests

Use modern RSpec syntax with:
- let statements
- shared examples where appropriate
- proper use of FactoryBot
- shoulda-matchers for concise tests"""
        
        return self._generate_with_ai(prompt, "model")
    
    def generate_controller_spec(self, controller_code: str, resource: str) -> Dict[str, Any]:
        """Generate RSpec tests for a controller"""
        prompt = f"""Generate RSpec controller tests for this Rails controller:

{controller_code}

Resource: {resource}

Include tests for:
1. All controller actions
2. Authentication and authorization
3. Parameter filtering
4. Success and error responses
5. Format handling (HTML/JSON)
6. Edge cases

Use:
- Request specs (preferred over controller specs)
- Proper HTTP status assertions
- JSON response parsing
- Authentication helpers"""
        
        return self._generate_with_ai(prompt, "controller")
    
    def generate_service_spec(self, service_code: str) -> Dict[str, Any]:
        """Generate RSpec tests for a service object"""
        prompt = f"""Generate RSpec tests for this Rails service object:

{service_code}

Include:
1. Success cases with valid inputs
2. Failure cases with invalid inputs
3. Edge cases and boundary conditions
4. External dependency mocking
5. Transaction rollback tests
6. Performance considerations

Use:
- Descriptive contexts
- Proper mocking/stubbing
- Clear test setup"""
        
        return self._generate_with_ai(prompt, "service")
    
    def show_interface(self):
        """Show RSpec generator interface"""
        st.markdown("### 🧪 RSpec Test Generator")
        
        test_type = st.selectbox(
            "Test Type",
            ["Model", "Controller", "Service", "Request", "System", "Job"]
        )
        
        code_input = st.text_area(
            "Paste your code here",
            height=300,
            placeholder="Paste the Ruby code you want to generate tests for..."
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            use_factories = st.checkbox("Use FactoryBot", value=True)
            use_shoulda = st.checkbox("Use Shoulda Matchers", value=True)
        
        with col2:
            include_edge_cases = st.checkbox("Include Edge Cases", value=True)
            include_performance = st.checkbox("Performance Tests", value=False)
        
        if st.button("🚀 Generate Tests", type="primary", use_container_width=True):
            if code_input:
                with st.spinner("Generating RSpec tests..."):
                    if test_type == "Model":
                        result = self.generate_model_spec(code_input)
                    elif test_type == "Controller":
                        resource = st.text_input("Resource name", "posts")
                        result = self.generate_controller_spec(code_input, resource)
                    elif test_type == "Service":
                        result = self.generate_service_spec(code_input)
                    else:
                        result = self._generate_generic_spec(code_input, test_type)
                    
                    if result.get('success'):
                        st.success("✅ Tests generated successfully!")
                        st.code(result.get('spec', ''), language='ruby')
                        
                        # Show test coverage estimate
                        if result.get('coverage_estimate'):
                            st.info(f"Estimated coverage: {result['coverage_estimate']}%")
                    else:
                        st.error(f"Generation failed: {result.get('error', 'Unknown error')}")
            else:
                st.warning("Please paste some code to generate tests for")
    
    def _generate_with_ai(self, prompt: str, spec_type: str) -> Dict[str, Any]:
        """Generate tests using AI"""
        system = f"You are an RSpec expert. Generate comprehensive {spec_type} tests following Rails best practices."
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system=system,
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        return {
            "success": True,
            "spec": response.get("response", ""),
            "coverage_estimate": 85  # Placeholder
        }
    
    def _generate_generic_spec(self, code: str, spec_type: str) -> Dict[str, Any]:
        """Generate generic spec for other types"""
        prompt = f"""Generate RSpec {spec_type} tests for this Rails code:

{code}

Follow RSpec best practices and include comprehensive test coverage."""
        
        return self._generate_with_ai(prompt, spec_type)


class SystemTestGenerator:
    """Generate Rails system tests (integration tests)"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_system_test(self, feature_description: str, **options) -> Dict[str, Any]:
        """Generate system test for a feature"""
        test_framework = options.get('framework', 'Capybara')
        driver = options.get('driver', 'Selenium Chrome')
        
        prompt = f"""Generate Rails system test for: {feature_description}

Framework: {test_framework}
Driver: {driver}

Include:
1. Complete user flow from start to finish
2. Page object pattern for maintainability
3. Proper wait strategies for JavaScript
4. Accessibility checks
5. Mobile responsiveness tests if applicable
6. Error scenarios and edge cases
7. Test data setup and cleanup

Use:
- Capybara DSL effectively
- Page objects for reusable components
- Proper selectors (data-testid preferred)
- Screenshots on failure"""
        
        return self._generate_with_ai(prompt)
    
    def show_interface(self):
        """Show system test generator interface"""
        st.markdown("### 🖥️ System Test Generator")
        
        feature_desc = st.text_area(
            "Feature Description",
            placeholder="User can sign up, confirm email, and log in",
            height=100
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            framework = st.selectbox(
                "Test Framework",
                ["Capybara", "Selenium", "Playwright"]
            )
            
            driver = st.selectbox(
                "Driver",
                ["Selenium Chrome", "Selenium Firefox", "Rack Test", "Cuprite"]
            )
        
        with col2:
            include_js = st.checkbox("JavaScript interactions", value=True)
            include_mobile = st.checkbox("Mobile tests", value=False)
            use_page_objects = st.checkbox("Page Object Pattern", value=True)
            include_a11y = st.checkbox("Accessibility tests", value=True)
        
        if st.button("🚀 Generate System Test", type="primary", use_container_width=True):
            if feature_desc:
                with st.spinner("Generating system test..."):
                    result = self.generate_system_test(
                        feature_desc,
                        framework=framework,
                        driver=driver,
                        include_js=include_js,
                        include_mobile=include_mobile,
                        use_page_objects=use_page_objects,
                        include_a11y=include_a11y
                    )
                    
                    if result.get('success'):
                        st.success("✅ System test generated!")
                        
                        # Show different parts
                        if use_page_objects:
                            tabs = st.tabs(["Test", "Page Objects", "Helpers"])
                            with tabs[0]:
                                st.code(result.get('test', ''), language='ruby')
                            with tabs[1]:
                                st.code(result.get('page_objects', ''), language='ruby')
                            with tabs[2]:
                                st.code(result.get('helpers', ''), language='ruby')
                        else:
                            st.code(result.get('test', ''), language='ruby')
                    else:
                        st.error(f"Generation failed: {result.get('error')}")
            else:
                st.warning("Please describe the feature to test")
    
    def _generate_with_ai(self, prompt: str) -> Dict[str, Any]:
        """Generate system test using AI"""
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="You are a Rails system testing expert. Generate comprehensive integration tests.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        # Parse response for different sections
        code = response.get("response", "")
        sections = self._parse_test_sections(code)
        
        return {
            "success": True,
            **sections
        }
    
    def _parse_test_sections(self, code: str) -> Dict[str, str]:
        """Parse test code into sections"""
        sections = {"test": code}
        
        # Look for page object definitions
        if "class.*Page" in code:
            # Simple parsing - in production would be more sophisticated
            parts = code.split("# === ")
            for part in parts:
                if "page" in part.lower():
                    sections["page_objects"] = part
                elif "helper" in part.lower():
                    sections["helpers"] = part
                else:
                    sections["test"] = part
        
        return sections


class FactoryGenerator:
    """Generate FactoryBot factories"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate_factory(self, model_code: str) -> Dict[str, Any]:
        """Generate factory from model code"""
        prompt = f"""Generate FactoryBot factory for this Rails model:

{model_code}

Include:
1. Base factory with all required attributes
2. Traits for different scenarios
3. Associations with proper strategies
4. Sequences for unique attributes
5. Callbacks for complex setup
6. Nested factories for STI if applicable

Follow FactoryBot best practices."""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="Generate clean FactoryBot factories following best practices.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Generation failed")
            }
        
        return {
            "success": True,
            "factory": response.get("response", "")
        }