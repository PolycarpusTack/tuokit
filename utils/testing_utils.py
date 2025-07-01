# utils/testing_utils.py
"""
Testing utilities for Rails system tests
"""
import re
from typing import Dict, List, Optional

class TestGenerator:
    """Rails test generation utilities"""
    
    @staticmethod
    def extract_test_components(test_code: str) -> Dict[str, str]:
        """Parse test code into sections"""
        components = {
            "setup": "",
            "exercise": "",
            "verify": "",
            "teardown": ""
        }
        
        # Try to extract setup section
        setup_match = re.search(r"# Setup(.+?)# Exercise", test_code, re.DOTALL)
        if setup_match:
            components["setup"] = setup_match.group(1).strip()
        
        # Try to extract exercise section
        exercise_match = re.search(r"# Exercise(.+?)# Verify", test_code, re.DOTALL)
        if exercise_match:
            components["exercise"] = exercise_match.group(1).strip()
        
        # Try to extract verify section
        verify_match = re.search(r"# Verify(.+?)(# Teardown|$)", test_code, re.DOTALL)
        if verify_match:
            components["verify"] = verify_match.group(1).strip()
        
        # Try to extract teardown section
        teardown_match = re.search(r"# Teardown(.+?)$", test_code, re.DOTALL)
        if teardown_match:
            components["teardown"] = teardown_match.group(1).strip()
        
        return components
    
    @staticmethod
    def generate_page_object(page_name: str, elements: List[Dict[str, str]]) -> str:
        """Generate page object pattern code"""
        class_name = ''.join(word.capitalize() for word in page_name.split('_'))
        
        code = f"""class {class_name}Page
  include Capybara::DSL
  
  # Page elements
"""
        
        for element in elements:
            name = element.get('name', 'element')
            selector = element.get('selector', '#element')
            code += f"  def {name}\n"
            code += f"    find('{selector}')\n"
            code += f"  end\n\n"
        
        code += """  # Page actions
  def visit_page
    visit page_url
  end
  
  private
  
  def page_url
    # Define your page URL here
    '/'
  end
end"""
        
        return code
    
    @staticmethod
    def generate_test_data_factory(model_name: str, attributes: List[str]) -> str:
        """Generate FactoryBot factory code"""
        factory_code = f"""FactoryBot.define do
  factory :{model_name.lower()} do
"""
        
        for attr in attributes:
            if 'email' in attr.lower():
                factory_code += f"    {attr} {{ Faker::Internet.email }}\n"
            elif 'name' in attr.lower():
                factory_code += f"    {attr} {{ Faker::Name.name }}\n"
            elif 'date' in attr.lower():
                factory_code += f"    {attr} {{ Faker::Date.backward(days: 30) }}\n"
            else:
                factory_code += f"    {attr} {{ Faker::Lorem.word }}\n"
        
        factory_code += "  end\nend"
        
        return factory_code
    
    @staticmethod
    def validate_test_structure(test_code: str) -> List[str]:
        """Validate test structure and return warnings"""
        warnings = []
        
        # Check for test description
        if not re.search(r"(describe|context|it)\s+['\"]", test_code):
            warnings.append("Missing test descriptions")
        
        # Check for assertions
        if not re.search(r"(expect|assert)", test_code):
            warnings.append("No assertions found")
        
        # Check for database cleaning
        if not re.search(r"(DatabaseCleaner|database_cleaner)", test_code):
            warnings.append("Consider adding database cleaning strategy")
        
        # Check for accessibility testing
        if not re.search(r"(axe|a11y|accessibility)", test_code):
            warnings.append("Consider adding accessibility checks")
        
        return warnings
