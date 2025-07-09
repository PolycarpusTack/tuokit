"""
Rails Code Generators
Model, Controller, Service Object, and other generators
"""

import streamlit as st
from typing import Dict, Any, List, Optional
import re

from utils.ollama import safe_ollama_generate
from .config import RAILS_CONFIG

class BaseGenerator:
    """Base class for all Rails generators"""
    
    def __init__(self, generator_type: str):
        self.generator_type = generator_type
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def generate(self, **kwargs) -> Dict[str, Any]:
        """Generate code based on parameters"""
        raise NotImplementedError
    
    def _generate_with_ai(self, prompt: str, system: str = None) -> Dict[str, Any]:
        """Generate code using AI model"""
        if not system:
            system = f"You are a Rails expert. Generate production-ready {self.generator_type} code following Rails conventions."
        
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
        
        # Parse the response to extract different code sections
        code = response.get("response", "")
        return self._parse_generated_code(code)
    
    def _parse_generated_code(self, code: str) -> Dict[str, Any]:
        """Parse generated code into sections"""
        # Override in subclasses for specific parsing
        return {
            "success": True,
            "full_code": code
        }


class ModelGenerator(BaseGenerator):
    """Generate Rails models with migrations"""
    
    def __init__(self):
        super().__init__("model")
    
    def generate(self, description: str, **options) -> Dict[str, Any]:
        """Generate Rails model based on description"""
        enhancements = []
        
        if options.get('use_uuid'):
            enhancements.append("Use UUID as primary key")
        if options.get('add_devise'):
            enhancements.append("Include Devise authentication modules")
        if options.get('add_factory'):
            enhancements.append("Include FactoryBot factory")
        if options.get('add_tests'):
            enhancements.append("Include RSpec model tests")
        
        prompt = f"""Generate a complete Rails 7 model for: {description}

{chr(10).join(enhancements) if enhancements else ''}

Provide complete code including:
1. Migration file with proper indexes and foreign keys
2. Model class with:
   - Validations (presence, uniqueness, format, etc.)
   - Associations if mentioned in description
   - Scopes for common queries
   - Callbacks if appropriate
   - Custom methods if needed
3. {"FactoryBot factory for testing" if options.get('add_factory') else ""}
4. {"RSpec model tests with full coverage" if options.get('add_tests') else ""}

Use modern Rails conventions and best practices. Include helpful comments.
Separate each file with a comment like: # === filename.rb ==="""
        
        return self._generate_with_ai(prompt)
    
    def _parse_generated_code(self, code: str) -> Dict[str, Any]:
        """Parse model generation into sections"""
        sections = {}
        current_section = None
        current_content = []
        
        # Parse code sections
        for line in code.split('\n'):
            if '# ===' in line and '.rb ===' in line:
                # Save previous section
                if current_section:
                    sections[current_section] = '\n'.join(current_content)
                
                # Start new section
                if 'migration' in line.lower():
                    current_section = 'migration'
                elif 'factory' in line.lower():
                    current_section = 'factory'
                elif 'spec' in line.lower() or 'test' in line.lower():
                    current_section = 'tests'
                elif 'model' in line.lower():
                    current_section = 'model'
                else:
                    current_section = 'other'
                
                current_content = []
            else:
                current_content.append(line)
        
        # Save last section
        if current_section:
            sections[current_section] = '\n'.join(current_content)
        
        # Extract model name
        model_name = "Model"
        model_match = re.search(r'class\s+(\w+)\s*<\s*(?:ApplicationRecord|ActiveRecord::Base)', code)
        if model_match:
            model_name = model_match.group(1)
        
        return {
            "success": True,
            "model_name": model_name,
            "full_code": code,
            **sections
        }


class ControllerGenerator(BaseGenerator):
    """Generate Rails controllers"""
    
    def __init__(self):
        super().__init__("controller")
    
    def generate(self, resource: str, actions: List[str], **options) -> Dict[str, Any]:
        """Generate Rails controller with specified actions"""
        context = []
        
        if options.get('api_only'):
            context.append(f"API-only controller (no views)")
            if options.get('api_version'):
                context.append(f"API version {options['api_version']}")
        
        if options.get('add_auth'):
            context.append("Include authentication (before_action)")
        
        if options.get('nested_under'):
            context.append(f"Nested under {options['nested_under']}")
        
        prompt = f"""Generate a Rails 7 controller for resource: {resource}
Actions to include: {', '.join(actions)}
{chr(10).join(context) if context else ''}

Include:
1. Controller class with all requested actions
2. Strong parameters
3. Proper error handling and status codes
4. {"JSON responses" if options.get('api_only') else "HTML and JSON format handling"}
5. Routes configuration
6. RSpec controller tests

Separate each file with: # === filename.rb ==="""
        
        return self._generate_with_ai(prompt)
    
    def _parse_generated_code(self, code: str) -> Dict[str, Any]:
        """Parse controller generation into sections"""
        sections = {}
        current_section = None
        current_content = []
        
        for line in code.split('\n'):
            if '# ===' in line and '.rb ===' in line:
                if current_section:
                    sections[current_section] = '\n'.join(current_content)
                
                if 'controller' in line.lower() and 'spec' not in line.lower():
                    current_section = 'controller'
                elif 'route' in line.lower():
                    current_section = 'routes'
                elif 'spec' in line.lower() or 'test' in line.lower():
                    current_section = 'tests'
                else:
                    current_section = 'other'
                
                current_content = []
            else:
                current_content.append(line)
        
        if current_section:
            sections[current_section] = '\n'.join(current_content)
        
        return {
            "success": True,
            "full_code": code,
            **sections
        }


class ServiceObjectGenerator(BaseGenerator):
    """Generate service objects following Rails patterns"""
    
    def __init__(self):
        super().__init__("service object")
    
    def generate(self, description: str, **options) -> Dict[str, Any]:
        """Generate service object pattern"""
        prompt = f"""Generate a Rails service object for: {description}

Include:
1. Service class with:
   - Single public method (call or perform)
   - Clear input validation
   - Error handling with Result object pattern
   - Private methods for steps
2. Result object for success/failure handling
3. Full RSpec tests with:
   - Happy path tests
   - Error cases
   - Mocking external dependencies
4. Usage example in a controller

Follow Rails service object best practices.
Separate files with: # === filename.rb ==="""
        
        return self._generate_with_ai(prompt)


class FormObjectGenerator(BaseGenerator):
    """Generate form objects for complex forms"""
    
    def __init__(self):
        super().__init__("form object")
    
    def generate(self, description: str, model: str, **options) -> Dict[str, Any]:
        """Generate form object pattern"""
        prompt = f"""Generate a Rails form object for: {description}
Primary model: {model}

Include:
1. Form object class with:
   - ActiveModel::Model inclusion
   - All necessary attributes
   - Validations matching business rules
   - Type casting for attributes
   - Save method with transaction
   - Nested attributes support if needed
2. Integration with Rails form helpers
3. RSpec tests covering:
   - Valid and invalid cases
   - Nested attributes
   - Transaction rollback on failure
4. Controller usage example

Separate files with: # === filename.rb ==="""
        
        return self._generate_with_ai(prompt)


class ApiDocsGenerator(BaseGenerator):
    """Generate API documentation"""
    
    def __init__(self):
        super().__init__("API documentation")
    
    def generate(self, controllers: List[str], format: str = "OpenAPI", **options) -> Dict[str, Any]:
        """Generate API documentation in specified format"""
        prompt = f"""Generate {format} documentation for Rails API.
Controllers to document: {', '.join(controllers)}

Include:
1. Complete API specification
2. All endpoints with:
   - HTTP methods and paths
   - Request/response schemas
   - Parameters and validations
   - Authentication requirements
   - Example requests/responses
3. Error responses
4. {"Swagger UI integration" if format == "OpenAPI" else "Documentation format"}

Follow {format} specification standards."""
        
        return self._generate_with_ai(prompt)