# toolkits/smalltalk/generators.py
"""Generator functions for SmallTalk code generation"""

from utils import safe_ollama_generate

def generate_class(description: str, include_tests: bool = False,
                  include_examples: bool = False, model: str = "deepseek-coder:6.7b") -> dict:
    """Generate SmallTalk class with optional tests and examples"""
    
    prompt = f"""Generate a SmallTalk class based on: {description}

Requirements:
1. Follow SmallTalk naming conventions (PascalCase)
2. Include proper class and method comments
3. Use meaningful variable names
4. Follow MVC pattern if UI-related

Structure:
- Class definition with instance/class variables
- Initialize method
- Core functionality methods
- Accessor methods if needed
{'- Comprehensive SUnit test class' if include_tests else ''}
{'- Usage examples in comments' if include_examples else ''}

Return complete, working SmallTalk code."""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.3,
        system="You are a SmallTalk expert. Generate clean, idiomatic SmallTalk code following best practices."
    )
    
    return result

def generate_snippet(category: str, complexity: str, 
                    specific_task: str = "", model: str = "deepseek-coder:6.7b") -> dict:
    """Generate code snippet for specific category and complexity"""
    
    prompt = f"""Generate a SmallTalk code snippet for:
Category: {category}
Complexity: {complexity}
{f'Specific task: {specific_task}' if specific_task else ''}

Requirements:
1. Working, practical code
2. Clear comments explaining the approach
3. Handle edge cases
4. Follow SmallTalk idioms
5. Include any necessary imports/dependencies

Format:
- Method or code block
- Usage example
- Expected output/behavior"""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.3,
        system="You are a SmallTalk expert. Generate practical, well-commented code snippets."
    )
    
    return result

def generate_seaside_component(description: str, component_type: str = "Component",
                             include_css: bool = True, model: str = "deepseek-coder:6.7b") -> dict:
    """Generate Seaside web component"""
    
    prompt = f"""Create a Seaside {component_type} for: {description}

Requirements:
1. Proper Seaside component structure
2. renderContentOn: method implementation
3. State management if needed
4. Callbacks and actions
{'5. CSS styling methods' if include_css else ''}
6. Component registration code

Include:
- Complete component class
- Required methods
- Usage instructions
- Registration snippet"""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.3,
        system="You are a Seaside framework expert. Generate production-ready Seaside components."
    )
    
    return result

def generate_metaprogramming(code: str, task: str, 
                           target_class: str = "", model: str = "deepseek-coder:6.7b") -> dict:
    """Generate metaprogramming solution"""
    
    # Fix the f-string issue by using proper concatenation
    code_section = ""
    if code:
        code_section = f"Target class/code:\\n```smalltalk\\n{code}\\n```"
    
    target_section = ""
    if target_class and not code:
        target_section = f"Target class name: {target_class}"
    
    prompt = f"""Perform this SmallTalk metaprogramming task: {task}

{code_section}
{target_section}

Use SmallTalk metaprogramming capabilities:
- Runtime class/method creation
- Method compilation with Compiler
- Reflection APIs (thisContext, etc.)
- Method wrappers and proxies
- Dynamic method dispatch

Provide:
1. Complete working code
2. Clear comments explaining the metaprogramming
3. Example usage
4. Important considerations

Follow VisualWorks SmallTalk conventions."""
    
    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.1,
        system="You are a SmallTalk metaprogramming expert. Generate advanced but practical metaprogramming solutions."
    )
    
    return result