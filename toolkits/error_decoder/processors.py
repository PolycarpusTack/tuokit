# toolkits/error_decoder/processors.py
"""Core processing logic for error analysis and fix generation"""

import re
import json
from utils import safe_ollama_generate
from .knowledge_base import LANGUAGE_GUIDANCE

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
    
    # Build context-aware prompt
    prompt = f"Language: {error_data['language'].capitalize()}\n"
    if error_data['language'] in LANGUAGE_GUIDANCE:
        prompt += f"Note: {LANGUAGE_GUIDANCE[error_data['language']]}\n"
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

def generate_prevention_strategies(error_type, language, model="deepseek-coder:6.7b"):
    """Generate prevention strategies for specific error type"""
    prompt = f"Create a prevention checklist for {error_type} errors in {language}"
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="Output as markdown checklist with 5-7 items"
    )
    
    if 'error' in response:
        return "Unable to generate prevention strategies"
    
    return response.get('response', '')

def get_educational_content(error_type, language, model="deepseek-coder:6.7b"):
    """Get structured educational content for error"""
    from .knowledge_base import EDUCATIONAL_CONTENT
    
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

def analyze_error_patterns(error_type, model="deepseek-coder:6.7b"):
    """Detect patterns in specific error types"""
    prompt = f"Identify patterns in {error_type} errors"
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="List common patterns and triggers"
    )
    
    if 'error' in response:
        return "Unable to analyze patterns"
    
    return response.get('response', '')

def get_common_misconceptions(error_type, language, model="deepseek-coder:6.7b"):
    """Get common misconceptions about error type"""
    prompt = f"What are common misconceptions about {error_type} in {language}?"
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="List 3-5 common misconceptions with explanations"
    )
    
    if 'error' in response:
        return "Unable to retrieve misconceptions"
    
    return response.get('response', '')

def get_historical_context(error_type, language, model="deepseek-coder:6.7b"):
    """Get historical context for error type"""
    prompt = f"Explain the historical origin of {error_type} in {language}"
    
    response = safe_ollama_generate(
        model=model,
        prompt=prompt,
        system="Provide historical context in 2-3 paragraphs"
    )
    
    if 'error' in response:
        return "Unable to retrieve historical context"
    
    return response.get('response', '')