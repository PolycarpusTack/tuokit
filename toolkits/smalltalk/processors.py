# toolkits/smalltalk/processors.py
"""Processing functions for SmallTalk code analysis and transformation"""

import re
from utils import safe_ollama_generate

def explain_code(code: str, detail_level: str = "Detailed", 
                focus_areas: list = None, model: str = "deepseek-coder:6.7b") -> dict:
    """Explain SmallTalk code with varying detail levels"""
    
    focus_str = ""
    if focus_areas:
        focus_str = f"Focus especially on: {', '.join(focus_areas)}"
    
    prompt = f"""Explain this SmallTalk code:

```smalltalk
{code}
```

Detail level: {detail_level}
{focus_str}

Provide:
1. Overall purpose and design
2. Class structure and relationships
3. Method-by-method breakdown
4. Key SmallTalk idioms used
5. Potential improvements
6. {'Advanced concepts and patterns' if detail_level == 'Expert' else 'Usage examples'}"""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.2,
        system="You are a SmallTalk expert teacher. Explain code clearly for the given detail level."
    )
    
    return result

def refactor_code(code: str, technique: str, 
                 preserve_behavior: bool = True, model: str = "deepseek-coder:6.7b") -> dict:
    """Refactor SmallTalk code using specific technique"""
    
    prompt = f"""Refactor this SmallTalk code using '{technique}':

```smalltalk
{code}
```

Requirements:
1. Apply the refactoring technique correctly
2. {'Preserve exact behavior' if preserve_behavior else 'Improve behavior if possible'}
3. Follow SmallTalk best practices
4. Add comments explaining changes
5. Ensure code remains readable

Provide:
- Refactored code
- Summary of changes
- Benefits of the refactoring"""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.2,
        system="You are a SmallTalk refactoring expert. Apply refactoring patterns correctly and safely."
    )
    
    return result

def convert_code(code: str, direction: str, 
                preserve_style: bool = True, model: str = "deepseek-coder:6.7b") -> dict:
    """Convert between SmallTalk and Ruby"""
    
    if direction == "SmallTalk to Ruby":
        source, target = "SmallTalk", "Ruby"
        guidelines = """
- Convert blocks to Ruby blocks/procs
- Map collection methods appropriately
- Handle message sends vs method calls
- Convert class definitions properly
- Preserve object-oriented structure"""
    else:
        source, target = "Ruby", "SmallTalk"
        guidelines = """
- Convert Ruby modules to SmallTalk protocols
- Map Ruby blocks to SmallTalk blocks
- Handle attr_accessor/reader/writer
- Convert exception handling
- Preserve dynamic features"""
    
    prompt = f"""Convert this {source} code to {target}:

```{source.lower()}
{code}
```

Conversion guidelines:
{guidelines}

Requirements:
1. Maintain functionality
2. Use idiomatic {target} patterns
3. Include comments for non-obvious conversions
4. {'Preserve coding style' if preserve_style else 'Use target language best practices'}
5. Note any feature incompatibilities

Provide complete, working {target} code."""

    result = safe_ollama_generate(
        model=model,
        prompt=prompt,
        temperature=0.2,
        system=f"You are an expert in both {source} and {target}. Create accurate, idiomatic conversions."
    )
    
    return result

def extract_class_info(code: str) -> dict:
    """Extract class information from SmallTalk code"""
    info = {
        "classes": [],
        "methods": [],
        "instance_vars": [],
        "class_vars": []
    }
    
    # Extract class definitions
    class_pattern = r'(\w+)\s+subclass:\s*#(\w+)'
    for match in re.finditer(class_pattern, code):
        info["classes"].append({
            "superclass": match.group(1),
            "name": match.group(2)
        })
    
    # Extract method definitions
    method_pattern = r'(\w+)\s*>>\s*([+\-\w]+[:\w\s]*)'
    for match in re.finditer(method_pattern, code):
        info["methods"].append({
            "class": match.group(1),
            "selector": match.group(2).strip()
        })
    
    # Extract instance variables
    inst_var_pattern = r'instanceVariableNames:\s*[\'"]([^\'\"]*)[\'"]'
    for match in re.finditer(inst_var_pattern, code):
        vars = match.group(1).split()
        info["instance_vars"].extend(vars)
    
    # Extract class variables
    class_var_pattern = r'classVariableNames:\s*[\'"]([^\'\"]*)[\'"]'
    for match in re.finditer(class_var_pattern, code):
        vars = match.group(1).split()
        info["class_vars"].extend(vars)
    
    return info