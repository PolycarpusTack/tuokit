# toolkits/smalltalk/config.py
"""Configuration for SmallTalk Toolkit"""

# Default models - dynamic selection
def get_smalltalk_models():
    """Get appropriate models for SmallTalk code generation"""
    from utils.model_manager import ModelManager
    from utils.ollama import get_available_models
    
    default = ModelManager.get_model_for_task("code")
    available = get_available_models()
    
    # Build fallback list from available models
    fallbacks = []
    for model in available:
        if model != default and any(keyword in model.lower() for keyword in ["deepseek", "coder", "llama"]):
            fallbacks.append(model)
            if len(fallbacks) >= 2:
                break
    
    return default, fallbacks or [default]

DEFAULT_MODEL, FALLBACK_MODELS = get_smalltalk_models()

# Tool categories for main menu
TOOL_CATEGORIES = {
    "Class Generator": {
        "icon": "🏗️",
        "description": "Generate SmallTalk classes with tests and documentation",
        "key": "class_generator"
    },
    "Code Explainer": {
        "icon": "📖",
        "description": "Explain SmallTalk code with detailed analysis",
        "key": "code_explainer"
    },
    "Snippet Generator": {
        "icon": "✨",
        "description": "Generate code snippets for common tasks",
        "key": "snippet_generator"
    },
    "Code Refactorer": {
        "icon": "🔧",
        "description": "Refactor SmallTalk code using best practices",
        "key": "refactorer"
    },
    "Ruby Converter": {
        "icon": "💎",
        "description": "Convert between SmallTalk and Ruby",
        "key": "ruby_converter"
    },
    "Seaside Generator": {
        "icon": "🌊",
        "description": "Generate Seaside web components",
        "key": "seaside_generator"
    },
    "Metaprogramming": {
        "icon": "🧙",
        "description": "Advanced metaprogramming solutions",
        "key": "metaprogramming"
    }
}

# Snippet categories
SNIPPET_CATEGORIES = {
    "Collections & Iteration": {
        "icon": "📚",
        "description": "Arrays, OrderedCollections, Sets, Dictionaries",
        "subcategories": ["Basic Operations", "Advanced Iteration", "Collection Conversion"]
    },
    "GUI Development (MVC)": {
        "icon": "🎨",
        "description": "Morphic UI, MVC patterns, Event handling",
        "subcategories": ["Basic Windows", "Custom Widgets", "Event Handling"]
    },
    "File I/O": {
        "icon": "📁",
        "description": "File reading, writing, and stream operations",
        "subcategories": ["Text Files", "Binary Files", "Directory Operations"]
    },
    "Database Access": {
        "icon": "🗄️",
        "description": "Glorp ORM, SQL connectivity, Persistence",
        "subcategories": ["Basic Queries", "Transactions", "Schema Management"]
    },
    "Testing & Debugging": {
        "icon": "🧪",
        "description": "SUnit tests, Debugging tools, Assertions",
        "subcategories": ["Unit Tests", "Mock Objects", "Performance Testing"]
    },
    "String Manipulation": {
        "icon": "✏️",
        "description": "String operations, parsing, formatting",
        "subcategories": ["Basic Operations", "Regular Expressions", "Text Processing"]
    },
    "Date & Time": {
        "icon": "🕐",
        "description": "Date arithmetic, formatting, time zones",
        "subcategories": ["Basic Operations", "Formatting", "Time Calculations"]
    },
    "Network & HTTP": {
        "icon": "🌐",
        "description": "HTTP requests, sockets, web services",
        "subcategories": ["HTTP Client", "Socket Programming", "REST APIs"]
    },
    "Exception Handling": {
        "icon": "⚠️",
        "description": "Error handling, custom exceptions, recovery",
        "subcategories": ["Basic Try-Catch", "Custom Exceptions", "Error Recovery"]
    },
    "Design Patterns": {
        "icon": "🏗️",
        "description": "Common OOP patterns in SmallTalk",
        "subcategories": ["Creational", "Structural", "Behavioral"]
    }
}

# Refactoring techniques
REFACTORING_TECHNIQUES = {
    "Extract Method": {
        "description": "Extract code into a new method",
        "example": "Turn repeated code into a reusable method"
    },
    "Rename Variable": {
        "description": "Give variables more meaningful names",
        "example": "Change 'x' to 'customerAge'"
    },
    "Introduce Parameter": {
        "description": "Replace hardcoded values with parameters",
        "example": "Make methods more flexible"
    },
    "Replace Conditional with Polymorphism": {
        "description": "Use object polymorphism instead of if/case",
        "example": "Replace type checks with method dispatch"
    },
    "Extract Class": {
        "description": "Move cohesive methods to a new class",
        "example": "Split large classes into focused ones"
    },
    "Inline Method": {
        "description": "Replace method call with method body",
        "example": "Remove unnecessary indirection"
    },
    "Move Method": {
        "description": "Move method to a more appropriate class",
        "example": "Improve class cohesion"
    },
    "Replace Temp with Query": {
        "description": "Replace temporary variable with method",
        "example": "Improve readability and reusability"
    }
}

# Explanation detail levels
DETAIL_LEVELS = ["Basic", "Detailed", "Expert"]

# Code conversion directions
CONVERSION_DIRECTIONS = ["SmallTalk to Ruby", "Ruby to SmallTalk"]

# Seaside component types
SEASIDE_TYPES = ["Component", "Task", "Decoration", "Custom"]

# Complexity levels
COMPLEXITY_LEVELS = ["Basic", "Intermediate", "Advanced"]