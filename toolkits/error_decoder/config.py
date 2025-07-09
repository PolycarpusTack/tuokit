# toolkits/error_decoder/config.py
"""Configuration constants for Error Decoder"""

# Default models - dynamic selection
def get_error_decoder_models():
    """Get appropriate models for error decoding"""
    from utils.model_manager import ModelManager
    from utils.ollama import get_available_models
    
    default = ModelManager.get_model_for_task("analysis")
    available = get_available_models()
    
    # Build fallback list from available models
    fallbacks = []
    for model in available:
        if model != default and any(keyword in model.lower() for keyword in ["deepseek", "llama", "mistral"]):
            fallbacks.append(model)
            if len(fallbacks) >= 2:
                break
    
    return default, fallbacks or [default]

DEFAULT_MODEL, FALLBACK_MODELS = get_error_decoder_models()

# UI Configuration
ERROR_INPUT_HEIGHT = 200
CODE_INPUT_HEIGHT = 300
MAX_RECENT_ERRORS = 5
MAX_ERROR_STATISTICS = 5

# Analysis depth options
ANALYSIS_DEPTHS = ["Quick", "Standard", "Deep"]
DEFAULT_DEPTH = "Standard"

# Language options
LANGUAGES = ["Auto-detect", "Python", "JavaScript", "Java", "C++", "Ruby", "Rails", "SmallTalk"]

# Language to code editor mapping
LANG_MAP = {
    "Auto-detect": "text",
    "Python": "python",
    "Ruby": "ruby",
    "Rails": "ruby",
    "SmallTalk": "smalltalk",
    "JavaScript": "javascript",
    "Java": "java",
    "C++": "cpp"
}

# Example errors by language
EXAMPLE_ERRORS = {
    "SmallTalk": [
        "MessageNotUnderstood: Array>>doesNotExist",
        "SubscriptOutOfBounds: 'Accessing index 5 of 3-element array'",
        "ZeroDivide: '5 / 0'",
        "ObjectNotFound: 'Non-existent object reference'"
    ],
    "Ruby": [
        "NoMethodError: undefined method 'name' for nil:NilClass",
        "ActiveRecord::RecordNotFound: Couldn't find User with 'id'=999",
        "SyntaxError: unexpected end-of-input",
        "NameError: uninitialized constant MyController"
    ],
    "Rails": [
        "NoMethodError: undefined method 'name' for nil:NilClass",
        "ActiveRecord::RecordNotFound: Couldn't find User with 'id'=999",
        "SyntaxError: unexpected end-of-input", 
        "NameError: uninitialized constant MyController"
    ],
    "default": [
        "ValueError: invalid literal for int() with base 10: 'abc'",
        "TypeError: Cannot read properties of undefined (reading 'name')",
        "NullPointerException: Attempt to invoke virtual method on null object",
        "IndexError: list index out of range"
    ]
}

# Error parsing patterns
ERROR_PATTERNS = {
    "python": r"File \"(.+?)\", line (\d+).*\n(\w+Error): (.+)",
    "javascript": r"at (.+?) \((.+?):(\d+):\d+\)\n(\w+Error): (.+)",
    "java": r"at (.+?)\((.+?):(\d+)\)\n(\w+Exception): (.+)",
    "c++": r"\((.+?):(\d+)\): error (.+?): (.+)",
    "ruby": r"(.+?):(\d+):in `(.+)': (.+) \((.+)\)",
    "rails": r"Completed (\d{3}) .* in (\d+ms).*\n\n(.+?) \(([\w:]+)\):\n\n(.+?):(\d+):in `(.+)'",
    "smalltalk": r"\[(.*?)\]: (.*?) (Error|Exception): (.*?)\nReceiver: (.*?)\nArguments: (.*?)\n(.*)",
    "generic": r"(\w+Error|\w+Exception): (.+)"
}