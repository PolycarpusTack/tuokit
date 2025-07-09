# toolkits/error_decoder/parsers.py
"""Error parsing logic for multiple languages"""

import re
from .config import ERROR_PATTERNS

def parse_error_message(error):
    """Enhanced error parsing with SmallTalk and Ruby support"""
    result = {
        "language": "unknown",
        "error_type": "Unknown",
        "message": "",
        "file": "",
        "line": 0,
        "context": ""
    }
    
    # Try language-specific patterns first
    for lang, pattern in ERROR_PATTERNS.items():
        match = re.search(pattern, error, re.DOTALL)
        if match:
            result["language"] = lang
            try:
                if lang == "python":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["error_type"] = match.group(3)
                    result["message"] = match.group(4)
                elif lang == "javascript":
                    result["context"] = match.group(1)
                    result["file"] = match.group(2)
                    result["line"] = int(match.group(3))
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(5)
                elif lang == "java":
                    result["context"] = match.group(1)
                    result["file"] = match.group(2)
                    result["line"] = int(match.group(3))
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(5)
                elif lang == "c++":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["error_type"] = match.group(3)
                    result["message"] = match.group(4)
                elif lang == "ruby":
                    result["file"] = match.group(1)
                    result["line"] = int(match.group(2))
                    result["context"] = match.group(3)
                    result["error_type"] = match.group(5)
                    result["message"] = match.group(4)
                elif lang == "rails":
                    result["status"] = match.group(1)
                    result["time"] = match.group(2)
                    result["error_type"] = match.group(4)
                    result["message"] = match.group(3)
                    result["file"] = match.group(5)
                    result["line"] = int(match.group(6))
                    result["context"] = match.group(7)
                elif lang == "smalltalk":
                    result["process"] = match.group(1)
                    result["timestamp"] = match.group(2)
                    result["error_type"] = match.group(4)
                    result["receiver"] = match.group(5)
                    result["arguments"] = match.group(6)
                    result["stack"] = match.group(7)
                    result["message"] = f"{match.group(4)}: {match.group(4)}"
                else:  # generic
                    result["error_type"] = match.group(1)
                    result["message"] = match.group(2)
            except (IndexError, ValueError):
                # Fallback to generic if pattern doesn't match fully
                result["error_type"] = "ParseError"
                result["message"] = error
            break
    
    return result

def get_error_statistics(db):
    """Get error frequency statistics from database"""
    try:
        # Get recent queries from error decoder tool
        recent = db.get_recent_queries(limit=100)
        error_queries = [q for q in recent if q[1] == "error_decoder"]
        
        if error_queries:
            # Count error types
            error_counts = {}
            for query in error_queries:
                parsed = parse_error_message(query[3])
                error_type = parsed["error_type"]
                if error_type != "Unknown":
                    error_counts[error_type] = error_counts.get(error_type, 0) + 1
            
            # Return sorted statistics
            return sorted(error_counts.items(), key=lambda x: x[1], reverse=True)[:5]
        return []
    except Exception:
        return []

def extract_error_context(error_data, user_code=""):
    """Extract relevant context for error analysis"""
    context = {
        "language": error_data['language'].capitalize(),
        "error_type": error_data['error_type'],
        "message": error_data['message'],
        "has_file": bool(error_data.get('file')),
        "has_line": error_data.get('line', 0) > 0,
        "has_code": bool(user_code.strip())
    }
    
    # Add language-specific context
    if error_data['language'] == "smalltalk":
        context.update({
            "receiver": error_data.get('receiver', ''),
            "arguments": error_data.get('arguments', ''),
            "process": error_data.get('process', '')
        })
    elif error_data['language'] == "rails":
        context.update({
            "status": error_data.get('status', ''),
            "time": error_data.get('time', '')
        })
    
    return context