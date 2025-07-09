"""
Annotated Text Helpers for TuoKit
Provides color-coded annotations for better readability of error messages, 
stack traces, and code analysis using streamlit-extras.
"""

from typing import List, Tuple, Optional, Union
import re
from streamlit_extras.annotated_text import annotated_text

# Color scheme for different annotation types
ANNOTATION_COLORS = {
    # Error-related colors
    "error": "#ff4444",          # Red for errors
    "error_type": "#ff6666",     # Light red for error types
    "error_class": "#ff8800",    # Orange for error classes
    "warning": "#ffaa00",        # Yellow for warnings
    "info": "#4444ff",           # Blue for info
    
    # Code-related colors
    "line_number": "#0088ff",    # Light blue for line numbers
    "file_path": "#00aa88",      # Teal for file paths
    "function": "#aa00ff",       # Purple for function names
    "variable": "#ff00aa",       # Pink for variables
    "keyword": "#00aaff",        # Cyan for keywords
    
    # SQL-specific colors
    "sql_keyword": "#0066cc",    # Dark blue for SQL keywords
    "table_name": "#009900",     # Green for table names
    "column_name": "#cc6600",    # Brown for column names
    
    # Severity levels
    "critical": "#ff0000",       # Bright red
    "high": "#ff6600",           # Orange-red
    "medium": "#ffaa00",         # Yellow-orange
    "low": "#00aa00",            # Green
    
    # General
    "highlight": "#ffff00",      # Yellow highlight
    "success": "#00ff00",        # Bright green
    "neutral": "#888888",        # Gray
}

def parse_error_message(error_text: str) -> List[Union[str, Tuple[str, str, str]]]:
    """
    Parse error message and return annotated components
    
    Args:
        error_text: The error message to parse
        
    Returns:
        List of strings and tuples for annotated_text
    """
    components = []
    
    # Common error patterns
    patterns = {
        r'(TypeError|ValueError|AttributeError|NameError|SyntaxError|RuntimeError)': 'error_type',
        r'(nil:NilClass|undefined method|undefined variable)': 'error_class',
        r'line (\d+)': 'line_number',
        r'(\/.+?\.\w+)': 'file_path',
        r'in `([^`]+)`': 'function',
    }
    
    # Split text and find matches
    last_end = 0
    matches = []
    
    for pattern, annotation_type in patterns.items():
        for match in re.finditer(pattern, error_text):
            matches.append((match.start(), match.end(), match.group(), annotation_type))
    
    # Sort matches by position
    matches.sort(key=lambda x: x[0])
    
    # Build components
    for start, end, text, annotation_type in matches:
        # Add text before match
        if start > last_end:
            components.append(error_text[last_end:start])
        
        # Add annotated match
        components.append((text, annotation_type, ANNOTATION_COLORS[annotation_type]))
        last_end = end
    
    # Add remaining text
    if last_end < len(error_text):
        components.append(error_text[last_end:])
    
    # If no patterns matched, return original text
    if not components:
        components = [error_text]
    
    return components

def parse_stack_trace(stack_trace: str) -> List[List[Union[str, Tuple[str, str, str]]]]:
    """
    Parse stack trace and return annotated lines
    
    Args:
        stack_trace: The stack trace to parse
        
    Returns:
        List of annotated line components
    """
    lines = stack_trace.strip().split('\n')
    parsed_lines = []
    
    for line in lines:
        if not line.strip():
            continue
            
        components = []
        
        # Check if it's a file/line reference
        file_match = re.match(r'^(\s*)(.*?)(:)(\d+)(:in\s+)?(.*)$', line)
        if file_match:
            indent, file_path, colon1, line_num, in_part, rest = file_match.groups()
            components.append(indent)
            components.append((file_path, "file_path", ANNOTATION_COLORS["file_path"]))
            components.append(colon1)
            components.append((line_num, "line_number", ANNOTATION_COLORS["line_number"]))
            if in_part:
                components.append(in_part)
            
            # Parse function name
            func_match = re.match(r"`([^']+)'(.*)$", rest)
            if func_match:
                components.append("`")
                components.append((func_match.group(1), "function", ANNOTATION_COLORS["function"]))
                components.append("'" + func_match.group(2))
            else:
                components.append(rest)
        else:
            # Check for error type at start of line
            error_match = re.match(r'^(\s*)(TypeError|ValueError|AttributeError|NameError|SyntaxError|RuntimeError):\s*(.*)$', line)
            if error_match:
                indent, error_type, message = error_match.groups()
                components.append(indent)
                components.append((error_type + ":", "error_type", ANNOTATION_COLORS["error_type"]))
                components.append(" ")
                # Parse the error message
                components.extend(parse_error_message(message))
            else:
                components.append(line)
        
        parsed_lines.append(components)
    
    return parsed_lines

def annotate_sql_query(sql_text: str) -> List[Union[str, Tuple[str, str, str]]]:
    """
    Parse SQL query and return annotated components
    
    Args:
        sql_text: The SQL query to parse
        
    Returns:
        List of strings and tuples for annotated_text
    """
    # SQL keywords
    sql_keywords = {
        'SELECT', 'FROM', 'WHERE', 'JOIN', 'LEFT', 'RIGHT', 'INNER', 'OUTER',
        'ON', 'AS', 'AND', 'OR', 'NOT', 'IN', 'EXISTS', 'BETWEEN', 'LIKE',
        'GROUP', 'BY', 'HAVING', 'ORDER', 'LIMIT', 'OFFSET', 'UNION', 'ALL',
        'INSERT', 'INTO', 'VALUES', 'UPDATE', 'SET', 'DELETE', 'CREATE',
        'TABLE', 'INDEX', 'VIEW', 'DROP', 'ALTER', 'ADD', 'COLUMN', 'CONSTRAINT',
        'PRIMARY', 'KEY', 'FOREIGN', 'REFERENCES', 'UNIQUE', 'NULL', 'DEFAULT',
        'CASE', 'WHEN', 'THEN', 'ELSE', 'END', 'DISTINCT', 'COUNT', 'SUM',
        'AVG', 'MIN', 'MAX', 'CAST', 'COALESCE', 'WITH'
    }
    
    components = []
    
    # Simple tokenization (can be improved)
    tokens = re.findall(r'\b\w+\b|[^\w\s]', sql_text)
    
    for i, token in enumerate(tokens):
        if token.upper() in sql_keywords:
            components.append((token, "sql_keyword", ANNOTATION_COLORS["sql_keyword"]))
        elif re.match(r'^\d+$', token):
            components.append(token)
        elif i > 0 and tokens[i-1].upper() in ['FROM', 'JOIN', 'INTO', 'UPDATE', 'TABLE']:
            components.append((token, "table_name", ANNOTATION_COLORS["table_name"]))
        else:
            components.append(token)
        
        # Add space after token if not punctuation
        if i < len(tokens) - 1 and not re.match(r'^[,;.]$', tokens[i+1]):
            components.append(" ")
    
    return components

def highlight_severity(severity: str) -> Tuple[str, str, str]:
    """
    Return annotated severity level
    
    Args:
        severity: Severity level (CRITICAL, HIGH, MEDIUM, LOW)
        
    Returns:
        Tuple for annotated_text
    """
    severity_upper = severity.upper()
    if severity_upper in ["CRITICAL", "HIGH", "MEDIUM", "LOW"]:
        return (severity_upper, "severity", ANNOTATION_COLORS[severity_upper.lower()])
    return (severity, "severity", ANNOTATION_COLORS["neutral"])

def create_code_snippet_annotation(code: str, error_line: Optional[int] = None) -> List[List[Union[str, Tuple[str, str, str]]]]:
    """
    Create annotated code snippet with optional error line highlighting
    
    Args:
        code: Code snippet
        error_line: Line number with error (1-based)
        
    Returns:
        List of annotated line components
    """
    lines = code.strip().split('\n')
    annotated_lines = []
    
    for i, line in enumerate(lines, 1):
        components = []
        
        # Line number
        line_num_str = f"{i:4d} "
        if i == error_line:
            components.append((line_num_str, "error_line", ANNOTATION_COLORS["error"]))
        else:
            components.append((line_num_str, "line_num", ANNOTATION_COLORS["neutral"]))
        
        # Code content
        if i == error_line:
            components.append((line, "error_code", ANNOTATION_COLORS["highlight"]))
        else:
            components.append(line)
        
        annotated_lines.append(components)
    
    return annotated_lines

def display_annotated_error(error_data: dict):
    """
    Display a complete annotated error with all components
    
    Args:
        error_data: Dictionary containing error information
            - error_type: Type of error
            - message: Error message
            - file: File path
            - line: Line number
            - stack_trace: Optional stack trace
            - severity: Optional severity level
    """
    from streamlit_extras.annotated_text import annotated_text
    import streamlit as st
    
    # Error header
    components = []
    
    if error_data.get('severity'):
        components.append(highlight_severity(error_data['severity']))
        components.append(" ")
    
    if error_data.get('error_type'):
        components.append((error_data['error_type'], "error_type", ANNOTATION_COLORS["error_type"]))
        components.append(": ")
    
    if error_data.get('message'):
        components.extend(parse_error_message(error_data['message']))
    
    if components:
        annotated_text(*components)
    
    # File and line info
    if error_data.get('file') or error_data.get('line'):
        file_components = []
        if error_data.get('file'):
            file_components.append("in ")
            file_components.append((error_data['file'], "file_path", ANNOTATION_COLORS["file_path"]))
        if error_data.get('line'):
            file_components.append(" at line ")
            file_components.append((str(error_data['line']), "line_number", ANNOTATION_COLORS["line_number"]))
        
        if file_components:
            annotated_text(*file_components)
    
    # Stack trace
    if error_data.get('stack_trace'):
        st.text("Stack Trace:")
        parsed_stack = parse_stack_trace(error_data['stack_trace'])
        for line_components in parsed_stack:
            if line_components:
                annotated_text(*line_components)