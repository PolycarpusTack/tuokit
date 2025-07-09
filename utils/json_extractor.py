# utils/json_extractor.py
"""
Centralized JSON extraction utility for TuoKit
Handles common JSON parsing patterns across the codebase
"""

import json
import re
from typing import Dict, Any, Optional, List

def extract_json_from_text(text: str) -> Optional[Dict[str, Any]]:
    """
    Extract the first valid JSON object from text.
    Handles cases where JSON is embedded in other content.
    
    Args:
        text: Text that may contain JSON
        
    Returns:
        Parsed JSON dict or None if no valid JSON found
    """
    if not text:
        return None
    
    # Try direct parsing first (fastest)
    try:
        return json.loads(text.strip())
    except json.JSONDecodeError:
        pass
    
    # Try to find JSON object pattern
    json_pattern = r'\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}'
    matches = re.finditer(json_pattern, text, re.DOTALL)
    
    for match in matches:
        try:
            return json.loads(match.group(0))
        except json.JSONDecodeError:
            continue
    
    return None

def extract_all_json_from_text(text: str) -> List[Dict[str, Any]]:
    """
    Extract all valid JSON objects from text.
    
    Args:
        text: Text that may contain multiple JSON objects
        
    Returns:
        List of parsed JSON dicts
    """
    if not text:
        return []
    
    results = []
    
    # Try to find all JSON object patterns
    json_pattern = r'\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}'
    matches = re.finditer(json_pattern, text, re.DOTALL)
    
    for match in matches:
        try:
            obj = json.loads(match.group(0))
            results.append(obj)
        except json.JSONDecodeError:
            continue
    
    return results

def extract_json_between_markers(text: str, start_marker: str = "```json", 
                               end_marker: str = "```") -> Optional[Dict[str, Any]]:
    """
    Extract JSON from text between markers (e.g., markdown code blocks).
    
    Args:
        text: Text containing JSON between markers
        start_marker: Start delimiter
        end_marker: End delimiter
        
    Returns:
        Parsed JSON dict or None
    """
    if not text or start_marker not in text:
        return None
    
    try:
        start_idx = text.find(start_marker) + len(start_marker)
        end_idx = text.find(end_marker, start_idx)
        
        if end_idx == -1:
            json_text = text[start_idx:].strip()
        else:
            json_text = text[start_idx:end_idx].strip()
        
        return json.loads(json_text)
    except (json.JSONDecodeError, ValueError):
        return None

def safe_get_value(data: Optional[Dict[str, Any]], key: str, default: Any = None) -> Any:
    """
    Safely get a value from a possibly None dict.
    
    Args:
        data: Dictionary or None
        key: Key to retrieve
        default: Default value if not found
        
    Returns:
        Value or default
    """
    if data is None or not isinstance(data, dict):
        return default
    return data.get(key, default)

def extract_crash_info(response: str) -> Dict[str, Any]:
    """
    Extract crash analysis information from various response formats.
    Handles both JSON and text-based responses.
    
    Args:
        response: Analysis response text
        
    Returns:
        Dictionary with extracted information
    """
    # Try JSON extraction first
    json_data = extract_json_from_text(response)
    
    if json_data:
        return json_data
    
    # Fallback to text parsing for common patterns
    result = {}
    
    # Extract severity
    severity_match = re.search(r'Severity:\s*(\w+)', response, re.IGNORECASE)
    if severity_match:
        result['severity'] = severity_match.group(1).upper()
    
    # Extract error type
    error_match = re.search(r'Error Type:\s*([^\n]+)', response, re.IGNORECASE)
    if error_match:
        result['error_type'] = error_match.group(1).strip()
    
    # Extract summary
    summary_match = re.search(r'Summary:\s*([^\n]+)', response, re.IGNORECASE)
    if summary_match:
        result['summary'] = summary_match.group(1).strip()
    
    # If we found any data, include the raw response
    if result:
        result['raw_response'] = response
    
    return result