"""
What's On Crash Report (WCR) Handler
Handles Smalltalk/VisualWorks specific crash dumps
"""
from typing import Dict, Any, Optional

def is_wcr_format(content: str) -> bool:
    """
    Check if the crash dump is in WCR format
    
    Args:
        content: Crash dump content
        
    Returns:
        True if this is a WCR file
    """
    return "BEGIN RUNTIME DIAGNOSTIC DUMP" in content[:200]

def extract_wcr_metadata(content: str) -> Dict[str, Any]:
    """
    Extract metadata from WCR crash dump
    
    Args:
        content: WCR crash dump content
        
    Returns:
        Dictionary with WCR metadata
    """
    metadata = {
        'format': 'WCR',
        'timestamp': '',
        'version': '',
        'site': '',
        'user': '',
        'oracle_version': '',
        'computer': '',
        'build': ''
    }
    
    # Extract key fields using simple parsing
    lines = content.splitlines()[:50]  # Check first 50 lines
    
    for line in lines:
        if 'Time:' in line:
            metadata['timestamp'] = line.split('Time:', 1)[1].strip()
        elif 'Version:' in line:
            metadata['version'] = line.split('Version:', 1)[1].strip()
        elif 'Site:' in line:
            metadata['site'] = line.split('Site:', 1)[1].strip()
        elif 'User:' in line:
            metadata['user'] = line.split('User:', 1)[1].strip()
        elif 'Oracle:' in line:
            metadata['oracle_version'] = line.split('Oracle:', 1)[1].strip()
        elif 'Computer:' in line:
            metadata['computer'] = line.split('Computer:', 1)[1].strip()
        elif 'Build:' in line:
            metadata['build'] = line.split('Build:', 1)[1].strip()
    
    return metadata

def get_wcr_summary(content: str) -> Dict[str, Any]:
    """
    Generate a quick summary of WCR crash
    
    Args:
        content: WCR crash dump content
        
    Returns:
        Dictionary with WCR summary
    """
    # Try to import WCR utilities if available
    try:
        from utils.wcr_patterns import extract_wcr_cause, generate_wcr_summary
        if generate_wcr_summary:
            return generate_wcr_summary(content)
    except ImportError:
        pass
    
    # Fallback summary
    metadata = extract_wcr_metadata(content)
    
    # Extract immediate error
    immediate_error = {'class': '', 'method': ''}
    for line in content.splitlines():
        if '>>>' in line and '<<<' in line:
            # Pattern: ClassName>>>methodName<<<
            parts = line.split('>>>')
            if len(parts) >= 2:
                immediate_error['class'] = parts[0].strip().split()[-1]
                method_part = parts[1].split('<<<')[0]
                immediate_error['method'] = method_part.strip()
                break
    
    return {
        'cause': 'Smalltalk runtime error',
        'category': 'Runtime Error',
        'user': metadata.get('user', 'Unknown'),
        'site': metadata.get('site', 'Unknown'),
        'version': metadata.get('version', 'Unknown'),
        'timestamp': metadata.get('timestamp', 'Unknown'),
        'immediate_error': immediate_error,
        'matched_patterns': []
    }

def format_wcr_display(metadata: Dict[str, Any], summary: Dict[str, Any]) -> str:
    """
    Format WCR information for display
    
    Args:
        metadata: WCR metadata
        summary: WCR summary
        
    Returns:
        Formatted string for display
    """
    display = f"""
**What's On Crash Report Detected**

**Environment:**
- User: {metadata.get('user', 'Unknown')}
- Site: {metadata.get('site', 'Unknown')}
- Version: {metadata.get('version', 'Unknown')}
- Timestamp: {metadata.get('timestamp', 'Unknown')}

**Error Summary:**
- Cause: {summary.get('cause', 'Unknown')}
- Category: {summary.get('category', 'Unknown')}
"""
    
    if summary.get('immediate_error', {}).get('class'):
        display += f"\n**Error Location:**\n- Class: {summary['immediate_error']['class']}\n- Method: {summary['immediate_error']['method']}"
    
    return display.strip()

def extract_wcr_stack_trace(content: str) -> Optional[str]:
    """
    Extract the stack trace from WCR dump
    
    Args:
        content: WCR crash dump content
        
    Returns:
        Extracted stack trace or None
    """
    # Look for stack trace section
    lines = content.splitlines()
    in_stack = False
    stack_lines = []
    
    for line in lines:
        if 'Call Stack:' in line or 'Stack Trace:' in line:
            in_stack = True
            continue
        elif in_stack and (line.strip() == '' or 'END' in line):
            break
        elif in_stack:
            stack_lines.append(line)
    
    return '\n'.join(stack_lines) if stack_lines else None

def get_wcr_patterns() -> Dict[str, Any]:
    """
    Get WCR-specific error patterns
    
    Returns:
        Dictionary of WCR patterns
    """
    return {
        "MessageNotUnderstood": {
            "pattern": r"MessageNotUnderstood|doesNotUnderstand:|DNU",
            "quick_fix": "Check method spelling and object class. Ensure method exists.",
            "prevention": "Use #respondsTo: before sending messages to uncertain objects",
            "severity": "High"
        },
        "BlockClosure": {
            "pattern": r"BlockClosure|Block.*Error|invalid block",
            "quick_fix": "Check block arguments match expected count",
            "prevention": "Validate block structure before execution",
            "severity": "Medium"
        },
        "Collection": {
            "pattern": r"Collection.*Error|index.*out.*bounds|key.*not.*found",
            "quick_fix": "Check collection bounds and keys before access",
            "prevention": "Use #at:ifAbsent: and bounds checking",
            "severity": "Medium"
        }
    }