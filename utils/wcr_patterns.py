"""
WCR (What's On Crash Report) Specific Patterns and Utilities
Enhances crash analysis for Smalltalk/VisualWorks crash dumps
"""

import re
from typing import Dict, List, Optional, Tuple

# WCR-specific error patterns
WCR_PATTERNS = {
    "MethodNotUnderstood": {
        "pattern": r"Message not understood:|doesNotUnderstand:|MessageNotUnderstood",
        "quick_fix": "Check if receiver is nil. Verify method exists in the class hierarchy. Add nil checks before method calls.",
        "prevention": "Implement defensive programming with nil checks. Use #ifNotNil: blocks. Verify API compatibility.",
        "severity": "High",
        "category": "Smalltalk Runtime"
    },
    "LowSpaceInterrupt": {
        "pattern": r"Emergency: No Space Left|LowSpaceInterrupt|Low Space|memory.*exhausted",
        "quick_fix": "Free disk space immediately. Increase JVM heap size. Close unnecessary applications.",
        "prevention": "Monitor disk usage. Implement automatic cleanup. Set up space alerts.",
        "severity": "Critical",
        "category": "System Resources"
    },
    "OracleError": {
        "pattern": r"ORA-\d+|Oracle.*Error|OracleException",
        "quick_fix": "Check database connection. Verify credentials. Review SQL query syntax.",
        "prevention": "Use connection pooling. Add retry logic. Validate queries before execution.",
        "severity": "High",
        "category": "Database"
    },
    "TransactionFailure": {
        "pattern": r"Transaction.*failed|rollback|ROLLBACK|transaction.*abort",
        "quick_fix": "Retry the transaction. Check for locking issues. Verify data integrity.",
        "prevention": "Use proper transaction isolation. Implement deadlock detection. Add transaction timeouts.",
        "severity": "High",
        "category": "Database Transaction"
    },
    "CollectionBoundsError": {
        "pattern": r"Subscript.*bounds|Index.*bounds|Collection.*bounds|at:.*failed",
        "quick_fix": "Check collection size before access. Use #at:ifAbsent: instead of #at:.",
        "prevention": "Always validate indices. Use safe collection access methods.",
        "severity": "Medium",
        "category": "Smalltalk Collections"
    },
    "NilError": {
        "pattern": r"UndefinedObject|nil.*error|Attempt.*nil",
        "quick_fix": "Add nil checks. Initialize variables properly. Use #ifNil: blocks.",
        "prevention": "Always initialize instance variables. Use lazy initialization patterns.",
        "severity": "High",
        "category": "Smalltalk Runtime"
    },
    "ProcessTerminated": {
        "pattern": r"Process.*terminated|TerminateException|Process.*killed",
        "quick_fix": "Check process lifecycle. Review termination conditions.",
        "prevention": "Implement proper process cleanup. Use ensure: blocks.",
        "severity": "Medium",
        "category": "Process Management"
    }
}

def extract_wcr_cause(content: str) -> Optional[str]:
    """Extract the 'Cause of Dump' line which is the primary error indicator"""
    match = re.search(r'Cause of Dump:\s*(.+?)(?:\n|$)', content, re.IGNORECASE)
    return match.group(1).strip() if match else None

def extract_wcr_stack_frames(content: str, limit: int = 10) -> List[str]:
    """Extract stack frames from WCR dump"""
    frames = []
    pattern = r'\[(\d+)\]\s+(.+?)(?=\n\[|\n\n|$)'
    matches = re.finditer(pattern, content, re.MULTILINE | re.DOTALL)
    
    for match in matches:
        frame_num = int(match.group(1))
        frame_content = match.group(2).strip()
        frames.append(f"[{frame_num}] {frame_content}")
        if len(frames) >= limit:
            break
    
    return frames

def extract_wcr_metadata(content: str) -> Dict[str, str]:
    """Extract metadata from WCR header"""
    metadata = {}
    
    patterns = {
        'timestamp': r'==(\d{4}/\d{1,2}/\d{1,2})==(\d{1,2}:\d{1,2}:\d{1,2})==',
        'user': r"Whats'On User:\s*(.+)",
        'site': r"Whats'On Site:\s*(.+)",
        'version': r"Whats'On Version:\s*(.+)",
        'process': r'Process named:\s*[\'"](.+?)[\'"]',
        'priority': r'Process priority:\s*(\d+)',
        'oracle_version': r'Oracle.*Version:\s*(.+)',
        'exception_class': r'class:\s*(\w+)',
        'command_line': r'Command Line:\s*(.+)'
    }
    
    for key, pattern in patterns.items():
        match = re.search(pattern, content, re.MULTILINE)
        if match:
            if key == 'timestamp':
                metadata[key] = f"{match.group(1)} {match.group(2)}"
            else:
                metadata[key] = match.group(1).strip()
    
    return metadata

def analyze_smalltalk_stack_frame(frame: str) -> Dict[str, str]:
    """Parse a Smalltalk stack frame"""
    # Pattern: ClassName(OptionalSuperclass)>>methodName or ClassName class>>methodName
    pattern = r'(\w+(?:\.\w+)*)(?: class)?(?:\((\w+)\))?>>(\w+(?::)*)'
    match = re.search(pattern, frame)
    
    if match:
        return {
            'class': match.group(1),
            'superclass': match.group(2) or '',
            'method': match.group(3),
            'is_class_method': ' class>>' in frame
        }
    return {'raw': frame}

def get_wcr_error_category(cause: str, stack_frames: List[str]) -> str:
    """Categorize the error based on cause and stack trace"""
    cause_lower = cause.lower() if cause else ""
    
    # Check cause first
    if 'message not understood' in cause_lower or 'doesnotunderstand' in cause_lower:
        return "Method Not Found"
    elif 'no space left' in cause_lower or 'lowspace' in cause_lower:
        return "Memory/Space Issue"
    elif 'timeout' in cause_lower:
        return "Timeout"
    elif 'permission' in cause_lower or 'denied' in cause_lower:
        return "Permission Error"
    
    # Check stack frames
    for frame in stack_frames[:5]:  # Check first 5 frames
        frame_lower = frame.lower()
        if 'oracle' in frame_lower or 'database' in frame_lower:
            return "Database Error"
        elif 'transaction' in frame_lower:
            return "Transaction Error"
        elif 'collection' in frame_lower and 'bounds' in frame_lower:
            return "Collection Error"
    
    return "Unknown Error"

def generate_wcr_summary(content: str) -> Dict[str, any]:
    """Generate a comprehensive summary of a WCR crash dump"""
    cause = extract_wcr_cause(content)
    frames = extract_wcr_stack_frames(content, limit=5)
    metadata = extract_wcr_metadata(content)
    category = get_wcr_error_category(cause, frames)
    
    # Find matching patterns
    matched_patterns = []
    for pattern_name, pattern_info in WCR_PATTERNS.items():
        if re.search(pattern_info['pattern'], content[:5000], re.IGNORECASE):
            matched_patterns.append({
                'name': pattern_name,
                'severity': pattern_info['severity'],
                'quick_fix': pattern_info['quick_fix']
            })
    
    # Parse first frame for immediate context
    immediate_context = {}
    if frames:
        immediate_context = analyze_smalltalk_stack_frame(frames[0])
    
    return {
        'cause': cause or "Not specified",
        'category': category,
        'timestamp': metadata.get('timestamp', 'Unknown'),
        'user': metadata.get('user', 'Unknown'),
        'site': metadata.get('site', 'Unknown'),
        'version': metadata.get('version', 'Unknown'),
        'immediate_error': immediate_context,
        'stack_preview': frames[:3],  # First 3 frames
        'matched_patterns': matched_patterns,
        'metadata': metadata
    }

# Enhanced expert prompt for WCR files
WCR_EXPERT_PROMPT_TEMPLATE = """You are an expert What's On/VisualWorks Smalltalk crash investigator.

## Crash Context
- System: What's On media management system
- Language: Smalltalk (VisualWorks)
- Database: Oracle
- Error: {cause}
- Category: {category}

## Stack Trace (first 5 frames)
{stack_frames}

## Analysis Guidelines
1. **Primary Error**: Focus on the "Cause of Dump" - this is the root issue
2. **Immediate Context**: Analyze frames [1-3] for the direct error location
3. **Business Logic**: Look for MediaGeniX.* classes to understand the operation
4. **Database Layer**: Check for Oracle/Transaction frames indicating data issues
5. **Smalltalk Specifics**: 
   - doesNotUnderstand: = method called on nil or wrong object
   - UndefinedObject = nil receiver
   - Emergency: = critical system state

## Required Sections
1. **Root Cause**: Technical explanation of what failed
2. **Business Impact**: What operation/feature is affected
3. **Immediate Fix**: Steps to resolve right now
4. **Long-term Prevention**: Code changes to prevent recurrence
5. **Similar Issues**: Common related problems

Be specific about Smalltalk syntax and What's On business logic.
"""