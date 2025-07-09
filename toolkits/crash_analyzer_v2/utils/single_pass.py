"""
Single-pass regex utilities for efficient log parsing
Combines multiple patterns for performance
"""
import re
from typing import Dict, List, Tuple, NamedTuple
from collections import defaultdict

class ErrorMatch(NamedTuple):
    """Structured error match result"""
    type: str
    message: str
    severity: str
    location: str
    line_number: int
    position: int
    context: str

# Combined mega-pattern for single-pass parsing
# Uses named groups to capture all info at once
MEGA_ERROR_PATTERN = re.compile(
    r"""
    # Capture line number if present
    (?:(?P<line_num>\d+):)?\s*
    
    # Timestamp patterns (optional)
    (?:
        \[?(?P<timestamp>
            \d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}(?:\.\d{3})?(?:Z|[+-]\d{2}:\d{2})?|
            \d{2}/\d{2}/\d{4}\s+\d{2}:\d{2}:\d{2}
        )\]?\s+
    )?
    
    # Log level
    (?P<level>FATAL|CRITICAL|ERROR|SEVERE|WARN|WARNING|INFO|DEBUG|TRACE)\s*
    (?::\s*)?
    
    # Error type and message patterns
    (?:
        # Exception pattern: SomeException: message
        (?P<exception_type>\w+(?:Exception|Error)):\s*(?P<exception_msg>[^\n]+)|
        
        # Generic error: ERROR: message
        (?P<generic_msg>[^\n]+)
    )
    
    # Optional location info on same or next line
    (?:
        \s+at\s+(?P<at_location>[^\n]+)|
        \s+in\s+(?P<in_location>[^\n]+)|
        \s+File\s+"(?P<file_location>[^"]+)",\s+line\s+(?P<file_line>\d+)
    )?
    """,
    re.VERBOSE | re.IGNORECASE | re.MULTILINE
)

# Severity scoring inline
INLINE_SEVERITY = {
    'FATAL': (10, '[!!!]'),
    'CRITICAL': (9, '[!!!]'),
    'SEVERE': (8, '[!!]'),
    'ERROR': (7, '[!!]'),
    'WARN': (5, '[!]'),
    'WARNING': (5, '[!]'),
    'INFO': (2, '[i]'),
    'DEBUG': (1, '[.]'),
    'TRACE': (0, '[.]')
}

def single_pass_parse(content: str, max_errors: int = 100) -> List[ErrorMatch]:
    """
    Parse log content in a single pass
    
    Args:
        content: Log content to parse
        max_errors: Maximum errors to return
        
    Returns:
        List of structured error matches with inline severity
    """
    errors = []
    
    for match in MEGA_ERROR_PATTERN.finditer(content):
        # Extract all captured groups
        groups = match.groupdict()
        
        # Determine error type and message
        if groups.get('exception_type'):
            error_type = groups['exception_type']
            message = groups.get('exception_msg', '').strip()
        else:
            error_type = groups.get('level', 'ERROR')
            message = groups.get('generic_msg', '').strip()
        
        # Extract location
        location = "Unknown"
        if groups.get('file_location'):
            location = f"{groups['file_location']}:{groups.get('file_line', '?')}"
        elif groups.get('at_location'):
            location = groups['at_location'].strip()
        elif groups.get('in_location'):
            location = groups['in_location'].strip()
        
        # Get severity
        level = groups.get('level', 'ERROR').upper()
        severity_score, severity_marker = INLINE_SEVERITY.get(level, (5, '[?]'))
        
        # Extract context (50 chars before and after)
        start = max(0, match.start() - 50)
        end = min(len(content), match.end() + 50)
        context = content[start:end].strip()
        
        # Create annotated error with inline severity
        errors.append(ErrorMatch(
            type=error_type,
            message=f"{severity_marker} {message}",  # Inline severity
            severity=level,
            location=location,
            line_number=int(groups.get('line_num', 0)) if groups.get('line_num') else 0,
            position=match.start(),
            context=context
        ))
        
        if len(errors) >= max_errors:
            break
    
    return errors

def format_as_timeline(errors: List[ErrorMatch]) -> str:
    """
    Format errors as a clean timeline table
    
    Args:
        errors: List of error matches
        
    Returns:
        Formatted table string
    """
    if not errors:
        return "No errors detected - consider checking log verbosity or format"
    
    # Build table
    lines = ["Time/Line | Level | Message | Location"]
    lines.append("-" * 80)
    
    for error in errors:
        time_or_line = f"L{error.line_number}" if error.line_number else f"@{error.position}"
        
        # Truncate message if too long
        msg = error.message
        if len(msg) > 40:
            msg = msg[:37] + "..."
        
        # Truncate location if too long
        loc = error.location
        if len(loc) > 20:
            loc = "..." + loc[-17:]
        
        lines.append(f"{time_or_line:10} | {error.severity:8} | {msg:40} | {loc}")
    
    return "\n".join(lines)

def get_error_summary(errors: List[ErrorMatch]) -> Dict[str, any]:
    """
    Get summary statistics from parsed errors
    
    Args:
        errors: List of error matches
        
    Returns:
        Summary dictionary
    """
    if not errors:
        return {
            "total_errors": 0,
            "severity_distribution": {},
            "top_error_types": [],
            "affected_locations": []
        }
    
    # Count by severity
    severity_counts = defaultdict(int)
    type_counts = defaultdict(int)
    locations = set()
    
    for error in errors:
        severity_counts[error.severity] += 1
        type_counts[error.type] += 1
        if error.location != "Unknown":
            locations.add(error.location)
    
    # Get top error types
    top_types = sorted(type_counts.items(), key=lambda x: x[1], reverse=True)[:5]
    
    return {
        "total_errors": len(errors),
        "severity_distribution": dict(severity_counts),
        "top_error_types": [f"{typ}: {count}" for typ, count in top_types],
        "affected_locations": list(locations)[:10]
    }