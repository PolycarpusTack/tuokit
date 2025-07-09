"""
Crash Pattern Definitions and Matching
Defines known error patterns and provides pattern matching functionality
"""
import re
from typing import List, Dict, Any

# Import WCR patterns if available
try:
    from utils.wcr_patterns import WCR_PATTERNS
    ENHANCED_PATTERNS = WCR_PATTERNS.copy()
except ImportError:
    ENHANCED_PATTERNS = {}

# Known error patterns with severity levels
KNOWN_PATTERNS = {
    **ENHANCED_PATTERNS,  # Include WCR patterns if available
    "NullPointerException": {
        "pattern": r"NullPointerException|NPE|null.*reference|nil.*reference|accessing null object|nil object",
        "quick_fix": "Add null checks before object access: `if (obj != null)`",
        "prevention": "Use defensive programming and Optional types",
        "severity": "High"
    },
    "OutOfMemoryError": {
        "pattern": r"OutOfMemoryError|OOM|heap.*space|memory.*exhausted|GC overhead limit|Java heap space",
        "quick_fix": "Increase JVM heap size: `-Xmx2g` or `-Xmx4g`. Check for memory leaks",
        "prevention": "Profile memory usage, fix memory leaks, optimize collections",
        "severity": "Critical"
    },
    "StackOverflow": {
        "pattern": r"StackOverflowError|stack.*overflow|recursion.*limit|too many nested calls",
        "quick_fix": "Check for infinite recursion or circular references",
        "prevention": "Add recursion depth limits and base cases",
        "severity": "High"
    },
    "DatabaseTimeout": {
        "pattern": r"timeout|connection.*timed.*out|query.*timeout|lock.*wait.*timeout",
        "quick_fix": "Increase timeout settings or optimize query",
        "prevention": "Add indexes, optimize queries, use connection pooling",
        "severity": "Medium"
    },
    "FileNotFound": {
        "pattern": r"FileNotFoundException|file.*not.*found|no.*such.*file|cannot.*open.*file",
        "quick_fix": "Verify file path and permissions",
        "prevention": "Add file existence checks before access",
        "severity": "Low"
    },
    "DeadlockDetected": {
        "pattern": r"deadlock.*detected|circular.*lock|transaction.*rollback.*deadlock",
        "quick_fix": "Retry transaction or reorder lock acquisition",
        "prevention": "Use consistent lock ordering, minimize lock scope",
        "severity": "High"
    },
    "PermissionDenied": {
        "pattern": r"permission.*denied|access.*denied|unauthorized|forbidden",
        "quick_fix": "Check file/resource permissions and user privileges",
        "prevention": "Implement proper permission checks before operations",
        "severity": "Medium"
    },
    "NetworkError": {
        "pattern": r"network.*error|connection.*refused|socket.*error|unreachable",
        "quick_fix": "Check network connectivity and firewall settings",
        "prevention": "Implement retry logic with exponential backoff",
        "severity": "Medium"
    }
}

def match_known_patterns(content: str, context_chars: int = 100) -> List[Dict[str, Any]]:
    """
    Match crash content against known patterns with context awareness
    
    Args:
        content: The crash dump content to analyze
        context_chars: Number of characters to include for context
        
    Returns:
        List of pattern matches with details and context
    """
    matches = []
    
    for pattern_name, pattern_info in KNOWN_PATTERNS.items():
        pattern_matches = list(re.finditer(pattern_info["pattern"], content, re.IGNORECASE))
        
        for match_obj in pattern_matches:
            start = max(0, match_obj.start() - context_chars)
            end = min(len(content), match_obj.end() + context_chars)
            context = content[start:end].strip()
            
            # Clean up context for display
            context = re.sub(r'\s+', ' ', context)  # Normalize whitespace
            if len(context) > 200:
                context = context[:200] + "..."
            
            matches.append({
                "pattern": pattern_name,
                "quick_fix": pattern_info["quick_fix"],
                "prevention": pattern_info["prevention"],
                "severity": pattern_info.get("severity", "Medium"),
                "context": context,
                "position": match_obj.start(),
                "match_text": match_obj.group(0)
            })
    
    # Sort by severity and position
    severity_order = {"Critical": 4, "High": 3, "Medium": 2, "Low": 1}
    matches.sort(key=lambda x: (-severity_order.get(x["severity"], 0), x["position"]))
    
    return matches

def get_pattern_by_name(pattern_name: str) -> Dict[str, Any]:
    """Get a specific pattern definition by name"""
    return KNOWN_PATTERNS.get(pattern_name, None)

def get_patterns_by_severity(severity: str) -> Dict[str, Dict[str, Any]]:
    """Get all patterns of a specific severity level"""
    return {
        name: info 
        for name, info in KNOWN_PATTERNS.items() 
        if info.get("severity", "Medium") == severity
    }

def add_custom_pattern(name: str, pattern: str, quick_fix: str, 
                      prevention: str, severity: str = "Medium") -> None:
    """
    Add a custom pattern to the pattern library
    
    Args:
        name: Pattern name
        pattern: Regular expression pattern
        quick_fix: Quick fix suggestion
        prevention: Prevention strategy
        severity: Severity level (Critical/High/Medium/Low)
    """
    KNOWN_PATTERNS[name] = {
        "pattern": pattern,
        "quick_fix": quick_fix,
        "prevention": prevention,
        "severity": severity
    }

# Pattern statistics helper
def get_pattern_statistics(matches: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Generate statistics from pattern matches
    
    Args:
        matches: List of pattern matches
        
    Returns:
        Dictionary with pattern statistics
    """
    if not matches:
        return {
            "total_matches": 0,
            "unique_patterns": 0,
            "severity_distribution": {},
            "most_common_pattern": None
        }
    
    # Count patterns
    pattern_counts = {}
    severity_counts = {"Critical": 0, "High": 0, "Medium": 0, "Low": 0}
    
    for match in matches:
        pattern = match["pattern"]
        severity = match["severity"]
        
        pattern_counts[pattern] = pattern_counts.get(pattern, 0) + 1
        severity_counts[severity] = severity_counts.get(severity, 0) + 1
    
    # Find most common
    most_common = max(pattern_counts.items(), key=lambda x: x[1])
    
    return {
        "total_matches": len(matches),
        "unique_patterns": len(pattern_counts),
        "severity_distribution": severity_counts,
        "most_common_pattern": most_common[0],
        "most_common_count": most_common[1],
        "pattern_frequency": pattern_counts
    }