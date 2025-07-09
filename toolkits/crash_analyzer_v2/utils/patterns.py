"""
Centralized pattern definitions for Crash Analyzer V2
Avoids duplication and ensures consistency across analyzers
"""
import re
from typing import Dict, List, Tuple, Pattern

# Error patterns used across analyzers
ERROR_PATTERNS: List[Tuple[str, str, str]] = [
    # Pattern, type, severity
    (r'(?i)null\s*pointer\s*exception', 'null_pointer', 'HIGH'),
    (r'(?i)out\s*of\s*memory(?:\s*error)?', 'memory_exhaustion', 'CRITICAL'),
    (r'(?i)stack\s*overflow(?:\s*error)?', 'stack_overflow', 'CRITICAL'),
    (r'(?i)deadlock', 'deadlock', 'HIGH'),
    (r'(?i)race\s*condition', 'race_condition', 'HIGH'),
    (r'(?i)buffer\s*overflow', 'buffer_overflow', 'CRITICAL'),
    (r'(?i)sql\s*injection', 'sql_injection', 'CRITICAL'),
    (r'(?i)cross[- ]?site[- ]?scripting|xss', 'xss', 'CRITICAL'),
    (r'(?i)segmentation\s*fault|segfault', 'segfault', 'CRITICAL'),
    (r'(?i)access\s*violation', 'access_violation', 'HIGH'),
    (r'(?i)divide\s*by\s*zero', 'divide_by_zero', 'MEDIUM'),
    (r'(?i)timeout\s*exception', 'timeout', 'MEDIUM'),
    (r'(?i)connection\s*refused', 'connection_refused', 'MEDIUM'),
    (r'(?i)file\s*not\s*found', 'file_not_found', 'LOW'),
    (r'(?i)permission\s*denied', 'permission_denied', 'MEDIUM'),
]

# Stack trace patterns for different languages
STACK_TRACE_PATTERNS: Dict[str, Pattern] = {
    'java': re.compile(r'^\s*at\s+[\w\.$]+\([^\)]+\)(?:\n\s*at\s+[\w\.$]+\([^\)]+\))*', re.MULTILINE),
    'python': re.compile(r'Traceback \(most recent call last\):(?:\n.*?)+?(?=\n(?:\S|\Z))', re.MULTILINE | re.DOTALL),
    'csharp': re.compile(r'^\s*at\s+[\w\.<>]+\([^\)]*\)(?:\s+in\s+[^\n]+)?(?:\n\s*at\s+[\w\.<>]+\([^\)]*\))*', re.MULTILINE),
    'cpp': re.compile(r'#\d+\s+0x[0-9a-fA-F]+\s+in\s+.+(?:\n#\d+\s+.*)*', re.MULTILINE),
    'generic': re.compile(r'(?m)^[\w\.]+ in [\w\.]+ at [^\n]+(?:\n\s+.*)*'),
}

# Timestamp patterns
TIMESTAMP_PATTERNS: List[Pattern] = [
    re.compile(r'\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}(?:\.\d{3})?(?:Z|[+-]\d{2}:\d{2})?'),  # ISO 8601
    re.compile(r'\d{2}/\d{2}/\d{4}\s+\d{2}:\d{2}:\d{2}'),  # MM/DD/YYYY HH:MM:SS
    re.compile(r'\d{4}/\d{2}/\d{2}\s+\d{2}:\d{2}:\d{2}'),  # YYYY/MM/DD HH:MM:SS
    re.compile(r'\w{3}\s+\d{1,2}\s+\d{2}:\d{2}:\d{2}'),     # Mon 01 12:34:56
    re.compile(r'\[\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}:\d{2}\]'),  # [YYYY-MM-DD HH:MM:SS]
]

# Log level patterns
LOG_LEVEL_PATTERNS: Dict[str, Pattern] = {
    'error': re.compile(r'(?i)\b(?:ERROR|ERR|SEVERE|FATAL|CRITICAL)\b'),
    'warning': re.compile(r'(?i)\b(?:WARN|WARNING|CAUTION)\b'),
    'info': re.compile(r'(?i)\b(?:INFO|INFORMATION|NOTICE)\b'),
    'debug': re.compile(r'(?i)\b(?:DEBUG|TRACE|VERBOSE)\b'),
}

# Security patterns
SECURITY_PATTERNS: List[Tuple[Pattern, str, str]] = [
    # Pattern, type, description
    (re.compile(r'(?i)password\s*[:=]\s*["\']?([^"\'\s\n]+)'), 'exposed_password', 'Password in plain text'),
    (re.compile(r'(?i)api[_-]?key\s*[:=]\s*["\']?([^"\'\s\n]+)'), 'exposed_api_key', 'API key exposed'),
    (re.compile(r'(?i)token\s*[:=]\s*["\']?([^"\'\s\n]+)'), 'exposed_token', 'Authentication token exposed'),
    (re.compile(r'(?i)secret\s*[:=]\s*["\']?([^"\'\s\n]+)'), 'exposed_secret', 'Secret value exposed'),
    (re.compile(r'\b(?:\d{4}[\s-]?){3}\d{4}\b'), 'possible_credit_card', 'Possible credit card number'),
    (re.compile(r'(?i)ssn:\s*\d{3}-\d{2}-\d{4}'), 'possible_ssn', 'Possible SSN'),
    (re.compile(r'(?i)private[_-]?key'), 'private_key_reference', 'Private key reference'),
]

# Performance patterns
PERFORMANCE_PATTERNS: Dict[str, Pattern] = {
    'slow_query': re.compile(r'(?i)query (?:took|executed in|duration:?)\s*(\d+(?:\.\d+)?)\s*(ms|milliseconds?|seconds?|s)\b'),
    'memory_usage': re.compile(r'(?i)(?:memory|heap|ram)(?:\s+usage)?:\s*(\d+(?:\.\d+)?)\s*(%|MB|GB|bytes?)'),
    'cpu_usage': re.compile(r'(?i)cpu(?:\s+usage)?:\s*(\d+(?:\.\d+)?)\s*%'),
    'thread_count': re.compile(r'(?i)(?:threads?|thread count):\s*(\d+)'),
    'gc_pause': re.compile(r'(?i)(?:gc|garbage collection) (?:pause|took|duration):?\s*(\d+(?:\.\d+)?)\s*ms'),
}

# Code quality patterns
CODE_QUALITY_PATTERNS: Dict[str, Pattern] = {
    'empty_catch': re.compile(r'catch\s*\(\s*(?:Exception|Throwable|\w+)\s+\w+\s*\)\s*{\s*}'),
    'catch_throwable': re.compile(r'catch\s*\(\s*Throwable\b'),
    'todo_fixme': re.compile(r'(?i)\b(?:TODO|FIXME|HACK|XXX|WORKAROUND)\b'),
    'magic_numbers': re.compile(r'(?<!\d)(?:86400|3600|1440|1024|2048|4096|65535)(?!\d)'),
    'deprecated': re.compile(r'@Deprecated|\.deprecated\(|DEPRECATED|deprecated'),
}

# Vulnerability patterns
VULNERABILITY_PATTERNS: List[Tuple[Pattern, str, str]] = [
    (re.compile(r'(?i)sql.*(?:select|insert|update|delete).*where.*["\']?\s*\+'), 'sql_injection', 'CRITICAL'),
    (re.compile(r'(?i)eval\s*\([^)]*\$|eval\s*\([^)]*request'), 'code_injection', 'CRITICAL'),
    (re.compile(r'(?i)os\.system|subprocess\.(?:call|run).*shell\s*=\s*True'), 'command_injection', 'CRITICAL'),
    (re.compile(r'(?i)disable.*ssl.*verif|verify\s*=\s*False|check_hostname\s*=\s*False'), 'ssl_disabled', 'HIGH'),
    (re.compile(r'(?i)(?:md5|sha1)\s*\('), 'weak_crypto', 'MEDIUM'),
    (re.compile(r'(?i)allow.*origin.*\*|Access-Control-Allow-Origin:\s*\*'), 'cors_wildcard', 'HIGH'),
]

def extract_error_location(content: str, error_match: re.Match) -> str:
    """
    Extract error location from context around error
    
    Args:
        content: Full content
        error_match: Regex match object for the error
        
    Returns:
        Location string or "Unknown"
    """
    # Look for file/line info near the error
    start = max(0, error_match.start() - 200)
    end = min(len(content), error_match.end() + 200)
    context = content[start:end]
    
    # Common location patterns
    location_patterns = [
        r'(?:at|in)\s+([\w./\\]+\.(?:java|py|cs|cpp|c|js|ts|rb|go))(?::(\d+))?',
        r'File\s*"([^"]+)",\s*line\s*(\d+)',
        r'([^:\s]+):(\d+):\d+:',  # file.ext:line:col:
        r'Source:\s*([^\n]+)',
    ]
    
    for pattern in location_patterns:
        match = re.search(pattern, context)
        if match:
            if match.lastindex and match.lastindex >= 2:
                return f"{match.group(1)}:{match.group(2)}"
            else:
                return match.group(1)
    
    return "Unknown"

def normalize_confidence(confidence: any) -> float:
    """
    Normalize confidence values to float [0.0, 1.0]
    
    Args:
        confidence: String or numeric confidence value
        
    Returns:
        Float confidence value
    """
    if isinstance(confidence, (int, float)):
        return max(0.0, min(1.0, float(confidence)))
    
    if isinstance(confidence, str):
        # Normalize string
        confidence_str = confidence.strip().upper()
        
        # String mappings
        confidence_map = {
            "VERY HIGH": 0.95,
            "HIGH": 0.85,
            "MEDIUM": 0.65,
            "LOW": 0.45,
            "VERY LOW": 0.25,
            "UNKNOWN": 0.5,
        }
        
        return confidence_map.get(confidence_str, 0.5)
    
    # Default
    return 0.5

def compile_patterns() -> Dict[str, List[Pattern]]:
    """
    Pre-compile all patterns for performance
    
    Returns:
        Dictionary of compiled pattern lists
    """
    compiled = {
        'errors': [re.compile(pattern) for pattern, _, _ in ERROR_PATTERNS],
        'timestamps': TIMESTAMP_PATTERNS,
        'log_levels': list(LOG_LEVEL_PATTERNS.values()),
        'security': [pattern for pattern, _, _ in SECURITY_PATTERNS],
        'performance': list(PERFORMANCE_PATTERNS.values()),
        'quality': list(CODE_QUALITY_PATTERNS.values()),
        'vulnerabilities': [pattern for pattern, _, _ in VULNERABILITY_PATTERNS],
    }
    return compiled

# Pre-compiled patterns for performance
COMPILED_PATTERNS = compile_patterns()