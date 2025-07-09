"""
Content Extraction Utilities for Crash Analyzer V2
Smart extraction of relevant sections from crash dumps
"""
import re
from typing import List, Dict, Any, Tuple, Optional

def extract_smart_context(content: str, max_size: int = 100 * 1024) -> str:
    """
    Extract the most relevant parts of a crash dump intelligently
    
    Args:
        content: Full crash dump content
        max_size: Maximum size of extracted content in bytes
        
    Returns:
        Extracted content with most relevant sections
    """
    if len(content) <= max_size:
        return content
    
    extracted_parts = []
    current_size = 0
    
    # Priority 1: Error messages and exceptions (highest priority)
    error_sections = extract_error_sections(content, max_size // 3)
    if error_sections:
        extracted_parts.append("=== ERROR SECTIONS ===\n" + error_sections)
        current_size += len(error_sections)
    
    # Priority 2: Stack traces
    if current_size < max_size:
        stack_traces = extract_stack_traces(content, (max_size - current_size) // 2)
        if stack_traces:
            extracted_parts.append("\n=== STACK TRACES ===\n" + stack_traces)
            current_size += len(stack_traces)
    
    # Priority 3: System information and metadata
    if current_size < max_size:
        metadata = extract_metadata(content, (max_size - current_size) // 3)
        if metadata:
            extracted_parts.append("\n=== SYSTEM INFO ===\n" + metadata)
            current_size += len(metadata)
    
    # Priority 4: Temporal context (events before/after errors)
    if current_size < max_size:
        temporal = extract_temporal_context(content, max_size - current_size)
        if temporal:
            extracted_parts.append("\n=== TEMPORAL CONTEXT ===\n" + temporal)
    
    return "\n".join(extracted_parts)

def extract_error_sections(content: str, max_size: int) -> str:
    """Extract sections containing errors, exceptions, and failures"""
    error_keywords = [
        'exception', 'error', 'fail', 'fatal', 'critical', 
        'severe', 'panic', 'crash', 'abort', 'violation'
    ]
    
    pattern = r'(?i)(' + '|'.join(error_keywords) + r')'
    sections = []
    current_size = 0
    
    for match in re.finditer(pattern, content):
        if current_size >= max_size:
            break
            
        # Extract context around error (500 chars before, 1000 after)
        start = max(0, match.start() - 500)
        end = min(len(content), match.end() + 1000)
        section = content[start:end]
        
        # Avoid duplicates
        if not any(section in s for s in sections):
            sections.append(section)
            current_size += len(section)
    
    return "\n---\n".join(sections)

def extract_stack_traces(content: str, max_size: int) -> str:
    """Extract complete stack traces from content"""
    stack_patterns = [
        # Java/C# style
        r'(?m)^\s*at\s+[\w\.$]+\([^\)]+\)(?:\n\s*at\s+[\w\.$]+\([^\)]+\))*',
        # Python style
        r'Traceback \(most recent call last\):(?:\n.*?)+(?=\n\S|\Z)',
        # C/C++ style
        r'#\d+\s+0x[0-9a-fA-F]+\s+in\s+.+(?:\n#\d+\s+.*)*',
        # Generic
        r'(?m)^[\w\.]+ in [\w\.]+ at [^\n]+(?:\n\s+.*)*'
    ]
    
    traces = []
    current_size = 0
    
    for pattern in stack_patterns:
        if current_size >= max_size:
            break
            
        for match in re.finditer(pattern, content):
            if current_size >= max_size:
                break
                
            trace = match.group(0)
            if trace not in traces:
                traces.append(trace)
                current_size += len(trace)
    
    return "\n\n".join(traces)

def extract_metadata(content: str, max_size: int) -> str:
    """Extract system information and metadata"""
    metadata_patterns = {
        'version': r'(?i)(?:version|build|release):\s*([^\n]+)',
        'timestamp': r'\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}',
        'memory': r'(?i)(?:memory|heap|ram):\s*([^\n]+)',
        'cpu': r'(?i)(?:cpu|processor):\s*([^\n]+)',
        'os': r'(?i)(?:os|operating system|platform):\s*([^\n]+)',
        'user': r'(?i)(?:user|username|account):\s*([^\n]+)',
        'thread': r'(?i)(?:thread|threads):\s*(\d+)',
        'process': r'(?i)(?:process|pid):\s*(\d+)',
    }
    
    metadata = []
    current_size = 0
    
    # First 10KB often contains metadata
    search_content = content[:10240] if len(content) > 10240 else content
    
    for key, pattern in metadata_patterns.items():
        if current_size >= max_size:
            break
            
        match = re.search(pattern, search_content)
        if match:
            info = f"{key.upper()}: {match.group(0)}"
            metadata.append(info)
            current_size += len(info)
    
    return "\n".join(metadata)

def extract_temporal_context(content: str, max_size: int) -> str:
    """Extract temporal context around errors"""
    # Find timestamps
    timestamp_pattern = r'(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}[^\n]*)'
    
    events = []
    for match in re.finditer(timestamp_pattern, content):
        if len(events) >= 50:  # Limit to 50 events
            break
            
        # Get line with timestamp and next few lines
        start = match.start()
        end = content.find('\n', start + 200)
        if end == -1:
            end = start + 200
            
        event = content[start:end].strip()
        events.append(event)
    
    # Sort by timestamp (assuming they're sortable)
    events = sorted(set(events))
    
    # Take events that fit in size limit
    result = []
    current_size = 0
    
    for event in events:
        if current_size + len(event) > max_size:
            break
        result.append(event)
        current_size += len(event) + 1
    
    return "\n".join(result)

def extract_error_chain(content: str) -> List[Dict[str, Any]]:
    """
    Extract chain of errors leading to the crash
    
    Returns:
        List of errors in sequence
    """
    chain = []
    
    # Common patterns for error chains
    patterns = [
        # Caused by pattern (Java/C#)
        r'Caused by:\s*(\w+(?:Exception|Error)):\s*([^\n]+)',
        # Python traceback
        r'During handling of the above exception[^\n]+:\n\s*(\w+Error):\s*([^\n]+)',
        # Generic error sequence
        r'(?:ERROR|FATAL):\s*([^\n]+).*?(?:caused by|due to|because):\s*([^\n]+)',
    ]
    
    for pattern in patterns:
        for match in re.finditer(pattern, content, re.IGNORECASE | re.DOTALL):
            if len(match.groups()) >= 2:
                chain.append({
                    "error": match.group(1),
                    "details": match.group(2).strip(),
                    "position": match.start()
                })
            else:
                chain.append({
                    "error": match.group(0),
                    "details": "",
                    "position": match.start()
                })
    
    # Sort by position to get chronological order
    chain.sort(key=lambda x: x["position"])
    
    # Remove duplicates while preserving order
    seen = set()
    unique_chain = []
    for item in chain:
        key = (item["error"], item["details"])
        if key not in seen:
            seen.add(key)
            unique_chain.append(item)
    
    return unique_chain[:10]  # Limit to 10 items

def extract_performance_indicators(content: str) -> Dict[str, Any]:
    """Extract performance-related indicators"""
    indicators = {
        "slow_queries": [],
        "memory_issues": [],
        "thread_issues": [],
        "timeouts": []
    }
    
    # Slow query patterns
    slow_query_pattern = r'(?i)query took (\d+(?:\.\d+)?)\s*(ms|seconds?)'
    for match in re.finditer(slow_query_pattern, content):
        time_val = float(match.group(1))
        unit = match.group(2).lower()
        if unit.startswith('second'):
            time_val *= 1000  # Convert to ms
        
        if time_val > 1000:  # Over 1 second
            indicators["slow_queries"].append({
                "time_ms": time_val,
                "context": content[max(0, match.start()-100):match.end()+100]
            })
    
    # Memory patterns
    memory_pattern = r'(?i)(out of memory|memory exhausted|heap space|memory leak)'
    for match in re.finditer(memory_pattern, content):
        indicators["memory_issues"].append({
            "type": match.group(1),
            "context": content[max(0, match.start()-200):match.end()+200]
        })
    
    # Thread patterns
    thread_pattern = r'(?i)(deadlock|thread blocked|thread starvation|too many threads)'
    for match in re.finditer(thread_pattern, content):
        indicators["thread_issues"].append({
            "type": match.group(1),
            "context": content[max(0, match.start()-200):match.end()+200]
        })
    
    # Timeout patterns
    timeout_pattern = r'(?i)(timeout|timed out|deadline exceeded|took too long)'
    for match in re.finditer(timeout_pattern, content):
        indicators["timeouts"].append({
            "type": match.group(1),
            "context": content[max(0, match.start()-150):match.end()+150]
        })
    
    return indicators

def extract_security_concerns(content: str) -> List[Dict[str, Any]]:
    """Extract potential security issues from crash dump"""
    concerns = []
    
    # Patterns for sensitive data
    sensitive_patterns = [
        (r'(?i)password\s*=\s*["\']?([^"\'\s]+)', 'exposed_password'),
        (r'(?i)api[_-]?key\s*=\s*["\']?([^"\'\s]+)', 'exposed_api_key'),
        (r'(?i)token\s*=\s*["\']?([^"\'\s]+)', 'exposed_token'),
        (r'(?i)secret\s*=\s*["\']?([^"\'\s]+)', 'exposed_secret'),
        (r'\b(?:\d{4}[\s-]?){3}\d{4}\b', 'possible_credit_card'),
        (r'(?i)ssn:\s*\d{3}-\d{2}-\d{4}', 'possible_ssn'),
    ]
    
    for pattern, concern_type in sensitive_patterns:
        for match in re.finditer(pattern, content):
            # Redact the actual value
            value = match.group(1) if match.lastindex else match.group(0)
            redacted = value[0] + '*' * (len(value) - 2) + value[-1] if len(value) > 2 else '***'
            
            concerns.append({
                "type": concern_type,
                "pattern": pattern,
                "location": match.start(),
                "redacted_value": redacted
            })
    
    return concerns[:10]  # Limit to prevent too many false positives