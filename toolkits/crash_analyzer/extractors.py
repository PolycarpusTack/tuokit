"""
Content Extraction Utilities
Extract critical sections, errors, and stack traces from crash dumps
"""
import re
from typing import List, Tuple, Dict, Any

def extract_critical_sections(content: str, max_length: int = 10000) -> str:
    """
    Extracts the most important sections from a crash dump.
    Prioritizes stack traces and exception messages.
    
    Args:
        content: Full crash dump content
        max_length: Maximum length of extracted content
        
    Returns:
        String containing critical sections
    """
    sections = []
    
    # Extract exception info with priority
    exception_patterns = [
        (r'(FATAL:.*)', 'fatal'),
        (r'(CRITICAL:.*)', 'critical'),
        (r'(.*Exception:.*)', 'exception'),
        (r'(.*Error:.*)', 'error'),
        (r'(SEVERE:.*)', 'severe'),
        (r'(WARNING:.*)', 'warning')
    ]
    
    for pattern, priority in exception_patterns:
        matches = re.finditer(pattern, content, re.IGNORECASE | re.MULTILINE)
        for match in matches:
            # Get surrounding context
            start = max(0, match.start() - 200)
            end = min(len(content), match.end() + 500)
            sections.append((priority, content[start:end]))
    
    # Extract stack traces
    stack_patterns = [
        r'(?s)(\s+at\s+.*\n)+',
        r'(Thread\s+\d+.*\n(?:.*\n){0,10})',
        r'(Call stack:.*?\n\n)',
        r'(Stack trace:.*?\n\n)',
        r'(Traceback.*?\n\n)'
    ]
    
    for pattern in stack_patterns:
        matches = re.finditer(pattern, content, re.MULTILINE)
        for match in matches:
            sections.append(("stack_trace", match.group(0)))
    
    # Build critical content with priority
    critical_content = ""
    
    # Group by type and add in priority order
    priority_order = ['fatal', 'critical', 'exception', 'error', 'severe', 'stack_trace', 'warning']
    
    for priority in priority_order:
        priority_sections = [s for p, s in sections if p == priority]
        if priority_sections:
            critical_content += f"\n--- {priority.upper()} ---\n"
            # Limit number of sections per type
            for section in priority_sections[:3]:
                critical_content += section + "\n---\n"
    
    # Fallback if no patterns matched
    if not critical_content:
        # Look for any error keywords
        error_lines = []
        for i, line in enumerate(content.splitlines()):
            if re.search(r'error|fail|crash|exception|fatal', line, re.IGNORECASE):
                # Get context around error line
                start_line = max(0, i - 5)
                end_line = min(len(content.splitlines()), i + 5)
                context_lines = content.splitlines()[start_line:end_line]
                error_lines.append('\n'.join(context_lines))
        
        if error_lines:
            critical_content = "\n--- ERROR CONTEXTS ---\n" + "\n---\n".join(error_lines[:5])
        else:
            # Last resort: beginning and end
            if len(content) > 4000:
                critical_content = content[:2000] + "\n\n[... middle section omitted ...]\n\n" + content[-2000:]
            else:
                critical_content = content
    
    # Truncate to max_length
    if len(critical_content) > max_length:
        critical_content = critical_content[:max_length] + "\n[... truncated ...]"
    
    return critical_content

def generate_strategic_samples(content: str, max_samples: int = 10, 
                             sample_size: int = 2000) -> List[Dict[str, Any]]:
    """
    Extract strategic samples from large files for quick analysis
    
    Args:
        content: Full file content
        max_samples: Maximum number of samples to extract
        sample_size: Size of each sample in characters
        
    Returns:
        List of sample dictionaries with type, content, description, and position
    """
    samples = []
    
    # 1. Get file header
    samples.append({
        "type": "header",
        "content": content[:sample_size],
        "description": "File beginning",
        "position": 0
    })
    
    # 2. Find error/exception locations
    error_keywords = r'error|exception|crash|fatal|failed|critical|severe|panic|abort'
    error_positions = []
    
    for match in re.finditer(error_keywords, content, re.IGNORECASE):
        error_positions.append(match.start())
    
    # 3. Sample around errors (limit to avoid too many samples)
    sampled_positions = set()
    error_samples_added = 0
    
    for pos in error_positions:
        # Avoid overlapping samples
        if not any(abs(pos - p) < sample_size for p in sampled_positions):
            start = max(0, pos - sample_size//2)
            end = min(len(content), pos + sample_size//2)
            
            # Extract the actual error line for description
            line_start = content.rfind('\n', 0, pos) + 1
            line_end = content.find('\n', pos)
            if line_end == -1:
                line_end = len(content)
            error_line = content[line_start:line_end].strip()[:50]
            
            samples.append({
                "type": "error_context",
                "content": content[start:end],
                "description": f"Error: {error_line}...",
                "position": pos
            })
            sampled_positions.add(pos)
            error_samples_added += 1
            
            if error_samples_added >= max_samples - 2:
                break
    
    # 4. Look for stack traces specifically
    stack_trace_pattern = r'(?m)^\s+at\s+.*$'
    stack_matches = list(re.finditer(stack_trace_pattern, content))
    
    if stack_matches and len(samples) < max_samples - 1:
        # Get first stack trace
        stack_start = stack_matches[0].start()
        start = max(0, stack_start - 500)
        end = min(len(content), stack_start + 1500)
        
        samples.append({
            "type": "stack_trace",
            "content": content[start:end],
            "description": "Stack trace context",
            "position": stack_start
        })
    
    # 5. Get file end
    samples.append({
        "type": "footer", 
        "content": content[-sample_size:],
        "description": "File end",
        "position": len(content) - sample_size
    })
    
    return samples

def extract_mermaid_diagrams(text: str) -> List[str]:
    """
    Extract Mermaid diagrams from markdown text
    
    Args:
        text: Text that may contain mermaid diagrams
        
    Returns:
        List of mermaid diagram code blocks
    """
    diagrams = []
    if isinstance(text, dict):
        text = text.get('response', '')
    lines = text.split('\n')
    in_mermaid = False
    current_diagram = []
    
    for line in lines:
        if line.strip().startswith("```mermaid"):
            in_mermaid = True
            continue
        elif in_mermaid and line.strip().startswith("```"):
            in_mermaid = False
            if current_diagram:
                diagrams.append('\n'.join(current_diagram))
                current_diagram = []
        elif in_mermaid:
            current_diagram.append(line)
    
    return diagrams

def extract_timestamp_info(content: str) -> Dict[str, Any]:
    """
    Extract timestamp information from crash dump
    
    Args:
        content: Crash dump content
        
    Returns:
        Dictionary with timestamp info
    """
    timestamps = re.findall(r'\d{4}-\d{2}-\d{2}[\s\-T]\d{2}:\d{2}:\d{2}', content)
    
    if not timestamps:
        return {
            "has_timestamps": False,
            "count": 0,
            "first": None,
            "last": None,
            "range": None
        }
    
    return {
        "has_timestamps": True,
        "count": len(timestamps),
        "first": timestamps[0],
        "last": timestamps[-1],
        "range": f"{timestamps[0][:10]} to {timestamps[-1][:10]}" if len(timestamps) > 1 else timestamps[0][:10]
    }

def extract_file_metadata(content: str) -> Dict[str, Any]:
    """
    Extract metadata about the crash file
    
    Args:
        content: Crash dump content
        
    Returns:
        Dictionary with file metadata
    """
    lines = content.splitlines()
    size_kb = len(content) / 1024
    
    # Count various elements
    error_count = len(re.findall(r'error|exception|fail|fatal|crash', content, re.IGNORECASE))
    stack_frames = len(re.findall(r'^\s+at\s+', content, re.MULTILINE))
    
    metadata = {
        "lines": len(lines),
        "size_kb": size_kb,
        "size_mb": size_kb / 1024 if size_kb > 1024 else 0,
        "error_count": error_count,
        "stack_frames": stack_frames,
        "has_stack_trace": stack_frames > 0
    }
    
    # Add timestamp info
    metadata.update(extract_timestamp_info(content))
    
    return metadata

def find_error_locations(content: str, limit: int = 10) -> List[Tuple[int, str]]:
    """
    Find locations of errors in the content
    
    Args:
        content: Crash dump content
        limit: Maximum number of locations to return
        
    Returns:
        List of tuples (position, error_text)
    """
    locations = []
    error_pattern = r'(.*(?:error|exception|crash|fatal|failed).*)'
    
    for match in re.finditer(error_pattern, content, re.IGNORECASE | re.MULTILINE):
        if len(locations) >= limit:
            break
        locations.append((match.start(), match.group(1).strip()))
    
    return locations