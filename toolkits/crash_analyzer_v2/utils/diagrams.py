"""
Diagram Generation Utilities for Crash Analyzer V2
Creates Mermaid diagrams for visual analysis
"""
from typing import List, Dict, Any

def generate_error_flow_diagram(error_chain: List[str]) -> str:
    """
    Generate a Mermaid diagram showing error flow
    
    Args:
        error_chain: List of errors in sequence
        
    Returns:
        Mermaid diagram code
    """
    if not error_chain:
        return ""
    
    diagram = "graph TD\n"
    
    # Create nodes for each error
    for i, error in enumerate(error_chain):
        # Sanitize error text for Mermaid
        clean_error = error.replace('"', "'").replace('\n', ' ')
        if len(clean_error) > 50:
            clean_error = clean_error[:50] + "..."
        
        node_id = f"E{i}"
        
        # Style based on position
        if i == 0:
            diagram += f'    {node_id}["{clean_error}"]:::rootCause\n'
        elif i == len(error_chain) - 1:
            diagram += f'    {node_id}["{clean_error}"]:::finalError\n'
        else:
            diagram += f'    {node_id}["{clean_error}"]:::intermediate\n'
        
        # Add arrow to next error
        if i < len(error_chain) - 1:
            diagram += f"    {node_id} --> E{i+1}\n"
    
    # Add styling
    diagram += """
    classDef rootCause fill:#ff6b6b,stroke:#c92a2a,stroke-width:3px,color:#fff
    classDef intermediate fill:#4ecdc4,stroke:#099268,stroke-width:2px,color:#fff
    classDef finalError fill:#ff8787,stroke:#fa5252,stroke-width:3px,color:#fff
"""
    
    return diagram

def generate_timeline_diagram(events: List[Dict[str, str]]) -> str:
    """
    Generate a timeline diagram for events
    
    Args:
        events: List of events with timestamps
        
    Returns:
        Mermaid gantt diagram code
    """
    if not events:
        return ""
    
    diagram = """gantt
    title Crash Timeline
    dateFormat HH:mm:ss
    axisFormat %H:%M:%S
    
"""
    
    # Group events by type
    sections = {
        "Errors": [],
        "Warnings": [],
        "Info": []
    }
    
    for event in events[:20]:  # Limit to 20 events
        if "ERROR" in event.get("event", "").upper():
            sections["Errors"].append(event)
        elif "WARN" in event.get("event", "").upper():
            sections["Warnings"].append(event)
        else:
            sections["Info"].append(event)
    
    # Add sections to diagram
    for section, items in sections.items():
        if items:
            diagram += f"    section {section}\n"
            for i, event in enumerate(items):
                event_name = event.get("event", "Event")[:30]
                diagram += f"    {event_name} :e{i}, 1s\n"
    
    return diagram

def generate_component_impact_diagram(components: List[str], severity_map: Dict[str, str] = None) -> str:
    """
    Generate a component impact diagram
    
    Args:
        components: List of affected components
        severity_map: Optional mapping of component to severity
        
    Returns:
        Mermaid mindmap diagram code
    """
    if not components:
        return ""
    
    diagram = """graph TB
    subgraph "Impact Analysis"
    CRASH[Crash Point]
"""
    
    # Default severity if not provided
    if not severity_map:
        severity_map = {}
    
    # Create component nodes
    for i, component in enumerate(components[:10]):  # Limit to 10 components
        node_id = f"C{i}"
        severity = severity_map.get(component, "medium")
        
        # Style based on severity
        if severity == "high":
            style_class = "highImpact"
        elif severity == "critical":
            style_class = "criticalImpact"
        else:
            style_class = "normalImpact"
        
        diagram += f'    {node_id}[{component}]:::{style_class}\n'
        diagram += f"    CRASH --> {node_id}\n"
    
    diagram += """    end
    
    classDef criticalImpact fill:#ff6b6b,stroke:#c92a2a,stroke-width:3px
    classDef highImpact fill:#ffa94d,stroke:#fd7e14,stroke-width:2px
    classDef normalImpact fill:#8ce99a,stroke:#51cf66,stroke-width:1px
"""
    
    return diagram

def generate_resource_usage_diagram(metrics: Dict[str, Any]) -> str:
    """
    Generate resource usage visualization
    
    Args:
        metrics: Resource usage metrics
        
    Returns:
        Mermaid pie chart code
    """
    diagram = """pie title Resource Usage at Crash Time
"""
    
    # Example metrics
    if metrics.get("memory_percent"):
        diagram += f'    "Memory Used" : {metrics["memory_percent"]}\n'
        diagram += f'    "Memory Free" : {100 - metrics["memory_percent"]}\n'
    
    if metrics.get("cpu_percent"):
        diagram += f'    "CPU Active" : {metrics["cpu_percent"]}\n'
        diagram += f'    "CPU Idle" : {100 - metrics["cpu_percent"]}\n'
    
    return diagram

def generate_analysis_summary_diagram(analysis_results: Dict[str, Any]) -> str:
    """
    Generate a summary diagram of the entire analysis
    
    Args:
        analysis_results: Complete analysis results
        
    Returns:
        Mermaid diagram code
    """
    severity = analysis_results.get("severity", "Unknown")
    confidence = analysis_results.get("confidence", 0)
    
    diagram = f"""graph LR
    subgraph "Crash Analysis Summary"
    A[File Analyzed] --> B[Root Cause Identified]
    B --> C[Severity: {severity}]
    C --> D[Confidence: {confidence:.0%}]
    
    B --> E[Immediate Actions]
    B --> F[Long-term Fixes]
    
    style A fill:#e3f2fd,stroke:#1976d2
    style B fill:#fff3e0,stroke:#f57c00
    style C fill:#ffebee,stroke:#d32f2f
    style D fill:#e8f5e9,stroke:#388e3c
    end
"""
    
    return diagram

def generate_error_pattern_diagram(patterns: List[Dict[str, str]]) -> str:
    """
    Generate a diagram showing error pattern relationships
    
    Args:
        patterns: List of detected patterns
        
    Returns:
        Mermaid diagram code
    """
    if not patterns:
        return ""
    
    diagram = """graph TD
    subgraph "Detected Patterns"
"""
    
    # Group patterns by category
    categories = {}
    for pattern in patterns:
        category = pattern.get("category", "other")
        if category not in categories:
            categories[category] = []
        categories[category].append(pattern["pattern"])
    
    # Create nodes for each category and pattern
    cat_index = 0
    for category, pattern_list in categories.items():
        cat_node = f"CAT{cat_index}"
        diagram += f'    {cat_node}["{category.upper()}"]:::category\n'
        
        for i, pattern_name in enumerate(pattern_list[:5]):  # Limit patterns per category
            pattern_node = f"P{cat_index}_{i}"
            diagram += f'    {pattern_node}["{pattern_name}"]:::pattern\n'
            diagram += f"    {cat_node} --> {pattern_node}\n"
        
        cat_index += 1
    
    diagram += """    end
    
    classDef category fill:#64b5f6,stroke:#1976d2,stroke-width:2px
    classDef pattern fill:#aed581,stroke:#689f38,stroke-width:1px
"""
    
    return diagram