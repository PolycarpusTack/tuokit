"""
Knowledge Capture Wrapper for TuoKit Agents
Adds automatic knowledge capture to all agent operations
"""

import time
import json
from typing import Dict, Any, Optional
from functools import wraps

# Import the enhanced knowledge capture
try:
    from knowledge_capture_enhanced import (
        capture_knowledge_enhanced,
        capture_agent_execution,
        capture_tool_execution,
        knowledge_capture,
        AgentKnowledgeEntry
    )
    KNOWLEDGE_CAPTURE_AVAILABLE = True
except ImportError:
    KNOWLEDGE_CAPTURE_AVAILABLE = False
    print("Warning: Enhanced knowledge capture not available")

# Also import original capture for compatibility
try:
    from utils import capture_knowledge as original_capture
    ORIGINAL_CAPTURE_AVAILABLE = True
except ImportError:
    ORIGINAL_CAPTURE_AVAILABLE = False

def capture_wrapper(agent_name: str, tool_name: str):
    """
    Decorator to automatically capture knowledge from agent operations
    
    Usage:
        @capture_wrapper("MyAgent", "my_tool")
        def my_agent_function(params):
            # ... agent logic ...
            return result
    """
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            start_time = time.time()
            
            # Extract parameters
            params = {}
            if args and isinstance(args[0], dict):
                params = args[0]
            elif kwargs:
                params = kwargs
            
            # Execute the function
            try:
                result = func(*args, **kwargs)
                success = True
                error_msg = None
                
                # Check if result indicates failure
                if isinstance(result, dict) and not result.get('success', True):
                    success = False
                    error_msg = result.get('error', 'Unknown error')
                    
            except Exception as e:
                result = {"success": False, "error": str(e)}
                success = False
                error_msg = str(e)
            
            # Calculate execution time
            execution_time = time.time() - start_time
            
            # Capture the operation
            if KNOWLEDGE_CAPTURE_AVAILABLE:
                try:
                    # Determine category based on tool name
                    category = determine_category(tool_name)
                    subcategory = determine_subcategory(tool_name, params)
                    
                    # Enhanced capture
                    capture_knowledge_enhanced(
                        tool=f"{agent_name}_{tool_name}",
                        category=category,
                        subcategory=subcategory,
                        prompt=json.dumps(params, indent=2),
                        response=json.dumps(result, indent=2) if isinstance(result, dict) else str(result),
                        metadata={
                            "agent": agent_name,
                            "execution_time": execution_time,
                            "param_count": len(params)
                        },
                        agent_name=agent_name,
                        success=success,
                        performance_metrics={
                            "execution_time_seconds": execution_time
                        }
                    )
                    
                    # Also capture tool execution
                    capture_tool_execution(
                        agent_name=agent_name,
                        tool=tool_name,
                        params=params,
                        result=result if isinstance(result, dict) else {"result": result},
                        execution_time=execution_time
                    )
                    
                except Exception as e:
                    print(f"Knowledge capture error: {e}")
            
            # Fallback to original capture
            elif ORIGINAL_CAPTURE_AVAILABLE:
                try:
                    original_capture(
                        tool=f"{agent_name}_{tool_name}",
                        model="deepseek-r1:1.5b",
                        prompt=str(params),
                        response=str(result)
                    )
                except Exception as e:
                    print(f"Original capture error: {e}")
            
            return result
        
        return wrapper
    return decorator

def determine_category(tool_name: str) -> str:
    """Determine knowledge category from tool name"""
    tool_categories = {
        "code": ["code_generator", "code_reviewer", "code_explainer", "test_generator"],
        "sql": ["sql_generator", "sql_optimizer", "sql_explainer", "nl_to_sql"],
        "analytics": ["data_profiling", "analyze_dataframe", "pattern_finder"],
        "architecture": ["system_architect", "generate_architecture", "risk_assessor"],
        "research": ["deep_research", "research_topic", "web_research"],
        "error": ["error_decoder", "analyze_error", "debug_assistant"],
        "documentation": ["doc_generator", "doc_qa", "doc_summarizer"]
    }
    
    tool_lower = tool_name.lower()
    for category, tools in tool_categories.items():
        for tool in tools:
            if tool in tool_lower:
                return category
    
    return "general"

def determine_subcategory(tool_name: str, params: Dict) -> str:
    """Determine subcategory based on tool and parameters"""
    if "generate" in tool_name:
        return "generation"
    elif "analyze" in tool_name or "analysis" in tool_name:
        return "analysis"
    elif "optimize" in tool_name:
        return "optimization"
    elif "explain" in tool_name:
        return "explanation"
    elif "test" in tool_name:
        return "testing"
    
    return "general"

# Specific wrappers for common agent operations

def capture_architecture_design(agent_name: str = "SystemArchitect"):
    """Capture system architecture designs"""
    def decorator(func):
        @wraps(func)
        def wrapper(requirements: str, *args, **kwargs):
            start_time = time.time()
            
            # Execute
            architecture = func(requirements, *args, **kwargs)
            
            # Capture if successful
            if KNOWLEDGE_CAPTURE_AVAILABLE and "error" not in architecture:
                try:
                    entry = AgentKnowledgeEntry(
                        agent_name=agent_name,
                        tool_name="architecture_generator",
                        operation_type="design",
                        category="architecture",
                        subcategory="system_design",
                        title=f"Architecture: {architecture.get('system_name', 'Unknown')}",
                        content=json.dumps(architecture, indent=2),
                        input_data={"requirements": requirements},
                        output_data=architecture,
                        metadata={
                            "quality_score": architecture.get("quality_score", 0),
                            "component_count": len(architecture.get("architecture", {}).get("components", [])),
                            "risk_count": len(architecture.get("risks", []))
                        },
                        tags=["architecture", "system_design", agent_name.lower()],
                        confidence_score=architecture.get("quality_score", 0.8),
                        performance_metrics={
                            "execution_time": time.time() - start_time
                        }
                    )
                    
                    knowledge_capture.capture_agent_operation(entry)
                    
                except Exception as e:
                    print(f"Architecture capture error: {e}")
            
            return architecture
        
        return wrapper
    return decorator

def capture_research_findings(agent_name: str = "DeepResearch"):
    """Capture research findings"""
    def decorator(func):
        @wraps(func)
        def wrapper(topic: str, *args, **kwargs):
            start_time = time.time()
            
            # Execute
            report = func(topic, *args, **kwargs)
            
            # Capture findings
            if KNOWLEDGE_CAPTURE_AVAILABLE:
                try:
                    # Capture complete report
                    knowledge_capture.capture_analysis_result(
                        agent_name=agent_name,
                        analysis_type="research",
                        data_info={
                            "name": topic,
                            "depth": kwargs.get("depth", "comprehensive")
                        },
                        results=report
                    )
                    
                    # Capture individual insights
                    for i, insight in enumerate(report.get("key_insights", [])):
                        capture_knowledge_enhanced(
                            tool="insight_extraction",
                            category="research",
                            subcategory="insights",
                            prompt=topic,
                            response=insight,
                            metadata={
                                "research_topic": topic,
                                "insight_index": i
                            },
                            agent_name=agent_name
                        )
                    
                except Exception as e:
                    print(f"Research capture error: {e}")
            
            return report
        
        return wrapper
    return decorator

def capture_data_analysis(agent_name: str = "DataAnalysis"):
    """Capture data analysis results"""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            start_time = time.time()
            
            # Execute analysis
            results = func(*args, **kwargs)
            
            # Extract dataset info from args/kwargs
            dataset_name = "Unknown"
            if args and len(args) > 1:
                dataset_name = args[1]  # Second arg is usually dataset name
            elif "dataset_name" in kwargs:
                dataset_name = kwargs["dataset_name"]
            
            # Capture if successful
            if KNOWLEDGE_CAPTURE_AVAILABLE and isinstance(results, dict):
                try:
                    # Determine what was analyzed
                    row_count = 0
                    column_count = 0
                    
                    if "analyses" in results:
                        profile = results["analyses"].get("profile", {})
                        if "shape" in profile:
                            row_count, column_count = profile["shape"]
                    
                    knowledge_capture.capture_analysis_result(
                        agent_name=agent_name,
                        analysis_type="comprehensive_data_analysis",
                        data_info={
                            "name": dataset_name,
                            "rows": row_count,
                            "columns": column_count
                        },
                        results=results
                    )
                    
                except Exception as e:
                    print(f"Analysis capture error: {e}")
            
            return results
        
        return wrapper
    return decorator

# Example usage functions that wrap existing agents

def wrap_system_architect_agent():
    """Wrap SystemArchitectAgent with knowledge capture"""
    try:
        from agent_hub_enhancements import SystemArchitectAgent
        
        # Wrap the generate_architecture method
        original_method = SystemArchitectAgent.generate_architecture
        SystemArchitectAgent.generate_architecture = capture_architecture_design()(original_method)
        
        print("✅ SystemArchitectAgent wrapped with knowledge capture")
        return True
        
    except ImportError:
        print("❌ Could not import SystemArchitectAgent")
        return False

def wrap_deep_research_agent():
    """Wrap DeepResearchAgent with knowledge capture"""
    try:
        from agent_hub_enhancements import DeepResearchAgent
        
        # Wrap the research_topic method
        original_method = DeepResearchAgent.research_topic
        DeepResearchAgent.research_topic = capture_research_findings()(original_method)
        
        print("✅ DeepResearchAgent wrapped with knowledge capture")
        return True
        
    except ImportError:
        print("❌ Could not import DeepResearchAgent")
        return False

def wrap_data_analysis_agent():
    """Wrap DataAnalysisAgent with knowledge capture"""
    try:
        from data_analysis_enhanced import DataAnalysisAgentEnhanced
        
        # The enhanced agent already has comprehensive capture!
        print("✅ DataAnalysisAgent already has comprehensive knowledge capture")
        return True
        
    except ImportError:
        print("❌ Could not import DataAnalysisAgent")
        return False

# Auto-wrap function to be called on startup

def auto_wrap_all_agents():
    """Automatically wrap all agents with knowledge capture"""
    print("\n🎯 Auto-wrapping agents with knowledge capture...")
    
    wrapped_count = 0
    
    # Wrap each agent
    if wrap_system_architect_agent():
        wrapped_count += 1
    
    if wrap_deep_research_agent():
        wrapped_count += 1
    
    if wrap_data_analysis_agent():
        wrapped_count += 1
    
    print(f"\n✅ Wrapped {wrapped_count} agents with knowledge capture")
    
    return wrapped_count

# Usage in your application:
# 1. Import this module
# 2. Call auto_wrap_all_agents() on startup
# 3. All agent operations will be automatically captured!

if __name__ == "__main__":
    # Test the wrapper
    print("Testing knowledge capture wrapper...")
    
    # Example: wrap a simple function
    @capture_wrapper("TestAgent", "test_function")
    def test_function(x, y):
        return {"result": x + y, "success": True}
    
    # This will automatically capture the operation
    result = test_function(5, 3)
    print(f"Result: {result}")
    
    # Auto-wrap all agents
    auto_wrap_all_agents()
