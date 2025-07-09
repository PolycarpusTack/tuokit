"""
Quick Agent Builder - Create custom agents on the fly
Simple, practical, no overengineering
"""

from typing import Dict, List, Any, Optional
from .core import BaseAgent, AgentType
from .registry import register_agent
import uuid


class QuickAgent(BaseAgent):
    """A simple agent created on-the-fly"""
    
    def __init__(self, name: str, tools: List[str], tool_implementations: Dict[str, Any]):
        self.tool_implementations = tool_implementations
        super().__init__(
            name=name,
            description=f"Quick agent for {name}",
            tools=tools,
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self) -> Dict[str, Any]:
        """Use provided tool implementations"""
        return self.tool_implementations
    
    def execute_tool(self, tool: str, params: Dict) -> Any:
        """Execute tool with fallback to base implementation"""
        if tool in self.tool_implementations:
            return self.tool_implementations[tool](params)
        else:
            return super().execute_tool(tool, params)


def quick_agent(name: str, tools: List[str], 
                prompts: Optional[Dict[str, str]] = None) -> QuickAgent:
    """
    Create a simple agent quickly
    
    Example:
        agent = quick_agent(
            name="Broadcast Monitor",
            tools=["check_stream", "analyze_quality"],
            prompts={
                "check_stream": "Check if {stream_url} is live and stable",
                "analyze_quality": "Analyze video quality metrics for {stream_url}"
            }
        )
    """
    # Generate tool implementations from prompts
    tool_implementations = {}
    
    if prompts:
        from utils import safe_ollama_generate
        
        for tool_name, prompt_template in prompts.items():
            def make_tool(template):
                def tool_func(params):
                    # Simple template filling
                    prompt = template
                    for key, value in params.items():
                        prompt = prompt.replace(f"{{{key}}}", str(value))
                    
                    response = safe_ollama_generate(
                        model=params.get('model', 'deepseek-r1:1.5b'),
                        prompt=prompt
                    )
                    return response['response']
                return tool_func
            
            tool_implementations[tool_name] = make_tool(prompt_template)
    
    # Create and return the agent
    agent = QuickAgent(name, tools, tool_implementations)
    
    # Optionally register it
    agent_id = f"quick_{name.lower().replace(' ', '_')}_{uuid.uuid4().hex[:8]}"
    register_agent(agent_id, agent)
    
    return agent


def quick_pipeline(steps: List[Dict[str, str]], name: Optional[str] = None) -> Dict[str, Any]:
    """
    Create a quick pipeline from simple step definitions
    
    Example:
        pipeline = quick_pipeline([
            {"do": "analyze this error log", "with": "error_decoder"},
            {"do": "generate a fix", "with": "code_generator"},
            {"do": "create documentation", "with": "doc_generator"}
        ])
    """
    from .registry import find_agent_by_tool
    
    pipeline_steps = []
    
    for i, step in enumerate(steps):
        task = step.get("do", f"Step {i+1}")
        tool = step.get("with", "")
        
        # Find which agent has this tool
        agent_name = find_agent_by_tool(tool)
        if not agent_name:
            agent_name = "code"  # Default fallback
        
        pipeline_steps.append({
            "agent": agent_name,
            "tool": tool,
            "params": {"task": task},
            "description": task
        })
    
    return {
        "name": name or "Quick Pipeline",
        "pipeline": pipeline_steps,
        "type": "simple"
    }
