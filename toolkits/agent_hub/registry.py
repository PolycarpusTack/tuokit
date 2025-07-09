"""
Agent Registry - Central registry for all agents
"""

from typing import Dict, Optional, Any
from .specialists import CodeAgent, SQLAgent, DocAgent, AnalysisAgent
from .core import BaseAgent


# Initialize agent instances
AGENT_REGISTRY: Dict[str, BaseAgent] = {
    "code": CodeAgent(),
    "sql": SQLAgent(),
    "docs": DocAgent(),
    "analysis": AnalysisAgent()
}

# Register multimodal agent if available
try:
    from .multimodal import MultiModalAgent
    AGENT_REGISTRY["multimodal"] = MultiModalAgent()
except ImportError:
    pass  # MultiModal features not available


def register_agent(name: str, agent: BaseAgent) -> None:
    """Register a new agent"""
    if not isinstance(agent, BaseAgent):
        raise ValueError("Agent must inherit from BaseAgent")
    
    AGENT_REGISTRY[name] = agent


def get_agent(name: str) -> Optional[BaseAgent]:
    """Get agent by name"""
    return AGENT_REGISTRY.get(name)


def list_agents() -> Dict[str, Dict[str, Any]]:
    """List all registered agents with their info"""
    return {
        name: agent.get_info() 
        for name, agent in AGENT_REGISTRY.items()
    }


def get_agent_tools(agent_name: str) -> list:
    """Get tools available for an agent"""
    agent = get_agent(agent_name)
    return agent.tools if agent else []


def find_agent_by_tool(tool_name: str) -> Optional[str]:
    """Find which agent has a specific tool"""
    for name, agent in AGENT_REGISTRY.items():
        if tool_name in agent.tools:
            return name
    return None
