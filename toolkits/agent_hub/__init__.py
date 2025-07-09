"""
TuoKit Agent Hub Toolkit
Modular AI agent system with orchestration capabilities
"""

from .core import AgentType, AgentState, BaseAgent
from .specialists import CodeAgent, SQLAgent, DocAgent, AnalysisAgent
from .orchestrator import AgentOrchestrator
from .pipelines import PipelineExecutor
from .registry import AGENT_REGISTRY, register_agent, get_agent
from .ui import AgentHubUI
from .quick_builder import quick_agent, quick_pipeline
from .memory import AgentMemory, BroadcastMemoryPatterns
from .templates import AgentTemplates
from .multimodal import (
    MultiModalAgent, ScreenshotAnalyzer, DocumentAnalyzer,
    render_screenshot_analyzer, render_document_analyzer,
    ClientPatterns
)
from .agent_manager import AgentManager, AgentConfig
from .agent_builder import AgentBuilder

__all__ = [
    'AgentType',
    'AgentState', 
    'BaseAgent',
    'CodeAgent',
    'SQLAgent',
    'DocAgent',
    'AnalysisAgent',
    'AgentOrchestrator',
    'PipelineExecutor',
    'AGENT_REGISTRY',
    'register_agent',
    'get_agent',
    'AgentHubUI',
    'quick_agent',
    'quick_pipeline',
    'AgentMemory',
    'BroadcastMemoryPatterns',
    'AgentTemplates',
    'MultiModalAgent',
    'ScreenshotAnalyzer',
    'DocumentAnalyzer',
    'render_screenshot_analyzer',
    'render_document_analyzer',
    'ClientPatterns',
    'AgentManager',
    'AgentConfig',
    'AgentBuilder'
]

# Version info
__version__ = '2.0.0'
__author__ = 'TuoKit Team'
