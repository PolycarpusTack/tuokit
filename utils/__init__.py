# utils/__init__.py
"""
TuoKit Utilities Package
Modular utilities for clean code organization
"""

from .database import DatabaseManager, DB_CONFIG
from .ollama import OllamaManager, safe_ollama_generate, OllamaToolBase
from .system import get_system_stats, get_platform_info
from .help import get_contextual_help, get_tool_examples, get_all_tools_summary
from .knowledge import (
    KnowledgePattern, 
    KnowledgeExtractor,
    standardize_knowledge_entry,
    capture_knowledge
)
from .knowledge_graph import KnowledgeGraph, knowledge_graph
from .file_handler import extract_text, extract_text_from_url, validate_file_size
from .learning_strategy import SimpleLearningStrategy
from .content_validator import SimpleContentValidator, validate_with_ai
from .sql_tools import SQLTools, generate_sql, optimize_sql, explain_sql
from .performance_utils import RubyPerformance
from .testing_utils import TestGenerator
from .pattern_utils import PatternMatcher
from .concurrency_utils import ConcurrencyAnalyzer
from .graphql_utils import GraphQLHelper
from .memory_utils import MemoryPatterns, MemoryProfiler
from .upgrade_utils import RailsUpgrader, UpgradeAutomation
from .kata_utils import KataGenerator, KataAnalyzer
from .component_utils import ComponentBuilder, ComponentPatterns

__all__ = [
    'DatabaseManager',
    'DB_CONFIG', 
    'OllamaManager',
    'safe_ollama_generate',
    'OllamaToolBase',
    'get_system_stats',
    'get_platform_info',
    'get_contextual_help',
    'get_tool_examples',
    'get_all_tools_summary',
    'KnowledgePattern',
    'KnowledgeExtractor',
    'standardize_knowledge_entry',
    'capture_knowledge',
    'KnowledgeGraph',
    'knowledge_graph',
    'extract_text',
    'extract_text_from_url',
    'validate_file_size',
    'SimpleLearningStrategy',
    'SimpleContentValidator',
    'validate_with_ai',
    'SQLTools',
    'generate_sql',
    'optimize_sql', 
    'explain_sql',
    'RubyPerformance',
    'TestGenerator',
    'PatternMatcher',
    'ConcurrencyAnalyzer',
    'GraphQLHelper',
    'MemoryPatterns',
    'MemoryProfiler',
    'RailsUpgrader',
    'UpgradeAutomation',
    'KataGenerator',
    'KataAnalyzer',
    'ComponentBuilder',
    'ComponentPatterns'
]
