# TuoKit Utils Module
# Clean, organized imports for all utilities

# Database
from .database import DatabaseManager, test_database_connection, save_to_knowledge_base

# Ollama
from .ollama import (
    get_ollama_manager, 
    get_available_models, 
    safe_ollama_generate,
    test_ollama_setup
)

# Knowledge Management
from .knowledge import KnowledgeExtractor, capture_knowledge
# Note: knowledge_graph should be imported directly when needed to avoid circular imports
# from .knowledge_graph import knowledge_graph

# UI/Theme
from .modern_theme import apply_modern_theme, render_hero_section, render_tool_card

# Base Classes
from .tool_base import TuoKitToolBase

# Common Utilities
from .common_utils import *
from .file_handler import validate_file_size, extract_text, extract_text_from_url

# Navigation
from .navigation import *

# Help
from .help import get_contextual_help

# SQL
# from .sql_tools import SQLTools  # Module archived

# System
from .system import get_system_stats

__all__ = [
    # Database
    'DatabaseManager',
    'test_database_connection',
    'save_to_knowledge_base',
    
    # Ollama
    'get_ollama_manager',
    'get_available_models', 
    'safe_ollama_generate',
    'test_ollama_setup',
    
    # Knowledge
    'KnowledgeExtractor',
    'capture_knowledge',
    
    # UI/Theme
    'apply_modern_theme',
    'render_hero_section',
    'render_tool_card',
    
    # Base Classes
    'TuoKitToolBase',
    
    # Help
    'get_contextual_help',
    
    # SQL
        # File handling
    'validate_file_size',
    'extract_text',
    'extract_text_from_url',
    
    # System
    'get_system_stats'
]
