# TuoKit Pattern Extraction Design Document

## Executive Summary

This document outlines how TuoKit will extract and implement the best patterns from existing coding assistants (Goose, OpenHands, Tabby, Claude Code) without attempting direct code conversion or merging.

## Pattern Extraction Matrix

| Source Tool | Pattern to Extract | TuoKit Implementation | Complexity |
|-------------|-------------------|----------------------|------------|
| Goose | MCP Protocol Concept | Simplified Python Protocol | Medium |
| Goose | Extension System | Plugin Architecture | Low |
| Goose | Shell Integration | Subprocess Wrapper | Low |
| OpenHands | Agent Framework | Base Agent Classes | Medium |
| OpenHands | Memory Management | SQLAlchemy Models | Low |
| OpenHands | State Preservation | PostgreSQL Storage | Low |
| Tabby | Local RAG Indexing | AST + SQLite | High |
| Tabby | Privacy-First | Local-Only Processing | Low |
| Claude Code | Context Compaction | Smart Summarization | High |
| Claude Code | Transparency | Token Counter UI | Medium |

## Detailed Component Designs

### 1. Plugin System (Goose-Inspired)

```python
# tuokit/core/plugin_system.py
"""
TuoKit Plugin System - Inspired by Goose's extension architecture
Simplified for Python/Streamlit environment
"""

from abc import ABC, abstractmethod
from typing import Dict, List, Any
import importlib
import inspect

class TuoKitPlugin(ABC):
    """Base plugin class inspired by Goose extensions"""
    
    def __init__(self, name: str, tuokit_core):
        self.name = name
        self.core = tuokit_core
        self.tools = {}
        self._register_tools()
    
    @abstractmethod
    def get_capabilities(self) -> List[str]:
        """Return list of capabilities this plugin provides"""
        pass
    
    @abstractmethod
    def get_prompt_additions(self) -> str:
        """Return any prompt additions for this plugin"""
        pass
    
    def _register_tools(self):
        """Auto-register methods decorated with @tool"""
        for name, method in inspect.getmembers(self, predicate=inspect.ismethod):
            if hasattr(method, '_is_tool'):
                self.tools[name] = {
                    'handler': method,
                    'metadata': getattr(method, '_tool_metadata', {})
                }

def tool(description: str, parameters: Dict[str, Any] = None):
    """Decorator for plugin tools - inspired by Goose's #[tool] macro"""
    def decorator(func):
        func._is_tool = True
        func._tool_metadata = {
            'description': description,
            'parameters': parameters or {}
        }
        return func
    return decorator

# Example Plugin
class CodeExplainerPlugin(TuoKitPlugin):
    """Example plugin showing the pattern"""
    
    def get_capabilities(self) -> List[str]:
        return ["explain_code", "analyze_complexity", "suggest_improvements"]
    
    def get_prompt_additions(self) -> str:
        return "You can explain code using the explain_code tool."
    
    @tool("Explains Python code with detailed analysis")
    def explain_code(self, code: str, level: str = "intermediate") -> str:
        # Implementation here
        pass
```

### 2. Agent Framework (OpenHands-Inspired)

```python
# tuokit/agents/base.py
"""
TuoKit Agent Framework - Inspired by OpenHands' agent architecture
Simplified without Docker/K8s complexity
"""

from enum import Enum
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
import asyncio

class AgentState(Enum):
    IDLE = "idle"
    THINKING = "thinking"
    EXECUTING = "executing"
    WAITING = "waiting"
    COMPLETE = "complete"
    ERROR = "error"

@dataclass
class AgentMemory:
    """Simplified memory inspired by OpenHands"""
    short_term: List[Dict[str, Any]]  # Current session
    long_term: Dict[str, Any]  # Persisted in PostgreSQL
    context_window: int = 4000  # Token limit
    
    def add_interaction(self, role: str, content: str):
        self.short_term.append({
            "role": role,
            "content": content,
            "timestamp": datetime.now()
        })

class TuoKitAgent(ABC):
    """Base agent class inspired by OpenHands but simplified"""
    
    def __init__(self, name: str, model: str = "deepseek-r1"):
        self.name = name
        self.model = model
        self.state = AgentState.IDLE
        self.memory = AgentMemory([], {})
        self.capabilities = self._define_capabilities()
    
    @abstractmethod
    def _define_capabilities(self) -> List[str]:
        """Define what this agent can do"""
        pass
    
    @abstractmethod
    async def process_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Main processing logic"""
        pass
    
    def save_state(self, db_connection):
        """Save agent state to PostgreSQL"""
        # Implementation
        pass

# Specialized Agents
class CodeAnalysisAgent(TuoKitAgent):
    """Agent specialized in code analysis"""
    
    def _define_capabilities(self) -> List[str]:
        return [
            "analyze_code_quality",
            "detect_patterns",
            "suggest_refactoring",
            "security_scan"
        ]
    
    async def process_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        self.state = AgentState.THINKING
        # Process code analysis request
        # Return structured results
        pass
```

### 3. Context Management (Claude Code-Inspired)

```python
# tuokit/core/context_manager.py
"""
TuoKit Context Manager - Inspired by Claude Code's /compact
Intelligent summarization and transparent tracking
"""

import tiktoken
from typing import List, Dict, Tuple
import streamlit as st

class ContextManager:
    """Manages conversation context with intelligent compression"""
    
    def __init__(self, max_tokens: int = 4000):
        self.max_tokens = max_tokens
        self.encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")
        self.messages = []
        self.preserved_keys = ["code", "requirements", "decisions", "errors"]
    
    def add_message(self, role: str, content: str, metadata: Dict = None):
        """Add message with metadata for intelligent preservation"""
        tokens = len(self.encoding.encode(content))
        self.messages.append({
            "role": role,
            "content": content,
            "tokens": tokens,
            "metadata": metadata or {},
            "timestamp": datetime.now()
        })
    
    def compact_context(self) -> Tuple[List[Dict], Dict[str, Any]]:
        """
        Intelligent context compaction inspired by Claude Code's /compact
        Returns: (compacted_messages, summary_stats)
        """
        # Separate messages by type
        preserved = []
        compressible = []
        
        for msg in self.messages:
            # Preserve important content
            if any(key in msg.get("metadata", {}) for key in self.preserved_keys):
                preserved.append(msg)
            else:
                compressible.append(msg)
        
        # Summarize compressible messages
        if compressible:
            summary = self._summarize_messages(compressible)
            preserved.insert(0, {
                "role": "system",
                "content": f"Previous conversation summary: {summary}",
                "tokens": len(self.encoding.encode(summary)),
                "metadata": {"type": "summary"}
            })
        
        stats = {
            "original_messages": len(self.messages),
            "preserved_messages": len(preserved),
            "tokens_before": sum(m["tokens"] for m in self.messages),
            "tokens_after": sum(m["tokens"] for m in preserved)
        }
        
        self.messages = preserved
        return preserved, stats
    
    def _summarize_messages(self, messages: List[Dict]) -> str:
        """Summarize messages intelligently"""
        # Group by topic/intent
        # Extract key points
        # Return concise summary
        pass
    
    def get_token_usage(self) -> Dict[str, int]:
        """Get current token usage - transparency like Claude Code"""
        total = sum(msg["tokens"] for msg in self.messages)
        by_type = {}
        for msg in self.messages:
            msg_type = msg.get("metadata", {}).get("type", "general")
            by_type[msg_type] = by_type.get(msg_type, 0) + msg["tokens"]
        
        return {
            "total": total,
            "remaining": self.max_tokens - total,
            "by_type": by_type,
            "percentage": (total / self.max_tokens) * 100
        }

# Streamlit UI Component
def render_context_manager(ctx_manager: ContextManager):
    """Render context usage in Streamlit UI"""
    usage = ctx_manager.get_token_usage()
    
    col1, col2, col3 = st.columns([2, 1, 1])
    
    with col1:
        st.progress(usage["percentage"] / 100)
        st.caption(f"Context: {usage['total']:,}/{ctx_manager.max_tokens:,} tokens")
    
    with col2:
        if st.button("🗜️ Compact", help="Intelligently compress context"):
            _, stats = ctx_manager.compact_context()
            st.success(f"Reduced from {stats['tokens_before']:,} to {stats['tokens_after']:,} tokens")
    
    with col3:
        if st.button("📊 Details"):
            st.json(usage["by_type"])
```

### 4. Local RAG System (Tabby-Inspired)

```python
# tuokit/indexing/local_rag.py
"""
TuoKit Local RAG - Inspired by Tabby's privacy-first indexing
Simplified for Python projects
"""

import ast
import sqlite3
from pathlib import Path
from typing import List, Dict, Optional
import hashlib
from dataclasses import dataclass

@dataclass
class CodeEntity:
    """Represents an indexed code entity"""
    file_path: str
    entity_type: str  # function, class, method
    name: str
    signature: str
    docstring: Optional[str]
    source_code: str
    line_start: int
    line_end: int
    hash: str

class LocalCodeIndex:
    """Local code indexing inspired by Tabby"""
    
    def __init__(self, project_path: str, index_path: str = "tuokit_index.db"):
        self.project_path = Path(project_path)
        self.index_path = index_path
        self._init_database()
    
    def _init_database(self):
        """Initialize SQLite database for index"""
        self.conn = sqlite3.connect(self.index_path)
        self.conn.execute("""
            CREATE TABLE IF NOT EXISTS code_entities (
                id INTEGER PRIMARY KEY,
                file_path TEXT,
                entity_type TEXT,
                name TEXT,
                signature TEXT,
                docstring TEXT,
                source_code TEXT,
                line_start INTEGER,
                line_end INTEGER,
                hash TEXT UNIQUE,
                indexed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Create FTS5 table for full-text search
        self.conn.execute("""
            CREATE VIRTUAL TABLE IF NOT EXISTS code_search 
            USING fts5(
                name, signature, docstring, source_code,
                content=code_entities
            )
        """)
    
    def index_project(self, file_patterns: List[str] = ["*.py"]):
        """Index entire project"""
        for pattern in file_patterns:
            for file_path in self.project_path.rglob(pattern):
                if self._should_index(file_path):
                    self._index_file(file_path)
    
    def _index_file(self, file_path: Path):
        """Index a single Python file using AST"""
        try:
            content = file_path.read_text()
            tree = ast.parse(content)
            
            for node in ast.walk(tree):
                entity = self._extract_entity(node, file_path, content)
                if entity:
                    self._store_entity(entity)
        except Exception as e:
            print(f"Error indexing {file_path}: {e}")
    
    def _extract_entity(self, node: ast.AST, file_path: Path, content: str) -> Optional[CodeEntity]:
        """Extract indexable entity from AST node"""
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            return self._extract_function(node, file_path, content)
        elif isinstance(node, ast.ClassDef):
            return self._extract_class(node, file_path, content)
        return None
    
    def search(self, query: str, limit: int = 10) -> List[Dict]:
        """Search indexed code using FTS5"""
        cursor = self.conn.execute("""
            SELECT 
                ce.file_path, ce.entity_type, ce.name, 
                ce.signature, ce.docstring, ce.source_code,
                ce.line_start, ce.line_end
            FROM code_search cs
            JOIN code_entities ce ON cs.rowid = ce.id
            WHERE code_search MATCH ?
            ORDER BY rank
            LIMIT ?
        """, (query, limit))
        
        return [dict(zip([col[0] for col in cursor.description], row)) 
                for row in cursor.fetchall()]
    
    def get_context_for_file(self, file_path: str) -> List[Dict]:
        """Get all entities in a file for context"""
        cursor = self.conn.execute("""
            SELECT * FROM code_entities 
            WHERE file_path = ?
            ORDER BY line_start
        """, (file_path,))
        
        return [dict(zip([col[0] for col in cursor.description], row)) 
                for row in cursor.fetchall()]
```

### 5. Integration Layer

```python
# tuokit/core/tuokit_core.py
"""
TuoKit Core - Integrates all pattern-inspired components
"""

import streamlit as st
from typing import Dict, List, Optional
import asyncio
from concurrent.futures import ThreadPoolExecutor

class TuoKitCore:
    """Main TuoKit application core"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        
        # Initialize components
        self.plugin_manager = PluginManager()
        self.agent_manager = AgentManager()
        self.context_manager = ContextManager(
            max_tokens=config.get("max_context_tokens", 4000)
        )
        self.local_index = LocalCodeIndex(
            project_path=config["project_path"]
        )
        
        # Database connections
        self.init_database()
        
        # Thread pool for async operations
        self.executor = ThreadPoolExecutor(max_workers=4)
    
    def init_database(self):
        """Initialize PostgreSQL connection"""
        # Setup PostgreSQL for:
        # - Knowledge base
        # - Agent memory
        # - User preferences
        # - Tool usage statistics
        pass
    
    async def process_user_request(self, request: str, context: Dict = None):
        """Main request processing pipeline"""
        
        # 1. Analyze request intent
        intent = await self._analyze_intent(request)
        
        # 2. Route to appropriate agent/plugin
        if intent["type"] == "code_explanation":
            agent = self.agent_manager.get_agent("code_analyzer")
            result = await agent.process_request({
                "request": request,
                "context": context,
                "index": self.local_index
            })
        
        # 3. Update context
        self.context_manager.add_message("user", request, {"intent": intent})
        self.context_manager.add_message("assistant", result["response"], 
                                       {"type": intent["type"]})
        
        # 4. Store in knowledge base
        self._store_interaction(request, result, intent)
        
        return result
    
    def render_ui(self):
        """Main Streamlit UI"""
        st.set_page_config(
            page_title="TuoKit - AI Development Assistant",
            page_icon="🔧",
            layout="wide"
        )
        
        # Sidebar
        with st.sidebar:
            st.title("🔧 TuoKit")
            
            # Context manager widget
            render_context_manager(self.context_manager)
            
            # Active plugins
            st.subheader("Active Plugins")
            for plugin in self.plugin_manager.get_active_plugins():
                st.write(f"✅ {plugin.name}")
            
            # Index status
            st.subheader("Code Index")
            stats = self.local_index.get_stats()
            st.metric("Indexed Files", stats["file_count"])
            st.metric("Code Entities", stats["entity_count"])
        
        # Main area
        tab1, tab2, tab3, tab4 = st.tabs([
            "💬 Chat", "📊 Knowledge Base", "🔍 Code Search", "⚙️ Settings"
        ])
        
        with tab1:
            self._render_chat_interface()
        
        with tab2:
            self._render_knowledge_base()
        
        with tab3:
            self._render_code_search()
        
        with tab4:
            self._render_settings()
```

## Implementation Phases

### Phase 1: Foundation (Week 1-2)
1. Basic plugin system
2. Simple context manager
3. PostgreSQL setup
4. Core Streamlit UI

### Phase 2: Intelligence (Week 3-4)
1. Agent framework
2. Local code indexing
3. Context compaction algorithm
4. Basic knowledge base

### Phase 3: Integration (Week 5-6)
1. Plugin marketplace
2. Advanced search
3. Team collaboration features
4. Performance optimization

### Phase 4: Polish (Week 7-8)
1. UI/UX improvements
2. Documentation
3. Test coverage
4. Deployment tools

## Technical Decisions

### Why These Patterns?

1. **Plugin System (Goose)**: Extensibility without complexity
2. **Agent Framework (OpenHands)**: Specialized capabilities
3. **Context Management (Claude Code)**: User control and transparency
4. **Local Indexing (Tabby)**: Privacy and speed

### What We're NOT Doing

1. **NOT** implementing MCP protocol fully (too complex)
2. **NOT** using Docker/K8s (unnecessary complexity)
3. **NOT** building custom language servers
4. **NOT** implementing model serving

## Success Metrics

1. **Simplicity**: Can a developer understand the code in 30 minutes?
2. **Performance**: Does it respond in <2 seconds?
3. **Utility**: Does it solve real developer problems?
4. **Extensibility**: Can users add custom plugins easily?

## Conclusion

This design extracts the best patterns from existing tools while maintaining TuoKit's philosophy of simplicity and practicality. We're not trying to replicate these tools - we're learning from them to build something uniquely valuable.
