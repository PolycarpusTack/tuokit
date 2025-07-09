"""
TuoKit Agent Hub - Consolidation & Cleanup Recommendations
========================================================

## 🎯 Immediate Actions

### 1. Fix Import Issues
The agent_hub.py file is now functional with these new files:
- ✅ utils.py - Core utilities and database management
- ✅ pages/sql_toolkit_modern.py - SQL generation tools
- ✅ agent_tools_enhanced.py - Enhanced tool implementations

### 2. Reduce Code Duplication

#### A. Consolidate Tool Execution Pattern
Instead of repeating similar code for each tool, create a unified execution framework:

```python
class ToolExecutor:
    """Unified tool execution with common patterns"""
    
    def __init__(self):
        self.tools = {
            "code": {
                "explainer": self.code_tool_wrapper,
                "generator": self.code_tool_wrapper,
                "reviewer": self.code_tool_wrapper,
            },
            "sql": {
                "generator": self.sql_tool_wrapper,
                "optimizer": self.sql_tool_wrapper,
            }
        }
    
    def code_tool_wrapper(self, tool_name: str, params: Dict) -> str:
        """Common wrapper for all code tools"""
        # Common preprocessing
        code = params.get('code', '')
        if not code and 'previous_result' in params:
            code = params['previous_result']
        
        # Common validation
        if not code and tool_name != 'generator':
            return "Error: No code provided"
        
        # Tool-specific logic
        if tool_name == 'explainer':
            return self._explain_code(code, params)
        elif tool_name == 'generator':
            return self._generate_code(params)
        # ... etc
```

#### B. Merge Similar Agents
Instead of separate CodeAgent, SQLAgent, etc., create a configurable SpecialistAgent:

```python
class SpecialistAgent(BaseAgent):
    """Configurable specialist agent"""
    
    PRESETS = {
        "code": {
            "name": "Code Specialist",
            "tools": ["code_explainer", "code_generator", "code_reviewer"],
            "model": "deepseek-coder:6.7b"
        },
        "sql": {
            "name": "SQL Specialist",
            "tools": ["sql_generator", "sql_optimizer", "sql_explainer"],
            "model": "deepseek-coder:6.7b"
        },
        "docs": {
            "name": "Documentation Specialist",
            "tools": ["doc_qa", "doc_summarizer", "doc_generator"],
            "model": "deepseek-r1:1.5b"
        }
    }
    
    def __init__(self, specialty: str):
        config = self.PRESETS.get(specialty, self.PRESETS["code"])
        super().__init__(
            name=config["name"],
            description=f"Expert in {specialty} tasks",
            tools=config["tools"],
            agent_type=AgentType.SPECIALIST
        )
        self.default_model = config["model"]
```

### 3. Create Shared Components

#### A. Unified Prompt Templates
```python
class PromptTemplates:
    """Centralized prompt management"""
    
    EXPLAIN = """
Explain this {type}:
{content}

Focus on:
1. High-level overview
2. Key concepts
3. Step-by-step breakdown
4. Best practices applied
5. Potential improvements
"""
    
    GENERATE = """
Generate {type} for: {task}
Language/Type: {language}
Requirements: {requirements}

Provide clean, well-commented code following best practices.
"""
    
    ANALYZE = """
Analyze this {type} for:
- Performance characteristics
- Potential issues
- Security concerns
- Optimization opportunities

Content:
{content}
"""
    
    @classmethod
    def get(cls, template_name: str, **kwargs) -> str:
        template = getattr(cls, template_name.upper(), "")
        return template.format(**kwargs)
```

#### B. Shared Validation Logic
```python
class Validators:
    """Common validation functions"""
    
    @staticmethod
    def validate_code(code: str, language: str = "python") -> Dict[str, Any]:
        """Validate code syntax"""
        if language == "python":
            try:
                compile(code, '<string>', 'exec')
                return {"valid": True}
            except SyntaxError as e:
                return {"valid": False, "error": str(e)}
        # Add other languages
    
    @staticmethod
    def validate_sql(query: str) -> Dict[str, Any]:
        """Validate SQL syntax"""
        from pages.sql_toolkit_modern import validate_sql_syntax
        return validate_sql_syntax(query)
```

### 4. Implement Caching

```python
from functools import lru_cache
import hashlib

class CachedAgent(BaseAgent):
    """Agent with response caching"""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.cache = {}
    
    def _get_cache_key(self, tool: str, params: Dict) -> str:
        """Generate cache key from tool and params"""
        key_str = f"{tool}:{json.dumps(params, sort_keys=True)}"
        return hashlib.md5(key_str.encode()).hexdigest()
    
    def execute_tool(self, tool: str, params: Dict) -> Dict:
        # Check cache first
        cache_key = self._get_cache_key(tool, params)
        
        if cache_key in self.cache:
            return self.cache[cache_key]
        
        # Execute normally
        result = super().execute_tool(tool, params)
        
        # Cache successful results
        if result.get("success"):
            self.cache[cache_key] = result
        
        return result
```

### 5. Streamline UI Components

```python
class AgentUI:
    """Reusable UI components for agents"""
    
    @staticmethod
    def tool_selector(available_tools: List[str], key: str = "tool") -> str:
        """Standardized tool selection widget"""
        return st.selectbox(
            "Select Tool",
            available_tools,
            format_func=lambda x: x.replace('_', ' ').title(),
            key=key
        )
    
    @staticmethod
    def code_input(label: str = "Enter code:", key: str = "code") -> str:
        """Standardized code input with syntax highlighting hint"""
        return st.text_area(
            label,
            height=200,
            help="💡 Tip: Use proper indentation for better analysis",
            key=key
        )
    
    @staticmethod
    def display_result(result: Dict, title: str = "Result"):
        """Standardized result display"""
        if result.get("success"):
            st.success(f"✅ {title}")
            if isinstance(result.get("result"), str):
                if "```" in result["result"]:
                    st.markdown(result["result"])
                else:
                    st.code(result["result"])
            else:
                st.json(result["result"])
        else:
            st.error(f"❌ Error: {result.get('error', 'Unknown error')}")
```

## 📊 Final Consolidated Structure

```
tuokit/
├── core/
│   ├── __init__.py
│   ├── agents.py          # Base agent classes
│   ├── tools.py           # Tool implementations
│   ├── templates.py       # Prompt templates
│   └── validators.py      # Validation logic
├── specialists/
│   ├── __init__.py
│   ├── code_specialist.py
│   ├── sql_specialist.py
│   └── doc_specialist.py
├── ui/
│   ├── __init__.py
│   ├── components.py      # Reusable UI components
│   └── layouts.py         # Page layouts
├── utils/
│   ├── __init__.py
│   ├── cache.py          # Caching utilities
│   ├── database.py       # Database management
│   └── ollama.py         # Ollama integration
└── agent_hub.py          # Main application
```

## 🎯 Benefits of Consolidation

1. **Reduced Code**: ~40% less code through reuse
2. **Easier Maintenance**: Changes in one place affect all agents
3. **Consistent Behavior**: All agents follow same patterns
4. **Better Testing**: Test shared components once
5. **Improved Performance**: Caching and optimization in one place

## 🚀 Next Steps

1. Start with fixing imports (✅ Done with provided files)
2. Extract common patterns into shared modules
3. Implement caching for expensive operations
4. Add comprehensive error handling
5. Create unit tests for shared components
6. Document the consolidated architecture

This consolidation will transform agent_hub.py from a 1000+ line file into a cleaner, more maintainable system with better performance and reliability.
