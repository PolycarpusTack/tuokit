# 🤖 Agent Hub Documentation

## Overview

The Agent Hub is TuoKit's unified AI agent orchestration system, designed to coordinate multiple specialized AI agents for complex task execution. Built following TuoKit Architect principles: minimalist, practical, and extensible.

## Table of Contents

1. [Architecture](#architecture)
2. [Core Components](#core-components)
3. [Agent Types](#agent-types)
4. [Usage Guide](#usage-guide)
5. [Pipeline System](#pipeline-system)
6. [API Reference](#api-reference)
7. [Development Guide](#development-guide)
8. [Migration from Legacy](#migration-from-legacy)

## Architecture

The Agent Hub follows a modular architecture with clear separation of concerns:

```
toolkits/agent_hub/
├── __init__.py      # Main exports and version info
├── core.py          # Base classes and data structures
├── specialists.py   # Domain-specific agent implementations
├── orchestrator.py  # Agent coordination and selection logic
├── pipelines.py     # Pipeline execution strategies
└── ui.py           # Streamlit user interface components
```

### Design Principles

1. **Modularity**: Each component has a single responsibility
2. **Extensibility**: Easy to add new agents and tools
3. **Error Resilience**: Comprehensive error handling and retry logic
4. **Performance**: Support for parallel execution and caching5. **Knowledge Capture**: Automatic logging to knowledge base

## Core Components

### 1. AgentType Enum
Defines the types of agents available in the system:
- `SPECIALIST`: Single-domain expert agents
- `TEAM`: Multi-agent collaborative teams
- `META`: Agents that manage other agents
- `PIPELINE`: Sequential task processing
- `EDUCATIONAL`: Learning-focused agents with explanations
- `ORCHESTRATOR`: Master coordination agents

### 2. AgentState Class
Manages the complete execution state:
```python
@dataclass
class AgentState:
    goal: str                    # The task to accomplish
    phase: str                   # Current phase (planning/execution/validation)
    steps: List[Dict]           # Execution plan
    current_step: int           # Progress tracker
    attempts: int               # Retry counter
    max_retries: int = 3        # Maximum retry attempts
    results: Dict               # Execution results
    agent_history: List[str]    # Agent interaction log
    execution_log: List[Dict]   # Detailed execution log
```

### 3. BaseAgent Class
Abstract base class for all agents:
- Provides common functionality for tool execution
- Handles knowledge base logging
- Implements validation methods
- Manages agent capabilities reporting

## Agent Types

### CodeAgent
**Specialization**: Code generation, analysis, and optimization  
**Available Tools**:
- `code_explainer`: Detailed code explanations
- `code_generator`: Generate code from requirements
- `code_reviewer`: Review code for issues and improvements
- `test_generator`: Generate comprehensive test suites

### SQLAgent
**Specialization**: SQL query generation and database operations  
**Available Tools**:
- `sql_generator`: Generate SQL queries from natural language
- `sql_optimizer`: Optimize existing queries for performance
- `sql_explainer`: Explain query execution plans

### DocAgent
**Specialization**: Documentation analysis and generation  
**Available Tools**:
- `doc_qa`: Answer questions based on documents
- `doc_summarizer`: Create concise summaries
- `doc_generator`: Generate various documentation types

### AnalysisAgent
**Specialization**: Error analysis and system optimization  
**Available Tools**:
- `error_decoder`: Analyze and explain errors
- `performance_analyzer`: Identify performance bottlenecks
- `security_scanner`: Scan for security vulnerabilities

## Usage Guide

### Basic Usage

#### 1. Single Agent Execution
```python
from toolkits.agent_hub import CodeAgent

# Create a code agent
agent = CodeAgent()

# Execute a tool
result = agent.execute_tool(
    tool="code_generator",
    params={
        "task": "Create a Python function to calculate fibonacci",
        "model": "deepseek-coder:6.7b"
    }
)
```

#### 2. Orchestrated Execution
```python
from toolkits.agent_hub import AgentOrchestrator

# Create orchestrator
orchestrator = AgentOrchestrator()

# Execute complex goal
result = orchestrator.orchestrate(
    goal="Build a REST API for user management with database schema and tests"
)
```

#### 3. Pipeline Execution
```python
from toolkits.agent_hub import PipelineExecutor

# Define pipeline steps
steps = [
    {
        "agent": "sql",
        "tool": "sql_generator",        "params": {"query": "user management schema"},
        "description": "Generate database schema"
    },
    {
        "agent": "code",
        "tool": "code_generator",
        "params": {"task": "REST API endpoints", "language": "Python"},
        "description": "Generate API code"
    }
]

# Execute pipeline
executor = PipelineExecutor()
result = executor.execute("advanced", steps)
```

## Pipeline System

The Agent Hub supports multiple pipeline execution strategies:

### 1. Simple Pipeline
- Sequential execution of steps
- Stops on first failure
- No retry logic

### 2. Parallel Pipeline
- Executes independent steps concurrently
- Groups steps by dependencies
- Uses thread pool for parallelization

### 3. Advanced Pipeline
- **Retry Logic**: Automatic retry with configurable attempts
- **Caching**: Caches successful results for reuse
- **Intelligent Routing**: Dynamic step modification based on results
- **Error Recovery**: Continues execution after failures when possible

### 4. Educational Pipeline- Adds explanations at each step
- Designed for learning and understanding
- Shows reasoning behind each action

### 5. Creative Pipeline
- Can modify its own execution steps
- Uses AI to optimize pipeline dynamically
- Tracks creative modifications

## API Reference

### AgentOrchestrator

```python
orchestrator = AgentOrchestrator()

# Select best agent for a task
agent = orchestrator.select_agent(goal="Generate SQL query")

# Execute with automatic agent selection
result = orchestrator.orchestrate(
    goal="Your task description",
    strategy="auto"  # or "full_stack", "debug"
)
```

### PipelineExecutor

```python
executor = PipelineExecutor()

# Execute pipeline with specific strategy
result = executor.execute(
    pipeline_type="advanced",  # simple, parallel, advanced, educational, creative
    steps=[...],              # List of step definitions
    state=AgentState(...)     # Optional state object
)
```

### Agent Tools

Each agent exposes tools through the `execute_tool` method:

```python
result = agent.execute_tool(
    tool="tool_name",
    params={
        "model": "model_name",     # AI model to use
        "task": "description",     # Task description
        # Additional tool-specific parameters
    }
)
```

## Development Guide

### Adding a New Agent

1. Create your agent class in `specialists.py`:
```python
class CustomAgent(BaseAgent):
    def __init__(self):
        super().__init__(
            name="Custom Specialist",
            description="Your agent description",
            tools=["tool1", "tool2"],
            agent_type=AgentType.SPECIALIST
        )
    
    def _get_tool_map(self):
        return {
            "tool1": self._execute_tool1,
            "tool2": self._execute_tool2
        }
    
    def _execute_tool1(self, params):
        # Tool implementation
        pass
```