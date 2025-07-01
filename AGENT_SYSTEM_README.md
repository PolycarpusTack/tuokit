# TuoKit Agent System Implementation Guide

## 🚀 Quick Start

The TuoKit Agent System adds orchestrated AI capabilities to your existing tools. Here's how to get started:

### 1. Basic Usage

```python
from agent_system import AgentOrchestrator

# Initialize orchestrator
orchestrator = AgentOrchestrator()

# Execute a goal with auto-selected agent
state = orchestrator.execute_goal("Generate SQL query for top customers by revenue")

# Or specify an agent
state = orchestrator.execute_goal("Debug this Python code", agent_name="code_architect")
```

### 2. Access via Streamlit

Navigate to the Agent Portal page in TuoKit, or run:
```bash
streamlit run pages/agent_portal.py
```

## 🤖 Available Agents

### Specialist Agents

1. **DataEngineer**
   - Tools: sql_generator, sql_optimizer, sql_pipeline
   - Use for: Database queries, ETL pipelines, data analysis
   
2. **CodeArchitect**
   - Tools: code_explainer, error_decoder, regex_generator
   - Use for: Code debugging, generation, regex patterns

3. **DocScientist**
   - Tools: doc_qa
   - Use for: Document analysis, Q&A, summarization

### Team Agents

1. **ProjectBuilder**
   - Members: DataEngineer + CodeArchitect + DocScientist
   - Use for: Complex multi-step projects

2. **DataPipeline**
   - Members: DataEngineer + CodeArchitect
   - Use for: Data extraction and visualization projects
## 🏗️ Architecture Overview

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────┐
│ Agent Portal UI │────▶│ Orchestrator │────▶│   Agents    │
└─────────────────┘     └──────────────┘     └─────────────┘
                               │                      │
                               ▼                      ▼
                        ┌─────────────┐        ┌──────────┐
                        │ PostgreSQL  │        │  Tools   │
                        │ Knowledge DB│        │ (Existing)│
                        └─────────────┘        └──────────┘
```

### Key Components

1. **AgentState** - Tracks execution progress
   - Goal definition
   - Execution phases (planning → execution → validation)
   - Step tracking with retry logic
   - Result aggregation

2. **BaseAgent** - Foundation for all agents
   - Tool execution with error handling
   - Automatic knowledge capture
   - Plan generation using Ollama

3. **AgentOrchestrator** - Central coordinator
   - Agent selection (manual or AI-driven)
   - State management
   - Result logging

## 🔧 Integration with Existing Tools

The agent system seamlessly integrates with existing TuoKit tools:

```python
# Agents reuse existing tool functions
def _execute_code_explainer(self, params: Dict) -> str:
    from pages.code_tools import explain_code
    return explain_code(params['code'], params.get('model', 'deepseek-coder:6.7b'))
```

This means:
- No duplication of logic
- Consistent behavior across direct tool use and agent execution
- Automatic knowledge capture for all agent activities
## 📋 Example Workflows

### Example 1: Sales Dashboard Creation
```python
# Goal: "Create a sales dashboard for Q4 2024"
# Auto-selects: DataEngineer agent

# Execution plan:
# 1. Generate SQL for Q4 sales data
# 2. Optimize query for performance  
# 3. Create visualization pipeline

state = orchestrator.execute_goal("Create a sales dashboard for Q4 2024")
```

### Example 2: Legacy Code Migration
```python
# Goal: "Migrate Flask app to FastAPI"
# Selects: ProjectBuilder team

# Team coordination:
# - DocScientist: Analyzes existing code structure
# - CodeArchitect: Generates migration plan
# - DataEngineer: Updates database queries
```

### Example 3: Error Investigation
```python
# Direct agent selection for specific task
state = orchestrator.execute_goal(
    "Debug TypeError in user authentication module",
    agent_name="code_architect"
)
```

## 🧹 Cleanup Opportunities Identified

1. **Tool Consolidation**
   - `test_sql_*.py` files could be consolidated into a single test suite
   - Multiple SQL-related tools could share common utility functions

2. **Knowledge Capture Enhancement**
   - Add structured metadata to agent executions
   - Create agent-specific knowledge repositories

3. **Error Handling Standardization**
   - Implement consistent error formats across all tools
   - Add automatic error pattern learning

## 🚀 Next Steps

1. **Immediate Actions**
   - Test agent system with existing workflows
   - Add more specialized agents as needed
   - Monitor performance and adjust retry logic

2. **Future Enhancements**
   - Meta-agent for creating new agents
   - Agent performance analytics dashboard
   - Cross-agent knowledge sharing
   - Human-in-the-loop approval for critical operations

## 🔐 Security Considerations

- Agents inherit tool permissions
- All executions logged to PostgreSQL
- Configurable retry limits prevent infinite loops
- Model selection restricted to approved list

## 📝 Notes

- Agents use minimal models (deepseek-r1:1.5b) for planning to optimize speed
- Specialized models (deepseek-coder:6.7b) used for domain-specific tasks
- Team agents automatically handle task dependencies
- Knowledge capture happens automatically for all agent activities

---

**Remember the TuoKit Architect principle**: Build fast, build smart, build exactly what's needed. This agent system provides orchestration without over-engineering, leveraging existing tools while adding intelligent coordination.
