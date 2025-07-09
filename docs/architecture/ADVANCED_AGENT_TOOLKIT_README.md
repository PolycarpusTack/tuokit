# TuoKit Advanced Agent Toolkit

A comprehensive multi-agent system that integrates the best AI agents from awesome-llm-apps, adapted for TuoKit's local Ollama environment.

## 🚀 Features

### Specialized Agents

1. **System Architect Agent** 🏗️
   - Analyzes requirements and designs system architecture
   - Provides structured decisions with rationale
   - Estimates costs and identifies risks
   - Generates implementation roadmaps

2. **Deep Research Agent** 🔍
   - Performs multi-step research on technical topics
   - Synthesizes findings into actionable insights
   - Simulates web research with local models
   - Provides confidence scores and citations

3. **Enhanced Code Agent** 💻
   - Generates production-ready code with documentation
   - Analyzes code complexity and quality
   - Creates comprehensive unit tests
   - Supports multiple programming languages

4. **Data Analysis Agent** 📊
   - Converts natural language to SQL queries
   - Analyzes query performance
   - Suggests optimal visualizations
   - Provides security and optimization recommendations

5. **Debugging Agent** 🐛
   - Analyzes errors with root cause analysis
   - Provides step-by-step debugging approaches
   - Generates fixed code with explanations
   - Offers prevention strategies

6. **Documentation Agent** 📚
   - Generates technical documentation
   - Creates user guides and API docs
   - Synthesizes information from all agents
   - Formats in clean Markdown

### Multi-Agent Orchestration

The toolkit includes a sophisticated orchestrator that:
- Automatically selects appropriate agents based on goals
- Manages shared context between agents
- Chains agent outputs for complex workflows
- Tracks execution history and artifacts

## 🛠️ Installation

1. **Ensure Ollama is running** with required models:
   ```bash
   ollama pull deepseek-r1:1.5b
   ollama pull deepseek-coder:6.7b
   ollama pull llama3.2:latest
   ollama pull nomic-embed-text:latest
   ```

2. **Install Python dependencies**:
   ```bash
   pip install streamlit pandas
   ```

3. **Optional: Set up vector database** (for future enhancements):
   ```bash
   docker run -p 6333:6333 qdrant/qdrant
   ```

## 📖 Usage

### Option 1: Streamlit UI (Recommended)

Run the full UI with all features:

```bash
streamlit run advanced_agent_toolkit.py
```

This provides:
- Visual agent selection
- Multi-agent goal execution
- Individual agent interfaces
- Artifact management
- Result visualization

### Option 2: Command Line Testing

Test individual agents or run demos:

```bash
# Run quick demo
python test_agent_toolkit.py demo

# Test specific agents
python test_agent_toolkit.py architect    # System architect
python test_agent_toolkit.py research     # Deep research
python test_agent_toolkit.py code         # Code generation
python test_agent_toolkit.py data         # Data analysis
python test_agent_toolkit.py debug        # Debugging
python test_agent_toolkit.py orchestrate  # Multi-agent
python test_agent_toolkit.py docs         # Documentation
```

### Option 3: Python Integration

Use agents programmatically in your code:

```python
from advanced_agent_toolkit import (
    SystemArchitectAgent,
    MultiAgentOrchestrator,
    AgentContext
)

# Single agent usage
architect = SystemArchitectAgent()
context = AgentContext(goal="Build a web app")
result = architect.analyze_requirements(
    "Create a scalable e-commerce platform",
    context
)

# Multi-agent orchestration
orchestrator = MultiAgentOrchestrator()
result = orchestrator.execute_complex_goal(
    "Build a complete REST API with documentation"
)
```

## 🎯 Example Use Cases

### 1. Full Application Development
```
Goal: "Build a real-time chat application with user authentication"
Agents: Architect → Researcher → Coder → Documenter
```

### 2. Code Problem Solving
```
Goal: "Create a Python function to process CSV files with error handling"
Agents: Coder → Debugger → Documenter
```

### 3. System Analysis
```
Goal: "Analyze and optimize our database queries"
Agents: Data Analyst → Documenter
```

### 4. Technical Research
```
Goal: "Research best practices for microservices authentication"
Agents: Researcher → Documenter
```

## 🔧 Configuration

### Model Selection

The toolkit uses these default models:
- **Reasoning**: `deepseek-r1:1.5b`
- **Coding**: `deepseek-coder:6.7b`
- **General**: `llama3.2:latest`
- **Embedding**: `nomic-embed-text:latest`

You can change models in the Streamlit UI sidebar or programmatically:

```python
architect = SystemArchitectAgent(model="your-model:tag")
```

### Agent Selection

The orchestrator automatically selects agents based on keywords in your goal:
- "build", "create", "develop" → Full stack (Architect, Research, Code, Docs)
- "analyze", "data", "query" → Data analysis
- "debug", "fix", "error" → Debugging
- "research", "investigate" → Research

Or manually specify agents:

```python
from advanced_agent_toolkit import AgentRole

orchestrator.execute_complex_goal(
    goal="Your goal",
    selected_agents=[AgentRole.ARCHITECT, AgentRole.CODER]
)
```

## 📊 Understanding Results

### Artifacts

Each agent produces artifacts stored in the shared context:
- `architecture_decision` - System design details
- `research_*` - Research findings
- `code_*` - Generated code and tests
- `data_analysis_*` - SQL queries and analysis
- `debug_*` - Error analysis and fixes
- `documentation_*` - Generated documentation

### Execution Timeline

The orchestrator tracks:
- Agent execution order
- Success/failure status
- Timestamps
- Generated artifacts

## 🔍 Architecture

```
MultiAgentOrchestrator
    ├── SystemArchitectAgent
    │   └── Reasoning → Structure → Roadmap
    ├── DeepResearchAgent
    │   └── Questions → Research → Synthesis
    ├── EnhancedCodeAgent
    │   └── Generate → Analyze → Test
    ├── DataAnalysisAgent
    │   └── NL→SQL → Analyze → Visualize
    ├── DebuggingAgent
    │   └── Analyze → Fix → Prevent
    └── DocumentationAgent
        └── Gather → Synthesize → Format
```

## 🎨 Customization

### Adding New Agents

```python
class CustomAgent:
    def __init__(self, model=None):
        self.model = model or "your-model"
    
    def execute(self, task, context):
        # Your agent logic
        result = {"output": "..."}
        context.add_artifact("custom_result", result)
        return result

# Register with orchestrator
orchestrator.agents[AgentRole.CUSTOM] = CustomAgent()
```

### Modifying Agent Behavior

Each agent's prompts and logic can be customized by:
1. Editing the prompt templates in the agent classes
2. Adjusting the analysis methods
3. Changing the output formats

## 🐛 Troubleshooting

### Common Issues

1. **"No models found"**
   - Ensure Ollama is running: `ollama serve`
   - Pull required models: `ollama pull model-name`

2. **"Connection error"**
   - Check if PostgreSQL is running (if using database features)
   - Verify Ollama is accessible at `http://localhost:11434`

3. **"Memory error"**
   - Reduce depth parameter in research agent
   - Use smaller models
   - Process fewer artifacts at once

### Debug Mode

Enable detailed logging:

```python
import logging
logging.basicConfig(level=logging.DEBUG)
```

## 🚀 Next Steps

1. **Integrate with TuoKit**: Add to your existing TuoKit pages
2. **Add Vector Search**: Implement Qdrant for better knowledge retrieval
3. **Custom Agents**: Create domain-specific agents
4. **API Endpoints**: Expose agents via REST API
5. **Batch Processing**: Handle multiple goals in parallel

## 📚 References

- Based on agents from [awesome-llm-apps](https://github.com/Shubhamsaboo/awesome-llm-apps)
- Adapted for local execution with Ollama
- Integrated with TuoKit's knowledge management system

## 📝 License

Same as TuoKit project
