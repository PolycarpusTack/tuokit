# TuoKit Lite Agent System

## 🚀 Overview

The Lite Agent System adds simple, practical automation to TuoKit without complexity:

1. **🔄 Pipeline Automator** - Chain tools together for multi-step workflows
2. **🎓 Educational Companion** - Get real-time guidance while using tools

## 📋 Quick Start

### 1. Run Database Migration

```bash
psql -U ollama_user -d ollama_knowledge -f database_migration_lite_agents.sql
```

### 2. Access the Agent Portal

1. Start TuoKit: `streamlit run app.py`
2. Navigate to "🤖 Agent Lite" in the sidebar

## 🔄 Pipeline Automator

### Creating a Pipeline

1. Click "➕ Add Step" to add workflow steps
2. For each step:
   - Give it a descriptive name
   - Select a tool (sql_generator, code_explainer, etc.)
   - Configure tool-specific parameters
3. Click "▶️ Execute Pipeline" to run all steps

### Example Pipelines

#### Data Analysis Workflow
```
Step 1: SQL Generator → "Get customer orders from last month"
Step 2: Regex Generator → "Extract email addresses"  
Step 3: Doc Summarizer → "Create executive summary"
```

#### Code Migration Helper
```
Step 1: Code Explainer → Analyze legacy code
Step 2: Code Generator → Generate modern version
Step 3: Error Decoder → Fix any issues
```

### Pipeline Features

- **Sequential Execution**: Each step runs in order
- **Result Passing**: Later steps can access earlier results
- **Error Handling**: Pipeline continues even if a step fails
- **Auto-Save**: Successful pipelines are saved to knowledge base
- **Templates**: Load pre-built pipelines with one click

## 🎓 Educational Companion

### Getting Guidance

1. Describe what you're working on
2. Select your current action:
   - Selecting the right tool
   - Configuring tool parameters
   - Understanding tool output
   - Debugging errors
   - Optimizing workflow
   - Learning best practices
3. Click "💡 Get Guidance"

### Guidance Components

Each guidance response includes:
- **📖 Explanation** - What this action does
- **💡 Pro Tip** - Best practice advice
- **⚠️ Common Mistake** - What to avoid
- **➡️ Next Step** - Suggested action

### Example Scenarios

| Context | Action | Guidance |
|---------|--------|----------|
| "Analyzing CSV files" | "Selecting the right tool" | Use SQL Generator for structured queries, Regex for data cleaning |
| "SQL query too slow" | "Optimizing workflow" | Add LIMIT for testing, use indexes, consider materialized views |
| "PDF text extraction" | "Configuring parameters" | Start with Doc Q&A tool, adjust chunk size for large files |

## 📊 Database Schema

### Tables Added

1. **pipelines** - Stores executed pipelines
   - name, steps (JSONB), results (JSONB)
   - execution_time_ms, success status
   
2. **pipeline_templates** - Pre-built workflow templates
   - Includes starter templates for common tasks
   
3. **educational_guidance** - Guidance history (optional)

### Analytics Views

- **pipeline_analytics** - Daily pipeline execution stats
- **popular_tools** - Most used tools and success rates

## 🛠️ Technical Details

### Pipeline Execution Flow

```python
# Pipeline structure
pipeline = [
    {
        "name": "Step Name",
        "tool": "tool_name",
        "params": {
            "param1": "value1",
            "param2": "value2"
        }
    }
]

# Results structure  
results = {
    "results": {
        "Step Name": "output..."
    },
    "log": [
        {
            "step": "Step Name",
            "tool": "tool_name",
            "success": true,
            "timestamp": "2024-01-20T10:30:00"
        }
    ]
}
```

### Adding New Tools

To add a tool to the pipeline system:

1. Add tool name to the tools list in `agent_lite.py`
2. Add parameter UI in the tool-specific section
3. Add execution logic in `run_pipeline()`

### Educational Agent Prompting

The Educational Agent uses structured prompts to ensure consistent, helpful guidance:

```python
prompt = f"""
User is working on: {context}
Current action: {action}

Provide helpful guidance in JSON format:
{
    "explanation": "One clear sentence",
    "tip": "Best practice tip",
    "mistake": "Common mistake to avoid",
    "next_step": "Suggested next action"
}
"""
```

## 💡 Best Practices

### For Pipeline Building
1. **Start Simple** - Test with 2-3 steps first
2. **Name Steps Clearly** - Makes debugging easier
3. **Test Individual Tools** - Before adding to pipeline
4. **Save Successful Pipelines** - Reuse as templates

### For Learning
1. **Be Specific** - Detailed context gets better guidance
2. **Follow the Flow** - Apply guidance before moving on
3. **Export History** - Review your learning journey
4. **Try Scenarios** - Use pre-built examples to learn

## 🚨 Troubleshooting

### Pipeline Issues
- **Tool not found**: Check tool name spelling
- **Empty results**: Verify parameters are filled
- **Execution fails**: Check individual tool pages

### Educational Agent Issues
- **Generic guidance**: Provide more specific context
- **JSON errors**: Agent will fallback to default guidance
- **Slow response**: Normal for first request

## 🎯 Use Cases

### Business Users
- Automate report generation workflows
- Chain data extraction and analysis
- Create reusable templates

### Developers  
- Multi-step code migrations
- Error analysis pipelines
- Documentation workflows

### Learners
- Understand tool selection
- Learn parameter optimization
- Avoid common mistakes

## 📈 Future Enhancements

Potential additions (keeping it simple):
- Pipeline scheduling
- Conditional steps
- Parallel execution
- More tool integrations

---

The Lite Agent System delivers practical automation without complexity, following TuoKit's philosophy of building exactly what's needed.
