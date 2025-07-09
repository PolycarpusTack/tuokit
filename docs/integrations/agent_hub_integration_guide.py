"""
Agent Hub Integration Guide
How to integrate the improvements into agent_hub.py
"""

# ========== STEP 1: Import Enhanced Tools ==========
# Add this to the imports section of agent_hub.py:

from agent_tools_enhanced import (
    enhanced_code_analyzer,
    enhanced_sql_analyzer,
    enhanced_error_analyzer,
    generate_unit_tests
)

# ========== STEP 2: Update Tool Implementations ==========
# Replace the generic implementations in BaseAgent class:

class BaseAgent:
    # ... existing code ...
    
    def _execute_code_explainer(self, params: Dict) -> str:
        """Enhanced code explanation with analysis"""
        code = params.get('code', '')
        
        # First, analyze the code structure
        analysis = enhanced_code_analyzer(code, params.get('language', 'python'))
        
        # Build comprehensive explanation prompt
        prompt = f"""
Explain this code in detail:

```{params.get('language', 'python')}
{code}
```

Code Analysis:
- Complexity: Cyclomatic {analysis['complexity'].get('cyclomatic', 'N/A')}
- Patterns: {', '.join(analysis['patterns']) if analysis['patterns'] else 'None detected'}
- Issues: {', '.join(analysis['issues']) if analysis['issues'] else 'No issues found'}

Please provide:
1. High-level overview
2. Step-by-step explanation
3. Key concepts used
4. Potential improvements
"""
        
        response = safe_ollama_generate(
            params.get('model', 'deepseek-coder:6.7b'), 
            prompt
        )
        
        # Combine analysis and explanation
        result = f"## Code Analysis\n"
        result += f"- **Complexity Score**: {analysis['complexity'].get('cyclomatic', 'N/A')}\n"
        result += f"- **Lines of Code**: {analysis['metrics'].get('code_lines', 0)}\n"
        result += f"- **Patterns Found**: {', '.join(analysis['patterns'])}\n\n"
        result += f"## Explanation\n{response['response']}"
        
        return result
    
    def _execute_test_generator(self, params: Dict) -> str:
        """Generate comprehensive tests using enhanced generator"""
        code = params.get('code', '')
        framework = params.get('framework', 'pytest')
        
        # Generate basic test structure
        test_code = generate_unit_tests(code, framework)
        
        # Enhance with LLM for specific test cases
        enhancement_prompt = f"""
Given this code:
```python
{code}
```

And this test skeleton:
```python
{test_code}
```

Generate specific test cases with:
1. Valid input examples
2. Edge cases
3. Error conditions
4. Performance considerations

Make the tests comprehensive and ready to run.
"""
        
        enhanced_response = safe_ollama_generate(
            'deepseek-coder:6.7b',
            enhancement_prompt
        )
        
        return enhanced_response['response']
    
    def _execute_error_decoder(self, params: Dict) -> str:
        """Enhanced error analysis with actionable solutions"""
        error = params.get('error', '')
        context = params.get('context', '')
        
        # Get structured analysis
        analysis = enhanced_error_analyzer(error, context)
        
        # Build comprehensive response
        result = f"## Error Analysis: {analysis['error_type']}\n\n"
        result += f"**Severity**: {analysis['severity'].upper()}\n\n"
        result += f"**Likely Cause**: {analysis['likely_cause']}\n\n"
        
        result += "**Solutions**:\n"
        for i, solution in enumerate(analysis['solutions'], 1):
            result += f"{i}. {solution}\n"
        
        if analysis['related_docs']:
            result += "\n**Related Documentation**:\n"
            for doc in analysis['related_docs']:
                result += f"- {doc}\n"
        
        # Get additional context from LLM
        context_prompt = f"""
For this error: {error}
In this context: {context}

Provide:
1. A specific code example that would cause this error
2. The corrected version of that code
3. Prevention tips
"""
        
        llm_response = safe_ollama_generate('deepseek-coder:6.7b', context_prompt)
        result += f"\n## Detailed Explanation\n{llm_response['response']}"
        
        return result

# ========== STEP 3: Add Pipeline Data Flow ==========

def execute_pipeline_with_context(steps: List[Dict]) -> Dict:
    """Execute pipeline with proper data flow between steps"""
    results = {}
    context = {}
    orchestrator = AgentOrchestrator()
    
    for i, step in enumerate(steps):
        step_name = step.get("name", f"Step {i+1}")
        tool = step["tool"]
        params = step.get("params", {})
        
        # Check dependencies
        if step.get("depends_on"):
            for dep in step["depends_on"]:
                if dep not in results or not results[dep].get("success"):
                    results[step_name] = {
                        "success": False,
                        "error": f"Dependency '{dep}' failed"
                    }
                    continue
        
        # Inject previous result if requested
        if step.get("use_previous_result") and i > 0:
            prev_step_name = steps[i-1].get("name", f"Step {i}")
            if prev_step_name in results:
                params["previous_result"] = results[prev_step_name].get("result", "")
        
        # Parameter templating
        for key, value in params.items():
            if isinstance(value, str) and "{{" in value:
                # Simple template replacement
                for ctx_key, ctx_value in context.items():
                    value = value.replace(f"{{{{{ctx_key}}}}}", str(ctx_value))
                params[key] = value
        
        # Execute tool
        try:
            agent = orchestrator.select_agent(tool)
            result = agent.execute_tool(tool, params)
            
            results[step_name] = result
            
            # Store in context for future steps
            if result.get("success"):
                context[step_name] = result.get("result", "")
                
                # Auto-detect useful context
                if tool == "code_generator":
                    context["last_code"] = result.get("result", "")
                elif tool == "error_decoder":
                    context["last_error_analysis"] = result.get("result", "")
        
        except Exception as e:
            results[step_name] = {
                "success": False,
                "error": str(e)
            }
    
    return {
        "results": results,
        "context": context,
        "success": all(r.get("success", False) for r in results.values())
    }

# ========== STEP 4: Add Agent Memory ==========

class MemoryAgent(BaseAgent):
    """Agent with conversation memory"""
    
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.memory = []
        self.max_memory = 10
    
    def remember(self, interaction: Dict):
        """Store interaction in memory"""
        self.memory.append({
            "timestamp": datetime.now().isoformat(),
            "tool": interaction.get("tool"),
            "input": interaction.get("input"),
            "output": interaction.get("output")
        })
        
        # Keep memory size manageable
        if len(self.memory) > self.max_memory:
            self.memory = self.memory[-self.max_memory:]
    
    def get_context(self) -> str:
        """Get memory context for prompts"""
        if not self.memory:
            return ""
        
        context = "Previous interactions:\n"
        for mem in self.memory[-3:]:  # Last 3 interactions
            context += f"- {mem['tool']}: {mem['input'][:50]}...\n"
        
        return context
    
    def execute_tool(self, tool: str, params: Dict) -> Dict:
        """Execute tool with memory context"""
        # Add memory context to params
        params["context"] = self.get_context()
        
        # Execute normally
        result = super().execute_tool(tool, params)
        
        # Remember this interaction
        self.remember({
            "tool": tool,
            "input": str(params),
            "output": str(result)
        })
        
        return result

# ========== STEP 5: Add Quick Actions UI ==========

def show_quick_actions():
    """Quick action buttons for common tasks"""
    st.subheader("⚡ Quick Actions")
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        if st.button("🐛 Debug Error", use_container_width=True):
            st.session_state.quick_action = "debug"
            
    with col2:
        if st.button("📝 Generate Code", use_container_width=True):
            st.session_state.quick_action = "generate"
            
    with col3:
        if st.button("🔍 Analyze Code", use_container_width=True):
            st.session_state.quick_action = "analyze"
            
    with col4:
        if st.button("🧪 Create Tests", use_container_width=True):
            st.session_state.quick_action = "test"
    
    # Handle quick actions
    if "quick_action" in st.session_state:
        action = st.session_state.quick_action
        
        if action == "debug":
            error_input = st.text_area(
                "Paste your error message:",
                placeholder="TypeError: unsupported operand type(s) for +: 'int' and 'str'"
            )
            
            if error_input and st.button("Analyze Error"):
                agent = AnalysisAgent()
                result = agent.execute_tool("error_decoder", {
                    "error": error_input,
                    "context": "Python development"
                })
                
                st.markdown(result["result"])
                
        elif action == "generate":
            task = st.text_area(
                "What code do you need?",
                placeholder="Create a function that validates email addresses"
            )
            
            language = st.selectbox("Language", ["Python", "JavaScript", "SQL", "Go"])
            
            if task and st.button("Generate Code"):
                agent = CodeAgent()
                result = agent.execute_tool("code_generator", {
                    "task": task,
                    "language": language
                })
                
                st.code(result["result"], language=language.lower())
                
        elif action == "analyze":
            code_input = st.text_area(
                "Paste your code:",
                height=200
            )
            
            if code_input and st.button("Analyze Code"):
                agent = CodeAgent()
                result = agent.execute_tool("code_explainer", {
                    "code": code_input,
                    "language": "python"
                })
                
                st.markdown(result["result"])
                
        elif action == "test":
            code_input = st.text_area(
                "Paste code to test:",
                height=200
            )
            
            framework = st.selectbox("Test Framework", ["pytest", "unittest", "jest", "mocha"])
            
            if code_input and st.button("Generate Tests"):
                agent = CodeAgent()
                result = agent.execute_tool("test_generator", {
                    "code": code_input,
                    "framework": framework
                })
                
                st.code(result["result"], language="python")

# ========== Integration Complete ==========
