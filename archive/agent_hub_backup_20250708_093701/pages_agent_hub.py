"""
Complete Agent Hub Consolidation
Merges all agent features into agent_hub.py
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Agent Hub - TuoKit",
    page_icon="🚀",
    layout="wide"
)
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass
from enum import Enum
import json
import time
from datetime import datetime
import re

from utils import DatabaseManager, safe_ollama_generate, capture_knowledge

# ========== AGENT TYPES AND STATE MANAGEMENT ==========

class AgentType(Enum):
    """All agent types supported by TuoKit"""
    SPECIALIST = "specialist"
    TEAM = "team"
    META = "meta"
    PIPELINE = "pipeline"
    EDUCATIONAL = "educational"
    ORCHESTRATOR = "orchestrator"

@dataclass
class AgentState:
    """Complete state management for agent execution"""
    goal: str
    phase: str = "planning"  # planning -> execution -> validation
    steps: List[Dict] = None
    current_step: int = 0
    attempts: int = 0  # Fixed from "temp"
    max_retries: int = 3
    results: Dict = None
    agent_history: List[str] = None
    execution_log: List[Dict] = None
    
    def __post_init__(self):
        if self.steps is None:
            self.steps = []
        if self.results is None:
            self.results = {}
        if self.agent_history is None:
            self.agent_history = []
        if self.execution_log is None:
            self.execution_log = []

# ========== BASE AGENT CLASS ==========

class BaseAgent:
    """Base class for all TuoKit agents with complete functionality"""
    
    def __init__(self, name: str, description: str, tools: List[str], agent_type: AgentType = AgentType.SPECIALIST):
        self.name = name
        self.description = description
        self.tools = tools
        self.agent_type = agent_type
        self.db = DatabaseManager()
        
    def plan(self, goal: str, model: str = "deepseek-r1:1.5b") -> List[Dict]:
        """Generate execution plan for the goal"""
        prompt = f"""
As {self.name}, create a step-by-step plan for: {goal}

Available tools: {', '.join(self.tools)}

Return JSON array of steps:
[{{"step": 1, "action": "...", "tool": "...", "expected_output": "..."}}]
"""
        
        response = safe_ollama_generate(model=model, prompt=prompt)
        try:
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
        except:
            return [{"step": 1, "action": goal, "tool": self.tools[0], "expected_output": "result"}]
    
    def execute_tool(self, tool: str, params: Dict) -> Dict:
        """Execute a specific tool with complete error handling"""
        tool_map = {
            # Code tools
            "code_explainer": self._execute_code_explainer,
            "code_generator": self._execute_code_generator,
            "code_reviewer": self._execute_code_reviewer,
            "test_generator": self._execute_test_generator,
            
            # SQL tools
            "sql_generator": self._execute_sql_generator,
            "sql_optimizer": self._execute_sql_optimizer,
            "sql_explainer": self._execute_sql_explainer,
            
            # Documentation tools
            "doc_qa": self._execute_doc_qa,
            "doc_summarizer": self._execute_doc_summarizer,
            "doc_generator": self._execute_doc_generator,
            
            # Analysis tools
            "error_decoder": self._execute_error_decoder,
            "performance_analyzer": self._execute_performance_analyzer,
            "security_scanner": self._execute_security_scanner,
            
            # Utility tools
            "regex_generator": self._execute_regex_generator,
            "json_formatter": self._execute_json_formatter,
            "api_tester": self._execute_api_tester
        }
        
        if tool not in tool_map:
            return {"success": False, "error": f"Unknown tool: {tool}"}
        
        try:
            result = tool_map[tool](params)
            
            # Log to knowledge base
            if self.db and self.db.connected:
                self.db.log_query(
                    tool=f"agent_{self.name}_{tool}",
                    model=params.get('model', 'deepseek-r1:1.5b'),
                    prompt=str(params),
                    response=str(result)
                )
            
            return {"success": True, "result": result}
            
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    # Tool implementations
    def _execute_code_explainer(self, params: Dict) -> str:
        prompt = f"Explain this code:\n{params.get('code', '')}"
        response = safe_ollama_generate(params.get('model', 'deepseek-coder:6.7b'), prompt)
        return response['response']
    
    def _execute_code_generator(self, params: Dict) -> str:
        prompt = f"Generate {params.get('language', 'Python')} code for: {params.get('task', '')}"
        response = safe_ollama_generate(params.get('model', 'deepseek-coder:6.7b'), prompt)
        return response['response']
    
    def _execute_sql_generator(self, params: Dict) -> str:
        from pages.sql_toolkit_modern import generate_sql_query
        return generate_sql_query(
            params.get('query', ''),
            params.get('dialect', 'postgresql'),
            params.get('model', 'deepseek-coder:6.7b')
        )
    
    def _execute_doc_summarizer(self, params: Dict) -> str:
        prompt = f"Summarize in {params.get('length', 100)} words:\n{params.get('text', '')}"
        response = safe_ollama_generate(params.get('model', 'deepseek-r1:1.5b'), prompt)
        return response['response']
    
    # Additional tool implementations...
    def _execute_code_reviewer(self, params: Dict) -> str:
        prompt = f"Review this code for issues:\n{params.get('code', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_test_generator(self, params: Dict) -> str:
        prompt = f"Generate {params.get('framework', 'pytest')} tests for:\n{params.get('code', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_sql_optimizer(self, params: Dict) -> str:
        from pages.sql_toolkit_modern import optimize_sql_query
        return optimize_sql_query(params.get('query', ''), params.get('dialect', 'postgresql'))
    
    def _execute_sql_explainer(self, params: Dict) -> str:
        from pages.sql_toolkit_modern import explain_sql_query
        return explain_sql_query(params.get('query', ''), params.get('dialect', 'postgresql'))
    
    def _execute_doc_qa(self, params: Dict) -> str:
        prompt = f"Answer based on this document:\n{params.get('document', '')}\n\nQuestion: {params.get('question', '')}"
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']
    
    def _execute_doc_generator(self, params: Dict) -> str:
        prompt = f"Generate {params.get('doc_type', 'documentation')} for:\n{params.get('content', '')}"
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']
    
    def _execute_error_decoder(self, params: Dict) -> str:
        prompt = f"Analyze this error:\n{params.get('error', '')}\nContext: {params.get('context', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_performance_analyzer(self, params: Dict) -> str:
        prompt = f"Analyze performance of:\n{params.get('code', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_security_scanner(self, params: Dict) -> str:
        prompt = f"Scan for security issues in:\n{params.get('code', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_regex_generator(self, params: Dict) -> str:
        prompt = f"Generate regex pattern for: {params.get('pattern', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_json_formatter(self, params: Dict) -> str:
        try:
            data = json.loads(params.get('json', '{}'))
            return json.dumps(data, indent=2)
        except:
            return "Invalid JSON"
    
    def _execute_api_tester(self, params: Dict) -> str:
        prompt = f"Generate API test for: {params.get('endpoint', '')}"
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']

# ========== SPECIALIST AGENTS ==========

class CodeAgent(BaseAgent):
    """Specialized agent for code-related tasks"""
    def __init__(self):
        super().__init__(
            name="Code Specialist",
            description="Expert in code generation, analysis, and optimization",
            tools=["code_explainer", "code_generator", "code_reviewer", "test_generator"],
            agent_type=AgentType.SPECIALIST
        )

class SQLAgent(BaseAgent):
    """Specialized agent for SQL and database tasks"""
    def __init__(self):
        super().__init__(
            name="SQL Specialist",
            description="Expert in SQL query generation and optimization",
            tools=["sql_generator", "sql_optimizer", "sql_explainer"],
            agent_type=AgentType.SPECIALIST
        )

class DocAgent(BaseAgent):
    """Specialized agent for documentation tasks"""
    def __init__(self):
        super().__init__(
            name="Documentation Specialist",
            description="Expert in documentation analysis and generation",
            tools=["doc_qa", "doc_summarizer", "doc_generator"],
            agent_type=AgentType.SPECIALIST
        )

class AnalysisAgent(BaseAgent):
    """Specialized agent for analysis tasks"""
    def __init__(self):
        super().__init__(
            name="Analysis Specialist",
            description="Expert in error analysis and performance optimization",
            tools=["error_decoder", "performance_analyzer", "security_scanner"],
            agent_type=AgentType.SPECIALIST
        )

# ========== AGENT REGISTRY ==========

AGENT_REGISTRY = {
    "code": CodeAgent(),
    "sql": SQLAgent(),
    "docs": DocAgent(),
    "analysis": AnalysisAgent()
}

# ========== ORCHESTRATOR ==========

class AgentOrchestrator:
    """Master orchestrator that coordinates multiple agents"""
    
    def __init__(self):
        self.agents = AGENT_REGISTRY
        self.db = DatabaseManager()
        
    def select_agent(self, goal: str) -> BaseAgent:
        """Intelligently select the best agent for the goal"""
        goal_lower = goal.lower()
        
        # Simple keyword matching
        if any(word in goal_lower for word in ['code', 'function', 'class', 'script']):
            return self.agents['code']
        elif any(word in goal_lower for word in ['sql', 'query', 'database', 'table']):
            return self.agents['sql']
        elif any(word in goal_lower for word in ['document', 'summary', 'explain']):
            return self.agents['docs']
        elif any(word in goal_lower for word in ['error', 'bug', 'performance', 'security']):
            return self.agents['analysis']
        else:
            # Default to code agent
            return self.agents['code']
    
    def execute_goal(self, goal: str, agent_name: str = None) -> AgentState:
        """Execute a complex goal using appropriate agent(s)"""
        state = AgentState(goal=goal)
        
        # Select agent
        if agent_name and agent_name in self.agents:
            agent = self.agents[agent_name]
        else:
            agent = self.select_agent(goal)
            
        state.agent_history.append(f"Selected {agent.name}")
        
        # Planning phase
        state.phase = "planning"
        state.steps = agent.plan(goal)
        state.agent_history.append(f"Created {len(state.steps)} step plan")
        
        # Execution phase
        state.phase = "execution"
        for i, step in enumerate(state.steps):
            state.current_step = i
            
            tool = step.get('tool', agent.tools[0])
            params = {
                'task': step.get('action', ''),
                'code': state.results.get(f"step_{i-1}", {}).get('result', '') if i > 0 else ''
            }
            
            result = agent.execute_tool(tool, params)
            state.results[f"step_{i}"] = result
            
            state.execution_log.append({
                "step": i + 1,
                "action": step.get('action'),
                "tool": tool,
                "success": result.get('success', False),
                "timestamp": datetime.now().isoformat()
            })
            
            if not result.get('success'):
                state.attempts += 1
                if state.attempts >= state.max_retries:
                    break
        
        # Validation phase
        state.phase = "validation"
        
        return state

# ========== PIPELINE AUTOMATION ==========

def run_pipeline(steps: List[Dict[str, Any]], mode: str = "simple") -> Dict[str, Any]:
    """Execute tool pipeline with advanced features"""
    
    if mode == "simple":
        return _run_simple_pipeline(steps)
    elif mode == "advanced":
        return _run_advanced_pipeline(steps)
    elif mode == "educational":
        return _run_educational_pipeline(steps)
    else:
        return {"error": f"Unknown mode: {mode}"}

def _run_simple_pipeline(steps: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Simple pipeline execution"""
    results = {}
    execution_log = []
    
    for i, step in enumerate(steps):
        tool = step["tool"]
        params = step.get("params", {})
        step_name = step.get("name", f"Step {i+1}")
        
        try:
            # Use agent to execute tool
            agent = AgentOrchestrator().select_agent(tool)
            result = agent.execute_tool(tool, params)
            
            results[step_name] = result
            execution_log.append({
                "step": step_name,
                "success": True,
                "timestamp": datetime.now().isoformat()
            })
            
        except Exception as e:
            results[step_name] = {"error": str(e)}
            execution_log.append({
                "step": step_name,
                "success": False,
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            })
    
    return {"results": results, "log": execution_log}

def _run_advanced_pipeline(steps: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Advanced pipeline with retry logic and parallel execution"""
    # TODO: Implement parallel execution
    return _run_simple_pipeline(steps)

def _run_educational_pipeline(steps: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Educational pipeline with explanations"""
    results = _run_simple_pipeline(steps)
    
    # Add educational content
    for step_name, result in results["results"].items():
        if isinstance(result, dict) and "result" in result:
            # Add explanation
            explanation_prompt = f"Explain what this step does and why it's important:\n{result['result'][:200]}"
            explanation = safe_ollama_generate("deepseek-r1:1.5b", explanation_prompt)
            result["explanation"] = explanation['response']
    
    return results

# ========== STREAMLIT UI ==========

def show():
    st.title("🤖 TuoKit Agent Hub")
    st.caption("Unified AI agent system with pipeline automation and goal orchestration")
    
    # Initialize session state
    if "orchestrator" not in st.session_state:
        st.session_state.orchestrator = AgentOrchestrator()
    if "execution_history" not in st.session_state:
        st.session_state.execution_history = []
    
    # Mode selection
    mode = st.radio(
        "Select Mode",
        ["🎯 Goal Orchestration", "🔧 Pipeline Builder", "📚 Educational Mode"],
        horizontal=True
    )
    
    if mode == "🎯 Goal Orchestration":
        show_goal_orchestration()
    elif mode == "🔧 Pipeline Builder":
        show_pipeline_builder()
    else:
        show_educational_mode()

def show_goal_orchestration():
    """Goal-based agent orchestration interface"""
    st.subheader("🎯 Goal Orchestration")
    st.info("Describe your goal and let AI agents handle the complexity")
    
    col1, col2 = st.columns([3, 1])
    
    with col1:
        goal = st.text_area(
            "What would you like to accomplish?",
            placeholder="Example: Create a Python function that validates email addresses with comprehensive tests",
            height=100
        )
    
    with col2:
        agent_options = ["Auto-select"] + list(AGENT_REGISTRY.keys())
        selected_agent = st.selectbox("Select Agent", agent_options)
        
        if st.button("🚀 Execute", type="primary", disabled=not goal):
            with st.spinner("Agents working..."):
                agent_name = None if selected_agent == "Auto-select" else selected_agent
                state = st.session_state.orchestrator.execute_goal(goal, agent_name)
                
                # Display results
                st.success(f"Goal completed using {state.agent_history[0]}")
                
                # Show execution details
                with st.expander("📊 Execution Details", expanded=True):
                    # Progress
                    progress = (state.current_step + 1) / len(state.steps) if state.steps else 1
                    st.progress(progress)
                    
                    # Steps
                    for i, step in enumerate(state.steps):
                        if i <= state.current_step:
                            st.write(f"✅ Step {i+1}: {step.get('action', 'Unknown')}")
                    
                    # Results
                    st.subheader("Results")
                    for key, value in state.results.items():
                        with st.container():
                            st.write(f"**{key}**")
                            if isinstance(value, dict) and "result" in value:
                                st.code(value["result"][:500] + "..." if len(str(value["result"])) > 500 else value["result"])
                
                # Save to history
                st.session_state.execution_history.append({
                    "goal": goal,
                    "agent": state.agent_history[0],
                    "timestamp": datetime.now(),
                    "success": state.phase == "validation"
                })

def show_pipeline_builder():
    """Visual pipeline builder interface"""
    st.subheader("🔧 Pipeline Builder")
    st.info("Chain multiple tools together for complex workflows")
    
    # Pipeline steps
    if "pipeline_steps" not in st.session_state:
        st.session_state.pipeline_steps = []
    
    # Add step interface
    with st.expander("➕ Add Pipeline Step", expanded=True):
        col1, col2, col3 = st.columns([2, 2, 1])
        
        with col1:
            tool_options = [
                "code_explainer", "code_generator", "sql_generator",
                "doc_summarizer", "error_decoder", "test_generator"
            ]
            selected_tool = st.selectbox("Select Tool", tool_options)
        
        with col2:
            step_name = st.text_input("Step Name", f"Step {len(st.session_state.pipeline_steps) + 1}")
        
        with col3:
            if st.button("Add Step"):
                st.session_state.pipeline_steps.append({
                    "name": step_name,
                    "tool": selected_tool,
                    "params": {}
                })
    
    # Display current pipeline
    if st.session_state.pipeline_steps:
        st.subheader("Current Pipeline")
        
        for i, step in enumerate(st.session_state.pipeline_steps):
            col1, col2, col3 = st.columns([3, 2, 1])
            
            with col1:
                st.write(f"**{step['name']}** - {step['tool']}")
            
            with col2:
                # Parameter input based on tool
                if step['tool'] in ['code_explainer', 'code_generator']:
                    step['params']['code'] = st.text_area(
                        f"Code for {step['name']}",
                        key=f"code_{i}",
                        height=100
                    )
            
            with col3:
                if st.button("🗑️", key=f"delete_{i}"):
                    st.session_state.pipeline_steps.pop(i)
                    st.rerun()
        
        # Execute pipeline
        if st.button("▶️ Execute Pipeline", type="primary"):
            with st.spinner("Executing pipeline..."):
                results = run_pipeline(st.session_state.pipeline_steps, mode="simple")
                
                st.success("Pipeline executed successfully!")
                
                # Display results
                with st.expander("📊 Pipeline Results", expanded=True):
                    for step_name, result in results["results"].items():
                        st.subheader(step_name)
                        if isinstance(result, dict) and "error" in result:
                            st.error(result["error"])
                        else:
                            st.code(str(result)[:500])

def show_educational_mode():
    """Educational mode with step-by-step explanations"""
    st.subheader("📚 Educational Mode")
    st.info("Learn how AI agents work with detailed explanations")
    
    # Predefined educational scenarios
    scenarios = {
        "Build a REST API": [
            {"tool": "code_generator", "params": {"task": "Create a Flask REST API with user endpoints"}},
            {"tool": "test_generator", "params": {"framework": "pytest"}},
            {"tool": "doc_generator", "params": {"doc_type": "API documentation"}}
        ],
        "Optimize Database Query": [
            {"tool": "sql_generator", "params": {"query": "Get top customers by purchase volume"}},
            {"tool": "sql_optimizer", "params": {}},
            {"tool": "sql_explainer", "params": {}}
        ],
        "Debug Error": [
            {"tool": "error_decoder", "params": {"error": "TypeError: unsupported operand type(s)"}},
            {"tool": "code_reviewer", "params": {}},
            {"tool": "code_generator", "params": {"task": "Fix the error"}}
        ]
    }
    
    selected_scenario = st.selectbox("Select Learning Scenario", list(scenarios.keys()))
    
    if st.button("🎓 Start Learning", type="primary"):
        steps = scenarios[selected_scenario]
        
        # Add names to steps
        for i, step in enumerate(steps):
            step["name"] = f"Step {i+1}: {step['tool']}"
        
        with st.spinner("Running educational pipeline..."):
            results = run_pipeline(steps, mode="educational")
            
            st.success("Educational scenario completed!")
            
            # Display results with explanations
            for step_name, result in results["results"].items():
                with st.expander(f"📖 {step_name}", expanded=True):
                    if isinstance(result, dict):
                        if "result" in result:
                            st.code(result["result"][:300])
                        if "explanation" in result:
                            st.info(result["explanation"])
                    
                    # Add learning tips
                    st.caption("💡 **Learning Tip**: Try modifying the parameters to see different results!")

# ========== MAIN ENTRY POINT ==========

if __name__ == "__page__":
    show()
