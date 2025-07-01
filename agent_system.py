"""
TuoKit Agent System - Minimal Implementation
Builds on existing tools with orchestration layer
"""
import streamlit as st
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import json
from datetime import datetime
from utils import DatabaseManager, safe_ollama_generate

class AgentType(Enum):
    SPECIALIST = "specialist"
    TEAM = "team"
    META = "meta"

@dataclass
class AgentState:
    goal: str
    phase: str = "planning"  # planning → execution → validation
    steps: List[Dict] = None
    current_step: int = 0
    attempts: int = 0
    max_retries: int = 3
    results: Dict = None
    agent_history: List[str] = None
    
    def __post_init__(self):
        if self.steps is None:
            self.steps = []
        if self.results is None:
            self.results = {}
        if self.agent_history is None:
            self.agent_history = []

class BaseAgent:
    """Base class for all TuoKit agents"""
    def __init__(self, name: str, description: str, tools: List[str]):
        self.name = name
        self.description = description
        self.tools = tools
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
            # Extract JSON from response
            import re
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
        except:
            # Fallback to simple plan
            return [{"step": 1, "action": goal, "tool": self.tools[0], "expected_output": "result"}]
    
    def execute_tool(self, tool: str, params: Dict) -> Dict:
        """Execute a specific tool with error handling"""
        tool_map = {
            "code_explainer": self._execute_code_explainer,
            "sql_generator": self._execute_sql_generator,
            "doc_qa": self._execute_doc_qa,
            "error_decoder": self._execute_error_decoder,
            "regex_generator": self._execute_regex_generator
        }
        
        if tool not in tool_map:
            return {"success": False, "error": f"Unknown tool: {tool}"}
        
        try:
            result = tool_map[tool](params)
            # Log to knowledge base
            if self.db.connected:
                self.db.log_query(
                    tool=f"agent_{self.name}_{tool}",
                    model=params.get('model', 'deepseek-r1:1.5b'),
                    prompt=str(params),
                    response=str(result)
                )
            return {"success": True, "result": result}
        except Exception as e:
            return {"success": False, "error": str(e)}
    
    def _execute_code_explainer(self, params: Dict) -> str:
        """Execute code explanation using existing tool logic"""
        from pages.code_tools import explain_code
        return explain_code(params['code'], params.get('model', 'deepseek-coder:6.7b'))
    
    def _execute_sql_generator(self, params: Dict) -> str:
        """Execute SQL generation"""
        prompt = f"Generate SQL for: {params['query']}"
        response = safe_ollama_generate(
            model=params.get('model', 'deepseek-coder:6.7b'),
            prompt=prompt
        )
        return response['response']
    
    def _execute_doc_qa(self, params: Dict) -> str:
        """Execute document Q&A"""
        prompt = f"Based on this document: {params['document'][:1000]}...\nQuestion: {params['question']}"
        response = safe_ollama_generate(
            model=params.get('model', 'deepseek-r1:1.5b'),
            prompt=prompt
        )
        return response['response']
    
    def _execute_error_decoder(self, params: Dict) -> str:
        """Decode error messages"""
        from pages.error_tool import decode_error_comprehensive
        return decode_error_comprehensive(
            params['error_message'],
            params.get('code_context', ''),
            params.get('model', 'deepseek-coder:6.7b')
        )
    
    def _execute_regex_generator(self, params: Dict) -> str:
        """Generate regex patterns"""
        from pages.regex_tool import generate_regex
        return generate_regex(
            params['description'],
            params.get('model', 'deepseek-coder:6.7b')
        )
class SpecialistAgent(BaseAgent):
    """Agent specialized in specific domain tasks"""
    def execute(self, state: AgentState) -> AgentState:
        """Execute the agent's plan with state tracking"""
        state.phase = "execution"
        
        for i, step in enumerate(state.steps):
            state.current_step = i
            state.agent_history.append(f"[{self.name}] Executing: {step['action']}")
            
            # Execute with retry logic
            for attempt in range(state.max_retries):
                state.attempts = attempt + 1
                result = self.execute_tool(step['tool'], step.get('params', {}))
                
                if result['success']:
                    state.results[f"step_{i}"] = result['result']
                    break
                elif attempt < state.max_retries - 1:
                    import time
                    time.sleep(2 ** attempt)  # Exponential backoff
            else:
                state.results[f"step_{i}"] = f"Failed after {state.max_retries} attempts"
        
        state.phase = "validation"
        return state

# Pre-configured Specialist Agents
AGENT_REGISTRY = {
    "data_engineer": SpecialistAgent(
        name="DataEngineer",
        description="SQL generation, data analysis, ETL pipelines",
        tools=["sql_generator", "sql_optimizer", "sql_pipeline"]
    ),
    "code_architect": SpecialistAgent(
        name="CodeArchitect", 
        description="Code explanation, debugging, generation",
        tools=["code_explainer", "error_decoder", "regex_generator"]
    ),
    "doc_scientist": SpecialistAgent(
        name="DocScientist",
        description="Document analysis, Q&A, summarization",
        tools=["doc_qa"]
    )
}
class AgentOrchestrator:
    """Manages agent selection and execution workflow"""
    def __init__(self):
        self.agents = AGENT_REGISTRY
        self.teams = {}  # Will be populated if team_agent is imported
        self.db = DatabaseManager()
        
        # Try to import team agents
        try:
            from team_agent import TEAM_REGISTRY
            self.teams = TEAM_REGISTRY
        except ImportError:
            pass
        
    def analyze_goal(self, goal: str, model: str = "deepseek-r1:1.5b") -> str:
        """Determine which agent is best suited for the goal"""
        agent_descriptions = "\n".join([
            f"- {name}: {agent.description}" 
            for name, agent in self.agents.items()
        ])
        
        prompt = f"""
        Given this goal: {goal}
        
        Available agents:
        {agent_descriptions}
        
        Which agent is best suited? Return only the agent name.
        """
        
        response = safe_ollama_generate(model=model, prompt=prompt)
        agent_name = response['response'].strip().lower()
        
        # Fuzzy match to registry keys
        for key in self.agents.keys():
            if key in agent_name or agent_name in key:
                return key
        
        # Default to data_engineer if unclear
        return "data_engineer"
    
    def execute_goal(self, goal: str, agent_name: Optional[str] = None) -> AgentState:
        """Execute a goal using appropriate agent"""
        # Select agent
        if not agent_name:
            agent_name = self.analyze_goal(goal)
        
        agent = self.agents.get(agent_name)
        if not agent:
            raise ValueError(f"Unknown agent: {agent_name}")
        
        # Initialize state
        state = AgentState(goal=goal)
        state.agent_history.append(f"Selected agent: {agent.name}")
        
        # Planning phase
        state.phase = "planning"
        state.steps = agent.plan(goal)
        state.agent_history.append(f"Generated {len(state.steps)} step plan")
        
        # Execution phase
        state = agent.execute(state)
        
        # Log final results
        if self.db.connected:
            self.db.log_query(
                tool=f"agent_orchestrator",
                model="deepseek-r1:1.5b",
                prompt=goal,
                response=json.dumps(state.results)
            )
        
        return state