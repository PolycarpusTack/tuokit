"""
Core components for Agent Hub
Base classes and data structures
"""

from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
import json
import time
from datetime import datetime
import uuid

from utils import DatabaseManager, safe_ollama_generate, capture_knowledge


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
    steps: List[Dict] = field(default_factory=list)
    current_step: int = 0
    attempts: int = 0  # Fixed from "temp"
    max_retries: int = 3
    results: Dict = field(default_factory=dict)
    agent_history: List[str] = field(default_factory=list)
    execution_log: List[Dict] = field(default_factory=list)
    
    def to_dict(self) -> Dict:
        """Convert state to dictionary for serialization"""
        return {
            'goal': self.goal,
            'phase': self.phase,
            'steps': self.steps,
            'current_step': self.current_step,
            'attempts': self.attempts,
            'max_retries': self.max_retries,
            'results': self.results,
            'agent_history': self.agent_history,
            'execution_log': self.execution_log
        }
    
    def add_log_entry(self, entry: Dict) -> None:
        """Add entry to execution log"""
        entry['timestamp'] = datetime.now().isoformat()
        self.execution_log.append(entry)
    
    def increment_step(self) -> bool:
        """Move to next step, return False if no more steps"""
        if self.current_step < len(self.steps) - 1:
            self.current_step += 1
            return True
        return False


class BaseAgent:
    """Base class for all agents"""
    
    def __init__(self, name: str, description: str, tools: List[str], 
                 agent_type: AgentType = AgentType.SPECIALIST):
        self.id = str(uuid.uuid4())
        self.name = name
        self.description = description
        self.tools = tools
        self.agent_type = agent_type
        self.db = None
        self.execution_count = 0
        self.success_rate = 1.0
        self._tool_registry = self._initialize_tools()
        
        # Try to connect to database
        try:
            self.db = DatabaseManager()
        except Exception as e:
            print(f"Database connection failed for agent {name}: {e}")
    
    def _initialize_tools(self) -> Dict[str, Callable]:
        """Initialize tool registry - to be overridden by subclasses"""
        return {}
    
    def plan(self, goal: str, context: Optional[Dict] = None) -> List[Dict]:
        """Create execution plan for the goal"""
        # Generate plan using AI
        prompt = f"""
As {self.name}, create a step-by-step plan to achieve this goal:
{goal}

Available tools: {', '.join(self.tools)}
Context: {json.dumps(context or {}, indent=2)}

Return a JSON array of steps:
[{{"step": 1, "tool": "tool_name", "params": {{}}, "description": "..."}}]
"""
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        try:
            # Extract JSON from response
            import re
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                steps = json.loads(json_match.group())
                return steps
        except:
            pass
        
        # Fallback plan
        return [{
            "step": 1,
            "tool": self.tools[0] if self.tools else "none",
            "params": {"input": goal},
            "description": "Process the request"
        }]
    
    def execute(self, state: AgentState) -> Dict[str, Any]:
        """Execute the agent's plan"""
        results = []
        
        for step in state.steps[state.current_step:]:
            state.add_log_entry({
                'agent': self.name,
                'step': step,
                'status': 'starting'
            })
            
            try:
                # Execute tool
                tool_name = step.get('tool')
                params = step.get('params', {})
                
                if tool_name in self._tool_registry:
                    result = self._tool_registry[tool_name](params)
                else:
                    result = self.execute_tool(tool_name, params)
                
                results.append({
                    'step': step,
                    'result': result,
                    'success': True
                })
                
                state.add_log_entry({
                    'agent': self.name,
                    'step': step,
                    'status': 'completed',
                    'result': str(result)[:200]  # Truncate for logging
                })
                
            except Exception as e:
                results.append({
                    'step': step,
                    'error': str(e),
                    'success': False
                })
                
                state.add_log_entry({
                    'agent': self.name,
                    'step': step,
                    'status': 'failed',
                    'error': str(e)
                })
                
                # Check retry logic
                if state.attempts < state.max_retries:
                    state.attempts += 1
                    return self.execute(state)  # Retry
                else:
                    break
            
            state.current_step += 1
        
        self.execution_count += 1
        return {
            'agent': self.name,
            'results': results,
            'success': all(r.get('success', False) for r in results)
        }
    
    def execute_tool(self, tool: str, params: Dict) -> Any:
        """Execute a specific tool - to be overridden by subclasses"""
        raise NotImplementedError(f"Tool {tool} not implemented in {self.name}")
    
    def validate(self, state: AgentState) -> Dict[str, Any]:
        """Validate execution results"""
        if not state.results:
            return {'valid': False, 'reason': 'No results to validate'}
        
        # Basic validation - can be extended by subclasses
        success_count = sum(1 for r in state.results.get('results', []) 
                          if r.get('success', False))
        total_count = len(state.results.get('results', []))
        
        validation = {
            'valid': success_count == total_count,
            'success_rate': success_count / total_count if total_count > 0 else 0,
            'summary': f"{success_count}/{total_count} steps succeeded"
        }
        
        # Update agent success rate
        self.success_rate = (self.success_rate * (self.execution_count - 1) + 
                           validation['success_rate']) / self.execution_count
        
        return validation
    
    def get_info(self) -> Dict[str, Any]:
        """Get agent information"""
        return {
            'id': self.id,
            'name': self.name,
            'description': self.description,
            'type': self.agent_type.value,
            'tools': self.tools,
            'execution_count': self.execution_count,
            'success_rate': self.success_rate
        }
