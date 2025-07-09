"""
Agent Orchestrator - Coordinates multiple agents for complex tasks
"""

from typing import Dict, List, Optional, Any, Tuple
import json
import re
from datetime import datetime

from .core import BaseAgent, AgentState, AgentType
from .registry import get_agent
from utils import safe_ollama_generate, capture_knowledge, DatabaseManager


class AgentOrchestrator:
    """Master orchestrator that coordinates multiple agents"""
    
    def __init__(self):
        self.db = None
        self.execution_history = []
        
        # Try to connect to database
        try:
            self.db = DatabaseManager()
        except Exception as e:
            print(f"Database connection failed for orchestrator: {e}")
    
    def select_agent(self, goal: str, available_agents: List[str] = None) -> str:
        """Intelligently select the best agent for the goal"""
        goal_lower = goal.lower()
        
        # Agent selection rules
        agent_keywords = {
            'code': ['code', 'function', 'class', 'script', 'program', 'debug', 'refactor'],
            'sql': ['sql', 'query', 'database', 'table', 'schema', 'join', 'index'],
            'docs': ['document', 'documentation', 'summary', 'explain', 'readme', 'guide'],
            'analysis': ['error', 'bug', 'performance', 'security', 'analyze', 'optimize', 'fix']  # Fixed from 'bug'
        }
        
        # Score each agent based on keyword matches
        scores = {}
        for agent_type, keywords in agent_keywords.items():
            if available_agents and agent_type not in available_agents:
                continue
            
            score = sum(1 for keyword in keywords if keyword in goal_lower)
            if score > 0:
                scores[agent_type] = score
        
        # Return agent with highest score, default to 'code'
        if scores:
            return max(scores, key=scores.get)
        return 'code'
    
    def decompose_goal(self, goal: str, context: Optional[Dict] = None) -> List[Dict]:
        """Decompose complex goal into sub-tasks"""
        prompt = f"""
Decompose this goal into specific sub-tasks:
Goal: {goal}
Context: {json.dumps(context or {}, indent=2)}

Return a JSON array of sub-tasks:
[{{"task": "...", "agent": "code|sql|docs|analysis", "depends_on": [], "priority": 1-5}}]

Consider dependencies between tasks and assign appropriate agents.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        try:
            # Extract JSON from response
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                tasks = json.loads(json_match.group())
                return tasks
        except:
            pass
        
        # Fallback decomposition
        return [{
            "task": goal,
            "agent": self.select_agent(goal),
            "depends_on": [],
            "priority": 1
        }]
    
    def orchestrate(self, goal: str, context: Optional[Dict] = None, 
                   mode: str = "sequential") -> Dict[str, Any]:
        """Orchestrate multiple agents to achieve complex goal"""
        
        # Initialize orchestration state
        start_time = datetime.now()
        
        # Decompose goal
        tasks = self.decompose_goal(goal, context)
        
        # Create execution plan
        execution_plan = {
            'goal': goal,
            'tasks': tasks,
            'mode': mode,
            'start_time': start_time.isoformat(),
            'status': 'planning'
        }
        
        # Log to history
        self.execution_history.append(execution_plan)
        
        # Execute based on mode
        if mode == "sequential":
            results = self._execute_sequential(tasks, goal)
        elif mode == "parallel":
            results = self._execute_parallel(tasks, goal)
        elif mode == "adaptive":
            results = self._execute_adaptive(tasks, goal)
        else:
            results = {'error': f'Unknown execution mode: {mode}'}
        
        # Update execution plan
        execution_plan.update({
            'end_time': datetime.now().isoformat(),
            'duration': (datetime.now() - start_time).total_seconds(),
            'status': 'completed' if results.get('success') else 'failed',
            'results': results
        })
        
        # Save to knowledge base
        if self.db:
            capture_knowledge(
                tool="agent_orchestrator",
                prompt=goal,
                response=json.dumps(results),
                metadata=execution_plan
            )
        
        return results
    
    def _execute_sequential(self, tasks: List[Dict], goal: str) -> Dict[str, Any]:
        """Execute tasks sequentially"""
        results = []
        context = {'goal': goal, 'previous_results': []}
        
        for task in sorted(tasks, key=lambda x: x.get('priority', 1)):
            # Get agent
            agent_name = task.get('agent', 'code')
            agent = get_agent(agent_name)
            
            if not agent:
                results.append({
                    'task': task,
                    'error': f'Agent {agent_name} not found',
                    'success': False
                })
                continue
            
            # Create state for agent
            state = AgentState(goal=task['task'])
            state.steps = agent.plan(task['task'], context)
            
            # Execute
            result = agent.execute(state)
            results.append({
                'task': task,
                'agent': agent_name,
                'result': result,
                'success': result.get('success', False)
            })
            
            # Update context with results
            context['previous_results'].append(result)
            
            # Check if we should continue
            if not result.get('success') and task.get('priority', 1) >= 4:
                break  # Critical task failed
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'mode': 'sequential'
        }
    
    def _execute_parallel(self, tasks: List[Dict], goal: str) -> Dict[str, Any]:
        """Execute independent tasks in parallel (simulated)"""
        # Group tasks by dependencies
        task_groups = self._group_by_dependencies(tasks)
        results = []
        context = {'goal': goal, 'previous_results': []}
        
        for group in task_groups:
            group_results = []
            
            # Execute all tasks in group "simultaneously"
            for task in group:
                agent_name = task.get('agent', 'code')
                agent = get_agent(agent_name)
                
                if not agent:
                    group_results.append({
                        'task': task,
                        'error': f'Agent {agent_name} not found',
                        'success': False
                    })
                    continue
                
                # Create state
                state = AgentState(goal=task['task'])
                state.steps = agent.plan(task['task'], context)
                
                # Execute
                result = agent.execute(state)
                group_results.append({
                    'task': task,
                    'agent': agent_name,
                    'result': result,
                    'success': result.get('success', False)
                })
            
            # Update context after group completes
            results.extend(group_results)
            context['previous_results'].extend([r['result'] for r in group_results])
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'mode': 'parallel',
            'groups': len(task_groups)
        }
    
    def _execute_adaptive(self, tasks: List[Dict], goal: str) -> Dict[str, Any]:
        """Adaptive execution that adjusts based on results"""
        results = []
        context = {'goal': goal, 'previous_results': []}
        remaining_tasks = tasks.copy()
        
        while remaining_tasks:
            # Select next best task
            next_task = self._select_next_task(remaining_tasks, context)
            if not next_task:
                break
            
            remaining_tasks.remove(next_task)
            
            # Execute task
            agent_name = next_task.get('agent', 'code')
            agent = get_agent(agent_name)
            
            if not agent:
                results.append({
                    'task': next_task,
                    'error': f'Agent {agent_name} not found',
                    'success': False
                })
                continue
            
            state = AgentState(goal=next_task['task'])
            state.steps = agent.plan(next_task['task'], context)
            result = agent.execute(state)
            
            results.append({
                'task': next_task,
                'agent': agent_name,
                'result': result,
                'success': result.get('success', False)
            })
            
            # Adapt plan based on result
            if not result.get('success'):
                # Add recovery task
                recovery_task = self._create_recovery_task(next_task, result)
                if recovery_task:
                    remaining_tasks.append(recovery_task)
            
            context['previous_results'].append(result)
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'mode': 'adaptive',
            'adaptations': len([r for r in results if 'recovery' in str(r.get('task', {}))])
        }
    
    def _group_by_dependencies(self, tasks: List[Dict]) -> List[List[Dict]]:
        """Group tasks by dependency levels"""
        groups = []
        processed = set()
        
        while len(processed) < len(tasks):
            group = []
            
            for task in tasks:
                task_id = task.get('task', '')
                if task_id in processed:
                    continue
                
                # Check if all dependencies are processed
                deps = task.get('depends_on', [])
                if all(dep in processed for dep in deps):
                    group.append(task)
            
            if not group:
                # No progress, add remaining tasks
                group = [t for t in tasks if t.get('task', '') not in processed]
            
            groups.append(group)
            processed.update(t.get('task', '') for t in group)
        
        return groups
    
    def _select_next_task(self, tasks: List[Dict], context: Dict) -> Optional[Dict]:
        """Select next best task based on context"""
        if not tasks:
            return None
        
        # Simple priority-based selection
        return max(tasks, key=lambda x: x.get('priority', 1))
    
    def _create_recovery_task(self, failed_task: Dict, error_result: Dict) -> Optional[Dict]:
        """Create recovery task for failed execution"""
        error_msg = error_result.get('error', 'Unknown error')
        
        return {
            'task': f"Recovery: Fix issue with {failed_task['task']} - {error_msg}",
            'agent': 'analysis',  # Use analysis agent for error recovery
            'depends_on': [],
            'priority': failed_task.get('priority', 1) + 1,
            'recovery': True
        }
