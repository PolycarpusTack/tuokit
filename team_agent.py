"""
TuoKit Team Agent Implementation
Allows multiple agents to collaborate on complex goals
"""
from typing import List, Dict
from agent_system import BaseAgent, SpecialistAgent, AgentState
from utils import safe_ollama_generate
import json

class TeamAgent(BaseAgent):
    """Coordinates multiple specialist agents"""
    def __init__(self, name: str, description: str, members: List[SpecialistAgent]):
        self.name = name
        self.description = description
        self.members = members
        self.tools = []  # Team doesn't use tools directly
        
    def plan_collaboration(self, goal: str, model: str = "deepseek-r1:1.5b") -> Dict:
        """Create collaboration plan for team members"""
        member_info = "\n".join([
            f"- {m.name}: {m.description} (tools: {', '.join(m.tools)})"
            for m in self.members
        ])
        
        prompt = f"""
        Create a collaboration plan for this team goal: {goal}
        
        Team members:
        {member_info}
        
        Return JSON with task assignments:
        {{"tasks": [{{"agent": "name", "subtask": "...", "depends_on": []}}]}}
        """
        
        response = safe_ollama_generate(model=model, prompt=prompt)
        try:
            import re
            json_match = re.search(r'\{.*\}', response['response'], re.DOTALL)
            if json_match:
                return json.loads(json_match.group())
        except:
            # Fallback plan
            return {"tasks": [{"agent": self.members[0].name, "subtask": goal, "depends_on": []}]}    
    def execute(self, state: AgentState) -> AgentState:
        """Execute team collaboration"""
        state.phase = "team_planning"
        collaboration_plan = self.plan_collaboration(state.goal)
        
        state.agent_history.append(f"[{self.name}] Created collaboration plan with {len(collaboration_plan['tasks'])} tasks")
        
        # Execute tasks in dependency order
        state.phase = "team_execution"
        task_results = {}
        
        for task in collaboration_plan['tasks']:
            # Wait for dependencies
            for dep in task.get('depends_on', []):
                if dep not in task_results:
                    state.agent_history.append(f"⚠️ Skipping {task['agent']} - dependency {dep} not met")
                    continue
            
            # Find the right agent
            agent = next((m for m in self.members if m.name == task['agent']), None)
            if not agent:
                state.agent_history.append(f"⚠️ Agent {task['agent']} not found in team")
                continue
            
            # Create sub-state for this agent
            sub_state = AgentState(goal=task['subtask'])
            sub_state.steps = agent.plan(task['subtask'])
            
            # Execute
            sub_state = agent.execute(sub_state)
            task_results[task['agent']] = sub_state.results
            
            state.agent_history.append(f"✅ {agent.name} completed: {task['subtask']}")
            state.results[f"{agent.name}_results"] = sub_state.results
        
        state.phase = "team_validation"
        return state
# Pre-configured Team Agents
from agent_system import AGENT_REGISTRY, safe_ollama_generate

PROJECT_BUILDER_TEAM = TeamAgent(
    name="ProjectBuilder",
    description="End-to-end project implementation team",
    members=[
        AGENT_REGISTRY["data_engineer"],
        AGENT_REGISTRY["code_architect"],
        AGENT_REGISTRY["doc_scientist"]
    ]
)

DATA_PIPELINE_TEAM = TeamAgent(
    name="DataPipeline",
    description="Data extraction, transformation, and visualization",
    members=[
        AGENT_REGISTRY["data_engineer"],
        AGENT_REGISTRY["code_architect"]
    ]
)

# Add teams to registry
TEAM_REGISTRY = {
    "project_builder": PROJECT_BUILDER_TEAM,
    "data_pipeline": DATA_PIPELINE_TEAM
}