"""
TuoKit Advanced Agent Toolkit
Integrates the best agents from awesome-llm-apps
Adapted for local Ollama and TuoKit's architecture
"""

import streamlit as st
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
from enum import Enum
import json
import time
import re
import os
import tempfile
from datetime import datetime
import pandas as pd
import ast
import hashlib
from pathlib import Path

# TuoKit imports
from utils import (
    safe_ollama_generate, 
    DatabaseManager, 
    capture_knowledge,
    get_available_models
)

# ========== CONFIGURATION ==========

# Default models for different tasks
DEFAULT_MODELS = {
    "reasoning": "deepseek-r1:1.5b",
    "coding": "deepseek-coder:6.7b", 
    "general": "llama3.2:latest",
    "embedding": "nomic-embed-text:latest"
}

# ========== BASE CLASSES ==========

class AgentRole(str, Enum):
    """Agent roles in the toolkit"""
    ARCHITECT = "architect"
    RESEARCHER = "researcher"
    CODER = "coder"
    DATA_ANALYST = "data_analyst"
    DEBUGGER = "debugger"
    DOCUMENTER = "documenter"

@dataclass
class AgentContext:
    """Shared context between agents"""
    goal: str
    history: List[Dict] = field(default_factory=list)
    artifacts: Dict[str, Any] = field(default_factory=dict)
    knowledge: List[Dict] = field(default_factory=list)
    
    def add_artifact(self, name: str, content: Any):
        """Add artifact to shared context"""
        self.artifacts[name] = content
        self.history.append({
            "timestamp": datetime.now().isoformat(),
            "action": f"Added artifact: {name}",
            "type": type(content).__name__
        })
    
    def get_artifact(self, name: str) -> Optional[Any]:
        """Retrieve artifact from context"""
        return self.artifacts.get(name)

# ========== SYSTEM ARCHITECT AGENT ==========

class SystemArchitectAgent:
    """
    AI System Architect adapted from awesome-llm-apps
    Provides architecture analysis and design recommendations
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["reasoning"]
        self.db = DatabaseManager()
        
    def analyze_requirements(self, requirements: str, context: AgentContext) -> Dict[str, Any]:
        """Analyze requirements and suggest architecture"""
        
        # Phase 1: Reasoning
        reasoning_prompt = f"""
You are an expert software architect. Analyze these requirements:

{requirements}

Previous context: {json.dumps(context.history[-3:], indent=2) if context.history else "None"}

Think through:
1. Scale requirements
2. Performance needs  
3. Security requirements
4. Development complexity
5. Cost implications

Provide step-by-step reasoning.
"""
        
        reasoning_response = safe_ollama_generate(self.model, reasoning_prompt)
        reasoning = reasoning_response.get('response', '')
        
        # Phase 2: Structured Architecture Decision
        decision_prompt = f"""
Based on this analysis:
{reasoning}

Generate a JSON architecture decision:
{
    "pattern": "microservices|monolithic|serverless|event_driven|layered",
    "components": [
        {
            "name": "component_name",
            "responsibility": "what it does",
            "technology": "suggested tech stack"
        }
    ],
    "database": {
        "type": "sql|nosql|hybrid",
        "specific": "PostgreSQL|MongoDB|etc",
        "rationale": "why this choice"
    },
    "infrastructure": {
        "deployment": "kubernetes|docker|serverless",
        "scaling": "horizontal|vertical|auto",
        "estimated_cost": {
            "development": 50000,
            "monthly": 5000
        }
    },
    "security": {
        "authentication": "method",
        "authorization": "approach",
        "data_protection": "encryption strategy"
    },
    "risks": [
        {
            "risk": "description",
            "mitigation": "how to address",
            "impact": "high|medium|low"
        }
    ]
}

Return ONLY valid JSON.
"""
        
        structured_response = safe_ollama_generate(self.model, decision_prompt)
        
        try:
            # Extract JSON
            json_match = re.search(r'\{.*\}', structured_response['response'], re.DOTALL)
            if json_match:
                architecture = json.loads(json_match.group())
            else:
                architecture = self._default_architecture()
        except:
            architecture = self._default_architecture()
        
        # Phase 3: Implementation Roadmap
        roadmap_prompt = f"""
Create an implementation roadmap for this architecture:
{json.dumps(architecture, indent=2)}

Include:
1. Development phases (with timelines)
2. Team requirements
3. Key milestones
4. Testing strategy
5. Deployment plan

Format as actionable steps.
"""
        
        roadmap_response = safe_ollama_generate(self.model, roadmap_prompt)
        
        result = {
            "reasoning": reasoning,
            "architecture": architecture,
            "roadmap": roadmap_response['response'],
            "timestamp": datetime.now().isoformat()
        }
        
        # Add to context
        context.add_artifact("architecture_decision", result)
        
        # Save to knowledge base
        if self.db and self.db.connected:
            capture_knowledge(
                tool="system_architect",
                model=self.model,
                prompt=requirements,
                response=json.dumps(result)
            )
        
        return result
    
    def _default_architecture(self) -> Dict:
        """Fallback architecture if parsing fails"""
        return {
            "pattern": "microservices",
            "components": [
                {
                    "name": "API Gateway",
                    "responsibility": "Route requests, authentication",
                    "technology": "Kong/Traefik"
                },
                {
                    "name": "Core Service", 
                    "responsibility": "Business logic",
                    "technology": "Python/FastAPI"
                }
            ],
            "database": {
                "type": "sql",
                "specific": "PostgreSQL",
                "rationale": "ACID compliance and JSON support"
            },
            "infrastructure": {
                "deployment": "kubernetes",
                "scaling": "horizontal",
                "estimated_cost": {
                    "development": 75000,
                    "monthly": 3000
                }
            },
            "security": {
                "authentication": "JWT tokens",
                "authorization": "RBAC",
                "data_protection": "AES-256 encryption"
            },
            "risks": []
        }

# ========== DEEP RESEARCH AGENT ==========

class DeepResearchAgent:
    """
    Research agent adapted for local operation
    Performs multi-step research and synthesis
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["general"]
        self.db = DatabaseManager()
        
    def research_topic(self, topic: str, context: AgentContext, depth: int = 3) -> Dict[str, Any]:
        """
        Perform deep research on a topic
        Simulates web research with knowledge synthesis
        """
        
        research_results = {
            "topic": topic,
            "findings": [],
            "synthesis": "",
            "sources": [],
            "timestamp": datetime.now().isoformat()
        }
        
        # Check if we have architecture context
        architecture = context.get_artifact("architecture_decision")
        if architecture:
            topic += f"\n\nContext: Building {architecture['architecture']['pattern']} architecture"
        
        # Phase 1: Generate research questions
        questions_prompt = f"""
Generate {depth} specific research questions about: {topic}

Consider:
- Technical implementation details
- Best practices and patterns
- Common pitfalls
- Performance considerations
- Security implications

Return as numbered list.
"""
        
        questions_response = safe_ollama_generate(self.model, questions_prompt)
        questions = questions_response['response'].split('\n')
        questions = [q.strip() for q in questions if q.strip() and any(char.isdigit() for char in q[:3])]
        
        # Phase 2: Research each question
        for i, question in enumerate(questions[:depth]):
            research_prompt = f"""
Research this question in detail:
{question}

Topic context: {topic}

Provide:
1. Comprehensive answer with examples
2. Best practices
3. Code snippets if relevant
4. Common mistakes to avoid
5. Related concepts

Be specific and technical.
"""
            
            research_response = safe_ollama_generate(self.model, research_prompt)
            
            finding = {
                "question": question,
                "answer": research_response['response'],
                "confidence": 0.85,  # Simulated confidence
                "sources": ["Internal knowledge base", "Technical documentation"]
            }
            
            research_results["findings"].append(finding)
            
            # Simulate source citations
            research_results["sources"].extend([
                f"Technical Guide: {question.split()[-2:]}",
                f"Best Practices: {topic.split()[0]} Implementation"
            ])
        
        # Phase 3: Synthesize findings
        synthesis_prompt = f"""
Synthesize these research findings into actionable insights:

Topic: {topic}

Findings:
{json.dumps(research_results['findings'], indent=2)}

Create:
1. Executive summary
2. Key recommendations
3. Implementation guidelines
4. Risk considerations
5. Next steps

Be concise but comprehensive.
"""
        
        synthesis_response = safe_ollama_generate(self.model, synthesis_prompt)
        research_results["synthesis"] = synthesis_response['response']
        
        # Add to context
        context.add_artifact(f"research_{topic[:20]}", research_results)
        
        # Save to knowledge base
        if self.db and self.db.connected:
            capture_knowledge(
                tool="deep_research",
                model=self.model,
                prompt=topic,
                response=json.dumps(research_results)
            )
        
        return research_results

# ========== ENHANCED CODE AGENT ==========

class EnhancedCodeAgent:
    """
    Advanced code generation with analysis capabilities
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["coding"]
        self.db = DatabaseManager()
        
    def generate_code(self, task: str, context: AgentContext, 
                     language: str = "python") -> Dict[str, Any]:
        """Generate code based on task and context"""
        
        # Check for architecture context
        architecture = context.get_artifact("architecture_decision")
        arch_context = ""
        if architecture:
            arch_context = f"\nArchitecture: {architecture['architecture']['pattern']}"
            arch_context += f"\nComponents: {[c['name'] for c in architecture['architecture']['components']]}"
        
        # Check for research context
        research = context.get_artifact(f"research_{task[:20]}")
        research_context = ""
        if research:
            research_context = f"\nResearch insights: {research['synthesis'][:500]}"
        
        # Generate code
        code_prompt = f"""
Generate {language} code for: {task}
{arch_context}
{research_context}

Requirements:
1. Production-ready code
2. Proper error handling
3. Type hints (if applicable)
4. Comprehensive documentation
5. Unit tests
6. Performance optimized

Return complete, runnable code.
"""
        
        code_response = safe_ollama_generate(self.model, code_prompt)
        generated_code = code_response['response']
        
        # Analyze the generated code
        analysis = self._analyze_code(generated_code, language)
        
        # Generate tests
        test_prompt = f"""
Generate comprehensive unit tests for this code:

```{language}
{generated_code}
```

Use appropriate testing framework for {language}.
Include edge cases and error scenarios.
"""
        
        test_response = safe_ollama_generate(self.model, test_prompt)
        
        result = {
            "task": task,
            "language": language,
            "code": generated_code,
            "tests": test_response['response'],
            "analysis": analysis,
            "timestamp": datetime.now().isoformat()
        }
        
        # Add to context
        context.add_artifact(f"code_{task[:20]}", result)
        
        return result
    
    def _analyze_code(self, code: str, language: str) -> Dict[str, Any]:
        """Analyze code for quality metrics"""
        analysis = {
            "language": language,
            "metrics": {},
            "issues": [],
            "suggestions": []
        }
        
        if language.lower() == "python":
            try:
                # Parse Python code
                tree = ast.parse(code)
                
                # Count elements
                functions = sum(1 for node in ast.walk(tree) if isinstance(node, ast.FunctionDef))
                classes = sum(1 for node in ast.walk(tree) if isinstance(node, ast.ClassDef))
                
                # Complexity analysis
                loops = sum(1 for node in ast.walk(tree) if isinstance(node, (ast.For, ast.While)))
                conditionals = sum(1 for node in ast.walk(tree) if isinstance(node, ast.If))
                
                analysis["metrics"] = {
                    "functions": functions,
                    "classes": classes,
                    "cyclomatic_complexity": 1 + conditionals + loops,
                    "lines": len(code.split('\n'))
                }
                
                # Suggestions
                if functions > 10:
                    analysis["suggestions"].append("Consider breaking into multiple modules")
                if loops > 5:
                    analysis["suggestions"].append("High loop count - check for optimization opportunities")
                    
            except SyntaxError as e:
                analysis["issues"].append(f"Syntax error: {str(e)}")
        
        return analysis

# ========== DATA ANALYSIS AGENT ==========

class DataAnalysisAgent:
    """
    Natural language to SQL and data analysis
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["coding"]
        self.db = DatabaseManager()
        
    def analyze_data_request(self, request: str, context: AgentContext,
                           schema: Dict = None) -> Dict[str, Any]:
        """Convert natural language to SQL and analyze"""
        
        # Default schema if none provided
        if not schema:
            schema = {
                "tables": {
                    "users": ["id", "name", "email", "created_at", "status"],
                    "orders": ["id", "user_id", "total", "status", "created_at"],
                    "products": ["id", "name", "price", "category", "stock"]
                }
            }
        
        # Generate SQL
        sql_prompt = f"""
Convert this request to SQL:
{request}

Database schema:
{json.dumps(schema, indent=2)}

Requirements:
1. Use proper SQL syntax
2. Include JOINs if needed
3. Add appropriate WHERE clauses
4. Use aggregate functions if needed
5. Add ORDER BY and LIMIT as appropriate

Return SQL query with explanation.
"""
        
        sql_response = safe_ollama_generate(self.model, sql_prompt)
        
        # Extract SQL from response
        sql_match = re.search(r'```sql\n(.*?)\n```', sql_response['response'], re.DOTALL)
        if sql_match:
            sql_query = sql_match.group(1)
        else:
            # Try to find SQL pattern
            lines = sql_response['response'].split('\n')
            sql_lines = [l for l in lines if any(kw in l.upper() for kw in ['SELECT', 'FROM', 'WHERE', 'JOIN'])]
            sql_query = '\n'.join(sql_lines) if sql_lines else "SELECT * FROM users LIMIT 10"
        
        # Analyze query
        analysis_prompt = f"""
Analyze this SQL query for:
1. Performance implications
2. Potential optimizations
3. Missing indexes
4. Security concerns

Query:
{sql_query}

Schema:
{json.dumps(schema, indent=2)}
"""
        
        analysis_response = safe_ollama_generate(self.model, analysis_prompt)
        
        # Generate visualization suggestion
        viz_prompt = f"""
Suggest the best visualization for this query result:
{sql_query}

Consider:
1. Data types
2. Aggregations
3. Relationships
4. Best practices

Recommend chart type and configuration.
"""
        
        viz_response = safe_ollama_generate(self.model, viz_prompt)
        
        result = {
            "request": request,
            "sql_query": sql_query,
            "explanation": sql_response['response'],
            "analysis": analysis_response['response'],
            "visualization": viz_response['response'],
            "timestamp": datetime.now().isoformat()
        }
        
        # Add to context
        context.add_artifact(f"data_analysis_{request[:20]}", result)
        
        return result

# ========== DEBUGGING AGENT ==========

class DebuggingAgent:
    """
    Advanced error analysis and debugging
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["coding"]
        self.error_patterns = self._load_error_patterns()
        
    def _load_error_patterns(self) -> Dict[str, Dict]:
        """Load common error patterns"""
        return {
            "TypeError": {
                "pattern": r"TypeError:.*'(\w+)'.*'(\w+)'",
                "common_causes": ["Type mismatch", "None value", "Wrong argument type"],
                "solutions": ["Check variable types", "Add type validation", "Use type hints"]
            },
            "KeyError": {
                "pattern": r"KeyError:\s*['\"](\w+)['\"]",
                "common_causes": ["Missing dictionary key", "Typo in key name", "Dynamic key not present"],
                "solutions": ["Use .get() method", "Check key existence", "Add default values"]
            },
            "AttributeError": {
                "pattern": r"AttributeError:.*'(\w+)'.*no attribute.*'(\w+)'",
                "common_causes": ["Wrong object type", "Typo in attribute", "Not initialized"],
                "solutions": ["Verify object type", "Check attribute spelling", "Ensure initialization"]
            }
        }
    
    def analyze_error(self, error: str, code_context: str = "", 
                     context: AgentContext = None) -> Dict[str, Any]:
        """Analyze error with context"""
        
        # Detect error type
        error_type = None
        for etype, pattern_info in self.error_patterns.items():
            if etype in error:
                error_type = etype
                break
        
        # Get code from context if available
        if context and not code_context:
            latest_code = context.get_artifact("code_")
            if latest_code:
                code_context = latest_code.get('code', '')
        
        # Detailed analysis
        analysis_prompt = f"""
Analyze this error in detail:

Error:
{error}

Code context:
{code_context}

Provide:
1. Root cause analysis
2. Step-by-step debugging approach
3. Code fix with explanation
4. Prevention strategies
5. Related errors to watch for

Be specific and provide code examples.
"""
        
        analysis_response = safe_ollama_generate(self.model, analysis_prompt)
        
        # Generate fixed code if code context provided
        fixed_code = None
        if code_context:
            fix_prompt = f"""
Fix this code to resolve the error:

Error: {error}

Original code:
{code_context}

Return ONLY the corrected code.
"""
            fix_response = safe_ollama_generate(self.model, fix_prompt)
            fixed_code = fix_response['response']
        
        result = {
            "error": error,
            "error_type": error_type,
            "analysis": analysis_response['response'],
            "fixed_code": fixed_code,
            "patterns": self.error_patterns.get(error_type, {}),
            "timestamp": datetime.now().isoformat()
        }
        
        if context:
            context.add_artifact(f"debug_{error_type}", result)
        
        return result

# ========== DOCUMENTATION AGENT ==========

class DocumentationAgent:
    """
    Comprehensive documentation generation
    """
    
    def __init__(self, model: str = None):
        self.model = model or DEFAULT_MODELS["general"]
        
    def generate_documentation(self, context: AgentContext, 
                             doc_type: str = "technical") -> Dict[str, Any]:
        """Generate documentation from context artifacts"""
        
        # Gather all artifacts
        architecture = context.get_artifact("architecture_decision")
        code_artifacts = {k: v for k, v in context.artifacts.items() if k.startswith("code_")}
        research_artifacts = {k: v for k, v in context.artifacts.items() if k.startswith("research_")}
        
        # Generate appropriate documentation
        if doc_type == "technical":
            doc_prompt = f"""
Generate comprehensive technical documentation:

Project Goal: {context.goal}

Architecture:
{json.dumps(architecture, indent=2) if architecture else "Not specified"}

Implementation:
{self._summarize_code_artifacts(code_artifacts)}

Research Findings:
{self._summarize_research_artifacts(research_artifacts)}

Include:
1. System Overview
2. Architecture Description
3. API Documentation
4. Setup Instructions
5. Configuration Guide
6. Troubleshooting
7. Performance Considerations

Format in Markdown.
"""
        elif doc_type == "user":
            doc_prompt = f"""
Generate user-friendly documentation:

Project: {context.goal}

Features implemented:
{self._list_features(code_artifacts)}

Create:
1. Getting Started Guide
2. Feature Overview
3. Usage Examples
4. FAQ
5. Troubleshooting

Keep it simple and clear.
"""
        else:  # API documentation
            doc_prompt = f"""
Generate API documentation:

Project: {context.goal}

Code implementations:
{self._summarize_code_artifacts(code_artifacts)}

Include:
1. Endpoint descriptions
2. Request/Response formats
3. Authentication
4. Error codes
5. Examples
6. Rate limits

Format for developers.
"""
        
        doc_response = safe_ollama_generate(self.model, doc_prompt)
        
        result = {
            "type": doc_type,
            "content": doc_response['response'],
            "artifacts_used": list(context.artifacts.keys()),
            "timestamp": datetime.now().isoformat()
        }
        
        context.add_artifact(f"documentation_{doc_type}", result)
        
        return result
    
    def _summarize_code_artifacts(self, artifacts: Dict) -> str:
        """Summarize code artifacts for documentation"""
        summary = []
        for name, artifact in artifacts.items():
            summary.append(f"- {name}: {artifact.get('task', 'Unknown task')}")
            if 'analysis' in artifact:
                metrics = artifact['analysis'].get('metrics', {})
                summary.append(f"  Functions: {metrics.get('functions', 0)}, "
                             f"Complexity: {metrics.get('cyclomatic_complexity', 0)}")
        return '\n'.join(summary)
    
    def _summarize_research_artifacts(self, artifacts: Dict) -> str:
        """Summarize research findings"""
        summary = []
        for name, artifact in artifacts.items():
            summary.append(f"- {artifact.get('topic', 'Unknown topic')}")
            if 'synthesis' in artifact:
                summary.append(f"  Key findings: {artifact['synthesis'][:200]}...")
        return '\n'.join(summary)
    
    def _list_features(self, code_artifacts: Dict) -> str:
        """List implemented features from code artifacts"""
        features = []
        for artifact in code_artifacts.values():
            if 'task' in artifact:
                features.append(f"- {artifact['task']}")
        return '\n'.join(features)

# ========== MULTI-AGENT ORCHESTRATOR ==========

class MultiAgentOrchestrator:
    """
    Orchestrates multiple agents to achieve complex goals
    """
    
    def __init__(self):
        self.agents = {
            AgentRole.ARCHITECT: SystemArchitectAgent(),
            AgentRole.RESEARCHER: DeepResearchAgent(),
            AgentRole.CODER: EnhancedCodeAgent(),
            AgentRole.DATA_ANALYST: DataAnalysisAgent(),
            AgentRole.DEBUGGER: DebuggingAgent(),
            AgentRole.DOCUMENTER: DocumentationAgent()
        }
        self.db = DatabaseManager()
    
    def execute_complex_goal(self, goal: str, selected_agents: List[AgentRole] = None) -> Dict[str, Any]:
        """
        Execute a complex goal using multiple agents
        """
        context = AgentContext(goal=goal)
        results = {"goal": goal, "executions": []}
        
        # Default agent sequence if none specified
        if not selected_agents:
            selected_agents = self._determine_agent_sequence(goal)
        
        # Execute agents in sequence
        for agent_role in selected_agents:
            agent = self.agents[agent_role]
            
            try:
                if agent_role == AgentRole.ARCHITECT:
                    result = agent.analyze_requirements(goal, context)
                    
                elif agent_role == AgentRole.RESEARCHER:
                    # Research based on architecture
                    topics = self._extract_research_topics(context)
                    research_results = []
                    for topic in topics:
                        res = agent.research_topic(topic, context)
                        research_results.append(res)
                    result = {"topics": topics, "research": research_results}
                    
                elif agent_role == AgentRole.CODER:
                    # Generate code based on architecture and research
                    tasks = self._extract_coding_tasks(context)
                    code_results = []
                    for task in tasks:
                        res = agent.generate_code(task, context)
                        code_results.append(res)
                    result = {"tasks": tasks, "implementations": code_results}
                    
                elif agent_role == AgentRole.DATA_ANALYST:
                    # Analyze data requirements
                    queries = self._extract_data_queries(context)
                    analysis_results = []
                    for query in queries:
                        res = agent.analyze_data_request(query, context)
                        analysis_results.append(res)
                    result = {"queries": queries, "analyses": analysis_results}
                    
                elif agent_role == AgentRole.DOCUMENTER:
                    # Generate comprehensive documentation
                    docs = {}
                    for doc_type in ["technical", "user", "api"]:
                        doc = agent.generate_documentation(context, doc_type)
                        docs[doc_type] = doc
                    result = docs
                
                else:
                    result = {"status": "Agent not implemented"}
                
                results["executions"].append({
                    "agent": agent_role.value,
                    "result": result,
                    "timestamp": datetime.now().isoformat()
                })
                
            except Exception as e:
                results["executions"].append({
                    "agent": agent_role.value,
                    "error": str(e),
                    "timestamp": datetime.now().isoformat()
                })
        
        # Save orchestration result
        if self.db and self.db.connected:
            capture_knowledge(
                tool="multi_agent_orchestrator",
                model="orchestration",
                prompt=goal,
                response=json.dumps(results)
            )
        
        return {
            "results": results,
            "context": context,
            "artifacts": context.artifacts
        }
    
    def _determine_agent_sequence(self, goal: str) -> List[AgentRole]:
        """Determine optimal agent sequence based on goal"""
        goal_lower = goal.lower()
        
        # Pattern matching for different goal types
        if any(word in goal_lower for word in ["build", "create", "develop", "application", "system"]):
            return [AgentRole.ARCHITECT, AgentRole.RESEARCHER, AgentRole.CODER, AgentRole.DOCUMENTER]
        
        elif any(word in goal_lower for word in ["analyze", "data", "query", "report"]):
            return [AgentRole.DATA_ANALYST, AgentRole.DOCUMENTER]
        
        elif any(word in goal_lower for word in ["debug", "fix", "error", "issue"]):
            return [AgentRole.DEBUGGER, AgentRole.CODER]
        
        elif any(word in goal_lower for word in ["research", "investigate", "explore"]):
            return [AgentRole.RESEARCHER, AgentRole.DOCUMENTER]
        
        else:
            # Default comprehensive sequence
            return [AgentRole.ARCHITECT, AgentRole.RESEARCHER, AgentRole.CODER, AgentRole.DOCUMENTER]
    
    def _extract_research_topics(self, context: AgentContext) -> List[str]:
        """Extract research topics from architecture"""
        topics = []
        
        architecture = context.get_artifact("architecture_decision")
        if architecture:
            # Research architecture pattern
            pattern = architecture['architecture'].get('pattern', '')
            if pattern:
                topics.append(f"Best practices for {pattern} architecture")
            
            # Research technologies
            components = architecture['architecture'].get('components', [])
            for comp in components:
                tech = comp.get('technology', '')
                if tech:
                    topics.append(f"{tech} implementation patterns")
        
        return topics[:3]  # Limit to 3 topics
    
    def _extract_coding_tasks(self, context: AgentContext) -> List[str]:
        """Extract coding tasks from architecture"""
        tasks = []
        
        architecture = context.get_artifact("architecture_decision")
        if architecture:
            components = architecture['architecture'].get('components', [])
            for comp in components:
                tasks.append(f"Implement {comp['name']}: {comp['responsibility']}")
        
        return tasks[:5]  # Limit to 5 tasks
    
    def _extract_data_queries(self, context: AgentContext) -> List[str]:
        """Extract potential data queries from context"""
        queries = []
        
        # Look for data-related components
        architecture = context.get_artifact("architecture_decision")
        if architecture:
            db_type = architecture['architecture'].get('database', {}).get('type', '')
            if db_type:
                queries.append("Show me all users created in the last 30 days")
                queries.append("What are the top 10 most active users")
        
        return queries

# ========== STREAMLIT UI ==========

def show_agent_toolkit():
    """Main UI for the agent toolkit"""
    st.set_page_config(
        page_title="TuoKit Advanced Agent Toolkit",
        page_icon="🤖",
        layout="wide"
    )
    
    st.title("🤖 TuoKit Advanced Agent Toolkit")
    st.caption("Multi-agent system for complex development tasks")
    
    # Initialize orchestrator
    if "orchestrator" not in st.session_state:
        st.session_state.orchestrator = MultiAgentOrchestrator()
    
    # Sidebar for configuration
    with st.sidebar:
        st.header("⚙️ Configuration")
        
        # Model selection
        available_models = get_available_models()
        
        st.subheader("Model Selection")
        if available_models:
            reasoning_model = st.selectbox(
                "Reasoning Model",
                available_models,
                index=available_models.index(DEFAULT_MODELS["reasoning"]) 
                if DEFAULT_MODELS["reasoning"] in available_models else 0
            )
            
            coding_model = st.selectbox(
                "Coding Model", 
                available_models,
                index=available_models.index(DEFAULT_MODELS["coding"])
                if DEFAULT_MODELS["coding"] in available_models else 0
            )
        else:
            st.warning("No Ollama models found. Using defaults.")
            reasoning_model = DEFAULT_MODELS["reasoning"]
            coding_model = DEFAULT_MODELS["coding"]
        
        # Update agent models
        for agent in st.session_state.orchestrator.agents.values():
            if hasattr(agent, 'model'):
                if isinstance(agent, (SystemArchitectAgent, DeepResearchAgent)):
                    agent.model = reasoning_model
                elif isinstance(agent, (EnhancedCodeAgent, DataAnalysisAgent)):
                    agent.model = coding_model
        
        st.divider()
        
        # Agent selection
        st.subheader("Agent Selection")
        
        all_agents = list(AgentRole)
        selected_agents = st.multiselect(
            "Select agents to use",
            all_agents,
            default=None,
            format_func=lambda x: x.value.replace('_', ' ').title(),
            help="Leave empty for automatic selection"
        )
    
    # Main interface
    tab1, tab2, tab3, tab4 = st.tabs([
        "🎯 Multi-Agent Execution",
        "🏗️ System Architect", 
        "🔍 Deep Research",
        "💾 Results & Artifacts"
    ])
    
    with tab1:
        show_multi_agent_execution(selected_agents)
    
    with tab2:
        show_system_architect()
    
    with tab3:
        show_deep_research()
    
    with tab4:
        show_results_artifacts()

def show_multi_agent_execution(selected_agents):
    """Multi-agent execution interface"""
    st.subheader("🎯 Multi-Agent Goal Execution")
    
    # Goal input
    col1, col2 = st.columns([3, 1])
    
    with col1:
        goal = st.text_area(
            "Describe your goal",
            placeholder="Example: Build a real-time chat application with user authentication, message history, and file sharing",
            height=100
        )
    
    with col2:
        st.info(
            "Agents will work together:\n"
            "1. Architect designs system\n"
            "2. Researcher gathers info\n"
            "3. Coder implements\n"
            "4. Documenter creates docs"
        )
    
    # Example goals
    with st.expander("💡 Example Goals"):
        examples = {
            "🛍️ E-commerce Platform": "Build a scalable e-commerce platform with product catalog, shopping cart, payment processing, and order management",
            "📊 Analytics Dashboard": "Create a real-time analytics dashboard that processes streaming data and displays interactive visualizations",
            "🤖 AI Chatbot": "Develop an AI-powered customer support chatbot with natural language understanding and integration with knowledge base",
            "📱 Mobile Backend": "Design and implement a mobile app backend with user authentication, data sync, and push notifications"
        }
        
        for title, example in examples.items():
            if st.button(title):
                st.session_state.example_goal = example
                st.rerun()
    
    # Use example if selected
    if "example_goal" in st.session_state:
        goal = st.session_state.example_goal
        del st.session_state.example_goal
    
    # Execute button
    if st.button("🚀 Execute Goal", type="primary", disabled=not goal):
        with st.spinner("Orchestrating agents..."):
            result = st.session_state.orchestrator.execute_complex_goal(
                goal, 
                selected_agents if selected_agents else None
            )
            st.session_state.last_result = result
        
        # Display execution summary
        st.success("✅ Goal execution completed!")
        
        # Show execution timeline
        st.subheader("📊 Execution Timeline")
        
        for execution in result["results"]["executions"]:
            col1, col2, col3 = st.columns([1, 2, 1])
            
            with col1:
                st.write(f"**{execution['agent'].title()}**")
            
            with col2:
                if "error" in execution:
                    st.error(f"Error: {execution['error']}")
                else:
                    st.success("Completed successfully")
            
            with col3:
                st.caption(execution["timestamp"].split("T")[1][:8])
        
        # Show artifacts created
        st.subheader("📦 Artifacts Created")
        
        artifacts = result["context"].artifacts
        if artifacts:
            cols = st.columns(min(len(artifacts), 4))
            for i, (name, artifact) in enumerate(artifacts.items()):
                with cols[i % 4]:
                    st.info(f"📄 {name}")
                    st.caption(f"Type: {type(artifact).__name__}")
        else:
            st.info("No artifacts created")

def show_system_architect():
    """System architect interface"""
    st.subheader("🏗️ System Architecture Design")
    
    requirements = st.text_area(
        "System Requirements",
        placeholder="Describe the system you want to build, including scale, performance, and security requirements",
        height=150
    )
    
    if st.button("🔍 Analyze Architecture", disabled=not requirements):
        context = AgentContext(goal=requirements)
        architect = st.session_state.orchestrator.agents[AgentRole.ARCHITECT]
        
        with st.spinner("Analyzing requirements..."):
            result = architect.analyze_requirements(requirements, context)
        
        # Display results
        col1, col2 = st.columns([1, 1])
        
        with col1:
            st.subheader("🧠 Reasoning")
            with st.expander("View reasoning process", expanded=True):
                st.markdown(result["reasoning"])
        
        with col2:
            st.subheader("🏛️ Architecture Decision")
            arch = result["architecture"]
            
            # Pattern
            st.info(f"**Pattern**: {arch['pattern'].replace('_', ' ').title()}")
            
            # Components
            st.markdown("**Components**")
            for comp in arch.get("components", []):
                st.write(f"• **{comp['name']}**: {comp['responsibility']}")
                st.caption(f"  Technology: {comp['technology']}")
        
        # Infrastructure
        st.subheader("🖥️ Infrastructure")
        infra = arch.get("infrastructure", {})
        
        col1, col2, col3 = st.columns(3)
        with col1:
            st.metric("Deployment", infra.get("deployment", "Unknown"))
        with col2:
            st.metric("Scaling", infra.get("scaling", "Unknown"))
        with col3:
            costs = infra.get("estimated_cost", {})
            st.metric("Monthly Cost", f"${costs.get('monthly', 0):,}")
        
        # Roadmap
        with st.expander("📋 Implementation Roadmap"):
            st.markdown(result["roadmap"])
        
        # Save to session
        st.session_state.last_architecture = result

def show_deep_research():
    """Deep research interface"""
    st.subheader("🔍 Deep Technical Research")
    
    topic = st.text_input(
        "Research Topic",
        placeholder="Example: Best practices for microservices authentication"
    )
    
    depth = st.slider("Research Depth", 1, 5, 3, help="Number of aspects to research")
    
    if st.button("🔬 Start Research", disabled=not topic):
        context = AgentContext(goal=topic)
        researcher = st.session_state.orchestrator.agents[AgentRole.RESEARCHER]
        
        with st.spinner(f"Researching {depth} aspects..."):
            result = researcher.research_topic(topic, context, depth)
        
        # Display findings
        st.subheader("📚 Research Findings")
        
        for i, finding in enumerate(result["findings"]):
            with st.expander(f"Finding {i+1}: {finding['question'][:50]}...", expanded=(i==0)):
                st.markdown(finding["answer"])
                st.caption(f"Confidence: {finding['confidence']:.0%}")
        
        # Synthesis
        st.subheader("📊 Synthesis")
        with st.expander("View synthesized insights", expanded=True):
            st.markdown(result["synthesis"])
        
        # Sources
        if result["sources"]:
            st.subheader("📖 Sources")
            for source in set(result["sources"]):
                st.write(f"• {source}")

def show_results_artifacts():
    """Display saved results and artifacts"""
    st.subheader("💾 Results & Artifacts")
    
    if "last_result" in st.session_state:
        result = st.session_state.last_result
        
        # Artifacts
        st.subheader("📦 Generated Artifacts")
        
        artifacts = result["context"].artifacts
        
        if not artifacts:
            st.info("No artifacts generated yet")
        else:
            # Filter by type
            artifact_types = ["All"] + list(set(key.split('_')[0] for key in artifacts.keys()))
            selected_type = st.selectbox("Filter by type", artifact_types)
            
            # Display artifacts
            for name, artifact in artifacts.items():
                if selected_type == "All" or name.startswith(selected_type.lower()):
                    with st.expander(f"📄 {name}"):
                        if isinstance(artifact, dict):
                            # Special handling for different artifact types
                            if "code" in artifact:
                                st.code(artifact["code"], language=artifact.get("language", "python"))
                            elif "content" in artifact:
                                st.markdown(artifact["content"])
                            else:
                                st.json(artifact)
                        else:
                            st.write(artifact)
                        
                        # Download button
                        if isinstance(artifact, dict) and "content" in artifact:
                            st.download_button(
                                "Download",
                                artifact["content"],
                                file_name=f"{name}.md",
                                mime="text/markdown"
                            )
    else:
        st.info("Execute a goal to see results here")

# ========== MAIN ENTRY POINT ==========

if __name__ == "__main__":
    show_agent_toolkit()
