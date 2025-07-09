"""
TuoKit Integration Example: System Architect Agent
Shows how to adapt the AI System Architect for TuoKit
"""

from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import json
import streamlit as st
from utils import safe_ollama_generate, DatabaseManager, capture_knowledge

# Reuse TuoKit's existing agent infrastructure
from toolkits.agent_hub import BaseAgent, AgentType

class ArchitecturePattern(str, Enum):
    """Architectural patterns for system design"""
    MICROSERVICES = "microservices"
    MONOLITHIC = "monolithic"
    SERVERLESS = "serverless"
    EVENT_DRIVEN = "event_driven"
    LAYERED = "layered"
    HEXAGONAL = "hexagonal"

@dataclass
class TuoKitArchitectureDecision:
    """Architecture decision format for TuoKit knowledge base"""
    pattern: ArchitecturePattern
    rationale: str
    trade_offs: Dict[str, List[str]]
    estimated_cost: Dict[str, float]
    implementation_steps: List[str]
    technology_stack: List[str]
    risks: List[Dict[str, str]]

class SystemArchitectAgent(BaseAgent):
    """
    System Architecture Agent adapted for TuoKit
    Uses local Ollama models instead of cloud APIs
    """
    
    def __init__(self):
        super().__init__(
            name="System Architect",
            description="Expert in software architecture and system design",
            tools=["architecture_planner", "tech_stack_advisor", "cost_estimator"],
            agent_type=AgentType.SPECIALIST
        )
        self.architecture_schema = self._get_architecture_schema()
    
    def _get_architecture_schema(self) -> str:
        """Get the JSON schema for architecture decisions"""
        return """
{
    "pattern": "microservices|monolithic|serverless|event_driven|layered|hexagonal",
    "rationale": "Detailed explanation for the architectural choice",
    "trade_offs": {
        "advantages": ["list of advantages"],
        "disadvantages": ["list of disadvantages"]
    },
    "estimated_cost": {
        "development": 50000,
        "monthly_operations": 5000,
        "scaling_per_1000_users": 500
    },
    "implementation_steps": [
        "Step 1: Set up development environment",
        "Step 2: Design service boundaries",
        "..."
    ],
    "technology_stack": [
        "Python/FastAPI",
        "PostgreSQL",
        "Redis",
        "Docker/Kubernetes",
        "..."
    ],
    "risks": [
        {
            "risk": "Description of risk",
            "mitigation": "How to mitigate",
            "impact": "high|medium|low"
        }
    ]
}
"""
    
    def analyze_requirements(self, requirements: str, model: str = "deepseek-r1:1.5b") -> Dict[str, Any]:
        """
        Analyze project requirements and suggest architecture
        Uses DeepSeek R1 for reasoning, similar to original
        """
        
        # Phase 1: Initial Analysis with Reasoning
        reasoning_prompt = f"""
You are an expert software architect. Analyze these requirements and think through the best architecture:

Requirements:
{requirements}

Consider:
1. Scale requirements
2. Performance needs
3. Security requirements
4. Development complexity
5. Maintenance burden
6. Cost implications

First, reason through your analysis step by step.
"""
        
        reasoning_response = safe_ollama_generate(model, reasoning_prompt)
        reasoning_content = reasoning_response.get('response', '')
        
        # Phase 2: Structured Output Generation
        structured_prompt = f"""
Based on the analysis of these requirements:
{requirements}

And this reasoning:
{reasoning_content}

Generate a JSON object with your architecture recommendation following this schema:
{self.architecture_schema}

Return ONLY valid JSON, no explanations.
"""
        
        structured_response = safe_ollama_generate(model, structured_prompt)
        
        try:
            # Parse JSON response
            import re
            json_match = re.search(r'\{.*\}', structured_response['response'], re.DOTALL)
            if json_match:
                architecture_decision = json.loads(json_match.group())
            else:
                # Fallback structure
                architecture_decision = self._create_fallback_architecture(requirements)
                
        except json.JSONDecodeError:
            architecture_decision = self._create_fallback_architecture(requirements)
        
        # Phase 3: Generate Detailed Explanation
        explanation_prompt = f"""
Explain this architecture decision in detail:

Decision: {json.dumps(architecture_decision, indent=2)}

Provide:
1. Why this architecture fits the requirements
2. Implementation roadmap with milestones
3. Team structure recommendations
4. Key technical decisions
5. Success metrics

Format as a comprehensive technical document.
"""
        
        explanation_response = safe_ollama_generate("deepseek-coder:6.7b", explanation_prompt)
        
        # Combine everything
        result = {
            "reasoning": reasoning_content,
            "architecture": architecture_decision,
            "detailed_explanation": explanation_response['response'],
            "knowledge_captured": False
        }
        
        # Capture in TuoKit's knowledge base
        if self.db and self.db.connected:
            query_id = self.db.log_query(
                tool="system_architect",
                model=model,
                prompt=requirements,
                response=json.dumps(result)
            )
            
            # Extract key insights for knowledge base
            self.db.save_knowledge_unit(
                query_id=query_id,
                title=f"Architecture: {architecture_decision.get('pattern', 'Unknown')}",
                content=json.dumps(architecture_decision, indent=2),
                category="Architecture Patterns"
            )
            
            result["knowledge_captured"] = True
            result["query_id"] = query_id
        
        return result
    
    def _create_fallback_architecture(self, requirements: str) -> Dict:
        """Create a sensible fallback architecture if parsing fails"""
        return {
            "pattern": "microservices",
            "rationale": "Microservices provide good scalability and team independence",
            "trade_offs": {
                "advantages": ["Scalability", "Team autonomy", "Technology flexibility"],
                "disadvantages": ["Complexity", "Network overhead", "Data consistency challenges"]
            },
            "estimated_cost": {
                "development": 100000,
                "monthly_operations": 5000,
                "scaling_per_1000_users": 1000
            },
            "implementation_steps": [
                "Define service boundaries",
                "Set up development environment",
                "Implement core services",
                "Add monitoring and logging",
                "Deploy to production"
            ],
            "technology_stack": ["Python", "PostgreSQL", "Redis", "Docker", "Kubernetes"],
            "risks": [
                {
                    "risk": "Service communication complexity",
                    "mitigation": "Use service mesh",
                    "impact": "medium"
                }
            ]
        }
    
    def generate_implementation_plan(self, architecture: Dict) -> str:
        """Generate detailed implementation plan from architecture decision"""
        prompt = f"""
Create a detailed implementation plan for this architecture:

{json.dumps(architecture, indent=2)}

Include:
1. Week-by-week timeline
2. Required team members and skills
3. Infrastructure setup checklist
4. Development milestones
5. Testing strategy
6. Deployment plan
7. Monitoring setup

Format as a project plan document.
"""
        
        response = safe_ollama_generate("deepseek-coder:6.7b", prompt)
        return response['response']

# Streamlit UI Component for TuoKit
def show_system_architect_ui():
    """UI component that can be added to TuoKit"""
    st.title("🏗️ System Architecture Planner")
    st.caption("AI-powered architecture design and planning")
    
    # Initialize agent
    if "architect_agent" not in st.session_state:
        st.session_state.architect_agent = SystemArchitectAgent()
    
    # Input section
    st.subheader("📋 Project Requirements")
    
    # Quick templates
    template = st.selectbox(
        "Start with a template (optional)",
        ["Custom", "E-commerce Platform", "Real-time Chat", "Data Pipeline", "Mobile Backend"]
    )
    
    if template != "Custom":
        templates = {
            "E-commerce Platform": """Build an e-commerce platform that needs to:
- Handle 100,000 concurrent users
- Process payments securely
- Manage inventory in real-time
- Support multiple payment gateways
- Scale globally with multi-region support""",
            
            "Real-time Chat": """Create a real-time chat application that requires:
- Support for 50,000 concurrent connections
- Message delivery under 100ms
- End-to-end encryption
- File sharing up to 100MB
- Voice/video calling capabilities""",
            
            "Data Pipeline": """Design a data processing pipeline that must:
- Ingest 1TB of data daily
- Process streaming data with <1 minute latency
- Support complex transformations
- Handle data quality validation
- Output to multiple data warehouses""",
            
            "Mobile Backend": """Develop a mobile app backend that needs to:
- Support iOS and Android apps
- Handle offline sync
- Push notifications
- User authentication with social login
- Real-time location tracking"""
        }
        requirements = st.text_area(
            "Project Requirements",
            value=templates[template],
            height=200
        )
    else:
        requirements = st.text_area(
            "Project Requirements",
            placeholder="Describe your project requirements, constraints, scale, and any specific needs...",
            height=200
        )
    
    # Advanced options
    with st.expander("⚙️ Advanced Options"):
        col1, col2 = st.columns(2)
        with col1:
            priority = st.selectbox(
                "Optimization Priority",
                ["Balanced", "Performance", "Cost", "Security", "Scalability"]
            )
        with col2:
            budget_range = st.select_slider(
                "Budget Range",
                ["<$10k", "$10-50k", "$50-100k", "$100-500k", ">$500k"]
            )
    
    # Analyze button
    if st.button("🔍 Analyze Architecture", type="primary", disabled=not requirements):
        with st.spinner("🤔 Analyzing requirements and designing architecture..."):
            result = st.session_state.architect_agent.analyze_requirements(requirements)
        
        # Display results
        col1, col2 = st.columns([1, 1])
        
        with col1:
            st.subheader("🧠 Reasoning Process")
            with st.expander("View reasoning", expanded=True):
                st.markdown(result["reasoning"])
        
        with col2:
            st.subheader("🏗️ Architecture Decision")
            arch = result["architecture"]
            
            # Key decision
            st.info(f"**Recommended Pattern**: {arch.get('pattern', 'Unknown').replace('_', ' ').title()}")
            
            # Rationale
            st.markdown("**Rationale**")
            st.write(arch.get("rationale", ""))
            
            # Trade-offs
            col_a, col_b = st.columns(2)
            with col_a:
                st.markdown("✅ **Advantages**")
                for adv in arch.get("trade_offs", {}).get("advantages", []):
                    st.write(f"• {adv}")
            
            with col_b:
                st.markdown("❌ **Disadvantages**")
                for dis in arch.get("trade_offs", {}).get("disadvantages", []):
                    st.write(f"• {dis}")
        
        # Cost estimation
        st.subheader("💰 Cost Estimation")
        costs = arch.get("estimated_cost", {})
        col1, col2, col3 = st.columns(3)
        with col1:
            st.metric("Development", f"${costs.get('development', 0):,.0f}")
        with col2:
            st.metric("Monthly Ops", f"${costs.get('monthly_operations', 0):,.0f}")
        with col3:
            st.metric("Per 1K Users", f"${costs.get('scaling_per_1000_users', 0):,.0f}")
        
        # Technology stack
        st.subheader("🔧 Technology Stack")
        tech_stack = arch.get("technology_stack", [])
        cols = st.columns(min(len(tech_stack), 5))
        for i, tech in enumerate(tech_stack[:5]):
            cols[i % 5].info(tech)
        
        # Implementation steps
        st.subheader("📋 Implementation Roadmap")
        for i, step in enumerate(arch.get("implementation_steps", [])[:5]):
            st.write(f"{i+1}. {step}")
        
        # Risks
        if arch.get("risks"):
            st.subheader("⚠️ Risk Assessment")
            for risk in arch["risks"]:
                impact_color = {"high": "🔴", "medium": "🟡", "low": "🟢"}
                impact = risk.get("impact", "medium")
                st.warning(
                    f"{impact_color.get(impact, '🟡')} **{risk.get('risk', 'Unknown risk')}**\n\n"
                    f"*Mitigation*: {risk.get('mitigation', 'No mitigation specified')}"
                )
        
        # Detailed explanation
        with st.expander("📄 Detailed Technical Specification"):
            st.markdown(result["detailed_explanation"])
        
        # Generate implementation plan
        if st.button("📅 Generate Implementation Plan"):
            with st.spinner("Creating detailed implementation plan..."):
                plan = st.session_state.architect_agent.generate_implementation_plan(arch)
            
            st.subheader("📅 Implementation Plan")
            st.markdown(plan)
            
            # Download button
            st.download_button(
                label="📥 Download Plan",
                data=plan,
                file_name="implementation_plan.md",
                mime="text/markdown"
            )
        
        # Knowledge capture status
        if result.get("knowledge_captured"):
            st.success(f"✅ Architecture decision saved to knowledge base (ID: {result.get('query_id')})")

# Integration function for TuoKit
def integrate_system_architect():
    """
    Add this to TuoKit's agent_hub.py or as a new page
    """
    # Add to agent registry
    from pages.agent_hub import AGENT_REGISTRY
    AGENT_REGISTRY["architect"] = SystemArchitectAgent()
    
    # Add to quick actions
    # In show_quick_actions() add:
    # if st.button("🏗️ Design Architecture", use_container_width=True):
    #     st.session_state.quick_action = "architect"
    
    # In handle_quick_action() add:
    # elif action == "architect":
    #     show_system_architect_ui()

if __name__ == "__main__":
    # Standalone testing
    st.set_page_config(page_title="System Architect - TuoKit", page_icon="🏗️", layout="wide")
    show_system_architect_ui()
