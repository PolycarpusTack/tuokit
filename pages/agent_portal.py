"""
TuoKit Agent Portal - Streamlit UI
Minimal implementation with practical focus
"""
import streamlit as st
from agent_system import AgentOrchestrator, AgentState, AGENT_REGISTRY
import json

# Page configuration
st.set_page_config(
    page_title="TuoKit - Agent Portal",
    page_icon="🤖",
    layout="wide"
)

st.title("🤖 TuoKit Agent Portal")
st.markdown("**Orchestrated AI agents for complex tasks**")

# Initialize orchestrator
if "orchestrator" not in st.session_state:
    st.session_state.orchestrator = AgentOrchestrator()

# Main interface
col1, col2 = st.columns([2, 1])

with col1:
    st.subheader("🎯 Goal Definition")
    
    # Goal input
    goal = st.text_area(
        "What would you like to accomplish?",
        placeholder="Example: Create a sales dashboard with last quarter's data",
        height=100
    )    
    # Agent selection
    agent_options = ["Auto-select"] + list(AGENT_REGISTRY.keys())
    selected_agent = st.selectbox(
        "Select Agent (or let AI choose)",
        agent_options,
        help="Auto-select analyzes your goal and picks the best agent"
    )
    
    # Execute button
    if st.button("🚀 Execute Goal", type="primary", disabled=not goal):
        with st.spinner("Agent working..."):
            try:
                # Execute with selected or auto-selected agent
                agent_name = None if selected_agent == "Auto-select" else selected_agent
                state = st.session_state.orchestrator.execute_goal(goal, agent_name)
                st.session_state.last_execution = state
                st.success("✅ Goal completed!")
            except Exception as e:
                st.error(f"❌ Execution failed: {str(e)}")

with col2:
    st.subheader("🤖 Available Agents")
    for name, agent in AGENT_REGISTRY.items():
        with st.expander(f"**{agent.name}**"):
            st.write(agent.description)
            st.write(f"**Tools:** {', '.join(agent.tools)}")

# Results section
if "last_execution" in st.session_state:
    st.divider()
    st.subheader("📊 Execution Results")
    
    state = st.session_state.last_execution    
    # Execution timeline
    with st.expander("🕐 Execution Timeline", expanded=True):
        for entry in state.agent_history:
            st.write(f"• {entry}")
    
    # Step results
    st.subheader("Step Results")
    for step_key, result in state.results.items():
        with st.expander(f"📝 {step_key}"):
            if isinstance(result, str):
                st.code(result)
            else:
                st.json(result)
    
    # Save to knowledge base
    if st.button("💾 Save to Knowledge Base"):
        # Extract meaningful patterns from execution
        knowledge_prompt = f"""
        Extract reusable patterns from this execution:
        Goal: {state.goal}
        Steps: {json.dumps(state.steps)}
        Results: Success
        
        Format as: Pattern name, description, when to use
        """
        
        response = st.session_state.orchestrator.db.log_query(
            tool="agent_knowledge_extraction",
            model="deepseek-r1:1.5b",
            prompt=knowledge_prompt,
            response=json.dumps({
                "goal": state.goal,
                "agent": state.agent_history[0],
                "steps": len(state.steps),
                "phase": state.phase
            })
        )
        
        if response:
            st.success("✅ Saved to knowledge base!")
        else:
            st.warning("⚠️ Could not save to knowledge base")

# Help section
with st.sidebar:
    st.subheader("🤔 Agent Portal Help")
    st.markdown("""
    **How to use:**
    1. Enter your goal in natural language
    2. Let AI select the best agent, or choose manually
    3. Click Execute to run the orchestrated workflow
    4. Review results and save patterns
    
    **Example Goals:**
    - "Analyze last month's sales data and create visualizations"
    - "Debug this Python function that's throwing errors"
    - "Generate API documentation from code comments"
    - "Create SQL queries for customer segmentation"
    """)
