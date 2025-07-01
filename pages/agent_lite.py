import streamlit as st
from utils import DatabaseManager, safe_ollama_generate
import json
from typing import List, Dict, Any
from datetime import datetime

# Simplified pipeline execution
def run_pipeline(steps: List[Dict[str, Any]]) -> Dict[str, Any]:
    """Execute tool sequence with data passing"""
    results = {}
    execution_log = []
    
    for i, step in enumerate(steps):
        tool = step["tool"]
        params = step.get("params", {})
        step_name = step.get("name", f"Step {i+1}")
        
        try:
            # Execute the tool
            if tool == "sql_generator":
                from pages.sql_generator import generate_sql
                result = generate_sql(
                    params.get("query", ""),
                    params.get("dialect", "PostgreSQL")
                )
                output = result.get('sql', result.get('raw_response', ''))
                
            elif tool == "doc_summarizer":
                prompt = f"Summarize this text in {params.get('length', 100)} words: {params.get('text', '')}"
                response = safe_ollama_generate("deepseek-r1:1.5b", prompt)
                output = response['response']
                
            elif tool == "code_explainer":
                from pages.code_tools import explain_code
                output = explain_code(
                    params.get("code", ""),
                    params.get("model", "deepseek-coder:6.7b")
                )
                
            elif tool == "regex_generator":
                from pages.regex_tool import generate_regex
                output = generate_regex(
                    params.get("description", ""),
                    params.get("model", "deepseek-coder:6.7b")
                )
                
            elif tool == "error_decoder":
                from pages.error_tool import decode_error_comprehensive
                output = decode_error_comprehensive(
                    params.get("error", ""),
                    params.get("code", ""),
                    params.get("model", "deepseek-coder:6.7b")
                )
            else:
                output = f"Tool '{tool}' not implemented yet"
            
            # Store results for next steps to potentially use
            results[step_name] = output
            params["previous_results"] = results  # Make previous results available
            
            execution_log.append({
                "step": step_name,
                "tool": tool,
                "input": params,
                "output": output[:200] + "..." if len(str(output)) > 200 else output,
                "success": True,
                "timestamp": datetime.now().isoformat()
            })
            
        except Exception as e:
            error_msg = f"Error in {tool}: {str(e)}"
            results[step_name] = error_msg
            execution_log.append({
                "step": step_name,
                "tool": tool,
                "input": params,
                "output": error_msg,
                "success": False,
                "timestamp": datetime.now().isoformat()
            })
    
    return {"results": results, "log": execution_log}
# Educational companion
class EducationalAgent:
    """Provides contextual guidance and learning support"""
    
    def guide(self, context: str, action: str) -> Dict[str, str]:
        """Provide contextual guidance"""
        prompt = f"""
        User is working on: {context}
        Current action: {action}
        
        Provide helpful guidance in JSON format:
        {{
            "explanation": "One clear sentence explaining what this action does",
            "tip": "Best practice tip for this situation",
            "mistake": "Common mistake to avoid",
            "next_step": "Suggested next action"
        }}
        
        Be specific and practical. Focus on TuoKit tools.
        """
        
        response = safe_ollama_generate(
            model="deepseek-r1:1.5b",
            prompt=prompt,
            temperature=0.7
        )
        
        try:
            # Try to parse JSON response
            result = json.loads(response['response'])
            return result
        except:
            # Fallback to structured response
            return {
                "explanation": "This action helps process your data efficiently.",
                "tip": "Start with simple parameters and refine as needed.",
                "mistake": "Avoid complex queries on your first attempt.",
                "next_step": "Test with a small dataset first."
            }

# Streamlit UI
st.set_page_config(
    page_title="TuoKit - Lite Agents",
    page_icon="🤖",
    layout="wide"
)

st.title("🤖 Lite Agent System")
st.markdown("**Simple automation and learning companion**")

# Initialize database
if "db" not in st.session_state:
    st.session_state.db = DatabaseManager()

# Agent selection
agent_type = st.radio(
    "Select Agent Mode",
    ["🔄 Pipeline Automator", "🎓 Educational Companion"],
    horizontal=True
)
# PIPELINE AGENT MODE
if agent_type == "🔄 Pipeline Automator":
    st.subheader("Build Workflow Pipeline")
    st.markdown("Chain tools together to automate complex tasks")
    
    # Initialize pipeline steps
    if "pipeline_steps" not in st.session_state:
        st.session_state.pipeline_steps = []
    
    # Step builder
    col1, col2 = st.columns([3, 1])
    
    with col1:
        st.markdown("### Pipeline Steps")
        
        # Display existing steps
        for i, step in enumerate(st.session_state.pipeline_steps):
            with st.container(border=True):
                step_cols = st.columns([2, 2, 1])
                
                with step_cols[0]:
                    step["name"] = st.text_input(
                        f"Step {i+1} Name",
                        value=step.get("name", ""),
                        placeholder="e.g., Extract Data",
                        key=f"name_{i}"
                    )
                
                with step_cols[1]:
                    tools = ["", "sql_generator", "code_explainer", "doc_summarizer", 
                            "regex_generator", "error_decoder"]
                    current_tool = step.get("tool", "")
                    tool_index = tools.index(current_tool) if current_tool in tools else 0
                    
                    step["tool"] = st.selectbox(
                        "Tool",
                        tools,
                        index=tool_index,
                        key=f"tool_{i}"
                    )
                
                with step_cols[2]:
                    if st.button("❌ Remove", key=f"remove_{i}"):
                        st.session_state.pipeline_steps.pop(i)
                        st.rerun()
                
                # Tool-specific parameters
                if step["tool"]:
                    st.markdown(f"**Parameters for {step['tool']}:**")
                    
                    if step["tool"] == "sql_generator":
                        step.setdefault("params", {})
                        step["params"]["query"] = st.text_area(
                            "Query Description",
                            value=step["params"].get("query", ""),
                            placeholder="Find top customers by revenue",
                            key=f"query_{i}"
                        )
                        step["params"]["dialect"] = st.selectbox(
                            "SQL Dialect",
                            ["PostgreSQL", "MySQL", "SQLite"],
                            key=f"dialect_{i}"
                        )
                    
                    elif step["tool"] == "code_explainer":
                        step.setdefault("params", {})
                        step["params"]["code"] = st.text_area(
                            "Code to Explain",
                            value=step["params"].get("code", ""),
                            placeholder="Paste code here...",
                            key=f"code_{i}"
                        )
                    
                    elif step["tool"] == "doc_summarizer":
                        step.setdefault("params", {})
                        step["params"]["text"] = st.text_area(
                            "Text to Summarize",
                            value=step["params"].get("text", ""),
                            placeholder="Paste document text...",
                            key=f"text_{i}"
                        )
                        step["params"]["length"] = st.slider(
                            "Summary Length (words)",
                            50, 500,
                            value=step["params"].get("length", 100),
                            key=f"length_{i}"
                        )
                    
                    elif step["tool"] == "regex_generator":
                        step.setdefault("params", {})
                        step["params"]["description"] = st.text_input(
                            "Pattern Description",
                            value=step["params"].get("description", ""),
                            placeholder="Match email addresses",
                            key=f"regex_{i}"
                        )
                    
                    elif step["tool"] == "error_decoder":
                        step.setdefault("params", {})
                        step["params"]["error"] = st.text_area(
                            "Error Message",
                            value=step["params"].get("error", ""),
                            placeholder="Paste error message...",
                            key=f"error_{i}"
                        )
                        step["params"]["code"] = st.text_area(
                            "Related Code (optional)",
                            value=step["params"].get("code", ""),
                            key=f"error_code_{i}"
                        )        
        # Add step button
        if st.button("➕ Add Step", type="primary", use_container_width=True):
            st.session_state.pipeline_steps.append({
                "name": "",
                "tool": "",
                "params": {}
            })
            st.rerun()
    
    with col2:
        st.markdown("### Actions")
        
        # Execute pipeline
        steps_ready = any(step.get("tool") for step in st.session_state.pipeline_steps)
        if st.button("▶️ Execute Pipeline", 
                    type="primary",
                    disabled=not steps_ready,
                    use_container_width=True):
            
            with st.spinner("Running workflow..."):
                result = run_pipeline(st.session_state.pipeline_steps)
            
            st.session_state.last_result = result
            
            # Save to database if connected
            if st.session_state.db.connected:
                pipeline_name = " → ".join([
                    s["name"] or s["tool"] 
                    for s in st.session_state.pipeline_steps if s.get("tool")
                ])
                
                # Log as a special query type
                st.session_state.db.log_query(
                    tool="pipeline_automator",
                    model="multiple",
                    prompt=json.dumps(st.session_state.pipeline_steps),
                    response=json.dumps(result),
                    metadata={
                        "pipeline_name": pipeline_name,
                        "step_count": len(st.session_state.pipeline_steps),
                        "success": all(step["success"] for step in result["log"])
                    }
                )
        
        # Load example pipelines
        st.markdown("### Example Pipelines")
        
        if st.button("📊 Data Analysis", use_container_width=True):
            st.session_state.pipeline_steps = [
                {
                    "name": "Extract Data",
                    "tool": "sql_generator",
                    "params": {"query": "Get customer orders from last month", "dialect": "PostgreSQL"}
                },
                {
                    "name": "Clean Data",
                    "tool": "regex_generator",
                    "params": {"description": "Extract and validate email addresses"}
                }
            ]
            st.rerun()
        
        if st.button("🔧 Code Migration", use_container_width=True):
            st.session_state.pipeline_steps = [
                {
                    "name": "Analyze Legacy Code",
                    "tool": "code_explainer",
                    "params": {"code": "# Add your legacy code here"}
                },
                {
                    "name": "Debug Issues",
                    "tool": "error_decoder",
                    "params": {"error": "# Add any error messages"}
                }
            ]
            st.rerun()
    
    # Display results
    if "last_result" in st.session_state:
        st.divider()
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.subheader("📊 Pipeline Results")
            for step_name, output in st.session_state.last_result["results"].items():
                with st.expander(f"**{step_name}**", expanded=True):
                    if isinstance(output, str) and len(output) > 500:
                        st.code(output[:500] + "...\n\n[Truncated]")
                    else:
                        st.code(output)
        
        with col2:
            st.subheader("📝 Execution Log")
            for entry in st.session_state.last_result["log"]:
                status = "✅" if entry["success"] else "❌"
                with st.expander(f"{status} {entry['step']}"):
                    st.json(entry)
# EDUCATIONAL COMPANION MODE
else:  # Educational Companion
    st.subheader("🎓 Learning Companion")
    st.markdown("Get real-time guidance while using TuoKit tools")
    
    # Initialize agent
    if "edu_agent" not in st.session_state:
        st.session_state.edu_agent = EducationalAgent()
        st.session_state.guidance_history = []
    
    # Context input
    col1, col2 = st.columns([2, 1])
    
    with col1:
        context = st.text_area(
            "What are you working on?",
            placeholder="Example: Trying to analyze customer feedback from CSV files and generate insights",
            height=100
        )
        
        if context:
            # Action selection
            user_action = st.selectbox(
                "What are you trying to do?",
                [
                    "Selecting the right tool",
                    "Configuring tool parameters",
                    "Understanding tool output",
                    "Debugging errors",
                    "Optimizing my workflow",
                    "Learning best practices"
                ]
            )
            
            # Get guidance
            if st.button("💡 Get Guidance", type="primary"):
                with st.spinner("Thinking..."):
                    guidance = st.session_state.edu_agent.guide(context, user_action)
                
                # Store in history
                st.session_state.guidance_history.append({
                    "context": context,
                    "action": user_action,
                    "guidance": guidance,
                    "timestamp": datetime.now()
                })
                
                # Display guidance
                st.success("Here's your guidance:")
                
                col_a, col_b = st.columns(2)
                
                with col_a:
                    st.markdown(f"**📖 Explanation**")
                    st.info(guidance.get("explanation", ""))
                    
                    st.markdown(f"**💡 Pro Tip**")
                    st.success(guidance.get("tip", ""))
                
                with col_b:
                    st.markdown(f"**⚠️ Common Mistake**")
                    st.warning(guidance.get("mistake", ""))
                    
                    st.markdown(f"**➡️ Next Step**")
                    st.info(guidance.get("next_step", ""))
    
    with col2:
        st.markdown("### Quick Scenarios")
        
        scenarios = {
            "📊 Data Analysis": {
                "context": "I need to analyze sales data from multiple CSV files",
                "action": "Selecting the right tool"
            },
            "🔍 Text Processing": {
                "context": "I want to extract phone numbers from documents",
                "action": "Configuring tool parameters"
            },
            "🐛 Error Fixing": {
                "context": "My SQL query is returning unexpected results",
                "action": "Debugging errors"
            },
            "🚀 Performance": {
                "context": "My pipeline is taking too long to run",
                "action": "Optimizing my workflow"
            }
        }
        
        for scenario_name, scenario_data in scenarios.items():
            if st.button(scenario_name, use_container_width=True):
                # Pre-fill the scenario
                st.session_state.prefill_context = scenario_data["context"]
                st.session_state.prefill_action = scenario_data["action"]
                st.rerun()
    
    # Apply pre-filled scenario if exists
    if "prefill_context" in st.session_state:
        context = st.session_state.prefill_context
        del st.session_state.prefill_context
    
    # Guidance History
    if st.session_state.guidance_history:
        st.divider()
        st.subheader("📚 Your Learning Journey")
        
        # Display in reverse chronological order
        for i, entry in enumerate(reversed(st.session_state.guidance_history)):
            with st.expander(
                f"Guidance #{len(st.session_state.guidance_history) - i} - "
                f"{entry['timestamp'].strftime('%I:%M %p')}"
            ):
                st.markdown(f"**Context:** {entry['context']}")
                st.markdown(f"**Action:** {entry['action']}")
                st.markdown("---")
                
                guidance = entry['guidance']
                st.markdown(f"📖 **Explanation:** {guidance.get('explanation', '')}")
                st.markdown(f"💡 **Tip:** {guidance.get('tip', '')}")
                st.markdown(f"⚠️ **Avoid:** {guidance.get('mistake', '')}")
                st.markdown(f"➡️ **Next:** {guidance.get('next_step', '')}")
        
        # Export learning history
        if st.button("📥 Export Learning History"):
            history_text = "# TuoKit Learning History\n\n"
            for entry in st.session_state.guidance_history:
                history_text += f"## {entry['timestamp'].strftime('%Y-%m-%d %I:%M %p')}\n"
                history_text += f"**Context:** {entry['context']}\n"
                history_text += f"**Action:** {entry['action']}\n\n"
                
                guidance = entry['guidance']
                history_text += f"### Guidance\n"
                history_text += f"- **Explanation:** {guidance.get('explanation', '')}\n"
                history_text += f"- **Tip:** {guidance.get('tip', '')}\n"
                history_text += f"- **Avoid:** {guidance.get('mistake', '')}\n"
                history_text += f"- **Next Step:** {guidance.get('next_step', '')}\n\n"
                history_text += "---\n\n"
            
            st.download_button(
                label="Download History",
                data=history_text,
                file_name=f"tuokit_learning_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md",
                mime="text/markdown"
            )

# Sidebar help
with st.sidebar:
    st.markdown("### 🤖 Lite Agents Help")
    
    with st.expander("Pipeline Automator"):
        st.markdown("""
        **Build multi-step workflows:**
        1. Add steps with the ➕ button
        2. Select tools for each step
        3. Configure parameters
        4. Execute to run all steps in sequence
        
        **Tips:**
        - Name your steps clearly
        - Start simple, then add complexity
        - Previous results are available to later steps
        """)
    
    with st.expander("Educational Companion"):
        st.markdown("""
        **Get contextual help:**
        1. Describe what you're working on
        2. Select your current action
        3. Get tailored guidance
        
        **Best for:**
        - Learning which tool to use
        - Understanding parameters
        - Avoiding common mistakes
        - Planning next steps
        """)
