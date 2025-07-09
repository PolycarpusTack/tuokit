"""
Agent Hub UI Components
Streamlit interface for the agent system
"""

import streamlit as st
from typing import Dict, List, Optional, Any
import json
import pandas as pd
from datetime import datetime

from .core import AgentState, AgentType
from .registry import list_agents, get_agent
from .orchestrator import AgentOrchestrator
from .pipelines import PipelineExecutor
from utils import apply_modern_theme


class AgentHubUI:
    """Main UI class for Agent Hub"""
    
    def __init__(self):
        self.orchestrator = AgentOrchestrator()
        self.pipeline_executor = PipelineExecutor()
        
        # Initialize session state
        self._init_session_state()
    
    def _init_session_state(self):
        """Initialize session state variables"""
        if 'agent_history' not in st.session_state:
            st.session_state.agent_history = []
        if 'current_pipeline' not in st.session_state:
            st.session_state.current_pipeline = []
        if 'execution_results' not in st.session_state:
            st.session_state.execution_results = None
    
    def run(self):
        """Main UI entry point"""
        apply_modern_theme()
        
        # Header
        st.markdown("""
        <h1 style='text-align: center; color: #4CAF50;'>
            🤖 TuoKit Agent Hub
        </h1>
        <p style='text-align: center; color: #888;'>
            Unified AI agent system with goal orchestration
        </p>
        """, unsafe_allow_html=True)
        
        # Sidebar
        with st.sidebar:
            self._render_sidebar()
        
        # Main content
        tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs([
            "🎯 Goal Orchestration", 
            "⚡ Quick Actions",
            "🔧 Pipeline Builder",
            "🖼️ MultiModal Analysis",
            "🤖 Agent Builder",
            "📊 Agent Analytics"
        ])
        
        with tab1:
            self._render_goal_orchestration()
        
        with tab2:
            self._render_quick_actions()
        
        with tab3:
            self._render_pipeline_builder()
        
        with tab4:
            self._render_multimodal()
        
        with tab5:
            self._render_agent_builder()
        
        with tab6:
            self._render_analytics()
    
    def _render_sidebar(self):
        """Render sidebar with agent info"""
        st.header("🤖 Available Agents")
        
        agents = list_agents()
        for name, info in agents.items():
            with st.expander(f"{name.capitalize()} Agent"):
                st.write(f"**Type:** {info['type']}")
                st.write(f"**Description:** {info['description']}")
                st.write(f"**Tools:** {', '.join(info['tools'])}")
                st.write(f"**Success Rate:** {info['success_rate']:.1%}")
                st.write(f"**Executions:** {info['execution_count']}")
        
        st.divider()
        
        # Execution history
        st.header("📜 Recent Executions")
        if st.session_state.agent_history:
            for i, execution in enumerate(st.session_state.agent_history[-5:]):
                with st.expander(f"Execution {i+1}"):
                    st.json(execution)
        else:
            st.info("No executions yet")
    
    def _render_goal_orchestration(self):
        """Render goal orchestration interface"""
        st.header("🎯 Define Your Goal")
        
        col1, col2 = st.columns([3, 1])
        
        with col1:
            goal = st.text_area(
                "What would you like to achieve?",
                placeholder="Example: Create a REST API with user authentication and CRUD operations for a blog system",
                height=100
            )
        
        with col2:
            execution_mode = st.selectbox(
                "Execution Mode",
                ["sequential", "parallel", "adaptive"],
                help="Sequential: One task at a time\nParallel: Independent tasks simultaneously\nAdaptive: Adjusts based on results"
            )
            
            st.divider()
            
            max_agents = st.number_input(
                "Max Agents",
                min_value=1,
                max_value=10,
                value=3,
                help="Maximum number of agents to involve"
            )
        
        # Context
        with st.expander("📋 Additional Context (Optional)"):
            context_text = st.text_area(
                "Provide any additional context",
                placeholder="Technical requirements, constraints, preferences..."
            )
            
            context = {}
            if context_text:
                try:
                    context = json.loads(context_text)
                except:
                    context = {"notes": context_text}
        
        # Execute button
        if st.button("🚀 Execute Goal", type="primary", use_container_width=True):
            if goal:
                with st.spinner("Orchestrating agents..."):
                    result = self.orchestrator.orchestrate(
                        goal=goal,
                        context=context,
                        mode=execution_mode
                    )
                    
                    st.session_state.execution_results = result
                    st.session_state.agent_history.append({
                        'timestamp': datetime.now().isoformat(),
                        'goal': goal,
                        'result': result
                    })
        
        # Display results
        if st.session_state.execution_results:
            self._render_execution_results(st.session_state.execution_results)
    
    def _render_execution_results(self, results: Dict[str, Any]):
        """Render execution results"""
        st.divider()
        
        # Summary
        success = results.get('success', False)
        if success:
            st.success("✅ Goal achieved successfully!")
        else:
            st.error("❌ Goal execution failed")
        
        # Metrics
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric(
                "Duration",
                f"{results.get('duration', 0):.1f}s"
            )
        
        with col2:
            st.metric(
                "Tasks",
                len(results.get('results', []))
            )
        
        with col3:
            success_count = sum(
                1 for r in results.get('results', []) 
                if r.get('success', False)
            )
            st.metric(
                "Success Rate",
                f"{success_count}/{len(results.get('results', []))}"
            )
        
        with col4:
            st.metric(
                "Mode",
                results.get('mode', 'unknown')
            )
        
        # Detailed results
        st.subheader("📋 Execution Details")
        
        for i, task_result in enumerate(results.get('results', [])):
            task = task_result.get('task', {})
            success = task_result.get('success', False)
            
            with st.expander(
                f"{'✅' if success else '❌'} Task {i+1}: {task.get('task', 'Unknown')}",
                expanded=not success
            ):
                st.write(f"**Agent:** {task_result.get('agent', 'Unknown')}")
                
                if success:
                    st.success("Task completed successfully")
                    result_data = task_result.get('result', {})
                    if isinstance(result_data, dict):
                        st.json(result_data)
                    else:
                        st.code(str(result_data))
                else:
                    st.error(f"Error: {task_result.get('error', 'Unknown error')}")
    
    def _render_quick_actions(self):
        """Render quick actions interface"""
        st.header("⚡ Quick Actions")
        
        # Predefined quick actions
        quick_actions = {
            "Code Review": {
                "agent": "code",
                "tool": "code_reviewer",
                "description": "Review code for issues and improvements"
            },
            "SQL Optimization": {
                "agent": "sql",
                "tool": "sql_optimizer", 
                "description": "Optimize SQL queries for performance"
            },
            "Generate Tests": {
                "agent": "code",
                "tool": "test_generator",
                "description": "Generate unit tests for code"
            },
            "Explain Error": {
                "agent": "analysis",
                "tool": "error_decoder",
                "description": "Decode and explain error messages"
            },
            "Document Code": {
                "agent": "docs",
                "tool": "doc_generator",
                "description": "Generate documentation for code"
            }
        }
        
        # Display quick actions
        cols = st.columns(3)
        
        for i, (action_name, action_info) in enumerate(quick_actions.items()):
            with cols[i % 3]:
                if st.button(
                    f"{action_name}\n{action_info['description']}",
                    key=f"quick_{action_name}",
                    use_container_width=True
                ):
                    st.session_state.selected_action = action_name
                    st.session_state.selected_agent = action_info['agent']
                    st.session_state.selected_tool = action_info['tool']
        
        # Action input area
        if 'selected_action' in st.session_state:
            st.divider()
            st.subheader(f"🎯 {st.session_state.selected_action}")
            
            input_text = st.text_area(
                "Enter your input:",
                placeholder="Paste your code, SQL query, or error message here...",
                height=200
            )
            
            if st.button("Execute", type="primary"):
                if input_text:
                    self._execute_quick_action(
                        st.session_state.selected_agent,
                        st.session_state.selected_tool,
                        input_text
                    )
    
    def _render_pipeline_builder(self):
        """Render pipeline builder interface"""
        st.header("🔧 Pipeline Builder")
        
        # Pipeline type selection
        pipeline_type = st.selectbox(
            "Pipeline Type",
            ["simple", "advanced", "educational", "research"],
            help="Choose the type of pipeline execution"
        )
        
        # Step builder
        st.subheader("Add Pipeline Steps")
        
        col1, col2, col3 = st.columns([2, 2, 1])
        
        with col1:
            available_agents = list_agents()
            selected_agent = st.selectbox(
                "Select Agent",
                list(available_agents.keys())
            )
        
        with col2:
            if selected_agent:
                agent = get_agent(selected_agent)
                selected_tool = st.selectbox(
                    "Select Tool",
                    agent.tools if agent else []
                )
        
        with col3:
            if st.button("➕ Add Step"):
                if selected_agent and selected_tool:
                    st.session_state.current_pipeline.append({
                        "agent": selected_agent,
                        "tool": selected_tool,
                        "params": {}
                    })
        
        # Display current pipeline
        if st.session_state.current_pipeline:
            st.subheader("Current Pipeline")
            
            for i, step in enumerate(st.session_state.current_pipeline):
                col1, col2, col3 = st.columns([3, 3, 1])
                
                with col1:
                    st.text(f"Step {i+1}: {step['agent']}")
                
                with col2:
                    st.text(f"Tool: {step['tool']}")
                
                with col3:
                    if st.button("🗑️", key=f"remove_{i}"):
                        st.session_state.current_pipeline.pop(i)
                        st.rerun()
            
            # Execute pipeline
            if st.button("🚀 Execute Pipeline", type="primary", use_container_width=True):
                result = self.pipeline_executor.execute(
                    pipeline_type,
                    st.session_state.current_pipeline
                )
                st.session_state.execution_results = result
            
            # Clear pipeline
            if st.button("🧹 Clear Pipeline"):
                st.session_state.current_pipeline = []
                st.rerun()
    
    def _render_analytics(self):
        """Render agent analytics"""
        st.header("📊 Agent Analytics")
        
        agents = list_agents()
        
        # Agent performance metrics
        st.subheader("Agent Performance")
        
        # Create DataFrame for visualization
        agent_data = []
        for name, info in agents.items():
            agent_data.append({
                'Agent': name.capitalize(),
                'Type': info['type'],
                'Executions': info['execution_count'],
                'Success Rate': info['success_rate'] * 100,
                'Tools': len(info['tools'])
            })
        
        df = pd.DataFrame(agent_data)
        
        # Display metrics
        col1, col2 = st.columns(2)
        
        with col1:
            st.bar_chart(df.set_index('Agent')['Executions'])
            st.caption("Execution Count by Agent")
        
        with col2:
            st.bar_chart(df.set_index('Agent')['Success Rate'])
            st.caption("Success Rate by Agent (%)")
        
        # Detailed stats table
        st.subheader("Detailed Statistics")
        st.dataframe(df, use_container_width=True)
        
        # Execution history analysis
        if st.session_state.agent_history:
            st.subheader("Execution History")
            
            # Recent executions chart
            history_data = []
            for execution in st.session_state.agent_history[-20:]:
                history_data.append({
                    'Time': execution['timestamp'],
                    'Success': execution['result'].get('success', False),
                    'Duration': execution['result'].get('duration', 0)
                })
            
            history_df = pd.DataFrame(history_data)
            
            if not history_df.empty:
                st.line_chart(history_df.set_index('Time')['Duration'])
                st.caption("Execution Duration Over Time")
    
    def _execute_quick_action(self, agent_name: str, tool: str, input_text: str):
        """Execute a quick action"""
        agent = get_agent(agent_name)
        if not agent:
            st.error(f"Agent {agent_name} not found")
            return
        
        with st.spinner(f"Executing {tool}..."):
            try:
                params = self._prepare_tool_params(tool, input_text)
                result = agent.execute_tool(tool, params)
                
                st.success("✅ Execution complete!")
                st.code(result)
                
            except Exception as e:
                st.error(f"Error: {str(e)}")
    
    def _prepare_tool_params(self, tool: str, input_text: str) -> Dict[str, Any]:
        """Prepare parameters for tool execution"""
        # Base parameters
        params = {}
        
        # Tool-specific parameter mapping
        if tool in ["code_explainer", "code_reviewer", "code_generator"]:
            params["code"] = input_text
        elif tool in ["sql_generator", "sql_optimizer", "sql_explainer"]:
            params["query"] = input_text
        elif tool == "error_decoder":
            params["error"] = input_text
        elif tool == "doc_qa":
            # Split into document and question
            lines = input_text.strip().split('\n')
            if len(lines) > 1:
                params["question"] = lines[0]
                params["document"] = '\n'.join(lines[1:])
            else:
                params["question"] = input_text
                params["document"] = ""
        elif tool == "doc_summarizer":
            params["text"] = input_text
        elif tool == "doc_generator":
            params["content"] = input_text
        else:
            # Generic parameter
            params["input"] = input_text
        
        # Add default model
        params["model"] = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
        
        return params
    
    def _render_analytics(self):
        """Render agent analytics"""
        st.header("📊 Agent Analytics")
        
        agents = list_agents()
        
        # Agent performance metrics
        st.subheader("Agent Performance")
        
        # Create DataFrame for visualization
        agent_data = []
        for name, info in agents.items():
            agent_data.append({
                'Agent': name.capitalize(),
                'Type': info['type'],
                'Executions': info['execution_count'],
                'Success Rate': info['success_rate'] * 100,
                'Tools': len(info['tools'])
            })
        
        df = pd.DataFrame(agent_data)
        
        # Display metrics
        col1, col2 = st.columns(2)
        
        with col1:
            st.bar_chart(df.set_index('Agent')['Executions'])
            st.caption("Execution Count by Agent")
        
        with col2:
            st.bar_chart(df.set_index('Agent')['Success Rate'])
            st.caption("Success Rate by Agent (%)")
        
        # Detailed stats table
        st.subheader("Detailed Statistics")
        st.dataframe(df, use_container_width=True)
        
        # Execution history analysis
        if st.session_state.agent_history:
            st.subheader("Execution History")
            
            # Recent executions chart
            history_data = []
            for execution in st.session_state.agent_history[-20:]:
                history_data.append({
                    'Time': execution['timestamp'],
                    'Success': execution['result'].get('success', False),
                    'Duration': execution['result'].get('duration', 0)
                })
            
            history_df = pd.DataFrame(history_data)
            
            if not history_df.empty:
                st.line_chart(history_df.set_index('Time')['Duration'])
                st.caption("Execution Duration Over Time")
    
    def _render_multimodal(self):
        """Render multimodal analysis tab"""
        st.header("🖼️ MultiModal Analysis")
        
        # Import UI components
        from .multimodal import render_screenshot_analyzer, render_document_analyzer
        
        # Sub-tabs for different analysis types
        analysis_type = st.radio(
            "Analysis Type",
            ["Screenshot Analysis", "Document Analysis"],
            horizontal=True
        )
        
        if analysis_type == "Screenshot Analysis":
            render_screenshot_analyzer(st)
        else:
            render_document_analyzer(st)
    
    def _render_agent_builder(self):
        """Render agent builder interface"""
        from .agent_builder import AgentBuilder
        builder = AgentBuilder()
        builder.render()
