"""
🏗️ Camunda Manager - AI-Powered Workflow Management
"""

import streamlit as st
import json
import pandas as pd
from datetime import datetime, timedelta
import plotly.graph_objects as go
import xml.etree.ElementTree as ET

# Import Camunda manager (will be created in toolkits)
import sys
sys.path.append('..')
try:
    from toolkits.camunda.manager import CamundaManager, CamundaWorkflowBuilder
except ImportError:
    st.error("Please ensure toolkits/camunda/manager.py exists")
    st.stop()

from utils import apply_modern_theme, DatabaseManager

# Page config
st.set_page_config(
    page_title="Camunda Manager - TuoKit",
    page_icon="🏗️",
    layout="wide"
)

apply_modern_theme()

# Initialize session state
if 'camunda_url' not in st.session_state:
    st.session_state.camunda_url = "http://localhost:8080/engine-rest"
if 'camunda_manager' not in st.session_state:
    st.session_state.camunda_manager = None

# Header
st.title("🏗️ Camunda Manager")
st.markdown("""
AI-powered management for your Camunda workflow engine. Monitor processes, 
analyze incidents, and generate workflows from natural language.
""")

# Connection settings
with st.expander("⚙️ Connection Settings", expanded=not st.session_state.camunda_manager):
    col1, col2 = st.columns([3, 1])
    with col1:
        url = st.text_input(
            "Camunda REST API URL",
            value=st.session_state.camunda_url,
            placeholder="http://localhost:8080/engine-rest"
        )
    with col2:
        if st.button("🔌 Connect", type="primary"):
            manager = CamundaManager(url)
            if manager.test_connection():
                st.session_state.camunda_manager = manager
                st.session_state.camunda_url = url
                st.success("✅ Connected to Camunda!")
                st.rerun()
            else:
                st.error("❌ Connection failed. Check URL and ensure Camunda is running.")

# Main interface
if st.session_state.camunda_manager:
    manager = st.session_state.camunda_manager
    
    # Tabs for different functions
    tab1, tab2, tab3, tab4, tab5 = st.tabs([
        "📊 Monitor", 
        "🚨 Incidents", 
        "🤖 Generate Workflow",
        "🔧 Optimize", 
        "📈 Analytics"
    ])
    
    with tab1:
        st.header("Process Monitoring")
        
        # Refresh button
        if st.button("🔄 Refresh"):
            st.rerun()
        
        # Active processes
        processes = manager.get_process_instances()
        
        if processes:
            st.metric("Active Processes", len(processes))
            
            # Process table
            df = pd.DataFrame(processes)
            df['startTime'] = pd.to_datetime(df['startTime'])
            df['duration'] = datetime.now() - df['startTime']
            
            st.dataframe(
                df[['id', 'processDefinitionId', 'startTime', 'duration']],
                use_container_width=True
            )
        else:
            st.info("No active processes found")
    
    with tab2:
        st.header("🚨 Incident Analysis")
        
        incidents = manager.get_incidents()
        
        if incidents:
            st.error(f"Found {len(incidents)} incidents")
            
            # Incident selector
            incident_options = {
                f"{inc['processDefinitionId']} - {inc['activityId']}": inc
                for inc in incidents
            }
            
            selected = st.selectbox(
                "Select incident to analyze",
                options=list(incident_options.keys())
            )
            
            if selected:
                incident = incident_options[selected]
                
                # Display incident details
                col1, col2 = st.columns(2)
                with col1:
                    st.json({
                        'Process': incident['processDefinitionId'],
                        'Activity': incident['activityId'],
                        'Type': incident['incidentType'],
                        'Message': incident['incidentMessage']
                    })
                
                # AI Analysis
                if st.button("🤖 Analyze with AI", type="primary"):
                    with st.spinner("Analyzing incident..."):
                        analysis = manager.analyze_incident_with_ai(incident)
                        
                    with col2:
                        st.markdown("### 🧠 AI Analysis")
                        st.markdown(analysis['analysis'])
        else:
            st.success("✅ No incidents found!")    
    with tab3:
        st.header("🤖 Generate BPMN from Natural Language")
        
        st.markdown("""
        Describe your workflow in plain English, and I'll generate the BPMN XML.
        
        **Example descriptions:**
        - "Customer submits order, system validates payment, warehouse ships product, send confirmation email"
        - "Employee requests leave, manager approves, HR updates records, notify payroll"
        """)
        
        # Workflow description
        description = st.text_area(
            "Describe your workflow",
            height=150,
            placeholder="Start with the trigger event, then describe each step..."
        )
        
        col1, col2 = st.columns([1, 1])
        
        with col1:
            process_id = st.text_input("Process ID", value="GeneratedProcess")
            process_name = st.text_input("Process Name", value="AI Generated Process")
        
        if st.button("🎯 Generate BPMN", type="primary", disabled=not description):
            with st.spinner("Generating BPMN workflow..."):
                bpmn_xml = manager.generate_bpmn_from_description(description)
            
            # Display XML
            st.subheader("Generated BPMN XML")
            st.code(bpmn_xml, language="xml")
            
            # Download button
            st.download_button(
                label="⬇️ Download BPMN",
                data=bpmn_xml,
                file_name=f"{process_id}.bpmn",
                mime="application/xml"
            )
            
            # Save to knowledge base
            if st.button("💾 Save to Knowledge Base"):
                try:
                    db = DatabaseManager()
                    db.add_knowledge_entry(
                        tool="camunda_workflow_generator",
                        query=description,
                        response=bpmn_xml,
                        metadata={'process_id': process_id, 'process_name': process_name}
                    )
                    st.success("Saved to knowledge base!")
                except Exception as e:
                    st.error(f"Failed to save: {e}")
    
    with tab4:
        st.header("🔧 Workflow Optimization")
        
        st.markdown("Upload a BPMN file for AI-powered optimization suggestions")
        
        uploaded_file = st.file_uploader("Choose BPMN file", type=['bpmn', 'xml'])
        
        if uploaded_file:
            bpmn_content = uploaded_file.read().decode('utf-8')
            
            # Parse and display basic info
            try:
                root = ET.fromstring(bpmn_content)
                process = root.find('.//{http://www.omg.org/spec/BPMN/20100524/MODEL}process')
                
                if process:
                    st.info(f"Process ID: {process.get('id')}, Name: {process.get('name')}")
            except:
                st.warning("Could not parse BPMN structure")
            
            # Optimization button
            if st.button("🚀 Analyze & Optimize", type="primary"):
                with st.spinner("Analyzing workflow..."):
                    optimization = manager.optimize_workflow_with_ai(bpmn_content)
                
                st.markdown("### 💡 Optimization Suggestions")
                st.markdown(optimization['optimizations'])
                
                # Quick actions based on common patterns
                st.markdown("### ⚡ Quick Actions")
                col1, col2, col3 = st.columns(3)
                
                with col1:
                    if st.button("Add Error Boundaries"):
                        st.info("Would add error boundary events to all service tasks")
                
                with col2:
                    if st.button("Parallelize Tasks"):
                        st.info("Would identify and parallelize independent tasks")
                
                with col3:
                    if st.button("Add SLA Timers"):
                        st.info("Would add timer boundary events for SLA monitoring")
    
    with tab5:
        st.header("📈 Process Analytics")
        
        # Process selector
        process_defs = ["OrderProcess", "LeaveRequest", "PaymentProcess"]  # Would fetch from API
        selected_process = st.selectbox("Select Process Definition", process_defs)
        
        # Time range
        col1, col2 = st.columns(2)
        with col1:
            days_back = st.slider("Days to analyze", 1, 30, 7)
        with col2:
            sla_minutes = st.number_input("SLA (minutes)", value=60, min_value=1)
        
        if st.button("📊 Analyze", type="primary"):
            # SLA Compliance
            sla_data = manager.monitor_sla_compliance(selected_process, sla_minutes)
            
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Total Processes", sla_data['total'])
            with col2:
                st.metric("Compliance Rate", f"{sla_data['compliance_rate']:.1f}%")
            with col3:
                st.metric("Violations", sla_data['violations'])
            
            # Visualization
            if sla_data['violation_details']:
                fig = go.Figure(data=[
                    go.Bar(
                        y=[f"Instance {i+1}" for i in range(len(sla_data['violation_details']))],
                        x=[v['exceeded_by'] for v in sla_data['violation_details']],
                        orientation='h',
                        marker_color='crimson'
                    )
                ])
                fig.update_layout(
                    title="Top SLA Violations (minutes over limit)",
                    xaxis_title="Minutes Exceeded",
                    height=300
                )
                st.plotly_chart(fig, use_container_width=True)

else:
    st.info("👈 Please configure your Camunda connection to get started")

# Footer with tips
with st.expander("💡 Pro Tips"):
    st.markdown("""
    - **BPMN Generation**: Be specific about decision points and parallel activities
    - **Incident Analysis**: The AI considers common Camunda patterns and best practices
    - **Optimization**: Focus on one workflow at a time for best results
    - **Integration**: Use the generated insights to update your actual Camunda processes
    """)

# Add to navigation
# In utils/navigation.py, add:
# {
#     "title": "Camunda Manager",
#     "icon": "🏗️",
#     "description": "AI-powered Camunda workflow management",
#     "page": "camunda_manager",
#     "tags": ["workflow", "camunda", "bpmn", "process"]
# }