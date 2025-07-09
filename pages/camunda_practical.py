"""
🏗️ Camunda Practical Toolkit - What Actually Works
Honest implementation following TuoKit philosophy
"""

import streamlit as st
import json
import xml.etree.ElementTree as ET
from datetime import datetime
import pandas as pd

# Import our practical manager
import sys
sys.path.append('..')
try:
    from toolkits.camunda.manager_v2 import (
        CamundaManagerV2,
        create_service_task,
        create_user_task,
        create_exclusive_gateway
    )
except ImportError:
    st.error("Cannot import Camunda manager. Please check the toolkit installation.")
    st.stop()

from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page config
st.set_page_config(
    page_title="Camunda Practical - TuoKit",
    page_icon="🏗️",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="camunda_practical")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🏗️ Camunda Practical Toolkit
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Real tools that actually work with Camunda Platform 7
    </p>
</div>
""", unsafe_allow_html=True)

# Initialize session state
if 'camunda_url' not in st.session_state:
    st.session_state.camunda_url = "http://localhost:8080/engine-rest"
if 'manager' not in st.session_state:
    st.session_state.manager = None
if 'connection_tested' not in st.session_state:
    st.session_state.connection_tested = False

# Connection Section
st.markdown("## 🔌 Connection")

col1, col2, col3 = st.columns([3, 1, 1])

with col1:
    url = st.text_input(
        "Camunda REST API URL",
        value=st.session_state.camunda_url,
        help="Usually http://localhost:8080/engine-rest"
    )

with col2:
    if st.button("Test Connection", type="primary"):
        manager = CamundaManagerV2(url)
        success, message = manager.test_connection()
        
        if success:
            st.success(message)
            st.session_state.manager = manager
            st.session_state.camunda_url = url
            st.session_state.connection_tested = True
        else:
            st.error(message)
            st.session_state.connection_tested = False

with col3:
    if st.session_state.connection_tested:
        st.success("✅ Connected")
    else:
        st.info("❓ Not tested")

# Main functionality - only show if connected
if st.session_state.manager and st.session_state.connection_tested:
    manager = st.session_state.manager
    
    # Tabs for different functions
    tab1, tab2, tab3, tab4, tab5 = st.tabs([
        "📋 Processes",
        "🚀 Deploy",
        "🎯 Start Process",
        "🚨 Incidents",
        "🤖 Generate BPMN"
    ])
    
    # Tab 1: View Processes
    with tab1:
        st.markdown("### Deployed Process Definitions")
        
        if st.button("🔄 Refresh", key="refresh_processes"):
            st.rerun()
        
        processes = manager.get_process_definitions()
        
        if processes:
            # Create a dataframe for display
            df_data = []
            for p in processes:
                df_data.append({
                    "ID": p.id,
                    "Name": p.name,
                    "Version": p.version,
                    "Active Instances": p.active_instances
                })
            
            df = pd.DataFrame(df_data)
            st.dataframe(df, use_container_width=True, hide_index=True)
            
            # Process selector for actions
            process_keys = [p.id.rsplit(':', 1)[0] for p in processes]
            selected_process = st.selectbox(
                "Select process for actions:",
                options=process_keys
            )
        else:
            st.info("No processes found. Deploy one using the Deploy tab.")
            if manager._last_error:
                st.error(f"Last error: {manager._last_error}")
    
    # Tab 2: Deploy BPMN
    with tab2:
        st.markdown("### Deploy BPMN Process")
        
        deployment_method = st.radio(
            "Deployment Method",
            ["Upload File", "Paste XML"],
            horizontal=True
        )
        
        bpmn_content = None
        filename = "process.bpmn"
        
        if deployment_method == "Upload File":
            uploaded_file = st.file_uploader(
                "Choose BPMN file",
                type=['bpmn', 'xml'],
                help="Select a valid BPMN 2.0 XML file"
            )
            
            if uploaded_file:
                bpmn_content = uploaded_file.read().decode('utf-8')
                filename = uploaded_file.name
                
                # Show preview
                with st.expander("Preview XML", expanded=False):
                    st.code(bpmn_content[:1000] + "..." if len(bpmn_content) > 1000 else bpmn_content, 
                           language='xml')
        
        else:  # Paste XML
            bpmn_content = st.text_area(
                "Paste BPMN XML here:",
                height=300,
                placeholder="""<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"...
"""
            )
        
        # Validate button
        col1, col2 = st.columns(2)
        
        with col1:
            if st.button("✓ Validate", disabled=not bpmn_content):
                is_valid, message = manager.validate_bpmn(bpmn_content)
                if is_valid:
                    st.success(message)
                else:
                    st.error(message)
        
        with col2:
            if st.button("🚀 Deploy", type="primary", disabled=not bpmn_content):
                success, message = manager.deploy_bpmn(filename, bpmn_content)
                if success:
                    st.success(message)
                    st.balloons()
                else:
                    st.error(message)
    
    # Tab 3: Start Process
    with tab3:
        st.markdown("### Start Process Instance")
        
        # Get available processes
        processes = manager.get_process_definitions()
        
        if processes:
            # Process selector
            process_keys = list(set(p.id.rsplit(':', 1)[0] for p in processes))
            selected_key = st.selectbox(
                "Select Process:",
                options=process_keys,
                help="Choose the process definition to start"
            )
            
            # Variables section
            st.markdown("#### Process Variables")
            
            # Variable builder
            col1, col2, col3 = st.columns([2, 2, 1])
            
            if 'variables' not in st.session_state:
                st.session_state.variables = {}
            
            with col1:
                var_name = st.text_input("Variable Name", key="var_name")
            
            with col2:
                var_value = st.text_input("Variable Value", key="var_value")
            
            with col3:
                st.markdown(" ")  # Spacer
                if st.button("➕ Add", disabled=not (var_name and var_value)):
                    st.session_state.variables[var_name] = var_value
                    st.rerun()
            
            # Show current variables
            if st.session_state.variables:
                st.markdown("**Current Variables:**")
                vars_df = pd.DataFrame(
                    list(st.session_state.variables.items()),
                    columns=["Name", "Value"]
                )
                st.dataframe(vars_df, use_container_width=True, hide_index=True)
                
                if st.button("🗑️ Clear All Variables"):
                    st.session_state.variables = {}
                    st.rerun()
            
            # Start button
            st.markdown("---")
            if st.button("▶️ Start Process Instance", type="primary"):
                success, message = manager.start_process(
                    selected_key,
                    st.session_state.variables
                )
                
                if success:
                    st.success(message)
                    # Clear variables after successful start
                    st.session_state.variables = {}
                else:
                    st.error(message)
        else:
            st.warning("No processes available. Deploy a process first.")
    
    # Tab 4: Incidents
    with tab4:
        st.markdown("### Current Incidents")
        
        if st.button("🔄 Refresh", key="refresh_incidents"):
            st.rerun()
        
        incidents = manager.get_incidents()
        
        if incidents:
            st.error(f"Found {len(incidents)} incident(s)")
            
            for incident in incidents:
                with st.expander(f"🚨 {incident.id} - {incident.activity}"):
                    col1, col2 = st.columns([3, 1])
                    
                    with col1:
                        st.markdown("**Details:**")
                        st.json({
                            "Incident ID": incident.id,
                            "Process Instance": incident.process_id,
                            "Activity": incident.activity,
                            "Error Message": incident.error_message,
                            "Timestamp": incident.timestamp
                        })
                    
                    with col2:
                        st.markdown("**Actions:**")
                        st.info("Open Camunda Cockpit to resolve incidents")
                        if st.button("📋 Copy ID", key=f"copy_{incident.id}"):
                            st.code(incident.process_id)
        else:
            st.success("✅ No incidents found!")
    
    # Tab 5: Generate BPMN
    with tab5:
        st.markdown("### AI-Assisted BPMN Generation")
        
        st.info("""
        Describe your process in simple terms. The AI will attempt to generate BPMN.
        If AI fails, you'll get a basic template to start with.
        """)
        
        # Process description
        description = st.text_area(
            "Describe your process:",
            height=150,
            placeholder="""Example: 
Order process where customer submits order, 
system checks inventory, 
if available then process payment, 
send confirmation email to customer"""
        )
        
        # Common patterns helper
        st.markdown("#### Quick Insert Patterns")
        
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("+ Service Task"):
                st.code(create_service_task("ServiceTask_1", "Call API", "api-topic"), language='xml')
        
        with col2:
            if st.button("+ User Task"):
                st.code(create_user_task("UserTask_1", "Review Document", "reviewer"), language='xml')
        
        with col3:
            if st.button("+ Gateway"):
                st.code(create_exclusive_gateway("Gateway_1", "Decision?"), language='xml')
        
        # Generate button
        if st.button("🤖 Generate BPMN", type="primary", disabled=not description):
            with st.spinner("Generating BPMN..."):
                generated_xml = manager.generate_simple_bpmn(description)
                
                # Validate the generated XML
                is_valid, validation_msg = manager.validate_bpmn(generated_xml)
                
                if is_valid:
                    st.success("✅ Generated valid BPMN!")
                else:
                    st.warning(f"Generated BPMN has issues: {validation_msg}")
                
                # Show the XML
                st.code(generated_xml, language='xml')
                
                # Download button
                st.download_button(
                    label="⬇️ Download BPMN",
                    data=generated_xml,
                    file_name="generated_process.bpmn",
                    mime="application/xml"
                )
                
                # Quick deploy option
                if is_valid:
                    if st.button("🚀 Deploy This Process"):
                        success, deploy_msg = manager.deploy_bpmn(
                            "generated_process.bpmn",
                            generated_xml
                        )
                        if success:
                            st.success(deploy_msg)
                        else:
                            st.error(deploy_msg)

else:
    # Not connected
    st.info("👆 Enter your Camunda URL and test the connection to get started")
    
    # Helpful information
    with st.expander("ℹ️ Setup Help"):
        st.markdown("""
        ### Quick Start Guide
        
        1. **Start Camunda Platform 7** locally:
           ```bash
           docker run -d -p 8080:8080 camunda/camunda-bpm-platform:latest
           ```
        
        2. **Default URL**: `http://localhost:8080/engine-rest`
        
        3. **Check if running**: Visit http://localhost:8080/camunda
        
        ### What This Tool Does:
        - ✅ View deployed processes
        - ✅ Deploy BPMN files
        - ✅ Start process instances with variables
        - ✅ Monitor incidents
        - ✅ Generate simple BPMN with AI assistance
        
        ### What It Doesn't Do (Yet):
        - ❌ Camunda 8 support (different architecture)
        - ❌ Complex process modifications
        - ❌ User task management
        - ❌ Process history analysis
        
        ### Tips:
        - Use Camunda Cockpit for visual monitoring
        - Keep your BPMN files simple and valid
        - Test processes thoroughly before production
        """)

# Footer
st.markdown("---")
st.markdown("""
<div style="text-align: center; color: #666;">
    <p>Honest tools that do what they promise - The TuoKit Way</p>
</div>
""", unsafe_allow_html=True)