"""
Agent Builder - Interactive agent creation workflow
"""

import streamlit as st
from typing import Dict, List, Optional
import json
from datetime import datetime

from .agent_manager import AgentManager, AgentConfig
from .registry import list_agents


class AgentBuilder:
    """Interactive agent creation and management UI"""
    
    def __init__(self):
        self.manager = AgentManager()
    
    def render(self):
        """Main UI for agent builder"""
        st.header("🤖 Agent Builder")
        
        tab1, tab2, tab3, tab4, tab5 = st.tabs([
            "Create New",
            "Browse Agents",
            "Templates",
            "Import/Export",
            "Usage Stats"
        ])
        
        with tab1:
            self._render_create_agent()
        
        with tab2:
            self._render_browse_agents()
        
        with tab3:
            self._render_templates()
        
        with tab4:
            self._render_import_export()
        
        with tab5:
            self._render_usage_stats()
    
    def _render_create_agent(self):
        """Create new agent interface"""
        st.subheader("Create New Agent")
        
        # Creation method
        method = st.radio(
            "Creation Method",
            ["Quick Builder", "From Template", "Clone Existing"],
            horizontal=True
        )
        
        if method == "Quick Builder":
            self._render_quick_builder()
        elif method == "From Template":
            self._render_from_template()
        else:
            self._render_clone_agent()
    
    def _render_quick_builder(self):
        """Quick agent creation form"""
        col1, col2 = st.columns(2)
        
        with col1:
            # Basic info
            name = st.text_input("Agent Name", placeholder="ESPN Stream Monitor")
            
            category = st.selectbox(
                "Category",
                ["team", "client", "function"],
                format_func=lambda x: x.capitalize()
            )
            
            if category == "team":
                subcategory = st.selectbox(
                    "Team",
                    ["support", "legal", "dev", "sales", "ops"]
                )
            elif category == "client":
                subcategory = st.selectbox(
                    "Client",
                    ["espn", "nbc", "cbs", "discovery", "other"]
                )
            else:
                subcategory = st.text_input(
                    "Function Type",
                    placeholder="monitoring, analysis, automation"
                )
        
        with col2:
            # Description
            description = st.text_area(
                "Description",
                placeholder="What does this agent do?",
                height=100
            )
            
            # Tags
            tags = st.text_input(
                "Tags (comma-separated)",
                placeholder="streaming, rtmp, monitoring"
            )
        
        # Tools selection
        st.subheader("Tools & Capabilities")
        
        available_tools = [
            "analyze_error", "check_stream", "monitor_quality",
            "generate_report", "check_compliance", "analyze_document",
            "send_alert", "query_database", "call_api"
        ]
        
        selected_tools = st.multiselect(
            "Select Tools",
            available_tools,
            help="Choose the capabilities this agent needs"
        )
        
        # Prompts for tools
        if selected_tools:
            st.subheader("Tool Prompts")
            prompts = {}
            
            for tool in selected_tools:
                prompt = st.text_area(
                    f"Prompt for {tool}",
                    placeholder=f"Template for {tool} - use {{variables}} for parameters",
                    height=80,
                    key=f"prompt_{tool}"
                )
                if prompt:
                    prompts[tool] = prompt
        
        # Client-specific config
        if category == "client" and subcategory:
            st.subheader("Client Configuration")
            
            col1, col2 = st.columns(2)
            with col1:
                protocols = st.multiselect(
                    "Protocols",
                    ["RTMP", "HLS", "DASH", "WebRTC", "SRT"]
                )
            
            with col2:
                check_interval = st.selectbox(
                    "Check Interval",
                    ["1min", "5min", "10min", "30min", "1hour"]
                )
        
        # Create button
        if st.button("🚀 Create Agent", type="primary", use_container_width=True):
            if name and category:
                try:
                    # Build config
                    config = AgentConfig(
                        id=self.manager._generate_agent_id(name, category, subcategory),
                        name=name,
                        category=category,
                        subcategory=subcategory,
                        tools=selected_tools,
                        prompts=prompts,
                        metadata={
                            "description": description,
                            "tags": [t.strip() for t in tags.split(",")] if tags else [],
                            "author": st.session_state.get("user", "unknown"),
                            "created_at": datetime.now().isoformat()
                        }
                    )
                    
                    # Add client config if applicable
                    if category == "client" and subcategory:
                        config.metadata["client_config"] = {
                            "protocols": protocols,
                            "check_interval": check_interval
                        }
                    
                    # Create agent
                    agent_id = self.manager.create_agent(config)
                    st.success(f"✅ Agent created successfully! ID: {agent_id}")
                    
                    # Show next steps
                    st.info("Agent is now available in the registry. You can start using it immediately!")
                    
                except Exception as e:
                    st.error(f"Failed to create agent: {str(e)}")
            else:
                st.warning("Please fill in all required fields")
    
    def _render_from_template(self):
        """Create agent from template"""
        st.info("📋 Start from a pre-configured template")
        
        # List available templates
        template_dir = self.manager.agent_store_path / "templates"
        template_dir.mkdir(exist_ok=True)
        
        templates = list(template_dir.glob("*.json"))
        
        if not templates:
            st.warning("No templates available yet. Create one from an existing agent!")
            return
        
        template_name = st.selectbox(
            "Select Template",
            [t.stem for t in templates],
            format_func=lambda x: x.replace("_", " ").title()
        )
        
        if template_name:
            # Load template
            template_path = template_dir / f"{template_name}.json"
            with open(template_path) as f:
                template = json.load(f)
            
            # Show template info
            with st.expander("Template Details", expanded=True):
                st.json(template)
            
            # Customization
            st.subheader("Customize Template")
            
            new_name = st.text_input(
                "Agent Name",
                value=template.get("name", "") + " (Custom)"
            )
            
            # Allow tool selection modification
            current_tools = template.get("tools", [])
            selected_tools = st.multiselect(
                "Tools",
                current_tools + ["custom_tool"],
                default=current_tools
            )
            
            if st.button("Create from Template", type="primary"):
                try:
                    config = AgentConfig.from_dict(template)
                    config.name = new_name
                    config.id = self.manager._generate_agent_id(
                        new_name, config.category, config.subcategory
                    )
                    config.tools = selected_tools
                    config.metadata["from_template"] = template_name
                    
                    agent_id = self.manager.create_agent(config)
                    st.success(f"✅ Agent created from template! ID: {agent_id}")
                    
                except Exception as e:
                    st.error(f"Failed to create agent: {str(e)}")
    
    def _render_clone_agent(self):
        """Clone existing agent"""
        st.info("🔄 Clone and modify an existing agent")
        
        # List agents
        agents = self.manager.list_agents()
        
        if not agents:
            st.warning("No agents available to clone")
            return
        
        # Agent selection
        agent_options = {
            f"{a['name']} ({a['category']}/{a['subcategory'] or 'general'})": a['agent_id']
            for a in agents
        }
        
        selected = st.selectbox(
            "Select Agent to Clone",
            list(agent_options.keys())
        )
        
        if selected:
            source_id = agent_options[selected]
            source_agent = self.manager.get_agent(source_id)
            
            # Show source details
            with st.expander("Source Agent Details"):
                st.json(source_agent.to_dict())
            
            # Customization
            st.subheader("Customize Clone")
            
            new_name = st.text_input(
                "New Agent Name",
                value=source_agent.name + " (Clone)"
            )
            
            # Modifications
            modify_tools = st.checkbox("Modify tools")
            if modify_tools:
                selected_tools = st.multiselect(
                    "Tools",
                    source_agent.tools + ["new_tool"],
                    default=source_agent.tools
                )
            else:
                selected_tools = source_agent.tools
            
            if st.button("Clone Agent", type="primary"):
                try:
                    modifications = {}
                    if modify_tools:
                        modifications["tools"] = selected_tools
                    
                    agent_id = self.manager.clone_agent(
                        source_id, new_name, modifications
                    )
                    st.success(f"✅ Agent cloned successfully! ID: {agent_id}")
                    
                except Exception as e:
                    st.error(f"Failed to clone agent: {str(e)}")
    
    def _render_browse_agents(self):
        """Browse and manage existing agents"""
        st.subheader("Browse Agents")
        
        # Filters
        col1, col2, col3 = st.columns(3)
        
        with col1:
            filter_category = st.selectbox(
                "Category",
                ["All", "team", "client", "function"],
                format_func=lambda x: x.capitalize()
            )
        
        with col2:
            filter_subcategory = st.text_input(
                "Subcategory",
                placeholder="e.g., espn, support"
            )
        
        with col3:
            show_inactive = st.checkbox("Show inactive agents")
        
        # Get agents
        agents = self.manager.list_agents(
            category=filter_category if filter_category != "All" else None,
            subcategory=filter_subcategory if filter_subcategory else None,
            active_only=not show_inactive
        )
        
        # Display agents
        if agents:
            for agent in agents:
                with st.expander(
                    f"{agent['name']} - {agent['category']}/{agent['subcategory'] or 'general'}"
                ):
                    col1, col2 = st.columns([3, 1])
                    
                    with col1:
                        st.write(f"**ID:** {agent['agent_id']}")
                        st.write(f"**Version:** {agent['version']}")
                        st.write(f"**Created:** {agent['created_at']}")
                        
                        if agent.get('metadata'):
                            meta = agent['metadata']
                            if meta.get('description'):
                                st.write(f"**Description:** {meta['description']}")
                            if meta.get('tags'):
                                st.write(f"**Tags:** {', '.join(meta['tags'])}")
                    
                    with col2:
                        if st.button("Edit", key=f"edit_{agent['agent_id']}"):
                            st.session_state.editing_agent = agent['agent_id']
                        
                        if st.button("Export", key=f"export_{agent['agent_id']}"):
                            try:
                                export_path = self.manager.export_agent(
                                    agent['agent_id'],
                                    include_history=True
                                )
                                st.success(f"Exported to: {export_path}")
                            except Exception as e:
                                st.error(f"Export failed: {str(e)}")
                        
                        if agent['is_active']:
                            if st.button("Deprecate", key=f"dep_{agent['agent_id']}"):
                                reason = st.text_input(
                                    "Deprecation reason",
                                    key=f"reason_{agent['agent_id']}"
                                )
                                if reason:
                                    self.manager.deprecate_agent(
                                        agent['agent_id'],
                                        reason
                                    )
                                    st.success("Agent deprecated")
                                    st.rerun()
        else:
            st.info("No agents found matching the filters")
    
    def _render_templates(self):
        """Manage agent templates"""
        st.subheader("Agent Templates")
        
        # Create template from existing agent
        st.write("### Create Template")
        
        agents = self.manager.list_agents()
        if agents:
            agent_options = {
                f"{a['name']} ({a['version']})": a['agent_id']
                for a in agents
            }
            
            selected = st.selectbox(
                "Create template from agent",
                list(agent_options.keys())
            )
            
            template_name = st.text_input(
                "Template Name",
                placeholder="client_monitor, issue_resolver"
            )
            
            if st.button("Create Template"):
                if selected and template_name:
                    try:
                        agent_id = agent_options[selected]
                        agent = self.manager.get_agent(agent_id)
                        
                        # Create template
                        template = agent.to_dict()
                        template['template_name'] = template_name
                        template['template_created'] = datetime.now().isoformat()
                        
                        # Save template
                        template_path = self.manager.agent_store_path / "templates" / f"{template_name}.json"
                        template_path.parent.mkdir(exist_ok=True)
                        
                        with open(template_path, 'w') as f:
                            json.dump(template, f, indent=2)
                        
                        st.success(f"✅ Template created: {template_name}")
                        
                    except Exception as e:
                        st.error(f"Failed to create template: {str(e)}")
        
        # List existing templates
        st.write("### Existing Templates")
        
        template_dir = self.manager.agent_store_path / "templates"
        templates = list(template_dir.glob("*.json")) if template_dir.exists() else []
        
        if templates:
            for template_file in templates:
                with st.expander(template_file.stem):
                    with open(template_file) as f:
                        template = json.load(f)
                    
                    st.json(template)
                    
                    if st.button(f"Delete {template_file.stem}", key=f"del_tmpl_{template_file.stem}"):
                        template_file.unlink()
                        st.success("Template deleted")
                        st.rerun()
        else:
            st.info("No templates created yet")
    
    def _render_import_export(self):
        """Import/Export agents"""
        st.subheader("Import/Export Agents")
        
        tab1, tab2 = st.tabs(["Export", "Import"])
        
        with tab1:
            st.write("### Export Agent")
            
            agents = self.manager.list_agents()
            if agents:
                agent_options = {
                    f"{a['name']} ({a['version']})": a['agent_id']
                    for a in agents
                }
                
                selected = st.selectbox(
                    "Select agent to export",
                    list(agent_options.keys())
                )
                
                include_history = st.checkbox(
                    "Include usage history",
                    value=True,
                    help="Includes statistics and example runs"
                )
                
                if st.button("Export Agent", type="primary"):
                    try:
                        agent_id = agent_options[selected]
                        export_path = self.manager.export_agent(
                            agent_id,
                            include_history=include_history
                        )
                        st.success(f"✅ Exported to: {export_path}")
                        
                        # Offer download
                        with open(export_path, 'rb') as f:
                            st.download_button(
                                "Download Export",
                                f.read(),
                                file_name=Path(export_path).name,
                                mime="application/zip"
                            )
                    except Exception as e:
                        st.error(f"Export failed: {str(e)}")
        
        with tab2:
            st.write("### Import Agent")
            
            uploaded_file = st.file_uploader(
                "Upload agent package",
                type=['zip'],
                help="Upload an exported agent package"
            )
            
            if uploaded_file:
                # Save temp
                temp_path = f"temp_{uploaded_file.name}"
                with open(temp_path, 'wb') as f:
                    f.write(uploaded_file.getbuffer())
                
                # Preview
                try:
                    import zipfile
                    with zipfile.ZipFile(temp_path, 'r') as zf:
                        package = json.loads(zf.read("agent.json"))
                    
                    st.write("### Package Contents")
                    agent_info = package['agent']
                    st.write(f"**Name:** {agent_info['name']}")
                    st.write(f"**Version:** {agent_info['version']}")
                    st.write(f"**Category:** {agent_info['category']}/{agent_info.get('subcategory', 'general')}")
                    
                    # Import options
                    rename = st.text_input(
                        "Rename agent (optional)",
                        placeholder="Leave empty to keep original name"
                    )
                    
                    if st.button("Import Agent", type="primary"):
                        try:
                            agent_id = self.manager.import_agent(temp_path, rename)
                            st.success(f"✅ Agent imported successfully! ID: {agent_id}")
                        except Exception as e:
                            st.error(f"Import failed: {str(e)}")
                    
                except Exception as e:
                    st.error(f"Invalid package: {str(e)}")
                finally:
                    # Cleanup
                    import os
                    if os.path.exists(temp_path):
                        os.remove(temp_path)
    
    def _render_usage_stats(self):
        """Display agent usage statistics"""
        st.subheader("Usage Statistics")
        
        # Time range
        days_back = st.selectbox(
            "Time Range",
            [7, 30, 90],
            format_func=lambda x: f"Last {x} days"
        )
        
        # Overall stats
        overall_stats = self.manager.get_usage_stats(days_back=days_back)
        
        if overall_stats.get("popular_agents"):
            st.write("### Most Used Agents")
            
            import pandas as pd
            df = pd.DataFrame(overall_stats["popular_agents"])
            
            if not df.empty:
                # Add agent names
                agent_names = []
                for agent_id in df['agent_id']:
                    agent = self.manager.get_agent(agent_id)
                    agent_names.append(agent.name if agent else agent_id)
                
                df['name'] = agent_names
                
                # Display chart
                st.bar_chart(df.set_index('name')['uses'])
        
        # Individual agent stats
        st.write("### Agent Details")
        
        agents = self.manager.list_agents()
        if agents:
            selected_agent = st.selectbox(
                "Select Agent",
                [a['agent_id'] for a in agents],
                format_func=lambda x: next(
                    (a['name'] for a in agents if a['agent_id'] == x),
                    x
                )
            )
            
            if selected_agent:
                stats = self.manager.get_usage_stats(
                    agent_id=selected_agent,
                    days_back=days_back
                )
                
                col1, col2, col3 = st.columns(3)
                
                with col1:
                    st.metric("Total Uses", stats['total_uses'])
                    st.metric("Unique Users", stats['unique_users'])
                
                with col2:
                    st.metric(
                        "Success Rate",
                        f"{stats['success_rate']:.1f}%" if stats['success_rate'] else "N/A"
                    )
                    st.metric(
                        "Avg Time",
                        f"{stats['avg_execution_time']:.2f}s" if stats['avg_execution_time'] else "N/A"
                    )
                
                with col3:
                    st.metric(
                        "Min Time",
                        f"{stats['min_execution_time']:.2f}s" if stats['min_execution_time'] else "N/A"
                    )
                    st.metric(
                        "Max Time",
                        f"{stats['max_execution_time']:.2f}s" if stats['max_execution_time'] else "N/A"
                    )
