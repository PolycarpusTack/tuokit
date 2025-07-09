"""
Agent Management Examples
Shows how to create, organize, and manage agents properly
"""

from toolkits.agent_hub import AgentManager, AgentConfig


def example_create_espn_monitor():
    """Example: Create ESPN-specific monitoring agent"""
    
    manager = AgentManager()
    
    # Define agent configuration
    config = AgentConfig(
        id="custom.client.espn.stream_monitor_v1",
        name="ESPN Stream Monitor",
        category="client",
        subcategory="espn",
        tools=[
            "check_rtmp_status",
            "monitor_mnf_quality",
            "check_scoreboard",
            "alert_ops_team"
        ],
        prompts={
            "check_rtmp_status": "Check RTMP status for ESPN stream {stream_id}. Look for connection state and retry count.",
            "monitor_mnf_quality": "Monitor Monday Night Football stream quality. Check bitrate >= {min_bitrate} and latency < {max_latency}ms",
            "check_scoreboard": "Verify ESPN scoreboard overlay is updating. Last update should be < 5 seconds ago.",
            "alert_ops_team": "Alert ESPN ops team about {issue_type}. Severity: {severity}. Stream: {stream_id}"
        },
        metadata={
            "description": "Specialized monitor for ESPN streams with MNF focus",
            "author": "john.doe@broadcast.com",
            "tags": ["espn", "monitoring", "rtmp", "mnf"],
            "client_config": {
                "primary_protocol": "RTMP",
                "backup_protocol": "HLS",
                "check_interval": "1min",
                "alert_threshold": 3,
                "contacts": ["espn-ops@broadcast.com"]
            }
        },
        permissions={
            "teams": ["ops", "support"],
            "users": ["*"]
        }
    )
    
    # Create the agent
    try:
        agent_id = manager.create_agent(config)
        print(f"✅ Created ESPN monitor: {agent_id}")
        
        # The agent is now:
        # 1. Saved to: toolkits/agent_hub/custom/by_client/espn/espn_stream_monitor.py
        # 2. Registered in the database
        # 3. Available for immediate use
        
    except Exception as e:
        print(f"❌ Failed to create agent: {e}")


def example_create_from_template():
    """Example: Create agent from template"""
    
    manager = AgentManager()
    
    # Load the stream monitor template
    import json
    with open("agent_store/templates/stream_monitor.json") as f:
        template = json.load(f)
    
    # Customize for NBC
    config = AgentConfig.from_dict(template)
    config.name = "NBC Affiliate Monitor"
    config.subcategory = "nbc"
    config.id = manager._generate_agent_id(config.name, config.category, config.subcategory)
    
    # NBC-specific modifications
    config.tools.append("check_affiliate_sync")
    config.prompts["check_affiliate_sync"] = "Verify all NBC affiliates are in sync. Max drift: {max_drift_seconds}s"
    
    config.metadata.update({
        "client_config": {
            "affiliate_count": 200,
            "primary_protocol": "HLS",
            "peacock_integration": True
        }
    })
    
    # Create
    agent_id = manager.create_agent(config)
    print(f"✅ Created NBC monitor from template: {agent_id}")


def example_browse_agents():
    """Example: Browse and filter agents"""
    
    manager = AgentManager()
    
    # Get all client agents
    print("\n=== Client-Specific Agents ===")
    client_agents = manager.list_agents(category="client")
    
    for agent in client_agents:
        print(f"• {agent['name']} ({agent['subcategory']}) - v{agent['version']}")
        if agent.get('metadata', {}).get('tags'):
            print(f"  Tags: {', '.join(agent['metadata']['tags'])}")
    
    # Get support team agents
    print("\n=== Support Team Agents ===")
    support_agents = manager.list_agents(category="team", subcategory="support")
    
    for agent in support_agents:
        print(f"• {agent['name']} - {agent['metadata'].get('description', 'No description')}")


def example_usage_tracking():
    """Example: Track agent usage"""
    
    manager = AgentManager()
    
    # Simulate agent execution
    agent_id = "custom.client.espn.stream_monitor_v1"
    
    # Log successful execution
    manager.log_usage(
        agent_id=agent_id,
        user="support@broadcast.com",
        success=True,
        execution_time=1.23,
        context={
            "stream": "ESPN-MNF-2024",
            "action": "check_quality",
            "result": "bitrate_normal"
        }
    )
    
    # Get usage stats
    stats = manager.get_usage_stats(agent_id, days_back=30)
    print(f"\n=== Usage Stats for {agent_id} ===")
    print(f"Total uses: {stats['total_uses']}")
    print(f"Success rate: {stats['success_rate']:.1f}%")
    print(f"Avg execution time: {stats['avg_execution_time']:.2f}s")


def example_export_import():
    """Example: Export and share agents"""
    
    manager = AgentManager()
    
    # Export ESPN monitor with history
    agent_id = "custom.client.espn.stream_monitor_v1"
    export_path = manager.export_agent(agent_id, include_history=True)
    print(f"✅ Exported to: {export_path}")
    
    # This creates a zip file containing:
    # - agent.json (configuration + metadata)
    # - code/espn_stream_monitor.py (if exists)
    # - usage statistics and example runs
    
    # Import to another system
    imported_id = manager.import_agent(export_path, rename="ESPN Monitor v2")
    print(f"✅ Imported as: {imported_id}")


def example_agent_lifecycle():
    """Example: Complete agent lifecycle"""
    
    manager = AgentManager()
    
    # 1. Create v1
    config = AgentConfig(
        id="custom.team.support.ticket_helper_v1",
        name="Ticket Helper",
        category="team",
        subcategory="support",
        tools=["analyze_ticket", "suggest_solution"],
        prompts={
            "analyze_ticket": "Analyze support ticket: {ticket_text}",
            "suggest_solution": "Suggest solution for: {issue_summary}"
        }
    )
    
    agent_id = manager.create_agent(config)
    print(f"✅ Created v1: {agent_id}")
    
    # 2. Update with new tool
    new_version = manager.update_agent(agent_id, {
        "tools": config.tools + ["check_sla"],
        "prompts": {
            **config.prompts,
            "check_sla": "Check SLA compliance for {client}"
        }
    })
    print(f"✅ Updated to v{new_version}")
    
    # 3. Clone for another team
    sales_version = manager.clone_agent(
        agent_id,
        "Sales Ticket Helper",
        modifications={
            "subcategory": "sales",
            "metadata": {"focus": "pre-sales questions"}
        }
    )
    print(f"✅ Cloned for sales: {sales_version}")
    
    # 4. Eventually deprecate old version
    manager.deprecate_agent(
        agent_id,
        reason="Replaced by TicketHelper v2 with better error handling"
    )
    print(f"✅ Deprecated old version")


if __name__ == "__main__":
    print("=== Agent Management Examples ===\n")
    
    # Run examples
    example_create_espn_monitor()
    example_create_from_template()
    example_browse_agents()
    example_usage_tracking()
    example_export_import()
    example_agent_lifecycle()
    
    print("\n✅ Examples complete!")
    print("\nKey Takeaways:")
    print("1. Agents are organized by team/client/function")
    print("2. Templates speed up creation")
    print("3. Usage tracking helps identify popular agents")
    print("4. Export/import enables sharing")
    print("5. Proper lifecycle management prevents clutter")
