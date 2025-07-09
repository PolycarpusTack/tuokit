"""Test agent management system"""
import sys
sys.path.insert(0, 'C:/Projects/Tuokit')

try:
    from toolkits.agent_hub import AgentManager, AgentConfig
    print("[SUCCESS] Agent management imports successful!")
    
    # Test manager creation
    manager = AgentManager()
    print("  - AgentManager initialized")
    
    # Test config creation
    config = AgentConfig(
        id="test.team.dev.test_agent_v1",
        name="Test Agent",
        category="team",
        subcategory="dev",
        tools=["test_tool"],
        prompts={"test_tool": "Test prompt"}
    )
    print("  - AgentConfig created")
    
    # Test listing agents (should work even if empty)
    agents = manager.list_agents()
    print(f"  - Found {len(agents)} existing agents")
    
    # Test template loading
    import json
    from pathlib import Path
    template_path = Path("agent_store/templates/stream_monitor.json")
    if template_path.exists():
        with open(template_path) as f:
            template = json.load(f)
        print(f"  - Template loaded: {template['name']}")
    
    print("\n[COMPLETE] Agent management system ready!")
    print("\nFeatures available:")
    print("  - Create custom agents organized by team/client/function")
    print("  - Use templates for quick creation")
    print("  - Track usage and analytics")
    print("  - Export/import for sharing")
    print("  - No directory or database clutter!")
    
except Exception as e:
    print(f"[ERROR] {e}")
    import traceback
    traceback.print_exc()
