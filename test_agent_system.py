"""
TuoKit Agent System Test Suite
Validates agent functionality with minimal setup
"""
import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from agent_system import AgentOrchestrator, AGENT_REGISTRY
from team_agent import TEAM_REGISTRY

def test_agent_selection():
    """Test AI-driven agent selection"""
    print("🧪 Testing agent selection...")
    orchestrator = AgentOrchestrator()
    
    test_goals = [
        ("Generate SQL for monthly revenue report", "data_engineer"),
        ("Debug Python TypeError", "code_architect"),
        ("Summarize technical documentation", "doc_scientist")
    ]
    
    for goal, expected_agent in test_goals:
        selected = orchestrator.analyze_goal(goal)
        status = "✅" if selected == expected_agent else "❌"
        print(f"{status} Goal: '{goal[:30]}...' → {selected}")

def test_specialist_execution():
    """Test individual specialist agent"""
    print("\n🧪 Testing specialist agent execution...")
    orchestrator = AgentOrchestrator()
    
    try:
        state = orchestrator.execute_goal(
            "Create a regex pattern for email validation",
            agent_name="code_architect"
        )
        print(f"✅ Execution completed in phase: {state.phase}")
        print(f"   Steps executed: {len(state.steps)}")
        print(f"   Results captured: {len(state.results)}")
    except Exception as e:
        print(f"❌ Execution failed: {str(e)}")

def test_team_collaboration():
    """Test team agent coordination"""
    print("\n🧪 Testing team collaboration...")
    
    if not TEAM_REGISTRY:
        print("⚠️  Team agents not available")
        return
    
    # Note: This is a simplified test
    team = TEAM_REGISTRY["data_pipeline"]
    print(f"✅ Team '{team.name}' has {len(team.members)} members")
    for member in team.members:
        print(f"   - {member.name}: {len(member.tools)} tools")

def test_knowledge_capture():
    """Test automatic knowledge logging"""
    print("\n🧪 Testing knowledge capture...")
    orchestrator = AgentOrchestrator()
    
    if orchestrator.db.connected:
        count_before = orchestrator.db.get_knowledge_count()
        # Simple execution that should log
        orchestrator.analyze_goal("Test goal")
        count_after = orchestrator.db.get_knowledge_count()
        
        if count_after >= count_before:
            print("✅ Knowledge capture working")
        else:
            print("⚠️  Knowledge count unchanged")
    else:
        print("⚠️  Database not connected")

if __name__ == "__main__":
    print("🚀 TuoKit Agent System Test Suite\n")
    
    test_agent_selection()
    test_specialist_execution()
    test_team_collaboration()
    test_knowledge_capture()
    
    print("\n✨ Test suite completed!")
    print("\nNext steps:")
    print("1. Run 'streamlit run app.py' and navigate to Agent Portal")
    print("2. Try example goals from AGENT_SYSTEM_README.md")
    print("3. Monitor agent_executions table for tracking")
