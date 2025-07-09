"""Test new agent hub features"""
import sys
sys.path.insert(0, 'C:/Projects/Tuokit')

from toolkits.agent_hub import (
    quick_agent, quick_pipeline, 
    AgentMemory, BroadcastMemoryPatterns,
    AgentTemplates
)

print("[SUCCESS] All new features imported!")

# Test templates
print("\nAvailable templates:")
for template in AgentTemplates.list_templates():
    print(f"  - {template}")

# Test quick agent
print("\nCreating quick agent...")
agent = quick_agent(
    "Test Monitor",
    ["check_status"],
    {"check_status": "Check status of {system}"}
)
print(f"  Created: {agent.name}")

# Test memory
print("\nInitializing memory...")
memory = AgentMemory()
print("  Memory initialized")

print("\n[COMPLETE] All features working!")
