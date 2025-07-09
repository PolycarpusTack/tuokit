"""
Test script to verify agent hub imports
"""

import sys
sys.path.insert(0, 'C:/Projects/Tuokit')

try:
    from toolkits.agent_hub import (
        AgentType, AgentState, BaseAgent,
        CodeAgent, SQLAgent, DocAgent, AnalysisAgent,
        AgentOrchestrator, PipelineExecutor,
        AGENT_REGISTRY, register_agent, get_agent,
        AgentHubUI
    )
    print("[SUCCESS] All imports successful!")
    
    # Test basic functionality
    print("\n[TEST] Creating agents...")
    code_agent = CodeAgent()
    print(f"  - CodeAgent created: {code_agent.name}")
    
    sql_agent = SQLAgent()
    print(f"  - SQLAgent created: {sql_agent.name}")
    
    print("\n[TEST] Agent Registry...")
    print(f"  - Registered agents: {list(AGENT_REGISTRY.keys())}")
    
    print("\n[TEST] Creating orchestrator...")
    orchestrator = AgentOrchestrator()
    print("  - Orchestrator created successfully")
    
    print("\n[TEST] Creating pipeline executor...")
    executor = PipelineExecutor()
    print("  - Pipeline executor created successfully")
    
    print("\n[COMPLETE] All tests passed!")
    
except Exception as e:
    print(f"[ERROR] Import failed: {e}")
    import traceback
    traceback.print_exc()
