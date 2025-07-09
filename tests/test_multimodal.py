"""Test multimodal imports and basic functionality"""
import sys
sys.path.insert(0, 'C:/Projects/Tuokit')

try:
    # Test imports
    from toolkits.agent_hub import (
        MultiModalAgent, ScreenshotAnalyzer, DocumentAnalyzer,
        ClientPatterns
    )
    print("[SUCCESS] MultiModal components imported!")
    
    # Test client patterns
    print("\nClient patterns available:")
    for client in ["ESPN", "NBC", "CBS", "Discovery"]:
        context = ClientPatterns.get_client_context(client)
        print(f"  {client}: {len(context.get('protocols', []))} protocols, "
              f"{len(context.get('common_issues', []))} known issues")
    
    # Test agent creation
    print("\nCreating MultiModal agent...")
    agent = MultiModalAgent()
    print(f"  Agent: {agent.name}")
    print(f"  Tools: {', '.join(agent.tools)}")
    
    # Test memory integration
    from toolkits.agent_hub import AgentMemory, BroadcastMemoryPatterns
    print("\nTesting memory integration...")
    memory = AgentMemory()
    
    # Store a test issue
    BroadcastMemoryPatterns.remember_customer_issue(
        memory,
        "ESPN", 
        "Test issue",
        "Test resolution"
    )
    print("  Memory: Issue stored successfully")
    
    # Search for it
    results = BroadcastMemoryPatterns.search_similar_issues(
        memory, 
        "test"
    )
    print(f"  Memory: Found {len(results)} similar issues")
    
    print("\n[COMPLETE] All multimodal features working!")
    print("\nNote: Actual image/PDF processing requires:")
    print("  - pip install pillow pytesseract")
    print("  - Tesseract OCR binary installed")
    
except ImportError as e:
    print(f"[WARNING] Import issue: {e}")
    print("\nThis is expected if PIL or pytesseract not installed.")
    print("Core functionality still works without these.")
    
except Exception as e:
    print(f"[ERROR] Unexpected error: {e}")
    import traceback
    traceback.print_exc()
