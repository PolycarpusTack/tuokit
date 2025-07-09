"""
Test Script for Advanced Agent Toolkit
Quick tests for individual agents without Streamlit UI
"""

import json
from advanced_agent_toolkit import (
    SystemArchitectAgent,
    DeepResearchAgent,
    EnhancedCodeAgent,
    DataAnalysisAgent,
    DebuggingAgent,
    DocumentationAgent,
    MultiAgentOrchestrator,
    AgentContext,
    AgentRole
)

def test_system_architect():
    """Test the System Architect agent"""
    print("=" * 60)
    print("Testing System Architect Agent")
    print("=" * 60)
    
    architect = SystemArchitectAgent()
    context = AgentContext(goal="Build a real-time chat application")
    
    requirements = """
    Build a real-time chat application that needs to:
    - Support 10,000 concurrent users
    - Provide instant message delivery (< 100ms latency)
    - Include user authentication and authorization
    - Store message history
    - Support file uploads up to 10MB
    - Work across web and mobile platforms
    """
    
    result = architect.analyze_requirements(requirements, context)
    
    print("\n🧠 Reasoning:")
    print(result["reasoning"][:500] + "...")
    
    print("\n🏗️ Architecture Decision:")
    arch = result["architecture"]
    print(f"Pattern: {arch['pattern']}")
    print(f"Components: {len(arch.get('components', []))}")
    print(f"Database: {arch.get('database', {}).get('type', 'Unknown')}")
    
    print("\n✅ System Architect Test Complete")
    return result

def test_deep_research():
    """Test the Deep Research agent"""
    print("\n" + "=" * 60)
    print("Testing Deep Research Agent")
    print("=" * 60)
    
    researcher = DeepResearchAgent()
    context = AgentContext(goal="Research WebSocket implementations")
    
    result = researcher.research_topic(
        "Best practices for WebSocket implementation in Python",
        context,
        depth=2
    )
    
    print(f"\n📚 Research Topic: {result['topic']}")
    print(f"Findings: {len(result['findings'])}")
    
    for i, finding in enumerate(result['findings']):
        print(f"\n{i+1}. {finding['question']}")
        print(f"   Answer preview: {finding['answer'][:200]}...")
    
    print("\n✅ Deep Research Test Complete")
    return result

def test_code_generation():
    """Test the Enhanced Code agent"""
    print("\n" + "=" * 60)
    print("Testing Enhanced Code Agent")
    print("=" * 60)
    
    coder = EnhancedCodeAgent()
    context = AgentContext(goal="Implement WebSocket server")
    
    # Add some context from previous agents
    context.add_artifact("architecture_decision", {
        "architecture": {
            "pattern": "microservices",
            "components": [
                {"name": "WebSocket Server", "technology": "Python/FastAPI"}
            ]
        }
    })
    
    result = coder.generate_code(
        "Create a WebSocket server with authentication",
        context,
        language="python"
    )
    
    print(f"\n💻 Task: {result['task']}")
    print(f"Language: {result['language']}")
    print("\nGenerated Code Preview:")
    print(result['code'][:300] + "...")
    
    if 'analysis' in result:
        print(f"\nCode Analysis:")
        print(f"- Functions: {result['analysis']['metrics'].get('functions', 0)}")
        print(f"- Complexity: {result['analysis']['metrics'].get('cyclomatic_complexity', 0)}")
    
    print("\n✅ Code Generation Test Complete")
    return result

def test_data_analysis():
    """Test the Data Analysis agent"""
    print("\n" + "=" * 60)
    print("Testing Data Analysis Agent")
    print("=" * 60)
    
    analyst = DataAnalysisAgent()
    context = AgentContext(goal="Analyze user activity")
    
    result = analyst.analyze_data_request(
        "Show me the top 10 most active users by message count in the last 30 days",
        context
    )
    
    print(f"\n📊 Request: {result['request']}")
    print(f"\nSQL Query:")
    print(result['sql_query'])
    print(f"\nVisualization Suggestion:")
    print(result['visualization'][:200] + "...")
    
    print("\n✅ Data Analysis Test Complete")
    return result

def test_debugging():
    """Test the Debugging agent"""
    print("\n" + "=" * 60)
    print("Testing Debugging Agent")
    print("=" * 60)
    
    debugger = DebuggingAgent()
    
    error = "TypeError: unsupported operand type(s) for +: 'int' and 'str'"
    code_context = """
def calculate_total(price, tax_rate):
    total = price + tax_rate
    return total

result = calculate_total(100, "0.08")
"""
    
    result = debugger.analyze_error(error, code_context)
    
    print(f"\n🐛 Error: {result['error']}")
    print(f"Type: {result['error_type']}")
    print("\nAnalysis Preview:")
    print(result['analysis'][:300] + "...")
    
    if result['fixed_code']:
        print("\nFixed Code:")
        print(result['fixed_code'][:200] + "...")
    
    print("\n✅ Debugging Test Complete")
    return result

def test_multi_agent_orchestration():
    """Test the Multi-Agent Orchestrator"""
    print("\n" + "=" * 60)
    print("Testing Multi-Agent Orchestration")
    print("=" * 60)
    
    orchestrator = MultiAgentOrchestrator()
    
    goal = "Build a simple REST API for user management with authentication"
    
    print(f"\n🎯 Goal: {goal}")
    print("\nExecuting with automatic agent selection...")
    
    result = orchestrator.execute_complex_goal(goal)
    
    print("\n📊 Execution Summary:")
    for execution in result["results"]["executions"]:
        status = "❌ Error" if "error" in execution else "✅ Success"
        print(f"- {execution['agent']}: {status}")
    
    print(f"\n📦 Artifacts Created: {len(result['artifacts'])}")
    for name in result['artifacts'].keys():
        print(f"  - {name}")
    
    print("\n✅ Multi-Agent Orchestration Test Complete")
    return result

def test_documentation_generation():
    """Test documentation generation from context"""
    print("\n" + "=" * 60)
    print("Testing Documentation Agent")
    print("=" * 60)
    
    # Create a context with some artifacts
    context = AgentContext(goal="Build a chat application")
    
    # Add fake artifacts
    context.add_artifact("architecture_decision", {
        "architecture": {
            "pattern": "microservices",
            "components": [
                {"name": "API Gateway", "responsibility": "Route requests"},
                {"name": "Chat Service", "responsibility": "Handle messages"}
            ]
        }
    })
    
    context.add_artifact("code_chat_service", {
        "task": "Implement chat service",
        "code": "class ChatService:\n    pass",
        "analysis": {"metrics": {"functions": 5, "classes": 2}}
    })
    
    documenter = DocumentationAgent()
    result = documenter.generate_documentation(context, "technical")
    
    print(f"\n📚 Documentation Type: {result['type']}")
    print(f"Artifacts Used: {len(result['artifacts_used'])}")
    print("\nContent Preview:")
    print(result['content'][:400] + "...")
    
    print("\n✅ Documentation Test Complete")
    return result

def run_quick_demo():
    """Run a quick demonstration of key features"""
    print("\n" + "🚀" * 30)
    print("\nTuoKit Advanced Agent Toolkit - Quick Demo")
    print("\n" + "🚀" * 30)
    
    # Create orchestrator
    orchestrator = MultiAgentOrchestrator()
    
    # Simple goal
    goal = "Create a Python function to validate email addresses with tests"
    
    print(f"\n🎯 Demo Goal: {goal}")
    print("\nOrchestrating agents...")
    
    # Execute with specific agents
    result = orchestrator.execute_complex_goal(
        goal,
        selected_agents=[AgentRole.CODER, AgentRole.DOCUMENTER]
    )
    
    print("\n✨ Results:")
    
    # Show generated code
    for name, artifact in result['artifacts'].items():
        if 'code' in name and isinstance(artifact, dict) and 'code' in artifact:
            print(f"\n💻 Generated Code ({name}):")
            print("-" * 40)
            print(artifact['code'])
            print("-" * 40)
    
    # Show documentation
    for name, artifact in result['artifacts'].items():
        if 'documentation' in name and isinstance(artifact, dict):
            print(f"\n📚 {artifact.get('type', 'Unknown').title()} Documentation Preview:")
            print("-" * 40)
            print(artifact.get('content', '')[:500] + "...")
            print("-" * 40)
    
    print("\n🎉 Demo Complete!")

def main():
    """Run all tests"""
    import sys
    
    if len(sys.argv) > 1:
        test_name = sys.argv[1]
        
        tests = {
            "architect": test_system_architect,
            "research": test_deep_research,
            "code": test_code_generation,
            "data": test_data_analysis,
            "debug": test_debugging,
            "orchestrate": test_multi_agent_orchestration,
            "docs": test_documentation_generation,
            "demo": run_quick_demo
        }
        
        if test_name in tests:
            tests[test_name]()
        else:
            print(f"Unknown test: {test_name}")
            print(f"Available tests: {', '.join(tests.keys())}")
    else:
        # Run demo by default
        run_quick_demo()

if __name__ == "__main__":
    main()
