"""
Practical Examples for Broadcast Software Teams
Shows real-world usage without overengineering
"""

from toolkits.agent_hub import (
    quick_agent, quick_pipeline, AgentMemory, 
    BroadcastMemoryPatterns, AgentTemplates,
    AgentOrchestrator
)


def example_support_workflow():
    """Example: Support team handling streaming issue"""
    
    # Initialize memory
    memory = AgentMemory()
    
    # Customer reports issue
    customer_issue = """
    Customer: ESPN
    Issue: RTMP stream dropping every 10 minutes
    Error: Connection reset by peer
    """
    
    # 1. Check if we've seen this before
    similar = BroadcastMemoryPatterns.search_similar_issues(
        memory, "RTMP connection reset"
    )
    
    if similar:
        print("Found similar issues:")
        for issue in similar[:3]:
            print(f"  - {issue['value'].get('issue', 'N/A')}")
            print(f"    Fix: {issue['value'].get('fix', 'N/A')}")
    
    # 2. Create quick agent to analyze
    stream_analyzer = quick_agent(
        name="Stream Analyzer",
        tools=["analyze_rtmp", "check_network"],
        prompts={
            "analyze_rtmp": "Analyze RTMP connection issues: {error_log}",
            "check_network": "Check network stability for streaming: {symptoms}"
        }
    )
    
    # 3. Use orchestrator for complex analysis
    orchestrator = AgentOrchestrator()
    result = orchestrator.orchestrate(
        goal=f"Debug and fix: {customer_issue}",
        mode="sequential"
    )
    
    # 4. Remember the solution
    if result.get('success'):
        BroadcastMemoryPatterns.remember_customer_issue(
            memory,
            customer="ESPN",
            issue="RTMP stream dropping",
            resolution=result.get('results', [{}])[0].get('result', 'Unknown')
        )


def example_dev_workflow():
    """Example: Dev team using templates"""
    
    # Get the API debugger template
    template = AgentTemplates.get_template("broadcast_api_debugger")
    
    # Execute it
    from toolkits.agent_hub import PipelineExecutor
    executor = PipelineExecutor()
    
    result = executor.execute(
        pipeline_type="simple",
        steps=template['pipeline'],
        context={
            "error": "WebRTC negotiation failed",
            "endpoint": "/api/v1/stream/negotiate"
        }
    )
    
    # Remember the fix
    if result.get('success'):
        memory = AgentMemory()
        BroadcastMemoryPatterns.remember_api_pattern(
            memory,
            endpoint="/api/v1/stream/negotiate",
            error="WebRTC negotiation failed",
            fix=str(result.get('results', []))
        )


def example_quick_pipeline():
    """Example: Quick pipeline for common task"""
    
    # Sales team needs a quick demo
    demo_pipeline = quick_pipeline([
        {"do": "Generate code for live streaming dashboard", "with": "code_generator"},
        {"do": "Create SQL for viewer analytics", "with": "sql_generator"},
        {"do": "Write executive summary of features", "with": "doc_generator"}
    ], name="Quick Demo Builder")
    
    # Execute it
    executor = PipelineExecutor()
    result = executor.execute("simple", demo_pipeline['pipeline'])
    
    return result


def example_compliance_check():
    """Example: Legal team compliance workflow"""
    
    # Use the compliance template
    template = AgentTemplates.get_template("compliance_checker")
    
    # Add memory for tracking
    memory = AgentMemory()
    
    # Execute compliance check
    executor = PipelineExecutor()
    result = executor.execute(
        pipeline_type="simple",
        steps=template['pipeline'],
        context={
            "component": "user_data_handler",
            "regulations": ["GDPR", "CCPA"]
        }
    )
    
    # Remember the compliance status
    BroadcastMemoryPatterns.remember_compliance_check(
        memory,
        component="user_data_handler",
        regulation="GDPR",
        status="compliant" if result.get('success') else "needs_review",
        notes=str(result.get('results', []))[:200]
    )


if __name__ == "__main__":
    print("=== Broadcast Software Team Examples ===\n")
    
    print("1. Support Workflow Example:")
    example_support_workflow()
    
    print("\n2. Dev Workflow Example:")
    example_dev_workflow()
    
    print("\n3. Quick Pipeline Example:")
    result = example_quick_pipeline()
    print(f"Pipeline completed: {result.get('success')}")
    
    print("\n4. Compliance Check Example:")
    example_compliance_check()
