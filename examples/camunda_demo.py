"""
Demo script for Camunda Toolkit
"""

from toolkits.camunda import CamundaToolkit
import json


def demo_monitoring():
    """Demo: Monitor running processes"""
    print("=== CAMUNDA MONITORING DEMO ===\n")
    
    # Initialize toolkit
    toolkit = CamundaToolkit(
        base_url="http://localhost:8080/engine-rest",
        username="demo",
        password="demo"
    )
    
    # Check connection
    if not toolkit.client.health_check():
        print("❌ Cannot connect to Camunda. Please check your configuration.")
        return
    
    print("✅ Connected to Camunda\n")
    
    # Get dashboard data
    dashboard = toolkit.monitor.get_dashboard_data()
    
    print(f"📊 Dashboard Summary:")
    print(f"  - Running Processes: {dashboard['total_running']}")
    print(f"  - Active Incidents: {dashboard['total_incidents']}")
    print(f"  - Pending Tasks: {dashboard['pending_tasks']}")
    print(f"  - Health Score: {dashboard['health_score']}%")
    
    # Find stuck processes
    print("\n🚧 Checking for stuck processes...")
    stuck = toolkit.monitor.find_stuck_processes()
    
    if stuck:
        print(f"Found {len(stuck)} stuck processes:")
        for s in stuck:
            print(f"  - Process {s['process']['id']} stuck at {s['stuck_activity']['activityId']}")
            print(f"    Duration: {s['duration']}")
            print(f"    AI Recommendation: {s['recommendation']}")
    else:
        print("No stuck processes found!")


def demo_generation():
    """Demo: Generate BPMN from text"""
    print("\n=== BPMN GENERATION DEMO ===\n")
    
    toolkit = CamundaToolkit("http://localhost:8080/engine-rest")
    
    # Example process description
    description = """
    When a purchase order is received, first validate the order details.
    If the order value is over $10,000, it requires manager approval.
    Once approved, check inventory availability.
    If items are in stock, reserve inventory and process payment.
    After successful payment, generate shipping label and notify warehouse.
    If payment fails, cancel the order and notify the customer.
    """
    
    print("📝 Process Description:")
    print(description)
    
    print("\n🤖 Generating BPMN...")
    bpmn_xml = toolkit.generator.from_text(description, "Purchase Order Process")
    
    print("\n✅ Generated BPMN XML:")
    print(bpmn_xml[:500] + "..." if len(bpmn_xml) > 500 else bpmn_xml)
    
    # Save to file
    with open("generated_process.bpmn", "w") as f:
        f.write(bpmn_xml)
    print("\n💾 Saved to generated_process.bpmn")


def demo_analysis():
    """Demo: Analyze existing process"""
    print("\n=== PROCESS ANALYSIS DEMO ===\n")
    
    toolkit = CamundaToolkit("http://localhost:8080/engine-rest")
    
    # Analyze a process (replace with your process key)
    process_key = "order-process"
    
    print(f"🔍 Analyzing process: {process_key}")
    analysis = toolkit.analyzer.analyze_process(process_key)
    
    if 'error' in analysis:
        print(f"❌ Error: {analysis['error']}")
        return
    
    print(f"\n📊 Analysis Results:")
    print(f"  - Optimization Score: {analysis['optimization_score']}/100")
    print(f"  - Complexity: {analysis['structure']['complexity_score']}")
    print(f"  - Success Rate: {analysis['performance']['success_rate']:.1f}%")
    
    if analysis['bottlenecks']:
        print(f"\n🚧 Top Bottlenecks:")
        for b in analysis['bottlenecks'][:3]:
            print(f"  - {b['activity']}: avg {b['avg_duration_minutes']:.1f} min")
    
    if analysis['recommendations']:
        print(f"\n🤖 AI Recommendations:")
        for i, rec in enumerate(analysis['recommendations'], 1):
            print(f"  {i}. {rec}")


if __name__ == "__main__":
    print("🏭 CAMUNDA TOOLKIT DEMO\n")
    print("This demo shows how to use TuoKit to manage Camunda workflows.\n")
    
    # Run demos
    try:
        demo_generation()  # This works without connection
        
        # These require a running Camunda instance
        # demo_monitoring()
        # demo_analysis()
        
    except Exception as e:
        print(f"\n❌ Demo error: {str(e)}")
    
    print("\n✅ Demo complete!")
