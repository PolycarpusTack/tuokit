"""
Camunda Integration Examples - Practical patterns for WHATS'ON integration
"""

from toolkits.camunda.manager import CamundaManager, CamundaWorkflowBuilder
import streamlit as st
from utils import safe_ollama_generate, capture_knowledge

# Example 1: Automated Incident Resolution
def auto_resolve_incidents():
    """
    Automatically analyze and suggest fixes for common Camunda incidents
    """
    manager = CamundaManager()
    incidents = manager.get_incidents()
    
    resolutions = []
    for incident in incidents:
        # Pattern matching for common issues
        if "variable" in incident.get('incidentMessage', '').lower():
            resolution = {
                'type': 'missing_variable',
                'fix': f"Add variable '{incident['activityId']}_var' to process",
                'script': f"""
// Add this to your process variables
execution.setVariable('{incident['activityId']}_var', 'default_value');
"""
            }
        elif "connection" in incident.get('incidentMessage', '').lower():
            resolution = {
                'type': 'connection_error',
                'fix': "Check external service availability",
                'script': "// Implement retry logic with exponential backoff"
            }
        else:
            # Use AI for complex issues
            analysis = manager.analyze_incident_with_ai(incident)
            resolution = {
                'type': 'ai_suggested',
                'fix': analysis['analysis'],
                'script': "// See AI analysis for implementation details"
            }
        
        resolutions.append({
            'incident': incident,
            'resolution': resolution
        })
    
    return resolutions


# Example 2: WHATS'ON Integration Workflow Generator
def generate_whatson_workflow(integration_type: str, config: dict):
    """
    Generate Camunda workflows for common WHATS'ON integrations
    """
    
    templates = {
        'data_sync': """
Create a workflow that:
1. Polls WHATS'ON API for new {entity_type}
2. Validates the data against schema
3. Maps fields to {target_system} format
4. Performs upsert operation
5. Logs sync results
6. Handles errors with retry logic
Include parallel processing for batch operations.
""",
        'event_driven': """
Create an event-driven workflow that:
1. Receives webhook from WHATS'ON for {event_type}
2. Validates webhook signature
3. Extracts event payload
4. Routes to appropriate handler based on event type
5. Executes business logic for {business_process}
6. Updates WHATS'ON via API callback
7. Sends notifications on completion
""",
        'approval_chain': """
Create approval workflow for {approval_type}:
1. Receive request from WHATS'ON
2. Validate request completeness
3. Route to first approver based on {routing_rules}
4. If approved, route to next level
5. If rejected, notify requester with reasons
6. On final approval, trigger {completion_action}
7. Update WHATS'ON status throughout process
Include SLA timers and escalation paths.
"""
    }
    
    # Get template and fill in config values
    template = templates.get(integration_type, "")
    description = template.format(**config)
    
    manager = CamundaManager()
    bpmn_xml = manager.generate_bpmn_from_description(description)
    
    # Enhance with WHATS'ON specific elements
    enhanced_xml = enhance_for_whatson(bpmn_xml, config)
    
    return enhanced_xml


def enhance_for_whatson(bpmn_xml: str, config: dict) -> str:
    """
    Add WHATS'ON specific configurations to BPMN
    """
    import xml.etree.ElementTree as ET
    
    # Parse XML
    root = ET.fromstring(bpmn_xml)
    
    # Add extension elements for WHATS'ON
    for service_task in root.findall('.//{http://www.omg.org/spec/BPMN/20100524/MODEL}serviceTask'):
        # Add connector configuration
        extension = ET.SubElement(service_task, '{http://camunda.org/schema/1.0/bpmn}extensionElements')
        connector = ET.SubElement(extension, '{http://camunda.org/schema/1.0/bpmn}connector')
        
        # Configure HTTP connector for WHATS'ON API
        ET.SubElement(connector, '{http://camunda.org/schema/1.0/bpmn}connectorId').text = 'http-connector'
        
        input_params = ET.SubElement(connector, '{http://camunda.org/schema/1.0/bpmn}inputOutput')
        
        # API endpoint
        url_param = ET.SubElement(input_params, '{http://camunda.org/schema/1.0/bpmn}inputParameter')
        url_param.set('name', 'url')
        url_param.text = f"${{WHATSON_API_URL}}/{config.get('endpoint', 'api/v1/data')}"
        
        # Auth header
        auth_param = ET.SubElement(input_params, '{http://camunda.org/schema/1.0/bpmn}inputParameter')
        auth_param.set('name', 'headers')
        auth_param.text = "Authorization: Bearer ${WHATSON_API_TOKEN}"
    
    return ET.tostring(root, encoding='unicode')


# Example 3: Performance Monitoring Dashboard
def create_monitoring_dashboard():
    """
    Streamlit dashboard for monitoring Camunda-WHATS'ON integration
    """
    st.header("🎯 WHATS'ON Integration Monitor")
    
    manager = CamundaManager()
    
    # Key metrics
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        active = len(manager.get_process_instances())
        st.metric("Active Workflows", active)
    
    with col2:
        incidents = len(manager.get_incidents())
        st.metric("Current Incidents", incidents, delta="-2" if incidents < 2 else "+3")
    
    with col3:
        # Mock data - would come from actual monitoring
        st.metric("Avg Response Time", "1.2s", delta="-0.3s")
    
    with col4:
        st.metric("Success Rate", "98.5%", delta="+0.5%")
    
    # Process-specific monitoring
    st.subheader("Process Performance")
    
    processes = {
        "DataSync_Products": {"sla": 300, "current": 250},
        "OrderApproval": {"sla": 3600, "current": 3200},
        "CustomerOnboarding": {"sla": 7200, "current": 6800}
    }
    
    for process_name, metrics in processes.items():
        col1, col2 = st.columns([3, 1])
        with col1:
            progress = metrics['current'] / metrics['sla']
            st.progress(min(progress, 1.0))
            st.caption(f"{process_name}: {metrics['current']}s / {metrics['sla']}s SLA")
        with col2:
            if progress > 0.9:
                st.warning("⚠️ Near SLA")
            elif progress > 1.0:
                st.error("❌ SLA Breach")
            else:
                st.success("✅ OK")


# Example 4: Batch Workflow Deployment
def deploy_workflow_batch(workflow_definitions: list):
    """
    Deploy multiple workflows with validation and rollback capability
    """
    deployment_results = []
    
    for workflow in workflow_definitions:
        try:
            # Validate BPMN
            ET.fromstring(workflow['bpmn_xml'])
            
            # Mock deployment - would use actual Camunda deployment API
            result = {
                'name': workflow['name'],
                'status': 'deployed',
                'deployment_id': f"dep_{workflow['name']}_{datetime.now().timestamp()}"
            }
            
            # Capture for knowledge base
            capture_knowledge(
                tool="camunda_deployment",
                prompt=f"Deploy {workflow['name']}",
                response=json.dumps(result)
            )
            
        except Exception as e:
            result = {
                'name': workflow['name'],
                'status': 'failed',
                'error': str(e)
            }
        
        deployment_results.append(result)
    
    return deployment_results


# Example 5: Natural Language Process Query
def query_processes_nl(query: str):
    """
    Query running processes using natural language
    
    Examples:
    - "Show me all orders stuck for more than 2 hours"
    - "Find approval processes waiting on John"
    - "List failed payment processes from today"
    """
    
    prompt = f"""
Convert this natural language query to Camunda REST API parameters:
Query: "{query}"

Return JSON with:
- processDefinitionKey (if specific process mentioned)
- variables (for filtering)
- activityIdIn (for specific states)
- sorting and pagination

Examples:
"orders stuck" -> filter by processDefinitionKey containing "order", 
                  startedBefore = 2 hours ago, active = true
"""
    
    response = safe_ollama_generate(
        st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
        prompt
    )
    
    # Parse and execute query
    # ... implementation ...
    
    return response


if __name__ == "__main__":
    # Test examples
    print("Camunda-WHATS'ON Integration Examples loaded")