# 🏗️ Enhanced Camunda Manager with SDK Patterns
# Following TuoKit's SDK-inspired design patterns from CLAUDE.md

import streamlit as st
import requests
import xml.etree.ElementTree as ET
from typing import Dict, List, Optional, Generator, Callable, Any
import json
from datetime import datetime
import asyncio
from dataclasses import dataclass
from enum import Enum
import time

from utils import (
    safe_ollama_generate, 
    capture_knowledge,
    DatabaseManager,
    get_available_models
)
from utils.tool_base import TuoKitToolBase

# Error Hierarchy (SDK Pattern)
class CamundaError(Exception):
    """Base Camunda error with user-friendly messaging"""
    def user_friendly_message(self) -> str:
        return "Something went wrong with Camunda. Try checking your connection."

class ConnectionError(CamundaError):
    """Connection-related errors"""
    def user_friendly_message(self) -> str:
        return "Cannot connect to Camunda. Check if it's running and the URL is correct."

class ProcessNotFoundError(CamundaError):
    """Process definition not found"""
    def user_friendly_message(self) -> str:
        return "Process not found. Verify the process ID or deploy the process first."

class DeploymentError(CamundaError):
    """Deployment failed"""
    def user_friendly_message(self) -> str:
        return "Failed to deploy process. Check the BPMN XML for errors."

# Event Types (SDK Pattern)
class CamundaEvent(Enum):
    CONNECT_START = "connect_start"
    CONNECT_SUCCESS = "connect_success"
    CONNECT_FAIL = "connect_fail"
    PROCESS_FETCH = "process_fetch"
    INCIDENT_ANALYSIS = "incident_analysis"
    BPMN_GENERATION = "bpmn_generation"
    OPTIMIZATION_START = "optimization_start"
    OPTIMIZATION_COMPLETE = "optimization_complete"

# Data Classes for Type Safety
@dataclass
class ProcessInstance:
    id: str
    process_definition_id: str
    start_time: datetime
    state: str
    business_key: Optional[str] = None
    
@dataclass
class Incident:
    id: str
    process_instance_id: str
    activity_id: str
    incident_type: str
    incident_message: str
    created_time: datetime

@dataclass
class OptimizationResult:
    score: float
    suggestions: List[str]
    risks: List[str]
    opportunities: List[str]
    
class StreamingHandler:
    """Handles streaming responses (SDK Pattern)"""
    def __init__(self):
        self.callbacks: Dict[str, List[Callable]] = {}
        
    def on(self, event: str, callback: Callable):
        """Register event callback"""
        if event not in self.callbacks:
            self.callbacks[event] = []
        self.callbacks[event].append(callback)
        
    def emit(self, event: str, data: Any = None):
        """Emit event to callbacks"""
        if event in self.callbacks:
            for callback in self.callbacks[event]:
                callback(data)

class EnhancedCamundaManager(TuoKitToolBase):
    """Enhanced Camunda Manager with SDK patterns"""
    
    def __init__(self, base_url: str = "http://localhost:8080/engine-rest"):
        super().__init__(
            tool_name="Enhanced Camunda Manager",
            tool_description="AI-powered Camunda workflow management with advanced features"
        )
        self.base_url = base_url
        self.headers = {"Content-Type": "application/json"}
        self.streaming_handler = StreamingHandler()
        self._connection_pool = None
        self._retry_count = 3
        self._retry_delay = 1.0
        
    def test_connection_with_retry(self) -> bool:
        """Test connection with automatic retry and progress feedback"""
        self.streaming_handler.emit(CamundaEvent.CONNECT_START.value)
        
        for attempt in range(self._retry_count):
            try:
                response = requests.get(
                    f"{self.base_url}/version",
                    timeout=5
                )
                if response.status_code == 200:
                    self.streaming_handler.emit(
                        CamundaEvent.CONNECT_SUCCESS.value,
                        {"version": response.json()}
                    )
                    return True
            except requests.RequestException as e:
                if attempt < self._retry_count - 1:
                    time.sleep(self._retry_delay * (attempt + 1))
                    continue
                    
        self.streaming_handler.emit(CamundaEvent.CONNECT_FAIL.value)
        raise ConnectionError(f"Failed to connect after {self._retry_count} attempts")
        
    def get_process_instances_streaming(self, 
                                      active_only: bool = True,
                                      stream: bool = True) -> Generator[ProcessInstance, None, None]:
        """Fetch process instances with streaming support"""
        self.streaming_handler.emit(CamundaEvent.PROCESS_FETCH.value, {"start": True})
        
        params = {"active": "true"} if active_only else {}
        
        try:
            response = requests.get(
                f"{self.base_url}/process-instance",
                params=params,
                stream=stream
            )
            response.raise_for_status()
            
            instances = response.json()
            total = len(instances)
            
            for i, instance in enumerate(instances):
                # Convert to typed object
                process = ProcessInstance(
                    id=instance['id'],
                    process_definition_id=instance['definitionId'],
                    start_time=datetime.fromisoformat(
                        instance['startTime'].replace('Z', '+00:00')
                    ),
                    state="active" if instance.get('ended') is False else "completed",
                    business_key=instance.get('businessKey')
                )
                
                # Emit progress
                self.streaming_handler.emit(
                    CamundaEvent.PROCESS_FETCH.value, 
                    {"progress": (i + 1) / total * 100}
                )
                
                yield process
                
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to fetch processes: {str(e)}")
            
    def analyze_incident_enhanced(self, 
                                incident: Incident,
                                context_size: int = 5) -> Dict:
        """Enhanced incident analysis with context awareness"""
        self.streaming_handler.emit(
            CamundaEvent.INCIDENT_ANALYSIS.value,
            {"status": "fetching_context"}
        )
        
        # Fetch process context
        context = self._fetch_incident_context(incident, context_size)
        
        self.streaming_handler.emit(
            CamundaEvent.INCIDENT_ANALYSIS.value,
            {"status": "analyzing"}
        )
        
        prompt = f"""
Analyze this Camunda incident with full context:

INCIDENT DETAILS:
- Process: {incident.process_instance_id}
- Activity: {incident.activity_id}
- Error Type: {incident.incident_type}
- Message: {incident.incident_message}
- Time: {incident.created_time}

CONTEXT:
- Previous Activities: {context.get('previous_activities', [])}
- Process Variables: {json.dumps(context.get('variables', {}), indent=2)}
- Similar Past Incidents: {context.get('similar_incidents', 0)}

Provide:
1. Root Cause Analysis (be specific)
2. Immediate Fix Steps
3. Long-term Prevention Strategy
4. Risk Assessment (1-10)
5. Estimated Resolution Time
"""
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        # Parse and structure response
        analysis = self._parse_incident_analysis(response['response'])
        
        # Capture to knowledge base
        self.capture_knowledge(
            "incident_analysis",
            json.dumps(incident.__dict__, default=str),
            response['response']
        )
        
        self.streaming_handler.emit(
            CamundaEvent.INCIDENT_ANALYSIS.value,
            {"status": "complete", "analysis": analysis}
        )
        
        return analysis
        
    def generate_bpmn_with_templates(self, 
                                   description: str,
                                   template: Optional[str] = None) -> str:
        """Generate BPMN with template support and validation"""
        self.streaming_handler.emit(
            CamundaEvent.BPMN_GENERATION.value,
            {"status": "preparing"}
        )
        
        # Load template if specified
        template_context = ""
        if template:
            template_context = self._load_bpmn_template(template)
            
        prompt = f"""
Generate BPMN 2.0 XML for this process:
"{description}"

{f"Use this template as reference: {template_context}" if template_context else ""}

Requirements:
1. Valid BPMN 2.0 XML with proper namespaces
2. Include:
   - Start and End events
   - Appropriate task types (user/service/script)
   - Gateways for decisions
   - Error boundary events where appropriate
   - Proper sequence flows with conditions
3. Follow Camunda best practices
4. Add implementation details (topics, forms, etc.)

Output ONLY the XML, no explanations.
"""
        
        self.streaming_handler.emit(
            CamundaEvent.BPMN_GENERATION.value,
            {"status": "generating"}
        )
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        # Validate and clean XML
        xml_content = self._extract_and_validate_xml(response['response'])
        
        self.streaming_handler.emit(
            CamundaEvent.BPMN_GENERATION.value,
            {"status": "complete", "xml": xml_content}
        )
        
        return xml_content
        
    def optimize_workflow_advanced(self, 
                                 bpmn_xml: str,
                                 optimization_goals: List[str] = None) -> OptimizationResult:
        """Advanced workflow optimization with specific goals"""
        self.streaming_handler.emit(
            CamundaEvent.OPTIMIZATION_START.value,
            {"goals": optimization_goals}
        )
        
        if not optimization_goals:
            optimization_goals = [
                "performance", 
                "reliability", 
                "maintainability",
                "cost"
            ]
            
        # Parse BPMN structure
        structure = self._analyze_bpmn_structure(bpmn_xml)
        
        prompt = f"""
Analyze and optimize this BPMN workflow focusing on: {', '.join(optimization_goals)}

WORKFLOW STRUCTURE:
- Tasks: {structure['task_count']} ({structure['task_types']})
- Gateways: {structure['gateway_count']} ({structure['gateway_types']})
- Events: {structure['event_count']}
- Subprocesses: {structure['subprocess_count']}

BPMN (excerpt):
{bpmn_xml[:1500]}...

Provide:
1. Optimization Score (0-100) for each goal
2. Specific improvements for each goal
3. Risk factors to consider
4. Quick wins vs long-term improvements
5. Implementation priority (1-5) for each suggestion

Format as structured data for parsing.
"""
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        # Parse optimization results
        result = self._parse_optimization_result(response['response'])
        
        self.streaming_handler.emit(
            CamundaEvent.OPTIMIZATION_COMPLETE.value,
            {"result": result}
        )
        
        return result
        
    def _fetch_incident_context(self, incident: Incident, size: int) -> Dict:
        """Fetch context around an incident"""
        context = {}
        
        try:
            # Get process history
            history = requests.get(
                f"{self.base_url}/history/activity-instance",
                params={
                    "processInstanceId": incident.process_instance_id,
                    "sortBy": "startTime",
                    "sortOrder": "desc",
                    "maxResults": size
                }
            ).json()
            
            context['previous_activities'] = [
                act['activityName'] for act in history
            ]
            
            # Get process variables
            variables = requests.get(
                f"{self.base_url}/process-instance/{incident.process_instance_id}/variables"
            ).json()
            
            context['variables'] = {
                k: v.get('value') for k, v in variables.items()
            }
            
            # Count similar incidents
            similar = requests.get(
                f"{self.base_url}/history/incident",
                params={
                    "activityId": incident.activity_id,
                    "incidentType": incident.incident_type
                }
            ).json()
            
            context['similar_incidents'] = len(similar)
            
        except Exception as e:
            st.warning(f"Could not fetch full context: {e}")
            
        return context
        
    def _parse_incident_analysis(self, ai_response: str) -> Dict:
        """Parse AI incident analysis into structured format"""
        # Basic parsing - in production, use more sophisticated NLP
        analysis = {
            "root_cause": "",
            "immediate_fix": [],
            "prevention": "",
            "risk_score": 5,
            "resolution_time": "Unknown"
        }
        
        lines = ai_response.split('\n')
        current_section = None
        
        for line in lines:
            line = line.strip()
            if "Root Cause" in line:
                current_section = "root_cause"
            elif "Immediate Fix" in line:
                current_section = "immediate_fix"
            elif "Prevention" in line:
                current_section = "prevention"
            elif "Risk Assessment" in line:
                current_section = "risk_score"
            elif "Resolution Time" in line:
                current_section = "resolution_time"
            elif line and current_section:
                if current_section == "immediate_fix":
                    if line.startswith(('-', '*', '•')):
                        analysis[current_section].append(line[1:].strip())
                elif current_section == "risk_score":
                    # Extract number
                    import re
                    match = re.search(r'\d+', line)
                    if match:
                        analysis[current_section] = int(match.group())
                else:
                    analysis[current_section] += line + " "
                    
        return analysis
        
    def _load_bpmn_template(self, template_name: str) -> str:
        """Load BPMN template"""
        templates = {
            "approval": """
                <bpmn:process id="ApprovalProcess">
                    <bpmn:startEvent id="Start"/>
                    <bpmn:userTask id="Submit" name="Submit Request"/>
                    <bpmn:userTask id="Review" name="Review Request"/>
                    <bpmn:exclusiveGateway id="Decision"/>
                    <bpmn:endEvent id="Approved"/>
                    <bpmn:endEvent id="Rejected"/>
                </bpmn:process>
            """,
            "service_orchestration": """
                <bpmn:process id="ServiceProcess">
                    <bpmn:startEvent id="Start"/>
                    <bpmn:serviceTask id="CallAPI" camunda:type="external" camunda:topic="api-call"/>
                    <bpmn:serviceTask id="ProcessData" camunda:type="external" camunda:topic="data-processing"/>
                    <bpmn:endEvent id="End"/>
                </bpmn:process>
            """
        }
        return templates.get(template_name, "")
        
    def _extract_and_validate_xml(self, response: str) -> str:
        """Extract and validate BPMN XML from AI response"""
        import re
        
        # Try to find XML in response
        xml_match = re.search(
            r'<\?xml.*?</bpmn:definitions>',
            response,
            re.DOTALL | re.IGNORECASE
        )
        
        if xml_match:
            xml_content = xml_match.group()
        else:
            # Try without XML declaration
            xml_match = re.search(
                r'<bpmn:definitions.*?</bpmn:definitions>',
                response,
                re.DOTALL | re.IGNORECASE
            )
            if xml_match:
                xml_content = '<?xml version="1.0" encoding="UTF-8"?>\n' + xml_match.group()
            else:
                raise ValueError("No valid BPMN XML found in response")
                
        # Validate XML
        try:
            ET.fromstring(xml_content)
        except ET.ParseError as e:
            # Try to fix common issues
            xml_content = self._fix_common_xml_issues(xml_content)
            ET.fromstring(xml_content)  # Validate again
            
        return xml_content
        
    def _fix_common_xml_issues(self, xml: str) -> str:
        """Fix common XML issues"""
        # Add missing namespaces
        if 'xmlns:bpmn' not in xml:
            xml = xml.replace(
                '<bpmn:definitions',
                '<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"'
            )
        return xml
        
    def _analyze_bpmn_structure(self, bpmn_xml: str) -> Dict:
        """Analyze BPMN structure for optimization"""
        try:
            root = ET.fromstring(bpmn_xml)
            ns = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
            
            structure = {
                'task_count': len(root.findall('.//bpmn:task', ns)),
                'gateway_count': len(root.findall('.//bpmn:*[contains(local-name(), "Gateway")]', ns)),
                'event_count': len(root.findall('.//bpmn:*[contains(local-name(), "Event")]', ns)),
                'subprocess_count': len(root.findall('.//bpmn:subProcess', ns)),
                'task_types': {},
                'gateway_types': {}
            }
            
            # Count task types
            for task_type in ['userTask', 'serviceTask', 'scriptTask', 'businessRuleTask']:
                count = len(root.findall(f'.//bpmn:{task_type}', ns))
                if count > 0:
                    structure['task_types'][task_type] = count
                    
            # Count gateway types
            for gw_type in ['exclusiveGateway', 'parallelGateway', 'inclusiveGateway']:
                count = len(root.findall(f'.//bpmn:{gw_type}', ns))
                if count > 0:
                    structure['gateway_types'][gw_type] = count
                    
        except Exception as e:
            structure = {
                'error': str(e),
                'task_count': 0,
                'gateway_count': 0,
                'event_count': 0,
                'subprocess_count': 0
            }
            
        return structure
        
    def _parse_optimization_result(self, ai_response: str) -> OptimizationResult:
        """Parse optimization results"""
        # Simple parsing - enhance with NLP in production
        result = OptimizationResult(
            score=75.0,
            suggestions=[],
            risks=[],
            opportunities=[]
        )
        
        lines = ai_response.split('\n')
        current_section = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            if "Score" in line and ":" in line:
                try:
                    score = float(line.split(':')[1].strip().replace('%', ''))
                    result.score = score
                except:
                    pass
            elif "Improvement" in line or "Suggestion" in line:
                current_section = "suggestions"
            elif "Risk" in line:
                current_section = "risks"
            elif "Opportunit" in line:
                current_section = "opportunities"
            elif line.startswith(('-', '*', '•', '1.', '2.', '3.')):
                cleaned = line.lstrip('-*•0123456789. ')
                if current_section == "suggestions":
                    result.suggestions.append(cleaned)
                elif current_section == "risks":
                    result.risks.append(cleaned)
                elif current_section == "opportunities":
                    result.opportunities.append(cleaned)
                    
        return result


# Predefined workflow templates
WORKFLOW_TEMPLATES = {
    "approval_chain": {
        "name": "Approval Chain",
        "description": "Multi-level approval workflow",
        "template": """
Employee submits request, 
direct manager reviews and approves or rejects,
if approved and amount > 1000 then department head must approve,
if department head approves then finance reviews,
finance can approve or request more info,
if approved by all then request is processed,
send notification to employee with final status
"""
    },
    "customer_onboarding": {
        "name": "Customer Onboarding",
        "description": "New customer setup process",
        "template": """
Customer fills registration form,
system validates email and phone,
if validation fails then notify customer and end,
create customer account,
send welcome email with activation link,
wait for activation (max 7 days),
if activated then assign account manager,
account manager schedules welcome call,
after call update customer profile,
if not activated within 7 days then send reminder,
wait 3 more days,
if still not activated then archive account
"""
    },
    "incident_management": {
        "name": "Incident Management",
        "description": "IT incident handling process",
        "template": """
User reports incident via portal,
system creates ticket and assigns priority,
if priority is critical then page on-call engineer immediately,
else assign to support queue,
engineer investigates issue,
if can resolve then implement fix,
else escalate to senior engineer,
senior engineer resolves issue,
update ticket with resolution,
notify user of resolution,
wait 24 hours for user confirmation,
if user confirms resolved then close ticket,
else reopen and investigate further
"""
    },
    "order_fulfillment": {
        "name": "Order Fulfillment",
        "description": "E-commerce order processing",
        "template": """
Customer places order,
validate payment information,
if payment fails then notify customer and cancel order,
charge payment,
check inventory for all items,
if any item out of stock then offer alternatives or refund,
pick items from warehouse,
pack items,
generate shipping label,
schedule pickup with carrier,
send tracking info to customer,
wait for delivery confirmation,
after delivery wait 3 days,
send feedback request to customer
"""
    }
}

# Common Camunda patterns for quick insertion
CAMUNDA_PATTERNS = {
    "retry_mechanism": """
<bpmn:serviceTask id="ServiceTask_Retry" name="Service Call with Retry">
  <bpmn:extensionElements>
    <camunda:failedJobRetryTimeCycle>R3/PT30S</camunda:failedJobRetryTimeCycle>
  </bpmn:extensionElements>
</bpmn:serviceTask>
""",
    "async_continuation": """
<bpmn:serviceTask id="AsyncTask" name="Async Task" camunda:asyncBefore="true" camunda:asyncAfter="true">
  <bpmn:extensionElements>
    <camunda:properties>
      <camunda:property name="exclusive" value="false"/>
    </camunda:properties>
  </bpmn:extensionElements>
</bpmn:serviceTask>
""",
    "timer_boundary": """
<bpmn:boundaryEvent id="TimerBoundary" attachedToRef="UserTask">
  <bpmn:timerEventDefinition>
    <bpmn:timeDuration>PT1H</bpmn:timeDuration>
  </bpmn:timerEventDefinition>
</bpmn:boundaryEvent>
""",
    "error_boundary": """
<bpmn:boundaryEvent id="ErrorBoundary" attachedToRef="ServiceTask">
  <bpmn:errorEventDefinition errorRef="Error_TechnicalError"/>
</bpmn:boundaryEvent>
""",
    "conditional_start": """
<bpmn:startEvent id="ConditionalStart">
  <bpmn:conditionalEventDefinition>
    <bpmn:condition xsi:type="bpmn:tFormalExpression">
      ${customerType == 'Premium' and orderValue > 1000}
    </bpmn:condition>
  </bpmn:conditionalEventDefinition>
</bpmn:startEvent>
"""
}