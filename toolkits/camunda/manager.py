# 🏗️ TuoKit Camunda Manager
# Practical AI-powered Camunda workflow management

import streamlit as st
import requests
import xml.etree.ElementTree as ET
from typing import Dict, List, Optional
import json
from datetime import datetime

from utils import (
    safe_ollama_generate, 
    capture_knowledge,
    DatabaseManager,
    get_available_models
)

class CamundaManager:
    """TuoKit's practical Camunda management interface"""
    
    def __init__(self, base_url: str = "http://localhost:8080/engine-rest"):
        self.base_url = base_url
        self.headers = {"Content-Type": "application/json"}
        
    def test_connection(self) -> bool:
        """Test Camunda connection"""
        try:
            response = requests.get(f"{self.base_url}/version")
            return response.status_code == 200
        except:
            return False
    
    def get_process_instances(self, active_only: bool = True) -> List[Dict]:
        """Fetch running process instances"""
        params = {"active": "true"} if active_only else {}
        response = requests.get(
            f"{self.base_url}/process-instance",
            params=params
        )
        return response.json() if response.status_code == 200 else []
    
    def get_incidents(self) -> List[Dict]:
        """Get current incidents/errors"""
        response = requests.get(f"{self.base_url}/incident")
        return response.json() if response.status_code == 200 else []
    
    def analyze_incident_with_ai(self, incident: Dict) -> Dict:
        """Use Ollama to analyze Camunda incidents"""
        prompt = f"""
Analyze this Camunda workflow incident:
- Process: {incident.get('processDefinitionId')}
- Activity: {incident.get('activityId')}
- Error: {incident.get('incidentMessage')}
- Type: {incident.get('incidentType')}

Provide:
1. Root cause analysis
2. Suggested fix
3. Prevention strategy
"""
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        # Capture to knowledge base
        capture_knowledge(
            tool="camunda_incident_analyzer",
            prompt=json.dumps(incident),
            response=response['response']
        )
        
        return {
            'analysis': response['response'],
            'incident_id': incident.get('id'),
            'timestamp': datetime.now().isoformat()
        }
    
    def generate_bpmn_from_description(self, description: str) -> str:
        """Generate BPMN XML from natural language description"""
        prompt = f"""
Convert this process description to BPMN 2.0 XML:
"{description}"

Generate valid BPMN with:
- Start/End events
- Service tasks for automated steps
- User tasks for manual steps
- Gateways for decisions
- Proper sequence flows

Return ONLY the XML, no explanations.
"""
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        # Extract XML from response
        xml_content = response['response']
        
        # Validate XML structure
        try:
            ET.fromstring(xml_content)
            return xml_content
        except ET.ParseError:
            # Try to extract XML from response
            import re
            xml_match = re.search(r'<\?xml.*?</bpmn:definitions>', 
                                 xml_content, re.DOTALL)
            if xml_match:
                return xml_match.group()
        
        return xml_content
    def optimize_workflow_with_ai(self, bpmn_xml: str) -> Dict:
        """Analyze and suggest optimizations for BPMN workflow"""
        prompt = f"""
Analyze this BPMN workflow and suggest optimizations:
{bpmn_xml[:1000]}...

Consider:
1. Parallel execution opportunities
2. Redundant steps
3. Error handling gaps
4. Performance bottlenecks
5. Automation opportunities

Provide specific, actionable recommendations.
"""
        
        response = safe_ollama_generate(
            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
            prompt
        )
        
        return {
            'optimizations': response['response'],
            'analyzed_at': datetime.now().isoformat()
        }
    
    def monitor_sla_compliance(self, process_def_id: str, 
                              sla_minutes: int = 60) -> Dict:
        """Monitor SLA compliance for processes"""
        instances = requests.get(
            f"{self.base_url}/history/process-instance",
            params={
                "processDefinitionId": process_def_id,
                "finished": "true",
                "sortBy": "endTime",
                "sortOrder": "desc",
                "maxResults": 100
            }
        ).json()
        
        violations = []
        compliant = []
        
        for instance in instances:
            start = datetime.fromisoformat(instance['startTime'].replace('Z', ''))
            end = datetime.fromisoformat(instance['endTime'].replace('Z', ''))
            duration_minutes = (end - start).total_seconds() / 60
            
            if duration_minutes > sla_minutes:
                violations.append({
                    'id': instance['id'],
                    'duration': duration_minutes,
                    'exceeded_by': duration_minutes - sla_minutes
                })
            else:
                compliant.append(instance['id'])
        
        return {
            'total': len(instances),
            'compliant': len(compliant),
            'violations': len(violations),
            'compliance_rate': len(compliant) / len(instances) * 100 if instances else 0,
            'violation_details': violations[:10]  # Top 10 violations
        }


class CamundaWorkflowBuilder:
    """Helper to build BPMN workflows programmatically"""
    
    def __init__(self):
        self.elements = []
        self.flows = []
        self.element_counter = 0
        
    def add_start_event(self, name: str = "Start") -> str:
        """Add start event"""
        element_id = f"StartEvent_{self.element_counter}"
        self.element_counter += 1
        
        self.elements.append(f'''
    <bpmn:startEvent id="{element_id}" name="{name}">
      <bpmn:outgoing>Flow_{element_id}</bpmn:outgoing>
    </bpmn:startEvent>''')
        
        return element_id
    
    def add_service_task(self, name: str, topic: str, 
                        prev_element: str) -> str:
        """Add service task"""
        element_id = f"ServiceTask_{self.element_counter}"
        flow_id = f"Flow_{self.element_counter}"
        self.element_counter += 1
        
        self.elements.append(f'''
    <bpmn:serviceTask id="{element_id}" name="{name}" 
                      camunda:type="external" camunda:topic="{topic}">
      <bpmn:incoming>Flow_{prev_element}</bpmn:incoming>
      <bpmn:outgoing>{flow_id}</bpmn:outgoing>
    </bpmn:serviceTask>''')
        
        self.flows.append(f'''
    <bpmn:sequenceFlow id="Flow_{prev_element}" 
                       sourceRef="{prev_element}" targetRef="{element_id}" />''')
        
        return element_id
    
    def add_end_event(self, name: str, prev_element: str) -> str:
        """Add end event"""
        element_id = f"EndEvent_{self.element_counter}"
        self.element_counter += 1
        
        self.elements.append(f'''
    <bpmn:endEvent id="{element_id}" name="{name}">
      <bpmn:incoming>Flow_{prev_element}</bpmn:incoming>
    </bpmn:endEvent>''')
        
        self.flows.append(f'''
    <bpmn:sequenceFlow id="Flow_{prev_element}" 
                       sourceRef="{prev_element}" targetRef="{element_id}" />''')
        
        return element_id
    
    def build(self, process_id: str, process_name: str) -> str:
        """Build complete BPMN XML"""
        return f'''<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"
                  xmlns:camunda="http://camunda.org/schema/1.0/bpmn"
                  targetNamespace="http://bpmn.io/schema/bpmn">
  <bpmn:process id="{process_id}" name="{process_name}" isExecutable="true">
    {''.join(self.elements)}
    {''.join(self.flows)}
  </bpmn:process>
</bpmn:definitions>'''