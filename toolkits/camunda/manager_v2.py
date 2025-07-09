# 🏗️ Camunda Manager V2 - Honest, Working Implementation
# Following TuoKit philosophy: Build what works, fix what's broken

import streamlit as st
import requests
import json
import xml.etree.ElementTree as ET
from typing import Dict, List, Optional, Tuple
from datetime import datetime
import re
from dataclasses import dataclass
import time

from utils.tool_base import TuoKitToolBase
from utils.ollama import safe_ollama_generate

# Fixed: Proper error hierarchy without name conflicts
class CamundaToolError(Exception):
    """Base error for Camunda operations"""
    def user_friendly_message(self) -> str:
        return "Something went wrong. Please check your connection and try again."

class CamundaConnectionError(CamundaToolError):
    """Connection-specific errors"""
    def user_friendly_message(self) -> str:
        return f"Cannot connect to Camunda: {str(self)}. Check if Camunda is running."

class CamundaValidationError(CamundaToolError):
    """Validation errors"""
    def user_friendly_message(self) -> str:
        return f"Invalid input: {str(self)}"

@dataclass
class SimpleProcess:
    """Simple process representation"""
    id: str
    name: str
    version: int
    active_instances: int = 0

@dataclass
class SimpleIncident:
    """Simple incident representation"""
    id: str
    process_id: str
    activity: str
    error_message: str
    timestamp: str

class CamundaManagerV2(TuoKitToolBase):
    """Practical Camunda manager - does what it says, no more"""
    
    def __init__(self, base_url: str = None):
        super().__init__(
            tool_name="Camunda Manager V2",
            tool_description="Practical Camunda workflow management"
        )
        self.base_url = base_url or "http://localhost:8080/engine-rest"
        self.headers = {"Content-Type": "application/json"}
        self._last_error = None
    
    def test_connection(self) -> Tuple[bool, str]:
        """Test connection - returns (success, message)"""
        try:
            response = requests.get(
                f"{self.base_url}/version",
                timeout=5
            )
            if response.status_code == 200:
                version = response.json()
                return True, f"Connected to Camunda {version.get('version', 'Unknown')}"
            else:
                return False, f"Server returned {response.status_code}"
        except requests.ConnectionError:
            return False, "Cannot connect - is Camunda running?"
        except requests.Timeout:
            return False, "Connection timed out"
        except Exception as e:
            return False, f"Unexpected error: {str(e)}"
    
    def get_process_definitions(self) -> List[SimpleProcess]:
        """Get list of deployed processes"""
        try:
            response = requests.get(
                f"{self.base_url}/process-definition",
                params={"latestVersion": "true"}
            )
            response.raise_for_status()
            
            processes = []
            for pd in response.json():
                # Get instance count
                count_response = requests.get(
                    f"{self.base_url}/process-instance/count",
                    params={"processDefinitionId": pd['id']}
                )
                count = count_response.json().get('count', 0) if count_response.ok else 0
                
                processes.append(SimpleProcess(
                    id=pd['id'],
                    name=pd.get('name', pd['key']),
                    version=pd.get('version', 1),
                    active_instances=count
                ))
            
            return processes
        except Exception as e:
            self._last_error = str(e)
            return []
    
    def deploy_bpmn(self, filename: str, content: str) -> Tuple[bool, str]:
        """Deploy BPMN file - returns (success, message)"""
        # Validate BPMN first
        is_valid, validation_msg = self.validate_bpmn(content)
        if not is_valid:
            return False, validation_msg
        
        try:
            files = {
                'deployment-name': (None, f'Python Deployment {datetime.now().isoformat()}'),
                'deployment-source': (None, 'TuoKit'),
                filename: (filename, content, 'text/xml')
            }
            
            response = requests.post(
                f"{self.base_url}/deployment/create",
                files=files
            )
            
            if response.status_code == 200:
                deployment = response.json()
                return True, f"Deployed successfully! ID: {deployment['id']}"
            else:
                return False, f"Deployment failed: {response.text}"
                
        except Exception as e:
            return False, f"Error during deployment: {str(e)}"
    
    def validate_bpmn(self, xml_content: str) -> Tuple[bool, str]:
        """Validate BPMN XML - returns (is_valid, message)"""
        try:
            # Basic XML validation
            root = ET.fromstring(xml_content)
            
            # Check for BPMN namespace - it could be in tag or as default namespace
            if not ('{http://www.omg.org/spec/BPMN/20100524/MODEL}' in root.tag or 
                    root.tag == 'definitions' or 
                    'bpmn:definitions' in root.tag):
                return False, "Not a valid BPMN file (missing BPMN definitions)"
            
            # Define namespaces for searching
            ns = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
            
            # Try to find processes with or without namespace prefix
            processes = root.findall('.//bpmn:process', ns)
            if not processes:
                # Try without namespace prefix
                processes = root.findall('.//process')
            
            if not processes:
                return False, "No process definitions found"
            
            # Check each process has start and end events
            for process in processes:
                # Try with namespace first
                starts = process.findall('.//bpmn:startEvent', ns)
                ends = process.findall('.//bpmn:endEvent', ns)
                
                # Try without namespace if not found
                if not starts:
                    starts = process.findall('.//startEvent')
                if not ends:
                    ends = process.findall('.//endEvent')
                
                if not starts:
                    return False, f"Process {process.get('id')} has no start event"
                if not ends:
                    return False, f"Process {process.get('id')} has no end event"
            
            return True, "BPMN is valid"
            
        except ET.ParseError as e:
            return False, f"XML parsing error: {str(e)}"
        except Exception as e:
            return False, f"Validation error: {str(e)}"
    
    def start_process(self, process_key: str, variables: Dict = None) -> Tuple[bool, str]:
        """Start a process instance"""
        try:
            payload = {}
            if variables:
                payload['variables'] = {
                    k: {"value": v} for k, v in variables.items()
                }
            
            response = requests.post(
                f"{self.base_url}/process-definition/key/{process_key}/start",
                json=payload,
                headers=self.headers
            )
            
            if response.status_code == 200:
                instance = response.json()
                return True, f"Started instance: {instance['id']}"
            else:
                return False, f"Failed to start: {response.text}"
                
        except Exception as e:
            return False, f"Error: {str(e)}"
    
    def get_incidents(self) -> List[SimpleIncident]:
        """Get current incidents"""
        try:
            response = requests.get(f"{self.base_url}/incident")
            response.raise_for_status()
            
            incidents = []
            for inc in response.json():
                incidents.append(SimpleIncident(
                    id=inc['id'],
                    process_id=inc.get('processInstanceId', 'Unknown'),
                    activity=inc.get('activityId', 'Unknown'),
                    error_message=inc.get('incidentMessage', 'No message'),
                    timestamp=inc.get('incidentTimestamp', '')
                ))
            
            return incidents
        except Exception as e:
            self._last_error = str(e)
            return []
    
    def generate_simple_bpmn(self, description: str) -> str:
        """Generate simple BPMN from description using AI"""
        prompt = f"""
Convert this description to a simple BPMN 2.0 XML:
"{description}"

Requirements:
1. Start with <?xml version="1.0" encoding="UTF-8"?>
2. Include proper BPMN namespace
3. One process with start and end events
4. Use service tasks for automated steps
5. Use user tasks for manual steps
6. Keep it simple and valid

Output ONLY the XML, no explanations.
"""
        
        try:
            response = safe_ollama_generate(
                model='mistral:latest',  # Use a model we know exists
                prompt=prompt
            )
            
            if response and 'response' in response:
                # Extract XML from response
                xml_text = response['response']
                
                # Try to find XML in the response
                xml_match = re.search(r'<\?xml.*?</bpmn:definitions>', xml_text, re.DOTALL)
                if xml_match:
                    return xml_match.group()
                
                # If no XML found, create a template
                return self._create_template_bpmn(description)
            else:
                return self._create_template_bpmn(description)
                
        except Exception as e:
            st.warning(f"AI generation failed: {str(e)}. Using template instead.")
            return self._create_template_bpmn(description)
    
    def _create_template_bpmn(self, description: str) -> str:
        """Create a simple template BPMN"""
        process_id = re.sub(r'[^a-zA-Z0-9]', '_', description[:20])
        
        return f"""<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"
                  xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                  xmlns:dc="http://www.omg.org/spec/DD/20100524/DC"
                  targetNamespace="http://bpmn.io/schema/bpmn">
  <bpmn:process id="{process_id}" name="{description[:50]}" isExecutable="true">
    <bpmn:startEvent id="StartEvent_1" name="Start">
      <bpmn:outgoing>Flow_1</bpmn:outgoing>
    </bpmn:startEvent>
    <bpmn:task id="Task_1" name="Process Step">
      <bpmn:incoming>Flow_1</bpmn:incoming>
      <bpmn:outgoing>Flow_2</bpmn:outgoing>
    </bpmn:task>
    <bpmn:endEvent id="EndEvent_1" name="End">
      <bpmn:incoming>Flow_2</bpmn:incoming>
    </bpmn:endEvent>
    <bpmn:sequenceFlow id="Flow_1" sourceRef="StartEvent_1" targetRef="Task_1" />
    <bpmn:sequenceFlow id="Flow_2" sourceRef="Task_1" targetRef="EndEvent_1" />
  </bpmn:process>
</bpmn:definitions>"""


# Simple helper functions for common patterns
def create_service_task(task_id: str, name: str, topic: str) -> str:
    """Create a service task XML snippet"""
    return f"""
    <bpmn:serviceTask id="{task_id}" name="{name}">
      <bpmn:extensionElements>
        <camunda:properties>
          <camunda:property name="topic" value="{topic}"/>
        </camunda:properties>
      </bpmn:extensionElements>
    </bpmn:serviceTask>"""

def create_user_task(task_id: str, name: str, assignee: str = None) -> str:
    """Create a user task XML snippet"""
    assignee_attr = f'camunda:assignee="{assignee}"' if assignee else ''
    return f"""
    <bpmn:userTask id="{task_id}" name="{name}" {assignee_attr}>
    </bpmn:userTask>"""

def create_exclusive_gateway(gateway_id: str, name: str) -> str:
    """Create an exclusive gateway XML snippet"""
    return f"""
    <bpmn:exclusiveGateway id="{gateway_id}" name="{name}">
    </bpmn:exclusiveGateway>"""