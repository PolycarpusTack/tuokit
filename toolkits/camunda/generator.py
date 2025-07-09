"""
BPMN Generator - Create Camunda workflows from natural language
"""

from typing import Dict, List, Optional, Tuple
import xml.etree.ElementTree as ET
import uuid
import re

try:
    from utils import safe_ollama_generate, capture_knowledge
except ImportError:
    # Fallback for testing without full TuoKit environment
    def safe_ollama_generate(model, prompt):
        return {'response': '{"tasks": [], "gateways": [], "flows": [], "events": []}'}
    def capture_knowledge(*args, **kwargs):
        pass


class BPMNGenerator:
    """Generate BPMN XML from natural language descriptions"""
    
    def __init__(self):
        self.namespaces = {
            'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL',
            'bpmndi': 'http://www.omg.org/spec/BPMN/20100524/DI',
            'dc': 'http://www.omg.org/spec/DD/20100524/DC',
            'di': 'http://www.omg.org/spec/DD/20100524/DI',
            'camunda': 'http://camunda.org/schema/1.0/bpmn'
        }
        
        # Register namespaces
        for prefix, uri in self.namespaces.items():
            ET.register_namespace(prefix, uri)
    
    def from_text(self, description: str, process_name: str = None) -> str:
        """Generate BPMN from natural language description"""
        # Extract workflow structure using AI
        workflow = self._analyze_workflow(description)
        
        # Generate process name if not provided
        if not process_name:
            process_name = self._generate_process_name(description)
        
        # Build BPMN XML
        bpmn_xml = self._build_bpmn(process_name, workflow)
        
        # Capture to knowledge base
        capture_knowledge(
            tool="camunda_generator",
            prompt=description,
            response=bpmn_xml,
            metadata={'process_name': process_name}
        )
        
        return bpmn_xml
    
    def _analyze_workflow(self, description: str) -> Dict:
        """Use AI to extract workflow structure from text"""
        prompt = f"""
Analyze this business process description and extract the workflow structure:

"{description}"

Return a JSON structure with:
{{
    "tasks": [
        {{
            "id": "task1",
            "name": "Task Name",
            "type": "user|service|businessRule|script",
            "assignee": "role or user (if mentioned)",
            "implementation": "external|java|expression (if service task)"
        }}
    ],
    "gateways": [
        {{
            "id": "gateway1", 
            "type": "exclusive|parallel|inclusive",
            "condition": "description of decision"
        }}
    ],
    "flows": [
        {{
            "from": "elementId",
            "to": "elementId",
            "condition": "optional condition expression"
        }}
    ],
    "events": [
        {{
            "id": "event1",
            "type": "start|end|timer|message|error",
            "name": "Event Name"
        }}
    ]
}}

Extract all process steps, decisions, and flow logic.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        try:
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response['response'], re.DOTALL)
            if json_match:
                import json
                return json.loads(json_match.group())
        except:
            pass
        
        # Fallback structure
        return self._extract_simple_workflow(description)
    
    def _extract_simple_workflow(self, description: str) -> Dict:
        """Fallback: Extract simple workflow using patterns"""
        workflow = {
            'tasks': [],
            'gateways': [],
            'flows': [],
            'events': [
                {'id': 'start', 'type': 'start', 'name': 'Start'},
                {'id': 'end', 'type': 'end', 'name': 'End'}
            ]
        }
        
        # Common patterns
        task_patterns = [
            r'(?:then\s+)?(\w+(?:\s+\w+)*?)(?:\s+(?:the|a|an)\s+)?(\w+)',
            r'(?:must|should|will)\s+(\w+(?:\s+\w+)*)',
            r'(?:process|handle|check|verify|approve|send|notify)\s+(\w+(?:\s+\w+)*)'
        ]
        
        # Extract tasks
        sentences = re.split(r'[.!?]', description)
        task_count = 0
        last_element = 'start'
        
        for sentence in sentences:
            sentence = sentence.strip()
            if not sentence:
                continue
                
            # Check for conditional
            if any(word in sentence.lower() for word in ['if', 'when', 'unless']):
                gateway_id = f'gateway_{len(workflow["gateways"])}'
                workflow['gateways'].append({
                    'id': gateway_id,
                    'type': 'exclusive',
                    'condition': sentence
                })
                workflow['flows'].append({'from': last_element, 'to': gateway_id})
                last_element = gateway_id
            
            # Extract tasks from sentence
            for pattern in task_patterns:
                matches = re.findall(pattern, sentence, re.IGNORECASE)
                for match in matches:
                    task_id = f'task_{task_count}'
                    task_name = ' '.join(match) if isinstance(match, tuple) else match
                    
                    workflow['tasks'].append({
                        'id': task_id,
                        'name': task_name.title(),
                        'type': 'user'  # Default to user task
                    })
                    
                    workflow['flows'].append({'from': last_element, 'to': task_id})
                    last_element = task_id
                    task_count += 1
        
        # Connect to end
        workflow['flows'].append({'from': last_element, 'to': 'end'})
        
        return workflow
    
    def _build_bpmn(self, process_name: str, workflow: Dict) -> str:
        """Build BPMN XML from workflow structure"""
        # Create root elements
        definitions = ET.Element('definitions', {
            'xmlns': self.namespaces['bpmn'],
            'xmlns:bpmndi': self.namespaces['bpmndi'],
            'xmlns:dc': self.namespaces['dc'],
            'xmlns:di': self.namespaces['di'],
            'xmlns:camunda': self.namespaces['camunda'],
            'targetNamespace': 'http://tuokit.camunda',
            'id': f'Definitions_{uuid.uuid4().hex[:8]}'
        })
        
        # Create process
        process_id = process_name.replace(' ', '_')
        process = ET.SubElement(definitions, 'process', {
            'id': process_id,
            'name': process_name,
            'isExecutable': 'true'
        })
        
        # Add events
        for event in workflow.get('events', []):
            self._add_event(process, event)
        
        # Add tasks
        for task in workflow.get('tasks', []):
            self._add_task(process, task)
        
        # Add gateways
        for gateway in workflow.get('gateways', []):
            self._add_gateway(process, gateway)
        
        # Add sequence flows
        for flow in workflow.get('flows', []):
            self._add_flow(process, flow)
        
        # Add diagram (basic layout)
        self._add_diagram(definitions, process_id, workflow)
        
        # Convert to string
        return ET.tostring(definitions, encoding='unicode', method='xml')
    
    def _add_event(self, process: ET.Element, event: Dict):
        """Add event to process"""
        if event['type'] == 'start':
            elem = ET.SubElement(process, 'startEvent', {
                'id': event['id'],
                'name': event.get('name', 'Start')
            })
        elif event['type'] == 'end':
            elem = ET.SubElement(process, 'endEvent', {
                'id': event['id'],
                'name': event.get('name', 'End')
            })
        # Add outgoing flows will be handled by sequence flows
    
    def _add_task(self, process: ET.Element, task: Dict):
        """Add task to process"""
        task_type = task.get('type', 'user')
        
        if task_type == 'user':
            elem = ET.SubElement(process, 'userTask', {
                'id': task['id'],
                'name': task['name']
            })
            if task.get('assignee'):
                elem.set(f"{{{self.namespaces['camunda']}}}assignee", task['assignee'])
                
        elif task_type == 'service':
            elem = ET.SubElement(process, 'serviceTask', {
                'id': task['id'],
                'name': task['name']
            })
            # Set implementation
            impl = task.get('implementation', 'external')
            if impl == 'external':
                elem.set(f"{{{self.namespaces['camunda']}}}type", "external")
                elem.set(f"{{{self.namespaces['camunda']}}}topic", task['id'])
    
    def _add_gateway(self, process: ET.Element, gateway: Dict):
        """Add gateway to process"""
        gateway_type = gateway.get('type', 'exclusive')
        
        if gateway_type == 'exclusive':
            elem = ET.SubElement(process, 'exclusiveGateway', {
                'id': gateway['id'],
                'name': gateway.get('condition', '')
            })
        elif gateway_type == 'parallel':
            elem = ET.SubElement(process, 'parallelGateway', {
                'id': gateway['id']
            })
    
    def _add_flow(self, process: ET.Element, flow: Dict):
        """Add sequence flow to process"""
        flow_elem = ET.SubElement(process, 'sequenceFlow', {
            'id': f"flow_{flow['from']}_{flow['to']}",
            'sourceRef': flow['from'],
            'targetRef': flow['to']
        })
        
        # Add condition if present
        if flow.get('condition'):
            condition = ET.SubElement(flow_elem, 'conditionExpression', {
                'xsi:type': 'tFormalExpression'
            })
            condition.text = flow['condition']
    
    def _add_diagram(self, definitions: ET.Element, process_id: str, workflow: Dict):
        """Add basic BPMN diagram layout"""
        diagram = ET.SubElement(definitions, f"{{{self.namespaces['bpmndi']}}}BPMNDiagram", {
            'id': f'Diagram_{process_id}'
        })
        
        plane = ET.SubElement(diagram, f"{{{self.namespaces['bpmndi']}}}BPMNPlane", {
            'id': f'Plane_{process_id}',
            'bpmnElement': process_id
        })
        
        # Simple left-to-right layout
        x_pos = 100
        y_pos = 100
        spacing = 150
        
        # Position elements (simplified)
        all_elements = (
            workflow.get('events', []) + 
            workflow.get('tasks', []) + 
            workflow.get('gateways', [])
        )
        
        for element in all_elements:
            shape = ET.SubElement(plane, f"{{{self.namespaces['bpmndi']}}}BPMNShape", {
                'id': f"Shape_{element['id']}",
                'bpmnElement': element['id']
            })
            
            bounds = ET.SubElement(shape, f"{{{self.namespaces['dc']}}}Bounds", {
                'x': str(x_pos),
                'y': str(y_pos),
                'width': '100',
                'height': '80'
            })
            
            x_pos += spacing
    
    def _generate_process_name(self, description: str) -> str:
        """Generate process name from description"""
        # Take first few words
        words = description.split()[:5]
        name = ' '.join(words)
        
        # Clean up
        name = re.sub(r'[^\w\s]', '', name)
        return name.title() + ' Process'
