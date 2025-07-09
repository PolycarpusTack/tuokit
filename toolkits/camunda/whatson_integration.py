"""
WHATS'ON Specific Integration Utilities for Camunda
"""

import json
import requests
from typing import Dict, List, Optional
from datetime import datetime
import xml.etree.ElementTree as ET

from .manager import CamundaManager, CamundaWorkflowBuilder
from utils import safe_ollama_generate, capture_knowledge


class WhatsOnCamundaIntegration:
    """Specialized integration patterns for WHATS'ON + Camunda"""
    
    def __init__(self, camunda_url: str, whatson_api_url: str = None):
        self.camunda = CamundaManager(camunda_url)
        self.whatson_api_url = whatson_api_url
        self.common_patterns = self._load_common_patterns()
    
    def _load_common_patterns(self) -> Dict:
        """Load common WHATS'ON integration patterns"""
        return {
            'entity_sync': {
                'products': ['id', 'name', 'price', 'status', 'category'],
                'customers': ['id', 'email', 'name', 'tier', 'credit_limit'],
                'orders': ['id', 'customer_id', 'total', 'status', 'items']
            },
            'event_types': [
                'order.created', 'order.updated', 'order.cancelled',
                'customer.registered', 'customer.updated',
                'product.created', 'product.updated', 'product.deleted'
            ],
            'approval_types': [
                'credit_approval', 'discount_approval', 'return_approval',
                'vendor_onboarding', 'contract_approval'
            ]
        }
    
    def generate_entity_sync_workflow(self, entity_type: str, 
                                    target_system: str,
                                    sync_mode: str = 'incremental') -> str:
        """Generate workflow for syncing WHATS'ON entities"""
        
        builder = CamundaWorkflowBuilder()
        
        # Start event
        start_id = builder.add_start_event(f"Sync {entity_type}")
        
        # Fetch data from WHATS'ON
        fetch_id = builder.add_service_task(
            name=f"Fetch {entity_type} from WHATS'ON",
            topic="whatson_fetch",
            prev_element=start_id
        )
        
        # Add sub-process for batch processing
        batch_process = f'''
    <bpmn:subProcess id="BatchProcess_{entity_type}" name="Process {entity_type} Batch">
      <bpmn:incoming>Flow_{fetch_id}</bpmn:incoming>
      <bpmn:outgoing>Flow_to_end</bpmn:outgoing>
      
      <bpmn:multiInstanceLoopCharacteristics isSequential="false">
        <bpmn:loopCardinality>${{{entity_type}_count}}</bpmn:loopCardinality>
      </bpmn:multiInstanceLoopCharacteristics>
      
      <bpmn:startEvent id="BatchStart">
        <bpmn:outgoing>Flow_to_validate</bpmn:outgoing>
      </bpmn:startEvent>
      
      <bpmn:serviceTask id="Validate_{entity_type}" name="Validate Data"
                        camunda:type="external" camunda:topic="validate_entity">
        <bpmn:incoming>Flow_to_validate</bpmn:incoming>
        <bpmn:outgoing>Flow_to_map</bpmn:outgoing>
      </bpmn:serviceTask>
      
      <bpmn:serviceTask id="Map_{entity_type}" name="Map to {target_system}"
                        camunda:type="external" camunda:topic="map_entity">
        <bpmn:incoming>Flow_to_map</bpmn:incoming>
        <bpmn:outgoing>Flow_to_sync</bpmn:outgoing>
      </bpmn:serviceTask>
      
      <bpmn:serviceTask id="Sync_{entity_type}" name="Sync to {target_system}"
                        camunda:type="external" camunda:topic="sync_entity">
        <bpmn:incoming>Flow_to_sync</bpmn:incoming>
        <bpmn:outgoing>Flow_to_batch_end</bpmn:outgoing>
      </bpmn:serviceTask>
      
      <bpmn:endEvent id="BatchEnd">
        <bpmn:incoming>Flow_to_batch_end</bpmn:incoming>
      </bpmn:endEvent>
      
      <!-- Error handling -->
      <bpmn:boundaryEvent id="SyncError" attachedToRef="Sync_{entity_type}">
        <bpmn:errorEventDefinition errorRef="SYNC_ERROR" />
        <bpmn:outgoing>Flow_to_error_handler</bpmn:outgoing>
      </bpmn:boundaryEvent>
      
      <bpmn:serviceTask id="HandleError" name="Handle Sync Error"
                        camunda:type="external" camunda:topic="handle_error">
        <bpmn:incoming>Flow_to_error_handler</bpmn:incoming>
        <bpmn:outgoing>Flow_to_batch_end</bpmn:outgoing>
      </bpmn:serviceTask>
    </bpmn:subProcess>'''
        
        # Update last sync timestamp
        update_id = builder.add_service_task(
            name="Update Sync Timestamp",
            topic="update_timestamp",
            prev_element="BatchProcess_" + entity_type
        )
        
        # End event
        builder.add_end_event("Sync Complete", update_id)
        
        # Build BPMN with custom subprocess
        base_bpmn = builder.build(
            f"Sync_{entity_type}_to_{target_system}",
            f"{entity_type} Synchronization to {target_system}"
        )
        
        # Insert subprocess into BPMN
        base_bpmn = base_bpmn.replace(
            f'<bpmn:sequenceFlow id="Flow_{fetch_id}"',
            batch_process + f'\n    <bpmn:sequenceFlow id="Flow_{fetch_id}"'
        )
        
        return self._add_whatson_config(base_bpmn, entity_type, target_system)
    
    def _add_whatson_config(self, bpmn_xml: str, 
                           entity_type: str, 
                           target_system: str) -> str:
        """Add WHATS'ON specific configuration to BPMN"""
        
        # Parse and enhance XML
        root = ET.fromstring(bpmn_xml)
        
        # Add process variables
        process = root.find('.//{http://www.omg.org/spec/BPMN/20100524/MODEL}process')
        if process:
            extension = ET.SubElement(process, 
                '{http://camunda.org/schema/1.0/bpmn}extensionElements')
            properties = ET.SubElement(extension, 
                '{http://camunda.org/schema/1.0/bpmn}properties')
            
            # Add configuration properties
            configs = [
                ('whatson_api_url', '${WHATSON_API_URL}'),
                ('whatson_api_token', '${WHATSON_API_TOKEN}'),
                ('target_system', target_system),
                ('entity_type', entity_type),
                ('batch_size', '100'),
                ('retry_attempts', '3'),
                ('retry_delay', '5000')
            ]
            
            for name, value in configs:
                prop = ET.SubElement(properties, 
                    '{http://camunda.org/schema/1.0/bpmn}property')
                prop.set('name', name)
                prop.set('value', value)
        
        return ET.tostring(root, encoding='unicode')
    
    def analyze_integration_health(self, process_definition_key: str) -> Dict:
        """Analyze health of WHATS'ON integration processes"""
        
        # Get recent instances
        instances = self.camunda.get_process_instances(active_only=False)
        relevant_instances = [
            i for i in instances 
            if i.get('processDefinitionKey') == process_definition_key
        ]
        
        # Calculate metrics
        total = len(relevant_instances)
        active = len([i for i in relevant_instances if not i.get('ended')])
        
        # Get incidents for this process
        all_incidents = self.camunda.get_incidents()
        process_incidents = [
            i for i in all_incidents 
            if i.get('processDefinitionKey') == process_definition_key
        ]
        
        # AI-powered health assessment
        health_prompt = f"""
Analyze the health of this WHATS'ON integration:
- Process: {process_definition_key}
- Total instances: {total}
- Active instances: {active}
- Current incidents: {len(process_incidents)}
- Incident types: {[i.get('incidentType') for i in process_incidents]}

Provide:
1. Health score (0-100)
2. Key issues
3. Optimization recommendations
4. Risk assessment
"""
        
        ai_assessment = safe_ollama_generate(
            'deepseek-r1:1.5b',
            health_prompt
        )
        
        return {
            'process': process_definition_key,
            'metrics': {
                'total_instances': total,
                'active_instances': active,
                'incidents': len(process_incidents),
                'success_rate': ((total - len(process_incidents)) / total * 100) if total > 0 else 0
            },
            'ai_assessment': ai_assessment['response'],
            'incidents': process_incidents[:5],  # Top 5 incidents
            'timestamp': datetime.now().isoformat()
        }
    
    def create_monitoring_rules(self, entity_type: str) -> List[Dict]:
        """Create monitoring rules for WHATS'ON integrations"""
        
        rules = [
            {
                'name': f'{entity_type}_sync_sla',
                'condition': 'duration > 300000',  # 5 minutes
                'action': 'alert',
                'severity': 'warning',
                'message': f'{entity_type} sync exceeding SLA'
            },
            {
                'name': f'{entity_type}_error_rate',
                'condition': 'error_count > 5',
                'action': 'escalate',
                'severity': 'critical',
                'message': f'High error rate in {entity_type} sync'
            },
            {
                'name': f'{entity_type}_stuck_process',
                'condition': 'no_activity > 3600000',  # 1 hour
                'action': 'investigate',
                'severity': 'high',
                'message': f'{entity_type} process appears stuck'
            }
        ]
        
        # Capture monitoring rules
        capture_knowledge(
            tool="whatson_monitoring_rules",
            prompt=f"Create monitoring for {entity_type}",
            response=json.dumps(rules)
        )
        
        return rules
    
    def generate_test_scenarios(self, process_type: str) -> List[Dict]:
        """Generate test scenarios for WHATS'ON integrations"""
        
        prompt = f"""
Generate test scenarios for WHATS'ON {process_type} integration:

Include:
1. Happy path scenario
2. Error handling scenarios
3. Edge cases
4. Performance test cases
5. Security test cases

For each scenario provide:
- Name
- Description
- Input data
- Expected outcome
- Validation steps
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        # Parse response into structured format
        # This is simplified - in production would parse more carefully
        scenarios = [
            {
                'name': 'Happy Path',
                'type': 'functional',
                'description': f'Successful {process_type} execution',
                'input': {'entity_count': 10, 'all_valid': True},
                'expected': 'All entities processed successfully'
            },
            {
                'name': 'Partial Failure',
                'type': 'error_handling',
                'description': 'Some entities fail validation',
                'input': {'entity_count': 10, 'invalid_count': 3},
                'expected': '7 processed, 3 errors logged'
            },
            {
                'name': 'High Volume',
                'type': 'performance',
                'description': 'Process large batch',
                'input': {'entity_count': 10000},
                'expected': 'Complete within SLA'
            }
        ]
        
        return scenarios


# Utility functions
def create_whatson_external_task_handler(topic: str):
    """Create external task handler for WHATS'ON operations"""
    
    def handler(task):
        """Process WHATS'ON related tasks"""
        variables = task.get_variables()
        
        try:
            if topic == "whatson_fetch":
                # Fetch data from WHATS'ON API
                entity_type = variables.get('entity_type')
                last_sync = variables.get('last_sync_timestamp')
                
                # Mock API call - replace with actual
                data = fetch_whatson_data(entity_type, last_sync)
                
                variables['entities'] = data
                variables['entity_count'] = len(data)
                
            elif topic == "validate_entity":
                # Validate entity data
                entity = variables.get('current_entity')
                is_valid = validate_entity(entity)
                
                variables['is_valid'] = is_valid
                
            elif topic == "map_entity":
                # Map WHATS'ON entity to target system format
                entity = variables.get('current_entity')
                target_system = variables.get('target_system')
                
                mapped = map_entity(entity, target_system)
                variables['mapped_entity'] = mapped
                
            return task.complete(variables)
            
        except Exception as e:
            return task.failure(
                error_message=str(e),
                error_details="WHATS'ON integration error",
                max_retries=3,
                retry_timeout=5000
            )
    
    return handler


def fetch_whatson_data(entity_type: str, since_timestamp: str = None) -> List[Dict]:
    """Mock function to fetch WHATS'ON data"""
    # Replace with actual API call
    return [
        {'id': i, 'name': f'{entity_type}_{i}', 'status': 'active'}
        for i in range(10)
    ]


def validate_entity(entity: Dict) -> bool:
    """Validate WHATS'ON entity"""
    required_fields = ['id', 'name', 'status']
    return all(field in entity for field in required_fields)


def map_entity(entity: Dict, target_system: str) -> Dict:
    """Map entity to target system format"""
    # Simplified mapping - would be more complex in production
    mappings = {
        'SAP': {
            'id': 'MaterialNumber',
            'name': 'Description',
            'status': 'Status'
        },
        'Salesforce': {
            'id': 'ExternalId__c',
            'name': 'Name',
            'status': 'Status__c'
        }
    }
    
    mapping = mappings.get(target_system, {})
    return {
        mapping.get(k, k): v 
        for k, v in entity.items()
    }