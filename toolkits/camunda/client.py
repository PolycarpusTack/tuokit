"""
Camunda REST API Client - Lightweight wrapper
"""

import requests
from typing import Dict, List, Optional, Any
from datetime import datetime
import json


class CamundaClient:
    """Minimal Camunda REST API client"""
    
    def __init__(self, base_url: str, username: str = None, password: str = None):
        self.base_url = base_url.rstrip('/')
        self.session = requests.Session()
        
        if username and password:
            self.session.auth = (username, password)
            
    def _request(self, method: str, endpoint: str, **kwargs) -> Any:
        """Make API request with error handling"""
        url = f"{self.base_url}/{endpoint}"
        
        try:
            response = self.session.request(method, url, **kwargs)
            response.raise_for_status()
            
            if response.content:
                return response.json()
            return None
            
        except requests.exceptions.RequestException as e:
            raise Exception(f"Camunda API error: {str(e)}")
    
    # Process Definition APIs
    def get_process_definitions(self) -> List[Dict]:
        """Get all deployed process definitions"""
        return self._request('GET', 'process-definition')
    
    def get_process_definition(self, key: str) -> Dict:
        """Get specific process definition by key"""
        return self._request('GET', f'process-definition/key/{key}')
    
    def get_process_diagram(self, id: str) -> str:
        """Get BPMN XML for a process"""
        return self._request('GET', f'process-definition/{id}/xml')
    
    # Process Instance APIs
    def get_process_instances(self, **filters) -> List[Dict]:
        """Get running process instances"""
        return self._request('GET', 'process-instance', params=filters)
    
    def start_process(self, key: str, variables: Dict = None) -> Dict:
        """Start a new process instance"""
        data = {"variables": self._format_variables(variables)} if variables else {}
        return self._request('POST', f'process-definition/key/{key}/start', json=data)
    
    def delete_process_instance(self, id: str, reason: str = "Deleted via API") -> None:
        """Delete a process instance"""
        self._request('DELETE', f'process-instance/{id}', json={"skipCustomListeners": True})
    
    # Task APIs
    def get_tasks(self, **filters) -> List[Dict]:
        """Get tasks (user tasks waiting for completion)"""
        return self._request('GET', 'task', params=filters)
    
    def complete_task(self, task_id: str, variables: Dict = None) -> None:
        """Complete a user task"""
        data = {"variables": self._format_variables(variables)} if variables else {}
        self._request('POST', f'task/{task_id}/complete', json=data)
    
    # Incident APIs
    def get_incidents(self) -> List[Dict]:
        """Get all incidents (errors in processes)"""
        return self._request('GET', 'incident')
    
    def get_process_incidents(self, process_instance_id: str) -> List[Dict]:
        """Get incidents for specific process"""
        return self._request('GET', 'incident', params={'processInstanceId': process_instance_id})
    
    # History APIs
    def get_history_processes(self, **filters) -> List[Dict]:
        """Get historical process instances"""
        return self._request('GET', 'history/process-instance', params=filters)
    
    def get_history_activities(self, process_instance_id: str) -> List[Dict]:
        """Get activity history for a process"""
        return self._request('GET', 'history/activity-instance', 
                           params={'processInstanceId': process_instance_id})
    
    # External Task APIs (for external workers)
    def fetch_and_lock_tasks(self, worker_id: str, topic: str, max_tasks: int = 1) -> List[Dict]:
        """Fetch external tasks for processing"""
        data = {
            "workerId": worker_id,
            "maxTasks": max_tasks,
            "topics": [{"topicName": topic, "lockDuration": 60000}]
        }
        return self._request('POST', 'external-task/fetchAndLock', json=data)
    
    def complete_external_task(self, task_id: str, worker_id: str, variables: Dict = None) -> None:
        """Complete an external task"""
        data = {"workerId": worker_id}
        if variables:
            data["variables"] = self._format_variables(variables)
        self._request('POST', f'external-task/{task_id}/complete', json=data)
    
    # Deployment APIs
    def deploy_bpmn(self, name: str, bpmn_xml: str) -> Dict:
        """Deploy a BPMN process definition"""
        files = {
            'deployment-name': (None, name),
            'data': (f'{name}.bpmn', bpmn_xml, 'text/xml')
        }
        return self._request('POST', 'deployment/create', files=files)
    
    # Utility methods
    def _format_variables(self, variables: Dict) -> Dict:
        """Format variables for Camunda API"""
        formatted = {}
        for key, value in variables.items():
            if isinstance(value, bool):
                formatted[key] = {"value": value, "type": "Boolean"}
            elif isinstance(value, int):
                formatted[key] = {"value": value, "type": "Integer"}
            elif isinstance(value, float):
                formatted[key] = {"value": value, "type": "Double"}
            else:
                formatted[key] = {"value": str(value), "type": "String"}
        return formatted
    
    def health_check(self) -> bool:
        """Check if Camunda is accessible"""
        try:
            self._request('GET', 'engine')
            return True
        except:
            return False
