"""
Process Monitor - Real-time Camunda monitoring with AI insights
"""

from typing import Dict, List, Optional, Tuple
from datetime import datetime, timedelta
import json
from collections import defaultdict

try:
    from utils import safe_ollama_generate, capture_knowledge
except ImportError:
    # Fallback for testing without full TuoKit environment
    def safe_ollama_generate(model, prompt):
        return {'response': 'Mock AI response for testing'}
    def capture_knowledge(*args, **kwargs):
        pass


class ProcessMonitor:
    """Monitor and analyze running Camunda processes"""
    
    def __init__(self, client):
        self.client = client
        self.alert_thresholds = {
            'stuck_duration': timedelta(hours=1),
            'incident_count': 3,
            'retry_count': 5
        }
    
    def get_dashboard_data(self) -> Dict:
        """Get comprehensive monitoring dashboard data"""
        try:
            # Gather all monitoring data
            processes = self.client.get_process_instances()
            incidents = self.client.get_incidents()
            tasks = self.client.get_tasks()
            definitions = self.client.get_process_definitions()
            
            # Calculate metrics
            metrics = {
                'total_running': len(processes),
                'total_incidents': len(incidents),
                'pending_tasks': len(tasks),
                'deployed_processes': len(definitions),
                'processes_by_definition': self._group_by_definition(processes),
                'incident_summary': self._analyze_incidents(incidents),
                'task_summary': self._analyze_tasks(tasks),
                'health_score': self._calculate_health_score(processes, incidents)
            }
            
            # Identify issues
            metrics['alerts'] = self._detect_issues(processes, incidents, tasks)
            
            return metrics
            
        except Exception as e:
            return {'error': str(e)}
    
    def get_running_processes(self, include_history: bool = False) -> List[Dict]:
        """Get all running processes with enriched data"""
        processes = self.client.get_process_instances()
        
        # Enrich with additional data
        for process in processes:
            # Add incidents if any
            process['incidents'] = self.client.get_process_incidents(process['id'])
            
            # Add current activities
            if include_history:
                activities = self.client.get_history_activities(process['id'])
                process['current_activities'] = [
                    a for a in activities if not a.get('endTime')
                ]
            
            # Calculate duration
            start_time = datetime.fromisoformat(process['startTime'].replace('Z', '+00:00'))
            process['duration'] = str(datetime.utcnow() - start_time.replace(tzinfo=None))
            
        return processes
    
    def find_stuck_processes(self) -> List[Dict]:
        """Identify processes that appear stuck"""
        stuck = []
        processes = self.get_running_processes(include_history=True)
        
        for process in processes:
            # Check if stuck based on duration
            start_time = datetime.fromisoformat(process['startTime'].replace('Z', '+00:00'))
            duration = datetime.utcnow() - start_time.replace(tzinfo=None)
            
            if duration > self.alert_thresholds['stuck_duration']:
                # Check if there's activity
                current_activities = process.get('current_activities', [])
                if current_activities:
                    # Check how long current activity has been running
                    for activity in current_activities:
                        act_start = datetime.fromisoformat(
                            activity['startTime'].replace('Z', '+00:00')
                        )
                        act_duration = datetime.utcnow() - act_start.replace(tzinfo=None)
                        
                        if act_duration > self.alert_thresholds['stuck_duration']:
                            stuck.append({
                                'process': process,
                                'stuck_activity': activity,
                                'duration': str(act_duration),
                                'recommendation': self._get_ai_recommendation(process, activity)
                            })
        
        return stuck
    
    def _get_ai_recommendation(self, process: Dict, activity: Dict = None) -> str:
        """Get AI recommendation for stuck process"""
        prompt = f"""
Analyze this stuck Camunda process and suggest resolution:

Process: {process.get('processDefinitionKey')}
Instance ID: {process.get('id')}
Duration: {process.get('duration')}
Incidents: {len(process.get('incidents', []))}

{f"Stuck at activity: {activity.get('activityId')}" if activity else ""}

Provide specific actionable recommendations.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response.get('response', 'Unable to generate recommendation')
    
    def _group_by_definition(self, processes: List[Dict]) -> Dict[str, int]:
        """Group processes by definition key"""
        groups = defaultdict(int)
        for process in processes:
            groups[process.get('processDefinitionKey', 'Unknown')] += 1
        return dict(groups)
    
    def _analyze_incidents(self, incidents: List[Dict]) -> Dict:
        """Analyze incidents for patterns"""
        if not incidents:
            return {'total': 0, 'by_type': {}, 'by_process': {}}
        
        by_type = defaultdict(int)
        by_process = defaultdict(int)
        
        for incident in incidents:
            by_type[incident.get('incidentType', 'Unknown')] += 1
            by_process[incident.get('processDefinitionKey', 'Unknown')] += 1
        
        return {
            'total': len(incidents),
            'by_type': dict(by_type),
            'by_process': dict(by_process),
            'most_common': max(by_type.items(), key=lambda x: x[1])[0] if by_type else None
        }
    
    def _analyze_tasks(self, tasks: List[Dict]) -> Dict:
        """Analyze pending tasks"""
        if not tasks:
            return {'total': 0, 'by_assignee': {}, 'overdue': 0}
        
        by_assignee = defaultdict(int)
        overdue = 0
        
        for task in tasks:
            assignee = task.get('assignee', 'Unassigned')
            by_assignee[assignee] += 1
            
            # Check if overdue
            if task.get('due'):
                due_date = datetime.fromisoformat(task['due'].replace('Z', '+00:00'))
                if datetime.utcnow() > due_date.replace(tzinfo=None):
                    overdue += 1
        
        return {
            'total': len(tasks),
            'by_assignee': dict(by_assignee),
            'overdue': overdue,
            'unassigned': by_assignee.get('Unassigned', 0)
        }
    
    def _calculate_health_score(self, processes: List[Dict], incidents: List[Dict]) -> int:
        """Calculate overall system health score (0-100)"""
        score = 100
        
        # Deduct for incidents
        score -= min(len(incidents) * 5, 30)
        
        # Deduct for stuck processes
        stuck = self.find_stuck_processes()
        score -= min(len(stuck) * 10, 40)
        
        # Ensure score stays in bounds
        return max(0, score)
    
    def _detect_issues(self, processes: List[Dict], incidents: List[Dict], 
                      tasks: List[Dict]) -> List[Dict]:
        """Detect and categorize issues"""
        alerts = []
        
        # High incident count
        if len(incidents) >= self.alert_thresholds['incident_count']:
            alerts.append({
                'type': 'HIGH_INCIDENTS',
                'severity': 'high',
                'message': f'{len(incidents)} incidents detected',
                'recommendation': 'Review incident logs and common failure points'
            })
        
        # Stuck processes
        stuck = self.find_stuck_processes()
        if stuck:
            alerts.append({
                'type': 'STUCK_PROCESSES',
                'severity': 'medium',
                'message': f'{len(stuck)} processes appear stuck',
                'details': stuck
            })
        
        # Unassigned tasks
        task_summary = self._analyze_tasks(tasks)
        if task_summary.get('unassigned', 0) > 5:
            alerts.append({
                'type': 'UNASSIGNED_TASKS',
                'severity': 'low',
                'message': f"{task_summary['unassigned']} tasks are unassigned",
                'recommendation': 'Assign tasks to appropriate users'
            })
        
        return alerts
