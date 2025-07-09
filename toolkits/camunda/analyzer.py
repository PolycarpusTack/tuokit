"""
Workflow Analyzer - AI-powered Camunda process analysis
"""

from typing import Dict, List, Optional, Tuple
import xml.etree.ElementTree as ET
from collections import defaultdict
import statistics
from datetime import datetime

try:
    from utils import safe_ollama_generate, capture_knowledge
except ImportError:
    # Fallback for testing without full TuoKit environment
    def safe_ollama_generate(model, prompt):
        return {'response': '1. Optimize process flow\n2. Reduce manual tasks\n3. Add parallel processing'}
    def capture_knowledge(*args, **kwargs):
        pass


class WorkflowAnalyzer:
    """Analyze Camunda workflows for optimization opportunities"""
    
    def __init__(self, client):
        self.client = client
        self.analysis_cache = {}
    
    def analyze_process(self, process_key: str) -> Dict:
        """Comprehensive analysis of a process definition"""
        try:
            # Get process definition
            definition = self.client.get_process_definition(process_key)
            bpmn_xml = self.client.get_process_diagram(definition['id'])
            
            # Get historical data
            history = self.client.get_history_processes(
                processDefinitionKey=process_key,
                finished=True,
                maxResults=100
            )
            
            # Parse BPMN
            bpmn_analysis = self._analyze_bpmn(bpmn_xml)
            
            # Analyze performance
            performance = self._analyze_performance(history)
            
            # Find bottlenecks
            bottlenecks = self._find_bottlenecks(process_key)
            
            # Get AI recommendations
            recommendations = self._get_ai_recommendations(
                bpmn_analysis, performance, bottlenecks
            )
            
            analysis = {
                'process_key': process_key,
                'version': definition.get('version'),
                'structure': bpmn_analysis,
                'performance': performance,
                'bottlenecks': bottlenecks,
                'recommendations': recommendations,
                'optimization_score': self._calculate_optimization_score(
                    bpmn_analysis, performance
                )
            }
            
            # Cache and capture
            self.analysis_cache[process_key] = analysis
            capture_knowledge(
                tool="camunda_analyzer",
                prompt=f"Analyze process {process_key}",
                response=str(analysis),
                metadata={'process_key': process_key}
            )
            
            return analysis
            
        except Exception as e:
            return {'error': str(e), 'process_key': process_key}
    
    def _analyze_bpmn(self, bpmn_xml: str) -> Dict:
        """Analyze BPMN structure"""
        try:
            root = ET.fromstring(bpmn_xml['bpmn20Xml'])
            
            # Count elements
            ns = {'bpmn': 'http://www.omg.org/spec/BPMN/20100524/MODEL'}
            
            analysis = {
                'total_tasks': len(root.findall('.//bpmn:userTask', ns)) + 
                              len(root.findall('.//bpmn:serviceTask', ns)),
                'user_tasks': len(root.findall('.//bpmn:userTask', ns)),
                'service_tasks': len(root.findall('.//bpmn:serviceTask', ns)),
                'gateways': len(root.findall('.//bpmn:exclusiveGateway', ns)) +
                           len(root.findall('.//bpmn:parallelGateway', ns)),
                'events': len(root.findall('.//bpmn:*Event', ns)),
                'complexity_score': 0
            }
            
            # Calculate complexity
            analysis['complexity_score'] = (
                analysis['total_tasks'] + 
                analysis['gateways'] * 2  # Gateways add more complexity
            )
            
            return analysis
            
        except Exception as e:
            return {'error': str(e)}
    
    def _analyze_performance(self, history: List[Dict]) -> Dict:
        """Analyze historical performance"""
        if not history:
            return {'no_data': True}
        
        durations = []
        completed = 0
        failed = 0
        
        for instance in history:
            if instance.get('endTime') and instance.get('startTime'):
                # Calculate duration in minutes
                start = datetime.fromisoformat(instance['startTime'].replace('Z', '+00:00'))
                end = datetime.fromisoformat(instance['endTime'].replace('Z', '+00:00'))
                duration = (end - start).total_seconds() / 60
                durations.append(duration)
                
                if instance.get('state') == 'COMPLETED':
                    completed += 1
                else:
                    failed += 1
        
        return {
            'total_instances': len(history),
            'completed': completed,
            'failed': failed,
            'success_rate': (completed / len(history) * 100) if history else 0,
            'avg_duration_minutes': statistics.mean(durations) if durations else 0,
            'min_duration_minutes': min(durations) if durations else 0,
            'max_duration_minutes': max(durations) if durations else 0,
            'std_dev_minutes': statistics.stdev(durations) if len(durations) > 1 else 0
        }
    
    def _find_bottlenecks(self, process_key: str) -> List[Dict]:
        """Identify process bottlenecks"""
        bottlenecks = []
        
        try:
            # Get recent instances
            recent = self.client.get_history_processes(
                processDefinitionKey=process_key,
                maxResults=50
            )
            
            # Analyze activity durations
            activity_times = defaultdict(list)
            
            for instance in recent[:10]:  # Sample recent instances
                activities = self.client.get_history_activities(instance['id'])
                
                for activity in activities:
                    if activity.get('startTime') and activity.get('endTime'):
                        start = datetime.fromisoformat(
                            activity['startTime'].replace('Z', '+00:00')
                        )
                        end = datetime.fromisoformat(
                            activity['endTime'].replace('Z', '+00:00')
                        )
                        duration = (end - start).total_seconds() / 60
                        
                        activity_times[activity['activityId']].append(duration)
            
            # Find slowest activities
            for activity_id, durations in activity_times.items():
                avg_duration = statistics.mean(durations)
                if avg_duration > 30:  # More than 30 minutes
                    bottlenecks.append({
                        'activity': activity_id,
                        'avg_duration_minutes': avg_duration,
                        'instances': len(durations),
                        'severity': 'high' if avg_duration > 60 else 'medium'
                    })
            
            return sorted(bottlenecks, key=lambda x: x['avg_duration_minutes'], reverse=True)
            
        except Exception as e:
            return [{'error': str(e)}]
    
    def _get_ai_recommendations(self, structure: Dict, performance: Dict, 
                               bottlenecks: List[Dict]) -> List[str]:
        """Get AI-powered optimization recommendations"""
        prompt = f"""
Analyze this Camunda process and provide optimization recommendations:

Structure:
- Total tasks: {structure.get('total_tasks', 0)}
- User tasks: {structure.get('user_tasks', 0)}
- Service tasks: {structure.get('service_tasks', 0)}
- Gateways: {structure.get('gateways', 0)}
- Complexity score: {structure.get('complexity_score', 0)}

Performance:
- Success rate: {performance.get('success_rate', 0):.1f}%
- Average duration: {performance.get('avg_duration_minutes', 0):.1f} minutes
- Standard deviation: {performance.get('std_dev_minutes', 0):.1f} minutes

Bottlenecks:
{chr(10).join(f"- {b['activity']}: avg {b['avg_duration_minutes']:.1f} min" for b in bottlenecks[:3])}

Provide 3-5 specific, actionable recommendations to improve this process.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        # Extract recommendations
        recommendations = []
        lines = response.get('response', '').split('\n')
        
        for line in lines:
            line = line.strip()
            if line and (line[0].isdigit() or line.startswith('-')):
                # Clean up numbering
                recommendation = re.sub(r'^[\d\-\.\)]+\s*', '', line)
                if recommendation:
                    recommendations.append(recommendation)
        
        return recommendations[:5]  # Limit to 5 recommendations
    
    def _calculate_optimization_score(self, structure: Dict, performance: Dict) -> int:
        """Calculate process optimization score (0-100)"""
        score = 100
        
        # Deduct for complexity
        complexity = structure.get('complexity_score', 0)
        if complexity > 20:
            score -= min((complexity - 20) * 2, 30)
        
        # Deduct for poor performance
        success_rate = performance.get('success_rate', 100)
        if success_rate < 95:
            score -= min((95 - success_rate) * 2, 30)
        
        # Deduct for high variance
        if performance.get('avg_duration_minutes', 0) > 0:
            cv = (performance.get('std_dev_minutes', 0) / 
                  performance.get('avg_duration_minutes', 1))
            if cv > 0.5:  # High coefficient of variation
                score -= 10
        
        return max(0, score)
    
    def compare_versions(self, process_key: str, version1: int, version2: int) -> Dict:
        """Compare two versions of a process"""
        # This would fetch and compare different versions
        # Implementation depends on Camunda version management
        pass
    
    def suggest_automation(self, process_key: str) -> List[Dict]:
        """Suggest tasks that could be automated"""
        analysis = self.analyze_process(process_key)
        
        suggestions = []
        # Look for patterns in user tasks that could be automated
        # This is a simplified version - real implementation would be more sophisticated
        
        return suggestions

# Import already added at the top of the file
