"""
Pipeline Executor - Manages different execution pipelines
"""

from typing import Dict, List, Any, Optional
import asyncio
import concurrent.futures
from datetime import datetime
import json

from .core import AgentState
from .registry import get_agent
from utils import capture_knowledge


class PipelineExecutor:
    """Executes different types of pipelines"""
    
    def __init__(self):
        self.pipelines = {
            'simple': self._run_simple_pipeline,
            'advanced': self._run_advanced_pipeline,
            'educational': self._run_educational_pipeline,
            'research': self._run_research_pipeline
        }
        self.max_workers = 4
        
    def execute(self, pipeline_type: str, steps: List[Dict], 
                context: Optional[Dict] = None) -> Dict[str, Any]:
        """Execute a pipeline of given type"""
        
        if pipeline_type not in self.pipelines:
            return {
                'success': False,
                'error': f'Unknown pipeline type: {pipeline_type}'
            }
        
        start_time = datetime.now()
        
        try:
            result = self.pipelines[pipeline_type](steps, context)
            
            # Add metadata
            result['pipeline_type'] = pipeline_type
            result['duration'] = (datetime.now() - start_time).total_seconds()
            result['step_count'] = len(steps)
            
            # Capture to knowledge base
            capture_knowledge(
                tool=f"pipeline_{pipeline_type}",
                prompt=json.dumps(steps),
                response=json.dumps(result),
                metadata={'context': context}
            )
            
            return result
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e),
                'pipeline_type': pipeline_type
            }
    
    def _run_simple_pipeline(self, steps: List[Dict], 
                           context: Optional[Dict] = None) -> Dict[str, Any]:
        """Simple sequential pipeline"""
        results = []
        
        for step in steps:
            tool = step.get('tool')
            params = step.get('params', {})
            
            # Find agent that has this tool
            agent = self._find_agent_for_tool(tool)
            if not agent:
                results.append({
                    'step': step,
                    'error': f'No agent found for tool: {tool}',
                    'success': False
                })
                continue
            
            # Execute tool
            try:
                result = agent.execute_tool(tool, params)
                results.append({
                    'step': step,
                    'result': result,
                    'success': True
                })
            except Exception as e:
                results.append({
                    'step': step,
                    'error': str(e),
                    'success': False
                })
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results
        }
    
    def _run_advanced_pipeline(self, steps: List[Dict], 
                             context: Optional[Dict] = None) -> Dict[str, Any]:
        """Advanced pipeline with retry logic and parallel execution"""
        results = []
        grouped_steps = self._group_parallel_steps(steps)
        
        for group in grouped_steps:
            if len(group) == 1:
                # Single step - execute with retry
                result = self._execute_with_retry(group[0], context)
                results.append(result)
            else:
                # Multiple steps - execute in parallel
                with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_workers) as executor:
                    futures = []
                    
                    for step in group:
                        future = executor.submit(self._execute_with_retry, step, context)
                        futures.append((step, future))
                    
                    for step, future in futures:
                        try:
                            result = future.result(timeout=30)
                            results.append(result)
                        except Exception as e:
                            results.append({
                                'step': step,
                                'error': str(e),
                                'success': False
                            })
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'parallel_groups': len(grouped_steps)
        }
    
    def _run_educational_pipeline(self, steps: List[Dict], 
                                context: Optional[Dict] = None) -> Dict[str, Any]:
        """Educational pipeline with explanations"""
        results = []
        learning_points = []
        
        for step in steps:
            # Execute step
            tool = step.get('tool')
            params = step.get('params', {})
            
            agent = self._find_agent_for_tool(tool)
            if not agent:
                continue
            
            try:
                result = agent.execute_tool(tool, params)
                
                # Generate explanation
                explanation = self._generate_explanation(step, result)
                
                results.append({
                    'step': step,
                    'result': result,
                    'explanation': explanation,
                    'success': True
                })
                
                learning_points.append(explanation)
                
            except Exception as e:
                results.append({
                    'step': step,
                    'error': str(e),
                    'success': False
                })
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'learning_summary': '\n\n'.join(learning_points)
        }
    
    def _run_research_pipeline(self, steps: List[Dict], 
                             context: Optional[Dict] = None) -> Dict[str, Any]:
        """Research pipeline with iterative refinement"""
        results = []
        research_context = context or {}
        research_context['findings'] = []
        
        for i, step in enumerate(steps):
            # Adapt step based on previous findings
            adapted_step = self._adapt_research_step(step, research_context)
            
            # Execute
            tool = adapted_step.get('tool')
            params = adapted_step.get('params', {})
            
            agent = self._find_agent_for_tool(tool)
            if not agent:
                continue
            
            try:
                result = agent.execute_tool(tool, params)
                
                # Analyze result
                analysis = self._analyze_research_result(result, adapted_step)
                
                results.append({
                    'step': adapted_step,
                    'result': result,
                    'analysis': analysis,
                    'success': True
                })
                
                # Update research context
                research_context['findings'].append({
                    'step': i,
                    'finding': analysis
                })
                
            except Exception as e:
                results.append({
                    'step': adapted_step,
                    'error': str(e),
                    'success': False
                })
        
        # Generate research summary
        summary = self._generate_research_summary(results, research_context)
        
        return {
            'success': all(r.get('success', False) for r in results),
            'results': results,
            'research_summary': summary,
            'iterations': len(results)
        }
    
    # Helper methods
    def _find_agent_for_tool(self, tool: str) -> Optional[Any]:
        """Find agent that has the specified tool"""
        from .registry import AGENT_REGISTRY
        
        for agent in AGENT_REGISTRY.values():
            if tool in agent.tools:
                return agent
        return None
    
    def _group_parallel_steps(self, steps: List[Dict]) -> List[List[Dict]]:
        """Group steps that can be executed in parallel"""
        groups = []
        current_group = []
        
        for step in steps:
            if step.get('parallel', True):
                current_group.append(step)
            else:
                if current_group:
                    groups.append(current_group)
                    current_group = []
                groups.append([step])
        
        if current_group:
            groups.append(current_group)
        
        return groups
    
    def _execute_with_retry(self, step: Dict, context: Optional[Dict] = None, 
                          max_retries: int = 3) -> Dict[str, Any]:
        """Execute step with retry logic"""
        tool = step.get('tool')
        params = step.get('params', {})
        
        agent = self._find_agent_for_tool(tool)
        if not agent:
            return {
                'step': step,
                'error': f'No agent found for tool: {tool}',
                'success': False
            }
        
        for attempt in range(max_retries):
            try:
                result = agent.execute_tool(tool, params)
                return {
                    'step': step,
                    'result': result,
                    'success': True,
                    'attempts': attempt + 1
                }
            except Exception as e:
                if attempt == max_retries - 1:
                    return {
                        'step': step,
                        'error': str(e),
                        'success': False,
                        'attempts': attempt + 1
                    }
                # Wait before retry
                import time
                time.sleep(1 * (attempt + 1))
    
    def _generate_explanation(self, step: Dict, result: Any) -> str:
        """Generate educational explanation for step"""
        return f"Step '{step.get('tool')}' processed the input and produced: {str(result)[:100]}..."
    
    def _adapt_research_step(self, step: Dict, research_context: Dict) -> Dict:
        """Adapt research step based on context"""
        adapted = step.copy()
        
        # Add previous findings to parameters
        if research_context.get('findings'):
            adapted['params'] = adapted.get('params', {})
            adapted['params']['previous_findings'] = research_context['findings'][-3:]
        
        return adapted
    
    def _analyze_research_result(self, result: Any, step: Dict) -> str:
        """Analyze research result for insights"""
        return f"Analysis of {step.get('tool')}: {str(result)[:200]}..."
    
    def _generate_research_summary(self, results: List[Dict], 
                                 context: Dict) -> str:
        """Generate comprehensive research summary"""
        findings = context.get('findings', [])
        
        summary_parts = [
            f"Research completed with {len(results)} steps.",
            f"Key findings: {len(findings)}",
            "\nMain insights:"
        ]
        
        for i, finding in enumerate(findings[:5], 1):
            summary_parts.append(f"{i}. {finding['finding'][:100]}...")
        
        return '\n'.join(summary_parts)
