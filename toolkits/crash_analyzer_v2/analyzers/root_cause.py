"""
Root Cause Analysis Analyzer
Comprehensive single-pass analysis with AI insights
"""
import re
import json
import time
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime

from .base import BaseAnalyzer
from ..config.settings import ANALYSIS_METHODS, MODEL_PREFERENCES
from ..utils.extractors import extract_smart_context, extract_error_chain
from ..utils.diagrams import generate_error_flow_diagram
from utils.json_extractor import extract_json_from_text

class RootCauseAnalyzer(BaseAnalyzer):
    """AI-powered root cause analysis with business impact assessment"""
    
    def __init__(self):
        super().__init__(ANALYSIS_METHODS["root_cause"])
        self.model_config = MODEL_PREFERENCES["root_cause"]
        
    def analyze(self, content: str, filename: str, selected_model: str = None, **kwargs) -> Dict[str, Any]:
        """
        Perform comprehensive root cause analysis
        
        Args:
            content: Crash dump content
            filename: Name of the file
            selected_model: User-selected AI model
            
        Returns:
            Comprehensive analysis with root cause and recommendations
        """
        # Pre-analysis
        metrics = self.pre_analysis(content)
        
        # Get progress callback if provided
        progress_callback = kwargs.get("progress_callback", lambda progress, msg=None: None)
        
        # Smart extraction - up to 100KB of most relevant content
        extracted_content = extract_smart_context(
            content, 
            max_size=self.config["max_content_size"]
        )
        
        results = {
            "executive_summary": {},
            "technical_analysis": {},
            "recommendations": {},
            "prevention_strategy": {},
            "visual_elements": {},
            "confidence": 0.0
        }
        
        # Step 1: Quick algorithmic analysis for context
        progress_callback(0.1, "Running initial analysis")
        quick_analysis = self._quick_analysis(extracted_content)
        
        # Step 2: AI-powered root cause analysis
        if selected_model or self.model_config["primary"]:
            model_to_use = selected_model or self.model_config["primary"][0]
            
            # Business impact analysis
            progress_callback(0.3, "Analyzing business impact")
            business_impact = self._analyze_business_impact(
                extracted_content, 
                quick_analysis,
                model_to_use
            )
            results["executive_summary"] = business_impact
            
            # Technical deep dive
            progress_callback(0.5, "Performing technical analysis")
            technical_analysis = self._analyze_technical_details(
                extracted_content,
                quick_analysis,
                model_to_use
            )
            results["technical_analysis"] = technical_analysis
            
            # Generate recommendations
            progress_callback(0.7, "Generating recommendations")
            recommendations = self._generate_recommendations(
                technical_analysis,
                business_impact,
                model_to_use
            )
            results["recommendations"] = recommendations
            
            # Prevention strategy
            progress_callback(0.85, "Creating prevention strategy")
            prevention = self._generate_prevention_strategy(
                technical_analysis,
                model_to_use
            )
            results["prevention_strategy"] = prevention
        else:
            # Fallback to algorithmic analysis only
            progress_callback(0.5, "Running algorithmic analysis")
            results = self._fallback_analysis(extracted_content, quick_analysis)
        
        # Step 3: Generate visual elements
        progress_callback(0.95, "Generating visualizations")
        results["visual_elements"] = self._generate_visuals(results)
        
        # Step 4: Calculate confidence
        results["confidence"] = self._calculate_comprehensive_confidence(results)
        
        # Step 5: Add formatted output for display
        results["formatted_output"] = self.format_output(results)
        
        # Post-analysis
        return self.post_analysis(results, content, filename)
    
    def _quick_analysis(self, content: str) -> Dict[str, Any]:
        """Quick algorithmic analysis for context"""
        analysis = {
            "primary_error": self._extract_primary_error(content),
            "error_chain": extract_error_chain(content),
            "environment": self._extract_environment(content),
            "timeline": self._extract_timeline(content),
            "affected_components": self._extract_components(content)
        }
        return analysis
    
    def _extract_primary_error(self, content: str) -> Dict[str, Any]:
        """Extract primary error details"""
        # Reuse pattern matching logic
        patterns = [
            (r'(?P<type>\w+(?:Exception|Error)):\s*(?P<message>.+?)(?:\n|\r|$)', 'exception'),
            (r'FATAL:\s*(?P<message>.+?)(?:\n|\r|$)', 'fatal'),
            (r'Caused by:\s*(?P<type>\w+):\s*(?P<message>.+?)(?:\n|\r|$)', 'root_cause'),
        ]
        
        for pattern, category in patterns:
            match = re.search(pattern, content, re.MULTILINE)
            if match:
                return {
                    "type": match.group('type') if 'type' in match.groupdict() else category,
                    "message": match.group('message'),
                    "category": category,
                    "position": match.start()
                }
        
        return {"type": "Unknown", "message": "No clear error found"}
    
    def _extract_environment(self, content: str) -> Dict[str, Any]:
        """Extract environment information"""
        env = {
            "is_production": bool(re.search(r'(?i)(prod|production|live)', content[:5000])),
            "memory_state": "Unknown",
            "thread_count": 0,
            "system_load": "Unknown"
        }
        
        # Memory patterns
        memory_match = re.search(r'(?i)memory:\s*(\d+)\s*(?:mb|gb)?\s*(?:used|free)', content)
        if memory_match:
            env["memory_state"] = memory_match.group(0)
        
        # Thread patterns
        thread_match = re.search(r'(?i)threads?:\s*(\d+)', content)
        if thread_match:
            env["thread_count"] = int(thread_match.group(1))
        
        # Recent deployment
        deploy_match = re.search(r'(?i)(?:version|build|deployment):\s*([^\n]+)', content)
        if deploy_match:
            env["deployment_info"] = deploy_match.group(1).strip()
        
        return env
    
    def _extract_timeline(self, content: str) -> List[Dict[str, str]]:
        """Extract timeline of events"""
        timeline = []
        
        # Common timestamp patterns
        timestamp_patterns = [
            r'(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2})',
            r'(\d{2}/\d{2}/\d{4}\s+\d{2}:\d{2}:\d{2})',
            r'(\w{3}\s+\d{1,2}\s+\d{2}:\d{2}:\d{2})'
        ]
        
        for pattern in timestamp_patterns:
            matches = re.finditer(pattern, content)
            for match in matches:
                # Get surrounding context
                start = max(0, match.start() - 100)
                end = min(len(content), match.end() + 200)
                context = content[start:end]
                
                # Extract event description
                event_match = re.search(r'(?:ERROR|WARN|INFO|FATAL):\s*(.+)', context)
                if event_match:
                    timeline.append({
                        "timestamp": match.group(1),
                        "event": event_match.group(1)[:100],
                        "position": match.start()
                    })
        
        # Sort by position (chronological order)
        timeline.sort(key=lambda x: x["position"])
        return timeline[:10]  # Top 10 events
    
    def _extract_components(self, content: str) -> List[str]:
        """Extract affected components"""
        components = set()
        
        # Class/module patterns
        patterns = [
            r'at\s+([\w\.]+)\.',  # Java/C# style
            r'File\s+"([^"]+)"',  # Python style
            r'in\s+(\w+)::'  # C++ style
        ]
        
        for pattern in patterns:
            matches = re.finditer(pattern, content)
            for match in matches:
                component = match.group(1)
                # Extract main component name
                if '.' in component:
                    component = component.split('.')[-2] if len(component.split('.')) > 1 else component
                if '/' in component:
                    component = component.split('/')[-1]
                components.add(component)
        
        return list(components)[:10]  # Top 10 components
    
    def _analyze_business_impact(self, content: str, quick_analysis: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Analyze business impact using AI"""
        prompt = f"""Analyze the business impact of this crash. Focus on user impact and revenue risk.

Error Type: {quick_analysis['primary_error'].get('type', 'Unknown')}
Error Message: {quick_analysis['primary_error'].get('message', 'Unknown')}
Environment: {"PRODUCTION" if quick_analysis['environment']['is_production'] else "Non-production"}
Affected Components: {', '.join(quick_analysis['affected_components'][:5])}

Key sections from crash dump:
{content[:2000]}

Provide analysis in this exact JSON format:
{{
    "business_impact": "Brief description of business impact",
    "affected_users": "Estimated number or percentage of affected users",
    "revenue_risk": "Estimated revenue impact (per hour if applicable)",
    "urgency": "IMMEDIATE/HIGH/MEDIUM/LOW",
    "key_stakeholders": ["List of teams/roles to notify"]
}}

Be specific and quantitative where possible."""

        response = self.generate_with_model(prompt, model, temperature=0.3)
        
        if response:
            # Use centralized JSON extraction
            json_data = extract_json_from_text(response)
            if json_data:
                return json_data
            else:
                # Log the error and show raw response for debugging
                self.logger.warning("JSON parsing failed")
                return {
                    "user_impact": "See raw analysis below",
                    "revenue_risk": "JSON parsing failed",
                    "affected_users": "Unknown",
                    "priority": "HIGH",
                    "raw_response": response[:500]  # First 500 chars of raw output
                }
        
        # Fallback
        return {
            "business_impact": f"{quick_analysis['primary_error']['type']} affecting system stability",
            "affected_users": "Unable to determine",
            "revenue_risk": "Requires business context",
            "urgency": "HIGH" if quick_analysis['environment']['is_production'] else "MEDIUM",
            "key_stakeholders": ["Engineering", "Operations"]
        }
    
    def _analyze_technical_details(self, content: str, quick_analysis: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Deep technical analysis using AI"""
        # Build error chain description
        error_chain_desc = " → ".join([e.get("error", "?") for e in quick_analysis['error_chain'][:5]])
        
        prompt = f"""Perform deep technical analysis of this crash. Think step by step.

Primary Error: {quick_analysis['primary_error']['type']}: {quick_analysis['primary_error']['message']}
Error Chain: {error_chain_desc}
Environment: Threads={quick_analysis['environment']['thread_count']}, {quick_analysis['environment']['memory_state']}

Crash dump sections:
{content[:3000]}

Provide technical analysis in this exact JSON format:
{{
    "root_cause": "The actual root cause (not just the symptom)",
    "error_chain": ["Step 1 what happened", "Step 2 what happened", "Step 3 final error"],
    "contributing_factors": ["Factor 1", "Factor 2"],
    "technical_details": "Detailed technical explanation",
    "confidence_level": 0.0 to 1.0
}}

Focus on identifying the true root cause, not just the final error."""

        response = self.generate_with_model(prompt, model, temperature=0.3)
        
        if response:
            json_data = extract_json_from_text(response)
            if json_data:
                return json_data
            else:
                self.logger.warning("JSON parsing failed in recommendations")
                return {
                    "immediate_actions": [f"AI response: {response[:200]}..."],
                    "investigation_steps": ["Review raw AI output for insights"],
                    "prevention_measures": ["JSON parsing error - check AI model output"],
                    "raw_response": response[:500]
                }
        
        # Fallback
        return {
            "root_cause": quick_analysis['primary_error']['message'],
            "error_chain": [e.get("error", "Unknown") for e in quick_analysis['error_chain'][:3]],
            "contributing_factors": ["Insufficient data for analysis"],
            "technical_details": "AI analysis unavailable",
            "confidence_level": 0.5
        }
    
    def _generate_recommendations(self, technical: Dict[str, Any], business: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Generate actionable recommendations"""
        prompt = f"""Based on this analysis, provide actionable recommendations.

Root Cause: {technical.get('root_cause', 'Unknown')}
Business Impact: {business.get('business_impact', 'Unknown')}
Urgency: {business.get('urgency', 'MEDIUM')}

Provide recommendations in this exact JSON format:
{{
    "immediate_actions": ["Action 1 to do right now", "Action 2"],
    "short_term_fixes": ["Fix to implement within 48h", "Another fix"],
    "long_term_improvements": ["Architectural change", "Process improvement"],
    "monitoring_additions": ["What to monitor", "Alerts to add"]
}}

Make recommendations specific and actionable."""

        response = self.generate_with_model(prompt, model, temperature=0.4)
        
        if response:
            try:
                json_data = extract_json_from_text(response)
                if json_data:
                    return json_data
            except json.JSONDecodeError as e:
                self.logger.warning(f"JSON parsing failed in advanced recommendations: {e}")
                # Return raw response as recommendations
                return {
                    "immediate_actions": ["Review AI analysis below"],
                    "investigation_steps": [response[:200] + "..."],
                    "prevention_measures": ["Check full raw output"],
                    "monitoring_setup": ["JSON parsing failed"],
                    "raw_response": response[:500]
                }
        
        # Fallback recommendations
        return {
            "immediate_actions": [
                f"Investigate {technical.get('root_cause', 'the error')}",
                "Enable detailed logging if not already enabled"
            ],
            "short_term_fixes": [
                "Add error handling for this scenario",
                "Implement retry logic with backoff"
            ],
            "long_term_improvements": [
                "Review error handling architecture",
                "Implement comprehensive monitoring"
            ],
            "monitoring_additions": [
                "Add alerts for this error type",
                "Monitor system resources"
            ]
        }
    
    def _generate_prevention_strategy(self, technical: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Generate prevention strategy"""
        prompt = f"""Create a prevention strategy for this type of crash.

Root Cause: {technical.get('root_cause', 'Unknown')}
Contributing Factors: {', '.join(technical.get('contributing_factors', [])[:3])}

Provide prevention strategy in this exact JSON format:
{{
    "code_changes": ["Specific code change needed"],
    "testing_improvements": ["Test to add"],
    "deployment_safeguards": ["Safeguard to implement"],
    "architectural_considerations": ["Design improvement"]
}}"""

        response = self.generate_with_model(prompt, model, temperature=0.4)
        
        if response:
            try:
                json_data = extract_json_from_text(response)
                if json_data:
                    return json_data
            except json.JSONDecodeError as e:
                self.logger.warning(f"JSON parsing failed in advanced recommendations: {e}")
                # Return raw response as recommendations
                return {
                    "immediate_actions": ["Review AI analysis below"],
                    "investigation_steps": [response[:200] + "..."],
                    "prevention_measures": ["Check full raw output"],
                    "monitoring_setup": ["JSON parsing failed"],
                    "raw_response": response[:500]
                }
        
        return {
            "code_changes": ["Add defensive programming checks"],
            "testing_improvements": ["Add unit tests for error conditions"],
            "deployment_safeguards": ["Implement canary deployments"],
            "architectural_considerations": ["Consider circuit breaker pattern"]
        }
    
    def _generate_visuals(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate visual elements for the analysis"""
        visuals = {}
        
        # Error flow diagram
        if results.get("technical_analysis", {}).get("error_chain"):
            visuals["error_flow"] = generate_error_flow_diagram(
                results["technical_analysis"]["error_chain"]
            )
        
        # Timeline visualization (placeholder for actual implementation)
        visuals["timeline"] = "Timeline visualization would go here"
        
        # Impact radius diagram (placeholder)
        visuals["impact_radius"] = "Impact visualization would go here"
        
        return visuals
    
    def _calculate_comprehensive_confidence(self, results: Dict[str, Any]) -> float:
        """Calculate overall confidence in the analysis"""
        confidence_factors = []
        
        # Technical confidence
        tech_confidence = results.get("technical_analysis", {}).get("confidence_level", 0.5)
        confidence_factors.append(tech_confidence * 0.4)  # 40% weight
        
        # Data quality confidence
        if results.get("executive_summary", {}).get("affected_users") != "Unable to determine":
            confidence_factors.append(0.2)  # 20% weight
        
        # Error chain clarity
        if len(results.get("technical_analysis", {}).get("error_chain", [])) > 1:
            confidence_factors.append(0.2)  # 20% weight
        
        # Root cause specificity
        root_cause = results.get("technical_analysis", {}).get("root_cause", "")
        if root_cause and root_cause != "Unknown" and len(root_cause) > 20:
            confidence_factors.append(0.2)  # 20% weight
        
        return min(0.95, sum(confidence_factors))
    
    def _fallback_analysis(self, content: str, quick_analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Fallback analysis when no AI model is available"""
        return {
            "executive_summary": {
                "business_impact": f"System crash due to {quick_analysis['primary_error']['type']}",
                "affected_users": "Unknown - requires AI analysis",
                "revenue_risk": "Unknown - requires AI analysis",
                "urgency": "HIGH" if quick_analysis['environment']['is_production'] else "MEDIUM",
                "key_stakeholders": ["Engineering"]
            },
            "technical_analysis": {
                "root_cause": quick_analysis['primary_error']['message'],
                "error_chain": [e.get("error", "Unknown") for e in quick_analysis['error_chain']],
                "contributing_factors": ["Requires AI analysis for details"],
                "technical_details": "AI model required for deep analysis",
                "confidence_level": 0.3
            },
            "recommendations": {
                "immediate_actions": ["Investigate error", "Check logs"],
                "short_term_fixes": ["Requires AI analysis"],
                "long_term_improvements": ["Requires AI analysis"],
                "monitoring_additions": ["Monitor for recurrence"]
            },
            "prevention_strategy": {
                "code_changes": ["Review error handling"],
                "testing_improvements": ["Add test coverage"],
                "deployment_safeguards": ["Use staged rollouts"],
                "architectural_considerations": ["Review system design"]
            },
            "visual_elements": {},
            "confidence": 0.3
        }
    
    def format_output(self, results: Dict[str, Any]) -> str:
        """Format analysis results as comprehensive markdown"""
        output = f"""# Root Cause Analysis Report

## Executive Summary
- **Business Impact**: {results['executive_summary'].get('business_impact', 'Unknown')}
- **Affected Users**: {results['executive_summary'].get('affected_users', 'Unknown')} 
- **Revenue Risk**: {results['executive_summary'].get('revenue_risk', 'Unknown')}
- **Urgency**: {results['executive_summary'].get('urgency', 'MEDIUM')}
- **Confidence**: {results.get('confidence', 0):.1%}

## Technical Analysis

### Root Cause
{results['technical_analysis'].get('root_cause', 'Not identified')}

### Error Chain"""
        
        # Format error chain
        for i, step in enumerate(results['technical_analysis'].get('error_chain', []), 1):
            output += f"\n{i}. {step}"
        
        output += f"""

### Contributing Factors"""
        
        for factor in results['technical_analysis'].get('contributing_factors', []):
            output += f"\n- {factor}"
        
        output += f"""

### Technical Details
{results['technical_analysis'].get('technical_details', 'No details available')}

## Actionable Recommendations

### 1. Immediate Actions"""
        
        for action in results['recommendations'].get('immediate_actions', []):
            output += f"\n- {action}"
        
        output += "\n\n### 2. Short-term Fixes (48 hours)"
        
        for fix in results['recommendations'].get('short_term_fixes', []):
            output += f"\n- {fix}"
        
        output += "\n\n### 3. Long-term Improvements"
        
        for improvement in results['recommendations'].get('long_term_improvements', []):
            output += f"\n- {improvement}"
        
        output += "\n\n### 4. Monitoring Additions"
        
        for monitor in results['recommendations'].get('monitoring_additions', []):
            output += f"\n- {monitor}"
        
        output += f"""

## Prevention Strategy

### Code Changes"""
        
        for change in results['prevention_strategy'].get('code_changes', []):
            output += f"\n- {change}"
        
        output += "\n\n### Testing Improvements"
        
        for test in results['prevention_strategy'].get('testing_improvements', []):
            output += f"\n- {test}"
        
        output += "\n\n### Deployment Safeguards"
        
        for safeguard in results['prevention_strategy'].get('deployment_safeguards', []):
            output += f"\n- {safeguard}"
        
        output += "\n\n### Architectural Considerations"
        
        for consideration in results['prevention_strategy'].get('architectural_considerations', []):
            output += f"\n- {consideration}"
        
        # Add visual elements if available
        if results.get('visual_elements', {}).get('error_flow'):
            output += f"\n\n## Visual Analysis\n\n### Error Flow Diagram\n```mermaid\n{results['visual_elements']['error_flow']}\n```"
        
        output += f"\n\n---\n*Analysis completed in {results.get('processing_time', 0):.2f} seconds*"
        
        return output