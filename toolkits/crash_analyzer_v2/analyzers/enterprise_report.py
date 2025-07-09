"""
Enterprise Report Analyzer
Structured enterprise-grade analysis with 7 comprehensive sections
"""
import re
import json
import time
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime
from collections import defaultdict, Counter

from .base import BaseAnalyzer
from ..config.settings import ANALYSIS_METHODS
from ..utils.extractors import extract_error_chain, extract_smart_context
from ..utils.patterns import extract_error_location

class EnterpriseReportAnalyzer(BaseAnalyzer):
    """Enterprise-grade structured crash analysis with comprehensive reporting"""
    
    def __init__(self):
        super().__init__(ANALYSIS_METHODS["enterprise_report"])
        
    def analyze(self, content: str, filename: str, selected_model: str = None, **kwargs) -> Dict[str, Any]:
        """
        Perform enterprise-grade analysis with structured sections
        
        Args:
            content: Crash dump content
            filename: Name of the file
            selected_model: AI model to use (required)
            
        Returns:
            Structured analysis with 7 comprehensive sections
        """
        if not selected_model:
            return {
                "error": "Enterprise Report requires an AI model. Please select a model.",
                "suggestion": "Use Ollama with deepseek-r1:latest or similar model"
            }
        
        # Pre-analysis
        metrics = self.pre_analysis(content)
        
        # Extract relevant content
        extracted_content = extract_smart_context(content, max_size=204800)  # 200KB
        
        # Initialize results structure
        results = {
            "executive_summary": {},
            "context_environment": {},
            "crash_stack_analysis": {},
            "pre_crash_sequence": {},
            "solutions_workarounds": {},
            "suggested_improvements": {},
            "additional_observations": {},
            "formatted_sections": {}
        }
        
        # Get progress callback
        progress_callback = kwargs.get("progress_callback", lambda p, m: None)
        
        # Analyze each section
        progress_callback(0.1, "Analyzing executive summary")
        results["executive_summary"] = self._analyze_executive_summary(content, selected_model)
        
        progress_callback(0.25, "Extracting context and environment")
        results["context_environment"] = self._analyze_context_environment(content)
        
        progress_callback(0.4, "Performing stack trace analysis")
        results["crash_stack_analysis"] = self._analyze_crash_stack(content, selected_model)
        
        progress_callback(0.55, "Reconstructing pre-crash sequence")
        results["pre_crash_sequence"] = self._analyze_pre_crash_sequence(content, selected_model)
        
        progress_callback(0.7, "Generating solutions and workarounds")
        results["solutions_workarounds"] = self._analyze_solutions(content, results, selected_model)
        
        progress_callback(0.85, "Identifying improvements")
        results["suggested_improvements"] = self._analyze_improvements(content, selected_model)
        
        progress_callback(0.95, "Gathering additional observations")
        results["additional_observations"] = self._analyze_additional_observations(content)
        
        # Format output for display
        results["formatted_output"] = self.format_output(results)
        
        # Post-analysis
        return self.post_analysis(results, content, filename)
    
    def _analyze_executive_summary(self, content: str, model: str) -> Dict[str, Any]:
        """Generate executive summary with actionable items"""
        prompt = f"""Analyze this crash dump and provide an executive summary.

{content[:3000]}

Provide a JSON response with:
{{
    "crash": "One-sentence summary of the error and direct impact",
    "root_cause": {{
        "technical": "Technical code-related cause",
        "data": "Data-related cause if applicable"
    }},
    "actionable_items": [
        {{
            "type": "record|method|action",
            "description": "Specific item to investigate",
            "oid": "Object ID if applicable",
            "priority": "immediate|high|medium"
        }}
    ]
}}"""
        
        response = self.generate_with_model(prompt, model, temperature=0.2)
        
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    return json.loads(json_match.group(0))
            except:
                pass
        
        # Fallback analysis
        return self._fallback_executive_summary(content)
    
    def _analyze_context_environment(self, content: str) -> Dict[str, Any]:
        """Extract context and environment information"""
        context = {
            "system_user": {},
            "application_vm": {},
            "database": {},
            "environment": {},
            "pre_crash_transcript": []
        }
        
        # System & User
        patterns = {
            "machine_name": r"COMPUTERNAME=(\S+)",
            "username": r"USERNAME=(\S+)",
            "whatson_user": r"Whats'On User:\s*(\S+)",
            "site": r"Whats'On Site:\s*([^\n]+)"
        }
        
        for key, pattern in patterns.items():
            match = re.search(pattern, content)
            if match:
                context["system_user"][key] = match.group(1).strip()
        
        # Application & VM
        app_patterns = {
            "whatson_version": r"Whats'On Version:\s*([^\n]+)",
            "build": r"Whats'On Build:\s*([^\n]+)",
            "smalltalk_version": r"Smalltalk Version:\s*'([^']+)'",
            "db_hash": r"Whats'On DB Hash:\s*(\w+)"
        }
        
        for key, pattern in app_patterns.items():
            match = re.search(pattern, content)
            if match:
                context["application_vm"][key] = match.group(1).strip()
        
        # Database
        db_patterns = {
            "server_version": r"Oracle Server Version:\s*([^\n]+)",
            "client_version": r"Oracle Client Version:\s*([^\n]+)",
            "database_encoding": r"Database encoding:\s*(\S+)",
            "session_encoding": r"Session encoding:\s*(\S+)"
        }
        
        for key, pattern in db_patterns.items():
            match = re.search(pattern, content)
            if match:
                context["database"][key] = match.group(1).strip()
        
        # Environment
        env_patterns = {
            "timezone_offset": r"TimeZone offset=(\d+)",
            "frame_rate": r"Frame rate:\s*([^\n]+)",
            "frame_rate_mode": r"Frame rate mode:\s*([^\n]+)"
        }
        
        for key, pattern in env_patterns.items():
            match = re.search(pattern, content)
            if match:
                context["environment"][key] = match.group(1).strip()
        
        # Pre-crash transcript (simplified)
        transcript_match = re.search(r"BEGIN RUNTIME DIAGNOSTIC DUMP(.*?)Cause of Dump:", content, re.DOTALL)
        if transcript_match:
            lines = transcript_match.group(1).strip().split('\n')
            context["pre_crash_transcript"] = [line.strip() for line in lines if line.strip()][:10]
        
        return context
    
    def _analyze_crash_stack(self, content: str, model: str) -> Dict[str, Any]:
        """Analyze crash and stack trace details"""
        crash_info = {
            "unhandled_exception": {},
            "failure_point": {},
            "object_state_analysis": []
        }
        
        # Extract exception info
        exception_match = re.search(r"Cause of Dump:\s*([^\n]+)", content)
        if exception_match:
            exception_text = exception_match.group(1)
            crash_info["unhandled_exception"] = {
                "type": "Unhandled exception",
                "details": exception_text
            }
            
            # Extract failing selector
            selector_match = re.search(r"#(\w+)", exception_text)
            if selector_match:
                crash_info["unhandled_exception"]["failing_selector"] = f"#{selector_match.group(1)}"
        
        # Extract stack frames
        stack_pattern = r"\[(\d+)\]\s+(.+)"
        stack_frames = []
        for match in re.finditer(stack_pattern, content):
            frame_num = int(match.group(1))
            frame_text = match.group(2).strip()
            stack_frames.append({
                "frame": frame_num,
                "text": frame_text
            })
            if frame_num > 20:  # Limit to first 20 frames
                break
        
        if stack_frames:
            crash_info["failure_point"]["stack_frames"] = stack_frames[:10]
            crash_info["failure_point"]["first_error_frame"] = stack_frames[0]["text"] if stack_frames else "Unknown"
        
        # AI analysis of object state
        if stack_frames:
            stack_text = "\n".join([f"[{f['frame']}] {f['text']}" for f in stack_frames[:10]])
            
            prompt = f"""Analyze the object state in these stack frames:

{stack_text}

Look for:
1. nil attributes that shouldn't be nil
2. Empty collections that should have data
3. Unexpected object types or values
4. Missing methods or selectors

Provide analysis in JSON format:
{{
    "object_states": [
        {{
            "frame": 1,
            "object": "object name",
            "issue": "description of state issue",
            "likely_cause": "why this happened"
        }}
    ],
    "nil_checks_needed": ["location1", "location2"],
    "data_validation_needed": ["field1", "field2"]
}}"""
            
            response = self.generate_with_model(prompt, model, temperature=0.3)
            if response:
                try:
                    json_match = re.search(r'\{.*\}', response, re.DOTALL)
                    if json_match:
                        state_analysis = json.loads(json_match.group(0))
                        crash_info["object_state_analysis"] = state_analysis.get("object_states", [])
                except:
                    pass
        
        return crash_info
    
    def _analyze_pre_crash_sequence(self, content: str, model: str) -> Dict[str, Any]:
        """Reconstruct pre-crash sequence"""
        sequence = {
            "user_actions": [],
            "system_state": {},
            "reproduction_scenario": []
        }
        
        # Extract command line info
        cmd_match = re.search(r"Command Line:\s*([^\n]+)", content)
        if cmd_match:
            sequence["system_state"]["command_line"] = cmd_match.group(1).strip()
        
        # Use AI to reconstruct sequence
        prompt = f"""Analyze this crash dump to reconstruct the user actions and sequence leading to the crash:

{content[:3000]}

Based on the stack trace and context, provide:
{{
    "user_actions": [
        {{
            "step": 1,
            "action": "User action description",
            "ui_element": "Window/dialog/button involved"
        }}
    ],
    "key_records": [
        {{
            "type": "record type",
            "id": "identifier",
            "state": "being edited/saved/deleted"
        }}
    ],
    "reproduction_steps": [
        "Step 1: ...",
        "Step 2: ...",
        "Step 3: ..."
    ]
}}"""
        
        response = self.generate_with_model(prompt, model, temperature=0.3)
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    seq_data = json.loads(json_match.group(0))
                    sequence["user_actions"] = seq_data.get("user_actions", [])
                    sequence["system_state"]["key_records"] = seq_data.get("key_records", [])
                    sequence["reproduction_scenario"] = seq_data.get("reproduction_steps", [])
            except:
                pass
        
        return sequence
    
    def _analyze_solutions(self, content: str, results: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Generate solutions and workarounds"""
        solutions = {
            "root_cause_analysis": {},
            "code_level_fixes": [],
            "immediate_workarounds": []
        }
        
        # Combine information from previous analyses
        exception_info = results.get("crash_stack_analysis", {}).get("unhandled_exception", {})
        
        prompt = f"""Based on this crash analysis, provide solutions:

Exception: {exception_info.get('details', 'Unknown')}
Failing selector: {exception_info.get('failing_selector', 'Unknown')}

Stack trace and previous analysis show missing method and potential data issues.

Provide solutions in JSON:
{{
    "root_causes": {{
        "technical": "Detailed technical explanation",
        "data": "Data-related issues if any"
    }},
    "code_fixes": [
        {{
            "location": "Class>>method",
            "fix": "Specific code change",
            "code_sample": "actualCode ifNotNil: [:obj | obj doSomething]"
        }}
    ],
    "workarounds": [
        {{
            "type": "immediate|temporary",
            "action": "What support team should do",
            "impact": "minimal|moderate|significant"
        }}
    ]
}}"""
        
        response = self.generate_with_model(prompt, model, temperature=0.3)
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    sol_data = json.loads(json_match.group(0))
                    solutions["root_cause_analysis"] = sol_data.get("root_causes", {})
                    solutions["code_level_fixes"] = sol_data.get("code_fixes", [])
                    solutions["immediate_workarounds"] = sol_data.get("workarounds", [])
            except:
                pass
        
        return solutions
    
    def _analyze_improvements(self, content: str, model: str) -> Dict[str, Any]:
        """Identify proactive improvements"""
        improvements = {
            "database_performance": [],
            "code_efficiency": [],
            "best_practices": []
        }
        
        # Look for SQL queries
        sql_pattern = r"(?i)(SELECT|INSERT|UPDATE|DELETE)\s+.*?(?:FROM|INTO)\s+(\w+)"
        sql_queries = []
        for match in re.finditer(sql_pattern, content):
            sql_queries.append(match.group(0))
        
        if sql_queries:
            query_text = "\n".join(sql_queries[:5])
            prompt = f"""Analyze these SQL queries for performance improvements:

{query_text}

Suggest:
1. Index recommendations
2. Query optimizations
3. Potential performance issues

Format as JSON:
{{
    "index_recommendations": [
        {{
            "table": "table_name",
            "columns": ["col1", "col2"],
            "reason": "why this index helps"
        }}
    ],
    "query_optimizations": ["optimization1", "optimization2"],
    "performance_risks": ["risk1", "risk2"]
}}"""
            
            response = self.generate_with_model(prompt, model, temperature=0.3)
            if response:
                try:
                    json_match = re.search(r'\{.*\}', response, re.DOTALL)
                    if json_match:
                        perf_data = json.loads(json_match.group(0))
                        improvements["database_performance"] = perf_data.get("index_recommendations", [])
                except:
                    pass
        
        # Code efficiency analysis
        prompt = f"""Analyze this stack trace for code efficiency and best practice issues:

{content[:2000]}

Identify:
1. Anti-patterns or code smells
2. Deprecated method usage
3. Potential future risks

Provide as JSON:
{{
    "anti_patterns": ["pattern1", "pattern2"],
    "deprecated_usage": ["method1", "method2"],
    "future_risks": ["risk1", "risk2"],
    "refactoring_suggestions": ["suggestion1", "suggestion2"]
}}"""
        
        response = self.generate_with_model(prompt, model, temperature=0.3)
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    code_data = json.loads(json_match.group(0))
                    improvements["code_efficiency"] = code_data.get("refactoring_suggestions", [])
                    improvements["best_practices"] = code_data.get("anti_patterns", [])
            except:
                pass
        
        return improvements
    
    def _analyze_additional_observations(self, content: str) -> Dict[str, Any]:
        """Analyze memory, processes, and other observations"""
        observations = {
            "memory_analysis": {},
            "process_health": {},
            "anomalies": []
        }
        
        # Memory analysis
        memory_match = re.search(r"Memory Report.*?Total:\s*(\d+)", content, re.DOTALL)
        if memory_match:
            observations["memory_analysis"]["total_memory"] = memory_match.group(1)
        
        # Look for object counts
        object_pattern = r"(\w+)\s+(\d+)\s+instances"
        object_counts = []
        for match in re.finditer(object_pattern, content):
            object_counts.append({
                "class": match.group(1),
                "count": int(match.group(2))
            })
        
        if object_counts:
            observations["memory_analysis"]["top_objects"] = sorted(
                object_counts, 
                key=lambda x: x["count"], 
                reverse=True
            )[:10]
        
        # Process health
        process_pattern = r"Process named:\s*'([^']+)'.*?priority:\s*(\d+)"
        processes = []
        for match in re.finditer(process_pattern, content, re.DOTALL):
            processes.append({
                "name": match.group(1),
                "priority": int(match.group(2))
            })
        
        if processes:
            observations["process_health"]["active_processes"] = len(processes)
            observations["process_health"]["process_list"] = processes[:5]
        
        # Anomalies
        if "deadlock" in content.lower():
            observations["anomalies"].append("Potential deadlock detected")
        
        if re.search(r"memory.*?9\d%", content, re.IGNORECASE):
            observations["anomalies"].append("High memory usage (>90%)")
        
        if re.search(r"timeout|timed out", content, re.IGNORECASE):
            observations["anomalies"].append("Timeout conditions detected")
        
        return observations
    
    def _fallback_executive_summary(self, content: str) -> Dict[str, Any]:
        """Fallback executive summary when AI is not available"""
        # Extract basic information
        exception_match = re.search(r"Cause of Dump:\s*([^\n]+)", content)
        exception = exception_match.group(1) if exception_match else "Unknown error"
        
        # Look for specific patterns
        error_type = "Unknown"
        if "doesNotUnderstand" in content:
            error_type = "Method Missing Error"
        elif "NullPointerException" in content:
            error_type = "Null Reference Error"
        elif "OutOfMemory" in content:
            error_type = "Memory Error"
        
        return {
            "crash": f"{error_type}: {exception}",
            "root_cause": {
                "technical": "Requires AI analysis for detailed root cause",
                "data": "Data analysis pending"
            },
            "actionable_items": [
                {
                    "type": "method",
                    "description": "Review stack trace for missing methods",
                    "priority": "immediate"
                }
            ]
        }
    
    def format_output(self, results: Dict[str, Any]) -> str:
        """Format results into structured sections with collapsible parts"""
        output = "# Enterprise Crash Analysis Report\n\n"
        
        # Section 1: Executive Summary
        output += "## 1. Executive Summary\n\n"
        exec_sum = results.get("executive_summary", {})
        output += f"**Crash:** {exec_sum.get('crash', 'Analysis pending')}\n\n"
        
        root_cause = exec_sum.get("root_cause", {})
        output += f"**Root Cause:**\n"
        output += f"- Technical: {root_cause.get('technical', 'Unknown')}\n"
        output += f"- Data: {root_cause.get('data', 'N/A')}\n\n"
        
        output += "**Actionable Items:**\n"
        for item in exec_sum.get("actionable_items", []):
            output += f"- [{item.get('priority', 'medium').upper()}] {item.get('description', '')}"
            if item.get('oid'):
                output += f" (OID: {item['oid']})"
            output += "\n"
        
        # Section 2: Context & Environment
        output += "\n## 2. Context & Environment\n\n"
        context = results.get("context_environment", {})
        
        output += "**System & User:**\n"
        for key, value in context.get("system_user", {}).items():
            output += f"- {key.replace('_', ' ').title()}: {value}\n"
        
        output += "\n**Application & VM:**\n"
        for key, value in context.get("application_vm", {}).items():
            output += f"- {key.replace('_', ' ').title()}: {value}\n"
        
        output += "\n**Database:**\n"
        for key, value in context.get("database", {}).items():
            output += f"- {key.replace('_', ' ').title()}: {value}\n"
        
        # Section 3: Crash & Stack-Trace Analysis
        output += "\n## 3. Crash & Stack-Trace Analysis\n\n"
        crash = results.get("crash_stack_analysis", {})
        
        exception = crash.get("unhandled_exception", {})
        output += f"**Unhandled Exception:** {exception.get('type', 'Unknown')}\n"
        output += f"- Details: {exception.get('details', 'N/A')}\n"
        if exception.get('failing_selector'):
            output += f"- Failing Selector: {exception['failing_selector']}\n"
        
        output += "\n**Failure Point:**\n"
        frames = crash.get("failure_point", {}).get("stack_frames", [])
        for frame in frames[:5]:
            output += f"[{frame['frame']}] {frame['text']}\n"
        
        if crash.get("object_state_analysis"):
            output += "\n**Object State Analysis:**\n"
            for state in crash["object_state_analysis"][:5]:
                output += f"- Frame {state.get('frame', '?')}: {state.get('issue', 'Unknown issue')}\n"
        
        # Section 4: Pre-Crash Sequence & Reproduction
        output += "\n## 4. Pre-Crash Sequence & Reproduction\n\n"
        sequence = results.get("pre_crash_sequence", {})
        
        output += "**User Actions:**\n"
        for action in sequence.get("user_actions", []):
            output += f"{action.get('step', '?')}. {action.get('action', 'Unknown action')}"
            if action.get('ui_element'):
                output += f" ({action['ui_element']})"
            output += "\n"
        
        output += "\n**Reproduction Scenario:**\n"
        for step in sequence.get("reproduction_scenario", []):
            output += f"- {step}\n"
        
        # Section 5: Solutions & Workarounds
        output += "\n## 5. Solutions & Workarounds\n\n"
        solutions = results.get("solutions_workarounds", {})
        
        output += "**Root Cause Analysis:**\n"
        root = solutions.get("root_cause_analysis", {})
        output += f"- Technical: {root.get('technical', 'Pending analysis')}\n"
        output += f"- Data: {root.get('data', 'N/A')}\n"
        
        output += "\n**Code-Level Fixes:**\n"
        for fix in solutions.get("code_level_fixes", []):
            output += f"- {fix.get('location', 'Unknown')}: {fix.get('fix', '')}\n"
            if fix.get('code_sample'):
                output += f"  ```smalltalk\n  {fix['code_sample']}\n  ```\n"
        
        output += "\n**Immediate Workarounds:**\n"
        for workaround in solutions.get("immediate_workarounds", []):
            output += f"- [{workaround.get('type', 'temporary').upper()}] {workaround.get('action', '')}\n"
        
        # Section 6: Suggested Improvements
        output += "\n## 6. Suggested Improvements (Proactive Analysis)\n\n"
        improvements = results.get("suggested_improvements", {})
        
        if improvements.get("database_performance"):
            output += "**Database Performance:**\n"
            for rec in improvements["database_performance"]:
                output += f"- Index on {rec.get('table', '?')}.{rec.get('columns', [])} - {rec.get('reason', '')}\n"
        
        if improvements.get("code_efficiency"):
            output += "\n**Code & API Efficiency:**\n"
            for eff in improvements["code_efficiency"]:
                output += f"- {eff}\n"
        
        if improvements.get("best_practices"):
            output += "\n**Best Practices & Potential Risks:**\n"
            for practice in improvements["best_practices"]:
                output += f"- {practice}\n"
        
        # Section 7: Additional Observations
        output += "\n## 7. Additional Observations\n\n"
        observations = results.get("additional_observations", {})
        
        if observations.get("memory_analysis"):
            output += "**Memory & Object Analysis:**\n"
            mem = observations["memory_analysis"]
            if mem.get("total_memory"):
                output += f"- Total Memory: {mem['total_memory']} bytes\n"
            if mem.get("top_objects"):
                output += "- Top Object Types:\n"
                for obj in mem["top_objects"][:5]:
                    output += f"  - {obj['class']}: {obj['count']:,} instances\n"
        
        if observations.get("anomalies"):
            output += "\n**Anomalies:**\n"
            for anomaly in observations["anomalies"]:
                output += f"- {anomaly}\n"
        
        output += f"\n---\n*Enterprise analysis completed in {results.get('processing_time', 0):.2f} seconds*"
        
        return output