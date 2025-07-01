"""
TuoKit Unified Agent System
Merges pipeline automation and educational guidance into one coherent interface
"""
import streamlit as st
from utils import DatabaseManager, safe_ollama_generate, capture_knowledge
from typing import Dict, List, Any, Optional, Tuple
import json
from datetime import datetime
import time

# Import tool functions with consistent interfaces
from pages.sql_generator import generate_sql
from pages.code_tools import explain_code, debug_code, generate_code
from pages.doc_tools import summarize_document, answer_question
from pages.regex_tool import generate_regex
from pages.error_tool import decode_error_comprehensive

class UnifiedToolExecutor:
    """Executes tools with consistent interface and error handling"""
    
    def __init__(self):
        self.tools = {
            "sql_generator": self._execute_sql_generator,
            "code_explainer": self._execute_code_explainer,
            "code_debugger": self._execute_code_debugger,
            "code_generator": self._execute_code_generator,
            "doc_summarizer": self._execute_doc_summarizer,
            "doc_qa": self._execute_doc_qa,
            "regex_generator": self._execute_regex_generator,
            "error_decoder": self._execute_error_decoder
        }
        self.db = DatabaseManager()
    
    def execute(self, tool_name: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Execute any tool with consistent error handling and response format"""
        start_time = time.time()
        
        try:
            if tool_name not in self.tools:
                raise ValueError(f"Unknown tool: {tool_name}")
            
            # Input validation
            params = self._validate_params(tool_name, params)
            
            # Execute tool
            result = self.tools[tool_name](params)
            
            # Log execution
            execution_time = int((time.time() - start_time) * 1000)
            
            return {
                "success": True,
                "output": result,
                "execution_time_ms": execution_time,
                "tool": tool_name,
                "educational_tip": self._get_educational_tip(tool_name, params)
            }
            
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "tool": tool_name,
                "recovery_suggestion": self._get_recovery_suggestion(tool_name, str(e))
            }    
    def _validate_params(self, tool_name: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Validate and sanitize tool parameters to prevent injection attacks"""
        # Basic validation rules per tool
        validations = {
            "sql_generator": ["query", "dialect"],
            "code_explainer": ["code"],
            "code_debugger": ["code", "error"],
            "code_generator": ["description"],
            "doc_summarizer": ["text", "length"],
            "doc_qa": ["document", "question"],
            "regex_generator": ["description"],
            "error_decoder": ["error", "code"]
        }
        
        required = validations.get(tool_name, [])
        
        # Check required parameters
        for param in required:
            if param not in params or not params[param]:
                raise ValueError(f"Missing required parameter: {param}")
        
        # Sanitize string inputs
        for key, value in params.items():
            if isinstance(value, str):
                # Basic sanitization - prevent script injection
                params[key] = value.replace("<script>", "").replace("</script>", "")
        
        return params
    
    def _get_educational_tip(self, tool_name: str, params: Dict[str, Any]) -> str:
        """Get context-aware educational tip for the tool usage"""
        tips = {
            "sql_generator": "💡 Always use parameterized queries in production to prevent SQL injection",
            "code_explainer": "💡 Look for patterns and anti-patterns in the explanation",
            "code_debugger": "💡 Read error messages carefully - they often point directly to the issue",
            "code_generator": "💡 Be specific in your descriptions for better code generation",
            "doc_summarizer": "💡 Adjust summary length based on your audience",
            "doc_qa": "💡 Ask specific questions for more accurate answers",
            "regex_generator": "💡 Test regex patterns with various edge cases",
            "error_decoder": "💡 Include full stack traces for better error analysis"
        }
        return tips.get(tool_name, "💡 Check the tool documentation for best practices")
    
    def _get_recovery_suggestion(self, tool_name: str, error: str) -> str:
        """Suggest recovery actions based on error type"""
        if "connection" in error.lower():
            return "Check if required services are running (Ollama, PostgreSQL)"
        elif "timeout" in error.lower():
            return "Try with simpler input or increase timeout settings"
        elif "parameter" in error.lower():
            return "Verify all required parameters are provided"
        else:
            return "Check input format and try again"    
    # Tool execution methods
    def _execute_sql_generator(self, params: Dict[str, Any]) -> str:
        result = generate_sql(
            params.get("query", ""),
            params.get("dialect", "PostgreSQL")
        )
        if result.get("error"):
            raise Exception(result.get("raw_response", "SQL generation failed"))
        return result.get("sql", "")
    
    def _execute_code_explainer(self, params: Dict[str, Any]) -> str:
        return explain_code(
            params["code"],
            params.get("model", "deepseek-coder:6.7b")
        )
    
    def _execute_code_debugger(self, params: Dict[str, Any]) -> str:
        return debug_code(
            params["code"],
            params["error"],
            params.get("model", "deepseek-coder:6.7b")
        )
    
    def _execute_code_generator(self, params: Dict[str, Any]) -> str:
        return generate_code(
            params["description"],
            params.get("model", "deepseek-coder:6.7b")
        )
    
    def _execute_doc_summarizer(self, params: Dict[str, Any]) -> str:
        return summarize_document(
            params["text"],
            params.get("length", 100)
        )
    
    def _execute_doc_qa(self, params: Dict[str, Any]) -> str:
        return answer_question(
            params["document"],
            params["question"]
        )
    
    def _execute_regex_generator(self, params: Dict[str, Any]) -> str:
        return generate_regex(
            params["description"],
            params.get("model", "deepseek-coder:6.7b")
        )
    
    def _execute_error_decoder(self, params: Dict[str, Any]) -> str:
        return decode_error_comprehensive(
            params["error"],
            params.get("code", ""),
            params.get("model", "deepseek-coder:6.7b")
        )
class PipelineExecutor:
    """Executes multi-step workflows with proper data passing"""
    
    def __init__(self, tool_executor: UnifiedToolExecutor):
        self.tool_executor = tool_executor
        self.execution_state = {}
        
    def execute_pipeline(self, steps: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Execute pipeline with data passing between steps"""
        results = {}
        execution_log = []
        pipeline_start = time.time()
        
        for i, step in enumerate(steps):
            step_name = step.get("name", f"Step {i+1}")
            
            # Allow steps to reference previous results
            if "params" in step:
                step["params"] = self._resolve_references(step["params"], results)
            
            # Execute step
            result = self.tool_executor.execute(step["tool"], step.get("params", {}))
            
            # Store result
            results[step_name] = result
            
            # Log execution
            execution_log.append({
                "step": step_name,
                "tool": step["tool"],
                "success": result["success"],
                "execution_time_ms": result.get("execution_time_ms", 0),
                "timestamp": datetime.now().isoformat()
            })
            
            # Stop on critical failure
            if not result["success"] and step.get("critical", True):
                break
        
        total_time = int((time.time() - pipeline_start) * 1000)
        
        return {
            "results": results,
            "execution_log": execution_log,
            "total_time_ms": total_time,
            "success": all(log["success"] for log in execution_log)
        }
    
    def _resolve_references(self, params: Dict[str, Any], results: Dict[str, Any]) -> Dict[str, Any]:
        """Resolve references to previous step results"""
        resolved = {}
        
        for key, value in params.items():
            if isinstance(value, str) and value.startswith("{{") and value.endswith("}}"):
                # Reference to previous result
                ref = value[2:-2].strip()
                if "." in ref:
                    step_name, field = ref.split(".", 1)
                    if step_name in results and results[step_name]["success"]:
                        resolved[key] = results[step_name].get("output", {}).get(field, value)
                    else:
                        resolved[key] = value
                else:
                    resolved[key] = value
            else:
                resolved[key] = value
                
        return resolved