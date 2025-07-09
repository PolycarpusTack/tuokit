"""
Enhanced JSON Helper for TuoKit - Robust JSON extraction with better error handling
Fixes the "Expecting value: line 1 column 1 (char 0)" errors
"""
import json
import re
from typing import Dict, Any, Optional

def ensure_json_response(prompt: str, add_json_instruction: bool = True) -> str:
    """
    Ensures the prompt will generate a JSON response
    by adding explicit JSON instructions if needed
    """
    if add_json_instruction and "json" in prompt.lower():
        # Add explicit JSON instruction at the end
        json_instruction = """

IMPORTANT: Respond ONLY with valid JSON. No additional text before or after the JSON.
Start your response with { and end with }"""
        return prompt + json_instruction
    return prompt

def extract_json_from_response(response_text: str, fallback_on_error: bool = True) -> Dict[str, Any]:
    """
    Extract JSON from response text with multiple fallback strategies
    
    Args:
        response_text: The raw response text from LLM
        fallback_on_error: If True, returns a structured fallback instead of error dict
        
    Returns:
        Parsed JSON dict or fallback structure
    """
    # Handle None or empty responses
    if not response_text or response_text.strip() == '':
        if fallback_on_error:
            return {
                "errors_found": [],
                "error_types": [],
                "severity": "Unknown",
                "root_cause_hints": "Empty response from model",
                "error_locations": [],
                "summary": "No analysis available - empty response"
            }
        else:
            return {"error": "Empty response", "raw_response": ""}
    
    # Clean up response text
    response_text = response_text.strip()
    
    # Strategy 1: Try direct parsing
    try:
        return json.loads(response_text)
    except json.JSONDecodeError:
        pass
    
    # Strategy 2: Extract JSON block from markdown code blocks
    # Handle ```json ... ``` blocks
    json_block_match = re.search(r'```(?:json)?\s*\n?(.*?)\n?```', response_text, re.DOTALL | re.IGNORECASE)
    if json_block_match:
        try:
            return json.loads(json_block_match.group(1).strip())
        except json.JSONDecodeError:
            pass
    
    # Strategy 3: Find JSON object anywhere in text
    # Look for content between first { and last }
    brace_positions = []
    for i, char in enumerate(response_text):
        if char == '{':
            brace_positions.append(('open', i))
        elif char == '}':
            brace_positions.append(('close', i))
    
    if brace_positions:
        # Find the first { and the matching }
        open_count = 0
        start_pos = None
        end_pos = None
        
        for brace_type, pos in brace_positions:
            if brace_type == 'open':
                if open_count == 0:
                    start_pos = pos
                open_count += 1
            else:  # close
                open_count -= 1
                if open_count == 0 and start_pos is not None:
                    end_pos = pos + 1
                    break
        
        if start_pos is not None and end_pos is not None:
            try:
                json_str = response_text[start_pos:end_pos]
                return json.loads(json_str)
            except json.JSONDecodeError:
                pass
    
    # Strategy 4: Try to extract key-value pairs from text
    # Look for patterns like "errors_found": [...] or "severity": "High"
    extracted_data = {}
    
    # Extract arrays
    array_patterns = [
        (r'"?errors_found"?\s*:\s*\[(.*?)\]', 'errors_found'),
        (r'"?error_types"?\s*:\s*\[(.*?)\]', 'error_types'),
        (r'"?error_locations"?\s*:\s*\[(.*?)\]', 'error_locations')
    ]
    
    for pattern, key in array_patterns:
        match = re.search(pattern, response_text, re.DOTALL)
        if match:
            # Try to parse the array content
            array_content = match.group(1).strip()
            if array_content:
                # Extract quoted strings
                items = re.findall(r'"([^"]*)"', array_content)
                extracted_data[key] = items
            else:
                extracted_data[key] = []
    
    # Extract string values
    string_patterns = [
        (r'"?severity"?\s*:\s*"([^"]*)"', 'severity'),
        (r'"?root_cause_hints"?\s*:\s*"([^"]*)"', 'root_cause_hints'),
        (r'"?summary"?\s*:\s*"([^"]*)"', 'summary')
    ]
    
    for pattern, key in string_patterns:
        match = re.search(pattern, response_text, re.IGNORECASE)
        if match:
            extracted_data[key] = match.group(1)
    
    # If we extracted some data, use it
    if extracted_data:
        # Fill in missing fields with defaults
        default_structure = {
            "errors_found": [],
            "error_types": [],
            "severity": "Unknown",
            "root_cause_hints": "",
            "error_locations": [],
            "summary": "Partial extraction from response"
        }
        default_structure.update(extracted_data)
        return default_structure
    
    # Strategy 5: Check if the response contains error indicators
    error_keywords = ['error', 'exception', 'fail', 'crash', 'fatal']
    response_lower = response_text.lower()
    
    if any(keyword in response_lower for keyword in error_keywords):
        # Response mentions errors but we couldn't parse it
        if fallback_on_error:
            return {
                "errors_found": ["Unparseable error in response"],
                "error_types": ["ParseError"],
                "severity": "Medium",
                "root_cause_hints": "Response contains error information but couldn't be parsed",
                "error_locations": [],
                "summary": f"Raw response (truncated): {response_text[:200]}..."
            }
    
    # Final fallback
    if fallback_on_error:
        return {
            "errors_found": [],
            "error_types": [],
            "severity": "Unknown", 
            "root_cause_hints": "Unable to parse response",
            "error_locations": [],
            "summary": "No structured data could be extracted"
        }
    else:
        return {
            "error": "Failed to parse JSON response",
            "raw_response": response_text[:500]
        }

def safe_json_analysis(content: str, model: str, prompt: str, max_retries: int = 2) -> Dict[str, Any]:
    """
    Safely get JSON response from LLM with retry logic
    
    Args:
        content: The content to analyze
        model: The LLM model to use
        prompt: The analysis prompt
        max_retries: Number of retries on failure
        
    Returns:
        Parsed analysis dict
    """
    from utils import safe_ollama_generate
    
    # Ensure prompt asks for JSON
    enhanced_prompt = ensure_json_response(prompt)
    
    for attempt in range(max_retries):
        try:
            # Get response with lower temperature for consistency
            response = safe_ollama_generate(
                model=model,
                prompt=enhanced_prompt,
                temperature=0.1,  # Very low for consistent JSON
                format="json"  # Try to use format parameter if supported
            )
            
            # Handle response
            if isinstance(response, dict):
                response_text = response.get('response', '')
            else:
                response_text = str(response)
            
            # Extract JSON with fallback enabled
            result = extract_json_from_response(response_text, fallback_on_error=True)
            
            # If we got a valid result (not an error dict), return it
            if 'error' not in result or result.get('errors_found') is not None:
                return result
                
        except Exception as e:
            if attempt < max_retries - 1:
                continue
            else:
                # Return safe fallback on final failure
                return {
                    "errors_found": [],
                    "error_types": [str(type(e).__name__)],
                    "severity": "Unknown",
                    "root_cause_hints": f"Analysis failed: {str(e)}",
                    "error_locations": [],
                    "summary": "Analysis could not be completed"
                }
    
    # Should not reach here, but just in case
    return {
        "errors_found": [],
        "error_types": [],
        "severity": "Unknown",
        "root_cause_hints": "Analysis failed after retries",
        "error_locations": [],
        "summary": "Could not complete analysis"
    }

# Specific template for crash analysis chunks
CRASH_CHUNK_TEMPLATE = """
{
    "errors_found": ["specific error messages found"],
    "error_types": ["NullPointerException", "OutOfMemoryError", etc],
    "severity": "Critical|High|Medium|Low",
    "root_cause_hints": "any indicators of root cause",
    "error_locations": ["file.java:123", "method names"],
    "summary": "brief summary of findings"
}
"""

def get_chunk_analysis_safe(chunk: str, chunk_info: str = "", model: str = None) -> Dict[str, Any]:
    """
    Get crash chunk analysis with maximum safety and fallback handling
    """
    # Check if chunk has potential errors
    has_errors = bool(re.search(r'error|exception|fail|crash|fatal|severe', chunk, re.IGNORECASE))
    
    if not has_errors:
        # Quick return for chunks without errors
        return {
            "errors_found": [],
            "error_types": [],
            "severity": "Low",
            "root_cause_hints": "",
            "error_locations": [],
            "summary": "No errors detected in this chunk"
        }
    
    prompt = f"""
Analyze this crash dump section{chunk_info}.

CHUNK CONTENT:
{chunk[:1500]}...

Respond with JSON matching this structure:
{CRASH_CHUNK_TEMPLATE}

ONLY output the JSON object, nothing else.
"""
    
    # Default model if none provided
    if model is None:
        try:
            import streamlit as st
            model = st.session_state.get('selected_model', 'deepseek-r1:latest')
        except:
            model = 'deepseek-r1:latest'
    
    return safe_json_analysis(chunk, model, prompt)
