"""
Core Analysis Processors
Implements the four main analysis methods: Basic, Expert, Sampling, and Full
"""
import time
import json
import re
from typing import Dict, Any, List, Optional, Callable

try:
    import streamlit as st
except ImportError:
    # For testing outside of Streamlit
    class MockST:
        class session_state:
            selected_model = "deepseek-r1:latest"
            last_analysis_time = ""
            last_expert_time = ""
            chunk_analysis_time = ""
        
        @staticmethod
        def info(msg): print(f"INFO: {msg}")
        @staticmethod
        def warning(msg): print(f"WARNING: {msg}")
        @staticmethod
        def error(msg): print(f"ERROR: {msg}")
        @staticmethod
        def spinner(msg): return MockSpinner()
        @staticmethod
        def progress(value, text=""): return MockProgress()
        @staticmethod
        def container(): return MockContainer()
        @staticmethod
        def columns(spec): return [MockColumn() for _ in spec]
        @staticmethod
        def button(label, **kwargs): return False
    
    class MockSpinner:
        def __enter__(self): return self
        def __exit__(self, *args): pass
    
    class MockProgress:
        def progress(self, value, text=""): pass
        def empty(self): pass
    
    class MockContainer:
        def write(self, msg): print(msg)
        def caption(self, msg): print(msg)
        def empty(self): pass
    
    class MockColumn:
        def __enter__(self): return self
        def __exit__(self, *args): pass
    
    st = MockST()

from utils import safe_ollama_generate
from .config import CRASH_ANALYZER_CONFIG, EXPERT_PROMPT_TEMPLATE
from .patterns import match_known_patterns
from .extractors import extract_critical_sections, generate_strategic_samples
from .wcr_handler import is_wcr_format

# Import helpers if available
try:
    from utils.json_helper import safe_json_analysis, get_chunk_analysis_safe
    JSON_HELPER_AVAILABLE = True
except ImportError:
    JSON_HELPER_AVAILABLE = False
    # Define fallback function that uses the selected model
    def get_chunk_analysis_safe(chunk: str, chunk_info: str = "", model: str = None) -> Dict[str, Any]:
        """Fallback chunk analysis using selected model"""
        prompt = f"""Analyze this crash dump chunk for errors and issues.
{chunk_info}

CHUNK CONTENT:
{chunk[:8000]}  # Limit chunk size for model

Provide your analysis in this exact JSON format:
{{
    "errors_found": ["list of specific errors found"],
    "error_types": ["types of errors"],
    "severity": "Critical/High/Medium/Low",
    "root_cause_hints": "likely root cause based on this chunk",
    "error_locations": ["file/method/line locations"],
    "summary": "brief summary of findings"
}}

Be specific and technical. Focus on actual errors."""
        
        response = safe_ollama_generate(
            model=model or "deepseek-r1:latest",
            prompt=prompt,
            temperature=0.3
        )
        
        # Try to parse JSON from response
        import json
        import re
        
        try:
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response.get('response', ''), re.DOTALL)
            if json_match:
                return json.loads(json_match.group(0))
        except:
            pass
        
        # Fallback structure
        return {
            "errors_found": ["Unable to parse response"],
            "error_types": ["Unknown"],
            "severity": "Unknown",
            "root_cause_hints": "Analysis failed",
            "error_locations": ["Unknown"],
            "summary": "Failed to analyze chunk"
        }

try:
    from utils.wcr_patterns import extract_wcr_cause
    WCR_PATTERNS_AVAILABLE = True
except ImportError:
    WCR_PATTERNS_AVAILABLE = False
    extract_wcr_cause = None

def analyze_crash_dump(content: str) -> Dict[str, Any]:
    """
    Basic crash analysis - fast JSON response with smart extraction
    
    Args:
        content: Crash dump content
        
    Returns:
        Dictionary with analysis results
    """
    start_time = time.time()
    
    # Check if this is a WCR file
    is_wcr = is_wcr_format(content)
    
    # If WCR, try to extract cause directly
    wcr_cause = None
    if is_wcr and WCR_PATTERNS_AVAILABLE and extract_wcr_cause:
        wcr_cause = extract_wcr_cause(content)
    
    # Use smart extraction for better results
    extracted = extract_critical_sections(content, 15000)
    
    # Build prompt with WCR context if available
    wcr_context = ""
    if wcr_cause:
        wcr_context = f"\nIMPORTANT: This is a What's On (Smalltalk) crash. Primary cause: {wcr_cause}\n"
    
    prompt = f"""Analyze this crash dump and provide a structured analysis.
Focus on identifying the root cause, error types, and providing actionable solutions.
{wcr_context}
CRASH DUMP:
{extracted}

Provide your analysis in this exact JSON format:
{{
    "root_cause": "Brief technical description of what caused the crash",
    "error_location": "File/method/line where crash occurred",
    "error_type": "Exception type or error category",  
    "severity": "Critical/High/Medium/Low",
    "quick_fix": "Immediate solution to resolve the crash",
    "prevention": "How to prevent this in the future"
}}

Be concise and technical. Focus only on the actual error."""
    
    try:
        # Use JSON helper if available
        if JSON_HELPER_AVAILABLE:
            analysis = safe_json_analysis(
                content=extracted,
                model=st.session_state.get("selected_model", "deepseek-r1:latest"),
                prompt=prompt,
                max_retries=3
            )
        else:
            # Fallback to direct generation
            response = safe_ollama_generate(
                model=st.session_state.get("selected_model", "deepseek-r1:latest"),
                prompt=prompt,
                temperature=0.3
            )
            
            if response.get('error'):
                raise Exception(response.get('error'))
            
            # Parse JSON response
            response_text = response.get('response', '')
            json_match = re.search(r'\{.*\}', response_text, re.DOTALL)
            if json_match:
                analysis = json.loads(json_match.group(0))
            else:
                raise ValueError("No JSON found in response")
        
        # Track performance
        elapsed = time.time() - start_time
        st.session_state.last_analysis_time = f"{elapsed:.2f}s"
        
        # Ensure all required fields with better defaults
        required_fields = {
            "root_cause": "Unable to determine root cause",
            "error_location": "Location not identified", 
            "error_type": "Unknown error type",
            "severity": "Medium",
            "quick_fix": "Manual review required",
            "prevention": "Implement proper error handling"
        }
        
        # Fill in any missing fields
        for field, default in required_fields.items():
            if field not in analysis or not analysis[field] or analysis[field] == "Not identified":
                # Try pattern matching as fallback
                if field in ["error_type", "severity", "quick_fix", "prevention"]:
                    patterns = match_known_patterns(extracted[:5000])
                    if patterns:
                        first_pattern = patterns[0]
                        if field == "error_type":
                            analysis[field] = first_pattern['pattern']
                        elif field == "severity":
                            analysis[field] = first_pattern['severity']
                        elif field == "quick_fix":
                            analysis[field] = first_pattern['quick_fix']
                        elif field == "prevention":
                            analysis[field] = first_pattern['prevention']
                    else:
                        analysis[field] = default
                else:
                    analysis[field] = default
                
        return analysis
        
    except Exception as e:
        # Track performance even on error
        elapsed = time.time() - start_time
        st.session_state.last_analysis_time = f"{elapsed:.2f}s"
        
        # Try pattern matching as complete fallback
        patterns = match_known_patterns(content[:10000])
        if patterns:
            first_pattern = patterns[0]
            return {
                "root_cause": f"Detected {first_pattern['pattern']} error pattern",
                "error_location": f"Found at position {first_pattern['position']}",
                "error_type": first_pattern['pattern'],
                "severity": first_pattern['severity'],
                "quick_fix": first_pattern['quick_fix'],
                "prevention": first_pattern['prevention']
            }
        
        return {
            "root_cause": f"Analysis error: {str(e)}",
            "error_location": "Unknown",
            "error_type": str(type(e).__name__),
            "severity": "Medium",
            "quick_fix": "Check crash dump format and try again",
            "prevention": "Ensure proper error logging format"
        }

def generate_expert_report(content: str) -> str:
    """
    Generate detailed expert report - comprehensive but slower
    
    Args:
        content: Crash dump content
        
    Returns:
        Detailed markdown report
    """
    start_time = time.time()
    
    # Use smart extraction
    extracted = extract_critical_sections(content, 20000)
    
    prompt = EXPERT_PROMPT_TEMPLATE.format(crash_content=extracted)
    
    result = safe_ollama_generate(
        model=st.session_state.get("selected_model", "deepseek-r1:latest"),
        prompt=prompt,
        temperature=0.3  # Lower temperature for more focused analysis
    )
    
    # Track performance
    elapsed = time.time() - start_time
    st.session_state.last_expert_time = f"{elapsed:.2f}s"
    
    # Extract the response string from the dictionary
    if isinstance(result, dict):
        return result.get('response', 'Failed to generate report')
    return str(result)

def analyze_with_chunking(content: str, chunk_size: Optional[int] = None, 
                         overlap: Optional[int] = None, 
                         progress_callback: Optional[Callable] = None) -> Dict[str, Any]:
    """
    Analyzes large files using chunked processing.
    Supports files up to 5MB with progress tracking and abort capability.
    
    Args:
        content: Full file content
        chunk_size: Size of each chunk (uses config default if None)
        overlap: Overlap between chunks (uses config default if None)
        progress_callback: Optional callback for progress updates
        
    Returns:
        Dictionary with aggregated analysis results
    """
    if chunk_size is None:
        chunk_size = CRASH_ANALYZER_CONFIG["chunk_size"]
    if overlap is None:
        overlap = CRASH_ANALYZER_CONFIG["chunk_overlap"]
    
    # Calculate chunks
    chunks = []
    start = 0
    while start < len(content):
        end = min(start + chunk_size, len(content))
        chunks.append((start, end, content[start:end]))
        start = end - overlap if end < len(content) else end
    
    total_chunks = len(chunks)
    st.info(f"📊 Processing {total_chunks} chunks from {len(content):,} characters...")
    
    # Initialize results storage
    chunk_results = {
        "errors": set(),
        "error_types": set(),
        "severities": [],
        "root_causes": [],
        "locations": set(),
        "patterns": [],
        "summaries": []
    }
    
    # Progress tracking
    progress_bar = st.progress(0, text=f"Processing chunk 0/{total_chunks}")
    status_container = st.container()
    
    # Failure tracking
    consecutive_failures = 0
    max_failures = CRASH_ANALYZER_CONFIG.get("max_consecutive_failures", 10)
    total_failures = 0
    chunks_with_errors = 0
    
    # Abort functionality
    col1, col2 = st.columns([3, 1])
    with col2:
        abort_button = st.button("🛑 Abort", key="abort_chunking") if CRASH_ANALYZER_CONFIG["enable_abort"] else None
    
    start_time = time.time()
    aborted = False
    
    for i, (start_pos, end_pos, chunk) in enumerate(chunks):
        # Check for abort
        if abort_button and abort_button:
            aborted = True
            st.warning("⚠️ Processing aborted by user")
            break
        
        with status_container:
            model_info = f" with {st.session_state.get('selected_model', 'default model')}" if st.session_state.get('selected_model') else ""
            st.write(f"🔍 Analyzing chunk {i+1}/{total_chunks} (chars {start_pos:,}-{end_pos:,}){model_info}")
        
        # Check if chunk likely contains errors (improved detection)
        has_error_keywords = bool(re.search(
            r'error|exception|fail|crash|fatal|severe|panic|abort|warning|critical', 
            chunk, 
            re.IGNORECASE
        ))
        
        # Skip chunks without any error indicators if configured
        if (CRASH_ANALYZER_CONFIG.get("skip_empty_chunks", True) and 
            not has_error_keywords and 
            consecutive_failures < 3):  # Still process if we're having issues
            # Quick skip for chunks without errors
            chunk_results["summaries"].append(f"Chunk {i+1}: Skipped (no error keywords)")
            # Update progress without delay
            progress = (i + 1) / total_chunks
            progress_bar.progress(progress, text=f"Processing chunk {i+1}/{total_chunks} ({progress*100:.1f}%) - Skipped")
            continue
        
        try:
            # Always use the model-based analysis with the selected model
            chunk_data = get_chunk_analysis_safe(
                chunk=chunk,
                chunk_info=f" (position {start_pos}-{end_pos} in file)",
                model=st.session_state.get("selected_model", "deepseek-r1:latest")
            )
            
            # Check if we got valid data
            if chunk_data and isinstance(chunk_data, dict):
                # Reset consecutive failures on any successful parse
                consecutive_failures = 0
                
                # Check if chunk has actual errors
                if chunk_data.get("errors_found") or chunk_data.get("error_types"):
                    chunks_with_errors += 1
                
                # Also run pattern matching for additional insights
                chunk_patterns = match_known_patterns(chunk)
                if chunk_patterns:
                    chunk_results["patterns"].extend(chunk_patterns)
                    # Add pattern-based error types
                    for pattern in chunk_patterns:
                        chunk_results["error_types"].add(pattern["pattern"])
                        if pattern["severity"] not in chunk_data.get("severities", []):
                            chunk_results["severities"].append(pattern["severity"])
            else:
                # Shouldn't happen with enhanced helper, but just in case
                st.warning(f"⚠️ Unexpected response format for chunk {i+1}")
                chunk_data = {
                    "errors_found": [],
                    "error_types": [],
                    "severity": "Unknown",
                    "root_cause_hints": "",
                    "error_locations": [],
                    "summary": "Processing error"
                }
                total_failures += 1
                consecutive_failures += 1
            
            # Aggregate results
            chunk_results["errors"].update(chunk_data.get("errors_found", []))
            chunk_results["error_types"].update(chunk_data.get("error_types", []))
            
            severity = chunk_data.get("severity", "Unknown")
            if severity and severity != "Unknown":
                chunk_results["severities"].append(severity)
            
            if chunk_data.get("root_cause_hints") and chunk_data["root_cause_hints"] != "Unable to parse response":
                chunk_results["root_causes"].append(chunk_data["root_cause_hints"])
            
            chunk_results["locations"].update(chunk_data.get("error_locations", []))
            
            # Create meaningful summary
            summary = chunk_data.get('summary', 'No findings')
            if chunk_data.get("errors_found"):
                summary = f"{summary} ({len(chunk_data['errors_found'])} errors)"
            chunk_results["summaries"].append(f"Chunk {i+1}: {summary}")
            
        except Exception as e:
            st.warning(f"⚠️ Error processing chunk {i+1}: {str(e)}")
            total_failures += 1
            consecutive_failures += 1
            
            # Still try pattern matching as fallback
            try:
                chunk_patterns = match_known_patterns(chunk)
                if chunk_patterns:
                    chunk_results["patterns"].extend(chunk_patterns)
                    chunk_results["summaries"].append(f"Chunk {i+1}: Pattern matching only (error: {str(e)[:50]})")
                else:
                    chunk_results["summaries"].append(f"Chunk {i+1}: Processing failed")
            except:
                chunk_results["summaries"].append(f"Chunk {i+1}: Complete failure")
            
            # Check if we should stop
            if consecutive_failures >= max_failures:
                st.error(f"❌ Stopping after {consecutive_failures} consecutive failures")
                break
        
        # Update progress
        progress = (i + 1) / total_chunks
        progress_bar.progress(progress, text=f"Processing chunk {i+1}/{total_chunks} ({progress*100:.1f}%)")
        
        # Estimated time remaining
        elapsed = time.time() - start_time
        if i > 0:
            avg_time_per_chunk = elapsed / (i + 1)
            remaining_chunks = total_chunks - (i + 1)
            eta = avg_time_per_chunk * remaining_chunks
            status_container.caption(f"⏱️ Estimated time remaining: {int(eta//60)}m {int(eta%60)}s")
        
        # Small delay to prevent overload
        if CRASH_ANALYZER_CONFIG["chunk_processing_delay"] > 0:
            time.sleep(CRASH_ANALYZER_CONFIG["chunk_processing_delay"])
        
        if progress_callback:
            progress_callback(i + 1, total_chunks)
    
    # Clear progress indicators
    progress_bar.empty()
    status_container.empty()
    
    # Processing time
    elapsed_time = time.time() - start_time
    st.session_state.chunk_analysis_time = f"{elapsed_time:.2f}s"
    
    if aborted:
        st.info(f"Processed {i+1}/{total_chunks} chunks before abort")
    
    # Synthesize final results
    severity_order = {"Critical": 4, "High": 3, "Medium": 2, "Low": 1, "Unknown": 0}
    highest_severity = max(chunk_results["severities"], 
                          key=lambda x: severity_order.get(x, 0)) if chunk_results["severities"] else "Unknown"
    
    # Determine most likely root cause
    root_cause = "Not identified"
    if chunk_results["root_causes"]:
        # Simple frequency analysis
        cause_freq = {}
        for cause in chunk_results["root_causes"]:
            cause_freq[cause] = cause_freq.get(cause, 0) + 1
        root_cause = max(cause_freq.items(), key=lambda x: x[1])[0]
    
    # Compile final analysis
    final_analysis = {
        "root_cause": root_cause,
        "error_location": ", ".join(list(chunk_results["locations"])[:3]) or "Multiple locations",
        "error_type": ", ".join(list(chunk_results["error_types"])[:3]) or "Multiple types",
        "severity": highest_severity,
        "quick_fix": "Review the detailed chunk summaries for specific fixes",
        "prevention": "Address the root cause and implement proper error handling",
        "total_errors": len(chunk_results["errors"]),
        "chunks_processed": i + 1,
        "chunks_failed": total_failures,
        "chunks_with_errors": chunks_with_errors,
        "patterns_found": len(chunk_results["patterns"]),
        "processing_time": elapsed_time,
        "chunk_summaries": chunk_results["summaries"],
        "aborted": aborted,
        "stopped_early": consecutive_failures >= max_failures
    }
    
    return final_analysis