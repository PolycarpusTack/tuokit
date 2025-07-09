"""
Quick Triage Analyzer
Provides immediate crash assessment in under 10 seconds without AI
"""
import re
import time
import logging
from typing import Dict, Any, List, Tuple, Optional
from datetime import datetime

from .base import BaseAnalyzer
from ..config.settings import SEVERITY_SCORING, ANALYSIS_METHODS
from ..utils.patterns import ERROR_PATTERNS, extract_error_location
from ..utils.single_pass import single_pass_parse, format_as_timeline, get_error_summary
from ..utils.logger import get_logger, log_analysis_start

logger = get_logger(__name__)

class QuickTriageAnalyzer(BaseAnalyzer):
    """Fast algorithmic crash analysis without AI processing"""
    
    def __init__(self):
        super().__init__(ANALYSIS_METHODS["quick_triage"])
        self.severity_keywords = SEVERITY_SCORING["keywords"]
        self.severity_patterns = SEVERITY_SCORING["patterns"]
        self.context_multipliers = SEVERITY_SCORING["context_multipliers"]
        
    def analyze(self, content: str, filename: str, **kwargs) -> Dict[str, Any]:
        """
        Perform quick triage analysis using single-pass regex
        
        Args:
            content: Crash dump content
            filename: Name of the file
            
        Returns:
            Triage results with severity and immediate insights
        """
        # Log start
        log_analysis_start("Quick Triage", filename)
        
        # Pre-analysis
        metrics = self.pre_analysis(content)
        
        # Limit content to first 5KB for speed
        analyzed_content = content[:self.config["max_content_size"]]
        
        # NEW: Single-pass parsing
        parsed_errors = single_pass_parse(analyzed_content, max_errors=50)
        error_summary = get_error_summary(parsed_errors)
        
        # Core analysis steps - confidence will be float, not string
        results = {
            "severity_score": 0,
            "error_type": "Unknown",
            "immediate_cause": "Not detected",
            "impact": "Unknown",
            "quick_fix": "Requires further analysis",
            "similar_crashes": 0,
            "confidence": 0.3,  # Start with low confidence as float
            "patterns_matched": [],
            # NEW: Add parsed results
            "error_timeline": format_as_timeline(parsed_errors[:20]),
            "error_summary": error_summary,
            "total_errors": len(parsed_errors)
        }
        
        # Step 1: Use parsed errors for primary error
        primary_error = None  # Initialize to avoid undefined reference
        
        if parsed_errors:
            # Get the most severe error
            primary = max(parsed_errors, key=lambda e: SEVERITY_SCORING["keywords"].get(e.severity, 0))
            results["error_type"] = primary.type
            results["immediate_cause"] = primary.message
            results["error_location"] = primary.location
            
            # Convert to primary_error dict for compatibility
            primary_error = {
                "type": primary.type,
                "message": primary.message.replace(re.match(r'^\[[^\]]+\]\s*', primary.message).group() if re.match(r'^\[[^\]]+\]\s*', primary.message) else '', ''),  # Remove severity marker
                "location": primary.location,
                "position": primary.position
            }
            
            # Set initial severity based on parsed errors
            results["severity_score"] = max(
                SEVERITY_SCORING["keywords"].get(e.severity, 0) 
                for e in parsed_errors[:5]
            ) / 10  # Normalize to 0-10
        else:
            # Fallback to traditional extraction
            primary_error = self._extract_primary_error(analyzed_content)
            if primary_error:
                results["error_type"] = primary_error["type"]
                results["immediate_cause"] = primary_error["message"]
                results["error_location"] = primary_error.get("location", "Unknown")
        
        # Step 2: Calculate severity score
        severity_data = self._calculate_severity_score(analyzed_content)
        results["severity_score"] = severity_data["score"]
        results["severity"] = severity_data["level"]
        results["severity_factors"] = severity_data["factors"]
        
        # Step 3: Pattern matching
        patterns = self._match_patterns(analyzed_content)
        results["patterns_matched"] = patterns
        
        # Step 4: Determine impact
        results["impact"] = self._assess_impact(analyzed_content, primary_error)
        
        # Step 5: Generate quick fix
        results["quick_fix"] = self._generate_quick_fix(primary_error, patterns)
        
        # Step 6: Check similar crashes
        if self.db:
            results["similar_crashes"] = self._count_similar_crashes(primary_error)
        
        # Step 7: Calculate confidence
        results["confidence"] = self._calculate_triage_confidence(results)
        
        # Post-analysis
        return self.post_analysis(results, content, filename)
    
    def _extract_primary_error(self, content: str) -> Optional[Dict[str, Any]]:
        """Extract the primary error from crash dump"""
        # Use centralized patterns first
        for pattern_str, error_type, severity in ERROR_PATTERNS:
            match = re.search(pattern_str, content, re.IGNORECASE)
            if match:
                # Extract error message
                message = match.group(0)
                
                # Get location using centralized function
                location = extract_error_location(content, match)
                
                return {
                    "type": error_type,
                    "message": message[:200],  # Limit message length
                    "location": location,
                    "position": match.start(),
                    "severity_hint": severity
                }
        
        # Fallback patterns for more specific cases
        specific_patterns = [
            # Java/C# exceptions with message
            (r'(\w+(?:Exception|Error)):\s*([^\n\r]+)', 'exception'),
            # Python errors with message
            (r'(\w+Error):\s*([^\n\r]+)', 'python_error'),
            # Generic error messages
            (r'(?:FATAL|CRITICAL|ERROR):\s*([^\n\r]+)', 'generic_error'),
        ]
        
        for pattern, error_class in specific_patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                if error_class in ['exception', 'python_error'] and match.lastindex >= 2:
                    error_type = match.group(1)
                    message = match.group(2).strip()
                else:
                    error_type = error_class.upper()
                    message = match.group(1).strip() if match.lastindex else match.group(0)
                
                # Extract location
                location = extract_error_location(content, match)
                
                return {
                    "type": error_type,
                    "message": message[:200],
                    "location": location,
                    "position": match.start()
                }
        
        return None
    
    
    def _calculate_severity_score(self, content: str) -> Dict[str, Any]:
        """Calculate severity score based on keywords and patterns"""
        score = 0
        factors = []
        
        # Check severity keywords
        for keyword, weight in self.severity_keywords.items():
            pattern = rf'\b{keyword}\b'
            matches = len(re.findall(pattern, content, re.IGNORECASE))
            if matches:
                score += weight * min(matches, 3)  # Cap at 3 occurrences
                factors.append(f"{keyword} ({matches}x)")
        
        # Check known patterns
        for pattern_name, weight in self.severity_patterns.items():
            if re.search(pattern_name, content, re.IGNORECASE):
                score += weight
                factors.append(pattern_name)
        
        # Apply context multipliers
        for context, multiplier in self.context_multipliers.items():
            if re.search(context, content, re.IGNORECASE):
                score *= multiplier
                factors.append(f"{context} (x{multiplier})")
        
        # Keep raw score - normalize based on expected range
        # Typical scores range from 0-100, normalize to 0-10
        normalized_score = min(10, score / 10) if score > 0 else 0
        
        # Determine severity level
        if normalized_score >= 9:
            level = "CRITICAL"
        elif normalized_score >= 7:
            level = "HIGH"
        elif normalized_score >= 5:
            level = "MEDIUM"
        elif normalized_score >= 3:
            level = "LOW"
        else:
            level = "INFO"
        
        return {
            "score": round(normalized_score, 1),
            "level": level,
            "factors": factors
        }
    
    def _match_patterns(self, content: str) -> List[Dict[str, str]]:
        """Match against known crash patterns"""
        matched = []
        
        # Common crash patterns
        patterns = [
            {
                "name": "Null Pointer",
                "regex": r'(?i)(null\s*pointer|null\s*reference|nullptr)',
                "category": "memory"
            },
            {
                "name": "Out of Memory",
                "regex": r'(?i)(out\s*of\s*memory|oom|heap\s*space)',
                "category": "resource"
            },
            {
                "name": "Stack Overflow",
                "regex": r'(?i)(stack\s*overflow|recursive\s*call)',
                "category": "stack"
            },
            {
                "name": "Timeout",
                "regex": r'(?i)(timeout|timed\s*out|deadline\s*exceeded)',
                "category": "performance"
            },
            {
                "name": "Connection Failed",
                "regex": r'(?i)(connection\s*(?:failed|refused|reset)|socket\s*error)',
                "category": "network"
            },
            {
                "name": "Permission Denied",
                "regex": r'(?i)(permission\s*denied|access\s*denied|unauthorized)',
                "category": "security"
            },
            {
                "name": "File Not Found",
                "regex": r'(?i)(file\s*not\s*found|no\s*such\s*file|path\s*not\s*found)',
                "category": "filesystem"
            }
        ]
        
        for pattern in patterns:
            if re.search(pattern["regex"], content):
                matched.append({
                    "pattern": pattern["name"],
                    "category": pattern["category"]
                })
        
        return matched
    
    def _assess_impact(self, content: str, error: Optional[Dict[str, Any]]) -> str:
        """Assess the impact of the crash"""
        # Impact indicators
        if re.search(r'(?i)(data\s*loss|corruption|corrupt)', content):
            return "Data integrity compromised"
        elif re.search(r'(?i)(authentication|login|auth)', content):
            return "Authentication system affected"
        elif re.search(r'(?i)(payment|transaction|billing)', content):
            return "Payment processing impacted"
        elif re.search(r'(?i)(api|service\s*unavailable)', content):
            return "Service availability affected"
        elif error and "database" in error.get("type", "").lower():
            return "Database operations impacted"
        elif error and "network" in str(error.get("message", "")).lower():
            return "Network connectivity affected"
        else:
            return "System stability affected"
    
    def _generate_quick_fix(self, error: Optional[Dict[str, Any]], patterns: List[Dict[str, str]]) -> str:
        """Generate immediate actionable fix suggestion"""
        if not error:
            return "Enable detailed logging to capture error information"
        
        error_type = error.get("type", "").lower()
        
        # Specific fixes based on error type
        fixes = {
            "nullpointerexception": "Add null check before accessing object",
            "outofmemoryerror": "Increase heap size or optimize memory usage",
            "stackoverflow": "Check for infinite recursion or deep call chains",
            "timeoutexception": "Increase timeout values or optimize slow operations",
            "filenotfoundexception": "Verify file path and permissions",
            "sqlexception": "Check database connection and query syntax",
            "ioexception": "Verify file/network resources are available"
        }
        
        # Check for specific fix
        for error_key, fix in fixes.items():
            if error_key in error_type.replace(" ", "").lower():
                return fix
        
        # Pattern-based fixes
        pattern_names = [p["pattern"] for p in patterns]
        if "Null Pointer" in pattern_names:
            return "Add defensive null checks in error location"
        elif "Out of Memory" in pattern_names:
            return "Analyze memory usage and increase heap allocation"
        elif "Connection Failed" in pattern_names:
            return "Check network connectivity and service availability"
        elif "Permission Denied" in pattern_names:
            return "Review and update access permissions"
        
        # Generic fix
        return f"Investigate {error_type} at {error.get('location', 'reported location')}"
    
    def _count_similar_crashes(self, error: Optional[Dict[str, Any]]) -> int:
        """Count similar crashes in database"""
        if not error or not self.db:
            return 0
        
        try:
            # Simple count of crashes with same error type in last 24h
            result = self.db.execute_query("""
                SELECT COUNT(*) as count 
                FROM crash_analysis 
                WHERE analysis->>'error_type' = %s 
                AND created_at > NOW() - INTERVAL '24 hours'
            """, (error["type"],))
            
            if result:
                return result[0]["count"]
        except:
            pass
        
        return 0
    
    def _calculate_triage_confidence(self, results: Dict[str, Any]) -> float:
        """Calculate confidence level for triage results"""
        confidence_score = 0.0
        
        # Factors affecting confidence (normalized to 0-1)
        if results.get("error_type") != "Unknown":
            confidence_score += 0.30
        if results.get("immediate_cause") != "Not detected":
            confidence_score += 0.20
        if results.get("error_location", "Unknown") != "Unknown":
            confidence_score += 0.20
        if len(results.get("patterns_matched", [])) > 0:
            confidence_score += 0.15
        if results.get("severity_score", 0) > 7:
            confidence_score += 0.15
        
        # Return as float between 0 and 1
        return min(1.0, confidence_score)
    
    def format_output(self, results: Dict[str, Any]) -> str:
        """Format triage results as YAML-style output"""
        severity_indicator = {
            "CRITICAL": "[!!!]",
            "HIGH": "[!!]", 
            "MEDIUM": "[!]",
            "LOW": "[*]",
            "INFO": "[i]"
        }.get(results.get("severity", "MEDIUM"), "[?]")
        
        output = f"""Crash Triage Report:
├── Severity: {severity_indicator} {results.get('severity', 'Unknown')} (score: {results.get('severity_score', 0)}/10)
├── Error Type: {results.get('error_type', 'Unknown')}
├── Immediate Cause: "{results.get('immediate_cause', 'Not detected')}"
├── Location: {results.get('error_location', 'Unknown')}
├── Impact: "{results.get('impact', 'Unknown')}"
├── Quick Fix: "{results.get('quick_fix', 'Requires analysis')}"
├── Similar Crashes: {results.get('similar_crashes', 0)} in last 24h
└── Confidence: {results.get('confidence', 0):.0%}

Processing Time: {results.get('processing_time', 0):.2f}s"""
        
        # Add pattern information if available
        if results.get("patterns_matched"):
            output += "\n\nPatterns Detected:"
            for pattern in results["patterns_matched"]:
                output += f"\n  - {pattern['pattern']} ({pattern['category']})"
        
        # Add severity factors if available
        if results.get("severity_factors"):
            output += "\n\nSeverity Factors:"
            for factor in results["severity_factors"][:5]:  # Top 5 factors
                output += f"\n  - {factor}"
        
        return output