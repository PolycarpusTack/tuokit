"""
Deep Forensic Analysis Analyzer
Comprehensive analysis including non-crash improvements
"""
import re
import json
import time
import logging
from typing import Dict, Any, List, Optional, Tuple, Set
from collections import defaultdict, Counter
from datetime import datetime
import concurrent.futures
from dataclasses import dataclass

from .base import BaseAnalyzer
from ..config.settings import (
    ANALYSIS_METHODS, 
    MODEL_PREFERENCES, 
    DEEP_ANALYSIS_DIMENSIONS,
    PERFORMANCE_CONFIG
)
from ..utils.extractors import (
    extract_error_chain,
    extract_performance_indicators,
    extract_security_concerns
)
from ..utils.diagrams import (
    generate_error_pattern_diagram,
    generate_component_impact_diagram,
    generate_analysis_summary_diagram
)
from ..utils.logger import get_logger

logger = get_logger(__name__)

@dataclass
class AnalysisChunk:
    """Represents a chunk of content for parallel analysis"""
    id: int
    content: str
    start_pos: int
    end_pos: int
    metadata: Dict[str, Any]

class DeepForensicAnalyzer(BaseAnalyzer):
    """Complete file analysis with multi-dimensional insights"""
    
    def __init__(self):
        super().__init__(ANALYSIS_METHODS["deep_forensic"])
        self.model_config = MODEL_PREFERENCES.get("deep_forensic", {})
        self.dimensions = DEEP_ANALYSIS_DIMENSIONS
        self.performance_config = PERFORMANCE_CONFIG
        
    def analyze(self, content: str, filename: str, selected_model: str = None, **kwargs) -> Dict[str, Any]:
        """
        Perform deep forensic analysis on entire file
        
        Args:
            content: Full crash dump content
            filename: Name of the file
            selected_model: User-selected AI model
            
        Returns:
            Comprehensive multi-dimensional analysis
        """
        # Pre-analysis
        metrics = self.pre_analysis(content)
        
        # Get phase callback if provided
        phase_callback = kwargs.get("phase_callback", lambda phase, progress: None)
        
        results = {
            "overview": {},
            "critical_findings": {},
            "crash_analysis": {},
            "performance_analysis": {},
            "security_analysis": {},
            "code_quality": {},
            "predictive_insights": {},
            "knowledge_integration": {},
            "visual_analysis": {},
            "comprehensive_recommendations": {}
        }
        
        # Step 1: Create analysis chunks for parallel processing
        phase_callback("chunking", 0.5)
        chunks = self._create_analysis_chunks(content)
        results["overview"]["total_chunks"] = len(chunks)
        results["overview"]["file_size"] = len(content)
        phase_callback("chunking", 1.0)
        
        # Step 2: Parallel chunk analysis
        phase_callback("parallel", 0.0)
        chunk_results = self._analyze_chunks_parallel(
            chunks, 
            selected_model,
            progress_callback=kwargs.get("progress_callback"),
            phase_callback=lambda progress: phase_callback("parallel", progress)
        )
        
        # Step 3: Dimension-specific analysis with error handling
        if self.dimensions["crash_analysis"]["enabled"]:
            try:
                phase_callback("crash", 0.0)
                results["crash_analysis"] = self._deep_crash_analysis(
                    content, 
                    chunk_results,
                    selected_model
                )
                phase_callback("crash", 1.0)
            except Exception as e:
                logger.error(f"Crash analysis failed: {e}")
                results["crash_analysis"] = {"error": str(e)}
        
        if self.dimensions["performance_analysis"]["enabled"]:
            try:
                phase_callback("performance", 0.0)
                results["performance_analysis"] = self._deep_performance_analysis(
                    content,
                    chunk_results
                )
                phase_callback("performance", 1.0)
            except Exception as e:
                logger.error(f"Performance analysis failed: {e}")
                results["performance_analysis"] = {"error": str(e)}
        
        if self.dimensions["security_analysis"]["enabled"]:
            try:
                phase_callback("security", 0.0)
                results["security_analysis"] = self._deep_security_analysis(
                    content,
                    chunk_results
                )
                phase_callback("security", 1.0)
            except Exception as e:
                logger.error(f"Security analysis failed: {e}")
                results["security_analysis"] = {"error": str(e)}
        
        if self.dimensions["code_quality"]["enabled"]:
            try:
                results["code_quality"] = self._analyze_code_quality(
                    content,
                    chunk_results,
                    selected_model
                )
            except Exception as e:
                logger.error(f"Code quality analysis failed: {e}")
                results["code_quality"] = {"error": str(e)}
        
        if self.dimensions["predictive_analysis"]["enabled"]:
            try:
                results["predictive_insights"] = self._generate_predictive_insights(
                    results,
                    selected_model
                )
            except Exception as e:
                logger.error(f"Predictive analysis failed: {e}")
                results["predictive_insights"] = {"error": str(e)}
        
        # Step 4: Aggregate critical findings
        results["critical_findings"] = self._aggregate_critical_findings(results)
        
        # Step 5: Knowledge integration
        results["knowledge_integration"] = self._integrate_knowledge_base(results)
        
        # Step 6: Generate visualizations
        results["visual_analysis"] = self._generate_comprehensive_visuals(results)
        
        # Step 7: Generate comprehensive recommendations
        phase_callback("recommendations", 0.0)
        results["comprehensive_recommendations"] = self._generate_comprehensive_recommendations(
            results,
            selected_model
        )
        phase_callback("recommendations", 1.0)
        
        # Calculate overall confidence
        results["confidence"] = self._calculate_forensic_confidence(results)
        
        # Add formatted output
        results["formatted_output"] = self.format_output(results)
        
        # Log summary of what we found
        logger.info(f"Deep Forensic Analysis complete - Results structure:")
        logger.info(f"  - Total chunks: {results.get('overview', {}).get('total_chunks', 0)}")
        logger.info(f"  - Critical findings: {len(results.get('critical_findings', {}).get('immediate_threats', []))}")
        logger.info(f"  - Has crash analysis: {'crash_analysis' in results}")
        logger.info(f"  - Has performance analysis: {'performance_analysis' in results}")
        logger.info(f"  - Has security analysis: {'security_analysis' in results}")
        logger.info(f"  - Formatted output length: {len(results.get('formatted_output', ''))}")
        
        # Post-analysis
        return self.post_analysis(results, content, filename)
    
    def _create_analysis_chunks(self, content: str) -> List[AnalysisChunk]:
        """Divide content into chunks for parallel analysis"""
        chunks = []
        chunk_size = self.config.get("chunk_size", 8192)
        overlap = self.performance_config.get("chunk_overlap", 512)  # Configurable overlap
        
        # Create base chunks
        pos = 0
        chunk_id = 0
        
        while pos < len(content):
            end_pos = min(pos + chunk_size, len(content))
            
            # Extend to line boundary
            if end_pos < len(content):
                newline_pos = content.find('\n', end_pos)
                if newline_pos != -1 and newline_pos - end_pos < 200:
                    end_pos = newline_pos + 1
            
            # Extract chunk
            chunk_content = content[pos:end_pos]
            
            # Analyze chunk metadata
            metadata = {
                "has_errors": bool(re.search(r'(?i)(error|exception|fail)', chunk_content)),
                "has_stack_trace": bool(re.search(r'\s+at\s+\w+', chunk_content)),
                "timestamp_count": len(re.findall(r'\d{4}-\d{2}-\d{2}', chunk_content)),
                "line_count": chunk_content.count('\n')
            }
            
            chunks.append(AnalysisChunk(
                id=chunk_id,
                content=chunk_content,
                start_pos=pos,
                end_pos=end_pos,
                metadata=metadata
            ))
            
            chunk_id += 1
            pos = end_pos - overlap if end_pos < len(content) else end_pos
        
        return chunks
    
    def _analyze_chunks_parallel(self, chunks: List[AnalysisChunk], model: str = None, 
                                progress_callback=None, phase_callback=None) -> List[Dict[str, Any]]:
        """Analyze chunks in parallel"""
        results = []
        
        # Determine number of parallel workers
        max_workers = min(self.performance_config["parallel_chunks"], len(chunks))
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all chunks for analysis
            future_to_chunk = {
                executor.submit(self._analyze_single_chunk, chunk, model): chunk 
                for chunk in chunks
            }
            
            # Process completed analyses
            completed = 0
            chunk_timeout = self.performance_config.get("chunk_timeout", 30)  # 30s default
            
            for future in concurrent.futures.as_completed(future_to_chunk, timeout=len(chunks) * chunk_timeout):
                chunk = future_to_chunk[future]
                try:
                    # Get result with timeout
                    result = future.result(timeout=chunk_timeout)
                    results.append(result)
                    completed += 1
                    
                    # Progress callbacks
                    if progress_callback:
                        progress_callback(completed / len(chunks))
                    if phase_callback:
                        phase_callback(completed / len(chunks))
                        
                except concurrent.futures.TimeoutError:
                    logger.warning(f"Chunk {chunk.id} timed out after {chunk_timeout}s")
                    results.append({
                        "chunk_id": chunk.id,
                        "error": f"Analysis timed out after {chunk_timeout}s",
                        "patterns": [],
                        "issues": []
                    })
                except Exception as e:
                    # Don't fail entire analysis for one chunk
                    logger.error(f"Chunk {chunk.id} failed: {e}")
                    results.append({
                        "chunk_id": chunk.id,
                        "error": str(e),
                        "patterns": [],
                        "issues": []
                    })
        
        # Sort results by chunk ID to maintain order
        results.sort(key=lambda x: x.get("chunk_id", 0))
        return results
    
    def _analyze_single_chunk(self, chunk: AnalysisChunk, model: str = None) -> Dict[str, Any]:
        """Analyze a single chunk"""
        result = {
            "chunk_id": chunk.id,
            "position": f"{chunk.start_pos}-{chunk.end_pos}",
            "patterns": [],
            "issues": [],
            "metrics": {}
        }
        
        # Quick pattern matching
        patterns_found = []
        
        # Error patterns
        error_patterns = [
            (r'(?i)null\s*pointer\s*exception', 'null_pointer'),
            (r'(?i)out\s*of\s*memory', 'memory_exhaustion'),
            (r'(?i)stack\s*overflow', 'stack_overflow'),
            (r'(?i)deadlock', 'deadlock'),
            (r'(?i)race\s*condition', 'race_condition'),
            (r'(?i)buffer\s*overflow', 'buffer_overflow'),
            (r'(?i)sql\s*injection', 'sql_injection'),
            (r'(?i)cross-site\s*scripting|xss', 'xss'),
        ]
        
        for pattern, pattern_type in error_patterns:
            if re.search(pattern, chunk.content):
                patterns_found.append({
                    "type": pattern_type,
                    "severity": "HIGH" if pattern_type in ['sql_injection', 'xss', 'buffer_overflow'] else "MEDIUM"
                })
        
        result["patterns"] = patterns_found
        
        # Performance indicators
        perf_indicators = extract_performance_indicators(chunk.content)
        if any(perf_indicators.values()):
            result["metrics"]["performance"] = {
                "slow_operations": len(perf_indicators.get("slow_queries", [])),
                "memory_issues": len(perf_indicators.get("memory_issues", [])),
                "thread_issues": len(perf_indicators.get("thread_issues", [])),
                "timeouts": len(perf_indicators.get("timeouts", []))
            }
        
        # Code quality indicators
        quality_issues = []
        
        # Deprecated API usage
        if re.search(r'@Deprecated|\.deprecated\(|DEPRECATED', chunk.content):
            quality_issues.append("deprecated_api_usage")
        
        # Poor error handling
        if re.search(r'catch\s*\(\s*Exception\s*e\s*\)\s*{\s*}', chunk.content):
            quality_issues.append("empty_catch_block")
        
        if re.search(r'catch\s*\(\s*Throwable', chunk.content):
            quality_issues.append("catching_throwable")
        
        # Magic numbers
        if re.search(r'(?<!\d)(?:86400|3600|1440|1024|2048)(?!\d)', chunk.content):
            quality_issues.append("magic_numbers")
        
        if quality_issues:
            result["issues"] = quality_issues
        
        # AI analysis for complex patterns (if model available)
        if model and chunk.metadata.get("has_errors"):
            ai_insights = self._get_ai_chunk_insights(chunk.content, model)
            if ai_insights:
                result["ai_insights"] = ai_insights
        
        return result
    
    def _get_ai_chunk_insights(self, chunk_content: str, model: str) -> Optional[Dict[str, Any]]:
        """Get AI insights for a chunk"""
        prompt = f"""Analyze this code/log chunk for issues beyond obvious errors.
Look for:
1. Anti-patterns and code smells
2. Performance bottlenecks
3. Security vulnerabilities
4. Architectural issues

Chunk:
{chunk_content[:1500]}

Provide insights in JSON format:
{{
    "anti_patterns": ["pattern1", "pattern2"],
    "performance_risks": ["risk1"],
    "security_concerns": ["concern1"],
    "improvement_suggestions": ["suggestion1"]
}}"""

        response = self.generate_with_model(prompt, model, temperature=0.3)
        
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    return json.loads(json_match.group(0))
            except:
                pass
        
        return None
    
    def _deep_crash_analysis(self, content: str, chunk_results: List[Dict[str, Any]], 
                            model: str = None) -> Dict[str, Any]:
        """Deep analysis of crash-related issues"""
        crash_analysis = {
            "error_taxonomy": {},
            "error_chains": [],
            "root_cause_candidates": [],
            "impact_assessment": {},
            "recovery_analysis": {}
        }
        
        # Build error taxonomy
        all_errors = defaultdict(list)
        for result in chunk_results:
            for pattern in result.get("patterns", []):
                all_errors[pattern["type"]].append({
                    "chunk": result["chunk_id"],
                    "position": result["position"],
                    "severity": pattern["severity"]
                })
        
        crash_analysis["error_taxonomy"] = dict(all_errors)
        
        # Extract error chains
        error_chains = extract_error_chain(content)
        if error_chains:
            crash_analysis["error_chains"] = error_chains[:10]
            
            # Identify root cause candidates
            if model:
                root_causes = self._identify_root_causes(error_chains, content, model)
                crash_analysis["root_cause_candidates"] = root_causes
        
        # Impact assessment
        crash_analysis["impact_assessment"] = {
            "data_loss_risk": self._assess_data_loss_risk(content),
            "service_disruption": self._assess_service_disruption(content),
            "security_impact": self._assess_security_impact(chunk_results),
            "user_impact": self._assess_user_impact(content)
        }
        
        # Recovery analysis
        crash_analysis["recovery_analysis"] = {
            "recovery_attempted": bool(re.search(r'(?i)(recover|restart|retry|rollback)', content)),
            "recovery_successful": bool(re.search(r'(?i)(recovered|restarted|resumed)', content)),
            "recovery_time": self._estimate_recovery_time(content)
        }
        
        return crash_analysis
    
    def _deep_performance_analysis(self, content: str, chunk_results: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Deep performance analysis"""
        performance = {
            "bottlenecks": [],
            "resource_usage": {},
            "scalability_issues": [],
            "optimization_opportunities": []
        }
        
        # Aggregate performance metrics from chunks
        total_slow_ops = 0
        memory_pressure_chunks = 0
        thread_issue_chunks = 0
        
        for result in chunk_results:
            if "performance" in result.get("metrics", {}):
                perf = result["metrics"]["performance"]
                total_slow_ops += perf.get("slow_operations", 0)
                if perf.get("memory_issues", 0) > 0:
                    memory_pressure_chunks += 1
                if perf.get("thread_issues", 0) > 0:
                    thread_issue_chunks += 1
        
        # Identify bottlenecks
        if total_slow_ops > 10:
            performance["bottlenecks"].append({
                "type": "slow_operations",
                "severity": "HIGH",
                "count": total_slow_ops,
                "recommendation": "Profile and optimize slow operations"
            })
        
        if memory_pressure_chunks > len(chunk_results) * 0.1:
            performance["bottlenecks"].append({
                "type": "memory_pressure",
                "severity": "CRITICAL",
                "affected_percentage": (memory_pressure_chunks / len(chunk_results)) * 100,
                "recommendation": "Urgent memory optimization required"
            })
        
        # Resource usage analysis
        performance["resource_usage"] = self._analyze_resource_usage(content)
        
        # Scalability issues
        if thread_issue_chunks > 5:
            performance["scalability_issues"].append({
                "issue": "Thread contention",
                "evidence": f"{thread_issue_chunks} chunks with thread issues",
                "impact": "Limited horizontal scalability"
            })
        
        # Find optimization opportunities
        performance["optimization_opportunities"] = self._find_optimization_opportunities(
            content, 
            chunk_results
        )
        
        return performance
    
    def _deep_security_analysis(self, content: str, chunk_results: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Deep security analysis"""
        security = {
            "vulnerabilities": [],
            "exposed_secrets": [],
            "unsafe_operations": [],
            "compliance_issues": [],
            "security_score": 0
        }
        
        # Check for known vulnerabilities
        vuln_patterns = [
            (r'(?i)sql.*select.*where.*=\s*["\']?\s*\+', 'sql_injection', 'CRITICAL'),
            (r'(?i)eval\s*\(', 'code_injection', 'CRITICAL'),
            (r'(?i)os\.system|subprocess\.call.*shell=True', 'command_injection', 'CRITICAL'),
            (r'(?i)disable.*ssl.*verif|verify\s*=\s*False', 'ssl_verification_disabled', 'HIGH'),
            (r'(?i)md5|sha1', 'weak_cryptography', 'MEDIUM'),
        ]
        
        for pattern, vuln_type, severity in vuln_patterns:
            matches = re.finditer(pattern, content)
            for match in matches:
                security["vulnerabilities"].append({
                    "type": vuln_type,
                    "severity": severity,
                    "position": match.start(),
                    "context": content[max(0, match.start()-50):match.end()+50]
                })
        
        # Check for exposed secrets
        all_secrets = extract_security_concerns(content)
        security["exposed_secrets"] = all_secrets[:10]  # Limit to top 10
        
        # Unsafe operations
        unsafe_patterns = [
            (r'(?i)chmod\s+777', 'overly_permissive_permissions'),
            (r'(?i)disable.*auth', 'authentication_disabled'),
            (r'(?i)trust.*all.*cert', 'certificate_validation_disabled'),
        ]
        
        for pattern, op_type in unsafe_patterns:
            if re.search(pattern, content):
                security["unsafe_operations"].append(op_type)
        
        # Calculate security score (0-100, where 100 is most secure)
        score = 100
        score -= len(security["vulnerabilities"]) * 10
        score -= len(security["exposed_secrets"]) * 15
        score -= len(security["unsafe_operations"]) * 5
        security["security_score"] = max(0, score)
        
        return security
    
    def _analyze_code_quality(self, content: str, chunk_results: List[Dict[str, Any]], 
                             model: str = None) -> Dict[str, Any]:
        """Analyze code quality issues"""
        quality = {
            "anti_patterns": [],
            "technical_debt": [],
            "maintainability_issues": [],
            "best_practice_violations": []
        }
        
        # Aggregate quality issues from chunks
        all_issues = []
        for result in chunk_results:
            all_issues.extend(result.get("issues", []))
            if "ai_insights" in result:
                all_issues.extend(result["ai_insights"].get("anti_patterns", []))
        
        # Count and categorize issues
        issue_counts = Counter(all_issues)
        
        # Categorize by severity
        for issue, count in issue_counts.most_common():
            if count > 5:
                quality["anti_patterns"].append({
                    "pattern": issue,
                    "occurrences": count,
                    "severity": "HIGH" if count > 10 else "MEDIUM"
                })
        
        # Technical debt indicators
        if re.search(r'(?i)todo|fixme|hack|workaround', content):
            todo_count = len(re.findall(r'(?i)todo|fixme', content))
            quality["technical_debt"].append({
                "indicator": "unresolved_todos",
                "count": todo_count,
                "impact": "Delayed maintenance"
            })
        
        # Best practice violations
        violations = []
        
        # Long methods/functions
        method_pattern = r'(?:def|function|public|private|protected)\s+\w+[^{]*\{[^}]{500,}'
        if re.search(method_pattern, content):
            violations.append("long_methods")
        
        # Deep nesting
        deep_nesting = r'(?:\{[^{}]*){5,}'
        if re.search(deep_nesting, content):
            violations.append("deep_nesting")
        
        quality["best_practice_violations"] = violations
        
        return quality
    
    def _generate_predictive_insights(self, results: Dict[str, Any], model: str = None) -> Dict[str, Any]:
        """Generate predictive insights based on analysis"""
        predictive = {
            "future_risks": [],
            "trending_issues": [],
            "capacity_projections": {},
            "preventive_measures": []
        }
        
        # Analyze trends
        if results["crash_analysis"]["error_taxonomy"]:
            # Identify growing problems
            for error_type, occurrences in results["crash_analysis"]["error_taxonomy"].items():
                if len(occurrences) > 3:
                    predictive["trending_issues"].append({
                        "issue": error_type,
                        "trend": "increasing",
                        "occurrences": len(occurrences),
                        "risk_level": "HIGH" if len(occurrences) > 10 else "MEDIUM"
                    })
        
        # Future risks based on current state
        if results["performance_analysis"]["bottlenecks"]:
            for bottleneck in results["performance_analysis"]["bottlenecks"]:
                if bottleneck["severity"] == "CRITICAL":
                    predictive["future_risks"].append({
                        "risk": f"{bottleneck['type']} will cause system failure",
                        "timeframe": "1-7 days",
                        "probability": "HIGH",
                        "impact": "Service outage"
                    })
        
        # Capacity projections
        if results["performance_analysis"]["resource_usage"]:
            usage = results["performance_analysis"]["resource_usage"]
            if usage.get("memory_usage_percent", 0) > 80:
                predictive["capacity_projections"]["memory"] = {
                    "current": usage["memory_usage_percent"],
                    "projected_exhaustion": "< 30 days",
                    "recommendation": "Increase memory allocation"
                }
        
        # Preventive measures
        predictive["preventive_measures"] = self._generate_preventive_measures(results)
        
        return predictive
    
    def _aggregate_critical_findings(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Aggregate all critical findings across dimensions"""
        critical = {
            "immediate_threats": [],
            "high_impact_issues": [],
            "security_critical": [],
            "performance_critical": [],
            "action_priority": []
        }
        
        # Security critical
        if results["security_analysis"]["security_score"] < 50:
            critical["security_critical"].append({
                "finding": "Critical security score",
                "score": results["security_analysis"]["security_score"],
                "action": "Immediate security review required"
            })
        
        for vuln in results["security_analysis"]["vulnerabilities"]:
            if vuln["severity"] == "CRITICAL":
                critical["immediate_threats"].append({
                    "type": "security",
                    "issue": vuln["type"],
                    "action": "Patch immediately"
                })
        
        # Performance critical
        for bottleneck in results["performance_analysis"]["bottlenecks"]:
            if bottleneck["severity"] == "CRITICAL":
                critical["performance_critical"].append(bottleneck)
        
        # Crash analysis critical
        if results["crash_analysis"]["impact_assessment"]["data_loss_risk"] == "HIGH":
            critical["high_impact_issues"].append({
                "type": "data_loss",
                "risk": "HIGH",
                "action": "Implement data protection measures"
            })
        
        # Prioritize actions
        all_critical = (
            critical["immediate_threats"] + 
            critical["security_critical"] + 
            critical["performance_critical"]
        )
        
        # Sort by urgency
        critical["action_priority"] = sorted(
            all_critical,
            key=lambda x: 0 if "immediate" in str(x).lower() else 1
        )[:10]
        
        return critical
    
    def _integrate_knowledge_base(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Integrate findings with knowledge base"""
        knowledge = {
            "similar_incidents": [],
            "known_solutions": [],
            "best_practices": [],
            "lessons_learned": []
        }
        
        # This would integrate with actual knowledge base
        # For now, provide structure for future implementation
        
        # Extract key patterns for knowledge matching
        key_patterns = []
        for error_type in results["crash_analysis"]["error_taxonomy"].keys():
            key_patterns.append(error_type)
        
        knowledge["search_patterns"] = key_patterns
        knowledge["integration_ready"] = True
        
        return knowledge
    
    def _generate_comprehensive_visuals(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate comprehensive visualizations"""
        visuals = {}
        
        # Error pattern diagram
        if results["crash_analysis"]["error_taxonomy"]:
            patterns = [
                {"pattern": k, "category": "error"}
                for k in results["crash_analysis"]["error_taxonomy"].keys()
            ]
            visuals["error_patterns"] = generate_error_pattern_diagram(patterns)
        
        # Component impact diagram
        if results["crash_analysis"]["error_chains"]:
            components = list(set([
                e.get("error", "Unknown") 
                for e in results["crash_analysis"]["error_chains"][:10]
            ]))
            visuals["component_impact"] = generate_component_impact_diagram(components)
        
        # Analysis summary
        visuals["analysis_summary"] = generate_analysis_summary_diagram({
            "severity": "CRITICAL" if results["critical_findings"]["immediate_threats"] else "HIGH",
            "confidence": results.get("confidence", 0.8)
        })
        
        return visuals
    
    def _generate_comprehensive_recommendations(self, results: Dict[str, Any], 
                                              model: str = None) -> Dict[str, Any]:
        """Generate comprehensive recommendations"""
        recommendations = {
            "immediate_actions": [],
            "short_term": [],
            "long_term": [],
            "architectural": [],
            "process_improvements": []
        }
        
        # Immediate actions from critical findings
        for finding in results["critical_findings"]["action_priority"][:5]:
            recommendations["immediate_actions"].append(finding.get("action", "Review critical issue"))
        
        # Short-term fixes
        if results["performance_analysis"]["optimization_opportunities"]:
            for opp in results["performance_analysis"]["optimization_opportunities"][:3]:
                recommendations["short_term"].append(opp)
        
        # Long-term improvements
        if results["code_quality"]["technical_debt"]:
            recommendations["long_term"].append("Address technical debt systematically")
        
        if results["predictive_insights"]["future_risks"]:
            recommendations["long_term"].append("Implement predictive monitoring")
        
        # Architectural recommendations
        if len(results["crash_analysis"]["error_chains"]) > 5:
            recommendations["architectural"].append("Implement circuit breaker pattern")
            recommendations["architectural"].append("Add error isolation boundaries")
        
        # Process improvements
        if results["security_analysis"]["exposed_secrets"]:
            recommendations["process_improvements"].append("Implement secret scanning in CI/CD")
        
        if results["code_quality"]["best_practice_violations"]:
            recommendations["process_improvements"].append("Enforce code quality gates")
        
        return recommendations
    
    def _calculate_forensic_confidence(self, results: Dict[str, Any]) -> float:
        """Calculate overall confidence in forensic analysis"""
        confidence_factors = []
        
        # Coverage confidence (full file analyzed)
        confidence_factors.append(0.95)  # High confidence due to complete coverage
        
        # Pattern detection confidence
        total_patterns = sum(len(v) for v in results["crash_analysis"]["error_taxonomy"].values())
        if total_patterns > 20:
            confidence_factors.append(0.9)
        elif total_patterns > 10:
            confidence_factors.append(0.8)
        else:
            confidence_factors.append(0.6)
        
        # Multi-dimensional analysis
        dimensions_analyzed = sum(1 for k, v in results.items() if v and k != "overview")
        confidence_factors.append(min(0.95, dimensions_analyzed * 0.15))
        
        # Critical findings clarity
        if results["critical_findings"]["immediate_threats"]:
            confidence_factors.append(0.85)
        
        return sum(confidence_factors) / len(confidence_factors)
    
    # Helper methods for specific analyses
    
    def _identify_root_causes(self, error_chains: List[Dict[str, Any]], content: str, 
                             model: str) -> List[Dict[str, Any]]:
        """Identify root cause candidates using AI"""
        if not error_chains:
            return []
        
        # Prepare context for AI
        error_summary = "\n".join([f"- {e.get('error', 'Unknown')}" for e in error_chains[:5]])
        
        prompt = f"""Based on this error chain, identify the most likely root causes:

Error Chain:
{error_summary}

Context (first errors in file):
{content[:1000]}

Identify root causes in JSON format:
{{
    "root_causes": [
        {{
            "cause": "description",
            "confidence": 0.0-1.0,
            "evidence": "what supports this"
        }}
    ]
}}"""

        response = self.generate_with_model(prompt, model, temperature=0.2)
        
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    data = json.loads(json_match.group(0))
                    return data.get("root_causes", [])
            except:
                pass
        
        return []
    
    def _assess_data_loss_risk(self, content: str) -> str:
        """Assess risk of data loss"""
        high_risk_indicators = [
            r'(?i)data.*corrupt',
            r'(?i)database.*fail',
            r'(?i)transaction.*rollback',
            r'(?i)write.*fail',
            r'(?i)disk.*full'
        ]
        
        risk_count = sum(1 for pattern in high_risk_indicators if re.search(pattern, content))
        
        if risk_count >= 3:
            return "HIGH"
        elif risk_count >= 1:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _assess_service_disruption(self, content: str) -> Dict[str, Any]:
        """Assess service disruption impact"""
        disruption = {
            "severity": "UNKNOWN",
            "affected_services": [],
            "downtime_estimate": "Unknown"
        }
        
        # Check for service indicators
        if re.search(r'(?i)(service.*down|unavailable|offline)', content):
            disruption["severity"] = "HIGH"
            
        if re.search(r'(?i)(all.*services|cluster.*fail|system.*down)', content):
            disruption["severity"] = "CRITICAL"
        
        # Extract affected services
        service_pattern = r'(?i)(?:service|api|endpoint):\s*(\w+)'
        services = re.findall(service_pattern, content)
        disruption["affected_services"] = list(set(services))[:10]
        
        return disruption
    
    def _assess_security_impact(self, chunk_results: List[Dict[str, Any]]) -> str:
        """Assess security impact based on chunk results"""
        security_pattern_count = 0
        
        for result in chunk_results:
            for pattern in result.get("patterns", []):
                if pattern["type"] in ["sql_injection", "xss", "buffer_overflow"]:
                    security_pattern_count += 1
        
        if security_pattern_count > 5:
            return "CRITICAL"
        elif security_pattern_count > 2:
            return "HIGH"
        elif security_pattern_count > 0:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _assess_user_impact(self, content: str) -> Dict[str, Any]:
        """Assess impact on users"""
        impact = {
            "affected_users": "Unknown",
            "user_experience": "Unknown",
            "business_impact": "Unknown"
        }
        
        # Look for user indicators
        user_match = re.search(r'(?i)(?:users?|customers?):\s*(\d+)', content)
        if user_match:
            impact["affected_users"] = user_match.group(1)
        
        # UX impact
        if re.search(r'(?i)(timeout|slow|unresponsive|hang)', content):
            impact["user_experience"] = "Degraded"
        
        if re.search(r'(?i)(crash|error.*page|failed.*load)', content):
            impact["user_experience"] = "Broken"
        
        # Business impact
        if re.search(r'(?i)(revenue|payment|transaction.*fail)', content):
            impact["business_impact"] = "Revenue Loss"
        elif re.search(r'(?i)(signup|registration|onboarding)', content):
            impact["business_impact"] = "Growth Impact"
        
        return impact
    
    def _estimate_recovery_time(self, content: str) -> str:
        """Estimate recovery time based on patterns"""
        # Look for recovery timestamps
        recovery_pattern = r'(?i)recover.*?(\d+)\s*(seconds?|minutes?|hours?)'
        match = re.search(recovery_pattern, content)
        
        if match:
            return f"{match.group(1)} {match.group(2)}"
        
        # Check for restart indicators
        if re.search(r'(?i)(restart|reboot|reinitializ)', content):
            return "5-10 minutes (estimated)"
        
        return "Unknown"
    
    def _analyze_resource_usage(self, content: str) -> Dict[str, Any]:
        """Analyze resource usage patterns"""
        usage = {
            "memory_usage_percent": 0,
            "cpu_usage_percent": 0,
            "disk_usage_percent": 0,
            "thread_count": 0
        }
        
        # Memory usage
        mem_pattern = r'(?i)memory.*?(\d+)\s*%'
        mem_match = re.search(mem_pattern, content)
        if mem_match:
            usage["memory_usage_percent"] = int(mem_match.group(1))
        
        # CPU usage
        cpu_pattern = r'(?i)cpu.*?(\d+)\s*%'
        cpu_match = re.search(cpu_pattern, content)
        if cpu_match:
            usage["cpu_usage_percent"] = int(cpu_match.group(1))
        
        # Thread count
        thread_pattern = r'(?i)threads?:\s*(\d+)'
        thread_match = re.search(thread_pattern, content)
        if thread_match:
            usage["thread_count"] = int(thread_match.group(1))
        
        return usage
    
    def _find_optimization_opportunities(self, content: str, 
                                       chunk_results: List[Dict[str, Any]]) -> List[str]:
        """Find specific optimization opportunities"""
        opportunities = []
        
        # Check for common performance issues
        if re.search(r'(?i)n\+1\s*quer', content):
            opportunities.append("Optimize N+1 query problem")
        
        if re.search(r'(?i)full\s*table\s*scan', content):
            opportunities.append("Add database indexes to prevent full table scans")
        
        if re.search(r'(?i)synchronized.*method|lock.*contention', content):
            opportunities.append("Review synchronization strategy to reduce contention")
        
        # From chunk analysis
        total_memory_issues = sum(
            result.get("metrics", {}).get("performance", {}).get("memory_issues", 0)
            for result in chunk_results
        )
        
        if total_memory_issues > 5:
            opportunities.append("Implement memory pooling or caching strategy")
        
        return opportunities[:10]  # Top 10 opportunities
    
    def _generate_preventive_measures(self, results: Dict[str, Any]) -> List[Dict[str, str]]:
        """Generate preventive measures based on analysis"""
        measures = []
        
        # Based on crash analysis
        if results["crash_analysis"]["error_taxonomy"]:
            measures.append({
                "measure": "Implement comprehensive error monitoring",
                "rationale": "Multiple error types detected",
                "priority": "HIGH"
            })
        
        # Based on security
        if results["security_analysis"]["security_score"] < 70:
            measures.append({
                "measure": "Security audit and hardening",
                "rationale": f"Low security score: {results['security_analysis']['security_score']}",
                "priority": "CRITICAL"
            })
        
        # Based on performance
        if results["performance_analysis"]["bottlenecks"]:
            measures.append({
                "measure": "Performance testing and capacity planning",
                "rationale": "Multiple bottlenecks identified",
                "priority": "HIGH"
            })
        
        return measures
    
    def format_output(self, results: Dict[str, Any]) -> str:
        """Format deep forensic analysis results"""
        output = f"""# Deep Forensic Analysis Report

## Executive Overview
- **Total Chunks Analyzed**: {results['overview'].get('total_chunks', 0)}
- **File Size**: {results['overview'].get('file_size', 0):,} bytes
- **Overall Confidence**: {results.get('confidence', 0):.1%}
- **Analysis Dimensions**: {sum(1 for k, v in results.items() if v and k not in ['overview', 'confidence', 'formatted_output'])}

## Critical Findings Summary

### Immediate Threats ({len(results['critical_findings'].get('immediate_threats', []))})"""
        
        for threat in results['critical_findings'].get('immediate_threats', [])[:5]:
            output += f"\n- **{threat.get('type', 'Unknown')}**: {threat.get('issue', 'Unknown')} - *{threat.get('action', 'Review')}*"
        
        output += f"\n\n### Security Critical ({len(results['critical_findings'].get('security_critical', []))})"
        for item in results['critical_findings'].get('security_critical', [])[:3]:
            output += f"\n- {item.get('finding', 'Unknown')} (Score: {item.get('score', 'N/A')})"
        
        output += f"\n\n## Crash Analysis"
        
        # Error taxonomy
        if results['crash_analysis'].get('error_taxonomy'):
            output += f"\n\n### Error Distribution"
            for error_type, occurrences in list(results['crash_analysis']['error_taxonomy'].items())[:10]:
                output += f"\n- **{error_type}**: {len(occurrences)} occurrences"
        
        # Impact assessment
        impact = results['crash_analysis'].get('impact_assessment', {})
        output += f"""

### Impact Assessment
- **Data Loss Risk**: {impact.get('data_loss_risk', 'Unknown')}
- **Service Disruption**: {impact.get('service_disruption', {}).get('severity', 'Unknown')}
- **Security Impact**: {impact.get('security_impact', 'Unknown')}
- **User Impact**: {impact.get('user_impact', {}).get('user_experience', 'Unknown')}"""
        
        # Performance analysis
        output += f"\n\n## Performance Analysis"
        
        if results['performance_analysis'].get('bottlenecks'):
            output += f"\n\n### Bottlenecks Identified"
            for bottleneck in results['performance_analysis']['bottlenecks']:
                output += f"\n- **{bottleneck['type']}** ({bottleneck['severity']}): {bottleneck.get('recommendation', 'Optimize')}"
        
        # Security analysis
        security = results.get('security_analysis', {})
        output += f"""

## Security Analysis
- **Security Score**: {security.get('security_score', 0)}/100
- **Vulnerabilities Found**: {len(security.get('vulnerabilities', []))}
- **Exposed Secrets**: {len(security.get('exposed_secrets', []))}"""
        
        if security.get('vulnerabilities'):
            output += f"\n\n### Critical Vulnerabilities"
            for vuln in security['vulnerabilities'][:5]:
                output += f"\n- **{vuln['type']}** ({vuln['severity']})"
        
        # Code quality
        quality = results.get('code_quality', {})
        if quality.get('anti_patterns'):
            output += f"\n\n## Code Quality Issues"
            for pattern in quality['anti_patterns'][:5]:
                output += f"\n- {pattern['pattern']}: {pattern['occurrences']} occurrences ({pattern['severity']})"
        
        # Predictive insights
        predictive = results.get('predictive_insights', {})
        if predictive.get('future_risks'):
            output += f"\n\n## Predictive Insights\n\n### Future Risks"
            for risk in predictive['future_risks'][:3]:
                output += f"\n- **{risk['risk']}** - Timeframe: {risk['timeframe']} (Probability: {risk['probability']})"
        
        # Comprehensive recommendations
        recs = results.get('comprehensive_recommendations', {})
        output += f"\n\n## Comprehensive Recommendations"
        
        if recs.get('immediate_actions'):
            output += f"\n\n### Immediate Actions Required"
            for i, action in enumerate(recs['immediate_actions'][:5], 1):
                output += f"\n{i}. {action}"
        
        if recs.get('architectural'):
            output += f"\n\n### Architectural Changes"
            for change in recs['architectural']:
                output += f"\n- {change}"
        
        if recs.get('process_improvements'):
            output += f"\n\n### Process Improvements"
            for improvement in recs['process_improvements']:
                output += f"\n- {improvement}"
        
        # Visual elements
        if results.get('visual_analysis'):
            output += f"\n\n## Visual Analysis"
            for visual_name, visual_content in results['visual_analysis'].items():
                if visual_content:
                    output += f"\n\n### {visual_name.replace('_', ' ').title()}"
                    output += f"\n```mermaid\n{visual_content}\n```"
        
        output += f"\n\n---\n*Deep forensic analysis completed in {results.get('processing_time', 0):.2f} seconds*"
        
        return output