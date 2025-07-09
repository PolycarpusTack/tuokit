"""
Strategic Sampling Analyzer
Intelligent analysis of large files without full processing
"""
import re
import json
import time
import random
from typing import Dict, Any, List, Optional, Tuple, Set
from collections import defaultdict, Counter
from datetime import datetime

from .base import BaseAnalyzer
from ..config.settings import ANALYSIS_METHODS, MODEL_PREFERENCES, SAMPLING_STRATEGY
from ..utils.extractors import (
    extract_smart_context, 
    extract_error_chain,
    extract_performance_indicators,
    extract_security_concerns
)

class StrategicSamplingAnalyzer(BaseAnalyzer):
    """Smart sampling for large files (1-5MB) with statistical confidence"""
    
    def __init__(self):
        super().__init__(ANALYSIS_METHODS["strategic_sampling"])
        self.model_config = MODEL_PREFERENCES.get("strategic_sampling", {})
        self.sampling_config = SAMPLING_STRATEGY
        
    def analyze(self, content: str, filename: str, selected_model: str = None, **kwargs) -> Dict[str, Any]:
        """
        Perform strategic sampling analysis on large files
        
        Args:
            content: Crash dump content
            filename: Name of the file
            selected_model: User-selected AI model
            
        Returns:
            Analysis with statistical confidence metrics
        """
        # Pre-analysis
        metrics = self.pre_analysis(content)
        file_size = len(content)
        
        results = {
            "sampling_coverage": {},
            "key_findings": {},
            "patterns_detected": {},
            "confidence_metrics": {},
            "recommendations": {},
            "performance_insights": {},
            "security_scan": {},
            "next_steps": []
        }
        
        # Step 1: Create sampling map
        sampling_map = self._create_sampling_map(content)
        results["sampling_coverage"] = {
            "total_size": file_size,
            "sampled_size": sampling_map["total_sampled"],
            "coverage_percent": (sampling_map["total_sampled"] / file_size) * 100,
            "sections_analyzed": len(sampling_map["sections"])
        }
        
        # Step 2: Extract priority sections
        priority_sections = self._extract_priority_sections(content, sampling_map)
        
        # Step 3: Analyze each section
        section_analyses = {}
        for section_name, section_content in priority_sections.items():
            if selected_model:
                analysis = self._analyze_section_with_ai(
                    section_content, 
                    section_name,
                    selected_model
                )
            else:
                analysis = self._analyze_section_algorithmic(
                    section_content,
                    section_name
                )
            section_analyses[section_name] = analysis
        
        # Step 4: Aggregate findings
        results["key_findings"] = self._aggregate_findings(section_analyses)
        
        # Step 5: Pattern detection across samples
        results["patterns_detected"] = self._detect_patterns_across_samples(
            priority_sections,
            section_analyses
        )
        
        # Step 6: Performance analysis
        results["performance_insights"] = self._analyze_performance_indicators(
            priority_sections
        )
        
        # Step 7: Security scan
        results["security_scan"] = self._security_quick_scan(priority_sections)
        
        # Step 8: Calculate statistical confidence
        results["confidence_metrics"] = self._calculate_sampling_confidence(
            sampling_map,
            results
        )
        
        # Step 9: Generate recommendations
        results["recommendations"] = self._generate_strategic_recommendations(
            results,
            selected_model
        )
        
        # Step 10: Suggest next steps
        results["next_steps"] = self._suggest_next_steps(results, file_size)
        
        # Add formatted output
        results["formatted_output"] = self.format_output(results)
        
        # Post-analysis
        return self.post_analysis(results, content, filename)
    
    def _create_sampling_map(self, content: str) -> Dict[str, Any]:
        """Create intelligent sampling map of the file"""
        sampling_map = {
            "sections": [],
            "total_sampled": 0,
            "error_clusters": [],
            "time_gaps": [],
            "anomalies": []
        }
        
        # Find error clusters
        error_positions = []
        for match in re.finditer(r'(?i)(error|exception|fail|crash)', content):
            error_positions.append(match.start())
        
        # Cluster nearby errors
        if error_positions:
            clusters = []
            current_cluster = [error_positions[0]]
            
            for pos in error_positions[1:]:
                if pos - current_cluster[-1] < 1000:  # Within 1KB
                    current_cluster.append(pos)
                else:
                    if len(current_cluster) >= self.sampling_config["anomaly_detection"]["error_cluster_threshold"]:
                        clusters.append({
                            "start": current_cluster[0],
                            "end": current_cluster[-1],
                            "density": len(current_cluster)
                        })
                    current_cluster = [pos]
            
            # Don't forget the last cluster
            if len(current_cluster) >= self.sampling_config["anomaly_detection"]["error_cluster_threshold"]:
                clusters.append({
                    "start": current_cluster[0],
                    "end": current_cluster[-1],
                    "density": len(current_cluster)
                })
            
            sampling_map["error_clusters"] = clusters
        
        # Find time gaps
        timestamp_pattern = r'(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2})'
        timestamps = []
        for match in re.finditer(timestamp_pattern, content):
            timestamps.append({
                "position": match.start(),
                "time": match.group(1)
            })
        
        # Detect gaps with proper time parsing
        if len(timestamps) > 1:
            for i in range(1, len(timestamps)):
                try:
                    # Parse timestamps for real time gap detection
                    time1 = datetime.fromisoformat(timestamps[i-1]["time"].replace(" ", "T"))
                    time2 = datetime.fromisoformat(timestamps[i]["time"].replace(" ", "T"))
                    time_diff = (time2 - time1).total_seconds()
                    
                    # Check for time gaps (configurable threshold)
                    time_gap_threshold = self.sampling_config["anomaly_detection"]["time_gap_threshold"]
                    
                    if time_diff > time_gap_threshold:
                        sampling_map["time_gaps"].append({
                            "before": timestamps[i-1]["position"],
                            "after": timestamps[i]["position"],
                            "gap_size": timestamps[i]["position"] - timestamps[i-1]["position"],
                            "time_gap_seconds": time_diff,
                            "before_time": timestamps[i-1]["time"],
                            "after_time": timestamps[i]["time"]
                        })
                except (ValueError, AttributeError):
                    # Fallback to position-based gap detection
                    if timestamps[i]["position"] - timestamps[i-1]["position"] > 10000:  # 10KB gap
                        sampling_map["time_gaps"].append({
                            "before": timestamps[i-1]["position"],
                            "after": timestamps[i]["position"],
                            "gap_size": timestamps[i]["position"] - timestamps[i-1]["position"]
                        })
        
        # Define sections to sample
        for config in self.sampling_config["priority_sections"]:
            if config["position"] == "start":
                sampling_map["sections"].append({
                    "name": config["name"],
                    "start": 0,
                    "size": min(config["size"], len(content))
                })
            elif config["position"] == "end":
                sampling_map["sections"].append({
                    "name": config["name"],
                    "start": max(0, len(content) - config["size"]),
                    "size": min(config["size"], len(content))
                })
            elif config["position"] == "around_errors":
                # Sample around error clusters
                for cluster in sampling_map["error_clusters"][:5]:  # Top 5 clusters
                    sampling_map["sections"].append({
                        "name": f"error_cluster_{cluster['start']}",
                        "start": max(0, cluster["start"] - config["size"] // 2),
                        "size": config["size"]
                    })
        
        # Calculate total sampled with overlap deduplication
        sampling_map["total_sampled"] = self._calculate_unique_coverage(sampling_map["sections"])
        
        return sampling_map
    
    def _calculate_unique_coverage(self, sections: List[Dict[str, Any]]) -> int:
        """Calculate total unique bytes covered, handling overlaps"""
        if not sections:
            return 0
        
        # Convert sections to intervals (start, end)
        intervals = []
        for section in sections:
            start = section["start"]
            end = start + section["size"]
            intervals.append((start, end))
        
        # Sort intervals by start position
        intervals.sort()
        
        # Merge overlapping intervals
        merged = [intervals[0]]
        for current in intervals[1:]:
            last_start, last_end = merged[-1]
            current_start, current_end = current
            
            if current_start <= last_end:
                # Overlapping intervals - merge them
                merged[-1] = (last_start, max(last_end, current_end))
            else:
                # Non-overlapping - add as new interval
                merged.append(current)
        
        # Calculate total unique coverage
        total_coverage = sum(end - start for start, end in merged)
        return total_coverage
    
    def _extract_priority_sections(self, content: str, sampling_map: Dict[str, Any]) -> Dict[str, str]:
        """Extract the priority sections based on sampling map"""
        sections = {}
        
        for section in sampling_map["sections"]:
            start = section["start"]
            end = min(start + section["size"], len(content))
            sections[section["name"]] = content[start:end]
        
        return sections
    
    def _analyze_section_with_ai(self, section: str, section_name: str, model: str) -> Dict[str, Any]:
        """Analyze a section using AI model"""
        prompt = f"""Analyze this section from a crash dump (section: {section_name}).
Focus on identifying errors, patterns, and potential issues.

Section content:
{section[:2000]}  # Limit for AI context

Provide analysis in JSON format:
{{
    "primary_issues": ["issue 1", "issue 2"],
    "error_types": ["type 1", "type 2"],
    "severity": "CRITICAL/HIGH/MEDIUM/LOW",
    "patterns": ["pattern 1", "pattern 2"],
    "recommendations": ["recommendation 1"]
}}"""

        response = self.generate_with_model(prompt, model, temperature=0.2)
        
        if response:
            try:
                json_match = re.search(r'\{.*\}', response, re.DOTALL)
                if json_match:
                    return json.loads(json_match.group(0))
            except:
                pass
        
        # Fallback to algorithmic analysis
        return self._analyze_section_algorithmic(section, section_name)
    
    def _analyze_section_algorithmic(self, section: str, section_name: str) -> Dict[str, Any]:
        """Algorithmic analysis of a section"""
        analysis = {
            "primary_issues": [],
            "error_types": [],
            "severity": "MEDIUM",
            "patterns": [],
            "recommendations": []
        }
        
        # Count error types
        error_counts = Counter()
        for pattern, error_type in [
            (r'NullPointerException', 'null_pointer'),
            (r'OutOfMemoryError', 'memory'),
            (r'StackOverflowError', 'stack_overflow'),
            (r'IOException', 'io_error'),
            (r'SQLException', 'database'),
            (r'TimeoutException', 'timeout'),
            (r'SecurityException', 'security')
        ]:
            count = len(re.findall(pattern, section))
            if count > 0:
                error_counts[error_type] = count
                analysis["error_types"].append(error_type)
        
        # Determine severity
        total_errors = sum(error_counts.values())
        if total_errors > 10 or 'security' in error_counts:
            analysis["severity"] = "CRITICAL"
        elif total_errors > 5:
            analysis["severity"] = "HIGH"
        elif total_errors > 0:
            analysis["severity"] = "MEDIUM"
        else:
            analysis["severity"] = "LOW"
        
        # Extract primary issues
        if error_counts:
            top_errors = error_counts.most_common(3)
            for error_type, count in top_errors:
                analysis["primary_issues"].append(f"{error_type}: {count} occurrences")
        
        # Detect patterns
        if re.search(r'at\s+\w+\.\w+\(.*\).*\n\s+at\s+\w+\.\w+\(.*\)', section):
            analysis["patterns"].append("stack_trace_detected")
        
        if re.search(r'(?i)(deadlock|thread\s+blocked)', section):
            analysis["patterns"].append("concurrency_issue")
        
        if re.search(r'(?i)connection\s+(refused|timeout|closed)', section):
            analysis["patterns"].append("network_issue")
        
        # Basic recommendations
        if 'memory' in error_counts:
            analysis["recommendations"].append("Increase heap size or optimize memory usage")
        if 'timeout' in error_counts:
            analysis["recommendations"].append("Review timeout settings and system performance")
        if 'security' in error_counts:
            analysis["recommendations"].append("URGENT: Security exception requires immediate review")
        
        return analysis
    
    def _aggregate_findings(self, section_analyses: Dict[str, Dict[str, Any]]) -> Dict[str, Any]:
        """Aggregate findings across all sections"""
        aggregated = {
            "total_issues": 0,
            "critical_sections": [],
            "common_errors": Counter(),
            "severity_distribution": Counter(),
            "top_recommendations": []
        }
        
        all_recommendations = []
        
        for section_name, analysis in section_analyses.items():
            # Count issues
            aggregated["total_issues"] += len(analysis.get("primary_issues", []))
            
            # Track critical sections
            if analysis.get("severity") in ["CRITICAL", "HIGH"]:
                aggregated["critical_sections"].append({
                    "section": section_name,
                    "severity": analysis["severity"],
                    "issues": analysis.get("primary_issues", [])[:2]
                })
            
            # Aggregate error types
            for error_type in analysis.get("error_types", []):
                aggregated["common_errors"][error_type] += 1
            
            # Severity distribution
            aggregated["severity_distribution"][analysis.get("severity", "UNKNOWN")] += 1
            
            # Collect recommendations
            all_recommendations.extend(analysis.get("recommendations", []))
        
        # Get top recommendations (deduplicated)
        rec_counts = Counter(all_recommendations)
        aggregated["top_recommendations"] = [rec for rec, _ in rec_counts.most_common(5)]
        
        # Convert Counters to dicts for JSON serialization
        aggregated["common_errors"] = dict(aggregated["common_errors"].most_common(10))
        aggregated["severity_distribution"] = dict(aggregated["severity_distribution"])
        
        return aggregated
    
    def _detect_patterns_across_samples(self, sections: Dict[str, str], analyses: Dict[str, Dict[str, Any]]) -> Dict[str, Any]:
        """Detect patterns across multiple samples"""
        patterns = {
            "recurring_errors": {},
            "error_sequences": [],
            "component_correlation": {},
            "temporal_patterns": []
        }
        
        # Find recurring errors across sections
        all_errors = []
        for section_content in sections.values():
            # Extract error messages
            error_matches = re.finditer(
                r'(?i)(?:error|exception):\s*([^\n]+)', 
                section_content
            )
            for match in error_matches:
                all_errors.append(match.group(1).strip())
        
        # Count recurring errors
        error_counts = Counter(all_errors)
        patterns["recurring_errors"] = dict(error_counts.most_common(5))
        
        # Detect error sequences
        for section_name, section_content in sections.items():
            chain = extract_error_chain(section_content)
            if len(chain) > 2:
                patterns["error_sequences"].append({
                    "section": section_name,
                    "sequence": [e.get("error", "Unknown") for e in chain[:5]]
                })
        
        # Component correlation
        component_errors = defaultdict(list)
        for section_content in sections.values():
            # Extract component names from stack traces
            component_matches = re.finditer(r'at\s+([\w\.]+)\.[^\(]+\(', section_content)
            components = set()
            for match in component_matches:
                component = match.group(1)
                # Get package name (first 2-3 parts)
                parts = component.split('.')
                if len(parts) > 2:
                    component = '.'.join(parts[:3])
                components.add(component)
            
            # Associate errors with components
            for component in components:
                for error_type, count in error_counts.items():
                    if count > 2:  # Significant errors only
                        component_errors[component].append(error_type)
        
        # Top correlated components
        patterns["component_correlation"] = {
            comp: list(set(errors)[:3]) 
            for comp, errors in sorted(
                component_errors.items(), 
                key=lambda x: len(x[1]), 
                reverse=True
            )[:5]
        }
        
        return patterns
    
    def _analyze_performance_indicators(self, sections: Dict[str, str]) -> Dict[str, Any]:
        """Analyze performance indicators from samples"""
        performance = {
            "slow_operations": [],
            "memory_pressure": False,
            "thread_issues": [],
            "resource_bottlenecks": []
        }
        
        # Analyze each section for performance issues
        for section_name, section_content in sections.items():
            indicators = extract_performance_indicators(section_content)
            
            # Aggregate slow queries
            for query in indicators.get("slow_queries", []):
                if query["time_ms"] > 1000:
                    performance["slow_operations"].append({
                        "type": "query",
                        "duration_ms": query["time_ms"],
                        "section": section_name
                    })
            
            # Check for memory pressure
            if indicators.get("memory_issues"):
                performance["memory_pressure"] = True
                performance["resource_bottlenecks"].append("memory")
            
            # Thread issues
            for issue in indicators.get("thread_issues", []):
                performance["thread_issues"].append({
                    "type": issue["type"],
                    "section": section_name
                })
            
            # Timeout patterns
            if len(indicators.get("timeouts", [])) > 2:
                performance["resource_bottlenecks"].append("response_time")
        
        # Deduplicate bottlenecks
        performance["resource_bottlenecks"] = list(set(performance["resource_bottlenecks"]))
        
        return performance
    
    def _security_quick_scan(self, sections: Dict[str, str]) -> Dict[str, Any]:
        """Quick security scan of sampled sections"""
        security = {
            "concerns_found": False,
            "critical_findings": [],
            "recommendations": []
        }
        
        all_concerns = []
        
        for section_name, section_content in sections.items():
            concerns = extract_security_concerns(section_content)
            if concerns:
                security["concerns_found"] = True
                for concern in concerns:
                    all_concerns.append({
                        "type": concern["type"],
                        "section": section_name,
                        "severity": "CRITICAL" if "password" in concern["type"] or "key" in concern["type"] else "HIGH"
                    })
        
        # Get most critical findings
        security["critical_findings"] = sorted(
            all_concerns,
            key=lambda x: 0 if x["severity"] == "CRITICAL" else 1
        )[:5]
        
        # Security recommendations
        if security["concerns_found"]:
            security["recommendations"] = [
                "Immediate review of security exposure required",
                "Sanitize logs before sharing",
                "Review credential management practices",
                "Enable log redaction for sensitive data"
            ]
        
        return security
    
    def _calculate_sampling_confidence(self, sampling_map: Dict[str, Any], results: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate statistical confidence in sampling results"""
        confidence_metrics = {
            "coverage_score": 0.0,
            "pattern_confidence": 0.0,
            "error_detection_confidence": 0.0,
            "overall_confidence": 0.0,
            "confidence_factors": []
        }
        
        # Coverage score (0-1)
        coverage_percent = results["sampling_coverage"]["coverage_percent"]
        if coverage_percent > 20:
            confidence_metrics["coverage_score"] = 0.9
            confidence_metrics["confidence_factors"].append("High coverage (>20%)")
        elif coverage_percent > 10:
            confidence_metrics["coverage_score"] = 0.7
            confidence_metrics["confidence_factors"].append("Good coverage (10-20%)")
        else:
            confidence_metrics["coverage_score"] = 0.5
            confidence_metrics["confidence_factors"].append("Limited coverage (<10%)")
        
        # Pattern confidence based on recurrence
        recurring_errors = results["patterns_detected"].get("recurring_errors", {})
        if any(count > 5 for count in recurring_errors.values()):
            confidence_metrics["pattern_confidence"] = 0.9
            confidence_metrics["confidence_factors"].append("Strong pattern recurrence")
        elif any(count > 2 for count in recurring_errors.values()):
            confidence_metrics["pattern_confidence"] = 0.7
            confidence_metrics["confidence_factors"].append("Moderate pattern recurrence")
        else:
            confidence_metrics["pattern_confidence"] = 0.4
        
        # Error detection confidence
        if sampling_map.get("error_clusters"):
            confidence_metrics["error_detection_confidence"] = 0.85
            confidence_metrics["confidence_factors"].append("Error clustering detected")
        else:
            confidence_metrics["error_detection_confidence"] = 0.6
        
        # Overall confidence (weighted average)
        confidence_metrics["overall_confidence"] = (
            confidence_metrics["coverage_score"] * 0.3 +
            confidence_metrics["pattern_confidence"] * 0.4 +
            confidence_metrics["error_detection_confidence"] * 0.3
        )
        
        return confidence_metrics
    
    def _generate_strategic_recommendations(self, results: Dict[str, Any], model: str = None) -> Dict[str, Any]:
        """Generate strategic recommendations based on sampling"""
        recommendations = {
            "immediate_actions": [],
            "investigation_priorities": [],
            "optimization_opportunities": [],
            "risk_mitigation": []
        }
        
        # Immediate actions based on severity
        severity_dist = results["key_findings"].get("severity_distribution", {})
        if severity_dist.get("CRITICAL", 0) > 0:
            recommendations["immediate_actions"].append(
                "CRITICAL issues detected - immediate investigation required"
            )
            if results["security_scan"]["concerns_found"]:
                recommendations["immediate_actions"].append(
                    "Security exposure detected - sanitize and secure immediately"
                )
        
        # Investigation priorities
        for section in results["key_findings"].get("critical_sections", [])[:3]:
            recommendations["investigation_priorities"].append(
                f"Deep dive into {section['section']}: {', '.join(section['issues'][:2])}"
            )
        
        # Optimization opportunities
        if results["performance_insights"]["memory_pressure"]:
            recommendations["optimization_opportunities"].append(
                "Memory optimization needed - consider heap analysis"
            )
        
        if results["performance_insights"]["slow_operations"]:
            recommendations["optimization_opportunities"].append(
                "Performance bottlenecks detected - profile slow operations"
            )
        
        # Risk mitigation
        common_errors = results["key_findings"].get("common_errors", {})
        if "null_pointer" in common_errors:
            recommendations["risk_mitigation"].append(
                "Implement null safety checks across identified components"
            )
        
        if results["patterns_detected"].get("error_sequences"):
            recommendations["risk_mitigation"].append(
                "Error cascades detected - implement circuit breakers"
            )
        
        return recommendations
    
    def _suggest_next_steps(self, results: Dict[str, Any], file_size: int) -> List[str]:
        """Suggest next steps based on sampling results"""
        next_steps = []
        
        # Based on confidence
        confidence = results["confidence_metrics"]["overall_confidence"]
        if confidence < 0.6:
            next_steps.append("Consider Deep Forensic Analysis for comprehensive coverage")
        elif confidence < 0.8:
            next_steps.append("Run targeted analysis on critical sections identified")
        
        # Based on findings
        if results["security_scan"]["concerns_found"]:
            next_steps.append("Perform security audit and credential rotation")
        
        if results["performance_insights"]["memory_pressure"]:
            next_steps.append("Generate heap dump for memory analysis")
        
        if len(results["patterns_detected"]["error_sequences"]) > 2:
            next_steps.append("Investigate error propagation paths")
        
        # Based on file size
        if file_size > 5 * 1024 * 1024:  # > 5MB
            next_steps.append("Consider splitting file for detailed analysis")
        
        return next_steps
    
    def format_output(self, results: Dict[str, Any]) -> str:
        """Format sampling analysis results"""
        output = f"""# Strategic Sampling Analysis Report

## Coverage Summary
- **File Coverage**: {results['sampling_coverage']['coverage_percent']:.1f}%
- **Sections Analyzed**: {results['sampling_coverage']['sections_analyzed']}
- **Sampled Size**: {results['sampling_coverage']['sampled_size']:,} bytes
- **Confidence**: {results['confidence_metrics']['overall_confidence']:.1%}

## Key Findings

### Severity Distribution"""
        
        for severity, count in results['key_findings'].get('severity_distribution', {}).items():
            output += f"\n- {severity}: {count} sections"
        
        output += f"\n\n### Critical Sections"
        for section in results['key_findings'].get('critical_sections', [])[:5]:
            output += f"\n- **{section['section']}** ({section['severity']})"
            for issue in section.get('issues', []):
                output += f"\n  - {issue}"
        
        output += f"\n\n### Common Error Types"
        for error_type, count in results['key_findings'].get('common_errors', {}).items():
            output += f"\n- {error_type}: {count} occurrences"
        
        output += f"\n\n## Pattern Analysis\n\n### Recurring Errors"
        for error, count in results['patterns_detected'].get('recurring_errors', {}).items():
            output += f"\n- {error}: {count} times"
        
        if results['patterns_detected'].get('error_sequences'):
            output += f"\n\n### Error Sequences Detected"
            for seq in results['patterns_detected']['error_sequences'][:3]:
                output += f"\n- In {seq['section']}: {' → '.join(seq['sequence'][:3])}"
        
        output += f"\n\n## Performance Insights"
        
        if results['performance_insights']['memory_pressure']:
            output += "\n- ⚠️ **Memory pressure detected**"
        
        if results['performance_insights']['slow_operations']:
            output += "\n- 🐌 **Slow operations found:**"
            for op in results['performance_insights']['slow_operations'][:5]:
                output += f"\n  - {op['type']}: {op['duration_ms']}ms in {op['section']}"
        
        if results['performance_insights']['resource_bottlenecks']:
            output += f"\n- 🔥 **Resource bottlenecks**: {', '.join(results['performance_insights']['resource_bottlenecks'])}"
        
        if results['security_scan']['concerns_found']:
            output += f"\n\n## 🔒 Security Scan\n\n**⚠️ SECURITY CONCERNS FOUND**"
            for finding in results['security_scan']['critical_findings'][:3]:
                output += f"\n- {finding['type']} in {finding['section']} ({finding['severity']})"
        
        output += f"\n\n## Recommendations\n\n### Immediate Actions"
        for action in results['recommendations']['immediate_actions']:
            output += f"\n1. {action}"
        
        output += f"\n\n### Investigation Priorities"
        for priority in results['recommendations']['investigation_priorities']:
            output += f"\n- {priority}"
        
        output += f"\n\n### Next Steps"
        for step in results['next_steps']:
            output += f"\n- {step}"
        
        output += f"\n\n## Confidence Factors"
        for factor in results['confidence_metrics']['confidence_factors']:
            output += f"\n- {factor}"
        
        output += f"\n\n---\n*Strategic sampling completed in {results.get('processing_time', 0):.2f} seconds*"
        
        return output