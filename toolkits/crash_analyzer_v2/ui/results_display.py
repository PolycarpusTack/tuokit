"""
Results Display UI for Crash Analyzer V2
Handles display of analysis results for all methods
"""
import streamlit as st
from typing import Dict, Any
import json
from datetime import datetime

from ..config import VISUAL_ELEMENTS, OUTPUT_TEMPLATES
from utils.annotated_text_helpers import (
    annotated_text, parse_error_message, parse_stack_trace,
    highlight_severity, display_annotated_error, ANNOTATION_COLORS
)

def display_analysis_results(results: Dict[str, Any], method: str, db=None, analyzer=None):
    """
    Display analysis results based on the method used
    
    Args:
        results: Analysis results dictionary
        method: Analysis method used
    """
    if results.get("error"):
        st.error(f"Analysis Error: {results['error']}")
        return
    
    # Debug info for deep forensic
    if method == "deep_forensic" and len(results) < 5:
        st.warning("⚠️ Deep Forensic analysis returned minimal results. This can happen with very long analysis times.")
        with st.expander("🔍 Debug Information", expanded=True):
            st.write(f"Result keys: {list(results.keys())}")
            st.write(f"Processing time: {results.get('processing_time', 'Unknown')}s")
            if 'formatted_output' in results:
                st.info("Formatted output is available")
    
    # Get display configuration
    display_config = OUTPUT_TEMPLATES.get(method, {})
    
    # Method-specific display
    if method == "quick_triage":
        _display_quick_triage_results(results)
    elif method == "root_cause":
        _display_root_cause_results(results)
    elif method == "strategic_sampling":
        _display_strategic_sampling_results(results)
    elif method == "deep_forensic":
        _display_deep_forensic_results(results)
    elif method == "enterprise_report":
        _display_enterprise_report_results(results)
    else:
        # Generic display
        _display_generic_results(results)
    
    # Common elements
    _display_common_elements(results, method, db, analyzer)

def _display_quick_triage_results(results: Dict[str, Any]):
    """Display Quick Triage results"""
    st.markdown("### ⚡ Quick Triage Results")
    
    # Severity indicator
    severity = results.get("severity", "Unknown")
    severity_score = results.get("severity_score", 0)
    severity_config = VISUAL_ELEMENTS["severity_indicators"].get(
        severity.upper(), 
        VISUAL_ELEMENTS["severity_indicators"]["MEDIUM"]
    )
    
    # Main metrics row
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric(
            "Severity",
            f"{severity_config['emoji']} {severity}",
            f"Score: {severity_score}/10"
        )
        # Also show with color annotation
        annotated_text(highlight_severity(severity))
    
    with col2:
        st.metric("Error Type", results.get("error_type", "Unknown"))
    
    with col3:
        st.metric("Confidence", results.get("confidence", "LOW"))
    
    with col4:
        similar = results.get("similar_crashes", 0)
        st.metric("Similar (24h)", similar)
    
    # Key findings box
    with st.container():
        st.markdown("#### 🎯 Key Findings")
        
        # Immediate cause with annotated text
        immediate_cause = results.get('immediate_cause', 'Not detected')
        st.markdown("**Immediate Cause:**")
        cause_components = parse_error_message(immediate_cause)
        annotated_text(*cause_components)
        
        # Location with annotated text
        if results.get("error_location", "Unknown") != "Unknown":
            st.markdown("**Location:**")
            location = results['error_location']
            # Parse file path and line number
            if ':' in location:
                parts = location.split(':')
                if len(parts) >= 2:
                    file_path = parts[0]
                    line_num = parts[1]
                    annotated_text(
                        (file_path, "file_path", ANNOTATION_COLORS["file_path"]),
                        ":",
                        (line_num, "line_number", ANNOTATION_COLORS["line_number"])
                    )
                else:
                    st.code(location)
            else:
                st.code(location)
        
        # Impact
        st.write(f"**Impact**: {results.get('impact', 'Unknown')}")
        
        # Quick fix
        st.success(f"**Quick Fix**: {results.get('quick_fix', 'Requires analysis')}")
    
    # Patterns detected
    if results.get("patterns_matched"):
        with st.expander("🔍 Patterns Detected", expanded=True):
            for pattern in results["patterns_matched"]:
                st.write(f"• **{pattern['pattern']}** ({pattern['category']})")
    
    # Severity factors
    if results.get("severity_factors"):
        with st.expander("📊 Severity Factors", expanded=False):
            for factor in results["severity_factors"][:10]:
                st.write(f"• {factor}")

def _display_root_cause_results(results: Dict[str, Any]):
    """Display Root Cause Analysis results"""
    st.markdown("### 🔍 Root Cause Analysis Results")
    
    # Executive summary
    if results.get("executive_summary"):
        exec_summary = results["executive_summary"]
        
        # Alert box for urgency
        urgency = exec_summary.get("urgency", "MEDIUM")
        urgency_colors = {
            "IMMEDIATE": "error",
            "HIGH": "warning",
            "MEDIUM": "info",
            "LOW": "success"
        }
        
        alert_func = getattr(st, urgency_colors.get(urgency, "info"))
        alert_func(f"**{urgency} Priority** - {exec_summary.get('business_impact', 'Unknown impact')}")
        
        # Business metrics
        col1, col2, col3 = st.columns(3)
        with col1:
            st.metric("Affected Users", exec_summary.get("affected_users", "Unknown"))
        with col2:
            st.metric("Revenue Risk", exec_summary.get("revenue_risk", "Unknown"))
        with col3:
            st.metric("Confidence", f"{results.get('confidence', 0):.0%}")
    
    # Technical analysis tabs
    tab1, tab2, tab3, tab4 = st.tabs(["🔬 Technical Analysis", "💡 Recommendations", "🛡️ Prevention", "📊 Visuals"])
    
    with tab1:
        tech = results.get("technical_analysis", {})
        
        # Root cause
        st.markdown("#### Root Cause")
        st.error(tech.get("root_cause", "Not identified"))
        
        # Error chain
        st.markdown("#### Error Chain")
        for i, step in enumerate(tech.get("error_chain", []), 1):
            st.write(f"{i}. {step}")
        
        # Contributing factors
        st.markdown("#### Contributing Factors")
        for factor in tech.get("contributing_factors", []):
            st.write(f"• {factor}")
        
        # Technical details
        with st.expander("Technical Details", expanded=False):
            st.write(tech.get("technical_details", "No details available"))
    
    with tab2:
        recs = results.get("recommendations", {})
        
        # Immediate actions
        st.markdown("#### 🚨 Immediate Actions")
        for action in recs.get("immediate_actions", []):
            st.write(f"1. {action}")
        
        # Short-term fixes
        st.markdown("#### 🔧 Short-term Fixes (48 hours)")
        for fix in recs.get("short_term_fixes", []):
            st.write(f"• {fix}")
        
        # Long-term improvements
        st.markdown("#### 🏗️ Long-term Improvements")
        for improvement in recs.get("long_term_improvements", []):
            st.write(f"• {improvement}")
        
        # Monitoring
        st.markdown("#### 📊 Monitoring Additions")
        for monitor in recs.get("monitoring_additions", []):
            st.write(f"• {monitor}")
    
    with tab3:
        prev = results.get("prevention_strategy", {})
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("#### 💻 Code Changes")
            for change in prev.get("code_changes", []):
                st.write(f"• {change}")
            
            st.markdown("#### 🧪 Testing Improvements")
            for test in prev.get("testing_improvements", []):
                st.write(f"• {test}")
        
        with col2:
            st.markdown("#### 🚀 Deployment Safeguards")
            for safeguard in prev.get("deployment_safeguards", []):
                st.write(f"• {safeguard}")
            
            st.markdown("#### 🏛️ Architecture")
            for consideration in prev.get("architectural_considerations", []):
                st.write(f"• {consideration}")
    
    with tab4:
        visuals = results.get("visual_elements", {})
        
        # Error flow diagram
        if visuals.get("error_flow"):
            st.markdown("#### Error Flow Diagram")
            st.code(visuals["error_flow"], language="mermaid")
        
        # Other visual placeholders
        if visuals.get("timeline"):
            st.markdown("#### Timeline Visualization")
            st.info(visuals["timeline"])
        
        if visuals.get("impact_radius"):
            st.markdown("#### Impact Radius")
            st.info(visuals["impact_radius"])

def _display_strategic_sampling_results(results: Dict[str, Any]):
    """Display Strategic Sampling results"""
    st.markdown("### 🎯 Strategic Sampling Results")
    
    # Coverage summary
    coverage = results.get("sampling_coverage", {})
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric("Coverage", f"{coverage.get('coverage_percent', 0):.1f}%")
    with col2:
        st.metric("Sections", coverage.get('sections_analyzed', 0))
    with col3:
        st.metric("Confidence", f"{results.get('confidence_metrics', {}).get('overall_confidence', 0):.0%}")
    with col4:
        st.metric("Issues Found", results.get('key_findings', {}).get('total_issues', 0))
    
    # Key findings
    findings = results.get("key_findings", {})
    if findings.get("critical_sections"):
        st.markdown("#### 🚨 Critical Sections")
        for section in findings["critical_sections"][:5]:
            with st.expander(f"{section['section']} - {section['severity']}", expanded=True):
                for issue in section.get('issues', []):
                    st.write(f"• {issue}")
    
    # Pattern analysis
    patterns = results.get("patterns_detected", {})
    if patterns.get("recurring_errors"):
        st.markdown("#### 🔄 Recurring Errors")
        for error, count in list(patterns["recurring_errors"].items())[:5]:
            st.write(f"• {error}: **{count} occurrences**")
    
    # Performance insights
    perf = results.get("performance_insights", {})
    if perf.get("memory_pressure") or perf.get("slow_operations") or perf.get("resource_bottlenecks"):
        st.markdown("#### ⚡ Performance Insights")
        if perf.get("memory_pressure"):
            st.warning("Memory pressure detected")
        if perf.get("slow_operations"):
            st.info(f"{len(perf['slow_operations'])} slow operations detected")
        if perf.get("resource_bottlenecks"):
            st.error(f"Bottlenecks: {', '.join(perf['resource_bottlenecks'])}")
    
    # Security scan
    security = results.get("security_scan", {})
    if security.get("concerns_found"):
        st.markdown("#### 🔒 Security Concerns")
        st.error("Security issues detected - review immediately")
        for finding in security.get("critical_findings", [])[:3]:
            st.write(f"• {finding['type']} in {finding['section']}")
    
    # Next steps
    if results.get("next_steps"):
        st.markdown("#### 📋 Recommended Next Steps")
        for step in results["next_steps"]:
            st.write(f"→ {step}")

def _display_deep_forensic_results(results: Dict[str, Any]):
    """Display Deep Forensic Analysis results"""
    st.markdown("### 🧬 Deep Forensic Analysis Results")
    
    # Debug - show what we received
    with st.expander("🐛 Debug: Raw Results", expanded=False):
        st.write("Results keys:", list(results.keys()))
        if "formatted_output" in results:
            st.text("Has formatted_output")
        st.json({k: type(v).__name__ for k, v in results.items()})
    
    # If we have formatted output, show it as the primary content
    if results.get("formatted_output"):
        # Show the formatted output directly, not in an expander
        st.markdown(results["formatted_output"])
        
        # Still show metrics if available
        if results.get("processing_time"):
            st.divider()
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Processing Time", f"{results.get('processing_time', 0):.1f}s")
            with col2:
                if results.get("confidence"):
                    st.metric("Confidence", f"{results.get('confidence', 0):.0%}")
            with col3:
                if results.get("overview", {}).get("total_chunks"):
                    st.metric("Chunks Analyzed", results["overview"]["total_chunks"])
        return
        
    # Check if results are empty or minimal
    if not results or len(results) <= 5:  # Only has basic fields
        st.warning("⚠️ Analysis completed but detailed results are not available.")
        st.info("The analysis took a very long time. Results may have been truncated.")
        
        # Show what we do have
        if results:
            st.json(results)
        return
        
    # Overview metrics
    overview = results.get("overview", {})
    critical = results.get("critical_findings", {})
    
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Chunks Analyzed", overview.get("total_chunks", 0))
    with col2:
        st.metric("Critical Issues", len(critical.get("immediate_threats", [])))
    with col3:
        st.metric("Security Score", f"{results.get('security_analysis', {}).get('security_score', 0)}/100")
    with col4:
        st.metric("Confidence", f"{results.get('confidence', 0):.0%}")
    
    # Critical findings alert
    if critical.get("immediate_threats"):
        st.error("⚠️ **IMMEDIATE ACTION REQUIRED**")
        for threat in critical["immediate_threats"][:3]:
            st.write(f"• **{threat.get('type', 'Unknown')}**: {threat.get('issue', '')} → {threat.get('action', '')}")
    
    # Multi-dimensional analysis tabs
    tab1, tab2, tab3, tab4, tab5 = st.tabs([
        "🔍 Crash Analysis", 
        "⚡ Performance", 
        "🔒 Security", 
        "📊 Code Quality",
        "🔮 Predictive"
    ])
    
    with tab1:
        crash = results.get("crash_analysis", {})
        if crash.get("error_taxonomy"):
            st.markdown("#### Error Distribution")
            for error_type, occurrences in list(crash["error_taxonomy"].items())[:10]:
                st.write(f"• **{error_type}**: {len(occurrences)} occurrences")
        
        impact = crash.get("impact_assessment", {})
        if impact:
            st.markdown("#### Impact Assessment")
            col1, col2 = st.columns(2)
            with col1:
                st.write(f"• Data Loss Risk: **{impact.get('data_loss_risk', 'Unknown')}**")
                st.write(f"• Security Impact: **{impact.get('security_impact', 'Unknown')}**")
            with col2:
                st.write(f"• Service Disruption: **{impact.get('service_disruption', {}).get('severity', 'Unknown')}**")
                st.write(f"• User Experience: **{impact.get('user_impact', {}).get('user_experience', 'Unknown')}**")
    
    with tab2:
        perf = results.get("performance_analysis", {})
        if perf.get("bottlenecks"):
            st.markdown("#### Performance Bottlenecks")
            for bottleneck in perf["bottlenecks"]:
                st.warning(f"**{bottleneck['type']}** ({bottleneck['severity']}): {bottleneck.get('recommendation', '')}")
        
        if perf.get("resource_usage"):
            st.markdown("#### Resource Usage")
            usage = perf["resource_usage"]
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Memory", f"{usage.get('memory_usage_percent', 0)}%")
            with col2:
                st.metric("CPU", f"{usage.get('cpu_usage_percent', 0)}%")
            with col3:
                st.metric("Threads", usage.get('thread_count', 0))
    
    with tab3:
        security = results.get("security_analysis", {})
        st.metric("Security Score", f"{security.get('security_score', 0)}/100")
        
        if security.get("vulnerabilities"):
            st.markdown("#### Vulnerabilities")
            for vuln in security["vulnerabilities"][:5]:
                severity_color = {"CRITICAL": "🔴", "HIGH": "🟠", "MEDIUM": "🟡"}.get(vuln['severity'], "🔵")
                st.write(f"{severity_color} **{vuln['type']}** ({vuln['severity']})")
        
        if security.get("exposed_secrets"):
            st.error(f"⚠️ {len(security['exposed_secrets'])} exposed secrets detected!")
    
    with tab4:
        quality = results.get("code_quality", {})
        if quality.get("anti_patterns"):
            st.markdown("#### Anti-Patterns Detected")
            for pattern in quality["anti_patterns"][:5]:
                st.write(f"• {pattern['pattern']}: {pattern['occurrences']} occurrences ({pattern['severity']})")
        
        if quality.get("technical_debt"):
            st.markdown("#### Technical Debt")
            for debt in quality["technical_debt"]:
                st.info(f"{debt['indicator']}: {debt.get('count', 'N/A')} - {debt.get('impact', '')}")
    
    with tab5:
        predictive = results.get("predictive_insights", {})
        if predictive.get("future_risks"):
            st.markdown("#### Future Risks")
            for risk in predictive["future_risks"][:3]:
                st.warning(f"**{risk['risk']}** - {risk['timeframe']} ({risk['probability']} probability)")
        
        if predictive.get("trending_issues"):
            st.markdown("#### Trending Issues")
            for issue in predictive["trending_issues"][:5]:
                st.write(f"• {issue['issue']}: {issue['trend']} ({issue['occurrences']} times)")
    
    # Comprehensive recommendations
    recs = results.get("comprehensive_recommendations", {})
    if recs.get("immediate_actions"):
        with st.expander("🚨 Immediate Actions Required", expanded=True):
            for i, action in enumerate(recs["immediate_actions"][:5], 1):
                st.write(f"{i}. {action}")
    
    # Visual analysis
    if results.get("visual_analysis"):
        with st.expander("📊 Visual Analysis", expanded=False):
            for visual_name, visual_content in results["visual_analysis"].items():
                if visual_content:
                    st.markdown(f"#### {visual_name.replace('_', ' ').title()}")
                    st.code(visual_content, language="mermaid")

def _display_enterprise_report_results(results: Dict[str, Any]):
    """Display Enterprise Report results"""
    st.markdown("### 📊 Enterprise Crash Analysis Report")
    
    # If we have formatted output, show it directly
    if results.get("formatted_output"):
        # The analyzer already formatted the report beautifully
        st.markdown(results["formatted_output"])
        
        # Add download button
        st.divider()
        col1, col2 = st.columns([3, 1])
        with col2:
            st.download_button(
                "📥 Download Report",
                results["formatted_output"],
                file_name=f"enterprise_report_{results.get('filename', 'crash')}.md",
                mime="text/markdown",
                use_container_width=True
            )
        return
    
    # Fallback to structured display if no formatted output
    # Executive Summary
    exec_summary = results.get("executive_summary", {})
    if exec_summary:
        with st.container():
            st.markdown("## 1. Executive Summary")
            
            # Main crash info
            st.error(f"**Crash:** {exec_summary.get('crash', 'Unknown error')}")
            
            # Root cause
            root_cause = exec_summary.get("root_cause", {})
            col1, col2 = st.columns(2)
            with col1:
                st.write("**Technical Cause:**")
                st.info(root_cause.get("technical", "Pending analysis"))
            with col2:
                st.write("**Data Cause:**")
                st.info(root_cause.get("data", "N/A"))
            
            # Actionable items
            if exec_summary.get("actionable_items"):
                st.markdown("### Actionable Items")
                for item in exec_summary["actionable_items"]:
                    priority_emoji = {"immediate": "🚨", "high": "⚠️", "medium": "📌"}.get(item.get("priority"), "📋")
                    st.write(f"{priority_emoji} **[{item.get('priority', 'medium').upper()}]** {item.get('description', '')}")
                    if item.get("oid"):
                        st.caption(f"OID: {item['oid']}")
    
    # Create tabs for the other sections
    tabs = st.tabs([
        "🌍 Context", 
        "🔍 Stack Analysis", 
        "📝 Pre-Crash", 
        "💡 Solutions", 
        "📈 Improvements", 
        "🔬 Observations"
    ])
    
    # Context & Environment
    with tabs[0]:
        context = results.get("context_environment", {})
        if context:
            st.markdown("## 2. Context & Environment")
            
            # System & User info
            if context.get("system_user"):
                st.markdown("### System & User")
                for key, value in context["system_user"].items():
                    st.write(f"• **{key.replace('_', ' ').title()}:** {value}")
            
            # Application & VM
            if context.get("application_vm"):
                st.markdown("### Application & VM")
                for key, value in context["application_vm"].items():
                    st.write(f"• **{key.replace('_', ' ').title()}:** {value}")
            
            # Database
            if context.get("database"):
                st.markdown("### Database")
                for key, value in context["database"].items():
                    st.write(f"• **{key.replace('_', ' ').title()}:** {value}")
    
    # Crash & Stack Analysis
    with tabs[1]:
        crash = results.get("crash_stack_analysis", {})
        if crash:
            st.markdown("## 3. Crash & Stack-Trace Analysis")
            
            # Unhandled exception
            exception = crash.get("unhandled_exception", {})
            if exception:
                st.error(f"**{exception.get('type', 'Unknown')}:** {exception.get('details', 'N/A')}")
                if exception.get("failing_selector"):
                    st.write(f"**Failing Selector:** `{exception['failing_selector']}`")
            
            # Stack frames with annotated text
            frames = crash.get("failure_point", {}).get("stack_frames", [])
            if frames:
                st.markdown("### Stack Trace")
                # Build full stack trace string
                stack_lines = []
                for frame in frames[:10]:
                    stack_lines.append(f"{frame['text']}")
                
                # Parse and display with annotations
                parsed_stack = parse_stack_trace('\n'.join(stack_lines))
                for line_components in parsed_stack:
                    if line_components:
                        annotated_text(*line_components)
            
            # Object state analysis
            if crash.get("object_state_analysis"):
                st.markdown("### Object State Issues")
                for state in crash["object_state_analysis"]:
                    st.warning(f"**Frame {state.get('frame', '?')}:** {state.get('issue', 'Unknown')}")
                    if state.get("likely_cause"):
                        st.caption(f"Likely cause: {state['likely_cause']}")
    
    # Pre-Crash Sequence
    with tabs[2]:
        sequence = results.get("pre_crash_sequence", {})
        if sequence:
            st.markdown("## 4. Pre-Crash Sequence & Reproduction")
            
            # User actions
            if sequence.get("user_actions"):
                st.markdown("### User Actions")
                for action in sequence["user_actions"]:
                    st.write(f"**Step {action.get('step', '?')}:** {action.get('action', 'Unknown')}")
                    if action.get("ui_element"):
                        st.caption(f"UI Element: {action['ui_element']}")
            
            # Key records
            if sequence.get("system_state", {}).get("key_records"):
                st.markdown("### Key Records Involved")
                for record in sequence["system_state"]["key_records"]:
                    st.info(f"**{record.get('type', 'Unknown')}** (ID: {record.get('id', 'N/A')}) - State: {record.get('state', 'Unknown')}")
            
            # Reproduction steps
            if sequence.get("reproduction_scenario"):
                st.markdown("### Reproduction Steps")
                for step in sequence["reproduction_scenario"]:
                    st.write(f"• {step}")
    
    # Solutions & Workarounds
    with tabs[3]:
        solutions = results.get("solutions_workarounds", {})
        if solutions:
            st.markdown("## 5. Solutions & Workarounds")
            
            # Root cause analysis
            root = solutions.get("root_cause_analysis", {})
            if root:
                col1, col2 = st.columns(2)
                with col1:
                    st.markdown("### Technical Root Cause")
                    st.info(root.get("technical", "Pending analysis"))
                with col2:
                    st.markdown("### Data Root Cause")
                    st.info(root.get("data", "N/A"))
            
            # Code fixes
            if solutions.get("code_level_fixes"):
                st.markdown("### Code-Level Fixes")
                for fix in solutions["code_level_fixes"]:
                    with st.expander(f"Fix: {fix.get('location', 'Unknown')}", expanded=True):
                        st.write(fix.get("fix", ""))
                        if fix.get("code_sample"):
                            st.code(fix["code_sample"], language="smalltalk")
            
            # Workarounds
            if solutions.get("immediate_workarounds"):
                st.markdown("### Immediate Workarounds")
                for workaround in solutions["immediate_workarounds"]:
                    type_emoji = {"immediate": "🚨", "temporary": "⏱️"}.get(workaround.get("type"), "📋")
                    impact_color = {"minimal": "green", "moderate": "orange", "significant": "red"}.get(workaround.get("impact"), "blue")
                    st.markdown(f"{type_emoji} **{workaround.get('action', '')}**")
                    st.caption(f"Impact: :{impact_color}[{workaround.get('impact', 'unknown')}]")
    
    # Improvements
    with tabs[4]:
        improvements = results.get("suggested_improvements", {})
        if improvements:
            st.markdown("## 6. Suggested Improvements")
            
            # Database performance
            if improvements.get("database_performance"):
                st.markdown("### Database Performance")
                for rec in improvements["database_performance"]:
                    st.write(f"• **Index on** `{rec.get('table', '?')}.{', '.join(rec.get('columns', []))}`")
                    st.caption(f"Reason: {rec.get('reason', '')}")
            
            # Code efficiency
            if improvements.get("code_efficiency"):
                st.markdown("### Code & API Efficiency")
                for eff in improvements["code_efficiency"]:
                    st.write(f"• {eff}")
            
            # Best practices
            if improvements.get("best_practices"):
                st.markdown("### Best Practices & Risks")
                for practice in improvements["best_practices"]:
                    st.warning(f"⚠️ {practice}")
    
    # Additional Observations
    with tabs[5]:
        observations = results.get("additional_observations", {})
        if observations:
            st.markdown("## 7. Additional Observations")
            
            # Memory analysis
            if observations.get("memory_analysis"):
                st.markdown("### Memory & Object Analysis")
                mem = observations["memory_analysis"]
                
                if mem.get("total_memory"):
                    st.metric("Total Memory", f"{int(mem['total_memory']):,} bytes")
                
                if mem.get("top_objects"):
                    st.markdown("**Top Object Types:**")
                    for obj in mem["top_objects"][:5]:
                        st.write(f"• **{obj['class']}:** {obj['count']:,} instances")
            
            # Process health
            if observations.get("process_health"):
                st.markdown("### Process Health")
                health = observations["process_health"]
                if health.get("active_processes"):
                    st.metric("Active Processes", health["active_processes"])
                if health.get("process_list"):
                    with st.expander("Process List", expanded=False):
                        for proc in health["process_list"]:
                            st.write(f"• {proc['name']} (Priority: {proc['priority']})")
            
            # Anomalies
            if observations.get("anomalies"):
                st.markdown("### Anomalies Detected")
                for anomaly in observations["anomalies"]:
                    st.error(f"⚠️ {anomaly}")

def _display_generic_results(results: Dict[str, Any]):
    """Generic results display for unknown methods"""
    st.markdown("### Analysis Results")
    
    # Try to display in a structured way
    for key, value in results.items():
        if key.startswith("_") or key in ["processing_time", "confidence"]:
            continue
            
        st.markdown(f"#### {key.replace('_', ' ').title()}")
        
        if isinstance(value, dict):
            st.json(value)
        elif isinstance(value, list):
            for item in value:
                st.write(f"• {item}")
        else:
            st.write(value)

def _display_common_elements(results: Dict[str, Any], method: str, db=None, analyzer=None):
    """Display common elements across all methods"""
    
    # Performance metrics
    with st.expander("⚡ Performance Metrics", expanded=False):
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.metric("Processing Time", f"{results.get('processing_time', 0):.2f}s")
        
        with col2:
            st.metric("Analysis Method", method.replace("_", " ").title())
        
        with col3:
            if results.get("confidence") is not None:
                confidence = results["confidence"]
                if isinstance(confidence, (int, float)):
                    st.metric("Confidence", f"{confidence:.0%}")
                else:
                    st.metric("Confidence", confidence)
    
    # Action buttons
    st.divider()
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        if st.button("💾 Save Analysis", use_container_width=True):
            if db and analyzer:
                try:
                    # Prepare the analysis data for saving
                    save_data = {
                        "method": method,
                        "timestamp": datetime.now().isoformat(),
                        "filename": results.get("filename", "unknown"),
                        "results": results
                    }
                    
                    # Create a summary for the prompt
                    prompt = f"Crash Analysis - Method: {method}, File: {results.get('filename', 'unknown')}"
                    
                    # Create a detailed response summary
                    response_parts = []
                    if results.get("error_type"):
                        response_parts.append(f"Error Type: {results['error_type']}")
                    if results.get("severity"):
                        response_parts.append(f"Severity: {results['severity']}")
                    if results.get("root_cause"):
                        response_parts.append(f"Root Cause: {results['root_cause']}")
                    if results.get("summary"):
                        response_parts.append(f"Summary: {results['summary']}")
                    
                    response = "\n".join(response_parts) if response_parts else json.dumps(results, indent=2)
                    
                    # Log to database
                    query_id = db.log_query(
                        tool="crash_analyzer_v2",
                        model=method,  # Using method as model for now
                        prompt=prompt,
                        response=response
                    )
                    
                    # Use TuoKitToolBase knowledge capture
                    if hasattr(analyzer, 'save_to_knowledge_base'):
                        # Determine category based on severity
                        severity = results.get("severity", "unknown").upper()
                        category = "Critical Issue" if severity in ["CRITICAL", "HIGH"] else "Error Solution"
                        
                        # Create a meaningful title
                        title = f"Crash Analysis: {results.get('error_type', 'Unknown')} - {method}"
                        
                        # Save to knowledge base
                        analyzer.save_to_knowledge_base(
                            title=title,
                            content=json.dumps(save_data, indent=2),
                            category=category,
                            query_id=query_id
                        )
                    
                    st.success(f"✅ Analysis saved to knowledge base! (ID: {query_id})")
                except Exception as e:
                    st.error(f"Failed to save: {str(e)}")
            else:
                st.warning("Database not available. Cannot save analysis.")
    
    with col2:
        if st.button("📄 Export Report", use_container_width=True):
            # Create downloadable report
            report = _generate_report(results, method)
            st.download_button(
                "Download",
                report,
                file_name=f"crash_analysis_{method}_{results.get('filename', 'report')}.md",
                mime="text/markdown"
            )
    
    with col3:
        if st.button("🔄 New Analysis", use_container_width=True):
            st.session_state.analysis_results = None
            st.session_state.analysis_method = None
            st.rerun()
    
    with col4:
        if st.button("📊 View Analytics", use_container_width=True):
            st.info("📊 Switch to the Analytics tab to view detailed crash analytics and trends!")

def _generate_report(results: Dict[str, Any], method: str) -> str:
    """Generate downloadable report"""
    # Get the formatted output from the analyzer
    if "formatted_output" in results:
        return results["formatted_output"]
    
    # Generic report generation
    report = f"# Crash Analysis Report\n\n"
    report += f"**Method**: {method.replace('_', ' ').title()}\n"
    report += f"**Generated**: {results.get('timestamp', 'Unknown')}\n"
    report += f"**Processing Time**: {results.get('processing_time', 0):.2f}s\n\n"
    
    # Add results
    for key, value in results.items():
        if key.startswith("_") or key in ["processing_time", "timestamp", "formatted_output"]:
            continue
        
        report += f"## {key.replace('_', ' ').title()}\n\n"
        
        if isinstance(value, dict):
            report += f"```json\n{json.dumps(value, indent=2)}\n```\n\n"
        elif isinstance(value, list):
            for item in value:
                report += f"- {item}\n"
            report += "\n"
        else:
            report += f"{value}\n\n"
    
    return report