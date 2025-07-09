"""
UI Components for Crash Analyzer
Reusable Streamlit components for the crash analyzer interface
"""
import streamlit as st
import json
from datetime import datetime, timedelta
from typing import Dict, Any, List, Optional

from utils import DatabaseManager, get_available_models
from utils.ollama import check_model_availability
from .config import SEVERITY_INDICATORS
from .patterns import match_known_patterns

def show_model_selection() -> Optional[str]:
    """
    Display model selection widget and return selected model
    
    Returns:
        Selected model name or None if Ollama not available
    """
    st.markdown("### 🤖 Model Selection")
    
    # Check if Ollama is available first
    try:
        from utils.ollama import get_ollama_manager
        ollama_mgr = get_ollama_manager()
        
        status = ollama_mgr.get_status()
        if not status.get("running", False):
            # Ollama not running
            st.error("🔴 **Ollama not available** - Please ensure Ollama is running")
            
            # Show disabled selector
            col1, col2, col3 = st.columns([2, 1, 1])
            with col1:
                st.selectbox(
                    "Select Analysis Model",
                    ["Ollama not available"],
                    disabled=True,
                    help="Start Ollama service to enable model selection"
                )
            with col2:
                st.metric("Status", "🔴 Offline")
            with col3:
                if st.button("🔄 Retry", help="Check Ollama connection again"):
                    st.rerun()
            
            return None
    except Exception as e:
        st.error(f"Error checking Ollama status: {str(e)}")
        return None
    
    # Get available models dynamically
    available_models = get_available_models()
    
    if not available_models:
        st.warning("⚠️ No models found. Please install models in Ollama first.")
        st.code("ollama pull deepseek-r1:latest", language="bash")
        
        col1, col2, col3 = st.columns([2, 1, 1])
        with col1:
            st.selectbox(
                "Select Analysis Model",
                ["No models installed"],
                disabled=True,
                help="Install models in Ollama first"
            )
        with col2:
            st.metric("Status", "⚠️ No Models")
        with col3:
            if st.button("🔄 Refresh", help="Check for models again"):
                st.rerun()
        
        return None
    
    # Model selection with dynamic models
    col1, col2, col3 = st.columns([2, 1, 1])
    
    with col1:
        # Try to find a good default model
        default_index = 0
        
        # Prefer certain models if available (but don't hardcode)
        preferred_keywords = ["deepseek", "llama", "mistral", "gpt"]
        for i, model in enumerate(available_models):
            if any(keyword in model.lower() for keyword in preferred_keywords):
                default_index = i
                break
        
        selected_model = st.selectbox(
            "Select Analysis Model",
            available_models,
            index=default_index,
            help="Choose the LLM model for crash analysis. Different models may provide different insights."
        )
    
    with col2:
        # Model status check
        if selected_model:
            model_status = check_model_availability(selected_model)
            status_color = {
                "ready": "🟢",
                "needs_pull": "🟡", 
                "pull_failed": "🔴"
            }.get(model_status["status"], "⚪")
            
            st.metric("Model Status", f"{status_color} {model_status['status'].title()}")
        else:
            st.metric("Model Status", "⚪ Unknown")
    
    with col3:
        # Refresh models button
        if st.button("🔄 Refresh Models", help="Reload available models from Ollama"):
            # Clear cached model list if exists
            if 'available_models' in st.session_state:
                del st.session_state.available_models
            st.rerun()
    
    # Show model info
    if selected_model:
        with st.expander("ℹ️ Model Information", expanded=False):
            st.write(f"**Selected Model:** `{selected_model}`")
            
            # Show basic model info from status
            st.write(f"**Model Available:** {'✅ Yes' if selected_model in available_models else '❌ No'}")
            st.write(f"**Total Models:** {len(available_models)}")
            
            if selected_model and 'model_status' in locals():
                st.write(f"**Status:** {model_status['status']}")
                if not model_status["available"] and model_status["can_attempt_pull"]:
                    st.info("📥 This model will be downloaded automatically when first used")
                elif not model_status["can_attempt_pull"]:
                    st.error("❌ This model failed to download previously. Try refreshing or selecting another model.")
    
    # Store in session state
    if selected_model:
        st.session_state.selected_model = selected_model
        return selected_model
    
    return None

def show_file_analysis_status(content: str) -> None:
    """
    Show visual indicators of file analysis status
    
    Args:
        content: File content
    """
    from .extractors import extract_file_metadata
    
    st.markdown("### 🔍 File Analysis Insights")
    metadata = extract_file_metadata(content)
    
    cols = st.columns(6)
    
    # Line count
    cols[0].metric("📄 Lines", f"{metadata['lines']:,}")
    
    # File size
    if metadata.get('size_mb', 0) > 0:
        cols[1].metric("💾 Size", f"{metadata['size_mb']:.1f} MB")
    else:
        cols[1].metric("💾 Size", f"{metadata['size_kb']:.1f} KB")
    
    # Error count estimate
    cols[2].metric("🐛 Errors", metadata.get('error_count', 0))
    
    # Stack trace detection
    cols[3].metric("📚 Stack Frames", metadata.get('stack_frames', 0))
    
    # Timestamp detection
    if metadata.get('has_timestamps'):
        cols[4].metric("🕐 Timestamps", metadata.get('count', 0))
        if metadata.get('range'):
            cols[5].metric("📅 Time Range", metadata['range'])
    else:
        cols[4].metric("🕐 Timestamps", "None")
    
    # Pattern preview
    patterns = match_known_patterns(content[:10000])  # Quick scan of first 10KB
    if patterns:
        st.info(f"🔍 Quick scan found {len(patterns)} known error patterns in first 10KB")

def show_performance_stats() -> None:
    """Display analysis performance metrics"""
    if any(key in st.session_state for key in ['last_analysis_time', 'last_expert_time', 'chunk_analysis_time']):
        st.markdown("### ⏱️ Performance Metrics")
        cols = st.columns(4)
        
        if 'last_analysis_time' in st.session_state:
            cols[0].metric("Basic Analysis", st.session_state.last_analysis_time)
        
        if 'last_expert_time' in st.session_state:
            cols[1].metric("Expert Analysis", st.session_state.last_expert_time)
        
        if 'chunk_analysis_time' in st.session_state:
            cols[2].metric("Chunk Analysis", st.session_state.chunk_analysis_time)
        
        if 'last_method_used' in st.session_state:
            cols[3].metric("Method Used", st.session_state.last_method_used)

def show_pattern_matches(pattern_matches: List[Dict[str, Any]]) -> None:
    """
    Display detected patterns in an organized way
    
    Args:
        pattern_matches: List of pattern match dictionaries
    """
    if not pattern_matches:
        return
        
    with st.expander(f"🎯 Detected {len(pattern_matches)} Known Patterns", expanded=True):
        # Group by severity
        severity_groups = {}
        for match in pattern_matches:
            severity = match['severity']
            if severity not in severity_groups:
                severity_groups[severity] = []
            severity_groups[severity].append(match)
        
        # Display by severity
        for severity in ["Critical", "High", "Medium", "Low"]:
            if severity in severity_groups:
                severity_info = SEVERITY_INDICATORS.get(severity, {})
                severity_emoji = severity_info.get("emoji", "⚪")
                
                st.markdown(f"#### {severity_emoji} {severity} Severity")
                
                for match in severity_groups[severity][:2]:  # Show max 2 per severity
                    col1, col2 = st.columns([3, 1])
                    with col1:
                        st.markdown(f"**{match['pattern']}** - Found: `{match['match_text']}`")
                        # Show context in a collapsible way
                        if st.checkbox(f"Show context for {match['pattern']}", key=f"context_{match['pattern']}_{match['position']}"):
                            st.code(match['context'], language="text")
                    with col2:
                        st.markdown("**Quick Fix:**")
                        st.caption(match['quick_fix'])

def show_analysis_results(analysis: Dict[str, Any], analysis_type: str = "basic") -> None:
    """
    Display analysis results in a consistent format
    
    Args:
        analysis: Analysis results dictionary
        analysis_type: Type of analysis (basic/chunk/sample)
    """
    st.markdown(f"### 📊 {analysis_type.title()} Analysis Results")
    
    # Check if this was from sampling
    if 'sample_info' in analysis:
        st.info(f"📍 Based on {analysis['sample_info']['count']} strategic samples")
    
    # Severity indicator with visual
    severity = analysis.get("severity", "Medium")
    severity_info = SEVERITY_INDICATORS.get(severity, SEVERITY_INDICATORS["Unknown"])
    
    # Main metrics
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Severity", f"{severity_info['emoji']} {severity}")
    with col2:
        st.metric("Error Type", analysis.get("error_type", "Unknown"))
    with col3:
        error_location = analysis.get("error_location", "Unknown")
        if len(error_location) > 30:
            error_location = error_location[:30] + "..."
        st.metric("Location", error_location)
    
    # Editable analysis fields
    st.markdown("#### 📝 Analysis Details")
    col1, col2 = st.columns(2)
    
    with col1:
        analysis["root_cause"] = st.text_area(
            "Root Cause",
            value=analysis.get("root_cause", ""),
            height=100,
            key=f"{analysis_type}_root_cause"
        )
        analysis["quick_fix"] = st.text_area(
            "Quick Fix",
            value=analysis.get("quick_fix", ""),
            height=100,
            key=f"{analysis_type}_quick_fix"
        )
        
    with col2:
        analysis["severity"] = st.selectbox(
            "Severity",
            ["Critical", "High", "Medium", "Low"],
            index=["Critical", "High", "Medium", "Low"].index(severity),
            key=f"{analysis_type}_severity"
        )
        analysis["prevention"] = st.text_area(
            "Prevention Strategy",
            value=analysis.get("prevention", ""),
            height=100,
            key=f"{analysis_type}_prevention"
        )

def show_chunk_analysis_results(chunk_analysis: Dict[str, Any]) -> None:
    """
    Display chunk analysis results with detailed metrics
    
    Args:
        chunk_analysis: Chunk analysis results
    """
    st.markdown("### 🧩 Full File Analysis Results")
    
    # Display metrics
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Total Errors", chunk_analysis.get("total_errors", 0))
    with col2:
        chunks_analyzed = chunk_analysis.get("chunks_analyzed", 0)
        chunks_skipped = chunk_analysis.get("chunks_skipped", 0)
        chunks_processed = chunk_analysis.get("chunks_processed", 0)
        st.metric("Chunks Analyzed", f"{chunks_analyzed}/{chunks_processed}", 
                 f"{chunks_skipped} skipped")
    with col3:
        st.metric("Patterns Found", chunk_analysis.get("patterns_found", 0))
    with col4:
        st.metric("Processing Time", f"{chunk_analysis.get('processing_time', 0):.1f}s")
    
    # Show failure info if there were failures
    if chunk_analysis.get("chunks_failed", 0) > 0:
        st.warning(f"⚠️ {chunks_failed} chunks failed to process. Using pattern matching for those chunks.")
    
    if chunk_analysis.get("stopped_early", False):
        st.error("❌ Processing stopped early due to too many consecutive failures.")
    
    # Show aggregated results
    st.markdown("#### 📊 Aggregated Analysis")
    
    # Show model used
    if chunk_analysis.get("model_used"):
        st.info(f"🤖 Analysis performed using: **{chunk_analysis.get('model_used')}**")
    
    # Severity indicator
    severity = chunk_analysis.get("severity", "Unknown")
    severity_info = SEVERITY_INDICATORS.get(severity, SEVERITY_INDICATORS["Unknown"])
    
    col1, col2 = st.columns(2)
    with col1:
        st.markdown(f"**Severity:** {severity_info['emoji']} {severity}")
        st.markdown(f"**Root Cause:** {chunk_analysis.get('root_cause', 'Not identified')}")
    with col2:
        st.markdown(f"**Error Types:** {chunk_analysis.get('error_type', 'Multiple types')}")
        st.markdown(f"**Locations:** {chunk_analysis.get('error_location', 'Multiple locations')}")
    
    # Show chunk summaries
    if chunk_analysis.get("chunk_summaries"):
        with st.expander("📋 Chunk-by-Chunk Analysis", expanded=False):
            summaries = chunk_analysis["chunk_summaries"]
            if len(summaries) > 15:
                for summary in summaries[:10]:
                    st.write(f"- {summary}")
                st.write("...")
                for summary in summaries[-5:]:
                    st.write(f"- {summary}")
            else:
                for summary in summaries:
                    st.write(f"- {summary}")

def show_save_section(analysis: Dict[str, Any], expert_report: Optional[str], 
                     filename: str, content_hash: str, content: str,
                     selected_model: str, db: DatabaseManager) -> None:
    """
    Display save to knowledge base section
    
    Args:
        analysis: Analysis results
        expert_report: Optional expert report
        filename: Name of analyzed file
        content_hash: Hash of file content
        content: Original file content
        selected_model: Model used for analysis
        db: Database manager instance
    """
    st.divider()
    st.markdown("### 💾 Save to Knowledge Base")
    
    col1, col2 = st.columns([3, 1])
    
    with col1:
        validator_name = st.text_input(
            "Your Name (required for validation)",
            placeholder="Enter your name to save this analysis"
        )
    
    with col2:
        quality = st.number_input("Quality Rating", 1, 5, 3, help="Rate the analysis quality")
    
    include_expert = False
    if expert_report:
        include_expert = st.checkbox(
            "Include expert report in knowledge base", 
            value=True,
            help="Expert reports provide valuable context for future reference"
        )
    
    # Business impact section for crash learning
    business_impact = {}
    try:
        from utils.crash_knowledge import CrashPatternLearner
        CRASH_KNOWLEDGE_ENABLED = True
    except ImportError:
        CRASH_KNOWLEDGE_ENABLED = False
    
    if CRASH_KNOWLEDGE_ENABLED:
        st.markdown("#### 📊 Business Impact (Optional)")
        col1, col2, col3 = st.columns(3)
        with col1:
            downtime = st.number_input("Downtime (minutes)", 0, 10000, 0)
            business_impact['downtime_minutes'] = downtime
        with col2:
            affected_features = st.text_input("Affected Features", placeholder="e.g., Login, Checkout")
            business_impact['affected_features'] = affected_features.split(',') if affected_features else []
        with col3:
            impact_notes = st.text_area("Impact Notes", placeholder="Additional context...")
            business_impact['notes'] = impact_notes
    
    col1, col2, col3 = st.columns([2, 1, 1])
    with col1:
        if st.button("💾 Save Validated Analysis", type="primary", use_container_width=True):
            if not validator_name:
                st.warning("⚠️ Please enter your name for validation")
            else:
                # Import save function from helpers
                from .helpers import save_crash_analysis
                
                if save_crash_analysis(
                    db,
                    filename,
                    content_hash,
                    analysis,
                    expert_report,
                    validator_name,
                    include_expert,
                    quality_rating=quality,
                    business_impact=business_impact if CRASH_KNOWLEDGE_ENABLED else None,
                    content=content,
                    model_used=selected_model
                ):
                    st.success("✅ Analysis saved to knowledge base!")
                    st.balloons()
    
    with col2:
        # Export functionality
        if st.button("📄 Export Report", use_container_width=True):
            from .helpers import export_crash_report
            report = export_crash_report(
                filename,
                analysis,
                expert_report if include_expert else None
            )
            st.download_button(
                label="Download",
                data=report,
                file_name=f"crash_report_{filename}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md",
                mime="text/markdown"
            )
    
    with col3:
        # Quick navigation
        if st.button("📚 Knowledge Library", use_container_width=True):
            st.switch_page("pages/knowledge_lib.py")

def show_crash_statistics_dashboard(db: DatabaseManager) -> None:
    """
    Display interactive crash statistics dashboard
    
    Args:
        db: Database manager instance
    """
    try:
        st.markdown("### 📊 Crash Statistics Dashboard")
        
        # Date range selector
        col1, col2 = st.columns(2)
        with col1:
            start_date = st.date_input(
                "Start Date", 
                value=datetime.now() - timedelta(days=30),
                max_value=datetime.now().date()
            )
        with col2:
            end_date = st.date_input(
                "End Date", 
                value=datetime.now().date(),
                max_value=datetime.now().date()
            )
        
        # Fetch crash data for the period
        crash_data = db.execute_query("""
            SELECT 
                analysis->>'severity' as severity,
                analysis->>'error_type' as error_type,
                analysis->>'root_cause' as root_cause,
                filename,
                validated_by,
                created_at
            FROM crash_analysis
            WHERE DATE(created_at) BETWEEN %s AND %s
            ORDER BY created_at DESC
        """, (start_date, end_date))
        
        if not crash_data:
            st.warning("No crash data available for the selected period.")
            return
        
        # Calculate key metrics
        total_crashes = len(crash_data)
        severity_counts = {"Critical": 0, "High": 0, "Medium": 0, "Low": 0}
        for crash in crash_data:
            severity = crash['severity'] or 'Unknown'
            if severity in severity_counts:
                severity_counts[severity] += 1
        
        # Display key metrics
        st.markdown("#### 🎯 Key Metrics")
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric("Total Crashes", total_crashes)
        with col2:
            critical_count = severity_counts['Critical']
            st.metric("Critical Issues", critical_count,
                     delta=f"{round(critical_count/total_crashes*100, 1)}%" if total_crashes > 0 else "0%")
        with col3:
            days_in_period = max(1, (end_date - start_date).days + 1)
            avg_per_day = round(total_crashes / days_in_period, 1)
            st.metric("Daily Average", avg_per_day)
        with col4:
            unique_validators = len(set(crash['validated_by'] for crash in crash_data if crash['validated_by']))
            st.metric("Active Validators", unique_validators)
        
        # Severity distribution chart
        st.markdown("#### 🔴 Severity Distribution")
        severity_data = []
        
        for severity, count in severity_counts.items():
            if count > 0:
                severity_data.append({
                    "Severity": severity,
                    "Count": count,
                    "Percentage": round(count/total_crashes*100, 1)
                })
        
        if severity_data:
            # Create bar chart using columns
            cols = st.columns(len(severity_data))
            for i, data in enumerate(severity_data):
                with cols[i]:
                    severity_info = SEVERITY_INDICATORS.get(data["Severity"], {})
                    st.metric(
                        data["Severity"],
                        data["Count"],
                        f"{data['Percentage']}%"
                    )
                    # Visual bar representation
                    bar_height = int(data["Percentage"] / 2)
                    st.markdown(
                        f'<div style="background-color: {severity_info.get("color", "#666")}; '
                        f'height: {bar_height}px; width: 100%; margin-top: 5px;"></div>',
                        unsafe_allow_html=True
                    )
        
        # Pattern analysis summary
        with st.expander("🔄 Pattern Analysis", expanded=False):
            from .helpers import recognize_crash_patterns
            patterns = recognize_crash_patterns(db, limit=50, min_occurrences=2)
            if patterns['patterns']:
                st.write(patterns['summary'])
                for i, pattern in enumerate(patterns['patterns'][:3], 1):
                    st.write(f"\n**Pattern {i}**: {pattern['error_type']}")
                    st.write(f"- Occurrences: {pattern['occurrences']}")
                    st.write(f"- Trend: {pattern['trend']}")
                    st.write(f"- Severity: {pattern['dominant_severity']}")
        
    except Exception as e:
        st.error(f"Error loading dashboard: {str(e)}")