"""
Helper Functions for Crash Analyzer
Database operations and utility functions
"""
import json
from datetime import datetime
from typing import List, Dict, Any, Optional

from utils import DatabaseManager, capture_knowledge

def find_similar_crashes(db: DatabaseManager, error_type: str, limit: int = 5) -> List[Dict[str, Any]]:
    """
    Find crashes with similar error patterns
    
    Args:
        db: Database manager instance
        error_type: Error type to search for
        limit: Maximum results to return
        
    Returns:
        List of similar crash records
    """
    if not db:
        return []
        
    try:
        return db.execute_query("""
            SELECT filename, 
                   analysis->>'severity' as severity,
                   analysis->>'root_cause' as root_cause,
                   validated_by,
                   created_at
            FROM crash_analysis
            WHERE analysis->>'error_type' ILIKE %s
               OR analysis->>'root_cause' ILIKE %s
            ORDER BY created_at DESC
            LIMIT %s
        """, (f"%{error_type}%", f"%{error_type}%", limit))
    except:
        return []

def export_crash_report(filename: str, analysis: Dict[str, Any], 
                       expert_report: Optional[str] = None) -> str:
    """
    Export crash analysis as markdown report
    
    Args:
        filename: Name of analyzed file
        analysis: Analysis results dictionary
        expert_report: Optional expert analysis report
        
    Returns:
        Markdown formatted report
    """
    from .config import SEVERITY_INDICATORS
    
    severity = analysis.get("severity", "Medium")
    severity_info = SEVERITY_INDICATORS.get(severity, SEVERITY_INDICATORS["Unknown"])
    
    report = f"""# Crash Analysis Report

**File**: {filename}  
**Date**: {datetime.now().strftime('%Y-%m-%d %H:%M')}  
**Severity**: {severity_info['emoji']} {severity}

## Summary
- **Error Type**: {analysis.get('error_type', 'Unknown')}
- **Location**: {analysis.get('error_location', 'Unknown')}
- **Root Cause**: {analysis.get('root_cause', 'Not identified')}

## Quick Fix
{analysis.get('quick_fix', 'No quick fix available')}

## Prevention
{analysis.get('prevention', 'No prevention strategy defined')}
"""
    
    if expert_report:
        report += f"\n---\n\n## Expert Analysis\n\n{expert_report}"
    
    return report

def save_crash_analysis(db: DatabaseManager, filename: str, content_hash: str, 
                       analysis: Dict[str, Any], expert_report: Optional[str], 
                       validated_by: str, include_expert: bool, 
                       quality_rating: int = 3, business_impact: Optional[Dict] = None, 
                       content: Optional[str] = None, model_used: Optional[str] = None) -> bool:
    """
    Save validated analysis to database with learning
    
    Args:
        db: Database manager instance
        filename: Analyzed file name
        content_hash: Hash of file content
        analysis: Analysis results
        expert_report: Optional expert report
        validated_by: Validator name
        include_expert: Whether to include expert report
        quality_rating: Quality rating (1-5)
        business_impact: Business impact data
        content: Original file content
        model_used: Model used for analysis
        
    Returns:
        True if saved successfully
    """
    if not db:
        return False
        
    try:
        # Save to crash_analysis table
        db.execute_query(
            """INSERT INTO crash_analysis 
            (filename, content_hash, analysis, expert_report, validated_by, created_at, quality_rating) 
            VALUES (%s, %s, %s, %s, %s, %s, %s)
            ON CONFLICT (content_hash) DO UPDATE 
            SET analysis = %s, expert_report = %s, validated_by = %s, created_at = %s, quality_rating = %s""",
            (filename, content_hash, json.dumps(analysis), 
             expert_report if include_expert else None, validated_by, datetime.now(), quality_rating,
             json.dumps(analysis), expert_report if include_expert else None, 
             validated_by, datetime.now(), quality_rating)
        )
        
        # Try to use crash knowledge system if available
        instance_id = None
        try:
            from utils.crash_knowledge import CrashPatternLearner
            
            crash_learner = CrashPatternLearner(db)
            
            if content:
                # Extract metadata if WCR
                metadata = {}
                if "BEGIN RUNTIME DIAGNOSTIC DUMP" in content[:200]:
                    from .wcr_handler import extract_wcr_metadata
                    metadata = extract_wcr_metadata(content)
                
                # Learn from this crash
                validation_data = {
                    'validator_name': validated_by,
                    'quality_rating': quality_rating,
                    'business_impact': business_impact
                }
                
                learning_result = crash_learner.learn_from_crash(content, analysis, validation_data)
                
                if learning_result:
                    import streamlit as st
                    st.success(f"🧠 Pattern Learning: {learning_result['action']} pattern '{learning_result.get('pattern_name', 'Unknown')}'")
        except ImportError:
            pass  # Crash knowledge system not available
        
        # Capture to general knowledge base
        knowledge_id = capture_knowledge(
            tool_name="crash_analyzer",
            prompt=f"Analyze crash in {filename}",
            response=json.dumps(analysis, indent=2) + 
                    ("\n\n---EXPERT REPORT---\n" + expert_report if include_expert and expert_report else ""),
            metadata={
                "filename": filename,
                "severity": analysis.get("severity", "Unknown"),
                "validated_by": validated_by,
                "has_expert_report": include_expert,
                "crash_instance_id": instance_id
            }
        )
        
        return True
    except Exception as e:
        import streamlit as st
        st.error(f"Database error: {str(e)}")
        return False

def recognize_crash_patterns(db: DatabaseManager, limit: int = 50, 
                           min_occurrences: int = 2) -> Dict[str, Any]:
    """
    Analyze crash patterns across multiple dumps to identify recurring issues.
    
    Args:
        db: Database manager instance
        limit: Maximum crashes to analyze
        min_occurrences: Minimum occurrences for a pattern
        
    Returns:
        Dictionary with patterns and summary
    """
    if not db:
        return {
            "patterns": [],
            "summary": "Database not available"
        }
        
    try:
        # Fetch recent crash analyses
        recent_crashes = db.execute_query("""
            SELECT 
                analysis->>'error_type' as error_type,
                analysis->>'error_location' as error_location,
                analysis->>'root_cause' as root_cause,
                analysis->>'severity' as severity,
                filename,
                created_at
            FROM crash_analysis
            ORDER BY created_at DESC
            LIMIT %s
        """, (limit,))
        
        if not recent_crashes:
            return {
                "patterns": [],
                "summary": "No crash data available for pattern analysis"
            }
        
        # Group crashes by error type and root cause
        pattern_groups = {}
        for crash in recent_crashes:
            # Create pattern key from error type and root cause
            pattern_key = f"{crash['error_type'] or 'Unknown'}:{crash['root_cause'] or 'Unknown'}"
            
            if pattern_key not in pattern_groups:
                pattern_groups[pattern_key] = {
                    "error_type": crash['error_type'] or 'Unknown',
                    "root_cause": crash['root_cause'] or 'Unknown',
                    "occurrences": 0,
                    "severities": [],
                    "locations": set(),
                    "files": [],
                    "first_seen": crash['created_at'],
                    "last_seen": crash['created_at']
                }
            
            group = pattern_groups[pattern_key]
            group['occurrences'] += 1
            group['severities'].append(crash['severity'] or 'Unknown')
            if crash['error_location']:
                group['locations'].add(crash['error_location'])
            group['files'].append({
                "filename": crash['filename'],
                "timestamp": crash['created_at']
            })
            
            # Update time range
            if crash['created_at'] < group['first_seen']:
                group['first_seen'] = crash['created_at']
            if crash['created_at'] > group['last_seen']:
                group['last_seen'] = crash['created_at']
        
        # Filter patterns with minimum occurrences and calculate metrics
        significant_patterns = []
        for pattern_key, group in pattern_groups.items():
            if group['occurrences'] >= min_occurrences:
                # Calculate severity distribution
                severity_counts = {}
                for severity in group['severities']:
                    severity_counts[severity] = severity_counts.get(severity, 0) + 1
                
                # Determine dominant severity
                dominant_severity = max(severity_counts.items(), key=lambda x: x[1])[0]
                
                # Calculate trend
                time_diff = (group['last_seen'] - group['first_seen']).total_seconds()
                if time_diff > 0:
                    # Check if occurrences are concentrated in recent time
                    recent_count = sum(1 for f in group['files'][-5:] 
                                     if (group['last_seen'] - f['timestamp']).days <= 1)
                    trend = "increasing" if recent_count >= 3 else "stable"
                else:
                    trend = "new"
                
                significant_patterns.append({
                    "pattern_id": pattern_key,
                    "error_type": group['error_type'],
                    "root_cause": group['root_cause'],
                    "occurrences": group['occurrences'],
                    "affected_locations": list(group['locations'])[:5],  # Top 5 locations
                    "severity_distribution": severity_counts,
                    "dominant_severity": dominant_severity,
                    "trend": trend,
                    "time_span_days": max(1, (group['last_seen'] - group['first_seen']).days),
                    "recent_files": [f['filename'] for f in group['files'][-3:]]  # Last 3 files
                })
        
        # Sort by occurrences and severity
        from .config import SEVERITY_INDICATORS
        severity_order = {name: info["priority"] for name, info in SEVERITY_INDICATORS.items()}
        
        significant_patterns.sort(
            key=lambda p: (p['occurrences'], severity_order.get(p['dominant_severity'], 0)),
            reverse=True
        )
        
        # Generate summary
        total_crashes = len(recent_crashes)
        pattern_count = len(significant_patterns)
        most_common = significant_patterns[0] if significant_patterns else None
        
        summary = f"Analyzed {total_crashes} crashes, found {pattern_count} recurring patterns."
        if most_common:
            summary += f" Most common: {most_common['error_type']} ({most_common['occurrences']} occurrences)"
        
        return {
            "patterns": significant_patterns[:10],  # Top 10 patterns
            "summary": summary,
            "total_analyzed": total_crashes,
            "pattern_count": pattern_count
        }
        
    except Exception as e:
        return {
            "patterns": [],
            "summary": f"Pattern analysis failed: {str(e)}"
        }