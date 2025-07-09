#!/usr/bin/env python3
"""
Simple test for Enterprise Report format
"""
import re
import json
from pathlib import Path

def test_enterprise_format():
    """Test the format_output method of Enterprise Report"""
    print("Testing Enterprise Report formatting...")
    
    # Mock results structure
    results = {
        "executive_summary": {
            "crash": "Application crashed due to missing method #isRelativeToEachAiring in license window",
            "root_cause": {
                "technical": "Method #isRelativeToEachAiring not implemented in CM2LicenseWindow class",
                "data": "No data corruption detected"
            },
            "actionable_items": [
                {
                    "type": "method",
                    "description": "Implement #isRelativeToEachAiring in CM2LicenseWindow",
                    "priority": "immediate"
                },
                {
                    "type": "record", 
                    "description": "Validate license record integrity",
                    "oid": "12345",
                    "priority": "high"
                }
            ]
        },
        "context_environment": {
            "system_user": {
                "machine_name": "DESKTOP-ABC123",
                "username": "jsmith",
                "whatson_user": "John Smith",
                "site": "Production Site A"
            },
            "application_vm": {
                "whatson_version": "4.5.2",
                "build": "2024.01.15",
                "smalltalk_version": "VisualWorks 8.3",
                "db_hash": "a1b2c3d4"
            },
            "database": {
                "server_version": "Oracle 19c",
                "client_version": "Oracle Client 19.3",
                "database_encoding": "UTF8",
                "session_encoding": "UTF8"
            }
        },
        "crash_stack_analysis": {
            "unhandled_exception": {
                "type": "Unhandled exception",
                "details": "doesNotUnderstand: #isRelativeToEachAiring",
                "failing_selector": "#isRelativeToEachAiring"
            },
            "failure_point": {
                "stack_frames": [
                    {"frame": 1, "text": "MediaGeniX.CM2LicenseWindow>>checkLicense"},
                    {"frame": 2, "text": "MediaGeniX.T3ChildSet>>validateChildren"},
                    {"frame": 3, "text": "UndefinedObject>>doesNotUnderstand:"}
                ]
            },
            "object_state_analysis": [
                {
                    "frame": 1,
                    "object": "CM2LicenseWindow",
                    "issue": "Missing method implementation",
                    "likely_cause": "Incomplete deployment or version mismatch"
                }
            ]
        },
        "pre_crash_sequence": {
            "user_actions": [
                {"step": 1, "action": "User opened License Manager", "ui_element": "Main Menu > Tools"},
                {"step": 2, "action": "Selected license for validation", "ui_element": "License List"},
                {"step": 3, "action": "Clicked Validate button", "ui_element": "License Window"}
            ],
            "system_state": {
                "command_line": "whatson.exe -site ProductionA",
                "key_records": [
                    {"type": "License", "id": "12345", "state": "being validated"}
                ]
            },
            "reproduction_scenario": [
                "Step 1: Open License Manager from Tools menu",
                "Step 2: Select any active license",
                "Step 3: Click Validate - crash occurs immediately"
            ]
        },
        "solutions_workarounds": {
            "root_cause_analysis": {
                "technical": "The method #isRelativeToEachAiring is called but not implemented in CM2LicenseWindow",
                "data": "No data issues found"
            },
            "code_level_fixes": [
                {
                    "location": "CM2LicenseWindow>>isRelativeToEachAiring",
                    "fix": "Implement the missing method to return appropriate boolean value",
                    "code_sample": "isRelativeToEachAiring\\n\\t\\\"Return whether license is relative to each airing\\\"\\n\\t^self license notNil and: [self license isRelativeToEachAiring]"
                }
            ],
            "immediate_workarounds": [
                {
                    "type": "immediate",
                    "action": "Disable license validation temporarily in config",
                    "impact": "minimal"
                },
                {
                    "type": "temporary",
                    "action": "Use older version of License Manager",
                    "impact": "moderate"
                }
            ]
        },
        "suggested_improvements": {
            "database_performance": [
                {
                    "table": "licenses",
                    "columns": ["user_id", "status", "expiry_date"],
                    "reason": "Speed up license validation queries"
                }
            ],
            "code_efficiency": [
                "Add method existence checks before calling",
                "Implement comprehensive error handling"
            ],
            "best_practices": [
                "Missing defensive programming checks",
                "No graceful degradation for missing methods"
            ]
        },
        "additional_observations": {
            "memory_analysis": {
                "total_memory": "536870912",
                "top_objects": [
                    {"class": "String", "count": 125432},
                    {"class": "Array", "count": 87654},
                    {"class": "Dictionary", "count": 12345}
                ]
            },
            "process_health": {
                "active_processes": 15,
                "process_list": [
                    {"name": "UI Process", "priority": 50},
                    {"name": "Background Worker", "priority": 30}
                ]
            },
            "anomalies": [
                "High memory usage (>90%)",
                "Timeout conditions detected in database layer"
            ]
        },
        "processing_time": 4.2
    }
    
    # Format output
    output = format_enterprise_output(results)
    
    print("Generated report:")
    print("=" * 80)
    print(output)
    print("=" * 80)
    
    # Save to file
    output_file = Path("test_enterprise_output.md")
    output_file.write_text(output)
    print(f"\nReport saved to: {output_file}")
    
    # Verify all sections present
    sections = [
        "1. Executive Summary",
        "2. Context & Environment", 
        "3. Crash & Stack-Trace Analysis",
        "4. Pre-Crash Sequence",
        "5. Solutions & Workarounds",
        "6. Suggested Improvements",
        "7. Additional Observations"
    ]
    
    print("\nVerifying sections:")
    for section in sections:
        if section in output:
            print(f"✓ {section}")
        else:
            print(f"✗ {section} - MISSING")

def format_enterprise_output(results):
    """Format results into structured sections"""
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

if __name__ == "__main__":
    test_enterprise_format()