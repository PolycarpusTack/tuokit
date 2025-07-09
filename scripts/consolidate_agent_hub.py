"""
Agent Hub Consolidation Script
Consolidates multiple agent hub implementations into the new toolkit structure
"""

import os
import shutil
from datetime import datetime
from pathlib import Path

def consolidate_agent_hub():
    """Consolidate agent hub implementations"""
    
    project_root = Path("C:/Projects/Tuokit")
    backup_dir = project_root / f"archive/agent_hub_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    
    print("[AGENT HUB] Starting Agent Hub Consolidation...")
    
    # Create backup directory
    backup_dir.mkdir(parents=True, exist_ok=True)
    print(f"[BACKUP] Created backup directory: {backup_dir}")
    
    # Files to backup and process
    agent_files = [
        "pages/agent_hub.py",
        "pages/agent_hub_enhanced.py",
        "agent_hub_enhancements.py",
        "advanced_agent_toolkit.py",
        "agent_tools_enhanced.py"
    ]
    
    # Backup existing files
    for file in agent_files:
        src = project_root / file
        if src.exists():
            dst = backup_dir / file.replace("/", "_")
            shutil.copy2(src, dst)
            print(f"[OK] Backed up: {file}")
    
    # Update navigation.py to use new refactored version
    nav_file = project_root / "utils/navigation.py"
    if nav_file.exists():
        with open(nav_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Update agent hub reference
        content = content.replace(
            '"file": "agent_hub.py"',
            '"file": "agent_hub_refactored.py"'
        )
        
        with open(nav_file, 'w', encoding='utf-8') as f:
            f.write(content)
        print("[OK] Updated navigation.py")
    
    # Create migration report
    report = {
        "timestamp": datetime.now().isoformat(),
        "backed_up_files": agent_files,
        "backup_location": str(backup_dir),
        "new_structure": {
            "toolkit": "toolkits/agent_hub/",
            "modules": [
                "core.py - Base classes and types",
                "specialists.py - Specialist agents",
                "orchestrator.py - Agent coordination",
                "pipelines.py - Pipeline execution",
                "ui.py - Streamlit interface"
            ],
            "entry_point": "pages/agent_hub_refactored.py"
        },
        "improvements": [
            "Fixed syntax errors (temp, bug placeholders)",
            "Implemented advanced pipeline with retry logic",
            "Modularized code structure",
            "Added proper error handling",
            "Enhanced UI with performance metrics",
            "Added pipeline builder interface"
        ]
    }
    
    # Save report
    report_file = project_root / "AGENT_HUB_CONSOLIDATION_REPORT.json"
    import json
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n[REPORT] Migration report saved to: {report_file}")
    print("\n[SUCCESS] Agent Hub consolidation complete!")
    print("\n[INFO] Next steps:")
    print("1. Test the new agent hub at pages/agent_hub_refactored.py")
    print("2. Remove old versions after verification")
    print("3. Update any references in other modules")
    
    return report

if __name__ == "__main__":
    consolidate_agent_hub()
