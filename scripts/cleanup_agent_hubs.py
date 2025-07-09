"""
Agent Hub Cleanup Script
Consolidates agent hub files and removes duplicates
"""

import os
import shutil
from datetime import datetime
from pathlib import Path

def cleanup_agent_hubs():
    """Clean up and consolidate agent hub files"""
    
    project_root = Path("C:/Projects/Tuokit")
    pages_dir = project_root / "pages"
    archive_dir = project_root / "archive" / f"agent_hub_cleanup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    
    # Create archive directory
    archive_dir.mkdir(parents=True, exist_ok=True)
    
    print("Starting Agent Hub cleanup...")
    
    # 1. Archive duplicate/obsolete files
    files_to_archive = [
        "agent_hub_consolidated.py",  # Duplicate of refactored
        "agent_hub_enhanced.py",       # Features integrated into toolkit
    ]
    
    for file in files_to_archive:
        source = pages_dir / file
        if source.exists():
            dest = archive_dir / file
            shutil.move(str(source), str(dest))
            print(f"[OK] Archived: {file}")
        else:
            print(f"[!] File not found: {file}")
    
    # 2. Rename refactored to be the main agent hub
    refactored_file = pages_dir / "agent_hub_refactored.py"
    old_main_file = pages_dir / "agent_hub.py"
    
    if refactored_file.exists() and old_main_file.exists():
        # Archive the old monolithic version
        shutil.move(str(old_main_file), str(archive_dir / "agent_hub_monolithic.py"))
        print("[OK] Archived: agent_hub.py (monolithic version)")
        
        # Rename refactored to be the main
        shutil.move(str(refactored_file), str(old_main_file))
        print("[OK] Renamed: agent_hub_refactored.py -> agent_hub.py")
    
    # 3. Clean up root directory agent files (move to archive)
    root_agent_files = [
        "agent_hub_enhancements.py",
        "advanced_agent_toolkit.py",
        "agent_tools_enhanced.py",
        "test_agent_hub_enhanced.py",
        "test_agent_enhancements.py",
        "test_agent_toolkit.py",
        "demo_agent_hub_enhanced.py",
        "migrate_agent_hub.py"
    ]
    
    for file in root_agent_files:
        source = project_root / file
        if source.exists():
            dest = archive_dir / file
            shutil.move(str(source), str(dest))
            print(f"[OK] Archived from root: {file}")
    
    # 4. Create consolidation report
    report = {
        "timestamp": datetime.now().isoformat(),
        "actions_taken": [
            "Archived agent_hub_consolidated.py (duplicate)",
            "Archived agent_hub_enhanced.py (integrated features)",
            "Archived original agent_hub.py as agent_hub_monolithic.py",
            "Renamed agent_hub_refactored.py to agent_hub.py",
            "Archived root-level agent implementation files"
        ],
        "final_structure": {
            "main_entry": "pages/agent_hub.py (uses modular toolkit)",
            "toolkit_location": "toolkits/agent_hub/",
            "archive_location": str(archive_dir)
        },
        "recommendation": "All agent functionality is now in the modular toolkit"
    }
    
    import json
    report_file = project_root / "AGENT_HUB_CLEANUP_REPORT.json"
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n[OK] Cleanup complete! Report saved to: {report_file}")
    print(f"[DIR] Archived files moved to: {archive_dir}")
    
    # 5. Update imports check
    print("\n[INFO] Checking for import references...")
    files_to_check = list(project_root.rglob("*.py"))
    import_issues = []
    
    for file in files_to_check:
        if "archive" in str(file) or "__pycache__" in str(file):
            continue
            
        try:
            content = file.read_text(encoding='utf-8')
            if any(ref in content for ref in ["agent_hub_enhanced", "agent_hub_consolidated", "agent_hub_refactored"]):
                import_issues.append(str(file))
        except:
            pass
    
    if import_issues:
        print("\n[WARNING] Files that may need import updates:")
        for file in import_issues:
            print(f"  - {file}")
    else:
        print("\n[OK] No import issues found!")

if __name__ == "__main__":
    cleanup_agent_hubs()
