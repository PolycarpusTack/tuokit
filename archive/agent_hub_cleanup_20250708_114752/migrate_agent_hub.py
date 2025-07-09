"""
Agent Hub Migration Script
Consolidates agent hub implementations and fixes imports
"""

import os
import shutil
from datetime import datetime
from pathlib import Path


def migrate_agent_hub():
    """Migrate agent hub to use the new toolkit"""
    
    print("Starting Agent Hub migration...")
    
    # Create backup directory
    backup_dir = Path(f"backups/agent_hub_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
    backup_dir.mkdir(parents=True, exist_ok=True)
    
    # Backup existing files
    files_to_backup = [
        "pages/agent_hub.py",
        "pages/agent_hub_enhanced.py",
        "agent_hub_enhancements.py"
    ]
    
    for file in files_to_backup:
        if Path(file).exists():
            shutil.copy2(file, backup_dir)
            print(f"[BACKUP] Backed up {file}")
    
    # Archive old implementations
    archive_dir = Path("archive/agent_hub_old")
    archive_dir.mkdir(parents=True, exist_ok=True)
    
    # Move old files to archive
    for file in ["pages/agent_hub.py", "pages/agent_hub_enhanced.py"]:
        if Path(file).exists():
            shutil.move(file, archive_dir / Path(file).name)
            print(f"[ARCHIVE] Archived {file}")
    
    # Rename consolidated version to main
    if Path("pages/agent_hub_consolidated.py").exists():
        shutil.move("pages/agent_hub_consolidated.py", "pages/agent_hub.py")
        print("[SUCCESS] Installed new consolidated agent hub")
    
    # Update navigation.py if needed
    nav_file = Path("utils/navigation.py")
    if nav_file.exists():
        content = nav_file.read_text()
        
        # Check if update is needed
        if '"agent_hub_enhanced.py"' in content:
            content = content.replace('"agent_hub_enhanced.py"', '"agent_hub.py"')
            nav_file.write_text(content)
            print("[SUCCESS] Updated navigation.py")
    
    # Create migration report
    report = {
        "migration_date": datetime.now().isoformat(),
        "backed_up_files": files_to_backup,
        "backup_location": str(backup_dir),
        "archive_location": str(archive_dir),
        "new_toolkit_location": "toolkits/agent_hub/",
        "status": "completed"
    }
    
    # Save report
    import json
    with open("AGENT_HUB_MIGRATION_REPORT.json", "w") as f:
        json.dump(report, f, indent=2)
    
    print("\n[COMPLETE] Migration completed successfully!")
    print(f"[INFO] Report saved to AGENT_HUB_MIGRATION_REPORT.json")
    print(f"[INFO] Backups saved to {backup_dir}")
    print(f"[INFO] Old files archived to {archive_dir}")
    

if __name__ == "__main__":
    migrate_agent_hub()
