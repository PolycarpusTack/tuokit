"""
Rails Toolkit Migration Script for TuoKit (Python version)
Migrates from individual Rails tools to unified toolkit
"""

import os
import shutil
from pathlib import Path

def migrate_rails_toolkit():
    """Migrate from individual Rails tools to unified toolkit"""
    
    print("Rails Toolkit Migration Script")
    print("=================================")
    
    # Check if we're in TuoKit directory
    if not os.path.exists("app.py"):
        print("❌ Error: Not in TuoKit root directory")
        print("Please run from C:/Projects/Tuokit/")
        return False
    
    # Rails tools to migrate
    old_tools = [
        "rails_controller_gen",
        "rails_debugger", 
        "rails_graphql",
        "rails_model_gen",
        "rails_scaffold",
        "rails_system_tests",
        "rails_toolkit",
        "rails_upgrader"
    ]
    
    # Create backup directory
    print("\n[+] Creating backup directory...")
    backup_dir = Path("backups/rails_tools_backup")
    backup_dir.mkdir(parents=True, exist_ok=True)
    
    # Backup old Rails tools
    print("\n[+] Backing up old Rails tools...")
    backed_up = 0
    for tool in old_tools:
        tool_file = f"{tool}.py"
        if os.path.exists(tool_file):
            shutil.copy2(tool_file, backup_dir / tool_file)
            print(f"  [OK] Backed up {tool_file}")
            backed_up += 1
    
    if backed_up == 0:
        print("  [INFO] No old Rails tools found to backup")
    
    # Archive old tools
    print("\n[+] Archiving old tools...")
    archive_dir = Path("archived/old_rails_tools")
    archive_dir.mkdir(parents=True, exist_ok=True)
    
    archived = 0
    for tool in old_tools:
        tool_file = f"{tool}.py"
        if os.path.exists(tool_file):
            shutil.move(tool_file, archive_dir / tool_file)
            print(f"  [OK] Archived {tool_file}")
            archived += 1
    
    # Update pages/__init__.py if it exists
    pages_init = Path("pages/__init__.py")
    if pages_init.exists():
        print("\n[+] Updating pages/__init__.py...")
        
        # Read current content
        with open(pages_init, 'r') as f:
            content = f.read()
        
        # Remove old imports
        lines = content.split('\n')
        new_lines = []
        for line in lines:
            if not any(tool in line for tool in old_tools):
                new_lines.append(line)
        
        # Add new import if not exists
        if "rails_ultimate_toolkit" not in '\n'.join(new_lines):
            new_lines.append("from . import rails_ultimate_toolkit")
        
        # Write back
        with open(pages_init, 'w') as f:
            f.write('\n'.join(new_lines))
        
        print("  [OK] Updated imports")
    
    # Create navigation update guide
    print("\n[+] Creating navigation update guide...")
    nav_guide = """# Navigation Update Guide for Rails Toolkit

## Update your app.py or navigation configuration:

### Old navigation entries (remove these):
```python
"Rails Scaffold": "rails_scaffold",
"Rails Model Gen": "rails_model_gen", 
"Rails Controller": "rails_controller_gen",
"Rails Debugger": "rails_debugger",
"Rails GraphQL": "rails_graphql",
"Rails Tests": "rails_system_tests",
"Rails Upgrader": "rails_upgrader",
```

### New navigation entry (add this):
```python
"Rails Toolkit": "rails_ultimate_toolkit",
```

Or in your pages structure:
```python
rails_pages = {
    "Rails Ultimate Toolkit": rails_ultimate_toolkit
}
```
"""
    
    with open("NAVIGATION_UPDATE_GUIDE.md", 'w') as f:
        f.write(nav_guide)
    
    # Summary
    print("\n[SUCCESS] Migration complete!")
    print("\nSummary:")
    print(f"  - Backed up {backed_up} old tools to: backups/rails_tools_backup/")
    print(f"  - Archived {archived} old tools to: archived/old_rails_tools/")
    print("  - New toolkit: rails_ultimate_toolkit.py")
    print("  - Navigation guide: NAVIGATION_UPDATE_GUIDE.md")
    
    print("\nNext steps:")
    print("  1. Review NAVIGATION_UPDATE_GUIDE.md")
    print("  2. Update your app.py navigation")
    print("  3. Test the new unified toolkit")
    print("  4. Delete backups once confirmed working")
    
    print("\nTips:")
    print("  - All Rails tools are now in one place")
    print("  - Access via tabs: Generators, Testing, API, Debug, Advanced")
    print("  - 25+ tools available (vs 8 before)")
    
    print("\nHappy Rails development!")
    
    return True

if __name__ == "__main__":
    # Run migration
    success = migrate_rails_toolkit()
    
    if success:
        print("\n[SUCCESS] Migration successful!")
    else:
        print("\n[ERROR] Migration failed - please check the errors above")
