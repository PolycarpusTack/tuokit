"""
TuoKit Tool Consolidation Script
Safely migrates to unified tools while preserving old code
Run this after testing the new unified tools
"""

import os
import shutil
from datetime import datetime

def create_archive_dir():
    """Create archive directory with timestamp"""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = f"archived_tools_{timestamp}"
    os.makedirs(archive_dir, exist_ok=True)
    return archive_dir

def migrate_sql_tools(archive_dir):
    """Archive old SQL tools and update imports"""
    print("🛢️ Migrating SQL tools...")
    
    # Files to archive
    sql_files = [
        "pages/sql_generator.py",
        "pages/sql_optimizer.py", 
        "pages/sql_pipeline.py"
    ]
    
    # Archive old files
    for file in sql_files:
        if os.path.exists(file):
            shutil.move(file, os.path.join(archive_dir, os.path.basename(file)))
            print(f"  ✓ Archived {file}")
    
    # Update app.py to use new sql_suite
    update_app_imports("sql_generator", "sql_suite")
    update_app_imports("sql_optimizer", "sql_suite")
    update_app_imports("sql_pipeline", "sql_suite")
    
    print("  ✓ SQL tools consolidated into sql_suite.py")

def migrate_agent_tools(archive_dir):
    """Archive old agent systems and update imports"""
    print("🤖 Migrating agent systems...")
    
    # Files to archive
    agent_files = [
        "agent_system.py",
        "team_agent.py",
        "pages/agent_portal.py",
        "pages/agent_unified.py",
        "pages/agent_lite.py"  # We'll extract its logic first
    ]    
    # Archive old files
    for file in agent_files:
        if os.path.exists(file):
            shutil.move(file, os.path.join(archive_dir, os.path.basename(file)))
            print(f"  ✓ Archived {file}")
    
    # Update app.py to use new agent_hub
    update_app_imports("agent_lite", "agent_hub")
    update_app_imports("agent_portal", "agent_hub")
    update_app_imports("agent_unified", "agent_hub")
    
    print("  ✓ Agent systems consolidated into agent_hub.py")

def update_app_imports(old_name, new_name):
    """Update imports in app.py"""
    app_file = "app.py"
    if os.path.exists(app_file):
        with open(app_file, 'r') as f:
            content = f.read()
        
        # Update page references
        content = content.replace(f'"{old_name}"', f'"{new_name}"')
        content = content.replace(f"'{old_name}'", f"'{new_name}'")
        content = content.replace(f"pages/{old_name}", f"pages/{new_name}")
        
        with open(app_file, 'w') as f:
            f.write(content)

def create_readme(archive_dir):
    """Create README in archive directory"""
    readme_content = f"""# Archived TuoKit Tools

Archived on: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

## Why these were archived:
- **SQL tools**: Consolidated into unified `sql_suite.py`
- **Agent systems**: Consolidated into unified `agent_hub.py`

## What's new:
- `pages/sql_suite.py`: Single entry point for all SQL operations (generate, optimize, pipeline, translate)
- `pages/agent_hub.py`: Unified agent interface with simple and advanced modes
- `utils/sql_tools.py`: Already existed as the backend for SQL operations

## If you need to restore:
1. Copy files back from this archive to their original locations
2. Update app.py to reference the old page names
3. Delete the new unified files

Note: The new unified tools preserve all functionality while reducing code duplication.
"""
    
    with open(os.path.join(archive_dir, "README.md"), 'w') as f:
        f.write(readme_content)

def verify_new_tools():
    """Verify new tools exist before migration"""
    required_files = [
        "pages/sql_suite.py",
        "pages/agent_hub.py",
        "utils/sql_tools.py"
    ]
    
    for file in required_files:
        if not os.path.exists(file):
            print(f"❌ Missing required file: {file}")
            return False
    
    print("✅ All new tools verified")
    return True

def main():
    """Run the consolidation"""
    print("🚀 TuoKit Tool Consolidation")
    print("=" * 40)
    
    # Check new tools exist
    if not verify_new_tools():
        print("\n⚠️  Please ensure new unified tools are created first!")
        return
    
    # Create archive directory
    archive_dir = create_archive_dir()
    print(f"\n📁 Archive directory: {archive_dir}")
    
    # Confirm with user
    response = input("\n⚠️  This will archive old tools. Continue? (yes/no): ")
    if response.lower() != 'yes':
        print("Cancelled.")
        return
    
    print()
    
    # Perform migrations
    migrate_sql_tools(archive_dir)
    print()
    migrate_agent_tools(archive_dir)
    
    # Create README
    create_readme(archive_dir)
    print(f"\n📄 Created README in {archive_dir}")
    
    print("\n✅ Consolidation complete!")
    print("\nNext steps:")
    print("1. Test the new unified tools (sql_suite.py and agent_hub.py)")
    print("2. Update any custom scripts that imported the old tools")
    print("3. After confirming everything works, you can delete the archive")
    print(f"\nOld files preserved in: {archive_dir}")

if __name__ == "__main__":
    main()