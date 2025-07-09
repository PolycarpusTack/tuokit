"""
TuoKit Tool Consolidation Script V2
Safely migrates to unified tools while preserving educational features
SQL Academy extracts learning features from sql_pipeline.py
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
    
    # Files to archive (keeping sql_pipeline.py for reference)
    sql_files = [
        "pages/sql_generator.py",
        "pages/sql_optimizer.py"
    ]
    
    # Archive old files
    for file in sql_files:
        if os.path.exists(file):
            shutil.move(file, os.path.join(archive_dir, os.path.basename(file)))
            print(f"  ✓ Archived {file}")
    
    # Special handling for sql_pipeline.py
    if os.path.exists("pages/sql_pipeline.py"):
        # Copy instead of move - keep for reference
        shutil.copy("pages/sql_pipeline.py", os.path.join(archive_dir, "sql_pipeline_reference.py"))
        # Then archive the original
        shutil.move("pages/sql_pipeline.py", os.path.join(archive_dir, "sql_pipeline.py"))
        print(f"  ✓ Archived sql_pipeline.py (educational features moved to sql_academy.py)")
    
    # Update app.py to use new tools
    update_app_imports("sql_generator", "sql_suite")
    update_app_imports("sql_optimizer", "sql_suite")
    update_app_imports("sql_pipeline", "sql_academy")
    
    print("  ✓ SQL tools consolidated:")
    print("    - Work tools → sql_suite.py")
    print("    - Learning tools → sql_academy.py")

def migrate_agent_tools(archive_dir):
    """Archive old agent systems and update imports"""
    print("🤖 Migrating agent systems...")
    
    # Files to archive
    agent_files = [
        "agent_system.py",
        "team_agent.py",
        "pages/agent_portal.py",
        "pages/agent_unified.py",
        "pages/agent_lite.py"
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

def update_page_categories():
    """Update app.py to add SQL Academy to educational tools"""
    app_file = "app.py"
    if os.path.exists(app_file):
        with open(app_file, 'r') as f:
            content = f.read()
        
        # Add SQL Academy to educational section if not already there
        if "sql_academy" not in content and "Educational Tools" in content:
            # Find educational tools section and add SQL Academy
            content = content.replace(
                '"study_guide_generator": "📚 Study Guide Generator"',
                '"study_guide_generator": "📚 Study Guide Generator",\n            "sql_academy": "🎓 SQL Academy"'
            )
        
        with open(app_file, 'w') as f:
            f.write(content)
        
        print("  ✓ Added SQL Academy to Educational Tools section")
def create_readme(archive_dir):
    """Create README in archive directory"""
    readme_content = f"""# Archived TuoKit Tools

Archived on: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

## Why these were archived:

### SQL Tools Consolidation:
- **sql_generator.py, sql_optimizer.py**: Consolidated into `sql_suite.py` (work tools)
- **sql_pipeline.py**: Educational features extracted to `sql_academy.py` (learning tools)

### Agent Systems Consolidation:
- **All agent files**: Consolidated into unified `agent_hub.py`

## What's new:

### Work Tools:
- `pages/sql_suite.py`: Single entry point for SQL work (generate, optimize, pipeline, translate)
- `pages/agent_hub.py`: Unified agent interface with simple and advanced modes

### Educational Tools:
- `pages/sql_academy.py`: Comprehensive SQL learning platform with:
  - Interactive concept library
  - Query analysis to identify concepts
  - Practice quizzes
  - Personalized learning paths
  - Integration with EduMind ecosystem

## Architecture Benefits:
- Clear separation: Work tools vs Learning tools
- No feature loss - everything preserved
- Better user experience - tools have clear purposes
- Easier maintenance - no code duplication

## If you need to restore:
1. Copy files back from this archive to their original locations
2. Update app.py to reference the old page names
3. Delete the new unified files

Note: The educational features from sql_pipeline.py are now better organized
in sql_academy.py as part of the TuoKit educational ecosystem.
"""
    
    with open(os.path.join(archive_dir, "README.md"), 'w') as f:
        f.write(readme_content)

def verify_new_tools():
    """Verify new tools exist before migration"""
    required_files = [
        "pages/sql_suite.py",
        "pages/sql_academy.py",
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
    print("🚀 TuoKit Tool Consolidation V2")
    print("=" * 50)
    print("This version preserves educational features!")
    print()
    
    # Check new tools exist
    if not verify_new_tools():
        print("\n⚠️  Please ensure new unified tools are created first!")
        print("Required files:")
        print("  - pages/sql_suite.py (SQL work tools)")
        print("  - pages/sql_academy.py (SQL learning tools)")
        print("  - pages/agent_hub.py (Unified agents)")
        return
    
    # Create archive directory
    archive_dir = create_archive_dir()
    print(f"\n📁 Archive directory: {archive_dir}")
    
    # Show what will happen
    print("\n📋 This will:")
    print("  1. Archive sql_generator.py and sql_optimizer.py")
    print("  2. Archive sql_pipeline.py (features moved to sql_academy.py)")
    print("  3. Archive all agent system files")
    print("  4. Update app.py to use new tools")
    print("  5. Add SQL Academy to Educational Tools section")
    
    # Confirm with user
    response = input("\n⚠️  Continue with consolidation? (yes/no): ")
    if response.lower() != 'yes':
        print("Cancelled.")
        return
    
    print()
    
    # Perform migrations
    migrate_sql_tools(archive_dir)
    print()
    migrate_agent_tools(archive_dir)
    print()
    update_page_categories()
    
    # Create README
    create_readme(archive_dir)
    print(f"\n📄 Created README in {archive_dir}")
    
    print("\n✅ Consolidation complete!")
    print("\n🎯 New architecture:")
    print("  Work Tools:")
    print("    - sql_suite.py: SQL generation, optimization, pipeline")
    print("    - agent_hub.py: Tool chaining and automation")
    print("  Educational Tools:")
    print("    - sql_academy.py: Interactive SQL learning")
    print("    - edu_mind.py: General education toolkit")
    print("    - study_guide_generator.py: Document-based learning")
    
    print("\n📝 Next steps:")
    print("1. Test all three new tools")
    print("2. Verify SQL Academy appears in Educational Tools menu")
    print("3. Check that all features work as expected")
    print(f"\n💾 Old files preserved in: {archive_dir}")

if __name__ == "__main__":
    main()