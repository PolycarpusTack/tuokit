"""
Migration script for SmallTalk tools consolidation
Archives old files and updates imports
"""

import os
import shutil
from datetime import datetime

def migrate_smalltalk_tools():
    """Archive old SmallTalk tools and update references"""
    
    # Tools to be replaced by smalltalk_toolkit.py
    old_tools = [
        'smalltalk_class_gen.py',
        'smalltalk_explainer.py',
        'smalltalk_snippets.py',
        'smalltalk_refactorer.py',
        'smalltalk_ruby_converter.py',
        'seaside_generator.py',
        'smalltalk_meta.py'
    ]
    
    # Create archive directory
    archive_dir = f'archive/smalltalk_tools_{datetime.now().strftime("%Y%m%d_%H%M%S")}'
    os.makedirs(archive_dir, exist_ok=True)
    
    print(f"📁 Creating archive directory: {archive_dir}")
    
    # Archive old files from pages directory
    archived = []
    pages_dir = 'pages'
    
    for tool in old_tools:
        tool_path = os.path.join(pages_dir, tool)
        if os.path.exists(tool_path):
            try:
                shutil.move(tool_path, os.path.join(archive_dir, tool))
                archived.append(tool)
                print(f"✅ Archived: {tool}")
            except Exception as e:
                print(f"❌ Failed to archive {tool}: {e}")
        else:
            print(f"⚠️  Not found: {tool_path}")
    
    # Create migration notes
    notes = f"""# SmallTalk Tools Migration Notes
Date: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

## Consolidated Tools
The following tools have been combined into `smalltalk_toolkit.py`:

{chr(10).join(f'- {tool}' for tool in old_tools)}

## New Unified Tool
All functionality is now available in `smalltalk_toolkit.py` with tabs:
- 🏗️ Class Generator
- 🧑‍🏫 Code Explainer
- 📚 Snippet Library
- 🔧 Refactorer
- 🔄 Ruby Converter
- 🌊 Seaside Generator
- ✨ Metaprogramming
- 📖 Quick Reference

## Benefits
1. Single entry point for all SmallTalk development
2. Shared configuration and database connections
3. Consistent UI/UX across all tools
4. Reduced code duplication
5. Easier maintenance

## Archived Files
{chr(10).join(f'- {tool}' for tool in archived)}

## Database Compatibility
The new tool uses the same database schema and will access all existing saved queries and knowledge units.
"""
    
    with open(os.path.join(archive_dir, 'MIGRATION_NOTES.md'), 'w', encoding='utf-8') as f:
        f.write(notes)
    
    print(f"\n📝 Migration notes saved to: {archive_dir}/MIGRATION_NOTES.md")
    
    # Update pages/__init__.py if it exists
    init_file = 'pages/__init__.py'
    if os.path.exists(init_file):
        print(f"\n📝 Updating {init_file}...")
        
        with open(init_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Comment out old imports and add new one
        new_content = content
        for tool in old_tools:
            tool_name = tool.replace('.py', '')
            new_content = new_content.replace(
                f'from pages.{tool_name} import show as {tool_name}_show',
                f'# from pages.{tool_name} import show as {tool_name}_show  # Moved to smalltalk_toolkit'
            )
        
        # Add new import if not already there
        if 'smalltalk_toolkit' not in new_content:
            new_content += '\nfrom pages.smalltalk_toolkit import show as smalltalk_toolkit_show\n'
        
        with open(init_file, 'w', encoding='utf-8') as f:
            f.write(new_content)
        
        print("✅ Updated imports in pages/__init__.py")
    
    print(f"\n✨ Migration complete! Archived {len(archived)} files.")
    print("\n🚀 Next steps:")
    print("1. Update any navigation references to point to smalltalk_toolkit.py")
    print("2. Test the new unified tool")
    print("3. Remove the archive directory once confirmed working")

if __name__ == "__main__":
    response = input("⚠️  This will archive old SmallTalk tools. Continue? (y/n): ")
    if response.lower() == 'y':
        migrate_smalltalk_tools()
    else:
        print("Migration cancelled.")
