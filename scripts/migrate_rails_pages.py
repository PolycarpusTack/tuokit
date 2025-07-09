#!/usr/bin/env python3
"""
Migration script for Rails pages
Archives individual Rails pages now consolidated in Rails Ultimate Toolkit
"""

import os
import shutil
from pathlib import Path
import datetime

def migrate_rails_pages():
    """Archive old Rails pages that are now in the toolkit"""
    
    # Create archive directory with timestamp
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = Path(f"archive/rails_pages_{timestamp}")
    archive_dir.mkdir(parents=True, exist_ok=True)
    
    # Pages to archive (keeping rails_ultimate_toolkit.py as the main entry)
    old_pages = [
        "pages/rails_controller_gen.py",
        "pages/rails_debugger.py",
        "pages/rails_graphql.py",
        "pages/rails_model_gen.py",
        "pages/rails_scaffold.py",
        "pages/rails_system_tests.py",
        "pages/rails_toolkit.py",  # Old Rails toolkit
        "pages/rails_upgrader.py",
        "rails_ultimate_toolkit.py"  # Move the root one to archive
    ]
    
    moved_files = []
    
    for page in old_pages:
        page_path = Path(page)
        if page_path.exists():
            # Archive the file
            archive_path = archive_dir / page_path.name
            shutil.move(str(page_path), str(archive_path))
            moved_files.append(page)
            print(f"✓ Archived: {page} -> {archive_path}")
        else:
            print(f"⚠ Not found: {page}")
    
    # Create a README in the archive
    readme_content = f"""# Archived Rails Pages
Archived on: {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

## Reason for Archive
These individual Rails tool pages have been consolidated into the Rails Ultimate Toolkit.

## New Location
Use the unified Rails Ultimate Toolkit instead:
- File: `pages/rails_ultimate_toolkit.py`
- Toolkit: `toolkits/rails/`

## Archived Files
{chr(10).join(f"- {f}" for f in moved_files)}

## Migration Notes
The functionality of these pages has been preserved and enhanced in the Rails Ultimate Toolkit:

### Tool Mapping
- rails_model_gen.py -> Rails Toolkit > Generators > Model
- rails_controller_gen.py -> Rails Toolkit > Generators > Controller
- rails_scaffold.py -> Rails Toolkit > Generators > Scaffold
- rails_graphql.py -> Rails Toolkit > API Tools > GraphQL
- rails_system_tests.py -> Rails Toolkit > Testing > System Tests
- rails_debugger.py -> Rails Toolkit > Debugging > Error Analyzer
- rails_upgrader.py -> Rails Toolkit > DevOps > Rails Upgrader

The new toolkit provides:
- Unified interface with category navigation
- 25+ Rails tools in one place
- Better code organization
- Consistent UI/UX
- Automatic knowledge capture
"""
    
    readme_path = archive_dir / "README.md"
    readme_path.write_text(readme_content)
    print(f"✓ Created archive README: {readme_path}")
    
    print(f"\n✅ Migration complete! Archived {len(moved_files)} files to {archive_dir}")
    print("\nNext steps:")
    print("1. Test the Rails Ultimate Toolkit: streamlit run pages/rails_ultimate_toolkit.py")
    print("2. Update navigation if needed")
    print("3. Commit the changes")

if __name__ == "__main__":
    migrate_rails_pages()