#!/usr/bin/env python3
"""
Migration script for health pages
Moves old health check pages to archive and updates references
"""

import os
import shutil
from pathlib import Path
import datetime

def migrate_health_pages():
    """Migrate old health check pages to archive"""
    
    # Create archive directory with timestamp
    timestamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = Path(f"archive/health_pages_{timestamp}")
    archive_dir.mkdir(parents=True, exist_ok=True)
    
    # Pages to archive
    old_pages = [
        "pages/database_health_check.py",
        "pages/ollama_health_check.py", 
        "pages/system_health.py"
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
    readme_content = f"""# Archived Health Pages
Archived on: {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}

## Reason for Archive
These individual health check pages have been consolidated into a unified Health Monitor toolkit.

## New Location
Use the unified Health Monitor instead:
- File: `pages/health_monitor.py`
- Toolkit: `toolkits/health/`

## Archived Files
{chr(10).join(f"- {f}" for f in moved_files)}

## Migration Notes
The functionality of these pages has been preserved and enhanced in the new Health Monitor:
- Database Health Check -> Health Monitor > Database Health tab
- Ollama Health Check -> Health Monitor > Ollama Health tab  
- System Health -> Health Monitor > System Diagnostics tab

The new system provides:
- Unified interface with tab navigation
- Better code organization following TuoKit principles
- Automatic knowledge capture through TuoKitToolBase
- Improved error handling and diagnostics
"""
    
    readme_path = archive_dir / "README.md"
    readme_path.write_text(readme_content)
    print(f"✓ Created archive README: {readme_path}")
    
    print(f"\n✅ Migration complete! Archived {len(moved_files)} files to {archive_dir}")
    print("\nNext steps:")
    print("1. Test the new Health Monitor: streamlit run pages/health_monitor.py")
    print("2. Update any documentation referencing the old pages")
    print("3. Commit the changes")

if __name__ == "__main__":
    migrate_health_pages()