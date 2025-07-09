#!/usr/bin/env python3
"""
Migration script to archive old Ruby-related pages
"""

import os
import shutil
from datetime import datetime

# Pages to archive
RUBY_PAGES = [
    "pages/ruby_memory_optimizer.py",
    "pages/ruby_profiler.py",
    "pages/ruby_pattern_matching.py",
    "pages/ruby_c_extensions.py",
    "pages/ruby_ractors.py",
    "pages/ruby_katas.py"
]

def archive_ruby_pages():
    """Archive old Ruby pages"""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    archive_dir = f"archive/ruby_pages_{timestamp}"
    
    # Create archive directory
    os.makedirs(archive_dir, exist_ok=True)
    
    # Archive each page
    archived = []
    not_found = []
    
    for page in RUBY_PAGES:
        if os.path.exists(page):
            dest = os.path.join(archive_dir, os.path.basename(page))
            shutil.move(page, dest)
            archived.append(page)
            print(f"✓ Archived: {page}")
        else:
            not_found.append(page)
            print(f"✗ Not found: {page}")
    
    # Create migration summary
    summary = f"""# Ruby Pages Migration Summary
    
Archive created: {timestamp}

## Archived Pages ({len(archived)}):
{chr(10).join(f"- {page}" for page in archived)}

## Not Found ({len(not_found)}):
{chr(10).join(f"- {page}" for page in not_found)}

## New Structure:
- Toolkit: toolkits/ruby/
  - analyzer.py - Main RubyToolkit class
  - optimization.py - Memory and performance tools
  - advanced.py - C extensions, Ractors, patterns
  - learning.py - Katas and best practices
  - config.py - Ruby configuration
  
- Page: pages/ruby_toolkit.py (thin wrapper)

## Access:
Navigate to "Development > Ruby Toolkit" in TuoKit
"""
    
    with open(os.path.join(archive_dir, "MIGRATION_SUMMARY.md"), "w") as f:
        f.write(summary)
    
    print(f"\n✅ Migration complete! Archived to: {archive_dir}")
    print(f"📄 See {archive_dir}/MIGRATION_SUMMARY.md for details")

if __name__ == "__main__":
    archive_ruby_pages()