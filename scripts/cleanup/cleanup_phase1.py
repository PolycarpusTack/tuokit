#!/usr/bin/env python3
"""
TuoKit Phase 1 Cleanup Script
Quick wins to reduce clutter and confusion
Run with: python cleanup_phase1.py
"""
import os
import shutil
from datetime import datetime

def create_archive_structure():
    """Create organized archive directories"""
    dirs = [
        "archive/consolidation_attempts",
        "archive/old_sql_tools", 
        "archive/agent_versions",
        "archive/ruby_tools",
        "archive/test_files"
    ]
    for d in dirs:
        os.makedirs(d, exist_ok=True)
    return dirs

def archive_consolidation_files():
    """Move all consolidation-related files to archive"""
    consolidation_files = [
        "consolidate_tools.py",
        "consolidate_tools_v2.py", 
        "consolidation_auto.py",
        "apply_consolidation.bat",
        "run_consolidation_auto.bat",
        "run_consolidation_now.bat",
        ".consolidation_complete"
    ]
    
    # Move all consolidation reports
    import glob
    consolidation_files.extend(glob.glob("consolidation_report_*.md"))
    
    archived = []
    for f in consolidation_files:
        if os.path.exists(f):
            shutil.move(f, "archive/consolidation_attempts/")
            archived.append(f)
    
    return archived

def cleanup_sql_tools():
    """Remove duplicate SQL implementations"""
    # These are duplicates - functionality exists in utils/sql_tools.py
    duplicates = [
        "sql_optimizer.py",
        "sql_pipeline.py",
        "test_sql_simple.py",
        "test_sql_enterprise.py", 
        "test_sql_generator_enhanced.py",
        "test_sql_optimizer.py",
        "test_sql_pipeline.py"
    ]
    
    archived = []
    for f in duplicates:
        if os.path.exists(f):
            shutil.move(f, "archive/old_sql_tools/")
            archived.append(f)
            
    # Update sql_generator.py to use utils
    # (Manual step - needs code changes)
    
    return archived

def identify_ruby_tools():
    """List Ruby/Rails tools for decision"""
    ruby_pages = [
        "pages/rails_controller_gen.py",
        "pages/rails_debugger.py",
        "pages/rails_graphql.py",
        "pages/rails_model_gen.py",
        "pages/rails_scaffold.py",
        "pages/rails_system_tests.py",
        "pages/rails_toolkit.py",
        "pages/rails_upgrader.py",
        "pages/rspec_generator.py",
        "pages/ruby_c_extensions.py",
        "pages/ruby_katas.py",
        "pages/ruby_memory_optimizer.py",
        "pages/ruby_pattern_matching.py",
        "pages/ruby_profiler.py",
        "pages/ruby_ractors.py",
        "pages/ruby_toolkit.py"
    ]
    
    print("\n🤔 Decision needed on Ruby/Rails tools:")
    print("Option 1: Keep them (30% of your pages)")
    print("Option 2: Move to separate 'TuoKit-Ruby' project")
    print("Option 3: Archive them\n")
    
    return ruby_pages

def main():
    print("🧹 TuoKit Phase 1 Cleanup\n")
    
    # Create archive structure
    print("📁 Creating archive structure...")
    create_archive_structure()
    
    # Archive consolidation attempts
    print("\n📦 Archiving consolidation files...")
    consolidated = archive_consolidation_files()
    print(f"   Archived {len(consolidated)} consolidation files")
    
    # Clean up SQL tools
    print("\n🗄️ Cleaning up duplicate SQL tools...")
    sql_cleaned = cleanup_sql_tools()
    print(f"   Archived {len(sql_cleaned)} duplicate SQL files")
    
    # Identify Ruby tools
    ruby_tools = identify_ruby_tools()
    
    # Generate report
    report = f"""
# TuoKit Phase 1 Cleanup Report
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Files Archived
- Consolidation attempts: {len(consolidated)}
- Duplicate SQL tools: {len(sql_cleaned)}

## Next Steps
1. Update pages/sql_generator.py to use utils/sql_tools.py
2. Decide on Ruby/Rails tools ({len(ruby_tools)} files)
3. Choose single agent system (recommend agent_lite.py)
4. Run phase 2 cleanup

## Manual Tasks Required
- [ ] Update SQL generator imports
- [ ] Remove agent system links from navigation
- [ ] Test remaining functionality
"""
    
    with open("PHASE1_CLEANUP_REPORT.md", "w") as f:
        f.write(report)
    
    print(f"\n✅ Phase 1 complete! See PHASE1_CLEANUP_REPORT.md")
    print("\n🎯 Immediate win: Project will be 30% cleaner!")

if __name__ == "__main__":
    main()
