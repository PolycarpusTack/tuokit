"""
Quick analysis of consolidation opportunities in TuoKit
"""
import os
from pathlib import Path
from collections import defaultdict

def analyze_consolidation_opportunities():
    project_root = Path("C:/Projects/Tuokit")
    
    # Define consolidation groups
    consolidation_groups = {
        "Ruby/Rails Tools": [
            "rails_controller_gen.py", "rails_model_gen.py", "rails_scaffold.py",
            "rails_graphql.py", "rails_system_tests.py", "rails_upgrader.py",
            "rspec_generator.py", "view_components.py", "rails_debugger.py",
            "ruby_profiler.py", "ruby_memory_optimizer.py", "ruby_pattern_matching.py",
            "ruby_c_extensions.py", "ruby_ractors.py", "ruby_katas.py"
        ],
        "Smalltalk Tools": [
            "smalltalk_class_gen.py", "smalltalk_explainer.py", "smalltalk_meta.py",
            "smalltalk_refactorer.py", "smalltalk_ruby_converter.py", 
            "smalltalk_snippets.py", "seaside_generator.py"
        ],
        "Error Analysis": [
            "error_tool.py", "exception_advisor.py", "crash_analyzer.py"
        ],
        "Learning Tools": [
            "edu_mind.py", "study_guide_generator.py", "sql_academy.py"
        ],
        "Documentation Tools": [
            "doc_tools.py", "knowledge_lib.py", "help_guide.py"
        ],
        "Setup Scripts": [],  # Will find dynamically
        "Test Scripts": [],   # Will find dynamically
        "Migration Scripts": []  # Will find dynamically
    }
    
    # Find setup, test, and migration scripts
    for file in project_root.glob("*.py"):
        filename = file.name
        if "setup" in filename.lower():
            consolidation_groups["Setup Scripts"].append(filename)
        elif "test_" in filename:
            consolidation_groups["Test Scripts"].append(filename)
        elif "migration" in filename.lower() or "migrate" in filename.lower():
            consolidation_groups["Migration Scripts"].append(filename)
    
    # Also check pages directory
    pages_dir = project_root / "pages"
    
    # Calculate metrics
    print("=" * 60)
    print("TuoKit Consolidation Opportunities Analysis")
    print("=" * 60)
    
    total_files = 0
    total_size = 0
    
    for group_name, files in consolidation_groups.items():
        if not files:
            continue
            
        group_size = 0
        existing_files = []
        
        for filename in files:
            # Check in pages directory first
            filepath = pages_dir / filename
            if not filepath.exists():
                # Check in root directory
                filepath = project_root / filename
            
            if filepath.exists():
                size = filepath.stat().st_size
                group_size += size
                existing_files.append(filename)
        
        if existing_files:
            total_files += len(existing_files)
            total_size += group_size
            
            print(f"\n{group_name}:")
            print(f"  Files: {len(existing_files)}")
            print(f"  Total size: {group_size:,} bytes ({group_size/1024:.1f} KB)")
            print(f"  Consolidation: {len(existing_files)} → 1 file")
            print(f"  Files to consolidate:")
            for f in existing_files[:5]:  # Show first 5
                print(f"    - {f}")
            if len(existing_files) > 5:
                print(f"    ... and {len(existing_files) - 5} more")
    
    print(f"\n{'=' * 60}")
    print(f"TOTAL CONSOLIDATION OPPORTUNITY:")
    print(f"  Current files: {total_files}")
    print(f"  After consolidation: ~10-15 files")
    print(f"  Reduction: ~{((total_files - 12) / total_files * 100):.0f}%")
    print(f"  Total size affected: {total_size:,} bytes ({total_size/1024/1024:.1f} MB)")
    print("=" * 60)

if __name__ == "__main__":
    analyze_consolidation_opportunities()
