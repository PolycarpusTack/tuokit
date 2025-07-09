import os
import shutil
from pathlib import Path

# Define where remaining files should go
file_mappings = {
    # Documentation files -> docs/
    "COMPLETE_CLEANUP_SUMMARY.md": "docs/status/",
    "CONSOLIDATED_SETUP_MIGRATION.md": "docs/guides/",
    "consolidation-summary.md": "docs/status/",
    "CRASH_ANALYZER_FIXED.md": "docs/status/",
    "DATABASE_SETUP.md": "docs/guides/",
    "ERROR_DECODER_ENHANCED.md": "docs/status/",
    "OLLAMA_UTILITIES_FIXED.md": "docs/status/",
    "README_scanner_v2.md": "docs/",
    "RUBY_RAILS_MIGRATION_EXAMPLE.md": "docs/guides/",
    "SMALLTALK_RAILS_TOOLS.md": "docs/guides/",
    "STUDY_GUIDE_ENHANCEMENTS.md": "docs/guides/",
    "UNIFIED_ENTRY_POINT.md": "docs/guides/",
    "Tuokit Ideas.md": "docs/",
    "master-consolidation-prompts.md": "docs/guides/",
    "fix_ollama_wsl.md": "docs/guides/",
    
    # Scripts -> scripts/
    "feature_toggle_snippet.py": "scripts/maintenance/",
    "migration_manager.py": "scripts/migration/",
    "start_scanner.py": "scripts/maintenance/",
    "tuokit_launcher.py": "scripts/maintenance/",
    
    # Shell scripts -> scripts/
    "run_mockup.sh": "scripts/maintenance/",
    "start_modern_ui.sh": "scripts/maintenance/",
    "start_tuokit.sh": "scripts/maintenance/",
    
    # Requirements variants -> docs/
    "requirements_complete_mockup.txt": "docs/",
    "requirements_fixed.txt": "docs/",
    "requirements_mockup.txt": "docs/",
    "requirements_scanner_v2.txt": "docs/",
    
    # Other files
    "code_scan_20250702_114358.json": "archive/",
    "repomix-output.xml": "archive/",
    "tuokit_scan_results.sarif": "archive/",
    "tuokit": "scripts/maintenance/",
}

# Process moves
moved_count = 0
errors = []

for filename, destination in file_mappings.items():
    src = Path(filename)
    if src.exists():
        try:
            dest_dir = Path(destination)
            dest_dir.mkdir(parents=True, exist_ok=True)
            dest_path = dest_dir / filename
            
            # Handle duplicates
            if dest_path.exists():
                dest_path = dest_dir / f"{src.stem}_moved{src.suffix}"
            
            shutil.move(str(src), str(dest_path))
            moved_count += 1
            print(f"[MOVED] {filename} -> {destination}")
        except Exception as e:
            errors.append(f"Error moving {filename}: {e}")
    else:
        print(f"[SKIP] {filename} not found")

print(f"\n[COMPLETE] Moved {moved_count} files")
if errors:
    print(f"[ERRORS] {len(errors)} errors occurred:")
    for error in errors:
        print(f"  - {error}")
