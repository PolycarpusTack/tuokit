#!/usr/bin/env python3
"""
TuoKit Automated Cleanup Script (Safe version - no emojis)
Run this to organize your project structure
"""

import os
import shutil
from pathlib import Path
from datetime import datetime
import json

class TuoKitCleaner:
    def __init__(self, project_path="."):
        self.project_path = Path(project_path)
        self.backup_dir = self.project_path / f"backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        self.report = {
            "files_moved": 0,
            "files_deleted": 0,
            "directories_created": 0,
            "errors": []
        }
        
    def create_directory_structure(self):
        """Create the proper directory structure"""
        directories = [
            "scripts/setup",
            "scripts/migration", 
            "scripts/maintenance",
            "tests",
            "docs",
            "docs/status",
            "docs/completed",
            "archive/old_scripts",
            "archive/consolidation_attempts"
        ]
        
        for dir_path in directories:
            full_path = self.project_path / dir_path
            if not full_path.exists():
                full_path.mkdir(parents=True, exist_ok=True)
                self.report["directories_created"] += 1
                print(f"[CREATED] {dir_path}")
                
    def organize_files(self):
        """Move files to their proper locations"""
        
        # Define file movement rules
        file_rules = {
            # Test files to tests/
            "test_*.py": "tests/",
            "*_test.py": "tests/",
            "test_config.json": "tests/",
            "test_document.txt": "tests/",
            
            # Documentation to docs/
            "*_COMPLETE.md": "docs/completed/",
            "*_STATUS.md": "docs/status/",
            "CLEANUP_*.md": "docs/status/",
            "cleanup_report_*.md": "docs/status/",
            "cleanup_report_*.txt": "docs/status/",
            "PRIORITY_ACTIONS.md": "docs/",
            "FEATURES*.md": "docs/",
            "DAILY_CONSOLIDATION_REPORT.md": "docs/status/",
            "tuokit-review-summary.md": "docs/",
            "TUOKIT_CODE_REVIEW_2025.md": "docs/",
            
            # Scripts to scripts/
            "setup_*.py": "scripts/setup/",
            "setup_*.sql": "scripts/setup/",
            "add_*.py": "scripts/setup/",
            "migrate_*.py": "scripts/migration/",
            "fix_*.py": "scripts/maintenance/",
            "emergency_fix.py": "scripts/maintenance/",
            "update_*.py": "scripts/maintenance/",
            
            # Old toolkits to archive/
            "diagnostic_toolkit.py": "archive/old_scripts/",
            "documentation_toolkit.py": "archive/old_scripts/",
            "error_analysis_toolkit.py": "archive/old_scripts/",
            "learning_toolkit.py": "archive/old_scripts/",
            "smalltalk_toolkit.py": "archive/old_scripts/",
            
            # Consolidation attempts to archive/
            "consolidation*.py": "archive/consolidation_attempts/",
            "*_consolidation*.py": "archive/consolidation_attempts/",
            "tuokit-consolidation*.py": "archive/consolidation_attempts/",
            "integrate_*.py": "archive/consolidation_attempts/",
            
            # Old/backup files to archive/
            "*_old.py": "archive/old_scripts/",
            "*_backup.py": "archive/old_scripts/",
            "*_v2.py": "archive/old_scripts/",
            "enhanced_scanner_v2.py": "archive/old_scripts/",
            "tuokit_unified.py": "archive/old_scripts/",
            "complete_unified_tools.py": "archive/old_scripts/",
            
            # Example files
            "scanner_examples.py": "examples/",
            "tuokit_tool_examples.py": "examples/",
            "sample_*.sql": "examples/",
            
            # Batch files
            "*.bat": "scripts/maintenance/",
        }
        
        # Special handling for certain files that should stay in root
        keep_in_root = {
            "app.py", "requirements.txt", ".env", ".env.example", 
            ".gitignore", "tuokit.db", "start.py", "README.md",
            ".consolidation_complete", "cleanup_tuokit.py", "cleanup_tuokit_safe.py"
        }
        
        # Process files
        for pattern, destination in file_rules.items():
            for file_path in self.project_path.glob(pattern):
                if file_path.is_file() and file_path.name not in keep_in_root:
                    # Skip if already in correct location
                    if str(destination) in str(file_path):
                        continue
                        
                    # Skip files in backup directories
                    if "backup" in str(file_path):
                        continue
                        
                    try:
                        dest_dir = self.project_path / destination
                        dest_dir.mkdir(parents=True, exist_ok=True)
                        
                        dest_path = dest_dir / file_path.name
                        
                        # Handle duplicates
                        if dest_path.exists():
                            dest_path = dest_dir / f"{file_path.stem}_moved{file_path.suffix}"
                        
                        shutil.move(str(file_path), str(dest_path))
                        self.report["files_moved"] += 1
                        print(f"[MOVED] {file_path.name} -> {destination}")
                        
                    except Exception as e:
                        self.report["errors"].append(f"Error moving {file_path}: {e}")
                        
    def consolidate_database_setup(self):
        """Create unified database setup script"""
        setup_script = '''#!/usr/bin/env python3
"""
Unified Database Setup for TuoKit
Run this to initialize the database with all required tables
"""

import sqlite3
import os
from pathlib import Path

def setup_database(db_path="tuokit.db"):
    """Create all required tables for TuoKit"""
    
    print(f"Setting up database: {db_path}")
    
    # Create absolute path from current directory
    if not os.path.isabs(db_path):
        db_path = os.path.abspath(db_path)
    
    with sqlite3.connect(db_path) as conn:
        # Enable foreign keys
        conn.execute("PRAGMA foreign_keys = ON")
        
        # Queries table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS queries (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                tool TEXT NOT NULL,
                model TEXT NOT NULL,
                prompt TEXT NOT NULL,
                ai_response TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Knowledge base table  
        conn.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_base (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                query_id INTEGER,
                title TEXT NOT NULL,
                content TEXT NOT NULL,
                category TEXT,
                metadata TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (query_id) REFERENCES queries(id)
            )
        """)
        
        # System logs table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS system_logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                level TEXT NOT NULL,
                message TEXT NOT NULL,
                context TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Create indexes for performance
        conn.execute("CREATE INDEX IF NOT EXISTS idx_queries_tool ON queries(tool)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_queries_created ON queries(created_at)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_kb_category ON knowledge_base(category)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_kb_created ON knowledge_base(created_at)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_logs_level ON system_logs(level)")
        
        # Verify tables
        cursor = conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = cursor.fetchall()
        
        print("\\nCreated tables:")
        for table in tables:
            print(f"  [OK] {table[0]}")
            
    print("\\n[SUCCESS] Database setup complete!")

if __name__ == "__main__":
    # Go back to project root if we're in scripts/setup
    if os.path.basename(os.getcwd()) == "setup":
        os.chdir("../..")
    
    setup_database()
'''
        
        # Save the unified setup script
        setup_path = self.project_path / "scripts" / "setup" / "setup_database.py"
        setup_path.parent.mkdir(parents=True, exist_ok=True)
        setup_path.write_text(setup_script)
        print(f"[CREATED] Unified database setup: {setup_path}")
        
    def identify_duplicates(self):
        """Identify potential duplicate files"""
        duplicates = {}
        
        # Common duplicate patterns
        patterns = [
            ("sql_*.py", "SQL tools"),
            ("ollama_*.py", "Ollama utilities"),
            ("test_ollama*.py", "Ollama tests"),
            ("*_toolkit.py", "Toolkit files"),
        ]
        
        for pattern, category in patterns:
            files = list(self.project_path.glob(pattern))
            if len(files) > 1:
                duplicates[category] = [f.name for f in files]
                
        if duplicates:
            print("\n[WARNING] Potential duplicates found:")
            for category, files in duplicates.items():
                print(f"\n{category}:")
                for f in files:
                    print(f"  - {f}")
                    
        return duplicates
        
    def generate_cleanup_report(self):
        """Generate a detailed cleanup report"""
        report_path = self.project_path / "CLEANUP_REPORT_AUTOMATED.md"
        
        report_content = f"""# TuoKit Automated Cleanup Report

Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Summary

- **Files Moved**: {self.report['files_moved']}
- **Files Deleted**: {self.report['files_deleted']}  
- **Directories Created**: {self.report['directories_created']}
- **Errors**: {len(self.report['errors'])}

## Directory Structure

```
TuoKit/
├── app.py              # Main entry point
├── pages/              # Streamlit pages
├── utils/              # Utilities
├── scripts/            # All scripts
│   ├── setup/          # Setup scripts
│   ├── migration/      # Migration scripts
│   └── maintenance/    # Cleanup scripts
├── tests/              # All tests
├── docs/               # All documentation
├── archive/            # Old/duplicate files
└── requirements.txt    # Dependencies
```

## Next Steps

1. Review files in `archive/` directory
2. Delete truly redundant files
3. Update imports in remaining files
4. Test the application thoroughly
5. Commit the cleaned structure

## Errors

"""
        
        if self.report['errors']:
            for error in self.report['errors']:
                report_content += f"- {error}\n"
        else:
            report_content += "No errors encountered!\n"
            
        report_path.write_text(report_content)
        print(f"\n[REPORT] Cleanup report saved: {report_path}")
        
    def run_cleanup(self, dry_run=False):
        """Run the complete cleanup process"""
        print("[CLEANUP] TuoKit Cleanup Script")
        print("=" * 50)
        
        if dry_run:
            print("[DRY RUN] DRY RUN MODE - No files will be moved")
            print("=" * 50)
            
        # Step 1: Create backup reminder
        if not dry_run:
            print(f"\n1. [BACKUP] Please ensure you have a backup before proceeding")
            
        # Step 2: Create directory structure  
        print("\n2. Creating directory structure...")
        if not dry_run:
            self.create_directory_structure()
            
        # Step 3: Identify duplicates
        print("\n3. Identifying duplicates...")
        self.identify_duplicates()
        
        # Step 4: Organize files
        print("\n4. Organizing files...")
        if not dry_run:
            self.organize_files()
            
        # Step 5: Create unified database setup
        print("\n5. Creating unified database setup...")
        if not dry_run:
            self.consolidate_database_setup()
            
        # Step 6: Generate report
        print("\n6. Generating cleanup report...")
        if not dry_run:
            self.generate_cleanup_report()
            
        print("\n[SUCCESS] Cleanup complete!")
        print(f"Files moved: {self.report['files_moved']}")
        print(f"Directories created: {self.report['directories_created']}")
        
        if self.report['errors']:
            print(f"\n[WARNING] {len(self.report['errors'])} errors occurred. Check the report.")

if __name__ == "__main__":
    import sys
    
    # Check for dry-run flag
    dry_run = "--dry-run" in sys.argv or "-n" in sys.argv
    
    # Run cleanup
    cleaner = TuoKitCleaner()
    cleaner.run_cleanup(dry_run=dry_run)
    
    if dry_run:
        print("\n[INFO] To perform actual cleanup, run without --dry-run flag")
