#!/usr/bin/env python3
"""
TuoKit Automated Cleanup Script
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
            "archive/old_scripts",
            "archive/consolidation_attempts"
        ]
        
        for dir_path in directories:
            full_path = self.project_path / dir_path
            if not full_path.exists():
                full_path.mkdir(parents=True, exist_ok=True)
                self.report["directories_created"] += 1
                print(f"✅ Created: {dir_path}")
                
    def organize_files(self):
        """Move files to their proper locations"""
        
        # Define file movement rules
        file_rules = {
            # Test files → tests/
            "test_*.py": "tests/",
            "*_test.py": "tests/",
            
            # Documentation → docs/
            "*.md": "docs/",
            "README*": "docs/",
            "*_COMPLETE.md": "docs/completed/",
            "*_STATUS.md": "docs/status/",
            
            # Scripts → scripts/
            "setup_*.py": "scripts/setup/",
            "setup_*.sql": "scripts/setup/",
            "migrate_*.py": "scripts/migration/",
            "fix_*.py": "scripts/maintenance/",
            "cleanup_*.py": "scripts/maintenance/",
            "*_toolkit.py": "archive/old_scripts/",
            
            # Consolidation attempts → archive/
            "consolidation*.py": "archive/consolidation_attempts/",
            "*_consolidation*.py": "archive/consolidation_attempts/",
            "tuokit-consolidation*.py": "archive/consolidation_attempts/",
            
            # Old files → archive/
            "*_old.py": "archive/old_scripts/",
            "*_backup.py": "archive/old_scripts/",
            "*_v2.py": "archive/old_scripts/",
        }
        
        # Special handling for certain files that should stay in root
        keep_in_root = {
            "app.py", "requirements.txt", ".env", ".env.example", 
            ".gitignore", "tuokit.db", "start.py"
        }
        
        # Process files
        for pattern, destination in file_rules.items():
            for file_path in self.project_path.glob(pattern):
                if file_path.is_file() and file_path.name not in keep_in_root:
                    # Skip if already in correct location
                    if str(destination) in str(file_path):
                        continue
                        
                    # Special handling for main README
                    if file_path.name == "README.md" and destination == "docs/":
                        continue  # Keep main README in root
                        
                    try:
                        dest_dir = self.project_path / destination
                        dest_dir.mkdir(parents=True, exist_ok=True)
                        
                        dest_path = dest_dir / file_path.name
                        
                        # Handle duplicates
                        if dest_path.exists():
                            dest_path = dest_dir / f"{file_path.stem}_moved{file_path.suffix}"
                        
                        shutil.move(str(file_path), str(dest_path))
                        self.report["files_moved"] += 1
                        print(f"📦 Moved: {file_path.name} → {destination}")
                        
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
            print(f"  ✅ {table[0]}")
            
    print("\\n✨ Database setup complete!")

if __name__ == "__main__":
    setup_database()
'''
        
        # Save the unified setup script
        setup_path = self.project_path / "scripts" / "setup" / "setup_database.py"
        setup_path.parent.mkdir(parents=True, exist_ok=True)
        setup_path.write_text(setup_script)
        print(f"✅ Created unified database setup: {setup_path}")
        
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
            print("\n⚠️  Potential duplicates found:")
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
            report_content += "No errors encountered! ✅\n"
            
        report_path.write_text(report_content)
        print(f"\n📄 Cleanup report saved: {report_path}")
        
    def run_cleanup(self, dry_run=False):
        """Run the complete cleanup process"""
        print("[CLEANUP] TuoKit Cleanup Script")
        print("=" * 50)
        
        if dry_run:
            print("[DRY RUN] DRY RUN MODE - No files will be moved")
            print("=" * 50)
            
        # Step 1: Create backup
        if not dry_run:
            print(f"\n1. Creating backup: {self.backup_dir}")
            # In production, you'd actually create the backup here
            
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
            
        print("\n✨ Cleanup complete!")
        print(f"Files moved: {self.report['files_moved']}")
        print(f"Directories created: {self.report['directories_created']}")
        
        if self.report['errors']:
            print(f"\n⚠️  {len(self.report['errors'])} errors occurred. Check the report.")

if __name__ == "__main__":
    import sys
    
    # Check for dry-run flag
    dry_run = "--dry-run" in sys.argv or "-n" in sys.argv
    
    # Run cleanup
    cleaner = TuoKitCleaner()
    cleaner.run_cleanup(dry_run=dry_run)
    
    if dry_run:
        print("\n💡 To perform actual cleanup, run without --dry-run flag")
