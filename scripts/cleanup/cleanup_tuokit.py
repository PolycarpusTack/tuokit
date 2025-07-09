#!/usr/bin/env python3
"""
TuoKit Cleanup Execution Script
Safely consolidates overlapping functionality
"""

import os
import shutil
from datetime import datetime
import json

class TuoKitCleaner:
    def __init__(self):
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.archive_dir = f"archived_{self.timestamp}"
        self.cleanup_log = []
        
    def log(self, message):
        """Log cleanup actions"""
        entry = f"[{datetime.now().strftime('%H:%M:%S')}] {message}"
        print(entry)
        self.cleanup_log.append(entry)
    
    def create_archive_structure(self):
        """Create organized archive directories"""
        dirs = [
            f"{self.archive_dir}/agents",
            f"{self.archive_dir}/sql_tools", 
            f"{self.archive_dir}/ruby_tools",
            f"{self.archive_dir}/smalltalk_tools",
            f"{self.archive_dir}/old_files"
        ]
        
        for dir_path in dirs:
            os.makedirs(dir_path, exist_ok=True)
        self.log(f"Created archive structure: {self.archive_dir}")
    
    def cleanup_agents(self):
        """Archive redundant agent implementations"""
        self.log("=== Cleaning up Agent Systems ===")
        
        to_archive = [
            ("pages/agent_hub.py", "Over-engineered hub"),
            ("pages/agent_portal.py", "Redundant portal"),
            ("pages/agent_unified.py", "Failed consolidation attempt"),
            ("agent_system.py", "Old robust system"),
            ("team_agent.py", "Team agent - functionality in agent_lite")
        ]
        
        archived_count = 0
        for file_path, reason in to_archive:
            if os.path.exists(file_path):
                dest = f"{self.archive_dir}/agents/{os.path.basename(file_path)}"
                shutil.move(file_path, dest)
                self.log(f"Archived {file_path} - Reason: {reason}")
                archived_count += 1
        
        self.log(f"Archived {archived_count} agent files. Keeping only agent_lite.py")
    
    def cleanup_sql_tools(self):
        """Prepare SQL tools for consolidation"""
        self.log("=== Analyzing SQL Tools ===")
        
        sql_tools = [
            "pages/sql_generator.py",
            "pages/sql_optimizer.py", 
            "pages/sql_pipeline.py",
            "pages/sql_suite.py"
        ]
        
        # First, analyze what functions exist in each
        duplicates = []
        for tool in sql_tools:
            if os.path.exists(tool):
                self.log(f"Found SQL tool: {tool}")
                # Check for generate_sql, optimize_sql functions
                with open(tool, 'r') as f:
                    content = f.read()
                    if 'def generate_sql' in content:
                        duplicates.append(f"{tool} has generate_sql")
                    if 'def optimize_sql' in content:
                        duplicates.append(f"{tool} has optimize_sql")
        
        if duplicates:
            self.log("Found duplicate SQL functions:")
            for dup in duplicates:
                self.log(f"  - {dup}")
        
        # Don't auto-archive yet - let user decide
        self.log("Recommendation: Consolidate into single sql_toolkit.py")
    
    def cleanup_old_files(self):
        """Remove clearly obsolete files"""
        self.log("=== Removing Obsolete Files ===")
        
        obsolete = [
            ("utils_old.py", "Backup of old utils"),
            ("CLEANUP_COMPLETE.py", "Old cleanup script"),
            ("app_modern_backup.py", "Backup file"),
            ("consolidate_tools.py", "Old consolidation attempt"),
            ("consolidate_tools_v2.py", "Old consolidation attempt v2"),
            ("verify_cleanup.py", "Verification script"),
            ("verify_error_decoder.py", "Verification script"),
            ("verify_smalltalk_tools.py", "Verification script")
        ]
        
        archived_count = 0
        for file_path, reason in obsolete:
            if os.path.exists(file_path):
                dest = f"{self.archive_dir}/old_files/{os.path.basename(file_path)}"
                shutil.move(file_path, dest)
                self.log(f"Archived {file_path} - Reason: {reason}")
                archived_count += 1
        
        self.log(f"Archived {archived_count} obsolete files")
    
    def analyze_ruby_rails_tools(self):
        """Analyze Ruby/Rails tools for consolidation"""
        self.log("=== Analyzing Ruby/Rails Tools ===")
        
        rails_tools = []
        ruby_tools = []
        
        pages_dir = "pages"
        if os.path.exists(pages_dir):
            for file in os.listdir(pages_dir):
                if file.startswith("rails_") and file.endswith(".py"):
                    rails_tools.append(file)
                elif file.startswith("ruby_") and file.endswith(".py"):
                    ruby_tools.append(file)
        
        self.log(f"Found {len(rails_tools)} Rails tools:")
        for tool in rails_tools:
            self.log(f"  - {tool}")
            
        self.log(f"Found {len(ruby_tools)} Ruby tools:")
        for tool in ruby_tools:
            self.log(f"  - {tool}")
        
        self.log("Recommendation: Consolidate into rails_toolkit.py and ruby_toolkit.py")
    
    def create_cleanup_report(self):
        """Generate detailed cleanup report"""
        report_path = f"{self.archive_dir}/cleanup_report.json"
        
        report = {
            "timestamp": self.timestamp,
            "archive_directory": self.archive_dir,
            "actions": self.cleanup_log,
            "recommendations": [
                "1. Review archived files before permanent deletion",
                "2. Create unified sql_toolkit.py to replace SQL tools",
                "3. Consolidate Ruby/Rails tools into toolkits",
                "4. Update app.py navigation to remove archived pages",
                "5. Test agent_lite.py to ensure it has all needed features"
            ]
        }
        
        with open(report_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        self.log(f"Created cleanup report: {report_path}")
    
    def run(self, safe_mode=True):
        """Execute cleanup with safety checks"""
        print("🧹 TuoKit Cleanup Script")
        print("=" * 50)
        
        if safe_mode:
            print("Running in SAFE MODE - files will be archived, not deleted")
            response = input("Continue? (y/n): ")
            if response.lower() != 'y':
                print("Cleanup cancelled")
                return
        
        # Execute cleanup phases
        self.create_archive_structure()
        self.cleanup_agents()
        self.cleanup_sql_tools()
        self.cleanup_old_files()
        self.analyze_ruby_rails_tools()
        self.create_cleanup_report()
        
        print("\n" + "=" * 50)
        print("✅ Cleanup Complete!")
        print(f"📁 Archived files are in: {self.archive_dir}")
        print("📋 Review the cleanup report before making permanent changes")
        print("\nNext steps:")
        print("1. Create consolidated tool files (sql_toolkit.py, etc.)")
        print("2. Update navigation in app.py")
        print("3. Test thoroughly")
        print("4. Delete archive after verification (30 days recommended)")

if __name__ == "__main__":
    cleaner = TuoKitCleaner()
    cleaner.run(safe_mode=True)
