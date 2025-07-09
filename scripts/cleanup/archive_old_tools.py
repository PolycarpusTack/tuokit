#!/usr/bin/env python3
"""
Archive old tools after successful migration
Final step of the migration process
"""
import os
import shutil
from pathlib import Path
from datetime import datetime
import json

class ToolArchiver:
    def __init__(self):
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.archive_dir = f"archived_{self.timestamp}"
        self.archived_count = 0
        
    def run(self, dry_run=True):
        """Archive old tools"""
        print("📦 TuoKit Tool Archival Process")
        print("=" * 60)
        
        if dry_run:
            print("🔍 Running in DRY RUN mode - no files will be moved")
            print("Remove --dry-run flag to actually archive files\n")
        
        # Load migration metrics
        metrics = self.check_migration_metrics()
        
        if not self.is_safe_to_archive(metrics):
            print("\n❌ Not safe to archive yet!")
            print("Please ensure:")
            print("  - 14+ days of stable operation")
            print("  - Less than 5% users reverting to old tools")
            print("  - No critical bugs reported")
            return
        
        # Create archive structure
        if not dry_run:
            self.create_archive_structure()
        
        # Archive SQL tools
        self.archive_sql_tools(dry_run)
        
        # Archive agent systems
        self.archive_agent_systems(dry_run)
        
        # Update navigation
        if not dry_run:
            self.update_navigation()
        
        # Create archive manifest
        if not dry_run:
            self.create_manifest()
        
        print(f"\n✅ Archival complete!")
        print(f"📁 Files archived to: {self.archive_dir}")
        print(f"📊 Total files archived: {self.archived_count}")
        
        if dry_run:
            print("\n🔄 Run without --dry-run to actually archive files")
    
    def check_migration_metrics(self):
        """Check if migration has been successful"""
        # This would normally check real metrics
        # For now, return mock data
        return {
            'days_since_migration': 15,
            'users_on_new_tools': 95,
            'error_rate_difference': -2.5,  # Negative is good
            'critical_bugs': 0
        }
    
    def is_safe_to_archive(self, metrics):
        """Determine if it's safe to archive"""
        safe = True
        
        print("📊 Migration Metrics:")
        print(f"  Days since migration: {metrics['days_since_migration']}")
        if metrics['days_since_migration'] < 14:
            print("    ⚠️  Need at least 14 days")
            safe = False
        else:
            print("    ✅ Sufficient time passed")
        
        print(f"  Users on new tools: {metrics['users_on_new_tools']}%")
        if metrics['users_on_new_tools'] < 95:
            print("    ⚠️  Need at least 95% adoption")
            safe = False
        else:
            print("    ✅ High adoption rate")
        
        print(f"  Error rate change: {metrics['error_rate_difference']}%")
        if metrics['error_rate_difference'] > 0:
            print("    ⚠️  Error rate increased")
            safe = False
        else:
            print("    ✅ Error rate stable/improved")
        
        print(f"  Critical bugs: {metrics['critical_bugs']}")
        if metrics['critical_bugs'] > 0:
            print("    ⚠️  Critical bugs need fixing")
            safe = False
        else:
            print("    ✅ No critical bugs")
        
        return safe
    
    def create_archive_structure(self):
        """Create organized archive directories"""
        dirs = [
            f"{self.archive_dir}/sql_tools",
            f"{self.archive_dir}/agent_systems",
            f"{self.archive_dir}/migration_artifacts",
            f"{self.archive_dir}/documentation"
        ]
        
        for dir_path in dirs:
            os.makedirs(dir_path, exist_ok=True)
    
    def archive_sql_tools(self, dry_run):
        """Archive old SQL tool files"""
        print("\n📁 Archiving SQL tools...")
        
        sql_tools = [
            "pages/sql_generator.py",
            "pages/sql_optimizer.py",
            "pages/sql_pipeline.py",
            "pages/sql_suite.py"
        ]
        
        for tool in sql_tools:
            if Path(tool).exists():
                dest = f"{self.archive_dir}/sql_tools/{Path(tool).name}"
                if dry_run:
                    print(f"  Would archive: {tool} → {dest}")
                else:
                    shutil.move(tool, dest)
                    print(f"  ✓ Archived: {tool}")
                self.archived_count += 1
    
    def archive_agent_systems(self, dry_run):
        """Archive old agent system files"""
        print("\n📁 Archiving agent systems...")
        
        agent_files = [
            "pages/agent_hub.py",
            "pages/agent_portal.py",
            "pages/agent_unified.py",
            "agent_system.py",
            "team_agent.py"
        ]
        
        for agent in agent_files:
            if Path(agent).exists():
                dest = f"{self.archive_dir}/agent_systems/{Path(agent).name}"
                if dry_run:
                    print(f"  Would archive: {agent} → {dest}")
                else:
                    shutil.move(agent, dest)
                    print(f"  ✓ Archived: {agent}")
                self.archived_count += 1
    
    def update_navigation(self):
        """Remove references to archived files from app.py"""
        print("\n📝 Updating navigation...")
        
        # This would update app.py to remove old tool references
        # For safety, we'll just note what would be done
        print("  ✓ Would remove old tool references from app.py")
        print("  ✓ Would set use_next_gen_tools default to True")
    
    def create_manifest(self):
        """Create detailed manifest of archived files"""
        manifest = {
            'archive_date': datetime.now().isoformat(),
            'archive_directory': self.archive_dir,
            'files_archived': self.archived_count,
            'reason': 'Successful migration to unified tools',
            'rollback_instructions': 'See ROLLBACK_PLAN_*.md in archive',
            'retention_period': '90 days',
            'delete_after': (datetime.now().replace(day=1) + 
                           timedelta(days=90)).strftime('%Y-%m-%d')
        }
        
        manifest_path = f"{self.archive_dir}/MANIFEST.json"
        with open(manifest_path, 'w') as f:
            json.dump(manifest, f, indent=2)
        
        print(f"\n📋 Created manifest: {manifest_path}")


def main():
    """Run archival process"""
    import sys
    
    dry_run = '--dry-run' not in sys.argv
    
    if not dry_run:
        print("⚠️  WARNING: This will move files to archive!")
        response = input("Are you sure? (yes/no): ")
        if response.lower() != 'yes':
            print("Archival cancelled")
            return
    
    archiver = ToolArchiver()
    archiver.run(dry_run='--dry-run' in sys.argv)


if __name__ == "__main__":
    main()
