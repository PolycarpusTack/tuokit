#!/usr/bin/env python3
"""
TuoKit Consolidated Migration Manager
Unified migration system with version tracking, rollback, and safety features
"""

import os
import sys
import json
import shutil
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Any
import psycopg2
import sqlite3
import pymysql
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

class MigrationManager:
    """Manages database migrations and tool consolidation"""
    
    def __init__(self, db_type: str = None):
        self.db_type = db_type or os.getenv('TUOKIT_DB_TYPE', 'postgresql')
        self.migration_table = 'migration_history'
        self.backup_dir = Path('backups/migrations')
        self.backup_dir.mkdir(parents=True, exist_ok=True)
        
        # Migration files registry
        self.sql_migrations = {
            'v0.1.0': {
                'file': 'database_setup.sql',
                'description': 'Initial database schema',
                'dependencies': []
            },
            'v0.4.0': {
                'file': 'database_migration_v0.4.sql',
                'description': 'Add verified column to knowledge_units',
                'dependencies': ['v0.1.0']
            },
            'v1.0.0': {
                'file': 'database_migration_agents.sql',
                'description': 'Agent system tables',
                'dependencies': ['v0.1.0']
            },
            'v1.1.0': {
                'file': 'database_migration_lite_agents.sql',
                'description': 'Lite agent pipeline storage',
                'dependencies': ['v1.0.0']
            },
            'v1.2.0': {
                'file': 'database_migration_knowledge_graph.sql',
                'description': 'Knowledge graph support',
                'dependencies': ['v0.1.0']
            },
            'v2.0.0': {
                'file': 'database_migration_ruby_tools.sql',
                'description': 'Ruby performance and testing tools',
                'dependencies': ['v0.1.0']
            },
            'v2.1.0': {
                'file': 'database_migration_advanced_ruby.sql',
                'description': 'Advanced Ruby features',
                'dependencies': ['v2.0.0']
            },
            'v2.2.0': {
                'file': 'database_migration_professional_ruby.sql',
                'description': 'Professional Ruby tools',
                'dependencies': ['v2.1.0']
            }
        }
        
        # Tool consolidation migrations
        self.tool_migrations = {
            'consolidate_sql': {
                'script': 'consolidate_tools.py',
                'description': 'Consolidate 5 SQL tools into unified SQL toolkit',
                'target_files': ['sql_toolkit.py'],
                'source_files': ['sql_generator.py', 'sql_optimizer.py', 'sql_pipeline.py'],
                'feature_toggle': 'use_sql_toolkit'
            },
            'consolidate_agents': {
                'script': 'complete_unified_tools.py',
                'description': 'Consolidate agent systems into unified hub',
                'target_files': ['agent_hub.py'],
                'source_files': ['agent_portal.py', 'agent_unified.py', 'agent_lite.py'],
                'feature_toggle': 'use_agent_hub'
            }
        }
        
        # Database configuration
        self.db_config = self._get_db_config()
        
    def _get_db_config(self) -> Dict[str, Any]:
        """Get database configuration based on type"""
        if self.db_type == 'postgresql':
            return {
                'host': os.getenv('TUOKIT_PG_HOST', 'localhost'),
                'port': os.getenv('TUOKIT_PG_PORT', '5432'),
                'dbname': os.getenv('TUOKIT_PG_DB', 'tuokit_knowledge'),
                'user': os.getenv('TUOKIT_PG_USER', 'tuokit_user'),
                'password': os.getenv('TUOKIT_PG_PASSWORD', 'your_secure_password')
            }
        elif self.db_type == 'sqlite':
            return {
                'path': os.getenv('TUOKIT_SQLITE_PATH', 'tuokit.db')
            }
        elif self.db_type == 'mysql':
            return {
                'host': os.getenv('TUOKIT_MYSQL_HOST', 'localhost'),
                'port': int(os.getenv('TUOKIT_MYSQL_PORT', '3306')),
                'database': os.getenv('TUOKIT_MYSQL_DB', 'tuokit_knowledge'),
                'user': os.getenv('TUOKIT_MYSQL_USER', 'tuokit_user'),
                'password': os.getenv('TUOKIT_MYSQL_PASSWORD', 'your_secure_password')
            }
        else:
            raise ValueError(f"Unknown database type: {self.db_type}")
    
    def _get_connection(self):
        """Get database connection"""
        if self.db_type == 'postgresql':
            return psycopg2.connect(**self.db_config)
        elif self.db_type == 'sqlite':
            return sqlite3.connect(self.db_config['path'])
        elif self.db_type == 'mysql':
            return pymysql.connect(**self.db_config)
    
    def initialize_migration_tracking(self) -> bool:
        """Create migration tracking table"""
        print("📊 Initializing migration tracking...")
        
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            # Create migration history table
            if self.db_type == 'postgresql':
                sql = """
                CREATE TABLE IF NOT EXISTS migration_history (
                    id SERIAL PRIMARY KEY,
                    version VARCHAR(20) NOT NULL UNIQUE,
                    migration_type VARCHAR(20) NOT NULL,
                    description TEXT,
                    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    rollback_at TIMESTAMP,
                    checksum VARCHAR(64),
                    status VARCHAR(20) DEFAULT 'applied'
                )
                """
            elif self.db_type == 'sqlite':
                sql = """
                CREATE TABLE IF NOT EXISTS migration_history (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    version TEXT NOT NULL UNIQUE,
                    migration_type TEXT NOT NULL,
                    description TEXT,
                    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    rollback_at TIMESTAMP,
                    checksum TEXT,
                    status TEXT DEFAULT 'applied'
                )
                """
            elif self.db_type == 'mysql':
                sql = """
                CREATE TABLE IF NOT EXISTS migration_history (
                    id INT AUTO_INCREMENT PRIMARY KEY,
                    version VARCHAR(20) NOT NULL UNIQUE,
                    migration_type VARCHAR(20) NOT NULL,
                    description TEXT,
                    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    rollback_at TIMESTAMP NULL,
                    checksum VARCHAR(64),
                    status VARCHAR(20) DEFAULT 'applied'
                )
                """
            
            cur.execute(sql)
            conn.commit()
            cur.close()
            conn.close()
            
            print("✅ Migration tracking initialized")
            return True
            
        except Exception as e:
            print(f"❌ Failed to initialize migration tracking: {e}")
            return False
    
    def get_current_version(self) -> Optional[str]:
        """Get current migration version"""
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            cur.execute("""
                SELECT version FROM migration_history 
                WHERE status = 'applied' AND migration_type = 'sql'
                ORDER BY applied_at DESC LIMIT 1
            """)
            
            result = cur.fetchone()
            cur.close()
            conn.close()
            
            return result[0] if result else None
            
        except Exception:
            return None
    
    def get_applied_migrations(self) -> List[str]:
        """Get list of applied migrations"""
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            cur.execute("""
                SELECT version FROM migration_history 
                WHERE status = 'applied'
                ORDER BY applied_at
            """)
            
            migrations = [row[0] for row in cur.fetchall()]
            cur.close()
            conn.close()
            
            return migrations
            
        except Exception:
            return []
    
    def get_pending_migrations(self) -> List[Tuple[str, Dict]]:
        """Get list of pending migrations"""
        applied = self.get_applied_migrations()
        pending = []
        
        for version, info in self.sql_migrations.items():
            if version not in applied:
                # Check if dependencies are met
                deps_met = all(dep in applied for dep in info['dependencies'])
                if deps_met:
                    pending.append((version, info))
        
        return pending
    
    def backup_database(self, reason: str = "migration") -> Optional[str]:
        """Create database backup"""
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        backup_name = f"backup_{reason}_{timestamp}"
        
        print(f"💾 Creating backup: {backup_name}")
        
        try:
            if self.db_type == 'postgresql':
                backup_file = self.backup_dir / f"{backup_name}.sql"
                cmd = [
                    'pg_dump',
                    f'-h{self.db_config["host"]}',
                    f'-p{self.db_config["port"]}',
                    f'-U{self.db_config["user"]}',
                    f'-d{self.db_config["dbname"]}',
                    f'-f{backup_file}'
                ]
                
                env = os.environ.copy()
                env['PGPASSWORD'] = self.db_config['password']
                
                result = subprocess.run(cmd, env=env, capture_output=True, text=True)
                if result.returncode != 0:
                    raise Exception(result.stderr)
                    
            elif self.db_type == 'sqlite':
                backup_file = self.backup_dir / f"{backup_name}.db"
                shutil.copy2(self.db_config['path'], backup_file)
                
            elif self.db_type == 'mysql':
                backup_file = self.backup_dir / f"{backup_name}.sql"
                cmd = [
                    'mysqldump',
                    f'-h{self.db_config["host"]}',
                    f'-P{self.db_config["port"]}',
                    f'-u{self.db_config["user"]}',
                    f'-p{self.db_config["password"]}',
                    self.db_config['database']
                ]
                
                with open(backup_file, 'w') as f:
                    result = subprocess.run(cmd, stdout=f, stderr=subprocess.PIPE, text=True)
                    if result.returncode != 0:
                        raise Exception(result.stderr)
            
            print(f"✅ Backup created: {backup_file}")
            return str(backup_file)
            
        except Exception as e:
            print(f"❌ Backup failed: {e}")
            return None
    
    def apply_sql_migration(self, version: str, dry_run: bool = False) -> bool:
        """Apply a SQL migration"""
        if version not in self.sql_migrations:
            print(f"❌ Unknown migration version: {version}")
            return False
        
        migration = self.sql_migrations[version]
        migration_file = Path(migration['file'])
        
        if not migration_file.exists():
            print(f"❌ Migration file not found: {migration_file}")
            return False
        
        print(f"\n{'[DRY RUN] ' if dry_run else ''}Applying migration {version}: {migration['description']}")
        
        if dry_run:
            print(f"Would execute: {migration_file}")
            return True
        
        try:
            # Read SQL file
            with open(migration_file, 'r') as f:
                sql = f.read()
            
            # Execute migration
            conn = self._get_connection()
            cur = conn.cursor()
            
            # Split and execute statements
            statements = self._split_sql_statements(sql)
            for statement in statements:
                if statement.strip():
                    cur.execute(statement)
            
            # Record migration
            cur.execute("""
                INSERT INTO migration_history (version, migration_type, description)
                VALUES (%s, %s, %s)
            """, (version, 'sql', migration['description']))
            
            conn.commit()
            cur.close()
            conn.close()
            
            print(f"✅ Migration {version} applied successfully")
            return True
            
        except Exception as e:
            print(f"❌ Migration {version} failed: {e}")
            if 'conn' in locals():
                conn.rollback()
            return False
    
    def _split_sql_statements(self, sql: str) -> List[str]:
        """Split SQL into individual statements"""
        # Remove comments
        lines = []
        for line in sql.split('\n'):
            if not line.strip().startswith('--'):
                lines.append(line)
        sql = '\n'.join(lines)
        
        # Split by semicolon
        statements = sql.split(';')
        return [s.strip() for s in statements if s.strip()]
    
    def rollback_migration(self, version: str) -> bool:
        """Rollback a specific migration"""
        print(f"🔄 Rolling back migration {version}...")
        
        # Create backup first
        backup_file = self.backup_database(f"rollback_{version}")
        if not backup_file:
            print("❌ Rollback cancelled: backup failed")
            return False
        
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            # Get rollback SQL if exists
            rollback_file = Path(f"rollback_{version}.sql")
            if rollback_file.exists():
                with open(rollback_file, 'r') as f:
                    rollback_sql = f.read()
                
                statements = self._split_sql_statements(rollback_sql)
                for statement in statements:
                    if statement.strip():
                        cur.execute(statement)
            
            # Update migration status
            cur.execute("""
                UPDATE migration_history 
                SET status = 'rolled_back', rollback_at = CURRENT_TIMESTAMP
                WHERE version = %s
            """, (version,))
            
            conn.commit()
            cur.close()
            conn.close()
            
            print(f"✅ Migration {version} rolled back")
            return True
            
        except Exception as e:
            print(f"❌ Rollback failed: {e}")
            return False
    
    def migrate_to_version(self, target_version: str, dry_run: bool = False) -> bool:
        """Migrate to a specific version"""
        print(f"🎯 Migrating to version {target_version}...")
        
        if target_version not in self.sql_migrations:
            print(f"❌ Unknown target version: {target_version}")
            return False
        
        # Get migration path
        path = self._get_migration_path(target_version)
        if not path:
            print("❌ No migration path found")
            return False
        
        print(f"📋 Migration path: {' → '.join(path)}")
        
        # Create backup before starting
        if not dry_run:
            backup_file = self.backup_database(f"migrate_to_{target_version}")
            if not backup_file:
                print("❌ Migration cancelled: backup failed")
                return False
        
        # Apply migrations in order
        for version in path:
            if not self.apply_sql_migration(version, dry_run):
                print(f"❌ Migration stopped at {version}")
                return False
        
        print(f"✅ Successfully migrated to {target_version}")
        return True
    
    def _get_migration_path(self, target: str) -> List[str]:
        """Get migration path to target version"""
        applied = set(self.get_applied_migrations())
        path = []
        
        # Build dependency graph
        to_apply = set()
        queue = [target]
        
        while queue:
            version = queue.pop(0)
            if version in applied or version in to_apply:
                continue
            
            to_apply.add(version)
            deps = self.sql_migrations[version]['dependencies']
            queue.extend(deps)
        
        # Sort by dependencies
        sorted_versions = []
        while to_apply:
            for version in to_apply:
                deps = self.sql_migrations[version]['dependencies']
                if all(dep in applied or dep not in to_apply for dep in deps):
                    sorted_versions.append(version)
                    to_apply.remove(version)
                    applied.add(version)
                    break
        
        return sorted_versions
    
    def consolidate_tools(self, consolidation_key: str, dry_run: bool = False) -> bool:
        """Run tool consolidation migration"""
        if consolidation_key not in self.tool_migrations:
            print(f"❌ Unknown consolidation: {consolidation_key}")
            return False
        
        consolidation = self.tool_migrations[consolidation_key]
        print(f"\n{'[DRY RUN] ' if dry_run else ''}Consolidating: {consolidation['description']}")
        
        if dry_run:
            print(f"Would run: {consolidation['script']}")
            print(f"Source files: {', '.join(consolidation['source_files'])}")
            print(f"Target files: {', '.join(consolidation['target_files'])}")
            return True
        
        # Create backup
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        backup_dir = Path(f"backups/consolidation_{timestamp}")
        backup_dir.mkdir(parents=True, exist_ok=True)
        
        # Backup source files
        for source_file in consolidation['source_files']:
            source_path = Path('pages') / source_file
            if source_path.exists():
                shutil.copy2(source_path, backup_dir / source_file)
        
        # Run consolidation script
        try:
            result = subprocess.run(
                [sys.executable, consolidation['script']],
                capture_output=True,
                text=True
            )
            
            if result.returncode != 0:
                raise Exception(result.stderr)
            
            # Record in migration history
            conn = self._get_connection()
            cur = conn.cursor()
            
            cur.execute("""
                INSERT INTO migration_history (version, migration_type, description)
                VALUES (%s, %s, %s)
            """, (consolidation_key, 'tool', consolidation['description']))
            
            conn.commit()
            cur.close()
            conn.close()
            
            print(f"✅ Consolidation complete: {consolidation_key}")
            print(f"💾 Backup saved to: {backup_dir}")
            return True
            
        except Exception as e:
            print(f"❌ Consolidation failed: {e}")
            return False
    
    def show_migration_status(self):
        """Display current migration status"""
        print("\n" + "="*60)
        print("📊 Migration Status")
        print("="*60)
        
        # Current version
        current = self.get_current_version()
        print(f"Current version: {current or 'None'}")
        
        # Applied migrations
        applied = self.get_applied_migrations()
        print(f"\nApplied migrations ({len(applied)}):")
        for version in applied:
            info = self.sql_migrations.get(version, {})
            print(f"  ✅ {version}: {info.get('description', 'Unknown')}")
        
        # Pending migrations
        pending = self.get_pending_migrations()
        if pending:
            print(f"\nPending migrations ({len(pending)}):")
            for version, info in pending:
                print(f"  ⏳ {version}: {info['description']}")
        else:
            print("\n✨ All migrations applied!")
        
        # Tool consolidations
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            cur.execute("""
                SELECT version, description, applied_at 
                FROM migration_history 
                WHERE migration_type = 'tool' AND status = 'applied'
                ORDER BY applied_at
            """)
            
            consolidations = cur.fetchall()
            if consolidations:
                print("\nTool consolidations:")
                for version, desc, applied_at in consolidations:
                    print(f"  ✅ {version}: {desc} (applied {applied_at})")
            
            cur.close()
            conn.close()
            
        except Exception:
            pass
        
        print("="*60)
    
    def run_all_pending(self, dry_run: bool = False) -> bool:
        """Run all pending migrations"""
        pending = self.get_pending_migrations()
        
        if not pending:
            print("✨ No pending migrations")
            return True
        
        print(f"\n{'[DRY RUN] ' if dry_run else ''}Running {len(pending)} pending migrations...")
        
        # Create backup
        if not dry_run:
            backup_file = self.backup_database("all_pending")
            if not backup_file:
                print("❌ Migration cancelled: backup failed")
                return False
        
        # Apply each migration
        for version, info in pending:
            if not self.apply_sql_migration(version, dry_run):
                return False
        
        return True
    
    def verify_integrity(self) -> bool:
        """Verify database integrity after migrations"""
        print("\n🔍 Verifying database integrity...")
        
        checks_passed = 0
        checks_failed = 0
        
        try:
            conn = self._get_connection()
            cur = conn.cursor()
            
            # Check core tables exist
            core_tables = [
                'queries',
                'knowledge_units',
                'migration_history'
            ]
            
            for table in core_tables:
                try:
                    cur.execute(f"SELECT COUNT(*) FROM {table}")
                    count = cur.fetchone()[0]
                    print(f"  ✅ Table {table}: {count} records")
                    checks_passed += 1
                except Exception as e:
                    print(f"  ❌ Table {table}: {str(e)}")
                    checks_failed += 1
            
            # Check indexes
            if self.db_type == 'postgresql':
                cur.execute("""
                    SELECT indexname FROM pg_indexes 
                    WHERE tablename IN ('queries', 'knowledge_units')
                """)
                indexes = cur.fetchall()
                print(f"  ✅ Found {len(indexes)} indexes")
                checks_passed += 1
            
            cur.close()
            conn.close()
            
        except Exception as e:
            print(f"  ❌ Integrity check failed: {e}")
            checks_failed += 1
        
        print(f"\n{'✅' if checks_failed == 0 else '❌'} Integrity check: {checks_passed} passed, {checks_failed} failed")
        return checks_failed == 0


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="TuoKit Migration Manager",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python migration_manager.py --status              # Show migration status
  python migration_manager.py --migrate             # Run all pending migrations
  python migration_manager.py --migrate --dry-run   # Preview migrations
  python migration_manager.py --version v2.0.0      # Migrate to specific version
  python migration_manager.py --rollback v2.0.0     # Rollback specific version
  python migration_manager.py --consolidate sql     # Run tool consolidation
  python migration_manager.py --verify              # Verify database integrity

Migration commands:
  --init         Initialize migration tracking
  --status       Show current migration status
  --migrate      Run all pending migrations
  --version      Migrate to specific version
  --rollback     Rollback specific version
  --consolidate  Run tool consolidation
  --verify       Verify database integrity
  
Options:
  --dry-run      Preview without applying changes
  --db-type      Database type (postgresql, sqlite, mysql)
        """
    )
    
    # Commands
    parser.add_argument('--init', action='store_true', help='Initialize migration tracking')
    parser.add_argument('--status', action='store_true', help='Show migration status')
    parser.add_argument('--migrate', action='store_true', help='Run all pending migrations')
    parser.add_argument('--version', help='Migrate to specific version')
    parser.add_argument('--rollback', help='Rollback specific version')
    parser.add_argument('--consolidate', help='Run tool consolidation (sql or agents)')
    parser.add_argument('--verify', action='store_true', help='Verify database integrity')
    
    # Options
    parser.add_argument('--dry-run', action='store_true', help='Preview without applying')
    parser.add_argument('--db-type', choices=['postgresql', 'sqlite', 'mysql'],
                       help='Database type (default from .env)')
    
    args = parser.parse_args()
    
    # Create migration manager
    manager = MigrationManager(db_type=args.db_type)
    
    # Handle commands
    if args.init:
        manager.initialize_migration_tracking()
    elif args.status or not any(vars(args).values()):
        manager.show_migration_status()
    elif args.migrate:
        manager.run_all_pending(dry_run=args.dry_run)
    elif args.version:
        manager.migrate_to_version(args.version, dry_run=args.dry_run)
    elif args.rollback:
        if args.dry_run:
            print("❌ Cannot dry-run rollback")
        else:
            manager.rollback_migration(args.rollback)
    elif args.consolidate:
        if args.consolidate == 'sql':
            manager.consolidate_tools('consolidate_sql', dry_run=args.dry_run)
        elif args.consolidate == 'agents':
            manager.consolidate_tools('consolidate_agents', dry_run=args.dry_run)
        else:
            print(f"❌ Unknown consolidation: {args.consolidate}")
    elif args.verify:
        manager.verify_integrity()


if __name__ == "__main__":
    main()