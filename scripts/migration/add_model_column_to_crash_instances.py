#!/usr/bin/env python3
"""
Add model column to crash_instances table
Tracks which LLM model was used for each crash analysis
"""

import os
import sys
import psycopg2
from datetime import datetime
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Database configuration
DB_CONFIG = {
    "dbname": os.getenv("TUOKIT_PG_DB", os.getenv("DB_NAME", "tuokit_knowledge")),
    "user": os.getenv("TUOKIT_PG_USER", os.getenv("DB_USER", "tuokit_user")),
    "password": os.getenv("TUOKIT_PG_PASSWORD", os.getenv("DB_PASSWORD", "your_secure_password")),
    "host": os.getenv("TUOKIT_PG_HOST", os.getenv("DB_HOST", "localhost")),
    "port": os.getenv("TUOKIT_PG_PORT", "5432")
}

def get_connection():
    """Get database connection"""
    return psycopg2.connect(**DB_CONFIG)

def run_migration():
    """Add model column to crash_instances table"""
    print("🚀 Adding model column to crash_instances table...")
    print("=" * 60)
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Start transaction
        conn.autocommit = False
        
        print("1. Checking if model column already exists...")
        cur.execute("""
            SELECT column_name 
            FROM information_schema.columns 
            WHERE table_name = 'crash_instances' AND column_name = 'model_used'
        """)
        
        if cur.fetchone():
            print("   ℹ️  Model column already exists")
            return
        
        print("2. Adding model_used column...")
        cur.execute("""
            ALTER TABLE crash_instances 
            ADD COLUMN model_used VARCHAR(100) DEFAULT 'deepseek-r1:latest'
        """)
        print("   ✅ model_used column added")
        
        print("3. Creating index on model_used...")
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_instances_model 
            ON crash_instances(model_used)
        """)
        print("   ✅ Index created")
        
        print("4. Updating analytics view...")
        cur.execute("""
            CREATE OR REPLACE VIEW crash_pattern_summary AS
            SELECT 
                cp.id,
                cp.pattern_name,
                cp.error_type,
                cp.occurrence_count,
                cp.confidence_score,
                cp.first_seen,
                cp.last_seen,
                COUNT(DISTINCT ci.site_name) as affected_sites,
                COUNT(DISTINCT ci.whatson_version) as affected_versions,
                COUNT(DISTINCT ci.model_used) as models_used,
                AVG(ci.downtime_minutes) as avg_downtime,
                array_agg(DISTINCT ci.severity_rating) as severity_ratings,
                array_agg(DISTINCT ci.model_used) as model_list
            FROM crash_patterns cp
            LEFT JOIN crash_instances ci ON cp.id = ci.pattern_id
            GROUP BY cp.id;
        """)
        print("   ✅ Analytics view updated")
        
        print("5. Updating recent crashes view...")
        cur.execute("""
            CREATE OR REPLACE VIEW recent_crashes AS
            SELECT 
                ci.*,
                cp.pattern_name,
                cp.confidence_score
            FROM crash_instances ci
            LEFT JOIN crash_patterns cp ON ci.pattern_id = cp.id
            WHERE ci.created_at > NOW() - INTERVAL '7 days'
            ORDER BY ci.created_at DESC;
        """)
        print("   ✅ Recent crashes view updated")
        
        # Commit transaction
        conn.commit()
        print("\n✅ Migration completed successfully!")
        
        # Show summary
        cur.execute("SELECT COUNT(*) FROM crash_instances")
        instance_count = cur.fetchone()[0]
        
        print(f"\n📊 Updated {instance_count} crash instances with model tracking")
        
    except Exception as e:
        conn.rollback()
        print(f"\n❌ Migration failed: {e}")
        raise
    finally:
        cur.close()
        conn.close()

def rollback_migration():
    """Remove model column if needed"""
    print("🔄 Removing model column from crash_instances table...")
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Drop index first
        cur.execute("DROP INDEX IF EXISTS idx_crash_instances_model")
        
        # Remove column
        cur.execute("ALTER TABLE crash_instances DROP COLUMN IF EXISTS model_used")
        
        # Restore original views
        cur.execute("""
            CREATE OR REPLACE VIEW crash_pattern_summary AS
            SELECT 
                cp.id,
                cp.pattern_name,
                cp.error_type,
                cp.occurrence_count,
                cp.confidence_score,
                cp.first_seen,
                cp.last_seen,
                COUNT(DISTINCT ci.site_name) as affected_sites,
                COUNT(DISTINCT ci.whatson_version) as affected_versions,
                AVG(ci.downtime_minutes) as avg_downtime,
                array_agg(DISTINCT ci.severity_rating) as severity_ratings
            FROM crash_patterns cp
            LEFT JOIN crash_instances ci ON cp.id = ci.pattern_id
            GROUP BY cp.id;
        """)
        
        conn.commit()
        print("✅ Rollback completed")
        
    except Exception as e:
        print(f"❌ Rollback failed: {e}")
        conn.rollback()
    finally:
        cur.close()
        conn.close()

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Add model column to crash_instances")
    parser.add_argument('--rollback', action='store_true', help='Remove the model column')
    args = parser.parse_args()
    
    if args.rollback:
        response = input("⚠️  This will remove model tracking. Continue? (yes/no): ")
        if response.lower() == 'yes':
            rollback_migration()
        else:
            print("Rollback cancelled")
    else:
        run_migration()
        print("\n💡 Model tracking enabled for crash analysis!")