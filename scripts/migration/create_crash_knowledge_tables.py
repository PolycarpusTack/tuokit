#!/usr/bin/env python3
"""
Create Crash Knowledge System tables
Enables self-learning crash analysis with pattern recognition
"""

import os
import sys
import psycopg2
from datetime import datetime

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

from utils.database import DB_CONFIG

def get_connection():
    """Get database connection"""
    return psycopg2.connect(**DB_CONFIG)

def run_migration():
    """Run the crash knowledge migration"""
    print("🚀 Starting Crash Knowledge System migration...")
    print("=" * 60)
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Start transaction
        conn.autocommit = False
        
        print("1. Creating crash_patterns table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS crash_patterns (
                id SERIAL PRIMARY KEY,
                fingerprint_exact VARCHAR(64) UNIQUE,
                fingerprint_structural VARCHAR(64),
                pattern_name VARCHAR(255),
                error_type VARCHAR(100),
                
                -- WCR specific fields
                cause_of_dump TEXT,
                stack_signature TEXT,
                
                -- Learning metrics
                occurrence_count INTEGER DEFAULT 1,
                first_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                
                -- Pattern quality
                confidence_score FLOAT DEFAULT 0.5 CHECK (confidence_score >= 0 AND confidence_score <= 1),
                validation_count INTEGER DEFAULT 0,
                false_positive_count INTEGER DEFAULT 0,
                
                -- Solution tracking
                verified_solutions JSONB DEFAULT '[]'::jsonb,
                prevention_strategies JSONB DEFAULT '[]'::jsonb,
                avg_resolution_time INTERVAL,
                
                -- Metadata
                metadata JSONB DEFAULT '{}'::jsonb,
                
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        """)
        print("   ✅ crash_patterns table created")
        
        print("\n2. Creating crash_instances table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS crash_instances (
                id SERIAL PRIMARY KEY,
                pattern_id INTEGER REFERENCES crash_patterns(id),
                
                -- Instance details
                filename TEXT NOT NULL,
                file_hash VARCHAR(32),
                crash_timestamp TIMESTAMP,
                
                -- WCR metadata
                whatson_version VARCHAR(50),
                site_name VARCHAR(100),
                user_name VARCHAR(100),
                oracle_version VARCHAR(50),
                
                -- Environment
                environment JSONB DEFAULT '{}'::jsonb,
                
                -- Analysis results
                analysis JSONB NOT NULL,
                expert_report TEXT,
                
                -- Validation
                validated_by VARCHAR(100),
                validation_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                validation_notes TEXT,
                quality_rating INTEGER CHECK (quality_rating >= 1 AND quality_rating <= 5),
                
                -- Business impact
                downtime_minutes INTEGER,
                affected_features TEXT[],
                severity_rating VARCHAR(20),
                
                -- Knowledge integration
                knowledge_id INTEGER REFERENCES knowledge_units(id),
                
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        """)
        print("   ✅ crash_instances table created")
        
        print("\n3. Creating crash_relationships table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS crash_relationships (
                id SERIAL PRIMARY KEY,
                source_pattern_id INTEGER REFERENCES crash_patterns(id) ON DELETE CASCADE,
                target_pattern_id INTEGER REFERENCES crash_patterns(id) ON DELETE CASCADE,
                relationship_type VARCHAR(50) NOT NULL,
                confidence FLOAT DEFAULT 0.5 CHECK (confidence >= 0 AND confidence <= 1),
                evidence JSONB DEFAULT '{}'::jsonb,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                
                -- Prevent self-relationships and duplicates
                CONSTRAINT no_self_relationship CHECK (source_pattern_id != target_pattern_id),
                CONSTRAINT unique_relationship UNIQUE (source_pattern_id, target_pattern_id, relationship_type)
            );
        """)
        print("   ✅ crash_relationships table created")
        
        print("\n4. Creating pattern_evolution table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS pattern_evolution (
                id SERIAL PRIMARY KEY,
                pattern_id INTEGER REFERENCES crash_patterns(id) ON DELETE CASCADE,
                version INTEGER NOT NULL,
                changes JSONB NOT NULL,
                learned_from_instance_id INTEGER REFERENCES crash_instances(id),
                change_type VARCHAR(50),
                change_description TEXT,
                created_by VARCHAR(100),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        """)
        print("   ✅ pattern_evolution table created")
        
        print("\n5. Creating crash_analytics_cache table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS crash_analytics_cache (
                id SERIAL PRIMARY KEY,
                metric_name VARCHAR(100) UNIQUE,
                metric_data JSONB NOT NULL,
                calculated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                expires_at TIMESTAMP,
                
                -- Auto-expire old cache entries
                CHECK (expires_at > calculated_at)
            );
        """)
        print("   ✅ crash_analytics_cache table created")
        
        print("\n6. Creating indexes for performance...")
        
        # Indexes on crash_patterns
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_structural 
            ON crash_patterns(fingerprint_structural);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_error_type 
            ON crash_patterns(error_type);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_occurrence 
            ON crash_patterns(occurrence_count DESC);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_last_seen 
            ON crash_patterns(last_seen DESC);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_confidence 
            ON crash_patterns(confidence_score DESC);
        """)
        
        # Indexes on crash_instances
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_instances_pattern 
            ON crash_instances(pattern_id);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_instances_timestamp 
            ON crash_instances(crash_timestamp DESC);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_instances_version 
            ON crash_instances(whatson_version);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_instances_site 
            ON crash_instances(site_name);
        """)
        
        # Indexes on crash_relationships
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_relationships_source 
            ON crash_relationships(source_pattern_id);
        """)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_relationships_target 
            ON crash_relationships(target_pattern_id);
        """)
        
        # Full-text search indexes
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_crash_patterns_cause_search 
            ON crash_patterns USING gin(to_tsvector('english', cause_of_dump));
        """)
        
        print("   ✅ All indexes created")
        
        print("\n7. Creating helper functions...")
        
        # Function to update pattern statistics
        cur.execute("""
            CREATE OR REPLACE FUNCTION update_pattern_stats()
            RETURNS TRIGGER AS $$
            BEGIN
                IF TG_OP = 'INSERT' AND NEW.pattern_id IS NOT NULL THEN
                    UPDATE crash_patterns 
                    SET occurrence_count = occurrence_count + 1,
                        last_seen = CURRENT_TIMESTAMP,
                        updated_at = CURRENT_TIMESTAMP
                    WHERE id = NEW.pattern_id;
                END IF;
                RETURN NEW;
            END;
            $$ LANGUAGE plpgsql;
        """)
        
        # Trigger to auto-update pattern stats
        cur.execute("""
            CREATE TRIGGER update_pattern_stats_trigger
            AFTER INSERT ON crash_instances
            FOR EACH ROW
            EXECUTE FUNCTION update_pattern_stats();
        """)
        
        print("   ✅ Helper functions created")
        
        print("\n8. Creating views for analytics...")
        
        # View for pattern summary
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
        
        # View for recent crashes
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
        
        print("   ✅ Analytics views created")
        
        # Commit transaction
        conn.commit()
        print("\n✅ Migration completed successfully!")
        
        # Show summary
        cur.execute("SELECT COUNT(*) FROM crash_patterns")
        pattern_count = cur.fetchone()[0]
        
        cur.execute("SELECT COUNT(*) FROM crash_instances")
        instance_count = cur.fetchone()[0]
        
        print(f"\n📊 Database Status:")
        print(f"   - Crash patterns: {pattern_count}")
        print(f"   - Crash instances: {instance_count}")
        
    except Exception as e:
        conn.rollback()
        print(f"\n❌ Migration failed: {e}")
        raise
    finally:
        cur.close()
        conn.close()

def rollback_migration():
    """Rollback the migration if needed"""
    print("🔄 Rolling back Crash Knowledge System migration...")
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Drop in reverse order due to foreign keys
        tables = [
            'crash_analytics_cache',
            'pattern_evolution',
            'crash_relationships',
            'crash_instances',
            'crash_patterns'
        ]
        
        for table in tables:
            cur.execute(f"DROP TABLE IF EXISTS {table} CASCADE")
            print(f"   ✅ Dropped {table}")
        
        # Drop views
        cur.execute("DROP VIEW IF EXISTS crash_pattern_summary CASCADE")
        cur.execute("DROP VIEW IF EXISTS recent_crashes CASCADE")
        
        # Drop functions
        cur.execute("DROP FUNCTION IF EXISTS update_pattern_stats() CASCADE")
        
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
    
    parser = argparse.ArgumentParser(description="Crash Knowledge System Migration")
    parser.add_argument('--rollback', action='store_true', help='Rollback the migration')
    args = parser.parse_args()
    
    if args.rollback:
        response = input("⚠️  This will delete all crash knowledge data. Continue? (yes/no): ")
        if response.lower() == 'yes':
            rollback_migration()
        else:
            print("Rollback cancelled")
    else:
        run_migration()
        print("\n💡 Next steps:")
        print("   1. Update Crash Analyzer to use the learning system")
        print("   2. Test with sample crash files")
        print("   3. View crash patterns in the analytics dashboard")