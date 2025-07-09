#!/usr/bin/env python3
"""
Database migration for Unified Knowledge Capture System
Creates new tables and updates existing ones with quality gates
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
    """Run the knowledge capture migration"""
    print("🚀 Starting Knowledge Capture System migration...")
    print("=" * 60)
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Start transaction
        conn.autocommit = False
        
        print("1. Creating knowledge_links table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_links (
                id SERIAL PRIMARY KEY,
                source_id INTEGER REFERENCES knowledge_units(id) ON DELETE CASCADE,
                target_id INTEGER REFERENCES knowledge_units(id) ON DELETE CASCADE,
                relationship_type VARCHAR(50) NOT NULL,
                strength FLOAT DEFAULT 1.0 CHECK (strength >= 0 AND strength <= 1),
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                
                -- Prevent self-links and duplicates
                CONSTRAINT no_self_link CHECK (source_id != target_id),
                CONSTRAINT unique_link UNIQUE (source_id, target_id, relationship_type)
            );
        """)
        print("   ✅ knowledge_links table created")
        
        print("\n2. Creating knowledge_maintenance_log table...")
        cur.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_maintenance_log (
                id SERIAL PRIMARY KEY,
                action VARCHAR(100) NOT NULL,
                details JSONB,
                units_affected INTEGER DEFAULT 0,
                execution_time_ms INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        """)
        print("   ✅ knowledge_maintenance_log table created")
        
        print("\n3. Enhancing knowledge_units table...")
        
        # Add new columns if they don't exist
        columns_to_add = [
            ("tags", "TEXT[]"),
            ("quality_score", "INTEGER DEFAULT 50"),
            ("usage_count", "INTEGER DEFAULT 0"),
            ("tool_specific_data", "JSONB"),
            ("updated_at", "TIMESTAMP DEFAULT CURRENT_TIMESTAMP"),
            ("last_accessed", "TIMESTAMP")
        ]
        
        for column_name, column_def in columns_to_add:
            cur.execute(f"""
                DO $$
                BEGIN
                    IF NOT EXISTS (
                        SELECT 1 FROM information_schema.columns 
                        WHERE table_name = 'knowledge_units' 
                        AND column_name = '{column_name}'
                    ) THEN
                        ALTER TABLE knowledge_units 
                        ADD COLUMN {column_name} {column_def};
                    END IF;
                END $$;
            """)
            print(f"   ✅ Added column {column_name}")
        
        # Add constraints
        print("\n4. Adding quality constraints...")
        cur.execute("""
            DO $$
            BEGIN
                -- Add content length constraint if not exists
                IF NOT EXISTS (
                    SELECT 1 FROM information_schema.table_constraints 
                    WHERE constraint_name = 'content_length'
                ) THEN
                    ALTER TABLE knowledge_units 
                    ADD CONSTRAINT content_length 
                    CHECK (char_length(content) >= 50);
                END IF;
                
                -- Add quality range constraint if not exists
                IF NOT EXISTS (
                    SELECT 1 FROM information_schema.table_constraints 
                    WHERE constraint_name = 'quality_range'
                ) THEN
                    ALTER TABLE knowledge_units 
                    ADD CONSTRAINT quality_range 
                    CHECK (quality_score >= 0 AND quality_score <= 100);
                END IF;
            END $$;
        """)
        print("   ✅ Quality constraints added")
        
        print("\n5. Creating indexes for performance...")
        indexes = [
            ("idx_knowledge_links_source", "knowledge_links", "source_id"),
            ("idx_knowledge_links_target", "knowledge_links", "target_id"),
            ("idx_knowledge_links_type", "knowledge_links", "relationship_type"),
            ("idx_knowledge_links_strength", "knowledge_links", "strength DESC"),
            ("idx_knowledge_links_source_type", "knowledge_links", "source_id, relationship_type"),
            ("idx_knowledge_quality", "knowledge_units", "quality_score DESC"),
            ("idx_knowledge_usage", "knowledge_units", "usage_count DESC"),
            ("idx_knowledge_updated", "knowledge_units", "updated_at DESC"),
            ("idx_maintenance_log_action", "knowledge_maintenance_log", "action"),
            ("idx_maintenance_log_created", "knowledge_maintenance_log", "created_at DESC")
        ]
        
        for index_name, table_name, columns in indexes:
            cur.execute(f"""
                CREATE INDEX IF NOT EXISTS {index_name} 
                ON {table_name} ({columns});
            """)
            print(f"   ✅ Created index {index_name}")
        
        # Add critical relationship constraints
        print("\n5b. Adding relationship integrity constraints...")
        cur.execute("""
            DO $$
            BEGIN
                -- Unique relationship constraint
                IF NOT EXISTS (
                    SELECT 1 FROM pg_constraint 
                    WHERE conname = 'unique_knowledge_relationship'
                ) THEN
                    CREATE UNIQUE INDEX idx_knowledge_links_unique
                    ON knowledge_links(source_id, target_id, relationship_type);
                END IF;
                
                -- Strength range constraint
                IF NOT EXISTS (
                    SELECT 1 FROM information_schema.table_constraints 
                    WHERE constraint_name = 'chk_strength_range'
                ) THEN
                    ALTER TABLE knowledge_links 
                    ADD CONSTRAINT chk_strength_range 
                    CHECK (strength >= 0.0 AND strength <= 1.0);
                END IF;
                
                -- No self-reference constraint
                IF NOT EXISTS (
                    SELECT 1 FROM information_schema.table_constraints 
                    WHERE constraint_name = 'chk_no_self_reference'
                ) THEN
                    ALTER TABLE knowledge_links 
                    ADD CONSTRAINT chk_no_self_reference 
                    CHECK (source_id != target_id);
                END IF;
                
                -- Valid relationship type constraint
                IF NOT EXISTS (
                    SELECT 1 FROM information_schema.table_constraints 
                    WHERE constraint_name = 'chk_valid_relationship_type'
                ) THEN
                    ALTER TABLE knowledge_links 
                    ADD CONSTRAINT chk_valid_relationship_type 
                    CHECK (relationship_type IN ('related', 'prerequisite', 'solution'));
                END IF;
            END $$;
        """)
        print("   ✅ Relationship constraints added")
        
        # Create full-text search index
        print("\n6. Creating full-text search capabilities...")
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_knowledge_search 
            ON knowledge_units 
            USING gin(to_tsvector('english', content || ' ' || COALESCE(title, '')));
        """)
        print("   ✅ Full-text search index created")
        
        # Create initial maintenance log entry
        print("\n7. Recording migration in maintenance log...")
        cur.execute("""
            INSERT INTO knowledge_maintenance_log (action, details, execution_time_ms)
            VALUES ('migration_knowledge_capture', %s, 0);
        """, (psycopg2.extras.Json({
            "version": "1.0",
            "description": "Initial knowledge capture system setup",
            "tables_created": ["knowledge_links", "knowledge_maintenance_log"],
            "columns_added": [col[0] for col in columns_to_add]
        }),))
        
        # Commit transaction
        conn.commit()
        print("\n✅ Migration completed successfully!")
        
        # Print summary
        cur.execute("SELECT COUNT(*) FROM knowledge_units")
        unit_count = cur.fetchone()[0]
        
        print(f"\n📊 Summary:")
        print(f"   - Existing knowledge units: {unit_count}")
        print(f"   - New tables created: 2")
        print(f"   - Indexes created: {len(indexes) + 1}")
        print(f"   - Ready for Knowledge Capture System!")
        
    except Exception as e:
        conn.rollback()
        print(f"\n❌ Migration failed: {e}")
        raise
    finally:
        cur.close()
        conn.close()

def rollback_migration():
    """Rollback the migration if needed"""
    print("🔄 Rolling back Knowledge Capture System migration...")
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        # Drop tables in reverse order
        cur.execute("DROP TABLE IF EXISTS knowledge_links CASCADE;")
        cur.execute("DROP TABLE IF EXISTS knowledge_maintenance_log CASCADE;")
        
        # Remove added columns
        columns_to_remove = ["tags", "quality_score", "usage_count", 
                           "tool_specific_data", "updated_at", "last_accessed"]
        
        for column in columns_to_remove:
            cur.execute(f"""
                ALTER TABLE knowledge_units 
                DROP COLUMN IF EXISTS {column};
            """)
        
        conn.commit()
        print("✅ Rollback completed")
        
    except Exception as e:
        conn.rollback()
        print(f"❌ Rollback failed: {e}")
        raise
    finally:
        cur.close()
        conn.close()

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description="Knowledge Capture System Migration")
    parser.add_argument("--rollback", action="store_true", 
                       help="Rollback the migration")
    
    args = parser.parse_args()
    
    try:
        if args.rollback:
            rollback_migration()
        else:
            run_migration()
    except Exception as e:
        print(f"\n❌ Error: {e}")
        sys.exit(1)