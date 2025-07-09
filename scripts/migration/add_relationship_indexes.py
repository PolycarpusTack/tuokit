#!/usr/bin/env python3
"""
Add database indexes and constraints for knowledge relationships
Critical performance and data integrity fixes
"""

import os
import sys
import psycopg2
from datetime import datetime

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))

# Import DB config directly to avoid Streamlit dependencies
from dotenv import load_dotenv
load_dotenv()

DB_CONFIG = {
    "dbname": os.getenv("TUOKIT_PG_DB", "tuokit_knowledge"),
    "user": os.getenv("TUOKIT_PG_USER", "tuokit_user"),
    "password": os.getenv("TUOKIT_PG_PASSWORD", "Th1s1s4Work"),
    "host": os.getenv("TUOKIT_PG_HOST", "localhost"),
    "port": os.getenv("TUOKIT_PG_PORT", "5432")
}

def get_connection():
    """Get database connection"""
    return psycopg2.connect(**DB_CONFIG)

def add_indexes_and_constraints():
    """Add critical indexes and constraints to knowledge_links table"""
    print("🏗️  Adding Knowledge Relationships Indexes & Constraints")
    print("=" * 60)
    
    conn = get_connection()
    cur = conn.cursor()
    
    try:
        print("1. Adding performance indexes...")
        
        # Index for source_id lookups (most common query)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_knowledge_links_source_id 
            ON knowledge_links(source_id);
        """)
        print("   ✅ Source ID index")
        
        # Index for target_id lookups (bidirectional queries)
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_knowledge_links_target_id 
            ON knowledge_links(target_id);
        """)
        print("   ✅ Target ID index")
        
        # Compound index for relationship type filtering
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_knowledge_links_source_type 
            ON knowledge_links(source_id, relationship_type);
        """)
        print("   ✅ Source + Type compound index")
        
        # Index for strength-based ordering
        cur.execute("""
            CREATE INDEX IF NOT EXISTS idx_knowledge_links_strength 
            ON knowledge_links(strength DESC);
        """)
        print("   ✅ Strength ordering index")
        
        print("\n2. Adding data integrity constraints...")
        
        # Prevent duplicate relationships
        cur.execute("""
            CREATE UNIQUE INDEX IF NOT EXISTS idx_knowledge_links_unique
            ON knowledge_links(source_id, target_id, relationship_type);
        """)
        print("   ✅ Unique relationship constraint")
        
        # Ensure strength is within valid range
        cur.execute("""
            ALTER TABLE knowledge_links 
            ADD CONSTRAINT IF NOT EXISTS chk_strength_range 
            CHECK (strength >= 0.0 AND strength <= 1.0);
        """)
        print("   ✅ Strength range constraint")
        
        # Prevent self-referential relationships
        cur.execute("""
            ALTER TABLE knowledge_links 
            ADD CONSTRAINT IF NOT EXISTS chk_no_self_reference 
            CHECK (source_id != target_id);
        """)
        print("   ✅ No self-reference constraint")
        
        # Ensure valid relationship types
        cur.execute("""
            ALTER TABLE knowledge_links 
            ADD CONSTRAINT IF NOT EXISTS chk_valid_relationship_type 
            CHECK (relationship_type IN ('related', 'prerequisite', 'solution'));
        """)
        print("   ✅ Valid relationship type constraint")
        
        print("\n3. Adding foreign key constraints...")
        
        # Ensure source_id references valid knowledge unit
        cur.execute("""
            ALTER TABLE knowledge_links 
            ADD CONSTRAINT IF NOT EXISTS fk_knowledge_links_source
            FOREIGN KEY (source_id) REFERENCES knowledge_units(id) 
            ON DELETE CASCADE;
        """)
        print("   ✅ Source foreign key with cascade delete")
        
        # Ensure target_id references valid knowledge unit
        cur.execute("""
            ALTER TABLE knowledge_links 
            ADD CONSTRAINT IF NOT EXISTS fk_knowledge_links_target
            FOREIGN KEY (target_id) REFERENCES knowledge_units(id) 
            ON DELETE CASCADE;
        """)
        print("   ✅ Target foreign key with cascade delete")
        
        # Commit all changes
        conn.commit()
        
        print("\n4. Analyzing table for optimal query planning...")
        cur.execute("ANALYZE knowledge_links;")
        print("   ✅ Table statistics updated")
        
        print("\n5. Verifying index creation...")
        cur.execute("""
            SELECT indexname, indexdef 
            FROM pg_indexes 
            WHERE tablename = 'knowledge_links' 
            AND schemaname = 'public'
            ORDER BY indexname;
        """)
        
        indexes = cur.fetchall()
        print(f"   📊 Total indexes: {len(indexes)}")
        for index_name, index_def in indexes:
            print(f"      • {index_name}")
        
        print(f"\n✅ Database optimization complete!")
        print(f"   📈 Expected query performance improvement: 10-50x")
        print(f"   🔒 Data integrity constraints active")
        print(f"   🚀 Ready for relationship discovery")
        
    except psycopg2.Error as e:
        print(f"\n❌ Database error: {e}")
        conn.rollback()
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        conn.rollback()
        sys.exit(1)
    finally:
        cur.close()
        conn.close()

if __name__ == "__main__":
    add_indexes_and_constraints()