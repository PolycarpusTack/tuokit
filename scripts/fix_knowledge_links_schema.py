#!/usr/bin/env python3
"""
Quick fix for knowledge_links table schema
Adds missing updated_at column
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.database import DatabaseManager

def fix_schema():
    """Add missing updated_at column to knowledge_links table"""
    print("🔧 Fixing knowledge_links schema...")
    
    try:
        db = DatabaseManager()
        
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                # Add updated_at column if missing
                cur.execute("""
                    ALTER TABLE knowledge_links 
                    ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
                """)
                
                # Update existing rows
                cur.execute("""
                    UPDATE knowledge_links 
                    SET updated_at = CURRENT_TIMESTAMP 
                    WHERE updated_at IS NULL;
                """)
                
                conn.commit()
                print("✅ Added updated_at column to knowledge_links")
                
                # Verify the fix
                cur.execute("""
                    SELECT column_name 
                    FROM information_schema.columns 
                    WHERE table_name = 'knowledge_links';
                """)
                
                columns = [row[0] for row in cur.fetchall()]
                print(f"✅ Table columns: {', '.join(columns)}")
                
    except Exception as e:
        print(f"❌ Error fixing schema: {e}")
        return False
    
    return True

if __name__ == "__main__":
    if fix_schema():
        print("\n🚀 Schema fixed! Now run:")
        print("python3 scripts/discover_knowledge_relationships.py")
    else:
        print("\n❌ Schema fix failed!")