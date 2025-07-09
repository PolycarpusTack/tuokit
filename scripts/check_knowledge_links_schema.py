#!/usr/bin/env python3
"""
Check the current schema of knowledge_links table
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.database import DatabaseManager

def check_schema():
    """Check the current knowledge_links schema"""
    print("🔍 Checking knowledge_links table schema...")
    
    try:
        db = DatabaseManager()
        
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                # Get column information
                cur.execute("""
                    SELECT column_name, data_type, is_nullable, column_default
                    FROM information_schema.columns
                    WHERE table_name = 'knowledge_links'
                    ORDER BY ordinal_position;
                """)
                
                columns = cur.fetchall()
                
                print("\n📊 Current columns in knowledge_links:")
                print("-" * 60)
                for col_name, data_type, nullable, default in columns:
                    print(f"{col_name:20} {data_type:20} NULL:{nullable:5} DEFAULT:{default or 'None'}")
                
                # Check if updated_at exists
                column_names = [col[0] for col in columns]
                
                if 'updated_at' not in column_names:
                    print("\n❌ Missing 'updated_at' column!")
                    print("\n🔧 To fix, run:")
                    print("   ALTER TABLE knowledge_links ADD COLUMN updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;")
                else:
                    print("\n✅ 'updated_at' column exists!")
                
    except Exception as e:
        print(f"❌ Error checking schema: {e}")

if __name__ == "__main__":
    check_schema()