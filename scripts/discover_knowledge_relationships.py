#!/usr/bin/env python3
"""
Discover relationships between existing knowledge units
Runs the relationship discovery engine on all captured knowledge
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.database import DatabaseManager
from utils.knowledge_relationships import discover_and_store_relationships

def main():
    """Discover and store relationships for existing knowledge"""
    print("🔗 Discovering Knowledge Relationships")
    print("=" * 50)
    
    try:
        # Initialize database
        print("1. Connecting to database...")
        db = DatabaseManager()
        
        # Check knowledge units count
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                cur.execute("SELECT COUNT(*) FROM knowledge_units WHERE quality_score >= 30")
                knowledge_count = cur.fetchone()[0]
                
        print(f"   Found {knowledge_count} quality knowledge units")
        
        if knowledge_count < 2:
            print("❌ Need at least 2 knowledge units to discover relationships")
            print("   Capture some knowledge first using the demo tools")
            return
        
        # Discover relationships
        print(f"\n2. Analyzing relationships between {knowledge_count} units...")
        relationships_stored = discover_and_store_relationships(db)
        
        if relationships_stored > 0:
            print(f"✅ Discovered and stored {relationships_stored} relationships!")
            
            # Show summary
            with db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT relationship_type, COUNT(*) as count
                        FROM knowledge_links
                        GROUP BY relationship_type
                        ORDER BY count DESC
                    """)
                    
                    print(f"\n📊 Relationship Summary:")
                    for rel_type, count in cur.fetchall():
                        print(f"   {rel_type}: {count} connections")
                        
            print(f"\n🎯 Related Knowledge widgets now available in all tools!")
            print(f"   Try the Knowledge Capture Demo to see relationships in action")
            
        else:
            print("💭 No new relationships discovered")
            print("   This might mean:")
            print("   - Relationships already exist")
            print("   - Knowledge units are too different to relate")
            print("   - Need more diverse knowledge content")
            
    except Exception as e:
        print(f"❌ Error discovering relationships: {e}")
        print("Make sure the database is running and migration is complete")
        sys.exit(1)

if __name__ == "__main__":
    main()