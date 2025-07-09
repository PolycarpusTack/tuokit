"""
Knowledge Management Script for TuoKit
Manages enhanced knowledge capture tables and provides utilities
Fixed version that works with DatabaseManager's connection pattern
"""

from utils import DatabaseManager
from datetime import datetime, timedelta
import json
import pandas as pd

def create_knowledge_tables(db: DatabaseManager):
    """Create all knowledge management tables"""
    
    tables = [
        # Main knowledge entries table
        """
        CREATE TABLE IF NOT EXISTS agent_knowledge_entries (
            id SERIAL PRIMARY KEY,
            agent_name VARCHAR(100),
            tool_name VARCHAR(100),
            operation_type VARCHAR(50),
            category VARCHAR(50),
            subcategory VARCHAR(50),
            title TEXT,
            content TEXT,
            input_data JSONB,
            output_data JSONB,
            metadata JSONB,
            tags TEXT[],
            performance_metrics JSONB,
            success BOOLEAN DEFAULT TRUE,
            error_message TEXT,
            confidence_score FLOAT DEFAULT 1.0,
            reusability_score FLOAT DEFAULT 1.0,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        
        # Create indexes for better search
        "CREATE INDEX IF NOT EXISTS idx_agent_tool ON agent_knowledge_entries(agent_name, tool_name)",
        "CREATE INDEX IF NOT EXISTS idx_category ON agent_knowledge_entries(category, subcategory)",
        "CREATE INDEX IF NOT EXISTS idx_created ON agent_knowledge_entries(created_at DESC)",
        "CREATE INDEX IF NOT EXISTS idx_tags ON agent_knowledge_entries USING GIN(tags)",
        
        # Knowledge relationships
        """
        CREATE TABLE IF NOT EXISTS knowledge_relationships (
            id SERIAL PRIMARY KEY,
            source_id INTEGER REFERENCES agent_knowledge_entries(id),
            target_id INTEGER REFERENCES agent_knowledge_entries(id),
            relationship_type VARCHAR(50),
            strength FLOAT DEFAULT 1.0,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        
        # Knowledge usage tracking
        """
        CREATE TABLE IF NOT EXISTS knowledge_usage (
            id SERIAL PRIMARY KEY,
            knowledge_id INTEGER REFERENCES agent_knowledge_entries(id),
            used_by_agent VARCHAR(100),
            used_in_context TEXT,
            usefulness_rating INTEGER,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        
        # Knowledge search cache
        """
        CREATE TABLE IF NOT EXISTS knowledge_search_cache (
            id SERIAL PRIMARY KEY,
            search_query TEXT,
            search_filters JSONB,
            result_ids INTEGER[],
            result_count INTEGER,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """
    ]
    
    # Execute each SQL statement using the connection context manager
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            for sql in tables:
                try:
                    cur.execute(sql)
                    if "CREATE TABLE" in sql:
                        table_name = sql.split("EXISTS")[1].split("(")[0].strip()
                        print(f"✅ Table {table_name} created/verified")
                    else:
                        print(f"✅ Index created")
                except Exception as e:
                    print(f"Note: {e}")

def show_knowledge_stats(db: DatabaseManager):
    """Show statistics about captured knowledge"""
    
    print("\n📊 Knowledge Base Statistics")
    print("=" * 60)
    
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            # Total entries
            cur.execute("SELECT COUNT(*) FROM agent_knowledge_entries")
            result = cur.fetchone()
            total = result[0] if result else 0
            print(f"Total Knowledge Entries: {total}")
            
            if total == 0:
                print("\nNo knowledge captured yet. Start using agents to build knowledge!")
                return
            
            # By agent
            print("\n📤 Knowledge by Agent:")
            cur.execute("""
                SELECT agent_name, COUNT(*) as count
                FROM agent_knowledge_entries
                GROUP BY agent_name
                ORDER BY count DESC
            """)
            
            for agent, count in cur.fetchall():
                print(f"  {agent}: {count} entries")
            
            # By category
            print("\n📁 Knowledge by Category:")
            cur.execute("""
                SELECT category, COUNT(*) as count
                FROM agent_knowledge_entries
                GROUP BY category
                ORDER BY count DESC
            """)
            
            for category, count in cur.fetchall():
                print(f"  {category}: {count} entries")
            
            # Success rate
            cur.execute("""
                SELECT 
                    COUNT(*) as total,
                    SUM(CASE WHEN success THEN 1 ELSE 0 END) as successful
                FROM agent_knowledge_entries
            """)
            result = cur.fetchone()
            
            if result and result[0] > 0:
                success_rate = (result[1] / result[0]) * 100
                print(f"\n✅ Success Rate: {success_rate:.1f}%")
            
            # High confidence entries
            cur.execute("""
                SELECT COUNT(*) 
                FROM agent_knowledge_entries
                WHERE confidence_score >= 0.8
            """)
            result = cur.fetchone()
            
            high_conf = result[0] if result else 0
            print(f"🎯 High Confidence Entries: {high_conf}")
            
            # Recent activity
            print("\n📅 Recent Activity:")
            cur.execute("""
                SELECT DATE(created_at) as date, COUNT(*) as count
                FROM agent_knowledge_entries
                WHERE created_at > CURRENT_DATE - INTERVAL '7 days'
                GROUP BY DATE(created_at)
                ORDER BY date DESC
            """)
            
            for date, count in cur.fetchall():
                print(f"  {date}: {count} entries")
            
            # Most reusable knowledge
            print("\n♻️ Most Reusable Knowledge:")
            cur.execute("""
                SELECT title, reusability_score, category
                FROM agent_knowledge_entries
                WHERE reusability_score > 0.7
                ORDER BY reusability_score DESC
                LIMIT 5
            """)
            
            for title, score, category in cur.fetchall():
                print(f"  [{category}] {title[:60]}... (Score: {score:.2f})")

def export_knowledge(db: DatabaseManager, output_file: str = "knowledge_export.json"):
    """Export knowledge base to JSON"""
    
    print(f"\n📤 Exporting knowledge to {output_file}...")
    
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            cur.execute("""
                SELECT 
                    agent_name, tool_name, operation_type, category, subcategory,
                    title, content, input_data, output_data, metadata, tags,
                    performance_metrics, success, confidence_score, reusability_score,
                    created_at
                FROM agent_knowledge_entries
                ORDER BY created_at DESC
            """)
            
            entries = []
            for row in cur.fetchall():
                entry = {
                    'agent_name': row[0],
                    'tool_name': row[1],
                    'operation_type': row[2],
                    'category': row[3],
                    'subcategory': row[4],
                    'title': row[5],
                    'content': row[6],
                    'input_data': row[7],
                    'output_data': row[8],
                    'metadata': row[9],
                    'tags': row[10],
                    'performance_metrics': row[11],
                    'success': row[12],
                    'confidence_score': row[13],
                    'reusability_score': row[14],
                    'created_at': row[15].isoformat() if row[15] else None
                }
                entries.append(entry)
    
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(entries, f, indent=2, ensure_ascii=False)
    
    print(f"✅ Exported {len(entries)} knowledge entries")

def search_knowledge(db: DatabaseManager, query: str, category: str = None):
    """Search knowledge base"""
    
    print(f"\n🔍 Searching for: '{query}'")
    
    sql = """
        SELECT id, title, category, subcategory, confidence_score, created_at
        FROM agent_knowledge_entries
        WHERE (
            title ILIKE %s OR
            content ILIKE %s OR
            %s = ANY(tags)
        )
    """
    
    params = [f"%{query}%", f"%{query}%", query]
    
    if category:
        sql += " AND category = %s"
        params.append(category)
    
    sql += " ORDER BY confidence_score DESC, created_at DESC LIMIT 10"
    
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            cur.execute(sql, params)
            result = cur.fetchall()
    
    if result:
        print(f"\nFound {len(result)} results:")
        for id, title, cat, subcat, conf, created in result:
            print(f"\n[{id}] {title[:80]}...")
            print(f"    Category: {cat}/{subcat}")
            print(f"    Confidence: {conf:.2f}")
            print(f"    Created: {created}")
    else:
        print("No results found")

def cleanup_old_knowledge(db: DatabaseManager, days: int = 90):
    """Clean up old knowledge entries"""
    
    cutoff = datetime.now() - timedelta(days=days)
    
    print(f"\n🧹 Cleaning knowledge older than {days} days...")
    
    # Don't delete high-value knowledge
    with db.get_connection() as conn:
        with conn.cursor() as cur:
            cur.execute("""
                DELETE FROM agent_knowledge_entries
                WHERE created_at < %s
                AND confidence_score < 0.7
                AND reusability_score < 0.7
                RETURNING id
            """, (cutoff,))
            
            deleted_ids = cur.fetchall()
            deleted = len(deleted_ids)
    
    print(f"✅ Deleted {deleted} low-value old entries")

def main():
    """Main function"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Manage TuoKit Knowledge Base')
    parser.add_argument('action', 
                       choices=['create', 'stats', 'export', 'search', 'cleanup'],
                       help='Action to perform')
    parser.add_argument('--query', help='Search query')
    parser.add_argument('--category', help='Filter by category')
    parser.add_argument('--days', type=int, default=90, help='Days for cleanup')
    parser.add_argument('--output', default='knowledge_export.json', 
                       help='Output file for export')
    
    args = parser.parse_args()
    
    # Connect to database
    db = DatabaseManager()
    if not db or not db.connected:
        print("❌ Could not connect to database")
        return
    
    try:
        if args.action == 'create':
            create_knowledge_tables(db)
        elif args.action == 'stats':
            show_knowledge_stats(db)
        elif args.action == 'export':
            export_knowledge(db, args.output)
        elif args.action == 'search':
            if args.query:
                search_knowledge(db, args.query, args.category)
            else:
                print("Please provide --query for search")
        elif args.action == 'cleanup':
            cleanup_old_knowledge(db, args.days)
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    import sys
    if len(sys.argv) == 1:
        # Default action
        db = DatabaseManager()
        if db and db.connected:
            show_knowledge_stats(db)
    else:
        main()
