"""
Analytics Database Management Script
Helps manage analytics tables in PostgreSQL
"""

from utils import DatabaseManager
from datetime import datetime, timedelta
import argparse

def create_analytics_tables(db: DatabaseManager):
    """Create all analytics tables if they don't exist"""
    tables = [
        """
        CREATE TABLE IF NOT EXISTS data_profiles (
            id SERIAL PRIMARY KEY,
            profile_name VARCHAR(255),
            source_file VARCHAR(255),
            row_count INTEGER,
            column_count INTEGER,
            profile_data JSONB,
            quality_score FLOAT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        """
        CREATE TABLE IF NOT EXISTS analytics_queries (
            id SERIAL PRIMARY KEY,
            natural_language_query TEXT,
            generated_sql TEXT,
            result_summary JSONB,
            row_count INTEGER,
            execution_time_ms INTEGER,
            success BOOLEAN,
            error_message TEXT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        """
        CREATE TABLE IF NOT EXISTS data_insights (
            id SERIAL PRIMARY KEY,
            dataset_name VARCHAR(255),
            insight_type VARCHAR(50),
            insight_description TEXT,
            insight_data JSONB,
            confidence_score FLOAT,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """,
        """
        CREATE TABLE IF NOT EXISTS data_patterns (
            id SERIAL PRIMARY KEY,
            dataset_name VARCHAR(255),
            pattern_type VARCHAR(50),
            pattern_description TEXT,
            affected_columns TEXT[],
            pattern_details JSONB,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
        """
    ]
    
    for table_sql in tables:
        try:
            db.connection.execute(table_sql)
            print(f"✅ Table created/verified")
        except Exception as e:
            print(f"❌ Error creating table: {e}")
    
    db.connection.commit()
    print("\n✅ All analytics tables ready!")

def show_analytics_stats(db: DatabaseManager):
    """Show statistics about saved analytics"""
    stats = {}
    
    tables = {
        'data_profiles': 'Data Profiles',
        'analytics_queries': 'Analytics Queries',
        'data_insights': 'Data Insights',
        'data_patterns': 'Data Patterns'
    }
    
    print("\n📊 Analytics Database Statistics")
    print("=" * 50)
    
    for table, name in tables.items():
        try:
            result = db.connection.execute(f"SELECT COUNT(*) FROM {table}").fetchone()
            count = result[0] if result else 0
            stats[table] = count
            print(f"{name}: {count} records")
        except:
            print(f"{name}: Table not found")
    
    # Additional stats
    try:
        # Success rate for queries
        result = db.connection.execute("""
            SELECT 
                COUNT(*) as total,
                SUM(CASE WHEN success THEN 1 ELSE 0 END) as successful
            FROM analytics_queries
        """).fetchone()
        
        if result and result[0] > 0:
            success_rate = (result[1] / result[0]) * 100
            print(f"\nQuery Success Rate: {success_rate:.1f}%")
        
        # Average quality score
        result = db.connection.execute("""
            SELECT AVG(quality_score) 
            FROM data_profiles
        """).fetchone()
        
        if result and result[0]:
            print(f"Average Data Quality Score: {result[0]:.1f}%")
        
        # Most common insight types
        result = db.connection.execute("""
            SELECT insight_type, COUNT(*) as count
            FROM data_insights
            GROUP BY insight_type
            ORDER BY count DESC
            LIMIT 5
        """).fetchall()
        
        if result:
            print("\nTop Insight Types:")
            for insight_type, count in result:
                print(f"  - {insight_type}: {count}")
                
    except Exception as e:
        print(f"\nError getting detailed stats: {e}")

def cleanup_old_records(db: DatabaseManager, days: int = 30):
    """Clean up records older than specified days"""
    cutoff_date = datetime.now() - timedelta(days=days)
    
    print(f"\n🧹 Cleaning records older than {days} days...")
    
    tables = ['data_profiles', 'analytics_queries', 'data_insights', 'data_patterns']
    
    for table in tables:
        try:
            result = db.connection.execute(f"""
                DELETE FROM {table}
                WHERE created_at < %s
                RETURNING id
            """, (cutoff_date,))
            
            deleted = len(result.fetchall())
            if deleted > 0:
                print(f"  Deleted {deleted} records from {table}")
                
        except Exception as e:
            print(f"  Error cleaning {table}: {e}")
    
    db.connection.commit()
    print("✅ Cleanup complete!")

def export_insights(db: DatabaseManager, output_file: str = "insights_export.json"):
    """Export all insights to JSON file"""
    import json
    
    print(f"\n📤 Exporting insights to {output_file}...")
    
    try:
        # Get all insights
        result = db.connection.execute("""
            SELECT dataset_name, insight_type, insight_description, 
                   insight_data, confidence_score, created_at
            FROM data_insights
            ORDER BY confidence_score DESC
        """).fetchall()
        
        insights = []
        for row in result:
            insights.append({
                'dataset_name': row[0],
                'type': row[1],
                'description': row[2],
                'data': row[3],
                'confidence': row[4],
                'created_at': row[5].isoformat() if row[5] else None
            })
        
        with open(output_file, 'w') as f:
            json.dump(insights, f, indent=2)
        
        print(f"✅ Exported {len(insights)} insights to {output_file}")
        
    except Exception as e:
        print(f"❌ Error exporting insights: {e}")

def main():
    parser = argparse.ArgumentParser(description='Manage TuoKit Analytics Database')
    parser.add_argument('action', choices=['create', 'stats', 'cleanup', 'export'],
                      help='Action to perform')
    parser.add_argument('--days', type=int, default=30,
                      help='Days to keep for cleanup (default: 30)')
    parser.add_argument('--output', default='insights_export.json',
                      help='Output file for export (default: insights_export.json)')
    
    args = parser.parse_args()
    
    # Connect to database
    db = DatabaseManager()
    if not db or not db.connected:
        print("❌ Could not connect to database")
        return
    
    try:
        if args.action == 'create':
            create_analytics_tables(db)
        elif args.action == 'stats':
            show_analytics_stats(db)
        elif args.action == 'cleanup':
            cleanup_old_records(db, args.days)
        elif args.action == 'export':
            export_insights(db, args.output)
    finally:
        db.close()

if __name__ == "__main__":
    # If run without arguments, show stats
    import sys
    if len(sys.argv) == 1:
        db = DatabaseManager()
        if db and db.connected:
            show_analytics_stats(db)
            db.close()
    else:
        main()
