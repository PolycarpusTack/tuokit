"""
Create Knowledge Tables with Better Error Handling
"""

from utils import DatabaseManager
import sys

def create_knowledge_tables():
    """Create knowledge tables with detailed error messages"""
    
    print("Creating TuoKit Knowledge Tables...")
    print("=" * 60)
    
    # Try to connect
    db = DatabaseManager()
    
    if not db or not db.connected:
        print("\n❌ Database connection failed!")
        print("\nTroubleshooting steps:")
        print("1. Make sure PostgreSQL is running")
        print("2. Check your .env file has correct credentials")
        print("3. Verify the database 'tuokit_knowledge' exists")
        print("4. Ensure user has proper permissions")
        
        print("\nTo create the database and user, run these commands in psql:")
        print("  CREATE DATABASE tuokit_knowledge;")
        print("  CREATE USER tuokit_user WITH PASSWORD 'your_password';")
        print("  GRANT ALL PRIVILEGES ON DATABASE tuokit_knowledge TO tuokit_user;")
        return False
    
    print("✅ Database connected successfully!")
    
    # Create tables one by one with error handling
    tables = [
        ("agent_knowledge_entries", """
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
        """),
        
        ("knowledge_relationships", """
            CREATE TABLE IF NOT EXISTS knowledge_relationships (
                id SERIAL PRIMARY KEY,
                source_id INTEGER REFERENCES agent_knowledge_entries(id),
                target_id INTEGER REFERENCES agent_knowledge_entries(id),
                relationship_type VARCHAR(50),
                strength FLOAT DEFAULT 1.0,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """),
        
        ("knowledge_usage", """
            CREATE TABLE IF NOT EXISTS knowledge_usage (
                id SERIAL PRIMARY KEY,
                knowledge_id INTEGER REFERENCES agent_knowledge_entries(id),
                used_by_agent VARCHAR(100),
                used_in_context TEXT,
                usefulness_rating INTEGER,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
    ]
    
    # Create each table
    created_count = 0
    
    try:
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                for table_name, create_sql in tables:
                    try:
                        cur.execute(create_sql)
                        print(f"✅ Table '{table_name}' created/verified")
                        created_count += 1
                    except Exception as e:
                        print(f"❌ Error creating table '{table_name}': {e}")
                
                # Create indexes
                indexes = [
                    "CREATE INDEX IF NOT EXISTS idx_agent_tool ON agent_knowledge_entries(agent_name, tool_name)",
                    "CREATE INDEX IF NOT EXISTS idx_category ON agent_knowledge_entries(category, subcategory)",
                    "CREATE INDEX IF NOT EXISTS idx_created ON agent_knowledge_entries(created_at DESC)"
                ]
                
                for index_sql in indexes:
                    try:
                        cur.execute(index_sql)
                        print(f"✅ Index created")
                    except Exception as e:
                        print(f"Note: Index may already exist: {e}")
                
    except Exception as e:
        print(f"\n❌ Database error: {e}")
        print("\nThis might be a permission issue. Try connecting as the postgres user.")
        return False
    
    print("\n" + "=" * 60)
    print(f"✅ Knowledge capture tables ready! ({created_count} tables created)")
    print("=" * 60)
    
    return True

if __name__ == "__main__":
    success = create_knowledge_tables()
    sys.exit(0 if success else 1)
