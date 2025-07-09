"""
Test database connection
"""

from utils import DatabaseManager

def test_connection():
    print("Testing database connection...")
    
    db = DatabaseManager()
    
    print(f"Connected: {db.connected}")
    
    if db.connected:
        print("✅ Database connection successful!")
        
        # Test a simple query
        try:
            with db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT version()")
                    version = cur.fetchone()
                    print(f"PostgreSQL version: {version[0]}")
        except Exception as e:
            print(f"Query error: {e}")
    else:
        print("❌ Could not connect to database")
        print("\nPlease check your .env file has these variables:")
        print("- TUOKIT_PG_DB or DB_NAME")
        print("- TUOKIT_PG_USER or DB_USER") 
        print("- TUOKIT_PG_PASSWORD or DB_PASSWORD")
        print("- TUOKIT_PG_HOST or DB_HOST")
        print("- TUOKIT_PG_PORT (default: 5432)")

if __name__ == "__main__":
    test_connection()
