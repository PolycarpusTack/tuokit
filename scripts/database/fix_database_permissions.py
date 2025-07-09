#!/usr/bin/env python3
"""
Fix database permissions for TuoKit user
Grants necessary permissions to complete the migration
"""

import os
import sys
import psycopg2
from dotenv import load_dotenv

def fix_permissions():
    """Fix database permissions using admin credentials"""
    print("🔧 Fixing database permissions...")
    print("=" * 50)
    
    load_dotenv()
    
    # Use admin credentials to fix permissions
    admin_config = {
        "dbname": os.getenv("TUOKIT_PG_DB", "tuokit_knowledge"),
        "user": os.getenv("PG_ADMIN_USER", "postgres"),
        "password": os.getenv("PG_ADMIN_PASSWORD", "Th1s1s4Work"),
        "host": os.getenv("TUOKIT_PG_HOST", "localhost"),
        "port": os.getenv("TUOKIT_PG_PORT", "5432")
    }
    
    tuokit_user = os.getenv("TUOKIT_PG_USER", "tuokit_user")
    
    try:
        print(f"Connecting as admin user: {admin_config['user']}")
        conn = psycopg2.connect(**admin_config)
        conn.autocommit = True
        cur = conn.cursor()
        
        # Grant necessary permissions
        print(f"\n1. Granting permissions to {tuokit_user}...")
        
        # Grant all privileges on all existing tables
        cur.execute(f"""
            GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO {tuokit_user};
        """)
        print(f"   ✅ Granted table privileges")
        
        # Grant all privileges on all sequences
        cur.execute(f"""
            GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO {tuokit_user};
        """)
        print(f"   ✅ Granted sequence privileges")
        
        # Grant usage on schema
        cur.execute(f"""
            GRANT USAGE ON SCHEMA public TO {tuokit_user};
        """)
        print(f"   ✅ Granted schema usage")
        
        # Grant create privileges
        cur.execute(f"""
            GRANT CREATE ON SCHEMA public TO {tuokit_user};
        """)
        print(f"   ✅ Granted create privileges")
        
        # Set default privileges for future objects
        cur.execute(f"""
            ALTER DEFAULT PRIVILEGES IN SCHEMA public 
            GRANT ALL ON TABLES TO {tuokit_user};
        """)
        print(f"   ✅ Set default table privileges")
        
        cur.execute(f"""
            ALTER DEFAULT PRIVILEGES IN SCHEMA public 
            GRANT ALL ON SEQUENCES TO {tuokit_user};
        """)
        print(f"   ✅ Set default sequence privileges")
        
        # Check current ownership and change if needed
        print(f"\n2. Checking table ownership...")
        cur.execute("""
            SELECT tablename, tableowner 
            FROM pg_tables 
            WHERE schemaname = 'public'
            ORDER BY tablename;
        """)
        
        tables = cur.fetchall()
        tables_to_change = []
        
        for table, owner in tables:
            if owner != tuokit_user:
                tables_to_change.append(table)
                print(f"   Table {table} owned by {owner}, needs change")
        
        if tables_to_change:
            print(f"\n3. Changing ownership of {len(tables_to_change)} tables...")
            for table in tables_to_change:
                cur.execute(f"ALTER TABLE {table} OWNER TO {tuokit_user};")
                print(f"   ✅ Changed {table} ownership to {tuokit_user}")
        else:
            print(f"\n3. All tables already owned by {tuokit_user}")
        
        cur.close()
        conn.close()
        
        print(f"\n✅ Database permissions fixed successfully!")
        print(f"   User {tuokit_user} now has full access to all tables")
        print(f"\n🚀 You can now re-run the migration:")
        print(f"   python scripts/migration/create_knowledge_capture_tables.py")
        
    except Exception as e:
        print(f"\n❌ Error fixing permissions: {e}")
        print(f"\nAlternative: Connect to PostgreSQL as admin and run:")
        print(f"   GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO {tuokit_user};")
        print(f"   ALTER TABLE knowledge_units OWNER TO {tuokit_user};")
        sys.exit(1)

if __name__ == "__main__":
    fix_permissions()