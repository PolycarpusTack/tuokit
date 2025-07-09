"""
TuoKit Database Health Check Dashboard
Comprehensive monitoring for PostgreSQL database health and setup validation
"""

import streamlit as st
import psycopg2
from psycopg2.extras import RealDictCursor
import json
import os
import sys
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dotenv import load_dotenv

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Configuration at top of file (easy to modify)
REQUIRED_TABLES = [
    'queries',
    'knowledge_units', 
    'knowledge_links',
    'knowledge_maintenance_log',
    'pipeline_templates',
    'pipelines',
    'code_snippets'
]

EXPECTED_COLUMNS = {
    'queries': ['id', 'tool', 'model', 'user_prompt', 'ai_response', 'metadata', 'created_at'],
    'knowledge_units': ['id', 'query_id', 'title', 'content', 'category', 'tags', 'quality_score', 'usage_count', 'created_at'],
    'knowledge_links': ['id', 'source_id', 'target_id', 'relationship_type', 'strength', 'created_at'],
    'knowledge_maintenance_log': ['id', 'action', 'details', 'units_affected', 'execution_time_ms', 'created_at']
}

def get_db_config():
    """Get database configuration from environment"""
    load_dotenv()
    
    return {
        "dbname": os.getenv("TUOKIT_PG_DB", os.getenv("DB_NAME", "tuokit_knowledge")),
        "user": os.getenv("TUOKIT_PG_USER", os.getenv("DB_USER", "tuokit_user")),
        "password": os.getenv("TUOKIT_PG_PASSWORD", os.getenv("DB_PASSWORD", "your_secure_password")),
        "host": os.getenv("TUOKIT_PG_HOST", os.getenv("DB_HOST", "localhost")),
        "port": os.getenv("TUOKIT_PG_PORT", "5432")
    }

def test_database_connection() -> Tuple[bool, str, Dict]:
    """Test database connection and return status"""
    config = get_db_config()
    
    try:
        conn = psycopg2.connect(**config)
        
        # Test basic query
        with conn.cursor() as cur:
            cur.execute("SELECT version()")
            version = cur.fetchone()[0]
            
            # Get database stats
            cur.execute("""
                SELECT 
                    pg_database_size(current_database()) as db_size,
                    current_database() as db_name,
                    current_user as current_user,
                    inet_server_addr() as server_ip,
                    inet_server_port() as server_port
            """)
            stats = cur.fetchone()
            
        conn.close()
        
        return True, "Connected successfully", {
            "version": version,
            "db_size": stats[0],
            "db_name": stats[1],
            "current_user": stats[2],
            "server_ip": stats[3],
            "server_port": stats[4],
            "config": config
        }
        
    except psycopg2.OperationalError as e:
        return False, f"Connection failed: {str(e)}", {"config": config}
    except Exception as e:
        return False, f"Unexpected error: {str(e)}", {"config": config}

def get_table_status() -> Dict[str, Any]:
    """Check which required tables exist and their status"""
    config = get_db_config()
    table_status = {}
    
    try:
        conn = psycopg2.connect(**config)
        
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            # Check which tables exist
            cur.execute("""
                SELECT table_name, table_type
                FROM information_schema.tables 
                WHERE table_schema = 'public'
                ORDER BY table_name
            """)
            existing_tables = {row['table_name']: row['table_type'] for row in cur.fetchall()}
            
            # Check each required table
            for table in REQUIRED_TABLES:
                if table in existing_tables:
                    # Get table details
                    cur.execute(f"""
                        SELECT COUNT(*) as row_count
                        FROM {table}
                    """)
                    row_count = cur.fetchone()['row_count']
                    
                    # Get table size
                    cur.execute(f"""
                        SELECT pg_size_pretty(pg_total_relation_size('{table}')) as size
                    """)
                    size = cur.fetchone()['size']
                    
                    # Get column info
                    cur.execute("""
                        SELECT column_name, data_type, is_nullable
                        FROM information_schema.columns 
                        WHERE table_name = %s
                        ORDER BY ordinal_position
                    """, (table,))
                    columns = cur.fetchall()
                    
                    # Check for recent activity
                    recent_activity = None
                    if table in ['queries', 'knowledge_units', 'knowledge_maintenance_log']:
                        try:
                            cur.execute(f"""
                                SELECT MAX(created_at) as last_activity
                                FROM {table}
                            """)
                            result = cur.fetchone()
                            recent_activity = result['last_activity'] if result else None
                        except:
                            pass
                    
                    table_status[table] = {
                        "exists": True,
                        "row_count": row_count,
                        "size": size,
                        "columns": [dict(col) for col in columns],
                        "last_activity": recent_activity,
                        "status": "healthy"
                    }
                else:
                    table_status[table] = {
                        "exists": False,
                        "status": "missing"
                    }
        
        conn.close()
        
    except Exception as e:
        st.error(f"Error checking table status: {e}")
        
    return table_status

def check_indexes_and_constraints() -> Dict[str, Any]:
    """Check database indexes and constraints"""
    config = get_db_config()
    
    try:
        conn = psycopg2.connect(**config)
        
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            # Check indexes
            cur.execute("""
                SELECT 
                    schemaname,
                    tablename,
                    indexname,
                    indexdef
                FROM pg_indexes 
                WHERE schemaname = 'public'
                ORDER BY tablename, indexname
            """)
            indexes = cur.fetchall()
            
            # Check constraints
            cur.execute("""
                SELECT 
                    table_name,
                    constraint_name,
                    constraint_type
                FROM information_schema.table_constraints 
                WHERE table_schema = 'public'
                ORDER BY table_name, constraint_name
            """)
            constraints = cur.fetchall()
            
        conn.close()
        
        return {
            "indexes": [dict(idx) for idx in indexes],
            "constraints": [dict(const) for const in constraints]
        }
        
    except Exception as e:
        st.error(f"Error checking indexes/constraints: {e}")
        return {"indexes": [], "constraints": []}

def get_recent_activity() -> Dict[str, List]:
    """Get recent database activity"""
    config = get_db_config()
    
    try:
        conn = psycopg2.connect(**config)
        
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            activity = {}
            
            # Recent queries
            cur.execute("""
                SELECT tool, model, created_at, 
                       LEFT(user_prompt, 100) as prompt_preview
                FROM queries 
                ORDER BY created_at DESC 
                LIMIT 10
            """)
            activity['recent_queries'] = [dict(row) for row in cur.fetchall()]
            
            # Recent knowledge captures
            cur.execute("""
                SELECT title, category, quality_score, created_at,
                       LEFT(content, 150) as content_preview
                FROM knowledge_units 
                ORDER BY created_at DESC 
                LIMIT 10
            """)
            activity['recent_knowledge'] = [dict(row) for row in cur.fetchall()]
            
            # Maintenance log
            cur.execute("""
                SELECT action, units_affected, created_at, details
                FROM knowledge_maintenance_log 
                ORDER BY created_at DESC 
                LIMIT 5
            """)
            activity['maintenance_log'] = [dict(row) for row in cur.fetchall()]
            
        conn.close()
        
        return activity
        
    except Exception as e:
        st.error(f"Error getting recent activity: {e}")
        return {}

def get_database_metrics() -> Dict[str, Any]:
    """Get database performance and usage metrics"""
    config = get_db_config()
    
    try:
        conn = psycopg2.connect(**config)
        
        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            metrics = {}
            
            # Connection stats
            cur.execute("""
                SELECT 
                    count(*) as total_connections,
                    count(*) FILTER (WHERE state = 'active') as active_connections,
                    count(*) FILTER (WHERE state = 'idle') as idle_connections
                FROM pg_stat_activity
                WHERE datname = current_database()
            """)
            metrics['connections'] = dict(cur.fetchone())
            
            # Database size and stats
            cur.execute("""
                SELECT 
                    pg_size_pretty(pg_database_size(current_database())) as total_size,
                    (SELECT count(*) FROM queries) as total_queries,
                    (SELECT count(*) FROM knowledge_units) as total_knowledge_units,
                    (SELECT count(*) FROM knowledge_links) as total_knowledge_links
            """)
            metrics['database'] = dict(cur.fetchone())
            
            # Recent activity counts
            cur.execute("""
                SELECT 
                    (SELECT count(*) FROM queries WHERE created_at > NOW() - INTERVAL '24 hours') as queries_24h,
                    (SELECT count(*) FROM knowledge_units WHERE created_at > NOW() - INTERVAL '24 hours') as knowledge_24h,
                    (SELECT count(*) FROM queries WHERE created_at > NOW() - INTERVAL '7 days') as queries_7d
            """)
            metrics['activity'] = dict(cur.fetchone())
            
            # Top tools by usage
            cur.execute("""
                SELECT tool, count(*) as usage_count
                FROM queries 
                WHERE created_at > NOW() - INTERVAL '30 days'
                GROUP BY tool 
                ORDER BY count(*) DESC 
                LIMIT 5
            """)
            metrics['top_tools'] = [dict(row) for row in cur.fetchall()]
            
        conn.close()
        
        return metrics
        
    except Exception as e:
        st.error(f"Error getting database metrics: {e}")
        return {}

def run_migration_check() -> Dict[str, Any]:
    """Check if migrations need to be run"""
    config = get_db_config()
    migration_status = {
        "needs_migration": False,
        "missing_tables": [],
        "missing_columns": [],
        "recommendations": []
    }
    
    try:
        conn = psycopg2.connect(**config)
        
        with conn.cursor() as cur:
            # Check for missing tables
            for table in REQUIRED_TABLES:
                cur.execute("""
                    SELECT COUNT(*) 
                    FROM information_schema.tables 
                    WHERE table_name = %s AND table_schema = 'public'
                """, (table,))
                
                if cur.fetchone()[0] == 0:
                    migration_status["missing_tables"].append(table)
                    migration_status["needs_migration"] = True
            
            # Check for missing columns in existing tables
            for table, expected_cols in EXPECTED_COLUMNS.items():
                cur.execute("""
                    SELECT COUNT(*) 
                    FROM information_schema.tables 
                    WHERE table_name = %s AND table_schema = 'public'
                """, (table,))
                
                if cur.fetchone()[0] > 0:  # Table exists
                    cur.execute("""
                        SELECT column_name 
                        FROM information_schema.columns 
                        WHERE table_name = %s AND table_schema = 'public'
                    """, (table,))
                    
                    existing_cols = [row[0] for row in cur.fetchall()]
                    missing_cols = [col for col in expected_cols if col not in existing_cols]
                    
                    if missing_cols:
                        migration_status["missing_columns"].extend([
                            f"{table}.{col}" for col in missing_cols
                        ])
                        migration_status["needs_migration"] = True
        
        conn.close()
        
        # Generate recommendations
        if migration_status["missing_tables"]:
            migration_status["recommendations"].append(
                "Run initial database setup: python scripts/setup/setup_database.py"
            )
        
        if "knowledge_links" in migration_status["missing_tables"] or "knowledge_maintenance_log" in migration_status["missing_tables"]:
            migration_status["recommendations"].append(
                "Run knowledge capture migration: python scripts/migration/create_knowledge_capture_tables.py"
            )
        
        if migration_status["missing_columns"]:
            migration_status["recommendations"].append(
                "Update existing tables with new columns - check migration scripts"
            )
        
        if not migration_status["needs_migration"]:
            migration_status["recommendations"].append("✅ All tables and columns are up to date!")
        
    except Exception as e:
        migration_status["error"] = str(e)
        
    return migration_status

def format_size(size_bytes: int) -> str:
    """Format bytes into human readable format"""
    for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
        if size_bytes < 1024.0:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024.0
    return f"{size_bytes:.1f} PB"

def main():
    st.set_page_config(
        page_title="Database Health Check",
        page_icon="🏥",
        layout="wide"
    )
    
    st.title("🏥 Database Health Check Dashboard")
    st.markdown("Monitor your PostgreSQL database health and TuoKit setup")
    
    # Auto-refresh option
    col1, col2, col3 = st.columns([2, 1, 1])
    with col1:
        auto_refresh = st.checkbox("Auto-refresh", value=False)
    with col2:
        if st.button("🔄 Refresh Now"):
            st.rerun()
    with col3:
        st.caption(f"Last update: {datetime.now().strftime('%H:%M:%S')}")
    
    # Connection Test
    st.subheader("🔌 Database Connection")
    is_connected, message, connection_info = test_database_connection()
    
    if is_connected:
        st.success(f"✅ {message}")
        
        # Show connection details
        with st.expander("📋 Connection Details", expanded=False):
            config = connection_info.get('config', {})
            col1, col2 = st.columns(2)
            
            with col1:
                st.info(f"**Host**: {config.get('host')}")
                st.info(f"**Port**: {config.get('port')}")
                st.info(f"**Database**: {config.get('dbname')}")
                st.info(f"**User**: {config.get('user')}")
                
            with col2:
                st.info(f"**Database Size**: {format_size(connection_info.get('db_size', 0))}")
                st.info(f"**Current User**: {connection_info.get('current_user', 'N/A')}")
                st.info(f"**Server**: {connection_info.get('server_ip', 'localhost')}:{connection_info.get('server_port', 'N/A')}")
                
            st.code(f"PostgreSQL Version: {connection_info.get('version', 'Unknown')}")
        
    else:
        st.error(f"❌ {message}")
        config = connection_info.get('config', {})
        
        st.info("""
        **Troubleshooting steps:**
        1. Ensure PostgreSQL is running: `pg_isready`
        2. Check if database exists: `psql -l`
        3. Verify credentials in .env file
        4. Test connection manually: `psql -h {host} -p {port} -U {user} -d {db}`
        """.format(**config))
        
        st.stop()  # Don't show other sections if not connected
    
    # Migration Status Check
    st.subheader("🔧 Migration Status")
    migration_status = run_migration_check()
    
    if migration_status.get("needs_migration"):
        st.warning("⚠️ Database setup incomplete!")
        
        if migration_status["missing_tables"]:
            st.error(f"Missing tables: {', '.join(migration_status['missing_tables'])}")
        
        if migration_status["missing_columns"]:
            st.warning(f"Missing columns: {', '.join(migration_status['missing_columns'])}")
        
        st.subheader("📝 Recommended Actions:")
        for recommendation in migration_status["recommendations"]:
            if recommendation.startswith("✅"):
                st.success(recommendation)
            else:
                st.code(recommendation)
                
    else:
        st.success("✅ All database tables and schema are up to date!")
        
        for recommendation in migration_status.get("recommendations", []):
            st.success(recommendation)
    
    # Table Status Overview
    st.subheader("📊 Table Status Overview")
    table_status = get_table_status()
    
    # Create columns for table grid
    cols = st.columns(3)
    
    for i, (table_name, status) in enumerate(table_status.items()):
        col = cols[i % 3]
        
        with col:
            if status.get("exists"):
                st.success(f"✅ **{table_name}**")
                st.metric("Rows", status.get("row_count", 0))
                st.caption(f"Size: {status.get('size', 'Unknown')}")
                
                if status.get("last_activity"):
                    time_ago = datetime.now() - status["last_activity"].replace(tzinfo=None)
                    if time_ago.days == 0:
                        st.caption(f"Last activity: {time_ago.seconds // 3600}h ago")
                    else:
                        st.caption(f"Last activity: {time_ago.days}d ago")
                        
            else:
                st.error(f"❌ **{table_name}**")
                st.caption("Table missing")
    
    # Database Metrics
    st.subheader("📈 Database Metrics")
    metrics = get_database_metrics()
    
    if metrics:
        # Performance metrics
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric("Total Size", metrics.get('database', {}).get('total_size', 'N/A'))
            
        with col2:
            st.metric("Active Connections", metrics.get('connections', {}).get('active_connections', 0))
            
        with col3:
            st.metric("Total Queries", metrics.get('database', {}).get('total_queries', 0))
            
        with col4:
            st.metric("Knowledge Units", metrics.get('database', {}).get('total_knowledge_units', 0))
        
        # Activity metrics
        st.subheader("📊 Recent Activity")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.metric("Queries (24h)", metrics.get('activity', {}).get('queries_24h', 0))
            
        with col2:
            st.metric("Knowledge (24h)", metrics.get('activity', {}).get('knowledge_24h', 0))
            
        with col3:
            st.metric("Queries (7d)", metrics.get('activity', {}).get('queries_7d', 0))
        
        # Top tools
        top_tools = metrics.get('top_tools', [])
        if top_tools:
            st.subheader("🏆 Most Active Tools (30d)")
            for tool in top_tools:
                st.metric(tool['tool'], tool['usage_count'])
    
    # Recent Activity
    st.subheader("🕒 Recent Database Activity")
    activity = get_recent_activity()
    
    if activity:
        tab1, tab2, tab3 = st.tabs(["Recent Queries", "Recent Knowledge", "Maintenance Log"])
        
        with tab1:
            recent_queries = activity.get('recent_queries', [])
            if recent_queries:
                for query in recent_queries:
                    with st.container():
                        col1, col2, col3 = st.columns([2, 1, 1])
                        with col1:
                            st.markdown(f"**{query['tool']}**: {query['prompt_preview']}...")
                        with col2:
                            st.caption(f"Model: {query['model']}")
                        with col3:
                            st.caption(str(query['created_at']))
            else:
                st.info("No recent queries found")
        
        with tab2:
            recent_knowledge = activity.get('recent_knowledge', [])
            if recent_knowledge:
                for knowledge in recent_knowledge:
                    with st.container():
                        col1, col2, col3 = st.columns([2, 1, 1])
                        with col1:
                            st.markdown(f"**{knowledge['title']}**")
                            st.caption(knowledge['content_preview'] + "...")
                        with col2:
                            st.caption(f"Category: {knowledge['category']}")
                            st.caption(f"Quality: {knowledge['quality_score']}/100")
                        with col3:
                            st.caption(str(knowledge['created_at']))
            else:
                st.info("No knowledge units found")
        
        with tab3:
            maintenance_log = activity.get('maintenance_log', [])
            if maintenance_log:
                for log in maintenance_log:
                    with st.container():
                        col1, col2, col3 = st.columns([2, 1, 1])
                        with col1:
                            st.markdown(f"**{log['action']}**")
                        with col2:
                            st.caption(f"Units affected: {log['units_affected'] or 0}")
                        with col3:
                            st.caption(str(log['created_at']))
            else:
                st.info("No maintenance log entries")
    
    # Database Schema Details
    with st.expander("🗂️ Database Schema Details", expanded=False):
        schema_info = check_indexes_and_constraints()
        
        if schema_info.get('indexes'):
            st.subheader("📑 Indexes")
            for idx in schema_info['indexes']:
                st.code(f"{idx['tablename']}.{idx['indexname']}")
        
        if schema_info.get('constraints'):
            st.subheader("🔒 Constraints")
            constraints_by_table = {}
            for const in schema_info['constraints']:
                table = const['table_name']
                if table not in constraints_by_table:
                    constraints_by_table[table] = []
                constraints_by_table[table].append(f"{const['constraint_name']} ({const['constraint_type']})")
            
            for table, constraints in constraints_by_table.items():
                st.markdown(f"**{table}**: {', '.join(constraints)}")
    
    # Auto-refresh logic
    if auto_refresh:
        time.sleep(5)
        st.rerun()

if __name__ == "__main__":
    main()