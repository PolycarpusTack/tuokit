"""
Database Health Checker for TuoKit
Monitors PostgreSQL database health and migration status
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

from .config import REQUIRED_TABLES, EXPECTED_COLUMNS

class DatabaseHealthChecker:
    """PostgreSQL database health monitoring"""
    
    def __init__(self):
        self.config = self._get_db_config()
        self._connection_cache = None
        self._last_check = None
        self._cache_duration = timedelta(seconds=30)
    
    def _get_db_config(self):
        """Get database configuration from environment"""
        load_dotenv()
        
        return {
            "dbname": os.getenv("TUOKIT_PG_DB", os.getenv("DB_NAME", "tuokit_knowledge")),
            "user": os.getenv("TUOKIT_PG_USER", os.getenv("DB_USER", "tuokit_user")),
            "password": os.getenv("TUOKIT_PG_PASSWORD", os.getenv("DB_PASSWORD", "your_secure_password")),
            "host": os.getenv("TUOKIT_PG_HOST", os.getenv("DB_HOST", "localhost")),
            "port": os.getenv("TUOKIT_PG_PORT", "5432")
        }
    
    def get_quick_status(self) -> Dict[str, Any]:
        """Get quick status for overview dashboard"""
        # Use cache if available and fresh
        if self._connection_cache and self._last_check:
            if datetime.now() - self._last_check < self._cache_duration:
                return self._connection_cache
        
        status = {
            'connected': False,
            'table_count': 0,
            'required_tables': len(REQUIRED_TABLES),
            'missing_tables': [],
            'error': None
        }
        
        try:
            conn = psycopg2.connect(**self.config)
            
            with conn.cursor() as cur:
                # Check connection
                cur.execute("SELECT 1")
                status['connected'] = True
                
                # Count existing tables
                cur.execute("""
                    SELECT COUNT(*) 
                    FROM information_schema.tables 
                    WHERE table_schema = 'public' 
                    AND table_name = ANY(%s)
                """, (REQUIRED_TABLES,))
                status['table_count'] = cur.fetchone()[0]
                
                # Find missing tables
                cur.execute("""
                    SELECT unnest(%s::text[]) AS table_name
                    EXCEPT
                    SELECT table_name 
                    FROM information_schema.tables 
                    WHERE table_schema = 'public'
                """, (REQUIRED_TABLES,))
                status['missing_tables'] = [row[0] for row in cur.fetchall()]
            
            conn.close()
            
        except Exception as e:
            status['error'] = str(e)
        
        # Cache the result
        self._connection_cache = status
        self._last_check = datetime.now()
        
        return status
    
    def get_detailed_status(self) -> Dict[str, Any]:
        """Get detailed status for reporting"""
        status = self.get_quick_status()
        
        if not status['connected']:
            return status
        
        try:
            conn = psycopg2.connect(**self.config)
            
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                # Database size
                cur.execute("""
                    SELECT 
                        pg_database_size(current_database()) as db_size,
                        pg_size_pretty(pg_database_size(current_database())) as db_size_pretty
                """)
                size_info = dict(cur.fetchone())
                status['database_size'] = size_info
                
                # Table details
                status['tables'] = {}
                for table in REQUIRED_TABLES:
                    if table not in status['missing_tables']:
                        cur.execute(f"""
                            SELECT 
                                COUNT(*) as row_count,
                                pg_size_pretty(pg_total_relation_size('{table}')) as size
                            FROM {table}
                        """)
                        status['tables'][table] = dict(cur.fetchone())
                
                # Recent activity
                cur.execute("""
                    SELECT 
                        (SELECT COUNT(*) FROM queries WHERE created_at > NOW() - INTERVAL '24 hours') as queries_24h,
                        (SELECT COUNT(*) FROM knowledge_units WHERE created_at > NOW() - INTERVAL '24 hours') as knowledge_24h
                """)
                status['recent_activity'] = dict(cur.fetchone())
            
            conn.close()
            
        except Exception as e:
            status['detail_error'] = str(e)
        
        return status
    
    def run_diagnostics(self) -> Dict[str, Dict[str, Any]]:
        """Run diagnostic tests"""
        diagnostics = {}
        
        # Test 1: Connection
        try:
            conn = psycopg2.connect(**self.config)
            conn.close()
            diagnostics['connection'] = {
                'passed': True,
                'message': 'Database connection successful'
            }
        except Exception as e:
            diagnostics['connection'] = {
                'passed': False,
                'message': f'Connection failed: {str(e)}'
            }
            return diagnostics  # Can't continue without connection
        
        # Test 2: Required tables
        status = self.get_quick_status()
        if status['missing_tables']:
            diagnostics['tables'] = {
                'passed': False,
                'message': f"Missing tables: {', '.join(status['missing_tables'])}"
            }
        else:
            diagnostics['tables'] = {
                'passed': True,
                'message': 'All required tables present'
            }
        
        # Test 3: Column integrity
        missing_columns = self._check_column_integrity()
        if missing_columns:
            diagnostics['columns'] = {
                'passed': False,
                'message': f"Missing columns: {', '.join(missing_columns[:3])}..."
            }
        else:
            diagnostics['columns'] = {
                'passed': True,
                'message': 'All table schemas correct'
            }
        
        # Test 4: Write permissions
        try:
            conn = psycopg2.connect(**self.config)
            with conn.cursor() as cur:
                cur.execute("CREATE TEMP TABLE test_write (id int)")
                cur.execute("DROP TABLE test_write")
            conn.close()
            diagnostics['permissions'] = {
                'passed': True,
                'message': 'Write permissions verified'
            }
        except Exception as e:
            diagnostics['permissions'] = {
                'passed': False,
                'message': f'Write permission error: {str(e)}'
            }
        
        return diagnostics
    
    def _check_column_integrity(self) -> List[str]:
        """Check for missing columns in existing tables"""
        missing_columns = []
        
        try:
            conn = psycopg2.connect(**self.config)
            
            with conn.cursor() as cur:
                for table, expected_cols in EXPECTED_COLUMNS.items():
                    # Check if table exists
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
                            missing_columns.extend([f"{table}.{col}" for col in missing_cols])
            
            conn.close()
            
        except Exception:
            pass
        
        return missing_columns
    
    def run(self):
        """Run the database health check interface"""
        st.subheader("🗄️ Database Health Check")
        
        # Auto-refresh option
        col1, col2, col3 = st.columns([2, 1, 1])
        with col1:
            auto_refresh = st.checkbox("Auto-refresh", value=False, key="db_auto_refresh")
        with col2:
            if st.button("🔄 Refresh", key="db_refresh"):
                self._connection_cache = None  # Clear cache
                st.rerun()
        with col3:
            st.caption(f"Last: {datetime.now().strftime('%H:%M:%S')}")
        
        # Connection status
        status = self.get_quick_status()
        
        if status['connected']:
            st.success("✅ Database connected")
            
            # Show metrics
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Tables", f"{status['table_count']}/{status['required_tables']}")
            with col2:
                detailed = self.get_detailed_status()
                if 'database_size' in detailed:
                    st.metric("Size", detailed['database_size']['db_size_pretty'])
            with col3:
                if 'recent_activity' in detailed:
                    st.metric("Queries (24h)", detailed['recent_activity']['queries_24h'])
            
            # Table status
            if status['missing_tables']:
                st.warning(f"⚠️ Missing tables: {', '.join(status['missing_tables'])}")
                
                st.info("""
                **To fix missing tables:**
                ```bash
                python scripts/setup/setup_database.py
                ```
                """)
            
            # Show table details
            with st.expander("📊 Table Details", expanded=False):
                self._show_table_details()
            
            # Recent activity
            with st.expander("🕒 Recent Activity", expanded=False):
                self._show_recent_activity()
        
        else:
            st.error(f"❌ Database connection failed")
            if status['error']:
                st.error(f"Error: {status['error']}")
            
            st.info(f"""
            **Connection settings:**
            - Host: {self.config['host']}
            - Port: {self.config['port']}
            - Database: {self.config['dbname']}
            - User: {self.config['user']}
            
            **Troubleshooting:**
            1. Check PostgreSQL is running: `pg_isready`
            2. Verify credentials in .env file
            3. Test connection: `psql -h {self.config['host']} -p {self.config['port']} -U {self.config['user']} -d {self.config['dbname']}`
            """)
        
        # Auto-refresh
        if auto_refresh:
            import time
            time.sleep(5)
            st.rerun()
    
    def _show_table_details(self):
        """Show detailed table information"""
        try:
            conn = psycopg2.connect(**self.config)
            
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                for table in REQUIRED_TABLES:
                    cur.execute("""
                        SELECT COUNT(*) 
                        FROM information_schema.tables 
                        WHERE table_name = %s AND table_schema = 'public'
                    """, (table,))
                    
                    if cur.fetchone()['count'] > 0:
                        # Get table info
                        cur.execute(f"""
                            SELECT 
                                COUNT(*) as row_count,
                                pg_size_pretty(pg_total_relation_size('{table}')) as size
                            FROM {table}
                        """)
                        info = dict(cur.fetchone())
                        
                        col1, col2, col3 = st.columns([2, 1, 1])
                        with col1:
                            st.info(f"**{table}**")
                        with col2:
                            st.caption(f"Rows: {info['row_count']}")
                        with col3:
                            st.caption(f"Size: {info['size']}")
            
            conn.close()
            
        except Exception as e:
            st.error(f"Error getting table details: {e}")
    
    def _show_recent_activity(self):
        """Show recent database activity"""
        try:
            conn = psycopg2.connect(**self.config)
            
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                # Recent queries
                cur.execute("""
                    SELECT tool, model, created_at, 
                           LEFT(user_prompt, 100) as prompt_preview
                    FROM queries 
                    ORDER BY created_at DESC 
                    LIMIT 5
                """)
                recent_queries = [dict(row) for row in cur.fetchall()]
                
                if recent_queries:
                    st.markdown("**Recent Queries:**")
                    for query in recent_queries:
                        st.caption(f"{query['tool']} ({query['model']}) - {query['created_at']}")
                        st.text(query['prompt_preview'] + "...")
                
                # Recent knowledge
                cur.execute("""
                    SELECT title, category, quality_score, created_at
                    FROM knowledge_units 
                    ORDER BY created_at DESC 
                    LIMIT 5
                """)
                recent_knowledge = [dict(row) for row in cur.fetchall()]
                
                if recent_knowledge:
                    st.markdown("**Recent Knowledge Captures:**")
                    for knowledge in recent_knowledge:
                        st.caption(f"{knowledge['title']} ({knowledge['category']}) - Score: {knowledge['quality_score']}")
            
            conn.close()
            
        except Exception as e:
            st.error(f"Error getting recent activity: {e}")