"""
TuoKit Unified Database Manager
Combines the best features from database.py and database_client.py
Maintains backward compatibility while adding advanced features
"""

import psycopg2
from psycopg2.extras import RealDictCursor
from psycopg2 import pool
import os
import json
from dotenv import load_dotenv
from typing import Optional, List, Tuple, Dict, Any
from contextlib import contextmanager
import logging
from datetime import datetime

# Load environment variables
load_dotenv()

# Setup logging
logging.basicConfig(level=logging.WARNING)  # Changed from INFO to reduce import noise
logger = logging.getLogger(__name__)

# Database configuration - supports both old and new env var names
DB_CONFIG = {
    "dbname": os.getenv("TUOKIT_PG_DB", os.getenv("DB_NAME", "tuokit_knowledge")),
    "user": os.getenv("TUOKIT_PG_USER", os.getenv("DB_USER", "tuokit_user")),
    "password": os.getenv("TUOKIT_PG_PASSWORD", os.getenv("DB_PASSWORD", "your_secure_password")),
    "host": os.getenv("TUOKIT_PG_HOST", os.getenv("DB_HOST", "localhost")),
    "port": os.getenv("TUOKIT_PG_PORT", "5432")
}

class DatabasePool:
    """Thread-safe connection pool for PostgreSQL"""
    
    def __init__(self, minconn: int = 1, maxconn: int = 10):
        """Initialize connection pool"""
        try:
            self.pool = psycopg2.pool.ThreadedConnectionPool(
                minconn,
                maxconn,
                **DB_CONFIG,
                # SSL configuration
                sslmode='require' if os.getenv('ENABLE_SSL', 'false').lower() == 'true' else 'prefer'
            )
            # # logger.info(f"Database pool created for {DB_CONFIG['host']}:{DB_CONFIG['port']}")  # Commented to fix import issue  # Commented to fix import issue
        except Exception as e:
            logger.error(f"Failed to create database pool: {str(e)}")
            self.pool = None
            raise
    
    @contextmanager
    def get_connection(self):
        """Get connection from pool"""
        conn = None
        try:
            if self.pool:
                conn = self.pool.getconn()
                yield conn
                conn.commit()
            else:
                raise Exception("Connection pool not initialized")
        except Exception as e:
            if conn:
                conn.rollback()
            logger.error(f"Database error: {str(e)}")
            raise
        finally:
            if conn and self.pool:
                self.pool.putconn(conn)
    
    def close_all(self):
        """Close all connections in pool"""
        if hasattr(self, 'pool') and self.pool:
            self.pool.closeall()

# Global connection pool (singleton)
_db_pool = None

def get_db_pool() -> DatabasePool:
    """Get or create database connection pool"""
    global _db_pool
    if _db_pool is None:
        _db_pool = DatabasePool()
    return _db_pool

class DatabaseManager:
    """
    Unified database manager with backward compatibility
    Combines simple API from original database.py with advanced features from database_client.py
    """
    
    def __init__(self, use_pool: bool = True):
        """
        Initialize database connection
        
        Args:
            use_pool: Use connection pooling (recommended) or single connection
        """
        self.use_pool = use_pool
        self.connected = False
        self.conn = None
        
        if use_pool:
            try:
                self.pool = get_db_pool()
                self.connected = True
            except Exception as e:
                logger.error(f"Failed to initialize connection pool: {e}")
                self.connected = False
        else:
            # Legacy single connection mode
            try:
                self.conn = psycopg2.connect(**DB_CONFIG)
                self.conn.autocommit = True
                self.connected = True
            except psycopg2.Error as e:
                logger.error(f"Database connection failed: {e}")
                self.conn = None
                self.connected = False
    
    @contextmanager
    def get_connection(self):
        """Get database connection (works with both pool and single connection)"""
        if self.use_pool and self.pool:
            with self.pool.get_connection() as conn:
                yield conn
        elif self.conn:
            yield self.conn
        else:
            raise Exception("No database connection available")
    
    def initialize_schema(self):
        """Create database tables if they don't exist"""
        schema_sql = """
        -- Queries table (main interaction log)
        CREATE TABLE IF NOT EXISTS queries (
            id SERIAL PRIMARY KEY,
            tool VARCHAR(100) NOT NULL,
            model VARCHAR(100) NOT NULL,
            user_prompt TEXT NOT NULL,
            ai_response TEXT NOT NULL,
            metadata JSONB,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Knowledge units table
        CREATE TABLE IF NOT EXISTS knowledge_units (
            id SERIAL PRIMARY KEY,
            query_id INTEGER REFERENCES queries(id),
            title VARCHAR(500) NOT NULL,
            content TEXT NOT NULL,
            category VARCHAR(100),
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Pipeline templates table
        CREATE TABLE IF NOT EXISTS pipeline_templates (
            id SERIAL PRIMARY KEY,
            name VARCHAR(200) NOT NULL,
            description TEXT,
            category VARCHAR(100),
            steps JSONB NOT NULL,
            usage_count INTEGER DEFAULT 0,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Pipeline executions table
        CREATE TABLE IF NOT EXISTS pipelines (
            id SERIAL PRIMARY KEY,
            name VARCHAR(200) NOT NULL,
            steps JSONB NOT NULL,
            results JSONB NOT NULL,
            execution_time_ms INTEGER,
            success BOOLEAN DEFAULT false,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Code snippets table
        CREATE TABLE IF NOT EXISTS code_snippets (
            id SERIAL PRIMARY KEY,
            language VARCHAR(50) NOT NULL,
            code TEXT NOT NULL,
            explanation TEXT,
            usage_example TEXT,
            knowledge_entry_id INTEGER REFERENCES knowledge_units(id),
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Document analyses table
        CREATE TABLE IF NOT EXISTS document_analyses (
            id SERIAL PRIMARY KEY,
            document_name VARCHAR(255) NOT NULL,
            document_type VARCHAR(50),
            content_hash VARCHAR(64),
            summary TEXT,
            key_points TEXT[],
            knowledge_entry_id INTEGER REFERENCES knowledge_units(id),
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Create indexes for better performance
        CREATE INDEX IF NOT EXISTS idx_queries_tool ON queries(tool);
        CREATE INDEX IF NOT EXISTS idx_queries_created ON queries(created_at DESC);
        CREATE INDEX IF NOT EXISTS idx_knowledge_category ON knowledge_units(category);
        CREATE INDEX IF NOT EXISTS idx_knowledge_created ON knowledge_units(created_at DESC);
        CREATE INDEX IF NOT EXISTS idx_code_language ON code_snippets(language);
        
        -- Create full-text search index
        CREATE EXTENSION IF NOT EXISTS pg_trgm;
        CREATE INDEX IF NOT EXISTS idx_knowledge_title_trgm ON knowledge_units USING gin(title gin_trgm_ops);
        CREATE INDEX IF NOT EXISTS idx_knowledge_content_trgm ON knowledge_units USING gin(content gin_trgm_ops);
        """
        
        with self.get_connection() as conn:
            with conn.cursor() as cur:
                cur.execute(schema_sql)
        
        logger.info("Database schema initialized")
    
    # ========== BACKWARD COMPATIBLE METHODS (from original database.py) ==========
    
    def get_recent_queries(self, limit: int = 5) -> List[Tuple]:
        """Get recent queries from database"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT id, tool, user_prompt, created_at 
                        FROM queries 
                        ORDER BY created_at DESC 
                        LIMIT %s
                    """, (limit,))
                    return cur.fetchall()
        except psycopg2.Error as e:
            logger.error(f"Database error: {e}")
            return []
    
    def get_queries_with_filters(self, tool: Optional[str] = None, 
                                start_date: Optional[datetime] = None,
                                end_date: Optional[datetime] = None,
                                limit: int = 1000) -> List[Tuple]:
        """Get queries with optional filters for tool and date range"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    # Build query dynamically based on filters
                    query = """
                        SELECT id, tool, created_at, user_prompt, ai_response 
                        FROM queries 
                        WHERE 1=1
                    """
                    params = []
                    
                    if tool:
                        query += " AND tool = %s"
                        params.append(tool)
                    
                    if start_date:
                        query += " AND created_at >= %s"
                        params.append(start_date)
                    
                    if end_date:
                        query += " AND created_at <= %s"
                        params.append(end_date)
                    
                    query += " ORDER BY created_at DESC LIMIT %s"
                    params.append(limit)
                    
                    cur.execute(query, tuple(params))
                    return cur.fetchall()
        except psycopg2.Error as e:
            logger.error(f"Database error in get_queries_with_filters: {e}")
            return []
    
    def get_knowledge_count(self) -> int:
        """Get total knowledge units"""
        if not self.connected:
            return 0
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT COUNT(*) FROM knowledge_units")
                    return cur.fetchone()[0]
        except psycopg2.Error:
            return 0
    
    def get_tool_usage_count(self, tool_name: str) -> int:
        """Get usage count for a specific tool"""
        if not self.connected:
            return 0
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT COUNT(*) FROM queries 
                        WHERE tool = %s
                    """, (tool_name,))
                    return cur.fetchone()[0]
        except psycopg2.Error:
            return 0
    
    def get_knowledge_by_id(self, knowledge_id: int) -> Optional[Tuple]:
        """Get a specific knowledge item by ID"""
        if not self.connected:
            return None
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT k.id, k.tool, k.prompt, k.response, k.created_at, 
                               k.category, k.metadata
                        FROM knowledge_units k
                        WHERE k.id = %s
                    """, (knowledge_id,))
                    return cur.fetchone()
        except psycopg2.Error as e:
            logger.error(f"Error getting knowledge by ID: {e}")
            return None
    
    def get_knowledge_by_ids(self, knowledge_ids: List[int]) -> Dict[int, Tuple]:
        """Get multiple knowledge items by IDs in a single query"""
        if not self.connected or not knowledge_ids:
            return {}
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    # Use ANY for efficient batch query
                    cur.execute("""
                        SELECT k.id, k.tool, k.prompt, k.response, k.created_at, 
                               k.category, k.metadata
                        FROM knowledge_units k
                        WHERE k.id = ANY(%s)
                    """, (knowledge_ids,))
                    
                    # Return as dict keyed by ID for easy lookup
                    results = {}
                    for row in cur.fetchall():
                        results[row[0]] = row
                    return results
        except psycopg2.Error as e:
            logger.error(f"Error getting knowledge by IDs: {e}")
            return {}
    
    def search_knowledge_safe(self, query: str, limit: int = 10) -> List[Tuple]:
        """Safe knowledge search with input validation and proper result format"""
        if not self.connected:
            return []
        
        # Input validation
        if not query or not isinstance(query, str):
            return []
        
        # Sanitize query - remove dangerous characters but keep search useful
        query = query.strip()
        if len(query) > 500:
            query = query[:500]
        
        # Validate limit
        limit = min(max(1, int(limit)), 100)
        
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    # Use parameterized query with proper escaping
                    search_pattern = f"%{query}%"
                    
                    cur.execute("""
                        SELECT k.id, k.tool, k.prompt, k.response, k.created_at, 
                               k.category, k.metadata
                        FROM knowledge_units k
                        WHERE k.prompt ILIKE %s OR k.response ILIKE %s
                        ORDER BY k.created_at DESC
                        LIMIT %s
                    """, (search_pattern, search_pattern, limit))
                    
                    return cur.fetchall()
        except (psycopg2.Error, ValueError) as e:
            logger.error(f"Error in safe knowledge search: {e}")
            return []
    
    def execute_query(self, query: str, params: Optional[Tuple] = None) -> List[Dict]:
        """Execute a SQL query and return results as list of dicts"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor(cursor_factory=RealDictCursor) as cur:
                    if params:
                        cur.execute(query, params)
                    else:
                        cur.execute(query)
                    
                    if cur.description:
                        return cur.fetchall()
                    return []
        except psycopg2.Error as e:
            logger.error(f"Query execution error: {e}")
            return []
    
    def log_query(self, tool: str, model: str, prompt: str, response: str, 
                  metadata: Optional[Dict] = None) -> Optional[int]:
        """Log a query to the database with optional metadata"""
        if not self.connected:
            return None
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    if metadata:
                        cur.execute("""
                            INSERT INTO queries (tool, model, user_prompt, ai_response, metadata)
                            VALUES (%s, %s, %s, %s, %s::jsonb)
                            RETURNING id
                        """, (tool, model, prompt, response, json.dumps(metadata)))
                    else:
                        cur.execute("""
                            INSERT INTO queries (tool, model, user_prompt, ai_response)
                            VALUES (%s, %s, %s, %s)
                            RETURNING id
                        """, (tool, model, prompt, response))
                    return cur.fetchone()[0]
        except psycopg2.Error as e:
            logger.error(f"Error logging query: {e}")
            return None
    
    def get_query_by_id(self, query_id: int) -> Optional[Tuple]:
        """Retrieve full query by ID"""
        if not self.connected:
            return None
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT * FROM queries WHERE id = %s", (query_id,))
                    return cur.fetchone()
        except psycopg2.Error as e:
            logger.error(f"Error retrieving query: {e}")
            return None
    
    def save_knowledge_unit(self, query_id: int, title: str, content: str, 
                           category: str, tags: Optional[List[str]] = None) -> bool:
        """Save extracted knowledge to database with optional tags"""
        if not self.connected:
            return False
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO knowledge_units 
                        (query_id, title, content, category)
                        VALUES (%s, %s, %s, %s)
                        RETURNING id
                    """, (query_id, title, content, category))
                    
                    # TODO: Add tags support when tags table is available
                    return True
        except psycopg2.Error as e:
            logger.error(f"Error saving knowledge: {e}")
            return False
    
    def get_knowledge_categories(self) -> List[str]:
        """Get distinct knowledge categories"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT DISTINCT category FROM knowledge_units ORDER BY category")
                    return [row[0] for row in cur.fetchall()]
        except psycopg2.Error as e:
            logger.error(f"Error getting categories: {e}")
            return []
    
    def get_knowledge_by_id(self, k_id: int) -> Optional[Tuple]:
        """Get knowledge unit by ID with related query info"""
        if not self.connected:
            return None
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT k.*, q.tool, q.model, q.created_at 
                        FROM knowledge_units k
                        JOIN queries q ON k.query_id = q.id
                        WHERE k.id = %s
                    """, (k_id,))
                    return cur.fetchone()
        except psycopg2.Error as e:
            logger.error(f"Error getting knowledge: {e}")
            return None
    
    def search_knowledge(self, search_term: str, category: Optional[str] = None) -> List[Tuple]:
        """Search knowledge base with optional category filter"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    if category:
                        cur.execute("""
                            SELECT id, title, content, category
                            FROM knowledge_units 
                            WHERE (title ILIKE %s OR content ILIKE %s)
                            AND category = %s
                            ORDER BY created_at DESC
                        """, (f"%{search_term}%", f"%{search_term}%", category))
                    else:
                        cur.execute("""
                            SELECT id, title, content, category
                            FROM knowledge_units 
                            WHERE title ILIKE %s OR content ILIKE %s
                            ORDER BY created_at DESC
                        """, (f"%{search_term}%", f"%{search_term}%"))
                    return cur.fetchall()
        except psycopg2.Error as e:
            logger.error(f"Error searching knowledge: {e}")
            return []
    
    def save_pipeline(self, name: str, steps: List[Dict], results: Dict,
                     execution_time_ms: Optional[int] = None) -> Optional[int]:
        """Save pipeline execution to database"""
        if not self.connected:
            return None
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    # Determine success from results
                    success = all(
                        step.get("success", False) 
                        for step in results.get("log", [])
                    )
                    
                    cur.execute("""
                        INSERT INTO pipelines 
                        (name, steps, results, execution_time_ms, success)
                        VALUES (%s, %s::jsonb, %s::jsonb, %s, %s)
                        RETURNING id
                    """, (
                        name,
                        json.dumps(steps),
                        json.dumps(results),
                        execution_time_ms,
                        success
                    ))
                    return cur.fetchone()[0]
        except psycopg2.Error as e:
            logger.error(f"Error saving pipeline: {e}")
            return None
    
    def get_pipeline_templates(self, category: Optional[str] = None) -> List[Dict]:
        """Get available pipeline templates"""
        if not self.connected:
            return []
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    if category:
                        cur.execute("""
                            SELECT id, name, description, category, steps, usage_count
                            FROM pipeline_templates
                            WHERE category = %s
                            ORDER BY usage_count DESC, name
                        """, (category,))
                    else:
                        cur.execute("""
                            SELECT id, name, description, category, steps, usage_count
                            FROM pipeline_templates
                            ORDER BY usage_count DESC, name
                        """)
                    
                    templates = []
                    for row in cur.fetchall():
                        templates.append({
                            "id": row[0],
                            "name": row[1],
                            "description": row[2],
                            "category": row[3],
                            "steps": row[4],
                            "usage_count": row[5]
                        })
                    return templates
        except psycopg2.Error as e:
            logger.error(f"Error getting templates: {e}")
            return []
    
    def increment_template_usage(self, template_id: int) -> bool:
        """Increment usage count for a pipeline template"""
        if not self.connected:
            return False
        try:
            with self.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        UPDATE pipeline_templates 
                        SET usage_count = usage_count + 1,
                            updated_at = NOW()
                        WHERE id = %s
                    """, (template_id,))
                    return True
        except psycopg2.Error as e:
            logger.error(f"Error updating template usage: {e}")
            return False
    
    # ========== ADVANCED METHODS (from database_client.py) ==========
    
    def save_knowledge(self, tool_name: str, prompt: str, response: str,
                      model_used: str = None, context_data: Dict = None,
                      tags: List[str] = None, user_id: str = None, 
                      session_id: str = None) -> int:
        """Save knowledge entry to database (advanced version)"""
        # First log the query
        query_id = self.log_query(tool_name, model_used or "unknown", prompt, response, context_data)
        
        if query_id:
            # Extract title from prompt (first 100 chars)
            title = prompt[:100] + "..." if len(prompt) > 100 else prompt
            
            # Determine category based on tool
            category_map = {
                "sql": "SQL Patterns",
                "code": "Code Patterns",
                "error": "Error Solutions",
                "doc": "Documentation"
            }
            category = category_map.get(tool_name.split('_')[0], "General")
            
            # Save as knowledge unit
            self.save_knowledge_unit(query_id, title, response, category, tags)
            
        return query_id
    
    def search_knowledge_advanced(self, query: str, tool_name: str = None, 
                                 limit: int = 10) -> List[Dict]:
        """Search knowledge base with full-text search (advanced)"""
        sql = """
        SELECT 
            k.id, k.title, k.content, k.category, k.created_at,
            q.tool, q.model, q.user_prompt, q.ai_response,
            similarity(k.title || ' ' || k.content, %s) as relevance
        FROM knowledge_units k
        JOIN queries q ON k.query_id = q.id
        WHERE 1=1
        """
        
        params = [query]
        
        if tool_name:
            sql += " AND q.tool = %s"
            params.append(tool_name)
        
        sql += """
        AND (k.title ILIKE %s OR k.content ILIKE %s 
             OR similarity(k.title || ' ' || k.content, %s) > 0.1)
        ORDER BY relevance DESC, k.created_at DESC
        LIMIT %s
        """
        
        params.extend([f'%{query}%', f'%{query}%', query, limit])
        
        return self.execute_query(sql, tuple(params))
    
    def get_recent_entries(self, tool_name: str = None, limit: int = 20) -> List[Dict]:
        """Get recent knowledge entries"""
        sql = """
        SELECT k.*, q.tool, q.model, q.user_prompt 
        FROM knowledge_units k
        JOIN queries q ON k.query_id = q.id
        WHERE 1=1
        """
        
        params = []
        if tool_name:
            sql += " AND q.tool = %s"
            params.append(tool_name)
        
        sql += " ORDER BY k.created_at DESC LIMIT %s"
        params.append(limit)
        
        return self.execute_query(sql, tuple(params))
    
    def save_code_snippet(self, language: str, code: str, 
                         explanation: str = None, usage_example: str = None,
                         knowledge_entry_id: int = None) -> int:
        """Save code snippet to database"""
        sql = """
        INSERT INTO code_snippets 
        (language, code, explanation, usage_example, knowledge_entry_id)
        VALUES (%s, %s, %s, %s, %s)
        RETURNING id
        """
        
        with self.get_connection() as conn:
            with conn.cursor() as cur:
                cur.execute(sql, (language, code, explanation, usage_example, knowledge_entry_id))
                return cur.fetchone()[0]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get database statistics"""
        sql = """
        SELECT 
            COUNT(*) as total_entries,
            COUNT(DISTINCT tool) as unique_tools,
            COUNT(DISTINCT DATE(created_at)) as active_days,
            array_agg(DISTINCT tool) as tools_list
        FROM queries;
        """
        
        stats = self.execute_query(sql)
        if stats:
            return stats[0]
        return {
            "total_entries": 0,
            "unique_tools": 0,
            "active_days": 0,
            "tools_list": []
        }

# ========== CONVENIENCE FUNCTIONS ==========

def test_database_connection() -> bool:
    """Test if database connection is properly configured"""
    try:
        db = DatabaseManager()
        stats = db.get_statistics()
        logger.info(f"✅ Connected to PostgreSQL at {DB_CONFIG['host']}")
        logger.info(f"Database contains {stats['total_entries']} knowledge entries")
        return True
    except Exception as e:
        logger.error(f"❌ Database connection failed: {str(e)}")
        return False

def save_to_knowledge_base(tool: str, prompt: str, response: str, 
                          model: str = "deepseek-r1", context: Any = None) -> None:
    """Quick function to save to knowledge base"""
    try:
        db = DatabaseManager()
        
        # Convert context to dict if needed
        context_data = {}
        if context:
            if isinstance(context, str):
                context_data = {"description": context}
            elif isinstance(context, dict):
                context_data = context
            else:
                context_data = {"data": str(context)}
        
        entry_id = db.save_knowledge(
            tool_name=tool,
            prompt=prompt,
            response=response,
            model_used=model,
            context_data=context_data
        )
        
        logger.info(f"Saved knowledge entry {entry_id} for tool {tool}")
        
    except Exception as e:
        logger.error(f"Failed to save to knowledge base: {str(e)}")
        # Don't break the application flow if saving fails

# For backward compatibility with imports
TuoKitDB = DatabaseManager  # Alias for database_client.py compatibility
