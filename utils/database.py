"""
Database utilities for TuoKit
Handles PostgreSQL connections and knowledge management
"""

import psycopg2
import os
import json
from dotenv import load_dotenv
from typing import Optional, List, Tuple, Dict

# Load environment variables
load_dotenv()

# Database configuration
DB_CONFIG = {
    "dbname": os.getenv("DB_NAME", "ollama_knowledge"),
    "user": os.getenv("DB_USER", "ollama_user"),
    "password": os.getenv("DB_PASSWORD", "secure_password"),
    "host": os.getenv("DB_HOST", "localhost")
}

class DatabaseManager:
    """Manages database connections and operations"""
    
    def __init__(self):
        """Initialize database connection"""
        try:
            self.conn = psycopg2.connect(**DB_CONFIG)
            self.conn.autocommit = True
            self.connected = True
        except psycopg2.Error as e:
            print(f"Database connection failed: {e}")
            self.conn = None
            self.connected = False    
    def get_recent_queries(self, limit: int = 5) -> List[Tuple]:
        """Get recent queries from database"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    SELECT id, tool, user_prompt, created_at 
                    FROM queries 
                    ORDER BY created_at DESC 
                    LIMIT %s
                """, (limit,))
                return cur.fetchall()
        except psycopg2.Error as e:
            print(f"Database error: {e}")
            return []
    
    def get_knowledge_count(self) -> int:
        """Get total knowledge units"""
        if not self.connected or not self.conn:
            return 0
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT COUNT(*) FROM knowledge_units")
                return cur.fetchone()[0]
        except psycopg2.Error:
            return 0
    
    def log_query(self, tool: str, model: str, prompt: str, response: str, 
                  metadata: Optional[Dict] = None) -> Optional[int]:
        """Log a query to the database with optional metadata"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                if metadata:
                    cur.execute("""
                        INSERT INTO queries (tool, model, user_prompt, ai_response, metadata)
                        VALUES (%s, %s, %s, %s, %s::jsonb)
                        RETURNING id
                    """, (tool, model, prompt, response, str(metadata)))
                else:
                    cur.execute("""
                        INSERT INTO queries (tool, model, user_prompt, ai_response)
                        VALUES (%s, %s, %s, %s)
                        RETURNING id
                    """, (tool, model, prompt, response))
                return cur.fetchone()[0]
        except psycopg2.Error as e:
            print(f"Error logging query: {e}")
            return None    
    def get_query_by_id(self, query_id: int) -> Optional[Tuple]:
        """Retrieve full query by ID"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT * FROM queries WHERE id = %s", (query_id,))
                return cur.fetchone()
        except psycopg2.Error as e:
            print(f"Error retrieving query: {e}")
            return None
    
    def save_knowledge_unit(self, query_id: int, title: str, content: str, 
                           category: str, tags: Optional[List[str]] = None) -> bool:
        """Save extracted knowledge to database with optional tags"""
        if not self.connected or not self.conn:
            return False
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO knowledge_units 
                    (query_id, title, content, category)
                    VALUES (%s, %s, %s, %s)
                    RETURNING id
                """, (query_id, title, content, category))
                
                # TODO: Add tags support when tags table is available
                return True
        except psycopg2.Error as e:
            print(f"Error saving knowledge: {e}")
            return False
    
    def get_knowledge_categories(self) -> List[str]:
        """Get distinct knowledge categories"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT DISTINCT category FROM knowledge_units ORDER BY category")
                return [row[0] for row in cur.fetchall()]
        except psycopg2.Error as e:
            print(f"Error getting categories: {e}")
            return []
    
    def get_knowledge_by_id(self, k_id: int) -> Optional[Tuple]:
        """Get knowledge unit by ID with related query info"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    SELECT k.*, q.tool, q.model, q.created_at 
                    FROM knowledge_units k
                    JOIN queries q ON k.query_id = q.id
                    WHERE k.id = %s
                """, (k_id,))
                return cur.fetchone()
        except psycopg2.Error as e:
            print(f"Error getting knowledge: {e}")
            return None
    
    def search_knowledge(self, search_term: str, category: Optional[str] = None) -> List[Tuple]:
        """Search knowledge base with optional category filter"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
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
            print(f"Error searching knowledge: {e}")
            return []
    
    def save_pipeline(self, name: str, steps: List[Dict], results: Dict,
                     execution_time_ms: Optional[int] = None) -> Optional[int]:
        """Save pipeline execution to database"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
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
            print(f"Error saving pipeline: {e}")
            return None
    
    def get_pipeline_templates(self, category: Optional[str] = None) -> List[Dict]:
        """Get available pipeline templates"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
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
            print(f"Error getting templates: {e}")
            return []
    
    def increment_template_usage(self, template_id: int) -> bool:
        """Increment usage count for a pipeline template"""
        if not self.connected or not self.conn:
            return False
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    UPDATE pipeline_templates 
                    SET usage_count = usage_count + 1,
                        updated_at = NOW()
                    WHERE id = %s
                """, (template_id,))
                return True
        except psycopg2.Error as e:
            print(f"Error updating template usage: {e}")
            return False
