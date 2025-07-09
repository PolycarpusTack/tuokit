"""
Agent Memory - Simple persistent context system
No overengineering, just SQLite with smart queries
"""

from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta
import json
import sqlite3
from pathlib import Path


class AgentMemory:
    """Simple memory system for agents using existing database"""
    
    def __init__(self, db_path: str = "tuokit.db"):
        self.db_path = db_path
        self._init_tables()
    
    def _init_tables(self):
        """Create memory tables if they don't exist"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_memory (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_name TEXT NOT NULL,
                    context_key TEXT NOT NULL,
                    context_value TEXT NOT NULL,
                    metadata TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    accessed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    access_count INTEGER DEFAULT 1
                )
            """)
            
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_agent_context 
                ON agent_memory(agent_name, context_key)
            """)
            
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_created 
                ON agent_memory(created_at)
            """)
    
    def remember(self, agent_name: str, key: str, value: Any, 
                metadata: Optional[Dict] = None) -> None:
        """Store something in memory"""
        with sqlite3.connect(self.db_path) as conn:
            # Check if already exists
            existing = conn.execute(
                "SELECT id FROM agent_memory WHERE agent_name = ? AND context_key = ?",
                (agent_name, key)
            ).fetchone()
            
            if existing:
                # Update existing
                conn.execute("""
                    UPDATE agent_memory 
                    SET context_value = ?, metadata = ?, 
                        accessed_at = CURRENT_TIMESTAMP, 
                        access_count = access_count + 1
                    WHERE id = ?
                """, (json.dumps(value), json.dumps(metadata or {}), existing[0]))
            else:
                # Insert new
                conn.execute("""
                    INSERT INTO agent_memory (agent_name, context_key, context_value, metadata)
                    VALUES (?, ?, ?, ?)
                """, (agent_name, key, json.dumps(value), json.dumps(metadata or {})))
    
    def recall(self, agent_name: str, key: str) -> Optional[Any]:
        """Retrieve something from memory"""
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute("""
                SELECT context_value FROM agent_memory 
                WHERE agent_name = ? AND context_key = ?
            """, (agent_name, key)).fetchone()
            
            if result:
                # Update access time
                conn.execute("""
                    UPDATE agent_memory 
                    SET accessed_at = CURRENT_TIMESTAMP, 
                        access_count = access_count + 1
                    WHERE agent_name = ? AND context_key = ?
                """, (agent_name, key))
                
                return json.loads(result[0])
            return None
    
    def search_memories(self, agent_name: str, query: str, 
                       limit: int = 10) -> List[Dict[str, Any]]:
        """Search memories by content"""
        with sqlite3.connect(self.db_path) as conn:
            # Simple LIKE search - could upgrade to FTS5 later if needed
            results = conn.execute("""
                SELECT context_key, context_value, metadata, created_at, access_count
                FROM agent_memory
                WHERE agent_name = ? 
                AND (context_key LIKE ? OR context_value LIKE ?)
                ORDER BY access_count DESC, accessed_at DESC
                LIMIT ?
            """, (agent_name, f"%{query}%", f"%{query}%", limit)).fetchall()
            
            return [
                {
                    "key": row[0],
                    "value": json.loads(row[1]),
                    "metadata": json.loads(row[2]),
                    "created_at": row[3],
                    "access_count": row[4]
                }
                for row in results
            ]
    
    def get_context(self, agent_name: str, 
                   categories: Optional[List[str]] = None,
                   days_back: int = 30) -> Dict[str, Any]:
        """Get relevant context for an agent"""
        with sqlite3.connect(self.db_path) as conn:
            cutoff_date = datetime.now() - timedelta(days=days_back)
            
            # Get frequently accessed memories
            frequent = conn.execute("""
                SELECT context_key, context_value, access_count
                FROM agent_memory
                WHERE agent_name = ? 
                AND accessed_at >= ?
                ORDER BY access_count DESC
                LIMIT 20
            """, (agent_name, cutoff_date)).fetchall()
            
            # Get recent memories
            recent = conn.execute("""
                SELECT context_key, context_value
                FROM agent_memory
                WHERE agent_name = ?
                ORDER BY created_at DESC
                LIMIT 10
            """, (agent_name,)).fetchall()
            
            return {
                "frequent": {row[0]: json.loads(row[1]) for row in frequent},
                "recent": {row[0]: json.loads(row[1]) for row in recent}
            }
    
    def cleanup_old_memories(self, days: int = 90) -> int:
        """Remove memories not accessed in X days"""
        with sqlite3.connect(self.db_path) as conn:
            cutoff_date = datetime.now() - timedelta(days=days)
            cursor = conn.execute("""
                DELETE FROM agent_memory 
                WHERE accessed_at < ? AND access_count < 5
            """, (cutoff_date,))
            return cursor.rowcount


class BroadcastMemoryPatterns:
    """Memory patterns specific to broadcast software teams"""
    
    @staticmethod
    def remember_customer_issue(memory: AgentMemory, customer: str, 
                              issue: str, resolution: str):
        """Support team pattern - remember customer issues"""
        memory.remember(
            "support_agent",
            f"customer_{customer}_issue_{hash(issue) % 10000}",
            {
                "customer": customer,
                "issue": issue,
                "resolution": resolution,
                "resolved_at": datetime.now().isoformat()
            },
            metadata={"type": "support_ticket"}
        )
    
    @staticmethod
    def remember_api_pattern(memory: AgentMemory, endpoint: str, 
                           error: str, fix: str):
        """Dev team pattern - remember API fixes"""
        memory.remember(
            "api_debugger",
            f"api_{endpoint.replace('/', '_')}",
            {
                "endpoint": endpoint,
                "error": error,
                "fix": fix,
                "last_seen": datetime.now().isoformat()
            },
            metadata={"type": "api_fix", "protocol": "broadcast"}
        )
    
    @staticmethod
    def remember_compliance_check(memory: AgentMemory, component: str,
                                regulation: str, status: str, notes: str):
        """Legal team pattern - track compliance checks"""
        memory.remember(
            "compliance_agent",
            f"compliance_{component}_{regulation}",
            {
                "component": component,
                "regulation": regulation,
                "status": status,
                "notes": notes,
                "checked_at": datetime.now().isoformat()
            },
            metadata={"type": "compliance", "industry": "broadcast"}
        )
    
    @staticmethod  
    def search_similar_issues(memory: AgentMemory, error_message: str) -> List[Dict]:
        """Find similar issues from memory"""
        # Simple keyword extraction
        keywords = error_message.lower().split()[:5]
        
        results = []
        for keyword in keywords:
            results.extend(memory.search_memories("support_agent", keyword, limit=5))
            results.extend(memory.search_memories("api_debugger", keyword, limit=5))
        
        # Deduplicate by key
        seen = set()
        unique_results = []
        for r in results:
            if r['key'] not in seen:
                seen.add(r['key'])
                unique_results.append(r)
        
        return unique_results[:10]  # Top 10 most relevant
