"""
Minimal Vector Search Implementation for TuoKit
Uses PostgreSQL with Ollama embeddings
Falls back to text similarity if pgvector not available
"""

import json
import numpy as np
from typing import List, Dict, Optional, Tuple
import psycopg2
from psycopg2.extras import RealDictCursor
import logging
from utils.ollama import OllamaClient
from utils.database import DatabaseManager

logger = logging.getLogger(__name__)

class VectorSearch:
    """Simple vector search using PostgreSQL and Ollama embeddings"""
    
    def __init__(self, embedding_model: str = "nomic-embed-text"):
        self.db = DatabaseManager()
        self.ollama = OllamaClient()
        self.embedding_model = embedding_model
        self.has_pgvector = self._check_pgvector()
        self.embedding_dim = 768  # nomic-embed-text dimension
        
        # Initialize schema
        self._init_schema()
    
    def _check_pgvector(self) -> bool:
        """Check if pgvector extension is available"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("SELECT * FROM pg_extension WHERE extname = 'vector'")
                    return cur.fetchone() is not None
        except:
            return False
    
    def _init_schema(self):
        """Initialize vector search schema"""
        with self.db.get_connection() as conn:
            with conn.cursor() as cur:
                if self.has_pgvector:
                    # Create vector-enabled table
                    cur.execute(f"""
                        CREATE TABLE IF NOT EXISTS knowledge_vectors (
                            id SERIAL PRIMARY KEY,
                            knowledge_id INTEGER REFERENCES knowledge_units(id),
                            content TEXT NOT NULL,
                            embedding vector({self.embedding_dim}),
                            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                        );
                        
                        CREATE INDEX IF NOT EXISTS idx_knowledge_vectors_embedding 
                        ON knowledge_vectors USING ivfflat (embedding vector_cosine_ops);
                    """)
                else:
                    # Fallback: Store embeddings as JSON
                    cur.execute("""
                        CREATE TABLE IF NOT EXISTS knowledge_embeddings (
                            id SERIAL PRIMARY KEY,
                            knowledge_id INTEGER REFERENCES knowledge_units(id),
                            content TEXT NOT NULL,
                            embedding JSONB NOT NULL,
                            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                        );
                        
                        CREATE INDEX IF NOT EXISTS idx_knowledge_embeddings_kid 
                        ON knowledge_embeddings(knowledge_id);
                    """)
                
                logger.info(f"Vector search initialized (pgvector: {self.has_pgvector})")
    
    def get_embedding(self, text: str) -> Optional[List[float]]:
        """Get embedding for text using Ollama"""
        try:
            response = self.ollama.embeddings(
                model=self.embedding_model,
                prompt=text
            )
            return response.get('embedding', [])
        except Exception as e:
            logger.error(f"Failed to get embedding: {e}")
            return None
    
    def index_knowledge(self, knowledge_id: int, title: str, content: str) -> bool:
        """Index a knowledge unit for vector search"""
        # Combine title and content for richer embedding
        full_text = f"{title}\n\n{content}"[:2000]  # Limit size
        
        embedding = self.get_embedding(full_text)
        if not embedding:
            return False
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    if self.has_pgvector:
                        # Store as native vector
                        cur.execute("""
                            INSERT INTO knowledge_vectors 
                            (knowledge_id, content, embedding)
                            VALUES (%s, %s, %s)
                            ON CONFLICT (knowledge_id) 
                            DO UPDATE SET content = EXCLUDED.content, 
                                        embedding = EXCLUDED.embedding
                        """, (knowledge_id, full_text, embedding))
                    else:
                        # Store as JSON
                        cur.execute("""
                            INSERT INTO knowledge_embeddings 
                            (knowledge_id, content, embedding)
                            VALUES (%s, %s, %s)
                            ON CONFLICT (knowledge_id) 
                            DO UPDATE SET content = EXCLUDED.content, 
                                        embedding = EXCLUDED.embedding
                        """, (knowledge_id, full_text, json.dumps(embedding)))
                    
                    return True
        except Exception as e:
            logger.error(f"Failed to index knowledge: {e}")
            return False
    
    def search(self, query: str, limit: int = 5, 
               category: Optional[str] = None) -> List[Dict]:
        """Search for similar knowledge using vector similarity"""
        query_embedding = self.get_embedding(query)
        if not query_embedding:
            # Fallback to text search
            return self._text_search(query, limit, category)
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor(cursor_factory=RealDictCursor) as cur:
                    if self.has_pgvector:
                        # Native vector similarity search
                        sql = """
                            SELECT 
                                k.id, k.title, k.content, k.category,
                                kv.embedding <=> %s::vector as distance,
                                1 - (kv.embedding <=> %s::vector) as similarity
                            FROM knowledge_units k
                            JOIN knowledge_vectors kv ON k.id = kv.knowledge_id
                            WHERE 1=1
                        """
                        params = [query_embedding, query_embedding]
                    else:
                        # Fallback: compute cosine similarity in Python
                        return self._json_vector_search(query_embedding, limit, category)
                    
                    if category:
                        sql += " AND k.category = %s"
                        params.append(category)
                    
                    sql += " ORDER BY distance LIMIT %s"
                    params.append(limit)
                    
                    cur.execute(sql, params)
                    results = cur.fetchall()
                    
                    return [dict(r) for r in results]
                    
        except Exception as e:
            logger.error(f"Vector search failed: {e}")
            return self._text_search(query, limit, category)
    
    def _json_vector_search(self, query_embedding: List[float], 
                           limit: int, category: Optional[str]) -> List[Dict]:
        """Search using JSON-stored embeddings"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor(cursor_factory=RealDictCursor) as cur:
                    # Fetch all embeddings (not scalable, but works for small datasets)
                    sql = """
                        SELECT 
                            k.id, k.title, k.content, k.category,
                            ke.embedding
                        FROM knowledge_units k
                        JOIN knowledge_embeddings ke ON k.id = ke.knowledge_id
                        WHERE 1=1
                    """
                    params = []
                    
                    if category:
                        sql += " AND k.category = %s"
                        params.append(category)
                    
                    cur.execute(sql, params)
                    all_results = cur.fetchall()
                    
                    # Compute similarities
                    query_vec = np.array(query_embedding)
                    results_with_sim = []
                    
                    for row in all_results:
                        doc_vec = np.array(row['embedding'])
                        # Cosine similarity
                        similarity = np.dot(query_vec, doc_vec) / (
                            np.linalg.norm(query_vec) * np.linalg.norm(doc_vec)
                        )
                        
                        result = dict(row)
                        result['similarity'] = float(similarity)
                        result['distance'] = 1 - similarity
                        del result['embedding']  # Remove large embedding
                        results_with_sim.append(result)
                    
                    # Sort by similarity and limit
                    results_with_sim.sort(key=lambda x: x['similarity'], reverse=True)
                    return results_with_sim[:limit]
                    
        except Exception as e:
            logger.error(f"JSON vector search failed: {e}")
            return []
    
    def _text_search(self, query: str, limit: int, 
                    category: Optional[str]) -> List[Dict]:
        """Fallback text-based similarity search"""
        sql = """
            SELECT 
                k.id, k.title, k.content, k.category,
                similarity(k.title || ' ' || k.content, %s) as similarity
            FROM knowledge_units k
            WHERE 1=1
        """
        params = [query]
        
        if category:
            sql += " AND k.category = %s"
            params.append(category)
        
        sql += """
            AND (k.title ILIKE %s OR k.content ILIKE %s 
                 OR similarity(k.title || ' ' || k.content, %s) > 0.1)
            ORDER BY similarity DESC
            LIMIT %s
        """
        
        params.extend([f'%{query}%', f'%{query}%', query, limit])
        
        return self.db.execute_query(sql, tuple(params))
    
    def reindex_all(self) -> Tuple[int, int]:
        """Reindex all knowledge units"""
        success_count = 0
        total_count = 0
        
        # Get all knowledge units
        knowledge_units = self.db.execute_query("""
            SELECT id, title, content 
            FROM knowledge_units 
            ORDER BY created_at DESC
        """)
        
        total_count = len(knowledge_units)
        
        for unit in knowledge_units:
            if self.index_knowledge(unit['id'], unit['title'], unit['content']):
                success_count += 1
        
        logger.info(f"Reindexed {success_count}/{total_count} knowledge units")
        return success_count, total_count
    
    def get_status(self) -> Dict:
        """Get vector search status"""
        status = {
            'enabled': True,
            'backend': 'pgvector' if self.has_pgvector else 'json_embeddings',
            'embedding_model': self.embedding_model,
            'embedding_dim': self.embedding_dim,
        }
        
        # Get indexed count
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    if self.has_pgvector:
                        cur.execute("SELECT COUNT(*) FROM knowledge_vectors")
                    else:
                        cur.execute("SELECT COUNT(*) FROM knowledge_embeddings")
                    
                    status['indexed_count'] = cur.fetchone()[0]
                    
                    # Get total knowledge units
                    cur.execute("SELECT COUNT(*) FROM knowledge_units")
                    status['total_count'] = cur.fetchone()[0]
                    
        except Exception as e:
            logger.error(f"Failed to get status: {e}")
            status['error'] = str(e)
        
        return status

# Singleton instance
_vector_search = None

def get_vector_search() -> VectorSearch:
    """Get or create vector search instance"""
    global _vector_search
    if _vector_search is None:
        _vector_search = VectorSearch()
    return _vector_search

# Integration functions
def search_knowledge_with_vectors(query: str, limit: int = 5, 
                                 category: Optional[str] = None) -> List[Dict]:
    """Search knowledge base using vector similarity"""
    vs = get_vector_search()
    return vs.search(query, limit, category)

def index_new_knowledge(knowledge_id: int, title: str, content: str) -> bool:
    """Index new knowledge for vector search"""
    vs = get_vector_search()
    return vs.index_knowledge(knowledge_id, title, content)

def get_vector_search_status() -> Dict:
    """Get current vector search status"""
    vs = get_vector_search()
    return vs.get_status()

if __name__ == "__main__":
    # Test vector search
    print("Testing vector search implementation...")
    vs = get_vector_search()
    status = vs.get_status()
    print(f"Status: {json.dumps(status, indent=2)}")
    
    # Try a search
    results = vs.search("error handling in Python", limit=3)
    print(f"\nSearch results: {len(results)} found")
    for r in results:
        print(f"- {r.get('title', 'No title')} (similarity: {r.get('similarity', 0):.3f})")