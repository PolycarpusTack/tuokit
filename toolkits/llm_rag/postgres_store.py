"""
PostgreSQL Vector Store Integration
Leverages existing TuoKit Postgres instance with pgvector
"""
import psycopg2
from psycopg2.extras import RealDictCursor
import numpy as np
from typing import List, Dict, Optional, Tuple
import json
from datetime import datetime
import logging

logger = logging.getLogger(__name__)

class PostgresVectorStore:
    """Manages vector storage in existing Postgres instance"""
    
    def __init__(self, connection_string: str = None):
        self.conn_string = connection_string or self._get_default_connection()
        self.dimension = 384  # Default for all-MiniLM-L6-v2
        self._init_db()
    
    def _get_default_connection(self) -> str:
        """Get connection from TuoKit config"""
        # TODO: Read from TuoKit main config
        return "postgresql://user:password@localhost:5432/tuokit"
    
    def _init_db(self):
        """Initialize pgvector and create tables"""
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor() as cur:
                # Enable pgvector extension
                cur.execute("CREATE EXTENSION IF NOT EXISTS vector")
                
                # Create knowledge chunks table
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS knowledge_chunks (
                        id SERIAL PRIMARY KEY,
                        content TEXT NOT NULL,
                        embedding vector(%s),
                        metadata JSONB DEFAULT '{}',
                        source_type VARCHAR(50),
                        source_path TEXT,
                        chunk_index INTEGER,
                        created_at TIMESTAMP DEFAULT NOW(),
                        updated_at TIMESTAMP DEFAULT NOW()
                    )
                """, (self.dimension,))
                
                # Create indexes for performance
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_embedding_vector 
                    ON knowledge_chunks USING ivfflat (embedding vector_cosine_ops)
                    WITH (lists = 100)
                """)
                
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_source_type 
                    ON knowledge_chunks(source_type)
                """)
                
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_metadata 
                    ON knowledge_chunks USING gin(metadata)
                """)
                
                # Create search history table for analytics
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS search_history (
                        id SERIAL PRIMARY KEY,
                        query TEXT NOT NULL,
                        results JSONB,
                        user_feedback INTEGER,
                        created_at TIMESTAMP DEFAULT NOW()
                    )
                """)
                
                conn.commit()
                logger.info("PostgreSQL vector store initialized")
    
    def add_chunks(self, chunks: List[Dict]) -> List[int]:
        """Add document chunks with embeddings"""
        chunk_ids = []
        
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor() as cur:
                for chunk in chunks:
                    cur.execute("""
                        INSERT INTO knowledge_chunks 
                        (content, embedding, metadata, source_type, source_path, chunk_index)
                        VALUES (%s, %s, %s, %s, %s, %s)
                        RETURNING id
                    """, (
                        chunk['content'],
                        chunk['embedding'],
                        json.dumps(chunk.get('metadata', {})),
                        chunk.get('source_type', 'unknown'),
                        chunk.get('source_path', ''),
                        chunk.get('chunk_index', 0)
                    ))
                    chunk_ids.append(cur.fetchone()[0])
                
                conn.commit()
        
        return chunk_ids
    
    def search(self, query_embedding: np.ndarray, 
               top_k: int = 10, 
               source_filter: Optional[str] = None,
               metadata_filter: Optional[Dict] = None) -> List[Dict]:
        """Vector similarity search with optional filters"""
        
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                # Build query with filters
                query = """
                    SELECT id, content, metadata, source_type, source_path,
                           1 - (embedding <=> %s::vector) as similarity
                    FROM knowledge_chunks
                    WHERE 1=1
                """
                params = [query_embedding.tolist()]
                
                if source_filter:
                    query += " AND source_type = %s"
                    params.append(source_filter)
                
                if metadata_filter:
                    query += " AND metadata @> %s"
                    params.append(json.dumps(metadata_filter))
                
                query += " ORDER BY embedding <=> %s::vector LIMIT %s"
                params.extend([query_embedding.tolist(), top_k])
                
                cur.execute(query, params)
                results = cur.fetchall()
                
                # Log search for analytics
                self._log_search(query_embedding, results)
                
                return results
    
    def _log_search(self, query_embedding, results):
        """Log searches for future analysis"""
        # TODO: Implement search logging
        pass
    
    def update_chunk(self, chunk_id: int, content: str, embedding: np.ndarray):
        """Update existing chunk"""
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor() as cur:
                cur.execute("""
                    UPDATE knowledge_chunks
                    SET content = %s, embedding = %s, updated_at = NOW()
                    WHERE id = %s
                """, (content, embedding.tolist(), chunk_id))
                conn.commit()
    
    def get_stats(self) -> Dict:
        """Get storage statistics"""
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                cur.execute("""
                    SELECT source_type, COUNT(*) as count
                    FROM knowledge_chunks
                    GROUP BY source_type
                """)
                return {"chunks_by_source": cur.fetchall()}
