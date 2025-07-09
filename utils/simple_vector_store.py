"""
Simple Vector Store for TuoKit
Uses PostgreSQL JSON columns - no pgvector required!
Practical solution that works with existing infrastructure
"""

import json
import numpy as np
from typing import List, Dict, Any, Optional, Tuple
from datetime import datetime
import logging
import heapq
from utils.vector_constants import (
    DEFAULT_EMBEDDING_MODEL,
    VECTOR_SEARCH_CHUNK_SIZE,
    MAX_TEXT_LENGTH,
    EMBEDDING_BATCH_SIZE,
    DEFAULT_SEARCH_LIMIT,
    DEFAULT_SIMILARITY_THRESHOLD
)

logger = logging.getLogger(__name__)

class SimpleVectorStore:
    """
    Vector storage using PostgreSQL JSON columns
    No external dependencies, just smart use of what we have
    """
    
    def __init__(self, db_manager, embedding_engine=None):
        self.db = db_manager
        self._embedding_engine = embedding_engine  # Store reference if provided
        self._ensure_tables()
    
    def _ensure_tables(self):
        """Create tables if they don't exist"""
        try:
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                
                # Simple embeddings table using JSON
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS knowledge_embeddings (
                        id SERIAL PRIMARY KEY,
                        knowledge_id INTEGER REFERENCES knowledge_units(id) ON DELETE CASCADE,
                        embedding_json TEXT,  -- JSON array of floats
                        model_name VARCHAR(100),
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        UNIQUE(knowledge_id, model_name)
                    )
                """)
                
                # Index for faster lookups
                cursor.execute("""
                    CREATE INDEX IF NOT EXISTS idx_knowledge_embeddings_knowledge_id 
                    ON knowledge_embeddings(knowledge_id)
                """)
                
                conn.commit()
                logger.info("Vector store tables initialized successfully")
        except Exception as e:
            logger.error(f"Failed to create vector store tables: {e}")
            raise RuntimeError(f"Vector store initialization failed: {e}")
    
    def store_embedding(self, knowledge_id: int, embedding: np.ndarray, model_name: str) -> bool:
        """Store embedding for a knowledge item"""
        try:
            # Validate inputs
            if not isinstance(knowledge_id, int) or knowledge_id <= 0:
                raise ValueError(f"Invalid knowledge_id: {knowledge_id}")
            
            if not isinstance(embedding, np.ndarray) or embedding.size == 0:
                raise ValueError("Invalid embedding: must be non-empty numpy array")
            
            embedding_list = embedding.tolist()
            embedding_json = json.dumps(embedding_list)
            
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                cursor.execute("""
                    INSERT INTO knowledge_embeddings (knowledge_id, embedding_json, model_name)
                    VALUES (%s, %s, %s)
                    ON CONFLICT (knowledge_id, model_name) 
                    DO UPDATE SET 
                        embedding_json = EXCLUDED.embedding_json,
                        created_at = CURRENT_TIMESTAMP
                """, (knowledge_id, embedding_json, model_name))
                conn.commit()
                return True
                
        except json.JSONEncodeError as e:
            logger.error(f"Failed to encode embedding to JSON: {e}")
            return False
        except Exception as e:
            logger.error(f"Failed to store embedding for knowledge {knowledge_id}: {e}")
            return False
    
    def get_embedding(self, knowledge_id: int, model_name: str) -> Optional[np.ndarray]:
        """Get embedding for a knowledge item"""
        try:
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                cursor.execute("""
                    SELECT embedding_json 
                    FROM knowledge_embeddings 
                    WHERE knowledge_id = %s AND model_name = %s
                """, (knowledge_id, model_name))
                
                result = cursor.fetchone()
                if result and result[0]:
                    try:
                        embedding_list = json.loads(result[0])
                        return np.array(embedding_list)
                    except (json.JSONDecodeError, ValueError) as e:
                        logger.error(f"Failed to decode embedding for knowledge {knowledge_id}: {e}")
                        return None
                return None
        except Exception as e:
            logger.error(f"Failed to get embedding for knowledge {knowledge_id}: {e}")
            return None
    
    def get_all_embeddings(self, model_name: str) -> List[Tuple[int, np.ndarray]]:
        """Get all embeddings for similarity search"""
        with self.db.get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute("""
                SELECT knowledge_id, embedding_json 
                FROM knowledge_embeddings 
                WHERE model_name = %s
            """, (model_name,))
            
            embeddings = []
            for knowledge_id, embedding_json in cursor.fetchall():
                embedding_list = json.loads(embedding_json)
                embeddings.append((knowledge_id, np.array(embedding_list)))
            
            return embeddings
    
    def search_similar(self, query_embedding: np.ndarray, 
                      model_name: str,
                      threshold: float = None,
                      limit: int = None) -> List[Tuple[int, float]]:
        """
        Search for similar knowledge items using chunked processing
        Processes embeddings in batches to avoid memory issues
        """
        # Use provided engine or create new one
        if self._embedding_engine:
            engine = self._embedding_engine
        else:
            # Import only when needed to avoid circular dependency
            from utils.embedding_engine import EmbeddingEngine
            engine = EmbeddingEngine()
        
        # Use default values from constants
        if threshold is None:
            threshold = DEFAULT_SIMILARITY_THRESHOLD
        if limit is None:
            limit = DEFAULT_SEARCH_LIMIT
        
        # Use heap to maintain top K results efficiently
        top_results = []  # Min heap of (-similarity, knowledge_id)
        
        # Process embeddings in chunks to avoid loading all into memory
        chunk_size = VECTOR_SEARCH_CHUNK_SIZE
        offset = 0
        
        with self.db.get_connection() as conn:
            cursor = conn.cursor()
            
            while True:
                # Fetch a chunk of embeddings
                cursor.execute("""
                    SELECT knowledge_id, embedding_json 
                    FROM knowledge_embeddings 
                    WHERE model_name = %s
                    ORDER BY knowledge_id
                    LIMIT %s OFFSET %s
                """, (model_name, chunk_size, offset))
                
                chunk = cursor.fetchall()
                if not chunk:
                    break
                
                # Process this chunk
                for knowledge_id, embedding_json in chunk:
                    try:
                        embedding = np.array(json.loads(embedding_json))
                        similarity = engine.cosine_similarity(query_embedding, embedding)
                        
                        if similarity >= threshold:
                            # Use negative similarity for min heap
                            if len(top_results) < limit:
                                heapq.heappush(top_results, (-similarity, knowledge_id))
                            elif -similarity > top_results[0][0]:
                                heapq.heapreplace(top_results, (-similarity, knowledge_id))
                    except Exception as e:
                        logger.warning(f"Error processing embedding for {knowledge_id}: {e}")
                
                offset += chunk_size
        
        # Convert heap to sorted list
        results = []
        while top_results:
            neg_similarity, knowledge_id = heapq.heappop(top_results)
            results.append((knowledge_id, -neg_similarity))
        
        # Results are in ascending order from heap, reverse for descending
        results.reverse()
        
        return results
    
    def update_all_embeddings(self, model_name: str = None, 
                            batch_size: int = None):
        """Update embeddings for all knowledge items"""
        # Use defaults from constants
        if model_name is None:
            model_name = DEFAULT_EMBEDDING_MODEL
        if batch_size is None:
            batch_size = EMBEDDING_BATCH_SIZE
            
        # Use provided engine or create new one
        if self._embedding_engine and self._embedding_engine.model == model_name:
            engine = self._embedding_engine
        else:
            from utils.embedding_engine import EmbeddingEngine
            engine = EmbeddingEngine(model=model_name)
        
        # Get all knowledge items without embeddings
        with self.db.get_connection() as conn:
            cursor = conn.cursor()
            cursor.execute("""
                SELECT k.id, k.prompt, k.response 
                FROM knowledge_units k
                LEFT JOIN knowledge_embeddings e 
                ON k.id = e.knowledge_id AND e.model_name = %s
                WHERE e.id IS NULL
                LIMIT %s
            """, (model_name, batch_size))
            
            items_to_embed = cursor.fetchall()
        
        if not items_to_embed:
            logger.info("All knowledge items already have embeddings")
            return 0
        
        # Generate embeddings
        embedded_count = 0
        for knowledge_id, prompt, response in items_to_embed:
            try:
                # Combine prompt and response for richer embedding
                text = f"{prompt}\n\n{response}"[:MAX_TEXT_LENGTH]
                embedding = engine.embed_text(text)
                
                if embedding is not None and self.store_embedding(knowledge_id, embedding, model_name):
                    embedded_count += 1
                else:
                    logger.warning(f"Failed to store embedding for knowledge {knowledge_id}")
            except Exception as e:
                logger.error(f"Failed to embed knowledge {knowledge_id}: {e}")
        
        logger.info(f"Generated {embedded_count} new embeddings")
        return embedded_count
    
    def get_stats(self) -> Dict[str, Any]:
        """Get statistics about embeddings"""
        with self.db.get_connection() as conn:
            cursor = conn.cursor()
            
            # Total embeddings
            cursor.execute("SELECT COUNT(*) FROM knowledge_embeddings")
            total_embeddings = cursor.fetchone()[0]
            
            # Embeddings by model
            cursor.execute("""
                SELECT model_name, COUNT(*) 
                FROM knowledge_embeddings 
                GROUP BY model_name
            """)
            by_model = dict(cursor.fetchall())
            
            # Knowledge items without embeddings
            cursor.execute("""
                SELECT COUNT(*) 
                FROM knowledge_units k
                LEFT JOIN knowledge_embeddings e ON k.id = e.knowledge_id
                WHERE e.id IS NULL
            """)
            without_embeddings = cursor.fetchone()[0]
            
            # Calculate coverage safely
            total_knowledge = total_embeddings + without_embeddings
            coverage = 0.0
            if total_knowledge > 0:
                coverage = (total_embeddings / total_knowledge) * 100
            
            return {
                "total_embeddings": total_embeddings,
                "by_model": by_model,
                "knowledge_without_embeddings": without_embeddings,
                "coverage_percentage": coverage,
                "total_knowledge": total_knowledge
            }