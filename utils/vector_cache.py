"""
Vector Search Cache and Performance Optimization
Implements LRU cache for search results and pre-computed similarity indices
"""

import json
import time
import hashlib
from typing import Dict, List, Tuple, Optional, Any
from collections import OrderedDict
import numpy as np
import logging
from datetime import datetime, timedelta
from utils.vector_constants import (
    DEFAULT_SEARCH_LIMIT,
    DEFAULT_SIMILARITY_THRESHOLD
)

logger = logging.getLogger(__name__)

class VectorSearchCache:
    """
    LRU cache for vector search results with TTL
    Reduces database queries for repeated searches
    """
    
    def __init__(self, max_size: int = 100, ttl_minutes: int = 30):
        self.max_size = max_size
        self.ttl = timedelta(minutes=ttl_minutes)
        self.cache = OrderedDict()
        self.hit_count = 0
        self.miss_count = 0
    
    def _get_cache_key(self, query_embedding: np.ndarray, model_name: str, 
                      threshold: float, limit: int) -> str:
        """Generate cache key from search parameters"""
        # Use hash of embedding for key
        embedding_hash = hashlib.md5(query_embedding.tobytes()).hexdigest()
        return f"{embedding_hash}:{model_name}:{threshold}:{limit}"
    
    def get(self, query_embedding: np.ndarray, model_name: str,
            threshold: float, limit: int) -> Optional[List[Tuple[int, float]]]:
        """Get cached search results if available and not expired"""
        key = self._get_cache_key(query_embedding, model_name, threshold, limit)
        
        if key in self.cache:
            entry = self.cache[key]
            if datetime.now() - entry['timestamp'] < self.ttl:
                # Move to end (most recently used)
                self.cache.move_to_end(key)
                self.hit_count += 1
                logger.debug(f"Cache hit for key {key[:16]}...")
                return entry['results']
            else:
                # Expired - remove it
                del self.cache[key]
        
        self.miss_count += 1
        return None
    
    def put(self, query_embedding: np.ndarray, model_name: str,
            threshold: float, limit: int, results: List[Tuple[int, float]]):
        """Cache search results"""
        key = self._get_cache_key(query_embedding, model_name, threshold, limit)
        
        # Remove oldest if at capacity
        if len(self.cache) >= self.max_size:
            self.cache.popitem(last=False)
        
        self.cache[key] = {
            'results': results,
            'timestamp': datetime.now()
        }
        logger.debug(f"Cached results for key {key[:16]}...")
    
    def clear(self):
        """Clear all cached results"""
        self.cache.clear()
        self.hit_count = 0
        self.miss_count = 0
    
    def get_stats(self) -> Dict[str, Any]:
        """Get cache statistics"""
        total_requests = self.hit_count + self.miss_count
        hit_rate = (self.hit_count / total_requests * 100) if total_requests > 0 else 0
        
        return {
            'size': len(self.cache),
            'max_size': self.max_size,
            'hit_count': self.hit_count,
            'miss_count': self.miss_count,
            'hit_rate': hit_rate,
            'ttl_minutes': self.ttl.total_seconds() / 60
        }


class VectorIndexManager:
    """
    Manages pre-computed similarity indices for faster search
    Creates and maintains sorted indices by embedding similarity
    """
    
    def __init__(self, db_manager):
        self.db = db_manager
        self._ensure_index_tables()
        self.indices = {}  # model_name -> sorted index
    
    def _ensure_index_tables(self):
        """Create index tables if they don't exist"""
        try:
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                
                # Table for storing pre-computed indices
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS vector_indices (
                        id SERIAL PRIMARY KEY,
                        model_name VARCHAR(100) UNIQUE,
                        index_data JSONB,
                        embedding_count INTEGER,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                """)
                
                # Table for tracking index updates
                cursor.execute("""
                    CREATE TABLE IF NOT EXISTS vector_index_updates (
                        id SERIAL PRIMARY KEY,
                        model_name VARCHAR(100),
                        last_knowledge_id INTEGER,
                        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                """)
                
                conn.commit()
                logger.info("Vector index tables initialized")
        except Exception as e:
            logger.error(f"Failed to create index tables: {e}")
    
    def needs_update(self, model_name: str) -> bool:
        """Check if index needs updating based on new embeddings"""
        try:
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                
                # Get last indexed knowledge ID
                cursor.execute("""
                    SELECT last_knowledge_id 
                    FROM vector_index_updates 
                    WHERE model_name = %s
                """, (model_name,))
                
                result = cursor.fetchone()
                last_indexed_id = result[0] if result else 0
                
                # Check if there are new embeddings
                cursor.execute("""
                    SELECT MAX(knowledge_id) 
                    FROM knowledge_embeddings 
                    WHERE model_name = %s
                """, (model_name,))
                
                result = cursor.fetchone()
                max_id = result[0] if result and result[0] else 0
                
                return max_id > last_indexed_id
                
        except Exception as e:
            logger.error(f"Error checking index update status: {e}")
            return True
    
    def build_index(self, model_name: str, force: bool = False) -> bool:
        """Build or update similarity index for a model"""
        if not force and not self.needs_update(model_name):
            logger.info(f"Index for {model_name} is up to date")
            return True
        
        try:
            logger.info(f"Building index for {model_name}...")
            start_time = time.time()
            
            # Get all embeddings for clustering
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                
                # Get embeddings with metadata
                cursor.execute("""
                    SELECT ke.knowledge_id, ke.embedding_json, k.category
                    FROM knowledge_embeddings ke
                    JOIN knowledge_units k ON ke.knowledge_id = k.id
                    WHERE ke.model_name = %s
                    ORDER BY ke.knowledge_id
                """, (model_name,))
                
                embeddings_data = []
                for knowledge_id, embedding_json, category in cursor:
                    embeddings_data.append({
                        'id': knowledge_id,
                        'embedding': json.loads(embedding_json),
                        'category': category
                    })
                
                if not embeddings_data:
                    logger.warning(f"No embeddings found for {model_name}")
                    return False
                
                # Create index structure (could implement more sophisticated indexing)
                index_data = {
                    'embedding_count': len(embeddings_data),
                    'categories': list(set(item['category'] for item in embeddings_data if item['category'])),
                    'id_list': [item['id'] for item in embeddings_data]
                }
                
                # Store index
                cursor.execute("""
                    INSERT INTO vector_indices (model_name, index_data, embedding_count)
                    VALUES (%s, %s, %s)
                    ON CONFLICT (model_name) 
                    DO UPDATE SET 
                        index_data = EXCLUDED.index_data,
                        embedding_count = EXCLUDED.embedding_count,
                        updated_at = CURRENT_TIMESTAMP
                """, (model_name, json.dumps(index_data), len(embeddings_data)))
                
                # Update tracking
                max_id = max(item['id'] for item in embeddings_data)
                cursor.execute("""
                    INSERT INTO vector_index_updates (model_name, last_knowledge_id)
                    VALUES (%s, %s)
                    ON CONFLICT (model_name)
                    DO UPDATE SET 
                        last_knowledge_id = EXCLUDED.last_knowledge_id,
                        updated_at = CURRENT_TIMESTAMP
                """, (model_name, max_id))
                
                conn.commit()
                
                elapsed = time.time() - start_time
                logger.info(f"Index built for {model_name} in {elapsed:.2f}s")
                return True
                
        except Exception as e:
            logger.error(f"Failed to build index: {e}")
            return False
    
    def get_index_stats(self, model_name: str) -> Dict[str, Any]:
        """Get statistics about an index"""
        try:
            with self.db.get_connection() as conn:
                cursor = conn.cursor()
                
                cursor.execute("""
                    SELECT index_data, embedding_count, created_at, updated_at
                    FROM vector_indices
                    WHERE model_name = %s
                """, (model_name,))
                
                result = cursor.fetchone()
                if result:
                    index_data, count, created, updated = result
                    return {
                        'exists': True,
                        'embedding_count': count,
                        'created_at': created,
                        'updated_at': updated,
                        'categories': index_data.get('categories', [])
                    }
                
                return {'exists': False}
                
        except Exception as e:
            logger.error(f"Error getting index stats: {e}")
            return {'exists': False, 'error': str(e)}


class OptimizedVectorSearch:
    """
    Optimized vector search with caching and indexing
    """
    
    def __init__(self, vector_store, cache_size: int = 100, cache_ttl: int = 30):
        self.vector_store = vector_store
        self.cache = VectorSearchCache(cache_size, cache_ttl)
        self.index_manager = VectorIndexManager(vector_store.db)
    
    def search_similar(self, query_embedding: np.ndarray, model_name: str,
                      threshold: float = None, limit: int = None) -> List[Tuple[int, float]]:
        """Search with caching and optimization"""
        # Use defaults
        if threshold is None:
            threshold = DEFAULT_SIMILARITY_THRESHOLD
        if limit is None:
            limit = DEFAULT_SEARCH_LIMIT
        
        # Check cache first
        cached_results = self.cache.get(query_embedding, model_name, threshold, limit)
        if cached_results is not None:
            return cached_results
        
        # Perform actual search
        results = self.vector_store.search_similar(
            query_embedding, model_name, threshold, limit
        )
        
        # Cache results
        self.cache.put(query_embedding, model_name, threshold, limit, results)
        
        return results
    
    def build_indices(self, model_names: List[str] = None):
        """Build indices for all or specified models"""
        if model_names is None:
            # Get all models with embeddings
            with self.vector_store.db.get_connection() as conn:
                cursor = conn.cursor()
                cursor.execute("""
                    SELECT DISTINCT model_name 
                    FROM knowledge_embeddings
                """)
                model_names = [row[0] for row in cursor.fetchall()]
        
        for model_name in model_names:
            self.index_manager.build_index(model_name)
    
    def get_performance_stats(self) -> Dict[str, Any]:
        """Get performance statistics"""
        return {
            'cache_stats': self.cache.get_stats(),
            'index_stats': {
                model: self.index_manager.get_index_stats(model)
                for model in ['nomic-embed-text']  # Add other models as needed
            }
        }
    
    def clear_cache(self):
        """Clear the search cache"""
        self.cache.clear()
        logger.info("Vector search cache cleared")