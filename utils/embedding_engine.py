"""
TuoKit Embedding Engine
Simple, practical vector embeddings using Ollama
Following the TuoKit philosophy: Build fast, build smart, build exactly what's needed
"""

import json
import numpy as np
from typing import List, Dict, Any, Optional, Tuple
from utils.ollama import get_ollama_manager
import hashlib
from pathlib import Path
import logging
import requests
from utils.vector_constants import (
    DEFAULT_EMBEDDING_DIM,
    FALLBACK_EMBEDDING_DIM,
    DEFAULT_EMBEDDING_MODEL,
    CACHE_VERSION,
    EMBEDDING_BATCH_SIZE
)

logger = logging.getLogger(__name__)

class EmbeddingEngine:
    """
    Lightweight embedding engine using Ollama models
    No external dependencies, works with what we have
    """
    
    def __init__(self, model: str = None, cache_dir: str = "./embeddings_cache"):
        self.model = model or DEFAULT_EMBEDDING_MODEL
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(exist_ok=True)
        self.embedding_dim = None  # Will be set on first embedding
        self.ollama_manager = get_ollama_manager()
        
    def _get_cache_key(self, text: str) -> str:
        """Generate cache key for text"""
        return hashlib.md5(f"{self.model}:{text}".encode()).hexdigest()
    
    def _load_from_cache(self, cache_key: str) -> Optional[np.ndarray]:
        """Load embedding from cache using secure JSON format"""
        cache_file = self.cache_dir / f"{cache_key}.json"
        if cache_file.exists():
            try:
                with open(cache_file, 'r') as f:
                    data = json.load(f)
                    # Validate data structure
                    if isinstance(data, dict) and 'embedding' in data and 'model' in data:
                        if data['model'] == self.model:
                            return np.array(data['embedding'])
            except (json.JSONDecodeError, ValueError) as e:
                logger.warning(f"Invalid cache file {cache_file}: {e}")
                # Remove corrupted cache file
                try:
                    cache_file.unlink()
                except:
                    pass
        return None
    
    def _save_to_cache(self, cache_key: str, embedding: np.ndarray):
        """Save embedding to cache using secure JSON format"""
        cache_file = self.cache_dir / f"{cache_key}.json"
        try:
            # Store with metadata for validation
            cache_data = {
                'embedding': embedding.tolist(),
                'model': self.model,
                'dimension': len(embedding),
                'version': CACHE_VERSION
            }
            with open(cache_file, 'w') as f:
                json.dump(cache_data, f)
        except Exception as e:
            logger.warning(f"Failed to cache embedding: {e}")
    
    def embed_text(self, text: str, use_cache: bool = True) -> np.ndarray:
        """
        Generate embedding for text using Ollama
        Falls back to simple TF-IDF style features if Ollama fails
        """
        if not text:
            return np.zeros(DEFAULT_EMBEDDING_DIM)
        
        # Check cache
        cache_key = self._get_cache_key(text)
        if use_cache:
            cached = self._load_from_cache(cache_key)
            if cached is not None:
                return cached
        
        try:
            # Try Ollama embedding API
            # Using the REST API directly for embeddings
            response = requests.post(
                f"{self.ollama_manager.host}/api/embeddings",
                json={
                    "model": self.model,
                    "prompt": text
                }
            )
            
            if response.status_code == 200:
                data = response.json()
                if "embedding" in data:
                    embedding = np.array(data["embedding"])
                else:
                    # Fallback: Generate pseudo-embedding from text features
                    embedding = self._generate_simple_embedding(text)
            else:
                # Fallback: Generate pseudo-embedding from text features
                logger.warning(f"Ollama embedding API returned {response.status_code}")
                embedding = self._generate_simple_embedding(text)
                
        except Exception as e:
            logger.warning(f"Ollama embedding failed, using fallback: {e}")
            embedding = self._generate_simple_embedding(text)
        
        # Cache the result
        if use_cache:
            self._save_to_cache(cache_key, embedding)
        
        # Set embedding dimension
        if self.embedding_dim is None:
            self.embedding_dim = len(embedding)
            
        return embedding
    
    def _generate_simple_embedding(self, text: str, dim: int = None) -> np.ndarray:
        """
        Generate simple embedding based on text features
        This is our fallback when Ollama isn't available
        """
        if dim is None:
            dim = FALLBACK_EMBEDDING_DIM
            
        # Simple but effective features
        features = []
        
        # Character-level features
        features.append(len(text) / 1000.0)  # Normalized length
        features.append(text.count(' ') / 100.0)  # Word count estimate
        features.append(text.count('\n') / 50.0)  # Line count
        
        # Keyword features (common programming terms)
        keywords = ['error', 'exception', 'function', 'class', 'method', 'return', 
                   'import', 'def', 'if', 'for', 'while', 'try', 'catch', 'sql', 
                   'select', 'from', 'where', 'join', 'table', 'database']
        
        for keyword in keywords:
            features.append(text.lower().count(keyword) / 10.0)
        
        # Hash-based features for remaining dimensions
        text_hash = hashlib.sha256(text.encode()).digest()
        hash_features = [b / 255.0 for b in text_hash[:dim - len(features)]]
        features.extend(hash_features)
        
        # Ensure correct dimension
        if len(features) < dim:
            features.extend([0.0] * (dim - len(features)))
        elif len(features) > dim:
            features = features[:dim]
            
        return np.array(features)
    
    def cosine_similarity(self, vec1: np.ndarray, vec2: np.ndarray) -> float:
        """Calculate cosine similarity between two vectors"""
        dot_product = np.dot(vec1, vec2)
        norm1 = np.linalg.norm(vec1)
        norm2 = np.linalg.norm(vec2)
        
        if norm1 == 0 or norm2 == 0:
            return 0.0
            
        return dot_product / (norm1 * norm2)
    
    def find_similar(self, query_embedding: np.ndarray, 
                    embeddings: List[Tuple[int, np.ndarray]], 
                    threshold: float = 0.5,
                    top_k: int = 10) -> List[Tuple[int, float]]:
        """
        Find similar embeddings
        Returns list of (id, similarity_score) tuples
        """
        similarities = []
        
        for item_id, embedding in embeddings:
            similarity = self.cosine_similarity(query_embedding, embedding)
            if similarity >= threshold:
                similarities.append((item_id, similarity))
        
        # Sort by similarity descending
        similarities.sort(key=lambda x: x[1], reverse=True)
        
        return similarities[:top_k]
    
    def batch_embed(self, texts: List[str], batch_size: int = None) -> List[np.ndarray]:
        """Embed multiple texts efficiently"""
        if batch_size is None:
            batch_size = EMBEDDING_BATCH_SIZE
            
        embeddings = []
        
        for i in range(0, len(texts), batch_size):
            batch = texts[i:i + batch_size]
            for text in batch:
                embeddings.append(self.embed_text(text))
                
        return embeddings
    
    def cleanup_old_pickle_cache(self):
        """Clean up old pickle cache files for security"""
        cleaned = 0
        for pkl_file in self.cache_dir.glob("*.pkl"):
            try:
                pkl_file.unlink()
                cleaned += 1
            except Exception as e:
                logger.warning(f"Failed to remove {pkl_file}: {e}")
        
        if cleaned > 0:
            logger.info(f"Cleaned up {cleaned} old pickle cache files")
        return cleaned