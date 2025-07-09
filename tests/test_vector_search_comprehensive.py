"""
Comprehensive test suite for vector search implementation
Tests all components: embedding, storage, search, caching, and optimization
"""

import unittest
import numpy as np
import tempfile
import shutil
import json
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# Import components to test
from utils.embedding_engine import EmbeddingEngine
from utils.simple_vector_store import SimpleVectorStore
from utils.vector_cache import VectorSearchCache, VectorIndexManager, OptimizedVectorSearch
from utils.vector_constants import *
from utils.database import DatabaseManager


class TestEmbeddingEngine(unittest.TestCase):
    """Test embedding generation and caching"""
    
    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.engine = EmbeddingEngine(cache_dir=self.temp_dir)
    
    def tearDown(self):
        shutil.rmtree(self.temp_dir)
    
    def test_embedding_generation_fallback(self):
        """Test fallback embedding generation"""
        # Test with empty text
        embedding = self.engine.embed_text("")
        self.assertEqual(len(embedding), DEFAULT_EMBEDDING_DIM)
        self.assertTrue(np.all(embedding == 0))
        
        # Test with normal text (will use fallback if Ollama not available)
        text = "Test embedding generation"
        embedding = self.engine.embed_text(text)
        self.assertEqual(len(embedding), DEFAULT_EMBEDDING_DIM)
        self.assertIsInstance(embedding, np.ndarray)
    
    def test_cache_functionality(self):
        """Test JSON cache storage and retrieval"""
        text = "Test caching functionality"
        
        # First call - generates embedding
        embedding1 = self.engine.embed_text(text)
        
        # Second call - should use cache
        embedding2 = self.engine.embed_text(text)
        
        # Should be identical
        np.testing.assert_array_equal(embedding1, embedding2)
        
        # Verify cache file exists
        cache_key = self.engine._get_cache_key(text)
        cache_file = Path(self.temp_dir) / f"{cache_key}.json"
        self.assertTrue(cache_file.exists())
        
        # Verify cache format
        with open(cache_file, 'r') as f:
            cache_data = json.load(f)
            self.assertIn('embedding', cache_data)
            self.assertIn('model', cache_data)
            self.assertIn('version', cache_data)
            self.assertEqual(cache_data['version'], CACHE_VERSION)
    
    def test_corrupted_cache_handling(self):
        """Test handling of corrupted cache files"""
        text = "Test corrupted cache"
        cache_key = self.engine._get_cache_key(text)
        cache_file = Path(self.temp_dir) / f"{cache_key}.json"
        
        # Create corrupted cache
        with open(cache_file, 'w') as f:
            f.write("corrupted data")
        
        # Should handle gracefully and generate new embedding
        embedding = self.engine.embed_text(text)
        self.assertIsNotNone(embedding)
        self.assertEqual(len(embedding), DEFAULT_EMBEDDING_DIM)
        
        # Corrupted file should be removed
        self.assertFalse(cache_file.exists())
    
    def test_similarity_calculation(self):
        """Test cosine similarity calculation"""
        vec1 = np.array([1, 0, 0])
        vec2 = np.array([1, 0, 0])
        vec3 = np.array([0, 1, 0])
        vec4 = np.array([-1, 0, 0])
        
        # Identical vectors
        self.assertAlmostEqual(self.engine.cosine_similarity(vec1, vec2), 1.0)
        
        # Orthogonal vectors
        self.assertAlmostEqual(self.engine.cosine_similarity(vec1, vec3), 0.0)
        
        # Opposite vectors
        self.assertAlmostEqual(self.engine.cosine_similarity(vec1, vec4), -1.0)


class TestSimpleVectorStore(unittest.TestCase):
    """Test vector storage and search"""
    
    def setUp(self):
        # Mock database
        self.mock_db = Mock(spec=DatabaseManager)
        self.mock_db.connected = True
        self.mock_db.get_connection = MagicMock()
        
        # Create mock connection context
        self.mock_conn = MagicMock()
        self.mock_cursor = MagicMock()
        self.mock_conn.__enter__ = MagicMock(return_value=self.mock_conn)
        self.mock_conn.__exit__ = MagicMock(return_value=None)
        self.mock_conn.cursor = MagicMock(return_value=self.mock_cursor)
        self.mock_db.get_connection.return_value = self.mock_conn
        
        # Mock embedding engine
        self.mock_engine = Mock(spec=EmbeddingEngine)
        self.mock_engine.cosine_similarity = lambda a, b: np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))
        
        self.vector_store = SimpleVectorStore(self.mock_db, self.mock_engine)
    
    def test_store_embedding_validation(self):
        """Test embedding storage with validation"""
        # Valid embedding
        valid_embedding = np.array([0.1, 0.2, 0.3])
        result = self.vector_store.store_embedding(1, valid_embedding, "test-model")
        self.assertTrue(result)
        
        # Invalid knowledge_id
        result = self.vector_store.store_embedding(-1, valid_embedding, "test-model")
        self.assertFalse(result)
        
        # Empty embedding
        result = self.vector_store.store_embedding(1, np.array([]), "test-model")
        self.assertFalse(result)
    
    def test_chunked_search(self):
        """Test chunked vector search"""
        # Mock database responses
        self.mock_cursor.fetchall.side_effect = [
            # First chunk
            [(1, '[0.1, 0.2, 0.3]'), (2, '[0.4, 0.5, 0.6]')],
            # Second chunk
            [(3, '[0.7, 0.8, 0.9]')],
            # Empty chunk (end)
            []
        ]
        
        query_embedding = np.array([0.1, 0.2, 0.3])
        results = self.vector_store.search_similar(
            query_embedding, 
            "test-model",
            threshold=0.5,
            limit=2
        )
        
        # Should process all chunks and return top results
        self.assertIsInstance(results, list)
        self.assertTrue(len(results) <= 2)
    
    def test_stats_calculation(self):
        """Test statistics calculation with edge cases"""
        # Mock database responses
        self.mock_cursor.fetchone.side_effect = [
            (10,),  # total embeddings
            (5,),   # without embeddings
        ]
        self.mock_cursor.fetchall.return_value = [("model1", 5), ("model2", 5)]
        
        stats = self.vector_store.get_stats()
        
        self.assertEqual(stats['total_embeddings'], 10)
        self.assertEqual(stats['knowledge_without_embeddings'], 5)
        self.assertEqual(stats['total_knowledge'], 15)
        self.assertAlmostEqual(stats['coverage_percentage'], 66.67, places=1)
        
        # Test division by zero case
        self.mock_cursor.fetchone.side_effect = [(0,), (0,)]
        self.mock_cursor.fetchall.return_value = []
        
        stats = self.vector_store.get_stats()
        self.assertEqual(stats['coverage_percentage'], 0.0)


class TestVectorCache(unittest.TestCase):
    """Test caching functionality"""
    
    def setUp(self):
        self.cache = VectorSearchCache(max_size=3, ttl_minutes=30)
    
    def test_cache_operations(self):
        """Test basic cache operations"""
        embedding = np.array([0.1, 0.2, 0.3])
        results = [(1, 0.9), (2, 0.8)]
        
        # Cache miss
        self.assertIsNone(self.cache.get(embedding, "model", 0.5, 10))
        self.assertEqual(self.cache.miss_count, 1)
        
        # Put in cache
        self.cache.put(embedding, "model", 0.5, 10, results)
        
        # Cache hit
        cached = self.cache.get(embedding, "model", 0.5, 10)
        self.assertEqual(cached, results)
        self.assertEqual(self.cache.hit_count, 1)
        
        # Different parameters = cache miss
        self.assertIsNone(self.cache.get(embedding, "model", 0.6, 10))
    
    def test_cache_eviction(self):
        """Test LRU eviction"""
        # Fill cache to capacity
        for i in range(4):
            embedding = np.array([i, i, i])
            self.cache.put(embedding, "model", 0.5, 10, [(i, 0.9)])
        
        # First entry should be evicted
        first_embedding = np.array([0, 0, 0])
        self.assertIsNone(self.cache.get(first_embedding, "model", 0.5, 10))
        
        # Others should still be there
        second_embedding = np.array([1, 1, 1])
        self.assertIsNotNone(self.cache.get(second_embedding, "model", 0.5, 10))
    
    def test_cache_expiration(self):
        """Test TTL expiration"""
        from datetime import datetime, timedelta
        
        embedding = np.array([0.1, 0.2, 0.3])
        results = [(1, 0.9)]
        
        # Put in cache
        self.cache.put(embedding, "model", 0.5, 10, results)
        
        # Manually expire the entry
        key = self.cache._get_cache_key(embedding, "model", 0.5, 10)
        self.cache.cache[key]['timestamp'] = datetime.now() - timedelta(hours=1)
        
        # Should return None (expired)
        self.assertIsNone(self.cache.get(embedding, "model", 0.5, 10))
        
        # Entry should be removed
        self.assertNotIn(key, self.cache.cache)


class TestOptimizedSearch(unittest.TestCase):
    """Test optimized search with caching"""
    
    def setUp(self):
        # Mock components
        self.mock_db = Mock()
        self.mock_vector_store = Mock(spec=SimpleVectorStore)
        self.mock_vector_store.db = self.mock_db
        self.mock_vector_store.search_similar.return_value = [(1, 0.9), (2, 0.8)]
        
        self.optimized_search = OptimizedVectorSearch(self.mock_vector_store, cache_size=10)
    
    def test_cached_search(self):
        """Test search with caching"""
        embedding = np.array([0.1, 0.2, 0.3])
        
        # First search - cache miss
        results1 = self.optimized_search.search_similar(embedding, "model")
        self.assertEqual(len(results1), 2)
        self.mock_vector_store.search_similar.assert_called_once()
        
        # Second search - cache hit
        results2 = self.optimized_search.search_similar(embedding, "model")
        self.assertEqual(results1, results2)
        # Should not call underlying search again
        self.mock_vector_store.search_similar.assert_called_once()
        
        # Check cache stats
        stats = self.optimized_search.get_performance_stats()
        self.assertEqual(stats['cache_stats']['hit_count'], 1)
        self.assertEqual(stats['cache_stats']['miss_count'], 1)
        self.assertEqual(stats['cache_stats']['hit_rate'], 50.0)


class TestIntegration(unittest.TestCase):
    """Integration tests for the complete system"""
    
    @patch('utils.database.DatabaseManager')
    @patch('utils.ollama.get_ollama_manager')
    def test_end_to_end_search(self, mock_ollama, mock_db_class):
        """Test complete search flow"""
        # Setup mocks
        mock_db = Mock()
        mock_db.connected = True
        mock_db.get_connection = MagicMock()
        mock_db_class.return_value = mock_db
        
        # Create components
        engine = EmbeddingEngine()
        vector_store = SimpleVectorStore(mock_db, engine)
        optimized = OptimizedVectorSearch(vector_store)
        
        # Generate query embedding
        query = "test query"
        query_embedding = engine.embed_text(query)
        
        # Mock search results
        vector_store.search_similar = Mock(return_value=[(1, 0.9), (2, 0.8)])
        
        # Perform search
        results = optimized.search_similar(query_embedding, "test-model")
        
        # Verify results
        self.assertEqual(len(results), 2)
        self.assertEqual(results[0][0], 1)
        self.assertAlmostEqual(results[0][1], 0.9)
    
    def test_input_validation_chain(self):
        """Test input validation through the chain"""
        mock_db = Mock()
        mock_db.connected = True
        
        # Test with invalid inputs
        engine = EmbeddingEngine()
        
        # Empty text should return zero vector
        embedding = engine.embed_text("")
        self.assertTrue(np.all(embedding == 0))
        
        # Very long text should be handled
        long_text = "x" * 10000
        embedding = engine.embed_text(long_text)
        self.assertEqual(len(embedding), DEFAULT_EMBEDDING_DIM)


def run_all_tests():
    """Run all tests and return results"""
    unittest.main(verbosity=2, exit=False)


if __name__ == "__main__":
    run_all_tests()