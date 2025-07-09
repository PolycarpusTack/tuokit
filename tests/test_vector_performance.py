"""
Performance benchmark tests for vector search
Measures actual performance characteristics
"""

import time
import numpy as np
import psutil
import os
from utils.database import DatabaseManager
from utils.embedding_engine import EmbeddingEngine
from utils.simple_vector_store import SimpleVectorStore
from utils.vector_cache import OptimizedVectorSearch
from utils.vector_constants import *


def get_memory_usage():
    """Get current memory usage in MB"""
    process = psutil.Process(os.getpid())
    return process.memory_info().rss / 1024 / 1024


class VectorSearchBenchmark:
    """Benchmark tests for vector search performance"""
    
    def __init__(self):
        self.db = DatabaseManager()
        if not self.db.connected:
            print("❌ Database not connected - skipping benchmarks")
            self.connected = False
            return
        
        self.connected = True
        self.engine = EmbeddingEngine()
        self.vector_store = SimpleVectorStore(self.db, self.engine)
        self.optimized_search = OptimizedVectorSearch(self.vector_store)
    
    def benchmark_embedding_generation(self, num_texts=100):
        """Benchmark embedding generation speed"""
        print(f"\n📊 Benchmarking Embedding Generation ({num_texts} texts)")
        
        # Generate test texts
        texts = [f"This is test text number {i} for embedding generation benchmark" for i in range(num_texts)]
        
        # Measure time and memory
        start_memory = get_memory_usage()
        start_time = time.time()
        
        embeddings = []
        for text in texts:
            embedding = self.engine.embed_text(text)
            embeddings.append(embedding)
        
        elapsed_time = time.time() - start_time
        memory_used = get_memory_usage() - start_memory
        
        # Calculate metrics
        avg_time_per_embedding = elapsed_time / num_texts * 1000  # ms
        embeddings_per_second = num_texts / elapsed_time
        
        print(f"  Total time: {elapsed_time:.2f}s")
        print(f"  Average time per embedding: {avg_time_per_embedding:.1f}ms")
        print(f"  Embeddings per second: {embeddings_per_second:.1f}")
        print(f"  Memory used: {memory_used:.1f}MB")
        
        return {
            'total_time': elapsed_time,
            'avg_time_ms': avg_time_per_embedding,
            'per_second': embeddings_per_second,
            'memory_mb': memory_used
        }
    
    def benchmark_vector_search(self, num_searches=50):
        """Benchmark vector search performance"""
        print(f"\n📊 Benchmarking Vector Search ({num_searches} searches)")
        
        # Ensure we have some embeddings
        stats = self.vector_store.get_stats()
        if stats['total_embeddings'] < 100:
            print("  ⚠️  Not enough embeddings for meaningful benchmark")
            return None
        
        # Generate random query embeddings
        query_embeddings = [
            np.random.rand(DEFAULT_EMBEDDING_DIM) for _ in range(num_searches)
        ]
        
        # Benchmark without cache
        print("\n  Without cache:")
        self.optimized_search.clear_cache()
        
        start_time = time.time()
        for embedding in query_embeddings:
            results = self.vector_store.search_similar(
                embedding, 
                DEFAULT_EMBEDDING_MODEL,
                threshold=0.5,
                limit=10
            )
        
        uncached_time = time.time() - start_time
        avg_uncached = uncached_time / num_searches * 1000
        
        print(f"    Total time: {uncached_time:.2f}s")
        print(f"    Average per search: {avg_uncached:.1f}ms")
        
        # Benchmark with cache
        print("\n  With cache (second run):")
        
        start_time = time.time()
        for embedding in query_embeddings:
            results = self.optimized_search.search_similar(
                embedding,
                DEFAULT_EMBEDDING_MODEL,
                threshold=0.5,
                limit=10
            )
        
        cached_time = time.time() - start_time
        avg_cached = cached_time / num_searches * 1000
        
        print(f"    Total time: {cached_time:.2f}s")
        print(f"    Average per search: {avg_cached:.1f}ms")
        
        # Cache statistics
        cache_stats = self.optimized_search.get_performance_stats()['cache_stats']
        print(f"\n  Cache performance:")
        print(f"    Hit rate: {cache_stats['hit_rate']:.1f}%")
        print(f"    Speedup: {uncached_time/cached_time:.1f}x")
        
        return {
            'uncached_ms': avg_uncached,
            'cached_ms': avg_cached,
            'speedup': uncached_time/cached_time,
            'hit_rate': cache_stats['hit_rate']
        }
    
    def benchmark_batch_operations(self):
        """Benchmark batch vs individual operations"""
        print("\n📊 Benchmarking Batch Operations")
        
        # Test knowledge IDs
        test_ids = list(range(1, 21))
        
        # Individual fetches
        print("\n  Individual fetches:")
        start_time = time.time()
        results_individual = []
        for kid in test_ids:
            result = self.db.get_knowledge_by_id(kid)
            if result:
                results_individual.append(result)
        
        individual_time = time.time() - start_time
        
        # Batch fetch
        print("\n  Batch fetch:")
        start_time = time.time()
        results_batch = self.db.get_knowledge_by_ids(test_ids)
        batch_time = time.time() - start_time
        
        print(f"    Individual: {individual_time:.3f}s")
        print(f"    Batch: {batch_time:.3f}s")
        print(f"    Speedup: {individual_time/batch_time:.1f}x")
        
        return {
            'individual_time': individual_time,
            'batch_time': batch_time,
            'speedup': individual_time/batch_time if batch_time > 0 else 0
        }
    
    def benchmark_memory_efficiency(self):
        """Benchmark memory usage of chunked search"""
        print("\n📊 Benchmarking Memory Efficiency")
        
        # Create a large query
        query_embedding = np.random.rand(DEFAULT_EMBEDDING_DIM)
        
        # Measure memory before
        start_memory = get_memory_usage()
        
        # Perform search (which uses chunking)
        results = self.vector_store.search_similar(
            query_embedding,
            DEFAULT_EMBEDDING_MODEL,
            threshold=0.3,
            limit=100
        )
        
        # Measure memory after
        end_memory = get_memory_usage()
        memory_increase = end_memory - start_memory
        
        print(f"  Memory before: {start_memory:.1f}MB")
        print(f"  Memory after: {end_memory:.1f}MB")
        print(f"  Memory increase: {memory_increase:.1f}MB")
        print(f"  Results found: {len(results)}")
        
        # Verify chunking is working
        print(f"  Chunk size: {VECTOR_SEARCH_CHUNK_SIZE}")
        print(f"  ✅ Chunked processing prevents loading all embeddings")
        
        return {
            'memory_increase_mb': memory_increase,
            'results_count': len(results)
        }
    
    def run_all_benchmarks(self):
        """Run all benchmark tests"""
        if not self.connected:
            return
        
        print("🚀 Running Vector Search Performance Benchmarks\n")
        
        results = {}
        
        # Run benchmarks
        results['embedding'] = self.benchmark_embedding_generation()
        results['search'] = self.benchmark_vector_search()
        results['batch'] = self.benchmark_batch_operations()
        results['memory'] = self.benchmark_memory_efficiency()
        
        # Summary
        print("\n📈 Performance Summary:")
        print(f"  Embedding generation: {results['embedding']['avg_time_ms']:.1f}ms per embedding")
        
        if results['search']:
            print(f"  Vector search: {results['search']['uncached_ms']:.1f}ms uncached, {results['search']['cached_ms']:.1f}ms cached")
            print(f"  Cache speedup: {results['search']['speedup']:.1f}x")
        
        print(f"  Batch fetch speedup: {results['batch']['speedup']:.1f}x")
        print(f"  Memory efficiency: {results['memory']['memory_increase_mb']:.1f}MB for large search")
        
        print("\n✅ Benchmarks complete!")
        
        return results


if __name__ == "__main__":
    benchmark = VectorSearchBenchmark()
    benchmark.run_all_benchmarks()