"""
Hybrid Search implementation for TuoKit RAG
Combines vector search with BM25 keyword search for better results
"""
import psycopg2
from psycopg2.extras import RealDictCursor
import numpy as np
from typing import List, Dict, Optional, Tuple
import logging
from collections import defaultdict
import math

logger = logging.getLogger(__name__)

class HybridSearcher:
    """Implements hybrid search combining vector and keyword search"""
    
    def __init__(self, connection_string: str):
        self.conn_string = connection_string
        self._setup_text_search()
    
    def _setup_text_search(self):
        """Setup full-text search capabilities in PostgreSQL"""
        try:
            with psycopg2.connect(self.conn_string) as conn:
                with conn.cursor() as cur:
                    # Add text search vector column if not exists
                    cur.execute("""
                        DO $$ 
                        BEGIN 
                            IF NOT EXISTS (
                                SELECT 1 FROM information_schema.columns 
                                WHERE table_name='knowledge_chunks' 
                                AND column_name='search_vector'
                            ) THEN
                                ALTER TABLE knowledge_chunks 
                                ADD COLUMN search_vector tsvector;
                            END IF;
                        END $$;
                    """)
                    
                    # Create GIN index for text search
                    cur.execute("""
                        CREATE INDEX IF NOT EXISTS idx_search_vector 
                        ON knowledge_chunks USING gin(search_vector);
                    """)
                    
                    # Update existing records to populate search vector
                    cur.execute("""
                        UPDATE knowledge_chunks 
                        SET search_vector = to_tsvector('english', content)
                        WHERE search_vector IS NULL;
                    """)
                    
                    # Create trigger to auto-update search vector
                    cur.execute("""
                        CREATE OR REPLACE FUNCTION update_search_vector()
                        RETURNS trigger AS $$
                        BEGIN
                            NEW.search_vector := to_tsvector('english', NEW.content);
                            RETURN NEW;
                        END
                        $$ LANGUAGE plpgsql;
                    """)
                    
                    cur.execute("""
                        DROP TRIGGER IF EXISTS update_search_vector_trigger 
                        ON knowledge_chunks;
                    """)
                    
                    cur.execute("""
                        CREATE TRIGGER update_search_vector_trigger
                        BEFORE INSERT OR UPDATE ON knowledge_chunks
                        FOR EACH ROW
                        EXECUTE FUNCTION update_search_vector();
                    """)
                    
                    conn.commit()
                    logger.info("Text search setup completed")
                    
        except Exception as e:
            logger.error(f"Error setting up text search: {e}")
    
    def vector_search(self, query_embedding: np.ndarray, 
                     top_k: int = 20,
                     source_filter: Optional[str] = None) -> List[Dict]:
        """Perform vector similarity search"""
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
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
                
                query += " ORDER BY embedding <=> %s::vector LIMIT %s"
                params.extend([query_embedding.tolist(), top_k])
                
                cur.execute(query, params)
                return cur.fetchall()
    
    def keyword_search(self, query: str, 
                      top_k: int = 20,
                      source_filter: Optional[str] = None) -> List[Dict]:
        """Perform BM25-like keyword search using PostgreSQL full-text search"""
        with psycopg2.connect(self.conn_string) as conn:
            with conn.cursor(cursor_factory=RealDictCursor) as cur:
                # Convert query to tsquery
                query_sql = """
                    SELECT id, content, metadata, source_type, source_path,
                           ts_rank_cd(search_vector, query) as rank
                    FROM knowledge_chunks,
                         plainto_tsquery('english', %s) query
                    WHERE search_vector @@ query
                """
                params = [query]
                
                if source_filter:
                    query_sql += " AND source_type = %s"
                    params.append(source_filter)
                
                query_sql += " ORDER BY rank DESC LIMIT %s"
                params.append(top_k)
                
                cur.execute(query_sql, params)
                results = cur.fetchall()
                
                # Normalize scores to 0-1 range
                if results:
                    max_rank = max(r['rank'] for r in results)
                    if max_rank > 0:
                        for result in results:
                            result['similarity'] = result['rank'] / max_rank
                    else:
                        for result in results:
                            result['similarity'] = 0
                
                return results
    
    def hybrid_search(self, query: str, 
                     query_embedding: np.ndarray,
                     top_k: int = 10,
                     source_filter: Optional[str] = None,
                     vector_weight: float = 0.5) -> List[Dict]:
        """
        Perform hybrid search combining vector and keyword search
        
        Args:
            query: Text query
            query_embedding: Query embedding vector
            top_k: Number of results to return
            source_filter: Optional source type filter
            vector_weight: Weight for vector search (0-1), keyword gets 1-vector_weight
        
        Returns:
            Combined and reranked results
        """
        # Get results from both searches
        vector_results = self.vector_search(
            query_embedding, 
            top_k=top_k * 2,  # Get more for better fusion
            source_filter=source_filter
        )
        
        keyword_results = self.keyword_search(
            query,
            top_k=top_k * 2,
            source_filter=source_filter
        )
        
        # Combine results using reciprocal rank fusion
        return self._reciprocal_rank_fusion(
            vector_results, 
            keyword_results,
            vector_weight,
            top_k
        )
    
    def _reciprocal_rank_fusion(self, 
                               vector_results: List[Dict],
                               keyword_results: List[Dict],
                               vector_weight: float,
                               top_k: int) -> List[Dict]:
        """
        Combine results using Reciprocal Rank Fusion (RRF)
        """
        # Create score dictionaries
        vector_scores = {}
        keyword_scores = {}
        all_docs = {}
        
        # Score vector results
        for rank, result in enumerate(vector_results):
            doc_id = result['id']
            # RRF score: 1 / (k + rank) where k=60 is a constant
            vector_scores[doc_id] = 1.0 / (60 + rank + 1)
            all_docs[doc_id] = result
        
        # Score keyword results
        for rank, result in enumerate(keyword_results):
            doc_id = result['id']
            keyword_scores[doc_id] = 1.0 / (60 + rank + 1)
            if doc_id not in all_docs:
                all_docs[doc_id] = result
        
        # Combine scores
        final_scores = {}
        for doc_id in all_docs:
            vector_score = vector_scores.get(doc_id, 0)
            keyword_score = keyword_scores.get(doc_id, 0)
            
            # Weighted combination
            final_scores[doc_id] = (
                vector_weight * vector_score + 
                (1 - vector_weight) * keyword_score
            )
        
        # Sort by combined score
        sorted_ids = sorted(final_scores.keys(), 
                           key=lambda x: final_scores[x], 
                           reverse=True)[:top_k]
        
        # Return results with combined scores
        results = []
        for doc_id in sorted_ids:
            result = all_docs[doc_id].copy()
            result['hybrid_score'] = final_scores[doc_id]
            result['vector_score'] = vector_scores.get(doc_id, 0)
            result['keyword_score'] = keyword_scores.get(doc_id, 0)
            results.append(result)
        
        return results
    
    def explain_search(self, query: str, 
                      query_embedding: np.ndarray,
                      top_k: int = 5) -> Dict:
        """
        Perform search and explain why results were returned
        Useful for debugging and tuning
        """
        # Get individual search results
        vector_results = self.vector_search(query_embedding, top_k=top_k)
        keyword_results = self.keyword_search(query, top_k=top_k)
        hybrid_results = self.hybrid_search(query, query_embedding, top_k=top_k)
        
        explanation = {
            'query': query,
            'vector_results': [
                {
                    'content': r['content'][:100] + '...',
                    'similarity': r['similarity'],
                    'source': r['source_path']
                } for r in vector_results
            ],
            'keyword_results': [
                {
                    'content': r['content'][:100] + '...',
                    'similarity': r['similarity'],
                    'source': r['source_path']
                } for r in keyword_results
            ],
            'hybrid_results': [
                {
                    'content': r['content'][:100] + '...',
                    'hybrid_score': r['hybrid_score'],
                    'vector_contribution': r['vector_score'],
                    'keyword_contribution': r['keyword_score'],
                    'source': r['source_path']
                } for r in hybrid_results
            ]
        }
        
        return explanation


# Reranker for even better results
class SimpleReranker:
    """Simple cross-encoder style reranker"""
    
    def __init__(self):
        # In production, use a real cross-encoder model
        # For now, we'll use a simple heuristic
        pass
    
    def rerank(self, query: str, results: List[Dict], top_k: int = 10) -> List[Dict]:
        """
        Rerank results based on query-document relevance
        """
        reranked = []
        
        for result in results:
            # Simple relevance scoring based on exact matches
            content_lower = result['content'].lower()
            query_lower = query.lower()
            
            # Count exact word matches
            query_words = set(query_lower.split())
            content_words = set(content_lower.split())
            
            exact_matches = len(query_words.intersection(content_words))
            
            # Check for phrase matches
            phrase_match = 1.0 if query_lower in content_lower else 0.0
            
            # Calculate relevance score
            relevance = (
                0.3 * result.get('hybrid_score', result.get('similarity', 0)) +
                0.4 * (exact_matches / max(len(query_words), 1)) +
                0.3 * phrase_match
            )
            
            result_copy = result.copy()
            result_copy['relevance_score'] = relevance
            reranked.append(result_copy)
        
        # Sort by relevance
        reranked.sort(key=lambda x: x['relevance_score'], reverse=True)
        
        return reranked[:top_k]


# Example usage
if __name__ == "__main__":
    # Test hybrid search
    searcher = HybridSearcher("postgresql://localhost:5432/tuokit")
    
    # Example query
    query = "crash analyzer error handling"
    
    # In real usage, get embedding from the embedding model
    # query_embedding = embedder.encode(query)
    
    # For testing, use random embedding
    query_embedding = np.random.randn(384)
    
    # Perform hybrid search
    results = searcher.hybrid_search(
        query=query,
        query_embedding=query_embedding,
        top_k=5,
        vector_weight=0.6  # Slightly favor vector search
    )
    
    print(f"Hybrid search results for: '{query}'")
    for i, result in enumerate(results):
        print(f"\n{i+1}. Score: {result['hybrid_score']:.3f}")
        print(f"   Vector: {result['vector_score']:.3f}, Keyword: {result['keyword_score']:.3f}")
        print(f"   Source: {result['source_path']}")
        print(f"   Content: {result['content'][:150]}...")