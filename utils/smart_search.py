"""
Smart Search Module for TuoKit
Automatically uses the best available search method
"""

import logging
from typing import List, Dict, Optional
from utils.database import DatabaseManager
from utils.ollama import OllamaClient

logger = logging.getLogger(__name__)

class SmartSearch:
    """
    Intelligent search that automatically selects the best method:
    1. Try pgvector if available
    2. Fall back to pg_trgm similarity
    3. Fall back to basic ILIKE search
    """
    
    def __init__(self):
        self.db = DatabaseManager()
        self.capabilities = self._detect_capabilities()
        logger.info(f"SmartSearch initialized with capabilities: {self.capabilities}")
    
    def _detect_capabilities(self) -> Dict[str, bool]:
        """Detect available search capabilities"""
        caps = {
            'pgvector': False,
            'pg_trgm': False,
            'embeddings': False
        }
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Check pgvector
                    cur.execute("SELECT EXISTS(SELECT 1 FROM pg_extension WHERE extname = 'vector')")
                    caps['pgvector'] = cur.fetchone()[0]
                    
                    # Check pg_trgm
                    cur.execute("SELECT EXISTS(SELECT 1 FROM pg_extension WHERE extname = 'pg_trgm')")
                    caps['pg_trgm'] = cur.fetchone()[0]
                    
                    # Check if Ollama embeddings work
                    try:
                        ollama = OllamaClient()
                        test_resp = ollama.embeddings(model="nomic-embed-text", prompt="test")
                        caps['embeddings'] = 'embedding' in test_resp
                    except:
                        caps['embeddings'] = False
                        
        except Exception as e:
            logger.error(f"Error detecting capabilities: {e}")
        
        return caps
    
    def search(self, query: str, table: str = "knowledge_units", 
              limit: int = 10, fields: Optional[List[str]] = None) -> List[Dict]:
        """
        Smart search that uses the best available method
        
        Args:
            query: Search query
            table: Table to search in
            limit: Maximum results
            fields: Fields to search (defaults to title and content)
        """
        if not fields:
            fields = ['title', 'content']
        
        try:
            # Method 1: Use pg_trgm similarity if available
            if self.capabilities['pg_trgm']:
                return self._similarity_search(query, table, fields, limit)
            
            # Method 2: Fall back to ILIKE search
            return self._ilike_search(query, table, fields, limit)
            
        except Exception as e:
            logger.error(f"Search failed: {e}")
            return []
    
    def _similarity_search(self, query: str, table: str, 
                          fields: List[str], limit: int) -> List[Dict]:
        """Search using pg_trgm similarity"""
        # Build similarity expression
        similarity_expr = " || ' ' || ".join(fields)
        
        sql = f"""
            SELECT *,
                   similarity({similarity_expr}, %s) as search_score
            FROM {table}
            WHERE similarity({similarity_expr}, %s) > 0.1
               OR {' OR '.join([f"{field} ILIKE %s" for field in fields])}
            ORDER BY search_score DESC
            LIMIT %s
        """
        
        # Parameters: query for similarity (2x), then ILIKE patterns, then limit
        ilike_params = [f"%{query}%" for _ in fields]
        params = [query, query] + ilike_params + [limit]
        
        return self.db.execute_query(sql, tuple(params))
    
    def _ilike_search(self, query: str, table: str, 
                     fields: List[str], limit: int) -> List[Dict]:
        """Basic ILIKE search"""
        where_clauses = [f"{field} ILIKE %s" for field in fields]
        
        sql = f"""
            SELECT *
            FROM {table}
            WHERE {' OR '.join(where_clauses)}
            ORDER BY created_at DESC
            LIMIT %s
        """
        
        params = [f"%{query}%" for _ in fields] + [limit]
        
        return self.db.execute_query(sql, tuple(params))
    
    def enhance_with_llm(self, query: str, results: List[Dict], 
                        model: str = "deepseek-r1:1.5b") -> List[Dict]:
        """
        Optional: Re-rank results using LLM
        Only use for small result sets due to cost
        """
        if not results or len(results) > 20:
            return results
        
        try:
            ollama = OllamaClient()
            
            # Create a prompt for relevance scoring
            prompt = f"""Rate the relevance of these search results for the query: "{query}"

For each result, provide a relevance score from 0-10.

Results:
"""
            for i, r in enumerate(results):
                prompt += f"\n{i+1}. {r.get('title', 'No title')}\n   {r.get('content', '')[:200]}...\n"
            
            prompt += "\nProvide scores in format: 1:8, 2:6, 3:9, etc."
            
            response = ollama.generate(model=model, prompt=prompt)
            
            # Parse scores and re-rank
            # (Simple implementation - can be improved)
            
        except Exception as e:
            logger.error(f"LLM ranking failed: {e}")
        
        return results
    
    def get_status(self) -> Dict:
        """Get search system status"""
        return {
            'capabilities': self.capabilities,
            'recommended_method': self._get_recommended_method(),
            'performance_notes': self._get_performance_notes()
        }
    
    def _get_recommended_method(self) -> str:
        """Get recommended search method based on capabilities"""
        if self.capabilities['pgvector'] and self.capabilities['embeddings']:
            return "vector_similarity"
        elif self.capabilities['pg_trgm']:
            return "text_similarity"
        else:
            return "basic_search"
    
    def _get_performance_notes(self) -> List[str]:
        """Get performance improvement suggestions"""
        notes = []
        
        if not self.capabilities['pg_trgm']:
            notes.append("Enable pg_trgm extension for better text search")
        
        if not self.capabilities['pgvector']:
            notes.append("Consider installing pgvector for semantic search")
        
        if not self.capabilities['embeddings']:
            notes.append("Ollama embeddings not available - check model")
        
        return notes

# Global instance
_smart_search = None

def get_smart_search() -> SmartSearch:
    """Get or create smart search instance"""
    global _smart_search
    if _smart_search is None:
        _smart_search = SmartSearch()
    return _smart_search

# Convenience functions
def search_knowledge(query: str, limit: int = 10) -> List[Dict]:
    """Search knowledge base using smart search"""
    search = get_smart_search()
    return search.search(query, "knowledge_units", limit)

def search_table(query: str, table: str, fields: List[str], limit: int = 10) -> List[Dict]:
    """Search any table using smart search"""
    search = get_smart_search()
    return search.search(query, table, limit, fields)

if __name__ == "__main__":
    # Test smart search
    search = get_smart_search()
    print(f"Search capabilities: {search.get_status()}")
    
    # Try a search
    results = search_knowledge("error handling")
    print(f"\nFound {len(results)} results")
    for r in results[:3]:
        print(f"- {r.get('title', 'No title')}")