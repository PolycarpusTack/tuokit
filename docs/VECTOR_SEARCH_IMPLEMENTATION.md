# Vector Search Implementation Guide for TuoKit

## Overview

Based on the analysis of Weaviate Magic Chat, implementing vector search in TuoKit would significantly enhance the knowledge base functionality. This guide outlines how to add semantic search capabilities to TuoKit.

## 🎯 Benefits of Vector Search for TuoKit

1. **Semantic Understanding**: Find related knowledge even without exact keyword matches
2. **Better Discovery**: Surface relevant solutions that use different terminology
3. **Context-Aware Search**: Understand the intent behind queries
4. **Multi-Modal Search**: Search across code, text, and structured data
5. **Similarity Matching**: Find similar errors, patterns, or solutions

## 🏗️ Implementation Options

### Option 1: PostgreSQL with pgvector (Recommended)

Since TuoKit already uses PostgreSQL, adding pgvector extension would be the most seamless approach.

```sql
-- Enable pgvector extension
CREATE EXTENSION vector;

-- Add embedding column to knowledge_units table
ALTER TABLE knowledge_units 
ADD COLUMN embedding vector(768);  -- 768 dimensions for all-MiniLM-L6-v2

-- Create index for similarity search
CREATE INDEX ON knowledge_units 
USING ivfflat (embedding vector_cosine_ops)
WITH (lists = 100);
```

**Implementation steps:**
```python
# utils/vector_search.py
from sentence_transformers import SentenceTransformer
import numpy as np
from typing import List, Tuple

class VectorSearchEngine:
    def __init__(self, model_name='all-MiniLM-L6-v2'):
        self.model = SentenceTransformer(model_name)
    
    def embed_text(self, text: str) -> np.ndarray:
        """Generate embedding for text"""
        return self.model.encode(text)
    
    def embed_batch(self, texts: List[str]) -> np.ndarray:
        """Generate embeddings for multiple texts"""
        return self.model.encode(texts)
    
    def search_similar(self, query: str, limit: int = 10) -> List[Tuple[int, float]]:
        """Search for similar knowledge items"""
        query_embedding = self.embed_text(query)
        
        # PostgreSQL query with pgvector
        sql = """
        SELECT id, 1 - (embedding <=> %s::vector) as similarity
        FROM knowledge_units
        WHERE embedding IS NOT NULL
        ORDER BY embedding <=> %s::vector
        LIMIT %s
        """
        # Returns list of (id, similarity_score)
```

### Option 2: Dedicated Vector Database

#### Weaviate Integration
```python
# utils/weaviate_client.py
import weaviate
from weaviate.embedded import EmbeddedOptions

class WeaviateKnowledgeStore:
    def __init__(self):
        self.client = weaviate.Client(
            embedded_options=EmbeddedOptions()
        )
        self._create_schema()
    
    def _create_schema(self):
        """Create Weaviate schema for knowledge"""
        schema = {
            "class": "KnowledgeItem",
            "vectorizer": "text2vec-transformers",
            "properties": [
                {"name": "tool", "dataType": ["string"]},
                {"name": "prompt", "dataType": ["text"]},
                {"name": "response", "dataType": ["text"]},
                {"name": "category", "dataType": ["string"]},
                {"name": "created_at", "dataType": ["date"]},
            ]
        }
        self.client.schema.create_class(schema)
    
    def add_knowledge(self, knowledge_item):
        """Add knowledge item to Weaviate"""
        self.client.data_object.create(
            data_object=knowledge_item,
            class_name="KnowledgeItem"
        )
    
    def semantic_search(self, query: str, limit: int = 10):
        """Perform semantic search"""
        result = self.client.query.get(
            "KnowledgeItem", 
            ["tool", "prompt", "response", "category"]
        ).with_near_text({
            "concepts": [query]
        }).with_limit(limit).do()
        
        return result
```

#### ChromaDB Integration (Lightweight option)
```python
# utils/chroma_client.py
import chromadb
from chromadb.config import Settings

class ChromaKnowledgeStore:
    def __init__(self, persist_directory="./chroma_db"):
        self.client = chromadb.Client(Settings(
            chroma_db_impl="duckdb+parquet",
            persist_directory=persist_directory
        ))
        
        self.collection = self.client.create_collection(
            name="knowledge_base",
            metadata={"hnsw:space": "cosine"}
        )
    
    def add_knowledge(self, texts, metadatas, ids):
        """Add knowledge items with embeddings"""
        self.collection.add(
            documents=texts,
            metadatas=metadatas,
            ids=ids
        )
    
    def search(self, query: str, n_results: int = 10):
        """Search for similar knowledge"""
        results = self.collection.query(
            query_texts=[query],
            n_results=n_results
        )
        return results
```

## 🚀 Enhanced Knowledge Explorer Features

### 1. Hybrid Search Implementation
```python
def hybrid_search(query: str, alpha: float = 0.5) -> List[Dict]:
    """
    Combine keyword and vector search results
    alpha: weight for vector search (0-1)
    """
    # Keyword search results
    keyword_results = db.search_knowledge(query)
    keyword_scores = {r[0]: r.score for r in keyword_results}
    
    # Vector search results
    vector_results = vector_engine.search_similar(query)
    vector_scores = {r[0]: r.similarity for r in vector_results}
    
    # Combine scores
    all_ids = set(keyword_scores.keys()) | set(vector_scores.keys())
    combined_scores = {}
    
    for id in all_ids:
        keyword_score = keyword_scores.get(id, 0)
        vector_score = vector_scores.get(id, 0)
        combined_scores[id] = (alpha * vector_score) + ((1-alpha) * keyword_score)
    
    # Sort by combined score
    sorted_results = sorted(combined_scores.items(), key=lambda x: x[1], reverse=True)
    return sorted_results
```

### 2. Knowledge Graph Visualization
```python
def build_knowledge_graph(central_item_id: int, depth: int = 2):
    """Build a graph of related knowledge items"""
    graph = {"nodes": [], "edges": []}
    visited = set()
    
    def explore_node(item_id, current_depth):
        if current_depth > depth or item_id in visited:
            return
        
        visited.add(item_id)
        
        # Get item details
        item = db.get_knowledge_item(item_id)
        graph["nodes"].append({
            "id": item_id,
            "label": item["tool"],
            "category": item["category"],
            "depth": current_depth
        })
        
        # Find similar items
        similar = vector_engine.search_similar_by_id(item_id, limit=5)
        
        for similar_id, similarity in similar:
            if similar_id != item_id:
                graph["edges"].append({
                    "source": item_id,
                    "target": similar_id,
                    "weight": similarity
                })
                explore_node(similar_id, current_depth + 1)
    
    explore_node(central_item_id, 0)
    return graph
```

### 3. Smart Suggestions
```python
def get_smart_suggestions(current_query: str, search_history: List[str]) -> List[str]:
    """Generate intelligent query suggestions"""
    suggestions = []
    
    # Based on current query
    current_embedding = vector_engine.embed_text(current_query)
    
    # Find queries that led to useful results
    useful_queries = db.get_high_rated_queries()
    
    for query in useful_queries:
        query_embedding = vector_engine.embed_text(query)
        similarity = cosine_similarity(current_embedding, query_embedding)
        
        if 0.3 < similarity < 0.8:  # Similar but not identical
            suggestions.append(query)
    
    # Based on search history pattern
    if len(search_history) > 1:
        # Identify search progression pattern
        history_embeddings = [vector_engine.embed_text(q) for q in search_history]
        # Predict next logical query...
    
    return suggestions[:5]
```

## 📊 Database Schema Updates

```sql
-- Add vector search tables
CREATE TABLE knowledge_embeddings (
    id SERIAL PRIMARY KEY,
    knowledge_id INTEGER REFERENCES knowledge_units(id),
    embedding vector(768),
    model_name VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Track search effectiveness
CREATE TABLE search_feedback (
    id SERIAL PRIMARY KEY,
    query TEXT,
    search_type VARCHAR(20), -- 'keyword', 'vector', 'hybrid'
    result_id INTEGER REFERENCES knowledge_units(id),
    position INTEGER,
    clicked BOOLEAN DEFAULT FALSE,
    helpful BOOLEAN,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Query suggestions
CREATE TABLE query_suggestions (
    id SERIAL PRIMARY KEY,
    original_query TEXT,
    suggested_query TEXT,
    suggestion_type VARCHAR(50),
    times_used INTEGER DEFAULT 0,
    success_rate FLOAT
);
```

## 🔧 Configuration

```python
# config/vector_search.py
VECTOR_SEARCH_CONFIG = {
    "embedding_model": "all-MiniLM-L6-v2",
    "embedding_dimension": 768,
    "similarity_threshold": 0.7,
    "hybrid_search_alpha": 0.5,  # Balance between keyword and vector
    "max_results": 20,
    "cache_embeddings": True,
    "batch_size": 32,
    
    # Model options for different use cases
    "models": {
        "fast": "all-MiniLM-L6-v2",  # 384 dims, fast
        "balanced": "all-mpnet-base-v2",  # 768 dims, good quality
        "accurate": "all-roberta-large-v1",  # 1024 dims, best quality
        "multilingual": "paraphrase-multilingual-MiniLM-L12-v2"
    }
}
```

## 🎯 UI Enhancements

### Visual Similarity Indicator
```python
def render_search_result(item, similarity_score):
    """Render search result with visual similarity indicator"""
    # Color gradient based on similarity
    color = interpolate_color(similarity_score, 
                             low_color="#ff6b6b",   # Red for low
                             high_color="#51cf66")  # Green for high
    
    st.markdown(f"""
    <div style="border-left: 4px solid {color}; padding-left: 1rem;">
        <div style="display: flex; justify-content: space-between;">
            <h4>{item.tool}</h4>
            <span style="color: {color};">{similarity_score:.0%} match</span>
        </div>
        <p>{item.prompt[:100]}...</p>
    </div>
    """, unsafe_allow_html=True)
```

### Search Mode Toggle
```python
# Streamlit UI component
search_mode = st.radio(
    "Search Mode",
    ["Quick", "Deep", "Related", "Expert"],
    horizontal=True,
    help={
        "Quick": "Fast keyword search",
        "Deep": "Thorough semantic search", 
        "Related": "Find conceptually similar items",
        "Expert": "Advanced hybrid search with filters"
    }
)
```

## 📈 Performance Considerations

1. **Embedding Cache**: Cache computed embeddings to avoid recomputation
2. **Batch Processing**: Process multiple queries together
3. **Approximate Search**: Use HNSW or IVF indexes for large datasets
4. **Async Processing**: Generate embeddings asynchronously
5. **Model Selection**: Balance between accuracy and speed

## 🚦 Implementation Phases

### Phase 1: Basic Vector Search (1 week)
- Set up pgvector or ChromaDB
- Implement basic embedding generation
- Add semantic search to Knowledge Explorer

### Phase 2: Hybrid Search (1 week)
- Implement score combination algorithms
- Add search mode selection UI
- Create feedback mechanism

### Phase 3: Advanced Features (2 weeks)
- Knowledge graph visualization
- Smart query suggestions
- Multi-modal search (code + text)
- Search analytics dashboard

## 🎉 Expected Benefits

1. **50-70% improvement** in search relevance
2. **Discover hidden connections** between knowledge items
3. **Natural language queries** ("how to fix database errors")
4. **Cross-domain insights** (find Python solutions for Ruby problems)
5. **Reduced search time** through better first-page results

## 📚 Resources

- [pgvector Documentation](https://github.com/pgvector/pgvector)
- [Sentence Transformers](https://www.sbert.net/)
- [Weaviate Python Client](https://weaviate.io/developers/weaviate/client-libraries/python)
- [ChromaDB Guide](https://docs.trychroma.com/)
- [Vector Search Best Practices](https://www.pinecone.io/learn/vector-search-best-practices/)