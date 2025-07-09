"""
Vector Search Implementation for TuoKit
Activates when document volume exceeds threshold
"""
import sqlite3
import streamlit as st
from typing import List, Tuple, Optional
import logging

# Configuration
DB_PATH = "tuokit.db"
VECTOR_THRESHOLD = 50  # Enable vector search after 50 documents
logger = logging.getLogger(__name__)

def check_enable_vector_search() -> Tuple[bool, int]:
    """
    Enable vector search when document volume increases (>50 documents)
    
    Returns:
        Tuple[bool, int]: (should_enable, document_count)
    """
    try:
        conn = sqlite3.connect(DB_PATH)
        doc_count = conn.execute(
            "SELECT COUNT(DISTINCT context) FROM knowledge WHERE tool='document_qa'"
        ).fetchone()[0]
        conn.close()
        
        if doc_count > VECTOR_THRESHOLD:
            # Initialize simple vector search using TF-IDF for now
            # Upgrade to ChromaDB/FAISS when scaling further
            logger.info(f"Document count ({doc_count}) exceeds threshold ({VECTOR_THRESHOLD})")
            return True, doc_count
        return False, doc_count
    except Exception as e:
        logger.warning(f"Vector search check failed: {e}")
        return False, 0

def index_documents_for_vector_search():
    """
    Build vector index from existing documents - minimal implementation
    This is called when threshold is reached
    """
    enabled, doc_count = check_enable_vector_search()
    
    if enabled:
        # NOTE: Add 'pip install chromadb' to requirements.txt when implementing
        st.info(f"📊 Document volume reached threshold ({doc_count} documents) - vector search recommended")
        
        # Future implementation steps:
        # 1. Initialize ChromaDB collection
        # 2. Extract all documents from knowledge base
        # 3. Generate embeddings for each document
        # 4. Store in vector database
        # 5. Enable similarity search in Document Q&A
        
        # Placeholder for future implementation
        logger.info("Vector search initialization triggered")
        
        # TODO: Implement actual vector indexing
        # Example structure:
        # from chromadb import Client
        # client = Client()
        # collection = client.create_collection("tuokit_documents")
        # documents = get_all_documents()
        # collection.add(
        #     documents=documents['texts'],
        #     metadatas=documents['metadata'],
        #     ids=documents['ids']
        # )
        
        pass

def get_all_documents() -> dict:
    """
    Retrieve all documents from knowledge base for indexing
    
    Returns:
        dict: Contains texts, metadata, and ids
    """
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.execute("""
        SELECT id, prompt, response, context, timestamp
        FROM knowledge
        WHERE tool = 'document_qa'
        ORDER BY timestamp DESC
    """)
    
    documents = {
        'texts': [],
        'metadata': [],
        'ids': []
    }
    
    for row in cursor.fetchall():
        doc_id, prompt, response, context, timestamp = row
        
        # Combine prompt and response for better search
        full_text = f"{prompt}\n\n{response}"
        
        documents['texts'].append(full_text)
        documents['metadata'].append({
            'source': context,
            'timestamp': timestamp,
            'type': 'document_qa'
        })
        documents['ids'].append(f"doc_{doc_id}")
    
    conn.close()
    return documents

def vector_search(query: str, top_k: int = 5) -> List[dict]:
    """
    Perform vector similarity search
    
    Args:
        query: Search query
        top_k: Number of results to return
        
    Returns:
        List of similar documents with scores
    """
    # Check if vector search is enabled
    enabled, doc_count = check_enable_vector_search()
    
    if not enabled:
        logger.info(f"Vector search not enabled. Document count: {doc_count}")
        return []
    
    # Placeholder for actual vector search
    # TODO: Implement ChromaDB search
    # Example:
    # collection = get_vector_collection()
    # results = collection.query(
    #     query_texts=[query],
    #     n_results=top_k
    # )
    
    # For now, return empty results
    return []

def update_vector_index(doc_id: str, text: str, metadata: dict):
    """
    Update vector index when new documents are added
    
    Args:
        doc_id: Document identifier
        text: Document text
        metadata: Document metadata
    """
    enabled, _ = check_enable_vector_search()
    
    if enabled:
        # TODO: Add document to vector index
        # collection = get_vector_collection()
        # collection.add(
        #     documents=[text],
        #     metadatas=[metadata],
        #     ids=[doc_id]
        # )
        logger.info(f"Document {doc_id} queued for vector indexing")

# Integration with Document Q&A
def enhanced_document_search(query: str, use_vector: bool = True) -> List[dict]:
    """
    Enhanced document search with optional vector similarity
    
    Args:
        query: Search query
        use_vector: Whether to use vector search if available
        
    Returns:
        List of relevant documents
    """
    results = []
    
    # Traditional keyword search
    conn = sqlite3.connect(DB_PATH)
    keyword_results = conn.execute("""
        SELECT id, prompt, response, context, timestamp
        FROM knowledge
        WHERE tool = 'document_qa'
        AND (prompt LIKE ? OR response LIKE ? OR context LIKE ?)
        ORDER BY timestamp DESC
        LIMIT 10
    """, (f"%{query}%", f"%{query}%", f"%{query}%")).fetchall()
    conn.close()
    
    # Convert to dict format
    for row in keyword_results:
        results.append({
            'id': row[0],
            'prompt': row[1],
            'response': row[2],
            'source': row[3],
            'timestamp': row[4],
            'score': 0.5  # Default score for keyword matches
        })
    
    # Add vector search results if enabled
    if use_vector:
        vector_results = vector_search(query)
        
        # Merge results, avoiding duplicates
        existing_ids = {r['id'] for r in results}
        for vr in vector_results:
            if vr['id'] not in existing_ids:
                results.append(vr)
    
    # Sort by score (when vector search is implemented)
    results.sort(key=lambda x: x.get('score', 0), reverse=True)
    
    return results

# Monitoring functions
def get_vector_search_status() -> dict:
    """Get current status of vector search system"""
    enabled, doc_count = check_enable_vector_search()
    
    status = {
        'enabled': enabled,
        'document_count': doc_count,
        'threshold': VECTOR_THRESHOLD,
        'progress': min(100, int((doc_count / VECTOR_THRESHOLD) * 100)),
        'next_steps': []
    }
    
    if not enabled:
        docs_needed = VECTOR_THRESHOLD - doc_count + 1
        status['next_steps'].append(f"Add {docs_needed} more documents to enable vector search")
    else:
        status['next_steps'].append("Vector search is ready to be implemented")
        status['next_steps'].append("Run: pip install chromadb sentence-transformers")
        
    return status

# Display function for UI
def show_vector_search_status():
    """Display vector search status in the UI"""
    status = get_vector_search_status()
    
    if status['enabled']:
        st.success(f"🚀 Vector search available! ({status['document_count']} documents indexed)")
    else:
        st.info(f"📊 Vector search will activate at {VECTOR_THRESHOLD} documents")
        st.progress(status['progress'] / 100, text=f"Progress: {status['document_count']}/{VECTOR_THRESHOLD}")
    
    with st.expander("Vector Search Details"):
        st.json(status)

# Future enhancements placeholder
"""
Future Vector Search Enhancements:

1. **ChromaDB Integration**
   - Persistent vector storage
   - Efficient similarity search
   - Metadata filtering

2. **Embedding Models**
   - Sentence-transformers for better embeddings
   - Support for multiple embedding models
   - Fine-tuning on domain-specific data

3. **Advanced Features**
   - Hybrid search (keyword + vector)
   - Semantic chunking for long documents
   - Re-ranking with cross-encoders
   - Query expansion

4. **Performance Optimization**
   - Batch processing for indexing
   - Incremental index updates
   - Caching frequent queries
   - Asynchronous indexing

5. **UI Enhancements**
   - Visual similarity scores
   - Search result explanations
   - Filter by document type
   - Time-based filtering
"""

if __name__ == "__main__":
    # Test the implementation
    print("Vector Search Status:")
    print(get_vector_search_status())
    
    # Check if indexing should be triggered
    index_documents_for_vector_search()
