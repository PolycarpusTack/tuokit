"""
TuoKit - Knowledge Explorer (Prototype)
Advanced semantic search and chat interface for the knowledge base
Inspired by Weaviate Magic Chat implementation
"""

import streamlit as st
from utils.ollama import get_available_models
from datetime import datetime
import json
from typing import List, Dict, Any, Optional
import time
from utils import DatabaseManager, apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation
from utils.ollama import get_ollama_response
from utils.simple_vector_store import SimpleVectorStore
from utils.embedding_engine import EmbeddingEngine
from utils.vector_constants import (
    SEMANTIC_SEARCH_THRESHOLD,
    HYBRID_SEARCH_THRESHOLD,
    DEFAULT_SEARCH_LIMIT
)

# Page configuration
st.set_page_config(
    page_title="Knowledge Explorer - TuoKit",
    page_icon="🔮",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="knowledge_explorer")

# Initialize session state
if "chat_messages" not in st.session_state:
    st.session_state.chat_messages = []
if "search_mode" not in st.session_state:
    st.session_state.search_mode = "hybrid"
if "db" not in st.session_state:
    st.session_state.db = DatabaseManager()
if "embedding_engine" not in st.session_state:
    st.session_state.embedding_engine = EmbeddingEngine()
if "vector_store" not in st.session_state:
    # Pass embedding engine to avoid circular imports
    st.session_state.vector_store = SimpleVectorStore(
        st.session_state.db, 
        st.session_state.embedding_engine
    )
if "optimized_search" not in st.session_state:
    from utils.vector_cache import OptimizedVectorSearch
    st.session_state.optimized_search = OptimizedVectorSearch(
        st.session_state.vector_store
    )

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🔮 Knowledge Explorer
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Chat with your knowledge base using semantic search and AI
    </p>
</div>
""", unsafe_allow_html=True)

# Sidebar configuration
with st.sidebar:
    st.header("🔍 Search Configuration")
    
    # Search mode selection
    search_mode = st.selectbox(
        "Search Mode",
        ["keyword", "semantic", "hybrid", "generative"],
        index=2,  # Default to hybrid
        format_func=lambda x: {
            "keyword": "🔤 Keyword Search",
            "semantic": "🧠 Semantic Search", 
            "hybrid": "🔀 Hybrid Search",
            "generative": "✨ Generative Search"
        }[x]
    )
    st.session_state.search_mode = search_mode
    
    # Search mode explanations
    with st.expander("ℹ️ Search Mode Information"):
        if search_mode == "keyword":
            st.info("""
            **Keyword Search**: 
            Traditional exact-match search. Finds knowledge entries containing your exact search terms.
            
            Best for: Finding specific error messages, function names, or exact phrases.
            """)
        elif search_mode == "semantic":
            st.info("""
            **Semantic Search**: 
            Understands meaning and context. Finds conceptually related knowledge even without exact keyword matches.
            
            Best for: Exploring concepts, finding similar solutions, discovering related topics.
            """)
        elif search_mode == "hybrid":
            st.info("""
            **Hybrid Search**: 
            Combines keyword and semantic search for best of both worlds. Balances exact matches with conceptual relevance.
            
            Best for: General knowledge exploration when you're not sure of exact terms.
            """)
        else:  # generative
            st.info("""
            **Generative Search**: 
            Uses AI to synthesize answers from multiple knowledge sources. Creates comprehensive responses.
            
            Best for: Complex questions requiring information from multiple sources.
            """)
    
    # Results configuration
    st.subheader("📊 Results Settings")
    max_results = st.slider("Maximum results", 3, 20, 10)
    
    # Model selection for generative mode
    if search_mode == "generative":
        model = st.selectbox(
            "AI Model",
            get_available_models() or ["No models available"],
            index=0
        )
    
    st.markdown("---")
    
    # Knowledge stats
    if st.session_state.db:
        knowledge_count = st.session_state.db.get_knowledge_count()
        st.metric("Knowledge Items", f"{knowledge_count:,}")
        
        # Embedding status
        with st.expander("🧠 Embedding Status"):
            stats = st.session_state.vector_store.get_stats()
            st.metric("Total Embeddings", stats.get("total_embeddings", 0))
            
            if stats.get("knowledge_without_embeddings", 0) > 0:
                if st.button("Generate Missing Embeddings", type="primary"):
                    with st.spinner("Generating embeddings..."):
                        count = st.session_state.vector_store.update_all_embeddings()
                        st.success(f"Generated {count} new embeddings!")
                        st.rerun()
            else:
                st.success("✅ All knowledge items have embeddings")
            
            if stats.get("by_model"):
                st.caption("Embeddings by Model:")
                for model, count in stats["by_model"].items():
                    st.caption(f"• {model}: {count}")
        
        # Performance monitoring
        with st.expander("⚡ Performance"):
            perf_stats = st.session_state.optimized_search.get_performance_stats()
            
            # Cache stats
            cache_stats = perf_stats['cache_stats']
            col1, col2 = st.columns(2)
            with col1:
                st.metric("Cache Hit Rate", f"{cache_stats['hit_rate']:.1f}%")
            with col2:
                st.metric("Cached Searches", f"{cache_stats['size']}/{cache_stats['max_size']}")
            
            # Index stats
            st.caption("Index Status:")
            for model, stats in perf_stats['index_stats'].items():
                if stats['exists']:
                    st.caption(f"• {model}: {stats['embedding_count']} indexed")
                else:
                    st.caption(f"• {model}: No index")
            
            if st.button("Build/Update Indices"):
                with st.spinner("Building indices..."):
                    st.session_state.optimized_search.build_indices()
                    st.success("Indices updated!")
                    st.rerun()
            
            if st.button("Clear Cache"):
                st.session_state.optimized_search.clear_cache()
                st.success("Cache cleared!")
                st.rerun()

# Helper functions
def search_knowledge(query: str, mode: str, limit: int = DEFAULT_SEARCH_LIMIT) -> List[Dict[str, Any]]:
    """Search knowledge base with different modes"""
    db = st.session_state.db
    vector_store = st.session_state.vector_store
    embedding_engine = st.session_state.embedding_engine
    optimized_search = st.session_state.optimized_search
    
    if mode == "keyword":
        # Traditional keyword search with safe method
        results = db.search_knowledge_safe(query, limit=limit)
    
    elif mode == "semantic":
        # Real semantic search using embeddings
        try:
            # Generate query embedding
            query_embedding = embedding_engine.embed_text(query)
            
            # Use optimized search with caching
            similar_items = optimized_search.search_similar(
                query_embedding, 
                model_name=embedding_engine.model,
                threshold=SEMANTIC_SEARCH_THRESHOLD,
                limit=limit
            )
            
            # Fetch full knowledge items in batch
            knowledge_ids = [kid for kid, _ in similar_items]
            knowledge_items = db.get_knowledge_by_ids(knowledge_ids)
            
            # Build results maintaining order and similarity scores
            results = []
            for knowledge_id, similarity in similar_items:
                if knowledge_id in knowledge_items:
                    item = knowledge_items[knowledge_id]
                    # Add similarity score to the result
                    results.append(item + (similarity,))
            
            # If vector search returns no results, fall back to keyword search
            if not results:
                results = db.search_knowledge_safe(query, limit=limit)
                
        except Exception as e:
            st.warning(f"Vector search unavailable, using keyword search: {str(e)}")
            results = db.search_knowledge_safe(query, limit=limit)
    
    elif mode == "hybrid":
        # Combine keyword and semantic results
        keyword_results = db.search_knowledge_safe(query, limit=limit//2)
        
        # Try semantic search
        semantic_results = []
        try:
            query_embedding = embedding_engine.embed_text(query)
            similar_items = optimized_search.search_similar(
                query_embedding,
                model_name=embedding_engine.model,
                threshold=HYBRID_SEARCH_THRESHOLD,
                limit=limit//2
            )
            
            # Batch fetch knowledge items
            knowledge_ids = [kid for kid, _ in similar_items]
            knowledge_items = db.get_knowledge_by_ids(knowledge_ids)
            
            for knowledge_id, similarity in similar_items:
                if knowledge_id in knowledge_items:
                    item = knowledge_items[knowledge_id]
                    semantic_results.append(item + (similarity,))
        except:
            # Fallback to safe keyword search - no manual query building
            semantic_results = db.search_knowledge_safe(query, limit=limit//2)
        
        # Merge and deduplicate
        seen_ids = set()
        results = []
        
        # Add keyword results first
        for r in keyword_results:
            if r[0] not in seen_ids:
                seen_ids.add(r[0])
                results.append(r)
        
        # Add semantic results
        for r in semantic_results:
            if r[0] not in seen_ids:
                seen_ids.add(r[0])
                results.append(r)
                if len(results) >= limit:
                    break
    
    else:  # generative
        # Get relevant knowledge using hybrid search
        results = search_knowledge(query, "hybrid", limit=limit)
    
    return results

def format_knowledge_cards(results: List[Any], query: str) -> str:
    """Format search results as cards"""
    if not results:
        return "No knowledge items found matching your query."
    
    response = f"I found {len(results)} relevant knowledge items:\n\n"
    
    for i, result in enumerate(results, 1):
        # Handle results with and without similarity scores
        if len(result) > 7:  # Has similarity score
            id, tool, prompt, resp, created_at, category, metadata, similarity = result
            similarity_text = f" (Similarity: {similarity:.2%})"
        else:
            id, tool, prompt, resp, created_at, category, metadata = result
            similarity_text = ""
        
        # Truncate long responses
        preview = resp[:200] + "..." if len(resp) > 200 else resp
        
        response += f"### {i}. {tool} - {category or 'General'}{similarity_text}\n"
        response += f"**Created**: {created_at.strftime('%Y-%m-%d %H:%M')}\n"
        response += f"**Context**: {prompt[:100]}...\n" if len(prompt) > 100 else f"**Context**: {prompt}\n"
        response += f"**Knowledge**: {preview}\n\n"
    
    return response

def generate_ai_response(query: str, results: List[Any], model: str) -> str:
    """Generate AI response from search results"""
    if not results:
        return "I couldn't find any relevant knowledge to answer your question. Try rephrasing or using different keywords."
    
    # Prepare context from search results
    context = "Based on the following knowledge items:\n\n"
    for i, result in enumerate(results[:5], 1):
        # Handle results with and without similarity scores
        if len(result) > 7:  # Has similarity score
            id, tool, prompt, response, created_at, category, metadata, similarity = result
        else:
            id, tool, prompt, response, created_at, category, metadata = result
            
        context += f"{i}. Tool: {tool}, Category: {category or 'General'}\n"
        context += f"   Context: {prompt}\n"
        context += f"   Knowledge: {response[:500]}...\n\n" if len(response) > 500 else f"   Knowledge: {response}\n\n"
    
    # Generate response
    ai_prompt = f"""Based on the knowledge base context provided, please answer the following question:

Question: {query}

Context:
{context}

Please provide a comprehensive answer that:
1. Directly addresses the question
2. Synthesizes information from multiple knowledge items if relevant
3. Provides practical examples or code snippets if applicable
4. Suggests related topics the user might want to explore

Keep the response concise but informative."""

    response = get_ollama_response(ai_prompt, model=model)
    return response

def simulate_typing(text: str, placeholder):
    """Simulate typing effect for responses"""
    typed_text = ""
    for char in text:
        typed_text += char
        placeholder.markdown(typed_text + "▌")
        time.sleep(0.01)  # Adjust speed as needed
    placeholder.markdown(text)

# Example prompts
st.markdown("### 💡 Try these examples:")
example_cols = st.columns(4)

example_prompts = [
    "How to optimize SQL queries?",
    "Python debugging techniques",
    "Error handling best practices",
    "Database connection issues"
]

for col, prompt in zip(example_cols, example_prompts):
    with col:
        if st.button(prompt, use_container_width=True):
            st.session_state.chat_messages.append({"role": "user", "content": prompt})
            st.rerun()

# Chat interface
st.markdown("---")

# Display chat messages
for message in st.session_state.chat_messages:
    with st.chat_message(message["role"]):
        st.markdown(message["content"])

# Chat input
if prompt := st.chat_input("Ask about your knowledge base..."):
    # Add user message
    st.session_state.chat_messages.append({"role": "user", "content": prompt})
    
    with st.chat_message("user"):
        st.markdown(prompt)
    
    # Generate assistant response
    with st.chat_message("assistant"):
        response_placeholder = st.empty()
        
        with st.spinner("Searching knowledge base..."):
            # Search knowledge
            results = search_knowledge(prompt, st.session_state.search_mode, max_results)
            
            if st.session_state.search_mode == "generative":
                # Generate AI response
                response = generate_ai_response(prompt, results, model)
                simulate_typing(response, response_placeholder)
            else:
                # Format results as cards
                response = format_knowledge_cards(results, prompt)
                response_placeholder.markdown(response)
        
        # Add assistant response to chat
        st.session_state.chat_messages.append({"role": "assistant", "content": response})

# Knowledge visualization section
if st.session_state.chat_messages:
    st.markdown("---")
    st.subheader("📊 Knowledge Insights")
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        # Recent searches
        st.markdown("**🕐 Recent Searches**")
        recent_searches = [msg["content"] for msg in st.session_state.chat_messages if msg["role"] == "user"][-5:]
        for search in recent_searches:
            st.caption(f"• {search[:50]}...")
    
    with col2:
        # Popular categories
        st.markdown("**📁 Top Categories**")
        if st.session_state.db:
            # This would need a real query to get category counts
            st.caption("• Error Handling (45)")
            st.caption("• Performance (38)")
            st.caption("• Database (32)")
            st.caption("• Python (28)")
            st.caption("• SQL (25)")
    
    with col3:
        # Suggested topics
        st.markdown("**💡 Suggested Topics**")
        suggestions = [
            "Database optimization",
            "Error debugging",
            "Code refactoring",
            "Testing strategies",
            "Security practices"
        ]
        for suggestion in suggestions[:5]:
            if st.button(suggestion, key=f"suggest_{suggestion}", use_container_width=True):
                st.session_state.chat_messages.append({"role": "user", "content": suggestion})
                st.rerun()

# Footer with tips
st.markdown("---")
st.caption("""
💡 **Tips**: 
- Use **Keyword Search** for exact error messages or function names
- Try **Semantic Search** to explore related concepts
- **Hybrid Search** gives the best overall results
- **Generative Search** creates comprehensive answers from multiple sources
""")

# Add welcome message if no chat history
if not st.session_state.chat_messages:
    with st.chat_message("assistant"):
        st.markdown("""
        👋 Welcome to Knowledge Explorer! I can help you search and explore your knowledge base in multiple ways:
        
        - 🔤 **Keyword Search**: Find exact matches
        - 🧠 **Semantic Search**: Discover related concepts
        - 🔀 **Hybrid Search**: Best of both worlds
        - ✨ **Generative Search**: AI-powered comprehensive answers
        
        Try asking me something or click one of the example prompts above!
        """)