"""
Vector Search Demo Page
Test and demonstrate vector search capabilities
"""

import streamlit as st
from utils.smart_search import get_smart_search, search_knowledge
from utils.vector_knowledge_system import (
    VectorKnowledgeSystem, IndexedContent, ContentType
)
from utils.database import DatabaseManager
import json

st.set_page_config(
    page_title="Vector Search Demo - TuoKit",
    page_icon="🔍",
    layout="wide"
)

st.title("🔍 Vector Search Demo")
st.markdown("Test TuoKit's intelligent search capabilities")

# Initialize systems
search = get_smart_search()
vks = VectorKnowledgeSystem()
db = DatabaseManager()

# Show current status
with st.container():
    col1, col2, col3 = st.columns(3)
    
    with col1:
        status = search.get_status()
        st.metric("Search Method", status['recommended_method'])
    
    with col2:
        stats = vks.get_statistics()
        total_content = sum(stats.get('content_by_type', {}).values())
        st.metric("Indexed Content", total_content)
    
    with col3:
        st.metric("Knowledge Units", db.get_knowledge_count())

# Tabs for different features
tab1, tab2, tab3, tab4 = st.tabs(["Search", "Index Content", "System Status", "RAG Demo"])

with tab1:
    st.header("Search Knowledge Base")
    
    # Search interface
    col1, col2 = st.columns([3, 1])
    with col1:
        query = st.text_input("Search Query", placeholder="e.g., error handling, SmallTalk collections")
    with col2:
        limit = st.number_input("Max Results", min_value=1, max_value=50, value=10)
    
    # Content type filter
    content_types = st.multiselect(
        "Filter by Content Type",
        options=[ct.value for ct in ContentType],
        default=[]
    )
    
    if st.button("Search", type="primary"):
        if query:
            with st.spinner("Searching..."):
                # Try vector knowledge system first
                if content_types:
                    filter_types = [ContentType(ct) for ct in content_types]
                    results = vks.search(query, content_types=filter_types, limit=limit)
                else:
                    # Use smart search for general queries
                    results = search_knowledge(query, limit)
                
                st.success(f"Found {len(results)} results")
                
                # Display results
                for i, result in enumerate(results):
                    with st.expander(f"{i+1}. {result.get('title', 'No title')}", expanded=i<3):
                        if 'similarity' in result:
                            st.metric("Similarity Score", f"{result['similarity']:.3f}")
                        
                        st.markdown("**Content:**")
                        st.text(result.get('content', '')[:500] + "...")
                        
                        if 'tags' in result and result['tags']:
                            st.markdown("**Tags:** " + ", ".join(result['tags']))
                        
                        if 'metadata' in result and result['metadata']:
                            st.json(result['metadata'])

with tab2:
    st.header("Index New Content")
    
    # Form for indexing content
    with st.form("index_content"):
        content_type = st.selectbox(
            "Content Type",
            options=[ct.value for ct in ContentType],
            format_func=lambda x: x.title()
        )
        
        title = st.text_input("Title", placeholder="Brief descriptive title")
        
        content = st.text_area(
            "Content", 
            placeholder="Main content to index",
            height=200
        )
        
        # Metadata fields
        st.markdown("### Metadata")
        col1, col2 = st.columns(2)
        
        with col1:
            source = st.text_input("Source", placeholder="Where this content came from")
            language = st.text_input("Language", placeholder="e.g., python, smalltalk")
        
        with col2:
            tags = st.text_input("Tags (comma-separated)", placeholder="tag1, tag2, tag3")
        
        # Type-specific metadata
        if content_type == ContentType.SMALLTALK.value:
            st.markdown("#### SmallTalk Specific")
            col1, col2 = st.columns(2)
            with col1:
                class_name = st.text_input("Class Name")
                method_name = st.text_input("Method Name")
            with col2:
                category = st.text_input("Category")
                package = st.text_input("Package")
        
        if st.form_submit_button("Index Content", type="primary"):
            if title and content:
                # Prepare metadata
                metadata = {}
                if content_type == ContentType.SMALLTALK.value:
                    metadata.update({
                        "class_name": class_name,
                        "method_name": method_name,
                        "category": category,
                        "package": package
                    })
                
                # Create indexed content
                indexed_content = IndexedContent(
                    content_type=ContentType(content_type),
                    title=title,
                    content=content,
                    metadata=metadata,
                    source=source,
                    language=language,
                    tags=[t.strip() for t in tags.split(",")] if tags else None
                )
                
                # Index it
                content_id = vks.index_content(indexed_content)
                
                if content_id:
                    st.success(f"✅ Content indexed successfully! (ID: {content_id})")
                else:
                    st.error("Failed to index content")
            else:
                st.error("Please provide both title and content")

with tab3:
    st.header("System Status")
    
    # Search capabilities
    st.subheader("Search Capabilities")
    status = search.get_status()
    
    col1, col2 = st.columns(2)
    with col1:
        for cap, enabled in status['capabilities'].items():
            st.checkbox(cap, value=enabled, disabled=True)
    
    with col2:
        st.markdown("**Performance Notes:**")
        for note in status['performance_notes']:
            st.info(note)
    
    # Content statistics
    st.subheader("Content Statistics")
    stats = vks.get_statistics()
    
    # Content by type
    if 'content_by_type' in stats and stats['content_by_type']:
        st.markdown("**Content by Type:**")
        for ctype, count in stats['content_by_type'].items():
            st.metric(ctype.title(), count)
    
    # Training data
    if 'training_pairs_by_type' in stats and stats['training_pairs_by_type']:
        st.markdown("**Training Pairs by Type:**")
        for ttype, count in stats['training_pairs_by_type'].items():
            st.metric(ttype.title(), count)
    
    # SmallTalk statistics
    if 'smalltalk' in stats:
        st.markdown("**SmallTalk Artifacts:**")
        col1, col2 = st.columns(2)
        with col1:
            st.metric("Classes", stats['smalltalk']['classes'])
        with col2:
            st.metric("Packages", stats['smalltalk']['packages'])

with tab4:
    st.header("RAG (Retrieval Augmented Generation) Demo")
    
    # RAG query interface
    rag_query = st.text_input("Ask a question", placeholder="How do I handle errors in Python?")
    
    if st.button("Generate RAG Response"):
        if rag_query:
            with st.spinner("Retrieving context and generating response..."):
                # Get RAG context
                context = vks.prepare_rag_context(rag_query, max_tokens=1500)
                
                if context:
                    st.subheader("Retrieved Context:")
                    with st.expander("View Context", expanded=True):
                        st.text(context)
                    
                    # TODO: Generate response using Ollama with context
                    st.info("🚧 LLM response generation coming soon!")
                    
                    # Show how to use with Ollama
                    st.code(f"""
# Example usage with Ollama:
from utils.ollama import OllamaClient

ollama = OllamaClient()
prompt = f'''Based on the following context, answer this question: {rag_query}

Context:
{context[:500]}...

Answer:'''

response = ollama.generate(model="deepseek-r1:1.5b", prompt=prompt)
                    """, language="python")
                else:
                    st.warning("No relevant context found for your query")

# Footer with instructions
st.markdown("---")
st.markdown("""
### 📚 How to Use Vector Search

1. **Basic Search**: Just type your query and search
2. **Filtered Search**: Select content types to narrow results
3. **Index Content**: Add new content to the knowledge base
4. **RAG Demo**: See how retrieval augmented generation works

### 🚀 Next Steps

1. Index your SmallTalk codebase using the Index Content tab
2. Build training datasets using `prepare_training_dataset()`
3. Fine-tune models for your specific domain
""")

# Debug info in expander
with st.expander("Debug Information"):
    st.json({
        "search_status": search.get_status(),
        "vks_stats": vks.get_statistics(),
        "db_connected": db.connected
    })