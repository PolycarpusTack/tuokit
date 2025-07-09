"""
RAG Search Interface for TuoKit
Provides intelligent search across codebase, documentation, and knowledge base
"""
import streamlit as st
import sys
from pathlib import Path

# Add toolkits to path
sys.path.append(str(Path(__file__).parent.parent))

from toolkits.llm_rag.rag_manager import TuoKitRAGManager
from utils.model_manager import ModelManager
import pandas as pd

# Page configuration
st.set_page_config(
    page_title="RAG Search - TuoKit",
    page_icon="🔍",
    layout="wide"
)

@st.cache_resource
def get_rag_manager():
    """Initialize RAG manager with caching"""
    config_path = Path("toolkits/llm_rag/config.yaml")
    if not config_path.exists():
        st.error("RAG config not found. Please run initialization first.")
        return None
    return TuoKitRAGManager(config_path=str(config_path))

def show():
    """Main page display function"""
    st.title("🔍 TuoKit RAG Search")
    st.markdown("Search across codebase, documentation, and knowledge base with AI-powered answers")
    
    # Initialize RAG
    rag = get_rag_manager()
    if not rag:
        st.stop()
    
    # Get stats
    stats = rag.get_stats()
    
    # Display metrics
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Indexed Files", stats.get('indexed_files', 0))
    with col2:
        st.metric("Total Chunks", sum(item['count'] for item in stats.get('chunks_by_source', [])))
    with col3:
        st.metric("Knowledge Sources", len(stats.get('chunks_by_source', [])))
    
    st.divider()
    
    # Search interface
    col1, col2 = st.columns([3, 1])
    
    with col1:
        query = st.text_input(
            "Ask a question or search for information",
            placeholder="How does the crash analyzer work?"
        )
    
    with col2:
        source_filter = st.selectbox(
            "Filter by source",
            ["All", "code", "documentation", "release_notes", "config"]
        )
    
    # Advanced options
    with st.expander("Advanced Options"):
        col1, col2, col3 = st.columns(3)
        with col1:
            top_k = st.slider("Number of results", 1, 20, 5)
        with col2:
            use_llm = st.toggle("Generate AI answer", value=True)
        with col3:
            show_chunks = st.toggle("Show source chunks", value=False)
    
    # Search button
    if st.button("🔍 Search", type="primary", disabled=not query):
        with st.spinner("Searching knowledge base..."):
            # Perform search
            source = None if source_filter == "All" else source_filter
            results = rag.search(query, top_k=top_k, source_filter=source, use_llm=use_llm)
            
            if results:
                # Show AI answer if available
                if use_llm and results[0].get('generated_answer'):
                    st.subheader("🤖 AI Answer")
                    st.info(results[0]['generated_answer'])
                    
                    if results[0].get('answer_metadata'):
                        meta = results[0]['answer_metadata']
                        st.caption(f"Generated from {meta.get('chunks_used', 0)} sources")
                
                # Show source chunks
                if show_chunks:
                    st.divider()
                    st.subheader("📚 Source Information")
                    
                    for i, result in enumerate(results):
                        with st.expander(f"Source {i+1}: {Path(result['source_path']).name} (Score: {result.get('similarity', 0):.3f})"):
                            st.markdown(f"**Type:** {result['source_type']}")
                            st.markdown(f"**Path:** `{result['source_path']}`")
                            st.markdown("**Content:**")
                            st.code(result['content'], language="text")
            else:
                st.warning("No results found. Try a different query or check if content is indexed.")
    
    # Sidebar for management
    with st.sidebar:
        st.header("📊 Knowledge Base Management")
        
        # Show chunk distribution
        if stats.get('chunks_by_source'):
            st.subheader("Content Distribution")
            df = pd.DataFrame(stats['chunks_by_source'])
            st.bar_chart(df.set_index('source_type')['count'])
        
        st.divider()
        
        # Quick actions
        st.subheader("Quick Actions")
        
        if st.button("🔄 Refresh Stats"):
            st.cache_resource.clear()
            st.rerun()
        
        # Index management
        with st.expander("Index New Content"):
            st.markdown("""
            Use the CLI to index content:
            ```bash
            # Index directory
            python -m toolkits.llm_rag.cli index-dir /path/to/docs
            
            # Index web docs
            python -m toolkits.llm_rag.cli index-web https://docs.site.com
            ```
            """)
        
        # Help
        st.divider()
        st.markdown("""
        ### 💡 Search Tips
        - Use natural language questions
        - Be specific about what you're looking for
        - Try different phrasings if needed
        - Use source filters for faster results
        """)

# Entry point for navigation
if __name__ == "__main__":
    show()
