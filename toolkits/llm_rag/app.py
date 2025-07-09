"""
TuoKit RAG Interface - Streamlit App
Simple UI for knowledge base management and search
"""
import streamlit as st
from pathlib import Path
import sys
import os

# Add parent directory to path
sys.path.append(str(Path(__file__).parent))

from rag_manager import TuoKitRAGManager
import pandas as pd

# Page config
st.set_page_config(
    page_title="TuoKit RAG System",
    page_icon="🧠",
    layout="wide"
)

# Initialize RAG manager
@st.cache_resource
def get_rag_manager():
    return TuoKitRAGManager(config_path="config.yaml")

def main():
    st.title("🧠 TuoKit Knowledge Base RAG System")
    
    rag = get_rag_manager()
    
    # Sidebar for management
    with st.sidebar:
        st.header("📊 System Stats")
        stats = rag.get_stats()
        st.metric("Indexed Files", stats.get('indexed_files', 0))
        st.metric("Indexed URLs", stats.get('indexed_urls', 0))
        
        if 'chunks_by_source' in stats:
            st.subheader("Chunks by Source")
            df = pd.DataFrame(stats['chunks_by_source'])
            st.dataframe(df)
        
        st.divider()
        
        # Index management
        st.header("🔧 Index Management")
        
        if st.button("🔄 Refresh Stats"):
            st.rerun()
        
        # Index new content
        with st.expander("📁 Index Local Directory"):
            dir_path = st.text_input("Directory Path", 
                                   value="C:/Projects/TuoKit/src")
            pattern = st.text_input("File Pattern", value="**/*.py")
            
            if st.button("Index Directory"):
                with st.spinner("Indexing..."):
                    results = rag.index_directory(dir_path, pattern)
                    st.success(f"Indexed {results['processed']} files, "
                             f"created {results['chunks_created']} chunks")
                    if results['errors']:
                        st.error(f"Errors: {len(results['errors'])} files failed")
        
        with st.expander("🌐 Index Web Documentation"):
            url = st.text_input("Documentation URL", 
                              placeholder="https://docs.example.com")
            max_pages = st.number_input("Max Pages", value=50, min_value=1)
            
            if st.button("Scrape & Index"):
                with st.spinner("Scraping and indexing..."):
                    results = rag.index_web_docs(url, max_pages)
                    st.success(f"Scraped {results['pages_scraped']} pages, "
                             f"created {results['chunks_created']} chunks")
        
        if st.button("🗑️ Clear Index", type="secondary"):
            if st.checkbox("I understand this will delete all indexed data"):
                rag.clear_index(confirm=True)
                st.success("Index cleared!")
                st.rerun()
    
    # Main search interface
    st.header("🔍 Search Knowledge Base")
    
    col1, col2, col3 = st.columns([3, 1, 1])
    
    with col1:
        query = st.text_input("Ask a question...", 
                            placeholder="How does the authentication system work?")
    
    with col2:
        source_filter = st.selectbox("Filter by source", 
                                   ["All", "code", "documentation", "release_notes"])
    
    with col3:
        top_k = st.number_input("Results", value=5, min_value=1, max_value=20)
    
    if query:
        with st.spinner("Searching..."):
            results = rag.search(
                query, 
                top_k=top_k,
                source_filter=None if source_filter == "All" else source_filter,
                use_llm=False  # TODO: Integrate LLM for answer generation
            )
        
        if results:
            st.subheader(f"Found {len(results)} relevant chunks")
            
            for i, result in enumerate(results):
                similarity = result.get('similarity', 0)
                
                # Color code by similarity
                if similarity > 0.8:
                    color = "🟢"
                elif similarity > 0.6:
                    color = "🟡"
                else:
                    color = "🔴"
                
                with st.expander(
                    f"{color} Result {i+1} - {result['source_type']} "
                    f"(similarity: {similarity:.2f})"
                ):
                    # Show content
                    st.markdown("**Content:**")
                    st.text(result['content'][:500] + "..." 
                           if len(result['content']) > 500 else result['content'])
                    
                    # Show metadata
                    col1, col2 = st.columns(2)
                    with col1:
                        st.caption(f"**Source:** {result['source_path']}")
                    with col2:
                        if result.get('metadata'):
                            st.caption(f"**Page:** {result['metadata'].get('page', 'N/A')}")
                    
                    # Copy button for full content
                    if st.button(f"📋 Copy Full Content", key=f"copy_{i}"):
                        st.code(result['content'], language="text")
        else:
            st.info("No results found. Try a different query or index more content.")
    
    # Footer
    st.divider()
    st.caption("TuoKit RAG System - Built with llm-search and PostgreSQL")

if __name__ == "__main__":
    main()
