"""
RAG Knowledge Base for TuoKit
Clean implementation using RAGLite
"""
import streamlit as st
import sys
from pathlib import Path
import logging

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent.parent))

from toolkits.llm_rag_v2 import TuoKitRAG, get_default_config
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="RAG Knowledge Base - TuoKit",
    page_icon="🔍",
    layout="wide"
)

# Initialize logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Initialize session state
if 'rag_instance' not in st.session_state:
    st.session_state.rag_instance = None
if 'search_history' not in st.session_state:
    st.session_state.search_history = []
if 'indexed_stats' not in st.session_state:
    st.session_state.indexed_stats = None

def initialize_rag():
    """Initialize or get RAG instance"""
    if st.session_state.rag_instance is None:
        with st.spinner("Initializing RAG system..."):
            try:
                # Use selected model from session state
                llm_model = f"ollama/{ModelManager.get_default_model()}"
                config = get_default_config(llm_model=llm_model)
                st.session_state.rag_instance = TuoKitRAG(config)
                st.success("RAG system initialized!")
            except Exception as e:
                st.error(f"Failed to initialize RAG: {e}")
                return None
    return st.session_state.rag_instance

def show():
    """Main page display function"""
    st.title("🔍 TuoKit RAG Knowledge Base")
    st.markdown("Search your codebase and documentation with AI-powered answers")
    
    # Add comprehensive instructions
    with st.expander("📖 **How to Use This Tool** (Click to expand)", expanded=False):
        st.markdown("""
        ### What is RAG?
        **RAG (Retrieval-Augmented Generation)** combines search with AI to answer questions about your codebase:
        1. 🔍 **Searches** your code and docs for relevant information
        2. 🤖 **Understands** the context using AI
        3. 💡 **Generates** accurate answers based on your actual code
        
        ### Getting Started
        
        #### Step 1: Index Your Codebase
        1. Go to the **📚 Index Management** tab
        2. Enter the path to your project (default is current directory ".")
        3. Click **📚 Index Directory** to scan and index all code files
        4. Wait for indexing to complete (shows progress)
        
        #### Step 2: Search and Ask Questions
        1. Go to the **🔍 Search** tab
        2. Type your question in natural language:
           - ❓ "How does the crash analyzer work?"
           - 🐛 "Why am I getting TypeError in my code?"
           - 💡 "Show me examples of error handling"
           - 🔧 "What does the ModelManager class do?"
        
        3. Choose a **Query Type** for better results:
           - **general**: For overview questions
           - **code_explanation**: To understand specific code
           - **debugging**: For error analysis
           - **implementation**: For coding guidance
        
        4. Click **🔍 Search** to get AI-powered answers
        
        ### Understanding Results
        
        - **🤖 AI Answer**: A synthesized explanation based on your code
        - **📚 Source Documents**: The actual code/docs used to generate the answer
        - **Score**: How relevant each source is (higher = more relevant)
        
        ### Advanced Features
        
        - **🔧 Number of results**: Increase to see more sources
        - **🤖 Generate AI answer**: Toggle off for just search results
        - **📄 Show source documents**: See the exact code referenced
        - **📜 History**: Review your previous searches
        
        ### Current Limitations
        
        ⚠️ **Note**: This is currently a search system, not a learning system:
        - It finds information but doesn't learn from your usage
        - Each search is independent (no context between queries)
        - Requires manual reindexing when code changes
        
        ### Tips for Better Results
        
        1. **Be Specific**: "How does crash_analyzer.py handle memory errors?" > "How does it work?"
        2. **Use Technical Terms**: Include class names, function names, error messages
        3. **Try Different Phrasings**: If you don't get good results, rephrase
        4. **Check Sources**: Always verify the AI answer against the source code
        5. **Index Regularly**: Reindex after significant code changes
        """)
    
    # Add status indicator
    if st.session_state.get('rag_instance'):
        if hasattr(st.session_state.rag_instance, 'get_stats'):
            stats = st.session_state.rag_instance.get_stats()
            if 'status' in stats and 'stub' in stats.get('status', ''):
                st.warning("""
                ⚠️ **Running in Demo Mode** - RAGLite dependencies not installed.
                
                To enable full functionality:
                ```bash
                pip install -r toolkits/llm_rag_v2/requirements.txt
                ```
                """)
            else:
                st.success("✅ RAG system fully operational")
    
    # Initialize RAG
    rag = initialize_rag()
    if not rag:
        st.stop()
    
    # Display stats if available
    if st.session_state.indexed_stats:
        stats = st.session_state.indexed_stats
        col1, col2, col3, col4 = st.columns(4)
        with col1:
            st.metric("Files Indexed", stats.get('indexed', 0))
        with col2:
            st.metric("Files Skipped", stats.get('skipped', 0))
        with col3:
            st.metric("Errors", stats.get('errors', 0))
        with col4:
            st.metric("Total Files", stats.get('total_files', 0))
    
    st.divider()
    
    # Main search interface
    search_tab, index_tab, history_tab = st.tabs(["🔍 Search", "📚 Index Management", "📜 History"])
    
    with search_tab:
        # Query input
        col1, col2 = st.columns([3, 1])
        with col1:
            query = st.text_input(
                "Ask a question or search for information",
                placeholder="How does the crash analyzer work?",
                key="search_query"
            )
        with col2:
            query_type = st.selectbox(
                "Query Type",
                ["general", "code_explanation", "debugging", "implementation"],
                help="Helps tailor the AI response"
            )
        
        # Search options
        with st.expander("Advanced Options"):
            col1, col2, col3 = st.columns(3)
            with col1:
                top_k = st.slider("Number of results", 1, 20, 5)
            with col2:
                generate_answer = st.toggle("Generate AI answer", value=True)
            with col3:
                show_sources = st.toggle("Show source documents", value=True)
        
        # Search button
        if st.button("🔍 Search", type="primary", disabled=not query):
            with st.spinner("Searching knowledge base..."):
                try:
                    if generate_answer:
                        # Get answer with sources
                        result = rag.generate_answer(
                            query=query,
                            query_type=query_type,
                            top_k=top_k,
                            include_sources=show_sources
                        )
                        
                        # Display answer
                        st.subheader("🤖 AI Answer")
                        st.markdown(result['answer'])
                        
                        # Add to history
                        st.session_state.search_history.append({
                            'query': query,
                            'answer': result['answer'],
                            'timestamp': result.get('timestamp'),
                            'type': query_type
                        })
                        
                        # Show sources if requested
                        if show_sources and 'sources' in result:
                            st.divider()
                            st.subheader("📚 Source Documents")
                            
                            for i, source in enumerate(result['sources']):
                                with st.expander(
                                    f"{i+1}. {Path(source['source_path']).name} "
                                    f"(Score: {source.get('score', 0):.3f})"
                                ):
                                    # Track interaction
                                    rag.track_interaction(source.get('result_id', f'source_{i}'), 'view_source')
                                    
                                    st.markdown(f"**Path:** `{source['source_path']}`")
                                    st.code(source['content'], language="text")
                                    
                                    # Add feedback buttons
                                    col1, col2, col3 = st.columns([1, 1, 4])
                                    with col1:
                                        if st.button("👍 Helpful", key=f"helpful_{i}_{query}"):
                                            rag.mark_helpful(source.get('result_id', f'source_{i}'))
                                            st.success("Thanks for the feedback!")
                                    with col2:
                                        if st.button("👎 Not helpful", key=f"not_helpful_{i}_{query}"):
                                            rag.mark_not_helpful(source.get('result_id', f'source_{i}'))
                                            st.info("Thanks, we'll improve!")
                    else:
                        # Just search without answer generation
                        results = rag.search(query, top_k=top_k)
                        
                        if results:
                            st.subheader(f"Found {len(results)} results")
                            
                            for i, result in enumerate(results):
                                with st.expander(
                                    f"{i+1}. {Path(result['source_path']).name} "
                                    f"(Score: {result.get('score', 0):.3f})"
                                ):
                                    st.markdown(f"**Path:** `{result['source_path']}`")
                                    st.code(result['content'], language="text")
                        else:
                            st.warning("No results found. Try a different query.")
                            
                except Exception as e:
                    st.error(f"Search error: {e}")
    
    with index_tab:
        st.subheader("📚 Index Management")
        
        # Index directory
        col1, col2 = st.columns([3, 1])
        with col1:
            index_path = st.text_input(
                "Directory to index",
                value=".",
                help="Path to directory containing code and documentation"
            )
        with col2:
            force_reindex = st.checkbox(
                "Force reindex",
                help="Reindex files even if already indexed"
            )
        
        if st.button("📚 Index Directory", type="primary"):
            with st.spinner(f"Indexing {index_path}..."):
                try:
                    stats = rag.index_directory(
                        index_path,
                        force_reindex=force_reindex,
                        show_progress=True
                    )
                    st.session_state.indexed_stats = stats
                    st.success(f"Indexing complete! Indexed {stats['indexed']} files.")
                    st.rerun()
                except Exception as e:
                    st.error(f"Indexing error: {e}")
        
        # Index single file
        st.divider()
        uploaded_file = st.file_uploader(
            "Or upload a single file to index",
            type=['py', 'md', 'txt', 'yaml', 'json']
        )
        
        if uploaded_file and st.button("📄 Index Uploaded File"):
            try:
                # Save uploaded file temporarily
                temp_path = Path(f"/tmp/{uploaded_file.name}")
                temp_path.write_bytes(uploaded_file.getbuffer())
                
                # Index it
                if rag.index_file(temp_path):
                    st.success(f"Successfully indexed {uploaded_file.name}")
                else:
                    st.error("Failed to index file")
                    
                # Clean up
                temp_path.unlink()
            except Exception as e:
                st.error(f"Error indexing uploaded file: {e}")
    
    with history_tab:
        st.subheader("📜 Search History")
        
        if st.session_state.search_history:
            # Clear history button
            if st.button("🗑️ Clear History"):
                st.session_state.search_history = []
                st.rerun()
            
            # Display history
            for item in reversed(st.session_state.search_history):
                with st.expander(
                    f"{item['query']} ({item.get('type', 'general')})"
                ):
                    st.markdown(f"**Time:** {item.get('timestamp', 'Unknown')}")
                    st.markdown(f"**Answer:** {item['answer']}")
        else:
            st.info("No search history yet. Start searching to build your history!")
    
    # Sidebar with information
    with st.sidebar:
        st.header("ℹ️ About RAG")
        st.markdown("""
        **Retrieval-Augmented Generation (RAG)** combines:
        - 🔍 **Search**: Find relevant documents
        - 🧠 **AI**: Generate answers from context
        - 💡 **Knowledge**: Your codebase & docs
        
        **Features:**
        - Hybrid search (vector + keyword)
        - Smart reranking for better results
        - Code-aware chunking
        - Local LLM integration
        """)
        
        st.divider()
        
        # Learning roadmap
        st.subheader("🎯 Future Learning Features")
        st.markdown("""
        **How This Tool Could Learn:**
        
        1. **📊 Usage Analytics**
           - Track which results you click
           - Learn what's most helpful
           - Improve ranking over time
        
        2. **👍 Feedback Loop**
           - Rate answer quality
           - System learns preferences
           - Personalizes results
        
        3. **🔄 Auto-Updates**
           - Detect code changes
           - Update index automatically
           - Learn new patterns
        
        4. **🧠 Context Memory**
           - Remember conversations
           - Understand follow-ups
           - Build knowledge graph
        
        5. **🎨 Pattern Learning**
           - Identify code patterns
           - Learn your style
           - Suggest improvements
        
        **Current Status**: Static search
        **Goal**: Adaptive learning system
        """)
        
        st.divider()
        
        # Popular queries
        if rag:
            popular = rag.get_popular_queries(5)
            if popular:
                st.subheader("🔥 Popular Searches")
                for pq in popular:
                    if st.button(pq, key=f"pop_{pq}"):
                        st.session_state.search_query = pq
                        st.rerun()
        
        st.divider()
        
        # System info
        if rag:
            stats = rag.get_stats()
            st.subheader("📊 System Info")
            
            # Show learning stats prominently
            if 'learning' in stats:
                learning = stats['learning']
                col1, col2 = st.columns(2)
                with col1:
                    st.metric("Total Searches", learning.get('total_queries', 0))
                with col2:
                    st.metric("Unique Queries", learning.get('unique_queries', 0))
                
                if learning.get('helpful_feedback', 0) > 0:
                    st.metric("Helpful Feedback", learning.get('helpful_feedback', 0))
            
            # Show config in expander
            with st.expander("Configuration"):
                st.json(stats)
        
        st.divider()
        
        # Tips
        st.subheader("💡 Search Tips")
        st.markdown("""
        - Use natural language questions
        - Be specific about what you need
        - Try different query types
        - Include technical terms
        - Ask about implementations
        """)

# Entry point
if __name__ == "__main__":
    show()