"""
Document Q&A with Vector Search Integration
Enhanced version showing how to integrate vector search
"""
import streamlit as st
import sqlite3
from datetime import datetime
from utils.vector_search import (
    check_enable_vector_search,
    enhanced_document_search,
    show_vector_search_status,
    update_vector_index
)

def show_document_qa_enhanced():
    """Enhanced Document Q&A with vector search capabilities"""
    
    st.header("📄 Document Q&A")
    st.caption("Upload documents and ask questions about them")
    
    # Show vector search status
    vector_enabled, doc_count = check_enable_vector_search()
    if doc_count > 0:
        show_vector_search_status()
    
    # File upload
    uploaded_file = st.file_uploader(
        "Choose a file", 
        type=['txt', 'md', 'py', 'js', 'json', 'pdf']
    )
    
    if uploaded_file:
        # Display file info
        st.success(f"✅ Uploaded: {uploaded_file.name} ({uploaded_file.size} bytes)")
        
        # Read file content
        try:
            if uploaded_file.type == "application/pdf":
                # Handle PDF files
                import PyPDF2
                import io
                
                pdf_reader = PyPDF2.PdfReader(io.BytesIO(uploaded_file.read()))
                content = ""
                for page in pdf_reader.pages:
                    content += page.extract_text() + "\n"
            else:
                # Handle text files
                content = uploaded_file.read().decode('utf-8')
            
            # Show preview
            with st.expander("📋 Document Preview"):
                st.text(content[:500] + "..." if len(content) > 500 else content)
            
            # Store document in session for this conversation
            st.session_state.current_document = {
                'name': uploaded_file.name,
                'content': content,
                'timestamp': datetime.now()
            }
            
            # Question interface
            col1, col2 = st.columns([4, 1])
            
            with col1:
                question = st.text_input(
                    "Ask a question about this document:",
                    placeholder="What is the main topic of this document?"
                )
            
            with col2:
                search_mode = st.selectbox(
                    "Search Mode",
                    ["Current Doc", "All Docs"] if vector_enabled else ["Current Doc"],
                    help="Search in current document only or across all documents"
                )
            
            if st.button("🔍 Get Answer", type="primary", use_container_width=True):
                if question:
                    with st.spinner("🤖 Analyzing document..."):
                        
                        if search_mode == "All Docs" and vector_enabled:
                            # Use vector search to find relevant documents
                            st.info("🔎 Searching across all documents...")
                            
                            # Get similar documents
                            similar_docs = enhanced_document_search(question, use_vector=True)
                            
                            # Build context from similar documents
                            context_parts = [content[:3000]]  # Current document
                            
                            if similar_docs:
                                st.markdown("**📚 Found relevant documents:**")
                                for doc in similar_docs[:3]:
                                    st.caption(f"- {doc['source']} (similarity: {doc.get('score', 0):.2f})")
                                    context_parts.append(doc['response'][:1000])
                            
                            full_context = "\n\n---\n\n".join(context_parts)
                        else:
                            # Use only current document
                            full_context = content[:3000]
                        
                        # Build prompt
                        prompt = f"""
                        Based ONLY on the following document content:
                        ---
                        {full_context}
                        ---
                        
                        Question: {question}
                        
                        Answer based only on the document. If the answer isn't in the document, say so.
                        """
                        
                        # Query AI model
                        response = query_ollama(prompt, st.session_state.get('selected_model', 'deepseek-r1:1.5b'))
                        
                        # Display answer
                        st.markdown("### 🎯 Answer")
                        st.markdown(response)
                        
                        # Save to knowledge base
                        save_to_knowledge_with_vector(
                            "document_qa", 
                            question, 
                            response, 
                            uploaded_file.name,
                            st.session_state.get('selected_model', 'deepseek-r1:1.5b'),
                            content
                        )
                        
                        # Show action buttons
                        col1, col2, col3 = st.columns(3)
                        with col1:
                            if st.button("💾 Save Answer", use_container_width=True):
                                st.success("✅ Saved to knowledge base!")
                        with col2:
                            if st.button("📋 Copy", use_container_width=True):
                                st.write("📋 Copied to clipboard!")
                        with col3:
                            if st.button("🔄 Ask Another", use_container_width=True):
                                st.experimental_rerun()
                        
                        # Show related questions
                        if vector_enabled:
                            st.markdown("### 💡 Related Questions")
                            related = get_related_questions(question)
                            for q in related[:3]:
                                if st.button(f"❓ {q}", use_container_width=True):
                                    st.session_state.next_question = q
                                    st.experimental_rerun()
                else:
                    st.warning("Please ask a question")
                    
        except Exception as e:
            st.error(f"Error reading file: {e}")
            st.info("💡 Try uploading a different file format")

def save_to_knowledge_with_vector(tool, question, response, filename, model, content):
    """Save to knowledge base and update vector index if enabled"""
    # Traditional save
    conn = sqlite3.connect("tuokit.db")
    cursor = conn.execute("""
        INSERT INTO knowledge (tool, prompt, response, context, model)
        VALUES (?, ?, ?, ?, ?)
        RETURNING id
    """, (tool, question, response, filename, model))
    
    doc_id = cursor.fetchone()[0]
    conn.commit()
    conn.close()
    
    # Update vector index if enabled
    vector_enabled, _ = check_enable_vector_search()
    if vector_enabled:
        # Prepare document for vector indexing
        full_text = f"{question}\n\n{response}"
        metadata = {
            'source': filename,
            'timestamp': datetime.now().isoformat(),
            'type': 'document_qa',
            'model': model
        }
        
        update_vector_index(f"doc_{doc_id}", full_text, metadata)

def get_related_questions(question):
    """Get related questions from knowledge base"""
    conn = sqlite3.connect("tuokit.db")
    
    # Simple keyword-based related questions
    # In production, this would use vector similarity
    keywords = question.lower().split()
    
    conditions = " OR ".join([f"prompt LIKE ?" for _ in keywords])
    params = [f"%{keyword}%" for keyword in keywords]
    
    cursor = conn.execute(f"""
        SELECT DISTINCT prompt
        FROM knowledge
        WHERE tool = 'document_qa'
        AND ({conditions})
        AND prompt != ?
        ORDER BY timestamp DESC
        LIMIT 5
    """, params + [question])
    
    related = [row[0] for row in cursor.fetchall()]
    conn.close()
    
    return related

def query_ollama(prompt, model):
    """Query Ollama model (mock implementation)"""
    import time
    time.sleep(2)  # Simulate processing
    
    # Mock response
    return """Based on the document provided, I can see that this is about implementing vector search functionality in TuoKit.

The key points from the document are:

1. **Vector search activates automatically** when document volume exceeds 50 documents
2. **Implementation uses a threshold-based approach** to avoid unnecessary complexity for small document sets
3. **The system is designed to scale** - starting with TF-IDF and upgrading to ChromaDB/FAISS when needed

The document indicates that vector search is not enabled by default but monitors document count and provides clear indicators when it should be activated.

This approach follows the TuoKit philosophy of "build exactly what's needed" by only adding complexity when the document volume justifies it."""

# Example usage in main app
if __name__ == "__main__":
    st.set_page_config(page_title="Document Q&A - TuoKit", layout="wide")
    
    # Initialize session state
    if 'selected_model' not in st.session_state:
        st.session_state.selected_model = 'deepseek-r1:1.5b'
    
    show_document_qa_enhanced()
