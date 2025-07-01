import streamlit as st
from utils import DatabaseManager, safe_ollama_generate
import re
import tempfile
import os
from pathlib import Path

# Page configuration
st.set_page_config(
    page_title="TuoKit - Document Tools",
    page_icon="📄",
    layout="wide"
)

def extract_text(uploaded_file):
    """Extract text from uploaded file with format handling"""
    filename = uploaded_file.name.lower()
    
    # Text file processing
    if filename.endswith('.txt'):
        return uploaded_file.read().decode('utf-8')
    
    # PDF processing
    elif filename.endswith('.pdf'):
        try:
            # Try PyMuPDF first (faster)
            import fitz
            doc = fitz.open(stream=uploaded_file.read(), filetype="pdf")
            text = ""
            for page in doc:
                text += page.get_text()
            return text        except ImportError:
            # Fallback to PyPDF2
            try:
                from PyPDF2 import PdfReader
                reader = PdfReader(uploaded_file)
                text = ""
                for page in reader.pages:
                    text += page.extract_text() + "\n"
                return text
            except Exception as e:
                st.error(f"Error reading PDF: {e}")
                return None
    
    # Unsupported format
    else:
        st.error("Unsupported file format. Please upload PDF or TXT.")
        return None

def summarize_document(text, model):
    """Generate concise document summary"""
    prompt = f"""
    Create a comprehensive summary of this document with:
    1. 3-5 key points
    2. Main conclusions
    3. Action items (if any)
    
    Document excerpt:
    {text[:5000]}... [truncated]
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return response['response']
def answer_document_question(text, question, model):
    """Answer question based on document content"""
    prompt = f"""
    Answer this question based ONLY on the provided document:
    Question: {question}
    
    Document context:
    {text[:5000]}... [truncated]
    
    If the answer isn't in the document, say "I couldn't find this information in the document."
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return response['response']

def extract_knowledge(text, model):
    """Extract structured knowledge from document"""
    prompt = f"""
    Extract key information from this document as structured JSON:
    {{
        "key_topics": ["list", "of", "topics"],
        "important_dates": ["YYYY-MM-DD"],
        "decisions_made": ["list", "of", "decisions"],
        "action_items": [
            {{"task": "description", "owner": "name", "due_date": "YYYY-MM-DD"}}
        ]
    }}
    
    Document excerpt:
    {text[:3000]}... [truncated]
    """
    response = safe_ollama_generate(model=model, prompt=prompt)
    return response['response']
# Initialize session state for database
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

# Main content
st.title("📄 Document Tools")
st.caption("Analyze and query your documents with AI")

# File upload section
uploaded_file = st.file_uploader("Upload document (PDF or TXT)", 
                                type=['pdf', 'txt'])

# Initialize state variables
if 'doc_text' not in st.session_state:
    st.session_state.doc_text = None
if 'doc_summary' not in st.session_state:
    st.session_state.doc_summary = None

# Model selection
model = st.selectbox("AI Model", 
                    ["deepseek-r1:6.7b", "deepseek-r1:1.5b"],
                    index=0)

# Process document on upload
if uploaded_file is not None:
    with st.spinner("Extracting text..."):
        st.session_state.doc_text = extract_text(uploaded_file)    
    if st.session_state.doc_text:
        st.success(f"Extracted {len(st.session_state.doc_text)} characters")
        st.caption(f"Document preview: {st.session_state.doc_text[:200]}...")

# Tool selection
tool = st.radio("Select Tool:", 
               ["Q&A", "Summarize", "Extract Knowledge"], 
               horizontal=True,
               disabled=st.session_state.doc_text is None)

# Q&A Tool
if tool == "Q&A" and st.session_state.doc_text:
    st.subheader("Document Question Answering")
    question = st.text_input("Ask about the document", 
                            placeholder="What were the main findings?")
    
    if st.button("Get Answer", disabled=not question.strip()):
        try:
            with st.spinner("Analyzing document..."):
                answer = answer_document_question(
                    st.session_state.doc_text, 
                    question, 
                    model
                )
            
            st.subheader("Answer")
            st.write(answer)
            
            # Log to database
            if st.session_state.db:
                query_id = st.session_state.db.log_query(                    tool="doc_qa",
                    model=model,
                    prompt=f"Document: {uploaded_file.name}\nQuestion: {question}",
                    response=answer
                )
                st.session_state.last_query_id = query_id
                st.success("✅ Answer saved to knowledge base")
        except Exception as e:
            st.error(f"Error: {e}")

# Summarization Tool
elif tool == "Summarize" and st.session_state.doc_text:
    st.subheader("Document Summarization")
    
    if st.button("Generate Summary"):
        try:
            with st.spinner("Creating concise summary..."):
                summary = summarize_document(
                    st.session_state.doc_text, 
                    model
                )
                st.session_state.doc_summary = summary
            
            st.subheader("Summary")
            st.write(summary)
            
            # Log to database
            if st.session_state.db:
                query_id = st.session_state.db.log_query(
                    tool="doc_summary",
                    model=model,                    prompt=f"Document: {uploaded_file.name}",
                    response=summary
                )
                st.session_state.last_query_id = query_id
                st.success("✅ Summary saved to knowledge base")
            
            # Download button
            st.download_button(
                label="Download Summary",
                data=summary,
                file_name=f"{Path(uploaded_file.name).stem}_summary.txt",
                mime="text/plain"
            )
        except Exception as e:
            st.error(f"Error: {e}")

# Knowledge Extraction Tool
elif tool == "Extract Knowledge" and st.session_state.doc_text:
    st.subheader("Knowledge Extraction")
    st.info("Extracts structured data: key topics, dates, decisions, and action items")
    
    if st.button("Extract Knowledge"):
        try:
            with st.spinner("Identifying key information..."):
                knowledge = extract_knowledge(
                    st.session_state.doc_text, 
                    model
                )
            
            # Try to parse JSON output
            try:                import json
                parsed = json.loads(knowledge)
                st.subheader("Extracted Knowledge")
                st.json(parsed)
                output = json.dumps(parsed, indent=2)
            except:
                st.warning("Couldn't parse JSON output. Showing raw response:")
                st.code(knowledge)
                output = knowledge
            
            # Log to database
            if st.session_state.db:
                query_id = st.session_state.db.log_query(
                    tool="doc_knowledge",
                    model=model,
                    prompt=f"Document: {uploaded_file.name}",
                    response=knowledge
                )
                st.session_state.last_query_id = query_id
                st.success("✅ Knowledge extraction saved")
            
            # Download button
            st.download_button(
                label="Download as JSON",
                data=output,
                file_name=f"{Path(uploaded_file.name).stem}_knowledge.json",
                mime="application/json"
            )
        except Exception as e:
            st.error(f"Error: {e}")
# Knowledge saving component
if 'last_query_id' in st.session_state and st.session_state.doc_text and st.session_state.db:
    st.divider()
    with st.expander("💾 Save to Knowledge Base"):
        col1, col2 = st.columns(2)
        with col1:
            title = st.text_input("Title", value=f"{tool} - {uploaded_file.name}")
        with col2:
            category = st.selectbox("Category", 
                                   ["Document Summary", "Research Findings", 
                                    "Meeting Notes", "Technical Documentation"])
        
        if st.button("Save to Library"):
            try:
                # Get last response
                last_query = st.session_state.db.get_query_by_id(st.session_state.last_query_id)
                if last_query:
                    content = last_query[3]  # ai_response field
                    
                    st.session_state.db.save_knowledge_unit(
                        query_id=st.session_state.last_query_id,
                        title=title,
                        content=content,
                        category=category
                    )
                    st.success(f"Saved to knowledge base as {category}!")
            except Exception as e:
                st.error(f"Error saving: {e}")

# Back to dashboard
st.divider()
col1, col2 = st.columns([1, 1])
with col1:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col2:
    if st.button("❓ Help", use_container_width=True):
        st.switch_page("pages/help_guide.py")