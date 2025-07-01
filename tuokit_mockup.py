"""
TuoKit - Complete Mockup Implementation
Following TuoKit Architect principles: Build fast, build smart, build exactly what's needed
"""
import streamlit as st
import sqlite3
import subprocess
import json
import os
from datetime import datetime
from pathlib import Path
import pandas as pd

# === CONFIGURATION ===
DB_PATH = "tuokit.db"
OLLAMA_MODELS = ["deepseek-r1:1.5b", "deepseek-coder:6.7b", "llama3.2:latest"]
DEFAULT_MODEL = "deepseek-r1:1.5b"

# === DATABASE SETUP ===
def init_database():
    """Initialize SQLite database with minimal schema"""
    conn = sqlite3.connect(DB_PATH)
    c = conn.cursor()
    
    # Single unified knowledge table - avoid over-normalization
    c.execute('''CREATE TABLE IF NOT EXISTS knowledge (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
        tool TEXT NOT NULL,
        model TEXT NOT NULL,
        prompt TEXT NOT NULL,
        response TEXT NOT NULL,
        context TEXT,
        rating INTEGER DEFAULT 0
    )''')
    
    conn.commit()
    conn.close()

# === OLLAMA UTILITIES ===
def query_ollama(prompt, model=DEFAULT_MODEL):
    """Simple Ollama query with error handling"""
    try:
        cmd = ["ollama", "run", model]
        result = subprocess.run(cmd, input=prompt, capture_output=True, text=True)
        if result.returncode == 0:
            return result.stdout.strip()
        else:
            return f"Error: {result.stderr}"
    except Exception as e:
        return f"Ollama connection failed: {str(e)}"

def check_ollama_status():
    """Check if Ollama is running and which models are available"""
    try:
        result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
        if result.returncode == 0:
            return True, result.stdout
        return False, "Ollama not responding"
    except:
        return False, "Ollama not installed"
# === KNOWLEDGE MANAGEMENT ===
def save_to_knowledge(tool, prompt, response, context="", model=DEFAULT_MODEL):
    """Automatic knowledge capture for all tools"""
    conn = sqlite3.connect(DB_PATH)
    c = conn.cursor()
    c.execute('''INSERT INTO knowledge (tool, model, prompt, response, context) 
                 VALUES (?, ?, ?, ?, ?)''', 
                 (tool, model, prompt, response, context))
    conn.commit()
    conn.close()

def get_knowledge_stats():
    """Simple knowledge base statistics"""
    conn = sqlite3.connect(DB_PATH)
    df = pd.read_sql_query('''
        SELECT tool, COUNT(*) as queries, 
               AVG(LENGTH(response)) as avg_response_length
        FROM knowledge 
        GROUP BY tool
    ''', conn)
    conn.close()
    return df

# === STREAMLIT PAGE CONFIG ===
st.set_page_config(
    page_title="TuoKit - AI Developer Assistant",
    page_icon="🛠️",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Initialize database
init_database()
# === MAIN NAVIGATION ===
st.title("🛠️ TuoKit - AI Developer Assistant")
st.caption("Build fast, build smart, build exactly what's needed")

# Sidebar configuration
with st.sidebar:
    st.header("Configuration")
    
    # Model selection
    selected_model = st.selectbox(
        "AI Model",
        OLLAMA_MODELS,
        index=0,
        help="Select which Ollama model to use"
    )
    
    # Ollama status
    if st.button("🔍 Check Ollama Status"):
        status, message = check_ollama_status()
        if status:
            st.success("✅ Ollama is running")
            st.code(message)
        else:
            st.error("❌ " + message)
    
    # Knowledge stats
    st.divider()
    st.subheader("📊 Knowledge Base")
    if st.button("Refresh Stats"):
        stats = get_knowledge_stats()
        if not stats.empty:
            st.dataframe(stats)
        else:
            st.info("No knowledge captured yet")
# === MAIN INTERFACE WITH TABS ===
tab1, tab2, tab3, tab4, tab5 = st.tabs([
    "🏠 Dashboard", 
    "💬 Code Assistant", 
    "📄 Document Q&A", 
    "🗄️ SQL Generator", 
    "🎓 Learning Mode"
])

# === TAB 1: DASHBOARD ===
with tab1:
    col1, col2, col3 = st.columns(3)
    
    with col1:
        st.metric("Total Queries", "0")
    with col2:
        st.metric("Active Model", selected_model.split(":")[0])
    with col3:
        st.metric("Knowledge Items", "0")
    
    st.subheader("Quick Start Guide")
    st.markdown("""
    1. **Code Assistant**: Get help with coding problems
    2. **Document Q&A**: Upload and query documents
    3. **SQL Generator**: Convert natural language to SQL
    4. **Learning Mode**: Interactive tutorials and examples
    
    All interactions are automatically saved to the knowledge base!
    """)
    
    # Recent activity
    st.subheader("Recent Activity")
    conn = sqlite3.connect(DB_PATH)
    recent = pd.read_sql_query(
        "SELECT timestamp, tool, prompt FROM knowledge ORDER BY timestamp DESC LIMIT 5", 
        conn
    )
    conn.close()
    
    if not recent.empty:
        st.dataframe(recent, use_container_width=True)
    else:
        st.info("No recent activity. Try one of the tools!")
# === TAB 2: CODE ASSISTANT ===
with tab2:
    st.header("💬 Code Assistant")
    st.caption("Get AI help with your coding problems")
    
    # Code input options
    input_method = st.radio("Input Method", ["Write Code", "Paste Code", "Describe Problem"])
    
    if input_method == "Write Code":
        language = st.selectbox("Language", ["Python", "JavaScript", "SQL", "Other"])
        code_input = st.text_area("Your Code", height=200, placeholder="Paste your code here...")
        
    elif input_method == "Paste Code":
        code_input = st.text_area("Paste Code", height=200, placeholder="Paste code to analyze...")
        
    else:  # Describe Problem
        code_input = st.text_area("Describe Your Problem", height=100, 
                                placeholder="What are you trying to build?")
    
    # Query type
    query_type = st.selectbox("What do you need?", 
        ["Explain this code", "Fix errors", "Add features", "Optimize performance", "Write tests"])
    
    if st.button("🚀 Get AI Help", type="primary", use_container_width=True):
        if code_input:
            with st.spinner("AI is thinking..."):
                # Build context-aware prompt
                prompt = f"""
                Task: {query_type}
                Code/Problem: {code_input}
                
                Please provide a clear, practical response with code examples if needed.
                """
                
                response = query_ollama(prompt, selected_model)
                
                # Display response
                st.subheader("AI Response")
                st.markdown(response)
                
                # Save to knowledge base
                save_to_knowledge("code_assistant", code_input[:100], response, query_type, selected_model)
                st.success("✅ Saved to knowledge base!")
        else:
            st.warning("Please provide some code or describe your problem")
# === TAB 3: DOCUMENT Q&A ===
with tab3:
    st.header("📄 Document Q&A")
    st.caption("Upload documents and ask questions about them")
    
    # File upload
    uploaded_file = st.file_uploader("Choose a file", type=['txt', 'md', 'py', 'js', 'json'])
    
    if uploaded_file:
        # Display file info
        st.success(f"Uploaded: {uploaded_file.name} ({uploaded_file.size} bytes)")
        
        # Read file content
        try:
            content = uploaded_file.read().decode('utf-8')
            
            # Show preview
            with st.expander("Document Preview"):
                st.text(content[:500] + "..." if len(content) > 500 else content)
            
            # Question interface
            question = st.text_input("Ask a question about this document:")
            
            if st.button("🔍 Get Answer", type="primary", use_container_width=True):
                if question:
                    with st.spinner("Analyzing document..."):
                        # Precise prompt to prevent hallucination
                        prompt = f"""
                        Based ONLY on this document content:
                        ---
                        {content[:3000]}
                        ---
                        
                        Question: {question}
                        
                        Answer based only on the document. If the answer isn't in the document, say so.
                        """
                        
                        response = query_ollama(prompt, selected_model)
                        
                        st.subheader("Answer")
                        st.markdown(response)
                        
                        # Save to knowledge
                        save_to_knowledge("document_qa", question, response, 
                                        uploaded_file.name, selected_model)
                else:
                    st.warning("Please ask a question")
                    
        except Exception as e:
            st.error(f"Error reading file: {e}")
# === TAB 4: SQL GENERATOR ===
with tab4:
    st.header("🗄️ SQL Generator")
    st.caption("Convert natural language to SQL queries")
    
    # Database schema input
    st.subheader("Database Schema")
    schema = st.text_area("Describe your tables (or paste CREATE statements):", 
        placeholder="users table: id, name, email, created_at\norders table: id, user_id, amount, status",
        height=150)
    
    # Natural language query
    st.subheader("What do you want to query?")
    nl_query = st.text_input("Describe in plain English:", 
        placeholder="Show me all users who made orders over $100 last month")
    
    # SQL dialect
    dialect = st.selectbox("SQL Dialect", ["PostgreSQL", "MySQL", "SQLite", "SQL Server"])
    
    if st.button("🔮 Generate SQL", type="primary", use_container_width=True):
        if schema and nl_query:
            with st.spinner("Generating SQL..."):
                prompt = f"""
                Database Schema:
                {schema}
                
                Task: Convert this natural language query to {dialect} SQL:
                "{nl_query}"
                
                Provide only the SQL query, with proper formatting.
                Include comments explaining complex parts.
                """
                
                response = query_ollama(prompt, selected_model)
                
                st.subheader("Generated SQL")
                st.code(response, language="sql")
                
                # Copy button
                st.button("📋 Copy SQL", on_click=lambda: st.write("Copied!"))
                
                # Save to knowledge
                save_to_knowledge("sql_generator", nl_query, response, 
                                f"Schema: {schema[:50]}...", selected_model)
        else:
            st.warning("Please provide both schema and query description")
# === TAB 5: LEARNING MODE ===
with tab5:
    st.header("🎓 Learning Mode")
    st.caption("Interactive tutorials and code examples")
    
    # Topic selection
    topic = st.selectbox("Choose a Topic", [
        "Python Basics",
        "Web Development",
        "Database Design",
        "API Development",
        "Testing Best Practices",
        "Performance Optimization"
    ])
    
    # Difficulty level
    level = st.radio("Your Level", ["Beginner", "Intermediate", "Advanced"], horizontal=True)
    
    # Generate lesson
    if st.button("📖 Generate Lesson", type="primary", use_container_width=True):
        with st.spinner("Creating personalized lesson..."):
            prompt = f"""
            Create a short, practical lesson on "{topic}" for a {level} developer.
            Include:
            1. Key concept explanation
            2. Code example
            3. Common mistake to avoid
            4. Practice exercise
            
            Keep it concise and practical.
            """
            
            response = query_ollama(prompt, selected_model)
            
            st.subheader(f"Lesson: {topic}")
            st.markdown(response)
            
            # Interactive practice
            with st.expander("Try it yourself"):
                user_code = st.text_area("Write your code here:", height=150)
                if st.button("Check My Solution"):
                    check_prompt = f"Review this {topic} code and provide feedback: {user_code}"
                    feedback = query_ollama(check_prompt, selected_model)
                    st.markdown("**Feedback:**")
                    st.markdown(feedback)
            
            # Save lesson
            save_to_knowledge("learning_mode", topic, response, 
                            f"Level: {level}", selected_model)
# === FOOTER & KNOWLEDGE EXPORT ===
st.divider()

col1, col2, col3 = st.columns(3)

with col1:
    st.caption("TuoKit v1.0 - Practical AI Tools")

with col2:
    # Export knowledge base
    if st.button("📥 Export Knowledge"):
        conn = sqlite3.connect(DB_PATH)
        df = pd.read_sql_query("SELECT * FROM knowledge", conn)
        conn.close()
        
        csv = df.to_csv(index=False)
        st.download_button(
            label="Download CSV",
            data=csv,
            file_name=f"tuokit_knowledge_{datetime.now().strftime('%Y%m%d')}.csv",
            mime="text/csv"
        )

with col3:
    # Clear knowledge (with confirmation)
    if st.button("🗑️ Clear Knowledge", type="secondary"):
        if st.checkbox("Confirm deletion"):
            conn = sqlite3.connect(DB_PATH)
            conn.execute("DELETE FROM knowledge")
            conn.commit()
            conn.close()
            st.rerun()

# === ERROR HANDLING WRAPPER ===
if __name__ == "__main__":
    try:
        # Check Ollama on startup
        status, _ = check_ollama_status()
        if not status:
            st.error("⚠️ Ollama is not running. Please start Ollama to use TuoKit.")
            st.code("ollama serve", language="bash")
    except Exception as e:
        st.error(f"Startup error: {e}")

# TODO: Add vector search when document volume increases
# TODO: Implement session memory for multi-turn conversations
# TODO: Add PostgreSQL option for production deployment