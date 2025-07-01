"""
TuoKit Complete - Tool Interface Examples
Shows how individual tools work within the unified platform
"""
import streamlit as st

# === SQL GENERATOR TOOL EXAMPLE ===
def sql_generator_interface():
    st.header("🔮 SQL Generator")
    st.caption("Convert natural language to optimized SQL queries")
    
    # Two-column layout
    input_col, output_col = st.columns([1, 1])
    
    with input_col:
        st.subheader("Input")
        
        # Database schema
        schema = st.text_area(
            "Database Schema",
            placeholder="users: id, name, email, created_at\norders: id, user_id, amount, status",
            height=100
        )
        
        # Natural language query
        query = st.text_input(
            "What do you want to query?",
            placeholder="Show me all users who placed orders over $100 last month"
        )
        
        # Advanced options
        with st.expander("Advanced Options"):
            dialect = st.selectbox("SQL Dialect", ["PostgreSQL", "MySQL", "Oracle", "SQL Server"])
            optimize = st.checkbox("Auto-optimize query", value=True)
            explain = st.checkbox("Include EXPLAIN plan", value=False)
            security = st.checkbox("Security hardening", value=True)
        
        # Generate button
        if st.button("Generate SQL", type="primary", use_container_width=True):
            st.session_state.generated_sql = True
    
    with output_col:
        st.subheader("Output")
        
        if st.session_state.get('generated_sql'):
            # Generated SQL
            st.code("""
-- Generated SQL for PostgreSQL
-- Query: Users with orders over $100 last month

WITH user_orders AS (
    SELECT 
        u.id,
        u.name,
        u.email,
        COUNT(o.id) as order_count,
        SUM(o.amount) as total_amount
    FROM users u
    INNER JOIN orders o ON u.id = o.user_id
    WHERE o.amount > 100
        AND o.created_at >= DATE_TRUNC('month', CURRENT_DATE - INTERVAL '1 month')
        AND o.created_at < DATE_TRUNC('month', CURRENT_DATE)
    GROUP BY u.id, u.name, u.email
)
SELECT * FROM user_orders
ORDER BY total_amount DESC;
            """, language="sql")
            
            # Action buttons
            col1, col2, col3 = st.columns(3)
            with col1:
                st.button("📋 Copy", use_container_width=True)
            with col2:
                st.button("🔍 Optimize", use_container_width=True)
            with col3:
                st.button("💾 Save", use_container_width=True)
            
            # Additional features
            st.info("✅ Query validated • 🔒 SQL injection safe • ⚡ Optimized for performance")

# === ERROR DECODER TOOL EXAMPLE ===
def error_decoder_interface():
    st.header("🐞 Advanced Error Decoder")
    st.caption("Understand and fix any programming error")
    
    # Error input
    error_text = st.text_area(
        "Paste your error message:",
        placeholder="TypeError: Cannot read property 'map' of undefined...",
        height=150
    )
    
    # Language detection
    col1, col2, col3 = st.columns([2, 1, 1])
    with col1:
        language = st.selectbox("Language", ["Auto-detect", "Python", "JavaScript", "Ruby", "Java"])
    with col2:
        depth = st.select_slider("Analysis Depth", ["Quick", "Standard", "Deep"])
    with col3:
        if st.button("🔍 Analyze", type="primary"):
            st.session_state.error_analyzed = True
    
    if st.session_state.get('error_analyzed'):
        # Analysis results
        st.divider()
        
        # Error summary
        st.error("TypeError: Cannot read property 'map' of undefined")
        
        # Tabs for different views
        tabs = st.tabs(["🎯 Quick Fix", "📖 Explanation", "🔧 Solution", "📚 Learn More"])
        
        with tabs[0]:
            st.markdown("""
            ### Quick Fix
            Add a null check before using `.map()`:
            ```javascript
            // Instead of:
            data.map(item => item.name)
            
            // Use:
            data?.map(item => item.name) || []
            // or
            (data || []).map(item => item.name)
            ```
            """)
        
        with tabs[1]:
            st.markdown("""
            ### What's Happening?
            You're trying to use the `.map()` method on a variable that is `undefined`. 
            This usually happens when:
            - Data hasn't loaded yet (async issue)
            - API returned null/undefined
            - Variable wasn't initialized
            """)
        
        with tabs[2]:
            st.code("""
// Complete solution with error handling
function processData(data) {
    // Check if data exists and is an array
    if (!Array.isArray(data)) {
        console.warn('Data is not an array:', data);
        return [];
    }
    
    return data.map(item => ({
        name: item.name || 'Unknown',
        value: item.value || 0
    }));
}
            """, language="javascript")
        
        with tabs[3]:
            st.markdown("""
            ### Related Concepts
            - [Optional Chaining in JavaScript](/)
            - [Defensive Programming Practices](/)
            - [Handling Async Data](/)
            """)

# === STUDY GUIDE GENERATOR EXAMPLE ===
def study_guide_interface():
    st.header("📚 Study Guide Generator")
    st.caption("Create comprehensive study materials from any content")
    
    # Input method
    input_method = st.radio("Choose input method:", ["Text", "File Upload", "URL"], horizontal=True)
    
    if input_method == "Text":
        content = st.text_area("Paste your content:", height=200)
    elif input_method == "File Upload":
        file = st.file_uploader("Upload document", type=["pdf", "txt", "docx"])
    else:
        url = st.text_input("Enter URL:")
    
    # Generation options
    st.subheader("Study Materials to Generate")
    col1, col2 = st.columns(2)
    
    with col1:
        summary = st.checkbox("📝 Summary", value=True)
        flashcards = st.checkbox("🎴 Flashcards", value=True)
        quiz = st.checkbox("❓ Quiz Questions", value=True)
    
    with col2:
        outline = st.checkbox("📋 Outline", value=True)
        terms = st.checkbox("📖 Key Terms", value=True)
        schedule = st.checkbox("📅 Study Schedule", value=False)
    
    if st.button("Generate Study Guide", type="primary", use_container_width=True):
        with st.spinner("Creating your personalized study guide..."):
            # Simulated generation
            st.success("✅ Study guide generated!")
            
            # Display results in tabs
            result_tabs = st.tabs(["Summary", "Flashcards", "Quiz", "Key Terms"])
            
            with result_tabs[0]:
                st.markdown("""
                ### Chapter Summary
                This chapter covers the fundamentals of machine learning...
                """)
            
            with result_tabs[1]:
                st.info("**Front:** What is supervised learning?")
                st.success("**Back:** A type of ML where the model learns from labeled training data")
            
            with result_tabs[2]:
                st.markdown("""
                **Q1:** Which of the following is NOT a type of machine learning?
                - A) Supervised Learning
                - B) Unsupervised Learning
                - C) Reinforcement Learning
                - D) Determined Learning ✓
                """)

# === MAIN DEMO SELECTOR ===
st.title("🧠 TuoKit Tool Interface Examples")

tool_choice = st.selectbox(
    "Select a tool to see its interface:",
    ["SQL Generator", "Error Decoder", "Study Guide Generator"]
)

if tool_choice == "SQL Generator":
    sql_generator_interface()
elif tool_choice == "Error Decoder":
    error_decoder_interface()
elif tool_choice == "Study Guide Generator":
    study_guide_interface()