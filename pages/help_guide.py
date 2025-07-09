import streamlit as st
from utils import DatabaseManager

# Page configuration
st.set_page_config(
    page_title="TuoKit - Help Guide",
    page_icon="❓",
    layout="wide"
)

# Initialize session state for database
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.session_state.db = None

st.title("❓ TuoKit Help Guide")
st.caption("Everything you need to master your AI development suite")

# Sidebar navigation
help_topic = st.sidebar.radio(
    "Help Topics",
    ["🚀 Getting Started", "💻 Code Tools", "📄 Document Tools",
     "🛢️ SQL Generator", "🔍 SQL Optimizer", "🔄 SQL Pipeline",
     "🔍 Regex Generator", "📚 Knowledge Library", 
     "⚙️ Database", "🔧 Troubleshooting", "❓ FAQ"]
)

st.sidebar.divider()
if st.sidebar.button("🧙‍♂️ Launch Interactive Tutorial", use_container_width=True):
    st.switch_page("pages/onboarding_wizard.py")

# --- Getting Started ---
if help_topic == "🚀 Getting Started":
    st.header("Launching TuoKit in 60 Seconds")
    
    with st.expander("📋 System Requirements", expanded=True):
        col1, col2 = st.columns(2)
        with col1:
            st.markdown("""
            **Minimum Hardware:**
            - CPU: 4-core modern processor
            - RAM: 8GB (16GB recommended)
            - Storage: 5GB free space
            """)
        with col2:
            st.markdown("""
            **Supported OS:**
            - Windows 10/11
            - macOS 10.15+
            - Linux Ubuntu 20.04+
            """)
    
    with st.expander("⚡ Quick Start Guide"):
        st.markdown("""
        1. **Install requirements**:
           ```bash
           pip install -r requirements.txt
           ```
        2. **Install Ollama** (if not already installed):
           - Visit https://ollama.ai
           - Download for your OS
           - Run installer
           
        3. **Pull AI models**:
           ```bash
           ollama pull deepseek-coder:6.7b
           ollama pull deepseek-r1:6.7b
           ```
        4. **Set up database** (optional):
           ```bash
           psql -U postgres -f database_setup.sql
           ```
        5. **Configure environment**:
           ```bash
           copy .env.example .env  # Windows
           cp .env.example .env    # Linux/Mac
           # Edit .env with your database credentials
           ```
        6. **Run TuoKit**:
           ```bash
           streamlit run app.py
           # Or use start_tuokit.bat (Windows)
           ```
        """)
    
    with st.expander("🎯 First Time Walkthrough"):
        st.markdown("""
        **Recommended first steps:**
        1. **Dashboard** → Check Ollama status (should show ✅ Running)
        2. **Code Tools** → Try explaining a simple function
        3. **Document Tools** → Upload test_document.txt
        4. **Knowledge Library** → Browse your saved insights
        
        **Sample workflow:**
        ```python
        # 1. Go to Code Tools
        # 2. Paste this code:
        def fibonacci(n):
            if n <= 1:
                return n
            return fibonacci(n-1) + fibonacci(n-2)
        
        # 3. Click "Analyze Code"
        # 4. Save to Knowledge Base
        ```
        """)

# --- Code Tools Help ---
elif help_topic == "💻 Code Tools":
    st.header("Mastering Code Tools")
    
    with st.expander("🔍 Code Explanation", expanded=True):
        st.markdown("""
        **How to use:**
        1. Paste any code snippet
        2. Select AI model (deepseek-coder recommended)
        3. Click "Analyze Code"
        
        **Best for:**
        - Understanding legacy code
        - Learning new libraries
        - Documenting complex logic
        - Code review preparation
        
        **Pro Tips:** 
        - Add comments like `# Security focus` for targeted analysis
        - Include imports for better context
        - Works with Python, JavaScript, SQL, and more
        
        **Example prompts hidden in the code:**
        ```python
        # COMPLEXITY: Analyze time and space complexity
        # SECURITY: Check for SQL injection
        # PERFORMANCE: Suggest optimizations
        ```
        """)
    
    with st.expander("🐞 Code Debugging"):
        st.markdown("""
        **How to use:**
        1. Enter problematic code
        2. Provide exact error message
        3. Click "Diagnose Issue"
        
        **Handles:**
        - Python exceptions (TypeError, ValueError, etc.)
        - Logical errors
        - Performance bottlenecks
        - Syntax issues
        
        **Best practices:**
        - Include full traceback
        - Mention Python version if relevant
        - Describe expected vs actual behavior
        
        **Example:**
        ```
        Code: result = 10 / user_input
        Error: ZeroDivisionError: division by zero
        ```
        """)
    
    with st.expander("✨ Code Generation"):
        st.markdown("""
        **How to use:**
        1. Describe what you need in natural language
        2. Select target language
        3. Click "Generate Code"
        
        **Effective descriptions:**
        - ✅ "Create a REST API client with retry logic and rate limiting"
        - ✅ "Generate a PyTorch CNN for MNIST with 99% accuracy"
        - ❌ "Make a function" (too vague)
        
        **Supported languages:**
        - Python (with type hints)
        - JavaScript (ES6+)
        - SQL (PostgreSQL dialect)
        - Bash (Linux/Mac compatible)
        """)

# --- Document Tools Help ---
elif help_topic == "📄 Document Tools":
    st.header("Document Processing Guide")
    
    with st.expander("📝 Supported Formats", expanded=True):
        st.markdown("""
        | Format | Max Size | Features | Limitations |
        |--------|----------|----------|-------------|
        | **PDF** | 10MB | Text extraction, Multi-page | Scanned PDFs need OCR |
        | **TXT** | 5MB | Full support, Fast | No formatting |
        | **Coming** | - | DOCX, HTML, Markdown | In development |
        """)
    
    with st.expander("❓ Document Q&A"):
        st.markdown("""
        **How to use:**
        1. Upload document (PDF or TXT)
        2. Wait for text extraction
        3. Ask specific questions
        4. Get context-aware answers
        
        **Effective questions:**
        - "What are the main findings?"
        - "List all action items with deadlines"
        - "What decisions were made in the meeting?"
        - "Summarize the methodology section"
        
        **Tips:**
        - Be specific rather than broad
        - Reference sections if known
        - Ask follow-up questions
        """)
    
    with st.expander("📊 Knowledge Extraction"):
        st.markdown("""
        **Extracts structured data:**
        - Key topics and themes
        - Important dates
        - Decisions made
        - Action items with owners
        
        **Output format:** JSON
        ```json
        {
            "key_topics": ["AI", "Development"],
            "important_dates": ["2025-01-15"],
            "decisions_made": ["Adopt Streamlit"],
            "action_items": [
                {"task": "Deploy", "owner": "DevOps", "due": "2025-02-01"}
            ]
        }
        ```
        """)

# --- SQL Generator Help ---
elif help_topic == "🛢️ SQL Generator":
    st.header("SQL Generator Guide")
    
    with st.expander("🎯 Natural Language to SQL", expanded=True):
        st.markdown("""
        **How to use:**
        1. Select database type (PostgreSQL or Oracle)
        2. Describe query in plain English
        3. Add schema hints for accuracy (optional)
        4. Click "Generate SQL"
        
        **Example descriptions:**
        - "Find top 5 customers by total sales in 2023"
        - "Show monthly revenue with year-over-year growth"
        - "List employees hired in last 90 days with departments"
        
        **Schema hints format:**
        ```
        customers(id, name, created_date)
        orders(id, customer_id, amount, order_date)
        order_items(order_id, product_id, quantity, price)
        ```
        
        **Advanced options:**
        - ✅ **Stored Procedure**: Generates complete procedure with error handling
        - ✅ **Security Hardening**: Adds parameter validation and injection prevention
        """)
    
    with st.expander("🚀 SQL Optimization"):
        st.markdown("""
        **Performance analysis includes:**
        - Query execution plan insights
        - Missing index recommendations
        - Query rewrite suggestions
        - Partitioning strategies
        
        **Common optimizations:**
        - Replace `SELECT *` with specific columns
        - Use proper JOINs instead of subqueries
        - Add appropriate indexes
        - Consider materialized views for complex aggregations
        """)
    
    with st.expander("🔄 SQL Translation"):
        st.markdown("""
        **Supported conversions:**
        - Oracle → PostgreSQL
        - PostgreSQL → Oracle
        
        **Handles these differences:**
        | Oracle | PostgreSQL |
        |--------|------------|
        | ROWNUM | LIMIT |
        | NVL() | COALESCE() |
        | TO_DATE() | TO_DATE() / ::date |
        | CONNECT BY | WITH RECURSIVE |
        | MERGE | INSERT ON CONFLICT |
        
        **Translation tips:**
        - Review function mappings
        - Check date format strings
        - Verify sequence syntax
        - Test hierarchical queries carefully
        """)
    
    with st.expander("🔒 Security Scanner"):
        st.markdown("""
        **Security checks performed:**
        1. **SQL Injection vulnerabilities**
           - Unescaped user inputs
           - Dynamic SQL construction
           - Missing parameterization
        
        2. **Access control issues**
           - Excessive privileges required
           - Missing row-level security
           - Direct table access patterns
        
        3. **Data exposure risks**
           - Sensitive column exposure
           - Missing data masking
           - Audit trail gaps
        
        **Risk levels:**
        - 🟢 **Low**: Best practices followed
        - 🟡 **Medium**: Some improvements recommended
        - 🔴 **High**: Critical issues requiring immediate attention
        """)
    
    with st.expander("💡 Best Practices"):
        st.markdown("""
        **Query design:**
        - Use CTEs for readability
        - Prefer set-based operations
        - Avoid cursors when possible
        - Include helpful comments
        
        **Performance:**
        - Index foreign keys
        - Use appropriate data types
        - Partition large tables
        - Update statistics regularly
        
        **Security:**
        - Always use parameterized queries
        - Implement least privilege access
        - Audit sensitive operations
        - Encrypt data at rest and in transit
        """)

# --- SQL Optimizer Help ---
elif help_topic == "🔍 SQL Optimizer":
    st.header("SQL Query Optimizer Guide")
    
    with st.expander("🎯 Overview", expanded=True):
        st.markdown("""
        **Purpose:**
        The SQL Optimizer analyzes slow queries and provides:
        - Execution plan analysis
        - Index recommendations
        - Optimized query alternatives
        - Validation and safety checks
        
        **Key Features:**
        - Professional validation framework
        - Anti-pattern detection
        - Functional equivalence checking
        - Safety guardrails
        
        **Best For:**
        - Improving slow query performance
        - Finding missing indexes
        - Learning optimization techniques
        - Database performance tuning
        """)
    
    with st.expander("📊 Understanding Results"):
        st.markdown("""
        **Confidence Levels:**
        - 🟢 **High (>80%)**: Recommendations are reliable
        - 🟡 **Moderate (60-80%)**: Review carefully
        - 🔴 **Low (<60%)**: Manual validation required
        
        **Validation Warnings:**
        - `too_many_columns`: Index has >3 columns
        - `low_selectivity`: Boolean/flag columns
        - `functional_dependency`: Multi-column order matters
        
        **Risk Indicators:**
        - Performance risks in execution plan
        - Index maintenance overhead
        - Query complexity issues
        """)
    
    with st.expander("🔧 Using Recommendations"):
        st.markdown("""
        **Index Recommendations:**
        1. Review the CREATE INDEX statement
        2. Check for existing similar indexes
        3. Test in staging environment first
        4. Monitor write performance impact
        
        **Query Alternatives:**
        1. Verify functional equivalence
        2. Run EXPLAIN ANALYZE on both
        3. Test with production data volume
        4. Check edge cases (NULLs, empty results)
        
        **Validation Steps:**
        - Always test in non-production first
        - Compare actual execution times
        - Verify result sets match
        - Monitor after deployment
        """)
    
    with st.expander("⚠️ Important Limitations"):
        st.markdown("""
        **AI Limitations:**
        - Suggestions based on patterns, not live data
        - Cannot see actual table statistics
        - May not consider all edge cases
        - Requires human validation
        
        **Safety Features:**
        - Blocks DROP/TRUNCATE operations
        - Warns about DELETE/UPDATE without WHERE
        - Validates SQL syntax before analysis
        - Provides confidence scores
        
        **Best Practices:**
        1. Start with simple queries
        2. Validate all suggestions
        3. Test thoroughly
        4. Use feedback to improve
        """)
    
    with st.expander("💡 Pro Tips"):
        st.markdown("""
        **Getting Better Results:**
        - Provide complete queries (with JOINs)
        - Include WHERE conditions
        - Specify the slow parts if known
        - Use dialect-specific features
        
        **Common Optimizations:**
        - Convert IN to EXISTS
        - Replace subqueries with JOINs
        - Add covering indexes
        - Partition large tables
        
        **Integration Workflow:**
        1. Generate query in SQL Generator
        2. If slow, optimize with SQL Optimizer
        3. Save both versions to Knowledge Base
        4. Document performance gains
        """)

# --- SQL Pipeline Help ---
elif help_topic == "🔄 SQL Pipeline":
    st.header("SQL Pipeline Guide")
    
    with st.expander("🎯 Overview", expanded=True):
        st.markdown("""
        **What is the SQL Pipeline?**
        The SQL Pipeline is a guided workflow that takes you from a natural language description to an optimized, tested SQL query in 4 simple steps.
        
        **Perfect for:**
        - Beginners learning SQL
        - Quick prototyping of queries
        - Understanding query optimization
        - Testing queries before production
        
        **The 4-Step Process:**
        1. **Describe** - Tell us what data you need in plain English
        2. **Generate** - Get an AI-generated SQL query
        3. **Optimize** - Improve performance automatically
        4. **Understand** - Learn what the query does and test it
        """)
    
    with st.expander("🚀 Step-by-Step Guide"):
        st.markdown("""
        ### Step 1: Describe Your Need
        - Write in plain English what data you want
        - Be specific about filters, sorting, and grouping
        - Example: "Show top 5 customers by total spending last month"
        
        ### Step 2: Review Generated SQL
        - Check the generated query
        - Edit if needed (the SQL is editable!)
        - See which SQL concepts are used
        - Regenerate for alternatives
        
        ### Step 3: Automatic Optimization
        - Query is analyzed for performance
        - Optimizations are applied automatically
        - See before/after comparison
        - Understand what improvements were made
        
        ### Step 4: Learn and Test
        - **Explanation Tab**: Plain English breakdown
        - **Test Tab**: Run with sample data
        - **Save Tab**: Store in knowledge base
        """)
    
    with st.expander("💡 Pro Tips"):
        st.markdown("""
        **Writing Better Descriptions:**
        - Include time ranges: "in the last 30 days"
        - Specify sorting: "ordered by amount descending"
        - Mention grouping: "grouped by category"
        - Add limits: "top 10" or "first 5"
        
        **Example Descriptions:**
        - ✅ "Find customers who spent over $1000 in Q4 2023, sorted by total spending"
        - ✅ "Monthly sales totals for each product category with year-over-year comparison"
        - ❌ "Show data" (too vague)
        - ❌ "Customer info" (not specific enough)
        
        **Testing Your Queries:**
        - Use provided sample data templates
        - Modify data to test edge cases
        - Check results match expectations
        - Verify NULL handling
        """)
    
    with st.expander("🧪 Sample Data Templates"):
        st.markdown("""
        The Pipeline includes ready-to-use data templates:
        
        **Customers & Orders:**
        - Customer records with emails and join dates
        - Order history with amounts and statuses
        - Perfect for sales analysis queries
        
        **Employees & Departments:**
        - Employee records with salaries
        - Department information with budgets
        - Great for HR analytics
        
        **Products & Categories:**
        - Product inventory with prices
        - Category information with discounts
        - Ideal for inventory queries
        
        You can also paste your own JSON data for testing!
        """)
    
    with st.expander("🔄 Integration with Other Tools"):
        st.markdown("""
        **From SQL Pipeline to:**
        - **SQL Generator**: For more complex queries
        - **SQL Optimizer**: For deeper performance analysis
        - **Knowledge Library**: All saves automatically integrate
        
        **Workflow Example:**
        1. Start with Pipeline for quick query
        2. If needs complexity → SQL Generator
        3. If needs deep optimization → SQL Optimizer
        4. Save all versions to Knowledge Library
        """)

# --- Knowledge Library Help ---
elif help_topic == "📚 Knowledge Library":
    st.header("Knowledge Management System")
    
    with st.expander("🔍 Search Techniques", expanded=True):
        st.markdown("""
        **Basic search:**
        - Type keywords to search titles and content
        - Use category filter for focused results
        - Sort by date or title
        
        **Advanced tips:**
        - Search for error messages
        - Use technical terms for precision
        - Try partial words for broader results
        
        **Categories explained:**
        - **Code Snippet**: Reusable code fragments
        - **Algorithm**: Complete implementations
        - **Error Solution**: Debugging fixes
        - **Document Summary**: Key document insights
        - **Research Findings**: Important discoveries
        """)
    
    with st.expander("✏️ Editing Knowledge"):
        st.markdown("""
        **How to edit:**
        1. Find knowledge unit
        2. Click "Edit" button
        3. Modify title or content
        4. Save changes
        
        **When to edit:**
        - Update outdated information
        - Add new insights
        - Correct errors
        - Improve clarity
        """)
    
    with st.expander("📤 Export Options"):
        st.markdown("""
        **Individual export:**
        - Click "Export" on any knowledge unit
        - Downloads as .txt file
        - Preserves formatting
        
        **Bulk export:**
        - Click "Export All Knowledge"
        - Creates comprehensive Markdown file
        - Includes all metadata
        - Perfect for backups or sharing
        """)

# --- Database Help ---
elif help_topic == "⚙️ Database":
    st.header("Database Management")
    
    with st.expander("🗄️ Setup Instructions", expanded=True):
        st.markdown("""
        **PostgreSQL Installation:**
        ```bash
        # Windows (using installer from postgresql.org)
        # Linux
        sudo apt install postgresql
        # macOS
        brew install postgresql
        ```
        
        **Create database:**
        ```bash
        psql -U postgres -f database_setup.sql
        ```
        
        **Configure .env file:**
        ```
        DB_NAME=ollama_knowledge
        DB_USER=ollama_user
        DB_PASSWORD=your_secure_password
        DB_HOST=localhost
        ```
        """)
    
    with st.expander("💾 Backup & Restore"):
        st.markdown("""
        **Backup database:**
        ```bash
        pg_dump -U ollama_user -d ollama_knowledge > backup_$(date +%Y%m%d).sql
        ```
        
        **Restore database:**
        ```bash
        psql -U ollama_user -d ollama_knowledge < backup_20250107.sql
        ```
        
        **Automatic backups (cron):**
        ```bash
        0 2 * * * pg_dump -U ollama_user -d ollama_knowledge > /backups/tuo_$(date +\\%Y\\%m\\%d).sql
        ```
        """)
    
    with st.expander("📊 Database Schema"):
        st.code("""
        -- Queries table
        CREATE TABLE queries (
            id SERIAL PRIMARY KEY,
            tool VARCHAR(100) NOT NULL,
            model VARCHAR(100) NOT NULL,
            user_prompt TEXT NOT NULL,
            ai_response TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        
        -- Knowledge units table
        CREATE TABLE knowledge_units (
            id SERIAL PRIMARY KEY,
            query_id INTEGER REFERENCES queries(id),
            title VARCHAR(255) NOT NULL,
            content TEXT NOT NULL,
            category VARCHAR(100) NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        );
        """, language="sql")

# --- Troubleshooting ---
elif help_topic == "🔧 Troubleshooting":
    st.header("Common Issues and Solutions")
    
    issues = [
        {
            "problem": "Ollama not responding",
            "symptoms": ["Dashboard shows ❌ Stopped", "Tools timeout", "No model list"],
            "solutions": [
                "Check if Ollama is running: `ollama list`",
                "Start Ollama service: `ollama serve`",
                "Verify models installed: `ollama pull deepseek-coder:6.7b`",
                "Check firewall settings for port 11434"
            ]
        },
        {
            "problem": "Database connection failed",
            "symptoms": ["Knowledge Library empty", "Can't save insights", "Connection errors"],
            "solutions": [
                "Verify PostgreSQL is running: `pg_isready`",
                "Check credentials in .env file",
                "Test connection: `psql -U ollama_user -d ollama_knowledge`",
                "Create database if missing: `psql -U postgres -f database_setup.sql`"
            ]
        },
        {
            "problem": "PDF processing errors",
            "symptoms": ["Can't extract text", "Upload fails", "Empty content"],
            "solutions": [
                "Check PDF size (max 10MB recommended)",
                "Verify PDF libraries: `python test_pdf.py`",
                "Try saving PDF as text first",
                "Update libraries: `pip install --upgrade pypdf2 pymupdf`"
            ]
        },
        {
            "problem": "Slow performance",
            "symptoms": ["Long processing times", "UI freezes", "Timeouts"],
            "solutions": [
                "Use smaller AI models (deepseek-r1:1.5b)",
                "Process smaller documents",
                "Check system resources (RAM/CPU)",
                "Restart Streamlit server"
            ]
        }
    ]
    
    for issue in issues:
        with st.expander(f"🔧 {issue['problem']}"):
            st.markdown("**Symptoms:**")
            for symptom in issue['symptoms']:
                st.markdown(f"- {symptom}")
            
            st.markdown("\n**Solutions:**")
            for i, solution in enumerate(issue['solutions'], 1):
                st.markdown(f"{i}. {solution}")

# --- FAQ ---
elif help_topic == "❓ FAQ":
    st.header("Frequently Asked Questions")
    
    faqs = [
        ("Can I use TuoKit offline?", 
         "Yes! TuoKit runs completely offline. You need internet only for initial setup and downloading AI models."),
        
        ("How do I add custom AI models?", 
         "Pull any Ollama-compatible model: `ollama pull modelname`. Then select it in the model dropdown."),
        
        ("Where is my data stored?", 
         "All data is stored locally in PostgreSQL at `localhost:5432/ollama_knowledge`. Nothing leaves your machine."),
        
        ("Can multiple users access TuoKit?", 
         "Currently single-user only. For team use, each member should run their own instance."),
        
        ("How much disk space do I need?", 
         "Base install: ~500MB. Each AI model: 3-30GB. Database grows ~1MB per 100 knowledge units."),
        
        ("Is my code/data secure?", 
         "Yes! Everything runs locally. No external API calls. Your code and documents never leave your machine."),
        
        ("Can I customize the categories?", 
         "Yes, edit the category lists in each tool's code. Database accepts any category string."),
        
        ("How do I update TuoKit?", 
         "Pull latest code, run `pip install -r requirements.txt`, and restart. Your data is preserved."),
    ]
    
    for question, answer in faqs:
        with st.expander(f"❔ {question}"):
            st.markdown(answer)
    
    st.divider()
    st.subheader("📬 Ask a Question")
    user_question = st.text_area("What would you like to know?")
    if st.button("Submit Question") and user_question:
        st.success("Question received! Check back for updates to the FAQ.")
        # In production, this would save to database

# Footer with quick links
st.divider()
col1, col2, col3 = st.columns(3)
with col1:
    if st.button("📚 View Docs", use_container_width=True):
        st.info("Check the /docs folder for detailed guides")
with col2:
    if st.button("🐛 Report Issue", use_container_width=True):
        st.info("Create an issue on GitHub (coming soon)")
with col3:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")