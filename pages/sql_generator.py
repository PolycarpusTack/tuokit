import streamlit as st
import ollama
import sqlparse
import pandas as pd
import re
import json
from datetime import datetime
from utils import DatabaseManager

# Initialize TuoKit database
tuokit_db = DatabaseManager()

# Optional imports for database connectivity
try:
    from sqlalchemy import create_engine, inspect, text
    from sqlalchemy.exc import SQLAlchemyError
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False
    st.sidebar.warning("SQLAlchemy not installed. Live DB features disabled.")

try:
    import cx_Oracle
    ORACLE_AVAILABLE = True
except ImportError:
    ORACLE_AVAILABLE = False

# Database connection cache
if "db_connections" not in st.session_state:
    st.session_state.db_connections = {}

def get_db_engine(db_type, params):
    """Create SQLAlchemy engine for Oracle/PostgreSQL"""
    if not SQLALCHEMY_AVAILABLE:
        st.error("SQLAlchemy required for database connections. Install with: pip install sqlalchemy")
        return None
    
    cache_key = f"{db_type}_{params['host']}_{params['dbname']}"
    
    if cache_key not in st.session_state.db_connections:
        try:
            if db_type == "PostgreSQL":
                conn_str = f"postgresql+psycopg2://{params['user']}:{params['password']}@{params['host']}:{params['port']}/{params['dbname']}"
            else:  # Oracle
                if not ORACLE_AVAILABLE:
                    st.error("cx_Oracle required for Oracle connections. Install with: pip install cx_Oracle")
                    return None
                dsn = cx_Oracle.makedsn(params['host'], params['port'], service_name=params.get('service', 'ORCL'))
                conn_str = f"oracle+cx_oracle://{params['user']}:{params['password']}@{dsn}"
            
            engine = create_engine(conn_str, pool_pre_ping=True)
            # Test connection
            with engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            st.session_state.db_connections[cache_key] = engine
            return engine
        except Exception as e:
            st.error(f"Connection failed: {str(e)}")
            return None
    return st.session_state.db_connections[cache_key]

def get_schema_summary(engine):
    """Extract schema metadata from live database"""
    if not SQLALCHEMY_AVAILABLE:
        return {}
    
    try:
        inspector = inspect(engine)
        schema = {}
        tables = inspector.get_table_names()[:20]  # Limit to first 20 tables
        
        for table in tables:
            columns = inspector.get_columns(table)
            indexes = inspector.get_indexes(table)
            
            schema[table] = {
                "columns": [
                    {"name": col['name'], "type": str(col['type'])} 
                    for col in columns[:50]  # Limit columns
                ],
                "indexes": [idx['name'] for idx in indexes[:10]]  # Limit indexes
            }
        return schema
    except Exception as e:
        st.error(f"Schema fetch error: {str(e)}")
        return {}

def execute_safe(engine, sql, limit=50):
    """Execute query with safeguards and return preview"""
    if not SQLALCHEMY_AVAILABLE:
        return "Database connectivity not available"
    
    # Security check
    dangerous_keywords = ["DROP", "TRUNCATE", "DELETE", "ALTER", "GRANT", "REVOKE"]
    if any(keyword in sql.upper() for keyword in dangerous_keywords):
        return "Dangerous operation blocked for safety"
    
    try:
        with engine.connect() as conn:
            # Add limits for safety
            if "oracle" in str(engine.url).lower():
                modified_sql = f"SELECT * FROM ({sql.strip(';')}) WHERE ROWNUM <= {limit}"
            else:
                modified_sql = f"{sql.strip(';')} LIMIT {limit}"
                
            result = conn.execute(text(modified_sql))
            df = pd.DataFrame(result.fetchall(), columns=result.keys())
            return df
    except Exception as e:
        return f"Execution error: {str(e)}"

def generate_sql(query: str, db_type: str, schema_hint: str = "", advanced_options: dict = None) -> dict:
    """Generate SQL with dialect-specific best practices"""
    advanced_options = advanced_options or {}
    
    prompt = f"""
    Create a {db_type} SQL query for: "{query}"
    {f"Schema context: {schema_hint}" if schema_hint else ""}
    
    Requirements:
    1. Use {db_type} syntax and functions
    2. Include comprehensive comments
    3. Optimize for performance
    4. Add error handling where applicable
    """
    
    # Add advanced options to prompt
    if advanced_options.get('stored_procedure'):
        prompt += "\n5. Output as a complete stored procedure with exception handling"
    if advanced_options.get('security_hardened'):
        prompt += "\n6. Include parameter validation and SQL injection prevention"
    if advanced_options.get('explain_plan'):
        prompt += "\n7. Include EXPLAIN PLAN analysis"
    
    prompt += "\nOutput in markdown with:\n```sql\n-- Generated SQL\n```"
    
    try:
        response = ollama.generate(
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={
                "temperature": 0.2,
                "num_ctx": 4096
            }
        )
        
        # Extract SQL from response
        sql_match = re.search(r'```sql\n(.*?)\n```', response['response'], re.DOTALL)
        sql_code = sql_match.group(1) if sql_match else response['response']
        
        # Format SQL if sqlparse is available
        try:
            formatted_sql = sqlparse.format(sql_code, reindent=True, keyword_case='upper')
        except:
            formatted_sql = sql_code
        
        return {
            "raw_response": response['response'],
            "sql": formatted_sql,
            "error": False
        }
    except Exception as e:
        return {
            "raw_response": f"Error generating SQL: {str(e)}",
            "sql": "",
            "error": True
        }
def optimize_sql(sql: str, db_type: str, schema_info: dict = None) -> str:
    """Provide optimization recommendations"""
    schema_context = f"\nAvailable indexes: {json.dumps(schema_info, indent=2)}" if schema_info else ""
    
    prompt = f"""
    Analyze this {db_type} SQL for performance optimization:
    ```sql
    {sql}
    ```
    {schema_context}
    
    Provide:
    1. Specific index recommendations (CREATE INDEX statements)
    2. Query restructuring suggestions
    3. Partitioning strategies if applicable
    4. Statistics gathering commands
    5. Estimated performance improvement
    """
    try:
        response = ollama.generate(
            model="deepseek-coder:6.7b", 
            prompt=prompt,
            options={"temperature": 0.2}
        )
        return response['response']
    except Exception as e:
        return f"Error optimizing SQL: {str(e)}"

def explain_sql(sql: str, db_type: str) -> str:
    """Explain SQL behavior and purpose"""
    prompt = f"""
    Analyze this {db_type} SQL:
    1. Explain purpose in plain English
    2. Identify complexity level (simple/moderate/complex)
    3. Note key operations performed
    4. Highlight any potential issues
    
    SQL:
    ```sql
    {sql}
    ```
    """
    try:
        response = ollama.generate(model="deepseek-coder:6.7b", prompt=prompt)
        return response['response']
    except Exception as e:
        return f"Error analyzing SQL: {str(e)}"

def translate_sql(sql: str, source_db: str, target_db: str) -> str:
    """Convert SQL between dialects"""
    prompt = f"""
    Translate this {source_db} SQL to {target_db} dialect:
    
    ```sql
    {sql}
    ```    
    Handle these conversions:
    - Function mappings: NVL → COALESCE, ROWNUM → LIMIT, etc.
    - Date/time functions: TO_DATE → TO_TIMESTAMP, SYSDATE → CURRENT_TIMESTAMP
    - String operations: || → CONCAT or ||
    - Hierarchical queries: CONNECT BY → WITH RECURSIVE
    - Analytics: Oracle analytics → PostgreSQL window functions
    - Data types: NUMBER → NUMERIC, VARCHAR2 → VARCHAR
    
    Provide:
    1. The translated SQL
    2. Notes on any functionality without direct equivalents
    3. Warnings about semantic differences
    """
    try:
        response = ollama.generate(
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={"temperature": 0.2}
        )
        
        # Extract SQL from response
        sql_match = re.search(r'```sql\n(.*?)\n```', response['response'], re.DOTALL)
        if sql_match:
            return sql_match.group(1).strip()
        return response['response']
    except Exception as e:
        return f"Error translating SQL: {str(e)}"

def detect_vulnerabilities(sql: str) -> dict:
    """Security audit for SQL queries"""
    prompt = f"""
    Perform a comprehensive security audit on this SQL:    
    ```sql
    {sql}
    ```
    
    Check for:
    1. SQL injection vulnerabilities (dynamic SQL, string concatenation)
    2. Missing parameter validation
    3. Excessive privileges required
    4. Sensitive data exposure (PII, credentials)
    5. Performance issues that could lead to DoS
    6. Compliance violations (GDPR, PCI-DSS, HIPAA)
    
    Provide:
    - Risk level: LOW/MEDIUM/HIGH
    - Specific vulnerabilities with line numbers
    - Remediation code examples
    - Best practice recommendations
    """
    try:
        response = ollama.generate(
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={"temperature": 0.1}
        )
        
        # Determine risk level
        response_text = response['response']
        risk_level = "LOW"
        if "HIGH" in response_text.upper() or "CRITICAL" in response_text.upper():
            risk_level = "HIGH"
        elif "MEDIUM" in response_text.upper() or "MODERATE" in response_text.upper():
            risk_level = "MEDIUM"
            
        return {
            "risk_level": risk_level,
            "details": response_text
        }
    except Exception as e:
        return {
            "risk_level": "ERROR",
            "details": f"Error analyzing security: {str(e)}"
        }
def show():
    st.title("🛢️ Enterprise SQL Generator")
    st.caption("AI-powered SQL development with optional database connectivity")
    
    # Quick navigation
    col1, col2, col3 = st.columns([2, 1, 1])
    with col3:
        if st.button("🔍 SQL Optimizer →", use_container_width=True):
            st.switch_page("pages/sql_optimizer.py")
    
    # Initialize session state
    if "active_conn" not in st.session_state:
        st.session_state.active_conn = None
    if "generated_sql" not in st.session_state:
        st.session_state.generated_sql = ""
    if "last_query_id" not in st.session_state:
        st.session_state.last_query_id = None
    
    # Sidebar with examples
    with st.sidebar:
        st.subheader("📚 Example Queries")
        examples = {
            "Sales Analysis": "Show monthly sales totals for 2023 with running total and growth percentage",
            "Customer Segmentation": "Group customers by purchase frequency and total spend into tiers",
            "Inventory Report": "Find products with stock below reorder point including supplier info",
            "Employee Hierarchy": "Create org chart showing reporting structure with department rollups",
            "Time Series": "Daily active users with 7-day moving average and trend",
            "Data Quality": "Find duplicate records and data inconsistencies in customer table"
        }
        
        for name, query in examples.items():
            if st.button(name, key=f"ex_{name}", use_container_width=True):
                st.session_state.example_query = query
                st.rerun()
    
    # Optional database connection panel
    if SQLALCHEMY_AVAILABLE:
        with st.expander("🔌 Live Database Connection (Optional)", expanded=False):
            st.info("Connect to a database for schema-aware generation and query testing")
            
            db_type = st.radio("Database Type", ["PostgreSQL", "Oracle"], horizontal=True, key="conn_db_type")
            
            if db_type == "Oracle" and not ORACLE_AVAILABLE:
                st.warning("cx_Oracle not installed. Install with: pip install cx_Oracle")
            
            col1, col2 = st.columns(2)
            with col1:
                host = st.text_input("Host", "localhost")
                port = st.number_input("Port", value=5432 if db_type=="PostgreSQL" else 1521)
                dbname = st.text_input("Database Name", "")
            with col2:
                user = st.text_input("User", "")
                password = st.text_input("Password", type="password")
                if db_type == "Oracle":
                    service = st.text_input("Service Name", "ORCL")
                else:
                    service = ""
            
            col1, col2 = st.columns(2)
            with col1:
                if st.button("🔗 Connect", key="db_connect", type="primary"):
                    if not all([host, dbname, user, password]):
                        st.error("Please fill all connection fields")
                    else:
                        engine = get_db_engine(
                            db_type,
                            {
                                "host": host, "port": port, "dbname": dbname,
                                "user": user, "password": password, "service": service
                            }
                        )
                        if engine:
                            with st.spinner("Fetching schema..."):
                                schema = get_schema_summary(engine)
                            st.session_state.active_conn = {
                                "type": db_type,
                                "engine": engine,
                                "schema": schema,
                                "host": host,
                                "dbname": dbname
                            }
                            st.success(f"✅ Connected to {db_type} successfully!")
                            st.rerun()
            
            with col2:
                if st.session_state.active_conn and st.button("🔌 Disconnect", key="db_disconnect"):
                    st.session_state.active_conn = None
                    st.rerun()
    
    # Connection status
    if st.session_state.active_conn:
        conn_info = st.session_state.active_conn
        st.success(f"🔗 Connected to {conn_info['type']} @ {conn_info['host']}:{conn_info['dbname']} | Tables: {len(conn_info['schema'])}")
        
        with st.expander("📊 Database Schema", expanded=False):
            if conn_info['schema']:
                for table, info in list(conn_info['schema'].items())[:10]:
                    st.write(f"**{table}**")
                    cols = [f"{c['name']} ({c['type']})" for c in info['columns'][:5]]
                    st.write(f"Columns: {', '.join(cols)}...")
            else:
                st.write("No schema information available")    
    # Main tabs
    tab1, tab2, tab3, tab4 = st.tabs(["Generate", "Optimize", "Translate", "Security"])
    
    # TAB 1: SQL Generation
    with tab1:
        st.subheader("Natural Language to SQL")
        
        # Load example if selected
        default_query = st.session_state.get('example_query', '')
        
        query_desc = st.text_area(
            "Describe your query", 
            height=100,
            value=default_query,
            placeholder="Find top 5 customers by total orders in 2023 with their contact info"
        )
        
        # Clear example after use
        if 'example_query' in st.session_state:
            del st.session_state.example_query
        
        col1, col2 = st.columns([3, 1])
        with col1:
            # Database selection
            if st.session_state.active_conn:
                db_type = st.session_state.active_conn['type']
                st.info(f"Using connected {db_type} database")
            else:
                db_type = st.radio("Target Database", ["PostgreSQL", "Oracle"], horizontal=True)
            
            # Schema hint
            schema_hint = st.text_area(
                "Schema Hint (Optional)", 
                height=80,
                placeholder="customers(id, name, email)\norders(id, customer_id, amount, order_date)"
            )        
        with col2:
            st.subheader("Options")
            stored_proc = st.checkbox("Stored Procedure")
            security_hard = st.checkbox("Security Hardening")
            explain_plan = st.checkbox("Include EXPLAIN")
            use_live_schema = st.checkbox(
                "Use Live Schema", 
                value=True,
                disabled=not st.session_state.active_conn
            )
        
        if st.button("🚀 Generate SQL", type="primary", key="gen_sql"):
            if not query_desc.strip():
                st.warning("Please describe your query")
            else:
                # Prepare schema context
                schema_context = schema_hint
                if use_live_schema and st.session_state.active_conn:
                    schema_info = st.session_state.active_conn['schema']
                    # Convert schema to text format
                    schema_lines = []
                    for table, info in list(schema_info.items())[:10]:
                        cols = [f"{c['name']}" for c in info['columns'][:10]]
                        schema_lines.append(f"{table}({', '.join(cols)})")
                    schema_context = "\n".join(schema_lines)
                
                with st.spinner(f"Generating optimized {db_type} SQL..."):
                    result = generate_sql(
                        query_desc, 
                        db_type, 
                        schema_context,
                        {
                            'stored_procedure': stored_proc,
                            'security_hardened': security_hard,
                            'explain_plan': explain_plan
                        }
                    )                
                if result['error']:
                    st.error(result['raw_response'])
                else:
                    st.session_state.generated_sql = result['sql']
                    
                    st.subheader("Generated SQL")
                    st.code(result['sql'], language="sql")
                    
                    # Action buttons
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        if st.button("📋 Copy SQL", key="copy_sql"):
                            st.write("SQL copied! (Copy the code above)")
                    with col2:
                        if st.button("🔍 Optimize This Query", key="optimize_sql", type="secondary"):
                            st.session_state.query_to_optimize = result['sql']
                            st.switch_page("pages/sql_optimizer.py")
                    
                    with st.expander("📝 Full AI Response"):
                        st.markdown(result['raw_response'])
                    
                    # Log to database
                    query_id = tuokit_db.log_query(
                        tool="sql_generator",
                        model="deepseek-coder:6.7b",
                        prompt=f"{db_type} query: {query_desc}",
                        response=result['sql']
                    )
                    st.session_state.last_query_id = query_id
                    
                    # Quick analysis
                    with st.spinner("Analyzing query..."):
                        analysis = explain_sql(result['sql'], db_type)
                    
                    st.subheader("Query Analysis")
                    st.markdown(analysis)    
    # TAB 2: SQL Optimization
    with tab2:
        st.subheader("SQL Performance Optimizer")
        
        sql_to_optimize = st.text_area(
            "SQL to optimize", 
            height=200,
            value=st.session_state.generated_sql,
            placeholder="SELECT * FROM orders WHERE customer_id = 123..."
        )
        
        opt_db_type = st.radio(
            "Database Type", 
            ["PostgreSQL", "Oracle"], 
            horizontal=True, 
            key="opt_db_type",
            index=0 if not st.session_state.active_conn else (0 if st.session_state.active_conn['type'] == "PostgreSQL" else 1)
        )
        
        if st.button("🔍 Analyze & Optimize", type="primary", key="opt_sql"):
            if not sql_to_optimize.strip():
                st.warning("Please provide SQL to optimize")
            else:
                # Get schema info if connected
                schema_info = None
                if st.session_state.active_conn and st.session_state.active_conn['type'] == opt_db_type:
                    schema_info = st.session_state.active_conn['schema']
                
                with st.spinner("Analyzing query performance..."):
                    optimization = optimize_sql(sql_to_optimize, opt_db_type, schema_info)
                
                st.subheader("Optimization Recommendations")
                st.markdown(optimization)                
                # Execution preview if connected
                if st.session_state.active_conn and st.session_state.active_conn['type'] == opt_db_type:
                    if st.button("▶️ Test Execute (First 50 rows)", key="test_opt"):
                        with st.spinner("Executing query..."):
                            results = execute_safe(
                                st.session_state.active_conn['engine'],
                                sql_to_optimize
                            )
                        
                        st.subheader("Execution Results")
                        if isinstance(results, pd.DataFrame):
                            st.dataframe(results)
                            st.caption(f"Showing first {len(results)} rows")
                        else:
                            st.error(results)
    
    # TAB 3: SQL Translation
    with tab3:
        st.subheader("SQL Dialect Translator")
        st.caption("Convert queries between Oracle and PostgreSQL")
        
        col1, col2 = st.columns(2)
        with col1:
            source_db = st.selectbox("From", ["Oracle", "PostgreSQL"], key="source_db")
        with col2:
            target_db = st.selectbox("To", ["PostgreSQL", "Oracle"], key="target_db")
        
        sql_to_translate = st.text_area(
            "SQL to translate", 
            height=150,
            value=st.session_state.generated_sql,
            placeholder="Enter your SQL query here..."
        )        
        if st.button("🔄 Translate SQL", type="primary", key="trans_sql"):
            if not sql_to_translate.strip():
                st.warning("Please provide SQL to translate")
            elif source_db == target_db:
                st.warning("Source and target databases must be different")
            else:
                with st.spinner(f"Translating from {source_db} to {target_db}..."):
                    translation = translate_sql(sql_to_translate, source_db, target_db)
                
                st.subheader("Translated SQL")
                st.code(translation, language="sql")
                
                # Update generated SQL
                st.session_state.generated_sql = translation
                
                # Common conversion notes
                with st.expander("📚 Common Conversions"):
                    st.markdown("""
                    | Oracle | PostgreSQL |
                    |--------|------------|
                    | ROWNUM | LIMIT |
                    | NVL() | COALESCE() |
                    | TO_DATE() | TO_DATE() / ::date |
                    | SYSDATE | CURRENT_TIMESTAMP |
                    | CONNECT BY | WITH RECURSIVE |
                    """)
    
    # TAB 4: Security Audit
    with tab4:
        st.subheader("SQL Security Scanner")
        st.caption("Detect vulnerabilities and security risks in SQL queries")        
        sql_to_audit = st.text_area(
            "SQL to audit", 
            height=200,
            value=st.session_state.generated_sql,
            placeholder="Paste your SQL query here..."
        )
        
        if st.button("🔒 Run Security Scan", type="primary", key="sec_sql"):
            if not sql_to_audit.strip():
                st.warning("Please provide SQL to audit")
            else:
                with st.spinner("Performing security analysis..."):
                    audit_result = detect_vulnerabilities(sql_to_audit)
                
                # Display risk level with appropriate styling
                risk_level = audit_result['risk_level']
                if risk_level == "HIGH":
                    st.error(f"⚠️ Risk Level: {risk_level}")
                elif risk_level == "MEDIUM":
                    st.warning(f"⚠️ Risk Level: {risk_level}")
                else:
                    st.success(f"✅ Risk Level: {risk_level}")
                
                st.subheader("Security Analysis Report")
                st.markdown(audit_result['details'])
                
                # Best practices reminder
                with st.expander("🛡️ Security Best Practices"):
                    st.markdown("""
                    - Always use parameterized queries
                    - Validate all user inputs
                    - Use least privilege principle
                    - Avoid dynamic SQL construction
                    - Implement proper error handling
                    - Log all database access
                    - Encrypt sensitive data
                    """)    
    # Knowledge saving section (accessible from all tabs)
    if st.session_state.last_query_id and st.session_state.generated_sql:
        st.divider()
        with st.expander("💾 Save to Knowledge Base", expanded=False):
            st.subheader("Save SQL Pattern")
            
            title = st.text_input(
                "Title", 
                value=f"SQL Query: {query_desc[:50] if 'query_desc' in locals() else 'Generated SQL'}"
            )
            
            # Get existing categories
            categories = tuokit_db.get_knowledge_categories() or []
            default_categories = ["SQL Pattern", "Report Query", "Data Model", 
                               "Performance Tip", "Security Pattern", "Stored Procedure"]
            all_categories = sorted(list(set(categories + default_categories)))
            
            category = st.selectbox("Category", all_categories)
            
            notes = st.text_area(
                "Additional Notes (Optional)",
                placeholder="Any special considerations, use cases, or warnings..."
            )
            
            if st.button("💾 Save to Knowledge Base", type="primary"):
                # Combine SQL with notes if provided
                content = st.session_state.generated_sql
                if notes:
                    content = f"{content}\n\n-- Notes:\n-- {notes.replace(chr(10), chr(10) + '-- ')}"
                
                saved = tuokit_db.save_knowledge_unit(
                    query_id=st.session_state.last_query_id,
                    title=title,
                    content=content,
                    category=category
                )
                if saved:
                    st.success("✅ Saved to knowledge base!")
                    st.balloons()
                else:
                    st.error("Failed to save to knowledge base")

# For testing
if __name__ == "__main__":
    show()