"""
🛢️ TuoKit SQL Toolkit - Complete Consolidated Version
Merges ALL features from sql_generator, sql_optimizer, sql_pipeline, sql_suite, and sql_toolkit
"""

import streamlit as st
import streamlit.components.v1 as components
import re
import json
import time
import pandas as pd
import sqlparse
import ollama
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any

# Import all utilities
from utils import (
    DatabaseManager, 
    safe_ollama_generate,
    capture_knowledge,
    validate_file_size,
    get_available_models,
    knowledge_graph as kg
)
from utils.sql_concepts import (
    SQL_CONCEPTS, get_concepts_in_query, get_concept_info,
    get_quiz_for_concept, get_difficulty_color, get_learning_path
)

# Database connectivity imports (from sql_generator)
try:
    from sqlalchemy import create_engine, inspect, text
    from sqlalchemy.exc import SQLAlchemyError
    SQLALCHEMY_AVAILABLE = True
except ImportError:
    SQLALCHEMY_AVAILABLE = False

try:
    import cx_Oracle
    ORACLE_AVAILABLE = True
except ImportError:
    ORACLE_AVAILABLE = False

class UnifiedSQLToolkit:
    """Complete SQL toolkit with all features from all SQL tools"""
    
    def __init__(self):
        self.db = DatabaseManager()
        self.db_connections = {}  # Cache for live connections
        
    # ========== CORE SQL OPERATIONS ==========
    
    def generate_sql(self, query: str, db_type: str, schema_hint: str = "", 
                    advanced_options: dict = None) -> dict:
        """Generate SQL with dialect-specific best practices (from sql_generator)"""
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
                model=st.session_state.get('sql_model', 'deepseek-coder:6.7b'),
                prompt=prompt,
                options={"temperature": 0.2, "num_ctx": 4096}
            )
            
            # Extract SQL from response
            sql_match = re.search(r'```sql\n(.*?)\n```', response['response'], re.DOTALL)
            sql_code = sql_match.group(1) if sql_match else response['response']
            
            # Format SQL
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
    
    def optimize_sql(self, sql: str, db_type: str, schema_info: dict = None,
                    explain_plan: str = None, target_improvement: str = "performance") -> dict:
        """Comprehensive SQL optimization with validation (merged from sql_optimizer)"""
        schema_context = f"\nAvailable indexes: {json.dumps(schema_info, indent=2)}" if schema_info else ""
        
        prompt = f"""
        Optimize this {db_type} SQL for {target_improvement}:
        
        ```sql
        {sql}
        ```
        {schema_context}
        {f'Current EXPLAIN ANALYZE output:\n{explain_plan}' if explain_plan else ''}
        
        Focus on:
        - {'Query execution speed' if target_improvement == 'performance' else ''}
        - {'Memory usage and temp file reduction' if target_improvement == 'memory' else ''}  
        - {'Code clarity and maintainability' if target_improvement == 'readability' else ''}
        - Index recommendations
        - JOIN optimization
        - Subquery elimination where beneficial
        
        Provide:
        1. Optimized SQL
        2. Specific index recommendations (CREATE INDEX statements)
        3. Query restructuring suggestions
        4. Estimated performance improvement
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get('sql_model', 'deepseek-coder:6.7b'), 
                prompt=prompt,
                options={"temperature": 0.2}
            )
            
            # Extract components
            optimized_sql = self._extract_sql(response['response'])
            indexes = self._extract_indexes(response['response'])
            
            return {
                "raw_response": response['response'],
                "optimized_sql": optimized_sql,
                "indexes": indexes,
                "target": target_improvement,
                "error": False
            }
        except Exception as e:
            return {
                "raw_response": f"Error optimizing SQL: {str(e)}",
                "optimized_sql": sql,
                "indexes": [],
                "error": True
            }
    
    def explain_sql(self, sql: str, db_type: str = "PostgreSQL", 
                   technical_level: str = "beginner") -> str:
        """Explain SQL behavior and purpose (enhanced from multiple sources)"""
        level_prompts = {
            "beginner": "Explain like I'm new to databases",
            "intermediate": "Explain with some technical details", 
            "expert": "Provide deep technical analysis"
        }
        
        prompt = f"""
        Analyze this {db_type} SQL:
        
        ```sql
        {sql}
        ```
        
        {level_prompts.get(technical_level, level_prompts['beginner'])}:
        1. Purpose in plain English
        2. Step-by-step breakdown
        3. Expected results
        4. Key SQL concepts used
        5. Complexity level (simple/moderate/complex)
        6. Potential issues or improvements
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"), 
                prompt=prompt,
                options={"temperature": 0.3}
            )
            return response['response']
        except Exception as e:
            return f"Error analyzing SQL: {str(e)}"
    
    def translate_sql(self, sql: str, source_db: str, target_db: str) -> str:
        """Convert SQL between dialects (from sql_generator)"""
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
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
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
    
    def detect_vulnerabilities(self, sql: str) -> dict:
        """Security audit for SQL queries (from sql_generator)"""
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
                model=st.session_state.get('sql_model', 'deepseek-coder:6.7b'),
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
    
    # ========== DATABASE CONNECTIVITY (from sql_generator) ==========
    
    def get_db_engine(self, db_type, params):
        """Create SQLAlchemy engine for Oracle/PostgreSQL"""
        if not SQLALCHEMY_AVAILABLE:
            st.error("SQLAlchemy required for database connections. Install with: pip install sqlalchemy")
            return None
        
        cache_key = f"{db_type}_{params['host']}_{params['dbname']}"
        
        if cache_key not in self.db_connections:
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
                self.db_connections[cache_key] = engine
                return engine
            except Exception as e:
                st.error(f"Connection failed: {str(e)}")
                return None
        return self.db_connections[cache_key]
    
    def get_schema_summary(self, engine):
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
    
    def execute_safe(self, engine, sql, limit=50):
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
    
    # ========== VALIDATION FUNCTIONS (from sql_optimizer) ==========
    
    def validate_explain_plan(self, plan):
        """Add validation metrics to EXPLAIN plan"""
        if not isinstance(plan, dict):
            return {"error": "Invalid plan format"}
        
        # Calculate confidence score
        confidence = 0.7  # Base confidence
        if "steps" in plan and len(plan["steps"]) > 0:
            confidence += 0.1
        if "cost_estimate" in plan:
            confidence += 0.1
        if "performance_risks" in plan and plan["performance_risks"]:
            confidence += 0.1
            
        # Add validation metadata
        plan["validation"] = {
            "confidence": min(0.95, confidence),
            "last_validated": datetime.now().strftime("%Y-%m-%d"),
            "requires_db_validation": True
        }
        return plan
    
    def validate_index_recommendation(self, index, query, dialect):
        """Add validation metadata to index recommendations"""
        # Check for common anti-patterns
        anti_patterns = []
        
        if "columns" in index:
            if len(index["columns"]) > 3:
                anti_patterns.append("too_many_columns")
            if any(col.lower().endswith(('_flag', '_bool', '_bit')) for col in index["columns"]):
                anti_patterns.append("low_selectivity")
            if len(index["columns"]) > 1 and not index.get("composite_justification"):
                anti_patterns.append("functional_dependency")
        
        # Add validation metadata
        index["validation"] = {
            "confidence": 0.8 - (0.1 * len(anti_patterns)),
            "warnings": anti_patterns,
            "requires_explain_validation": True,
            "test_command": f"EXPLAIN (ANALYZE, BUFFERS) {query}" if dialect == "PostgreSQL" else f"EXPLAIN {query}"
        }
        return index
    
    def validate_alternative_query(self, alt_query, original, dialect):
        """Validate functional equivalence of alternative queries"""
        prompt = f"""
        Verify functional equivalence between these {dialect} queries:
        
        Original:
        {original}
        
        Alternative:
        {alt_query['sql']}
        
        Consider:
        - NULL handling
        - Duplicate rows
        - Sorting order
        - Aggregation behavior
        - Edge cases
        
        Respond ONLY with JSON: {{"equivalent": true/false, "differences": ["list of differences"]}}
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.1}
            )
            
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response['response'], re.DOTALL)
            if json_match:
                equivalence = json.loads(json_match.group())
            else:
                equivalence = {"equivalent": "unknown", "differences": ["Could not parse validation"]}
                
            alt_query["validation"] = equivalence
        except Exception as e:
            alt_query["validation"] = {
                "equivalent": "unknown",
                "differences": [f"Validation failed: {str(e)}"]
            }
        
        return alt_query
    
    def validate_sql(self, query, dialect):
        """Basic SQL syntax validation"""
        if not query or not query.strip():
            return False, "Query is empty"
        
        # Check for basic SQL structure
        query_upper = query.upper().strip()
        valid_starts = ['SELECT', 'WITH', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'ALTER', 'DROP']
        
        if not any(query_upper.startswith(start) for start in valid_starts):
            return False, "Query must start with a valid SQL keyword"
        
        # Check for balanced parentheses
        if query.count('(') != query.count(')'):
            return False, "Unbalanced parentheses"
        
        # Check for basic syntax issues
        if ';;' in query:
            return False, "Double semicolon detected"
        
        return True, "Syntax appears valid"
    
    def check_query_safety(self, query):
        """Check for potentially dangerous operations"""
        dangerous_patterns = [
            (r'\bDROP\s+TABLE\b', "DROP TABLE operations blocked"),
            (r'\bDROP\s+DATABASE\b', "DROP DATABASE operations blocked"),
            (r'\bTRUNCATE\b', "TRUNCATE operations blocked"),
            (r'\bDELETE\s+FROM\s+\w+\s*;', "DELETE without WHERE clause detected"),
            (r'\bUPDATE\s+\w+\s+SET\s+.*\s*;', "UPDATE without WHERE clause detected")
        ]
        
        for pattern, message in dangerous_patterns:
            if re.search(pattern, query, re.IGNORECASE | re.MULTILINE):
                return False, message
        
        return True, "Query passed safety checks"
    
    # ========== NATURAL LANGUAGE PIPELINE (from sql_pipeline) ==========
    
    def generate_sql_from_nl(self, description, dialect):
        """Convert natural language to SQL"""
        prompt = f"""
        Convert this request to {dialect} SQL:
        "{description}"
        
        Requirements:
        1. Use proper {dialect} syntax
        2. Include helpful comments
        3. Optimize for readability
        4. Handle edge cases appropriately
        
        Output ONLY the SQL query, no explanations.
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.2}
            )
            
            # Clean response
            sql = response['response'].strip()
            # Remove markdown code blocks if present
            if sql.startswith("```sql"):
                sql = sql[6:]
            if sql.endswith("```"):
                sql = sql[:-3]
            sql = sql.strip()
            
            return sql
        except Exception as e:
            return f"-- Error generating SQL: {str(e)}"
    
    def test_query_with_sample_data(self, query, dialect, sample_data):
        """Simulate query execution on sample data"""
        prompt = f"""
        Execute this {dialect} SQL query on the provided JSON data and show results:
        
        Query:
        ```sql
        {query}
        ```
        
        Data:
        ```json
        {sample_data}
        ```
        
        Output the results as a markdown table. If the query would fail, explain why.
        Include column headers and all matching rows.
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.1}
            )
            return response['response']
        except Exception as e:
            return f"Error testing query: {str(e)}"
    
    # ========== ADVANCED ANALYSIS (merged features) ==========
    
    def explain_query_plan(self, query, dialect):
        """Generate EXPLAIN plan with validation (from sql_optimizer)"""
        prompt = f"""
        Analyze this {dialect} query and provide an execution plan analysis:
        
        ```sql
        {query}
        ```
        
        Provide output as JSON with these exact fields:
        {{
            "summary": "1-sentence overview of query execution",
            "steps": ["list", "of", "execution", "steps"],
            "cost_estimate": "estimated relative cost (e.g., 'Low', 'Medium', 'High')",
            "performance_risks": ["list", "of", "potential", "issues"],
            "complexity": "O(n) complexity analysis"
        }}
        
        Be specific and technical in your analysis.
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.1}
            )
            
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response['response'], re.DOTALL)
            if json_match:
                plan = json.loads(json_match.group())
            else:
                plan = {
                    "summary": "Could not parse execution plan",
                    "performance_risks": ["Analysis failed - please check query syntax"]
                }
        except Exception as e:
            plan = {
                "summary": f"Analysis error: {str(e)}",
                "performance_risks": ["Could not generate plan"]
            }
        
        return self.validate_explain_plan(plan)
    
    def recommend_indexes(self, query, dialect):
        """Get validated index recommendations (from sql_optimizer)"""
        prompt = f"""
        For this {dialect} query, recommend optimal indexes:
        
        ```sql
        {query}
        ```
        
        Return a JSON array of index recommendations with these exact fields for each:
        [{{
            "columns": ["column1", "column2"],
            "index_type": "BTREE/HASH/GIN/etc",
            "creation_sql": "CREATE INDEX idx_name ON table(columns)",
            "expected_impact": "High/Medium/Low",
            "tradeoffs": "storage and update performance considerations",
            "composite_justification": "why these columns together (if multi-column)"
        }}]
        
        Consider selectivity, cardinality, and query patterns.
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.2}
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                indexes = json.loads(json_match.group())
            else:
                indexes = []
        except Exception as e:
            indexes = [{
                "columns": ["Unknown"],
                "creation_sql": f"-- Error: {str(e)}",
                "expected_impact": "Unknown"
            }]
        
        return [self.validate_index_recommendation(idx, query, dialect) for idx in indexes]
    
    def suggest_alternative_queries(self, query, dialect):
        """Generate validated query alternatives (from sql_optimizer)"""
        prompt = f"""
        Optimize this {dialect} query and provide 2 alternative versions:
        
        ```sql
        {query}
        ```
        
        Return JSON array with exactly 2 alternatives:
        [{{
            "sql": "optimized SQL query",
            "rationale": "specific optimization strategy used",
            "estimated_gain": "e.g., '2-5x faster' or '50% less I/O'"
        }}]
        
        Focus on:
        - Join order optimization
        - Subquery elimination
        - Index-friendly predicates
        - Aggregation pushdown
        - CTE vs subquery tradeoffs
        """
        
        try:
            response = ollama.generate(
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                prompt=prompt,
                options={"temperature": 0.3}
            )
            
            # Extract JSON array from response
            json_match = re.search(r'\[.*\]', response['response'], re.DOTALL)
            if json_match:
                alternatives = json.loads(json_match.group())
            else:
                alternatives = []
        except Exception as e:
            alternatives = [{
                "sql": query,
                "rationale": "Original query (optimization failed)",
                "estimated_gain": "0%"
            }]
        
        return [self.validate_alternative_query(alt, query, dialect) for alt in alternatives[:2]]
    
    # ========== HELPER METHODS ==========
    
    def _extract_sql(self, text: str) -> str:
        """Extract SQL code from response"""
        # Look for code blocks
        sql_match = re.search(r'```sql\n(.*?)\n```', text, re.DOTALL)
        if sql_match:
            return sql_match.group(1).strip()
        
        # Fallback: look for SELECT/INSERT/UPDATE/DELETE/CREATE
        lines = text.split('\n')
        sql_lines = []
        in_sql = False
        
        for line in lines:
            if re.match(r'^\s*(SELECT|INSERT|UPDATE|DELETE|CREATE|WITH|ALTER)', line, re.IGNORECASE):
                in_sql = True
            if in_sql and line.strip():
                sql_lines.append(line)
            elif in_sql and not line.strip() and sql_lines:
                break
                
        return '\n'.join(sql_lines) if sql_lines else text
    
    def _extract_indexes(self, text: str) -> list:
        """Extract CREATE INDEX statements"""
        indexes = []
        for match in re.finditer(r'CREATE\s+(?:UNIQUE\s+)?INDEX.*?;', text, re.IGNORECASE | re.DOTALL):
            indexes.append(match.group(0).strip())
        return indexes
    
    def _extract_explanation(self, text: str) -> str:
        """Extract explanation text, removing SQL code"""
        # Remove SQL blocks
        text = re.sub(r'```sql.*?```', '', text, flags=re.DOTALL)
        text = re.sub(r'CREATE\s+(?:UNIQUE\s+)?INDEX.*?;', '', text, flags=re.IGNORECASE | re.DOTALL)
        
        # Clean up
        lines = [line.strip() for line in text.split('\n') if line.strip()]
        return '\n'.join(lines)


# ========== STREAMLIT UI WITH ALL FEATURES ==========

def show():
    st.set_page_config(
        page_title="SQL Toolkit - Complete",
        page_icon="🛢️",
        layout="wide"
    )
    
    st.title("🛢️ Complete SQL Toolkit")
    st.caption("All SQL features unified - Generate, Optimize, Pipeline, Learn, Connect")
    
    # Initialize toolkit
    toolkit = UnifiedSQLToolkit()
    
    # Initialize session state
    if "generated_sql" not in st.session_state:
        st.session_state.generated_sql = ""
    if "pipeline_state" not in st.session_state:
        st.session_state.pipeline_state = {
            "nl_description": "",
            "generated_sql": "",
            "optimized_sql": "",
            "current_step": 1
        }
    if "active_conn" not in st.session_state:
        st.session_state.active_conn = None
    if "last_query_id" not in st.session_state:
        st.session_state.last_query_id = None
    
    # Sidebar with model selection and examples
    with st.sidebar:
        st.subheader("🤖 AI Model")
        # Model selection - dynamically load from Ollama
        available_models = get_available_models(["deepseek-coder:6.7b", "deepseek-r1:6.7b"])
        sql_model = st.selectbox("SQL Generation Model", 
                                available_models,
                                index=0,
                                help="Models currently available in Ollama",
                                key="sql_model")
        
        st.divider()
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
    
    # Main tabs with all features
    tabs = st.tabs([
        "🚀 Generate",
        "⚡ Optimize", 
        "🔄 Pipeline",
        "🌐 Translate",
        "🔒 Security",
        "🔌 Database",
        "📚 Learn"
    ])
    
    # TAB 1: SQL Generation (Enhanced)
    with tabs[0]:
        render_generation_tab(toolkit)
    
    # TAB 2: SQL Optimization (Professional)
    with tabs[1]:
        render_optimization_tab(toolkit)
    
    # TAB 3: SQL Pipeline (Step-by-step)
    with tabs[2]:
        render_pipeline_tab(toolkit)
    
    # TAB 4: SQL Translation
    with tabs[3]:
        render_translation_tab(toolkit)
    
    # TAB 5: Security Audit
    with tabs[4]:
        render_security_tab(toolkit)
    
    # TAB 6: Database Connectivity
    with tabs[5]:
        render_database_tab(toolkit)
    
    # TAB 7: Learning & Concepts
    with tabs[6]:
        render_learning_tab(toolkit)
    
    # Knowledge saving section (accessible from all tabs)
    if st.session_state.last_query_id and st.session_state.generated_sql:
        render_knowledge_save_section(toolkit)


def render_generation_tab(toolkit):
    """SQL Generation with all advanced features"""
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
            db_type = st.radio("Target Database", ["PostgreSQL", "Oracle", "MySQL", "SQLite"], horizontal=True)
        
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
                result = toolkit.generate_sql(
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
                
                # Detect and display concepts
                if kg:
                    concepts = kg.detect_concepts_in_query(result['sql'])
                    if concepts:
                        st.subheader("💡 SQL Concepts Used")
                        concept_cols = st.columns(min(3, len(concepts)))
                        for i, concept_id in enumerate(concepts[:6]):
                            concept = kg.get_concept(concept_id)
                            if concept:
                                with concept_cols[i % 3]:
                                    st.info(f"**{concept.get('name')}**\n{concept.get('description', '')[:60]}...")
                
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
                query_id = toolkit.db.log_query(
                    tool="sql_generator",
                    model=st.session_state.get('sql_model', 'deepseek-coder:6.7b'),
                    prompt=f"{db_type} query: {query_desc}",
                    response=result['sql']
                )
                st.session_state.last_query_id = query_id
                
                # Quick analysis
                with st.spinner("Analyzing query..."):
                    analysis = toolkit.explain_sql(result['sql'], db_type)
                
                st.subheader("Query Analysis")
                st.markdown(analysis)


def render_optimization_tab(toolkit):
    """Professional SQL optimization with validation"""
    st.subheader("SQL Performance Optimizer")
    
    # Professional disclaimer
    with st.expander("⚠️ Professional Advisory", expanded=True):
        st.warning("""
        **AI-Generated Optimizations - Always Validate:**
        1. Test functional equivalence in staging environment
        2. Verify performance with EXPLAIN ANALYZE
        3. Check index impact on write operations
        4. Compare results with production data samples
        """)
    
    sql_to_optimize = st.text_area(
        "SQL to optimize", 
        height=200,
        value=st.session_state.generated_sql,
        placeholder="SELECT * FROM orders WHERE customer_id = 123..."
    )
    
    col1, col2 = st.columns(2)
    with col1:
        opt_db_type = st.radio(
            "Database Type", 
            ["PostgreSQL", "Oracle", "MySQL"], 
            horizontal=True, 
            key="opt_db_type"
        )
        improvement_type = st.selectbox(
            "Optimization Goal",
            ["performance", "memory", "readability"]
        )
    
    with col2:
        st.write("**Validation Level:**")
        validation_level = st.select_slider(
            "",
            options=["Basic", "Standard", "Comprehensive"],
            value="Standard",
            label_visibility="collapsed"
        )
        
        has_explain = st.checkbox("I have EXPLAIN ANALYZE output")
    
    if has_explain:
        explain_input = st.text_area("Paste EXPLAIN ANALYZE output", height=150)
    else:
        explain_input = None
    
    if st.button("🔍 Analyze & Optimize", type="primary", key="opt_sql"):
        if not sql_to_optimize.strip():
            st.warning("Please provide SQL to optimize")
        else:
            # Validation steps
            with st.status("Validating and analyzing query...", expanded=True) as status:
                # Step 1: Syntax validation
                st.write("🔍 Validating SQL syntax...")
                syntax_ok, syntax_msg = toolkit.validate_sql(sql_to_optimize, opt_db_type)
                if not syntax_ok:
                    st.error(f"❌ Syntax Error: {syntax_msg}")
                    status.update(label="Validation failed", state="error", expanded=True)
                    return
                st.write("✅ Syntax valid")
                
                # Step 2: Safety check
                st.write("🛡️ Checking query safety...")
                safety_ok, safety_msg = toolkit.check_query_safety(sql_to_optimize)
                if not safety_ok:
                    st.error(f"❌ Safety Issue: {safety_msg}")
                    status.update(label="Safety check failed", state="error", expanded=True)
                    return
                st.write("✅ Query is safe to analyze")
                
                # Step 3: Query plan analysis
                st.write("📊 Analyzing execution plan...")
                plan = toolkit.explain_query_plan(sql_to_optimize, opt_db_type)
                
                # Step 4: Index recommendations
                st.write("📈 Generating index recommendations...")
                indexes = toolkit.recommend_indexes(sql_to_optimize, opt_db_type)
                
                # Step 5: Query alternatives
                st.write("⚡ Creating optimized alternatives...")
                alternatives = toolkit.suggest_alternative_queries(sql_to_optimize, opt_db_type)
                
                # Step 6: Get schema info if connected
                schema_info = None
                if st.session_state.active_conn and st.session_state.active_conn['type'] == opt_db_type:
                    schema_info = st.session_state.active_conn['schema']
                
                # Step 7: Full optimization
                st.write("🚀 Generating comprehensive optimization...")
                with st.spinner("Analyzing query performance..."):
                    optimization = toolkit.optimize_sql(
                        sql_to_optimize, 
                        opt_db_type, 
                        schema_info,
                        explain_input,
                        improvement_type
                    )
                
                status.update(label="✅ Optimization complete!", state="complete", expanded=False)
            
            # Display results
            display_optimization_results(plan, indexes, alternatives, optimization, toolkit)


def render_pipeline_tab(toolkit):
    """Natural language SQL pipeline with concepts"""
    st.subheader("SQL Development Pipeline")
    st.caption("Build queries step by step with integrated learning")
    
    # Initialize pipeline state
    state = st.session_state.pipeline_state
    
    # Progress indicator
    progress_cols = st.columns(4)
    steps = [
        ("1️⃣ Describe", "Natural Language", 1),
        ("2️⃣ Generate", "SQL Query", 2),
        ("3️⃣ Optimize", "Performance", 3),
        ("4️⃣ Understand", "Learn & Test", 4)
    ]
    
    for col, (title, subtitle, step_num) in zip(progress_cols, steps):
        with col:
            if state["current_step"] >= step_num:
                st.success(f"**{title}**")
            else:
                st.info(f"**{title}**")
            st.caption(subtitle)
    
    # Progress bar
    progress = (state["current_step"] - 1) / 4
    st.progress(progress, text=f"Step {state['current_step']} of 4")
    
    # Step 1: Natural Language
    with st.expander("📝 STEP 1: Describe Your Data Need", 
                     expanded=state["current_step"] == 1):
        render_pipeline_step1(toolkit, state)
    
    # Step 2: Generated SQL
    if state["current_step"] >= 2 and state["generated_sql"]:
        with st.expander("⚡ STEP 2: Generated SQL", 
                        expanded=state["current_step"] == 2):
            render_pipeline_step2(toolkit, state)
    
    # Step 3: Optimization
    if state["current_step"] >= 3 and state["optimized_sql"]:
        with st.expander("🚀 STEP 3: Optimization Results", 
                        expanded=state["current_step"] == 3):
            render_pipeline_step3(toolkit, state)
    
    # Step 4: Understanding
    if state["current_step"] >= 4:
        with st.expander("🧠 STEP 4: Understand Your Query", expanded=True):
            render_pipeline_step4(toolkit, state)


def render_translation_tab(toolkit):
    """SQL dialect translation"""
    st.subheader("SQL Dialect Translator")
    st.caption("Convert queries between Oracle and PostgreSQL")
    
    col1, col2 = st.columns(2)
    with col1:
        source_db = st.selectbox("From", ["Oracle", "PostgreSQL", "MySQL", "SQLite"], key="source_db")
    with col2:
        target_db = st.selectbox("To", ["PostgreSQL", "Oracle", "MySQL", "SQLite"], key="target_db")
    
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
                translation = toolkit.translate_sql(sql_to_translate, source_db, target_db)
            
            st.subheader("Translated SQL")
            st.code(translation, language="sql")
            
            # Update generated SQL
            st.session_state.generated_sql = translation
            
            # Common conversion notes
            with st.expander("📚 Common Conversions"):
                st.markdown("""
                | Oracle | PostgreSQL | MySQL | SQLite |
                |--------|------------|-------|---------|
                | ROWNUM | LIMIT | LIMIT | LIMIT |
                | NVL() | COALESCE() | IFNULL() | COALESCE() |
                | TO_DATE() | TO_DATE() / ::date | STR_TO_DATE() | DATE() |
                | SYSDATE | CURRENT_TIMESTAMP | NOW() | datetime('now') |
                | CONNECT BY | WITH RECURSIVE | WITH RECURSIVE | WITH RECURSIVE |
                """)


def render_security_tab(toolkit):
    """SQL security scanner"""
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
                audit_result = toolkit.detect_vulnerabilities(sql_to_audit)
            
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


def render_database_tab(toolkit):
    """Database connectivity and live features"""
    if not SQLALCHEMY_AVAILABLE:
        st.warning("SQLAlchemy not installed. Install with: pip install sqlalchemy")
        return
    
    st.subheader("🔌 Live Database Connection")
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
                engine = toolkit.get_db_engine(
                    db_type,
                    {
                        "host": host, "port": port, "dbname": dbname,
                        "user": user, "password": password, "service": service
                    }
                )
                if engine:
                    with st.spinner("Fetching schema..."):
                        schema = toolkit.get_schema_summary(engine)
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
    
    # Connection status and features
    if st.session_state.active_conn:
        conn_info = st.session_state.active_conn
        st.success(f"🔗 Connected to {conn_info['type']} @ {conn_info['host']}:{conn_info['dbname']} | Tables: {len(conn_info['schema'])}")
        
        # Schema explorer
        with st.expander("📊 Database Schema", expanded=False):
            if conn_info['schema']:
                search = st.text_input("Search tables", placeholder="Filter tables...")
                
                for table, info in conn_info['schema'].items():
                    if search.lower() in table.lower():
                        with st.expander(f"📋 {table}"):
                            st.write("**Columns:**")
                            for col in info['columns']:
                                st.write(f"• {col['name']} ({col['type']})")
                            if info['indexes']:
                                st.write("\n**Indexes:**")
                                for idx in info['indexes']:
                                    st.write(f"• {idx}")
            else:
                st.write("No schema information available")
        
        # Query executor
        with st.expander("▶️ Execute Query", expanded=False):
            test_sql = st.text_area("Test SQL", height=150, placeholder="SELECT * FROM ...")
            
            if st.button("▶️ Execute (First 50 rows)", key="exec_test"):
                with st.spinner("Executing query..."):
                    results = toolkit.execute_safe(
                        conn_info['engine'],
                        test_sql
                    )
                
                st.subheader("Execution Results")
                if isinstance(results, pd.DataFrame):
                    st.dataframe(results)
                    st.caption(f"Showing first {len(results)} rows")
                else:
                    st.error(results)


def render_learning_tab(toolkit):
    """SQL concepts and learning features"""
    st.subheader("📚 SQL Learning Center")
    
    # Check if knowledge graph is available
    if not kg:
        st.info("Knowledge graph features not available. Check utils/knowledge_graph.py")
        return
    
    # Concept explorer
    all_concepts = list(kg.graph.nodes.values()) if hasattr(kg, 'graph') else []
    
    if all_concepts:
        col1, col2 = st.columns([2, 1])
        
        with col1:
            st.write("**Explore SQL Concepts:**")
            concept_names = [c.get("name", "Unknown") for c in all_concepts]
            selected_concept_name = st.selectbox(
                "Select a concept", 
                options=concept_names,
                index=0
            )
            
            # Find concept by name
            selected_concept = None
            for concept in all_concepts:
                if concept.get("name") == selected_concept_name:
                    selected_concept = concept
                    break
            
            if selected_concept:
                st.markdown(f"### {selected_concept.get('name', 'Unknown')}")
                st.info(selected_concept.get("description", "No description available"))
                
                # Show prerequisites
                if selected_concept.get("prerequisites"):
                    st.write("**Prerequisites:**")
                    for prereq in selected_concept["prerequisites"]:
                        st.write(f"• {prereq}")
                
                # Show resources
                if selected_concept.get("resources"):
                    st.write("**Learning Resources:**")
                    for resource in selected_concept["resources"]:
                        st.markdown(f"• [{resource.get('title', 'Resource')}]({resource.get('url', '#')})")
        
        with col2:
            # Visualize knowledge graph
            st.write("**Knowledge Graph:**")
            try:
                graph_img = kg.visualize_graph()
                st.image(graph_img, use_column_width=True)
            except:
                st.info("Graph visualization not available")
    
    # Learning path builder
    st.divider()
    st.subheader("🗺️ Build Learning Path")
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        start_concept = st.selectbox(
            "Start From",
            options=["Basic SELECT", "JOINs", "Aggregation", "Subqueries"],
            index=0
        )
    
    with col2:
        target_concept = st.selectbox(
            "Target Concept",
            options=["Window Functions", "CTEs", "Performance Tuning", "Advanced Analytics"],
            index=0
        )
    
    with col3:
        st.write("")  # Spacing
        if st.button("🚀 Generate Path", type="primary", use_container_width=True):
            st.info(f"Learning path: {start_concept} → {target_concept}")
            # In a real implementation, this would generate an actual learning path


# ========== HELPER FUNCTIONS FOR UI COMPONENTS ==========

def display_optimization_results(plan, indexes, alternatives, optimization, toolkit):
    """Display comprehensive optimization results"""
    # EXPLAIN plan section
    st.subheader("📊 Execution Plan Analysis")
    
    if plan.get("validation"):
        display_validation_badge(plan.get("validation"))
    
    col1, col2 = st.columns([2, 1])
    with col1:
        st.write("**Summary:**")
        st.info(plan.get("summary", "No summary available"))
        
        if plan.get("performance_risks"):
            st.write("**Performance Risks:**")
            for risk in plan.get("performance_risks", []):
                st.error(f"• {risk}")
        else:
            st.success("• No major performance risks identified")
            
    with col2:
        st.write("**Complexity Analysis:**")
        st.code(plan.get("complexity", "Unknown"), language="text")
        
        if plan.get("cost_estimate"):
            cost_color = "🟢" if plan["cost_estimate"] == "Low" else "🟡" if plan["cost_estimate"] == "Medium" else "🔴"
            st.metric("Estimated Cost", f"{cost_color} {plan['cost_estimate']}")
    
    if plan.get("steps"):
        with st.expander("View Execution Steps"):
            for i, step in enumerate(plan["steps"], 1):
                st.write(f"{i}. {step}")
    
    # Optimization results
    st.subheader("🚀 Optimization Results")
    st.markdown(optimization.get("raw_response", "No optimization available"))
    
    if optimization.get("optimized_sql"):
        col1, col2 = st.columns(2)
        with col1:
            st.write("**Original SQL:**")
            st.code(st.session_state.get("sql_to_optimize", ""), language="sql")
        with col2:
            st.write("**Optimized SQL:**")
            st.code(optimization["optimized_sql"], language="sql")
    
    # Index recommendations
    st.subheader("📈 Index Recommendations")
    
    if not indexes:
        st.info("No index recommendations generated")
    else:
        for i, idx in enumerate(indexes):
            columns = idx.get('columns', ['Unknown'])
            with st.expander(f"Index {i+1}: {', '.join(columns)}", expanded=i==0):
                col1, col2 = st.columns([1, 2])
                
                with col1:
                    impact = idx.get("expected_impact", "Unknown")
                    impact_color = "🟢" if impact == "High" else "🟡" if impact == "Medium" else "⚪"
                    st.metric("Impact", f"{impact_color} {impact}")
                    
                    if idx.get("index_type"):
                        st.write(f"**Type:** {idx['index_type']}")
                    
                    if idx.get("validation"):
                        display_validation_badge(idx.get("validation"))
                
                with col2:
                    st.code(idx.get("creation_sql", "-- No SQL generated"), language="sql")
                
                if idx.get("tradeoffs"):
                    st.info(f"**Tradeoffs:** {idx['tradeoffs']}")
                    
                if idx.get("composite_justification") and len(columns) > 1:
                    st.write(f"**Why these columns together:** {idx['composite_justification']}")
    
    # Query alternatives
    st.subheader("⚡ Optimized Query Alternatives")
    
    if not alternatives:
        st.info("No alternative queries generated")
    else:
        for i, alt in enumerate(alternatives):
            with st.expander(
                f"Alternative {i+1}: {alt.get('rationale', 'No description')}", 
                expanded=i==0
            ):
                st.code(alt.get("sql", "-- No SQL generated"), language="sql")
                
                col1, col2 = st.columns(2)
                with col1:
                    gain = alt.get("estimated_gain", "Unknown")
                    if "faster" in str(gain).lower() or "%" in str(gain):
                        st.success(f"**Estimated Gain:** {gain}")
                    else:
                        st.info(f"**Estimated Gain:** {gain}")
                
                with col2:
                    if alt.get("validation"):
                        display_equivalence_validation(alt["validation"])
    
    # Professional validation checklist
    st.subheader("🔬 Validation Checklist")
    with st.expander("Professional Validation Steps", expanded=False):
        st.markdown("""
        ### 1. Functional Testing
        - ✓ Verify results match original query with test data
        - ✓ Check edge cases (NULL values, empty results)
        - ✓ Test with different data distributions
        
        ### 2. Performance Testing  
        - ✓ Run `EXPLAIN ANALYZE` on both queries
        - ✓ Compare actual vs estimated rows
        - ✓ Check buffer/cache hit rates
        - ✓ Test with production-like data volume
        
        ### 3. Index Implementation
        - ✓ Check for existing similar indexes
        - ✓ Test index creation time on clone
        - ✓ Monitor write performance impact
        - ✓ Verify index usage with `EXPLAIN`
        
        ### 4. Production Rollout
        - ✓ Deploy during low-traffic window
        - ✓ Monitor query performance metrics
        - ✓ Have rollback plan ready
        - ✓ Document changes for team
        """)


def display_validation_badge(validation):
    """Show validation status badge"""
    if not validation:
        return
    
    confidence = validation.get("confidence", 0)
    if confidence > 0.8:
        st.success(f"✅ High Confidence ({confidence*100:.0f}%)")
    elif confidence > 0.6:
        st.warning(f"⚠️ Moderate Confidence ({confidence*100:.0f}%)")
    else:
        st.error(f"❌ Low Confidence ({confidence*100:.0f}%)")
    
    if validation.get("warnings"):
        with st.expander("⚠️ Validation Warnings"):
            warning_descriptions = {
                "too_many_columns": "Indexes with >3 columns may have diminishing returns and higher maintenance cost",
                "low_selectivity": "Boolean/flag columns typically have low selectivity, making indexes less effective",
                "functional_dependency": "Multi-column indexes require careful column ordering based on query patterns",
            }
            for warning in validation["warnings"]:
                st.write(f"• {warning_descriptions.get(warning, warning)}")
    
    if validation.get("requires_db_validation"):
        st.info("💡 **Pro Tip:** Verify with actual EXPLAIN ANALYZE on your database")


def display_equivalence_validation(validation):
    """Show query equivalence validation"""
    if validation.get("equivalent") is True:
        st.success("✅ Functionally Equivalent")
    elif validation.get("equivalent") is False:
        st.error("❌ Functional Differences Detected")
        with st.expander("View Differences"):
            for diff in validation.get("differences", []):
                st.write(f"• {diff}")
    else:
        st.warning("⚠️ Equivalence Unknown - Manual validation recommended")


def render_knowledge_save_section(toolkit):
    """Knowledge base saving interface"""
    st.divider()
    with st.expander("💾 Save to Knowledge Base", expanded=False):
        st.subheader("Save SQL Pattern")
        
        title = st.text_input(
            "Title", 
            value=f"SQL Query: Generated at {datetime.now().strftime('%Y-%m-%d %H:%M')}"
        )
        
        # Get existing categories
        categories = toolkit.db.get_knowledge_categories() or []
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
            
            saved = toolkit.db.save_knowledge_unit(
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


# Pipeline step rendering functions
def render_pipeline_step1(toolkit, state):
    """Render pipeline step 1"""
    st.write("Tell me what data you need in plain English:")
    
    nl_description = st.text_area(
        "Your description:",
        height=100,
        placeholder="Example: Show me the top 5 customers by total spending in the last quarter",
        value=state["nl_description"],
        key="nl_input"
    )
    
    col1, col2 = st.columns([2, 1])
    with col1:
        dialect = st.selectbox(
            "Database Type:",
            options=["PostgreSQL", "MySQL", "SQL Server", "SQLite", "Oracle"],
            index=0,
            key="dialect_select"
        )
    
    with col2:
        st.write("")  # Spacing
        st.write("")  # Spacing
        if st.button("▶️ Generate SQL", type="primary", key="generate_btn", use_container_width=True):
            if not nl_description.strip():
                st.error("Please describe what data you need")
            else:
                with st.spinner("🤖 Generating SQL query..."):
                    generated_sql = toolkit.generate_sql_from_nl(nl_description, dialect)
                    state.update({
                        "nl_description": nl_description,
                        "generated_sql": generated_sql,
                        "current_step": 2
                    })
                    st.rerun()


def render_pipeline_step2(toolkit, state):
    """Render pipeline step 2"""
    st.write("Here's your generated SQL query:")
    
    # Editable SQL
    edited_sql = st.text_area(
        "Generated SQL (you can edit):",
        value=state["generated_sql"],
        height=200,
        key="sql_editor"
    )
    
    # Detect and display concepts
    if kg:
        detected_concepts = kg.detect_concepts_in_query(edited_sql)
        
        if detected_concepts:
            st.subheader("💡 Concepts in This Query")
            
            # Create concept cards
            concept_cols = st.columns(min(3, len(detected_concepts)))
            for i, concept_id in enumerate(detected_concepts[:6]):
                concept = kg.get_concept(concept_id)
                if concept:
                    with concept_cols[i % 3]:
                        difficulty = concept.get('difficulty', 'Beginner')
                        color_map = {
                            'Beginner': '#51CF66',
                            'Intermediate': '#FFB800',
                            'Advanced': '#FF6B6B'
                        }
                        color = color_map.get(difficulty, '#51CF66')
                        
                        st.markdown(f"""
                        <div style="
                            border: 2px solid {color};
                            border-radius: 8px;
                            padding: 12px;
                            margin: 5px 0;
                            background-color: rgba(255,255,255,0.05);
                        ">
                            <h5 style="margin: 0; color: {color};">
                                {concept.get('name', concept_id)}
                            </h5>
                            <p style="font-size: 0.85em; margin: 8px 0;">
                                {concept.get('description', '')[:60]}...
                            </p>
                            <p style="font-size: 0.75em; margin: 0; color: {color};">
                                Difficulty: {difficulty}
                            </p>
                        </div>
                        """, unsafe_allow_html=True)
    
    # Action buttons
    col1, col2, col3 = st.columns(3)
    with col1:
        if st.button("🔄 Regenerate", key="regen_btn", use_container_width=True):
            with st.spinner("Generating alternative..."):
                new_sql = toolkit.generate_sql_from_nl(
                    state["nl_description"], 
                    st.session_state.get("dialect_select", "PostgreSQL")
                )
                state["generated_sql"] = new_sql
                st.rerun()
    
    with col2:
        if st.button("📋 Copy SQL", key="copy_sql_btn", use_container_width=True):
            st.info("SQL copied to clipboard!")
    
    with col3:
        if st.button("▶️ Optimize SQL", type="primary", key="optimize_btn", use_container_width=True):
            with st.spinner("🔍 Analyzing and optimizing..."):
                result = toolkit.optimize_sql(edited_sql, st.session_state.get("dialect_select", "PostgreSQL"))
                state.update({
                    "generated_sql": edited_sql,
                    "optimized_sql": result.get("optimized_sql", edited_sql),
                    "optimization_summary": result.get("raw_response", ""),
                    "current_step": 3
                })
                st.rerun()


def render_pipeline_step3(toolkit, state):
    """Render pipeline step 3"""
    st.write("**Optimization Summary:**")
    st.info(state["optimization_summary"][:500] + "..." if len(state["optimization_summary"]) > 500 else state["optimization_summary"])
    
    # Before/After comparison
    st.write("**Query Comparison:**")
    col1, col2 = st.columns(2)
    
    with col1:
        st.write("Original Query:")
        st.code(state["generated_sql"], language="sql")
    
    with col2:
        st.write("Optimized Query:")
        st.code(state["optimized_sql"], language="sql")
    
    # Navigation
    col1, col2, col3 = st.columns(3)
    with col1:
        if st.button("⬅️ Back", key="back_to_2", use_container_width=True):
            state["current_step"] = 2
            st.rerun()
    
    with col3:
        if st.button("▶️ Understand", type="primary", key="understand_btn", use_container_width=True):
            state["current_step"] = 4
            st.rerun()


def render_pipeline_step4(toolkit, state):
    """Render pipeline step 4"""
    tab1, tab2, tab3 = st.tabs([
        "📖 Explanation", 
        "🧪 Test Query", 
        "💾 Save"
    ])
    
    with tab1:
        st.subheader("Plain English Explanation")
        with st.spinner("Generating explanation..."):
            explanation = toolkit.explain_sql(
                state["optimized_sql"], 
                st.session_state.get("dialect_select", "PostgreSQL")
            )
        st.markdown(explanation)
    
    with tab2:
        st.subheader("Test Your Query")
        st.write("Test your optimized query with sample data:")
        
        # Example data templates
        example_data_templates = {
            "customers_orders": """{
  "customers": [
    {"id": 1, "name": "Alice Johnson", "email": "alice@email.com"},
    {"id": 2, "name": "Bob Smith", "email": "bob@email.com"}
  ],
  "orders": [
    {"id": 101, "customer_id": 1, "amount": 150.99, "order_date": "2024-01-10"},
    {"id": 102, "customer_id": 2, "amount": 299.99, "order_date": "2024-02-20"}
  ]
}"""
        }
        
        sample_data = st.text_area(
            "Sample Data (JSON format):",
            height=200,
            value=example_data_templates["customers_orders"],
            key="sample_data_input"
        )
        
        if st.button("▶️ Run Test Query", type="primary", key="test_query_btn"):
            with st.spinner("Executing query on sample data..."):
                results = toolkit.test_query_with_sample_data(
                    state["optimized_sql"],
                    st.session_state.get("dialect_select", "PostgreSQL"),
                    sample_data
                )
            
            st.subheader("Query Results")
            st.markdown(results)
    
    with tab3:
        st.subheader("Save to Knowledge Base")
        
        save_title = st.text_input(
            "Title:",
            value=f"SQL: {state['nl_description'][:50]}...",
            key="save_title"
        )
        
        save_category = st.selectbox(
            "Category:",
            options=["SQL Pattern", "Optimized Query", "Business Query", "Report Query"],
            key="save_category"
        )
        
        if st.button("💾 Save Complete Pipeline", type="primary", key="save_pipeline_btn"):
            try:
                # Prepare complete content
                content = f"""-- Natural Language Description:
-- {state['nl_description']}

-- Original Generated SQL:
{state['generated_sql']}

-- Optimized SQL:
{state['optimized_sql']}

-- Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M')}
"""
                
                # Log to database
                query_id = toolkit.db.log_query(
                    tool="sql_pipeline",
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b"),
                    prompt=state['nl_description'],
                    response=content
                )
                
                # Save to knowledge base
                saved = toolkit.db.save_knowledge_unit(
                    query_id=query_id,
                    title=save_title,
                    content=content,
                    category=save_category
                )
                
                if saved:
                    st.success("✅ Complete pipeline saved to knowledge base!")
                    st.balloons()
                else:
                    st.error("Failed to save to knowledge base")
                    
            except Exception as e:
                st.error(f"Error saving: {str(e)}")


# Entry point
if __name__ == "__main__":
    show()