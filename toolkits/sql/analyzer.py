"""
SQL Toolkit Main Analyzer
Provides comprehensive SQL tools with TuoKit integration
"""

import re
from typing import Optional

import streamlit as st

from utils.tool_base import TuoKitToolBase

from .config import DEFAULT_MODELS, QUERY_TYPES, SAMPLE_SCHEMAS, SQL_DIALECTS
from .processors import (
    analyze_query_dependencies,
    analyze_query_performance,
    convert_sql_dialect,
    explain_sql_query,
    format_sql_query,
    generate_sample_data_query,
    generate_schema_from_description,
    generate_sql_query,
    generate_table_documentation,
    optimize_sql_query,
    sql_to_code,
    validate_sql_syntax,
)


class SQLToolkit(TuoKitToolBase):
    """
    Complete SQL workflow toolkit with multiple tools:
    - Query Generation from natural language
    - Query Optimization
    - Query Explanation
    - Syntax Validation
    - Schema Generation
    - Dialect Conversion
    - Performance Analysis
    """

    def __init__(self):
        super().__init__(
            tool_name="SQL Toolkit",
            tool_description="Complete SQL workflow - generate, optimize, explain, and analyze SQL queries",
        )
        self._init_session_state_vars()

    def _init_session_state_vars(self):
        """Initialize SQL-specific session state variables"""
        if "sql_history" not in st.session_state:
            st.session_state.sql_history = []
        if "last_generated_sql" not in st.session_state:
            st.session_state.last_generated_sql = ""
        if "selected_dialect" not in st.session_state:
            st.session_state.selected_dialect = "PostgreSQL"

    def run(self):
        """Main entry point for the SQL Toolkit"""
        st.title("🗄️ SQL Toolkit")
        st.caption("Complete SQL workflow - generate, optimize, explain, and analyze queries")

        # Show knowledge status
        self.show_knowledge_status_indicator()

        # Main navigation - organized by use case
        tool_tabs = st.tabs(
            [
                "🔨 Generate SQL",
                "🎨 Format SQL",
                "⚡ Optimize Query",
                "📖 Explain Query",
                "✅ Validate Syntax",
                "🏗️ Generate Schema",
                "📋 Doc Generator",
                "🔄 Convert Dialect",
                "💻 SQL to Code",
                "🎲 Sample Data",
                "📊 Analyze Performance",
            ]
        )

        with tool_tabs[0]:
            self._show_generate_sql()

        with tool_tabs[1]:
            self._show_format_sql()

        with tool_tabs[2]:
            self._show_optimize_query()

        with tool_tabs[3]:
            self._show_explain_query()

        with tool_tabs[4]:
            self._show_validate_syntax()

        with tool_tabs[5]:
            self._show_generate_schema()

        with tool_tabs[6]:
            self._show_doc_generator()

        with tool_tabs[7]:
            self._show_convert_dialect()

        with tool_tabs[8]:
            self._show_sql_to_code()

        with tool_tabs[9]:
            self._show_sample_data()

        with tool_tabs[10]:
            self._show_analyze_performance()

        # Show metrics in sidebar
        with st.sidebar:
            st.header("📊 Toolkit Metrics")
            self.show_metrics()

            # Show SQL history
            if st.session_state.sql_history:
                st.divider()
                st.subheader("📜 Recent Queries")
                for i, query in enumerate(reversed(st.session_state.sql_history[-5:])):
                    with st.expander(f"Query {len(st.session_state.sql_history) - i}", expanded=False):
                        st.code(query, language="sql")

    def _show_generate_sql(self):
        """SQL Generation tool UI"""
        st.header("🔨 Generate SQL from Natural Language")

        # Search for related knowledge
        self.show_knowledge_search_widget()

        col1, col2 = st.columns([3, 1])
        with col1:
            natural_language = st.text_area(
                "Describe what you want the SQL query to do:",
                height=100,
                placeholder="e.g., Find all customers who placed orders in the last 30 days with total spending over $100",
            )

        with col2:
            dialect = st.selectbox(
                "SQL Dialect:", SQL_DIALECTS, index=SQL_DIALECTS.index(st.session_state.selected_dialect)
            )
            st.session_state.selected_dialect = dialect

            model = st.selectbox("AI Model:", ["deepseek-coder:6.7b", "deepseek-r1:1.5b", "llama2:latest"], index=0)

        # Examples section
        with st.expander("📚 Example Queries", expanded=False):
            example = st.selectbox(
                "Select an example:",
                [
                    "Custom query...",
                    "Find top 10 customers by revenue",
                    "Show products low in stock (< 10 items)",
                    "List orders from last week with customer details",
                    "Calculate monthly sales totals for current year",
                    "Find duplicate email addresses in users table",
                ],
            )
            if example != "Custom query..." and st.button("Use this example"):
                st.session_state.example_query = example
                st.rerun()

        # Use example if selected
        if "example_query" in st.session_state:
            natural_language = st.session_state.example_query
            del st.session_state.example_query

        if st.button("🚀 Generate SQL", type="primary", disabled=not natural_language):
            with st.spinner("Generating SQL query..."):
                try:
                    # Generate with knowledge capture
                    prompt = f"""Generate a {dialect} SQL query for: {natural_language}

Please provide a clean, optimized query following {dialect} best practices."""

                    result = self.generate_with_capture(
                        prompt=prompt, model=model, temperature=0.3  # Lower temperature for more deterministic SQL
                    )

                    if not result["error"]:
                        sql = generate_sql_query(natural_language, dialect.lower(), model)

                        st.success("✅ SQL query generated successfully!")
                        st.code(sql, language="sql")

                        # Save to history
                        st.session_state.sql_history.append(sql)
                        st.session_state.last_generated_sql = sql

                        # Quick actions
                        col1, col2, col3 = st.columns(3)
                        with col1:
                            if st.button("📋 Copy to Clipboard"):
                                st.code(sql, language="sql")
                                st.info("Query displayed for copying")

                        with col2:
                            if st.button("⚡ Optimize This Query"):
                                st.session_state.optimize_query = sql
                                st.info("Switch to the Optimize tab")

                        with col3:
                            if st.button("📖 Explain This Query"):
                                st.session_state.explain_query = sql
                                st.info("Switch to the Explain tab")

                        # Offer to save to knowledge base
                        self.show_knowledge_save_widget(
                            default_title=f"SQL: {natural_language[:50]}...", default_category="SQL Query", content=sql
                        )
                    else:
                        st.error(f"Error generating SQL: {result['error']}")

                except Exception as e:
                    self.handle_error(e, "generating SQL query")

    def _show_optimize_query(self):
        """Query Optimization tool UI"""
        st.header("⚡ Optimize SQL Query")

        # Check if query was passed from generate tab
        initial_query = ""
        if "optimize_query" in st.session_state:
            initial_query = st.session_state.optimize_query
            del st.session_state.optimize_query

        query_to_optimize = st.text_area(
            "Enter SQL query to optimize:", value=initial_query, height=200, placeholder="Paste your SQL query here..."
        )

        col1, col2 = st.columns([1, 1])
        with col1:
            dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS, key="optimize_dialect")

        with col2:
            model = st.selectbox("AI Model:", ["deepseek-coder:6.7b", "deepseek-r1:1.5b"], key="optimize_model")

        if st.button("🚀 Optimize Query", type="primary", disabled=not query_to_optimize):
            with st.spinner("Optimizing query..."):
                try:
                    # First validate the query
                    validation = validate_sql_syntax(query_to_optimize, dialect.lower())

                    if not validation["is_valid"]:
                        st.error("Query has syntax errors:")
                        for error in validation["errors"]:
                            st.error(f"• {error}")
                        return

                    # Show warnings if any
                    if validation["warnings"]:
                        st.warning("Potential issues detected:")
                        for warning in validation["warnings"]:
                            st.warning(f"• {warning}")

                    # Optimize with knowledge capture
                    optimized = optimize_sql_query(query_to_optimize, dialect.lower(), model)

                    st.success("✅ Query optimized successfully!")

                    # Show before/after comparison
                    col1, col2 = st.columns(2)
                    with col1:
                        st.subheader("Original Query")
                        st.code(query_to_optimize, language="sql")

                    with col2:
                        st.subheader("Optimized Query")
                        st.code(optimized, language="sql")

                    # Performance analysis
                    st.subheader("📊 Performance Analysis")
                    original_analysis = analyze_query_performance(query_to_optimize, dialect.lower())
                    optimized_analysis = analyze_query_performance(optimized, dialect.lower())

                    col1, col2 = st.columns(2)
                    with col1:
                        st.metric("Original Complexity", original_analysis["complexity"])
                    with col2:
                        st.metric("Optimized Complexity", optimized_analysis["complexity"])

                    # Save to knowledge base
                    self.show_knowledge_save_widget(
                        default_title=f"SQL Optimization: {validation['query_type']} query",
                        default_category="SQL Optimization",
                        content=f"**Original:**\n```sql\n{query_to_optimize}\n```\n\n**Optimized:**\n```sql\n{optimized}\n```",
                    )

                except Exception as e:
                    self.handle_error(e, "optimizing query")

    def _show_explain_query(self):
        """Query Explanation tool UI"""
        st.header("📖 Explain SQL Query")

        # Check if query was passed from generate tab
        initial_query = ""
        if "explain_query" in st.session_state:
            initial_query = st.session_state.explain_query
            del st.session_state.explain_query

        query_to_explain = st.text_area(
            "Enter SQL query to explain:", value=initial_query, height=200, placeholder="Paste your SQL query here..."
        )

        dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS, key="explain_dialect")

        if st.button("🚀 Explain Query", type="primary", disabled=not query_to_explain):
            with st.spinner("Analyzing query..."):
                try:
                    explanation = explain_sql_query(query_to_explain, dialect.lower())

                    st.success("✅ Query explained successfully!")
                    st.markdown(explanation)

                    # Also show the query for reference
                    with st.expander("📄 Original Query", expanded=False):
                        st.code(query_to_explain, language="sql")

                    # Save explanation
                    self.show_knowledge_save_widget(
                        default_title="SQL Query Explanation",
                        default_category="SQL Documentation",
                        content=f"**Query:**\n```sql\n{query_to_explain}\n```\n\n**Explanation:**\n{explanation}",
                    )

                except Exception as e:
                    self.handle_error(e, "explaining query")

    def _show_validate_syntax(self):
        """Syntax Validation tool UI"""
        st.header("✅ Validate SQL Syntax")

        query_to_validate = st.text_area(
            "Enter SQL query to validate:", height=200, placeholder="Paste your SQL query here..."
        )

        dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS, key="validate_dialect")

        if st.button("🚀 Validate Syntax", type="primary", disabled=not query_to_validate):
            validation = validate_sql_syntax(query_to_validate, dialect.lower())

            if validation["is_valid"]:
                st.success(f"✅ Valid {validation['query_type']} query!")
            else:
                st.error("❌ Query has syntax errors")

            # Show errors
            if validation["errors"]:
                st.error("**Errors found:**")
                for error in validation["errors"]:
                    st.error(f"• {error}")

            # Show warnings
            if validation["warnings"]:
                st.warning("**Warnings:**")
                for warning in validation["warnings"]:
                    st.warning(f"• {warning}")

            # Show query info
            if validation["query_type"]:
                st.info(f"Query type: **{validation['query_type']}**")

            # Performance quick check
            if validation["is_valid"]:
                st.subheader("🔍 Quick Performance Check")
                analysis = analyze_query_performance(query_to_validate, dialect.lower())

                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("Complexity", analysis["complexity"])
                with col2:
                    st.metric("Estimated Cost", analysis["estimated_cost"])
                with col3:
                    st.metric("Suggestions", len(analysis["optimization_suggestions"]))

                if analysis["optimization_suggestions"]:
                    with st.expander("💡 Optimization Suggestions"):
                        for suggestion in analysis["optimization_suggestions"]:
                            st.info(f"• {suggestion}")

    def _show_generate_schema(self):
        """Schema Generation tool UI"""
        st.header("🏗️ Generate Database Schema")

        # Schema examples
        col1, col2 = st.columns([3, 1])
        with col1:
            example_choice = st.selectbox(
                "Use an example or create custom:", ["Custom Schema..."] + list(SAMPLE_SCHEMAS.keys())
            )

        with col2:
            dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS, key="schema_dialect")

        if example_choice != "Custom Schema...":
            schema_description = SAMPLE_SCHEMAS[example_choice]
        else:
            schema_description = st.text_area(
                "Describe your database schema:",
                height=150,
                placeholder="e.g., A project management system with users, projects, tasks, and comments...",
            )

        if st.button("🚀 Generate Schema", type="primary", disabled=not schema_description):
            with st.spinner("Generating schema..."):
                try:
                    schema_sql = generate_schema_from_description(schema_description, dialect.lower())

                    st.success("✅ Schema generated successfully!")
                    st.code(schema_sql, language="sql")

                    # Quick actions
                    col1, col2 = st.columns(2)
                    with col1:
                        st.download_button(
                            label="📥 Download SQL File",
                            data=schema_sql,
                            file_name=f"schema_{dialect.lower()}.sql",
                            mime="text/plain",
                        )

                    with col2:
                        if st.button("✅ Validate Schema"):
                            # Basic validation
                            lines = schema_sql.split("\n")
                            table_count = sum(1 for line in lines if "CREATE TABLE" in line.upper())
                            st.info(f"Schema contains {table_count} tables")

                    # Save to knowledge base
                    self.show_knowledge_save_widget(
                        default_title=f"Database Schema: {example_choice if example_choice != 'Custom Schema...' else 'Custom'}",
                        default_category="Database Schema",
                        content=schema_sql,
                    )

                except Exception as e:
                    self.handle_error(e, "generating schema")

    def _show_convert_dialect(self):
        """Dialect Conversion tool UI"""
        st.header("🔄 Convert SQL Dialect")

        col1, col2 = st.columns(2)
        with col1:
            from_dialect = st.selectbox("From Dialect:", SQL_DIALECTS, key="from_dialect")

        with col2:
            to_dialect = st.selectbox(
                "To Dialect:", SQL_DIALECTS, index=1 if SQL_DIALECTS[0] == from_dialect else 0, key="to_dialect"
            )

        query_to_convert = st.text_area(
            f"Enter {from_dialect} query:", height=200, placeholder=f"Paste your {from_dialect} query here..."
        )

        if st.button("🚀 Convert Query", type="primary", disabled=not query_to_convert or from_dialect == to_dialect):
            with st.spinner(f"Converting from {from_dialect} to {to_dialect}..."):
                try:
                    converted_sql = convert_sql_dialect(query_to_convert, from_dialect.lower(), to_dialect.lower())

                    st.success("✅ Query converted successfully!")

                    # Show before/after
                    col1, col2 = st.columns(2)
                    with col1:
                        st.subheader(f"{from_dialect} (Original)")
                        st.code(query_to_convert, language="sql")

                    with col2:
                        st.subheader(f"{to_dialect} (Converted)")
                        st.code(converted_sql, language="sql")

                    # Conversion notes
                    st.info(
                        f"""
                    **Conversion Notes:**
                    - Data types have been mapped to {to_dialect} equivalents
                    - Functions have been converted where possible
                    - Please review and test the converted query
                    """
                    )

                    # Save conversion
                    self.show_knowledge_save_widget(
                        default_title=f"SQL Conversion: {from_dialect} to {to_dialect}",
                        default_category="SQL Conversion",
                        content=f"**From {from_dialect}:**\n```sql\n{query_to_convert}\n```\n\n**To {to_dialect}:**\n```sql\n{converted_sql}\n```",
                    )

                except Exception as e:
                    self.handle_error(e, "converting dialect")

    def _show_analyze_performance(self):
        """Performance Analysis tool UI"""
        st.header("📊 Analyze Query Performance")

        query_to_analyze = st.text_area(
            "Enter SQL query to analyze:", height=200, placeholder="Paste your SQL query here..."
        )

        dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS, key="analyze_dialect")

        if st.button("🚀 Analyze Performance", type="primary", disabled=not query_to_analyze):
            analysis = analyze_query_performance(query_to_analyze, dialect.lower())

            # Display metrics
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Query Complexity", analysis["complexity"].upper())
            with col2:
                st.metric("Estimated Cost", analysis["estimated_cost"].upper())
            with col3:
                st.metric(
                    "Total Issues", len(analysis["optimization_suggestions"]) + len(analysis["index_recommendations"])
                )

            # Optimization suggestions
            if analysis["optimization_suggestions"]:
                st.subheader("💡 Optimization Suggestions")
                for suggestion in analysis["optimization_suggestions"]:
                    st.info(f"• {suggestion}")

            # Index recommendations
            if analysis["index_recommendations"]:
                st.subheader("🔍 Index Recommendations")
                for recommendation in analysis["index_recommendations"]:
                    st.warning(f"• {recommendation}")

            # Query breakdown
            with st.expander("📋 Query Analysis Details"):
                query_upper = query_to_analyze.upper()

                # Count various operations
                metrics = {
                    "JOIN operations": len(re.findall(r"\bJOIN\b", query_upper)),
                    "Subqueries": query_upper.count("(SELECT"),
                    "WHERE conditions": len(re.findall(r"\bWHERE\b|\bAND\b|\bOR\b", query_upper)),
                    "GROUP BY": "Yes" if "GROUP BY" in query_upper else "No",
                    "ORDER BY": "Yes" if "ORDER BY" in query_upper else "No",
                    "DISTINCT": "Yes" if "DISTINCT" in query_upper else "No",
                    "Aggregate functions": len(re.findall(r"\b(COUNT|SUM|AVG|MAX|MIN)\b", query_upper)),
                }

                for metric, value in metrics.items():
                    st.write(f"**{metric}:** {value}")

            # Show query for reference
            with st.expander("📄 Original Query"):
                st.code(query_to_analyze, language="sql")
    
    def _show_format_sql(self):
        """SQL Formatter tool UI"""
        st.header("🎨 Format SQL Query")
        st.caption("Beautify messy SQL queries for better readability")
        
        # Input area
        query_to_format = st.text_area(
            "Paste your messy SQL query:",
            height=200,
            placeholder="SELECT*FROM users WHERE status='active'AND created_at>='2024-01-01'ORDER BY name",
            help="Paste any SQL query - we'll clean it up and format it nicely!"
        )
        
        col1, col2 = st.columns([1, 1])
        with col1:
            indent_size = st.slider("Indentation size:", 2, 8, 4)
        with col2:
            uppercase_keywords = st.checkbox("Uppercase keywords", value=True)
        
        if st.button("🎨 Format SQL", type="primary", disabled=not query_to_format):
            try:
                # Format the query
                formatted = format_sql_query(query_to_format, indent_size)
                
                # Optionally uppercase keywords
                if uppercase_keywords:
                    keywords = ['SELECT', 'FROM', 'WHERE', 'AND', 'OR', 'JOIN', 'ON', 
                               'GROUP BY', 'ORDER BY', 'HAVING', 'LIMIT', 'INSERT', 
                               'UPDATE', 'DELETE', 'CREATE', 'ALTER', 'DROP']
                    for keyword in keywords:
                        formatted = re.sub(rf'\b{keyword}\b', keyword, formatted, flags=re.IGNORECASE)
                
                st.success("✅ SQL formatted successfully!")
                
                # Show before/after
                col1, col2 = st.columns(2)
                with col1:
                    st.subheader("Before")
                    st.code(query_to_format, language="sql")
                
                with col2:
                    st.subheader("After")
                    st.code(formatted, language="sql")
                
                # Copy button
                if st.button("📋 Copy Formatted SQL"):
                    st.code(formatted, language="sql")
                    st.info("Formatted SQL displayed for copying")
                
                # Analyze the formatted query
                with st.expander("📊 Query Analysis", expanded=False):
                    deps = analyze_query_dependencies(formatted)
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.metric("Tables", len(deps["tables"]))
                        if deps["tables"]:
                            st.write("Tables:", ", ".join(deps["tables"]))
                    with col2:
                        st.metric("Has Subquery", "Yes" if deps["has_subquery"] else "No")
                        st.metric("Has Union", "Yes" if deps["has_union"] else "No")
                    with col3:
                        st.metric("Has Aggregation", "Yes" if deps["has_aggregation"] else "No")
                        if deps["joins"]:
                            st.write(f"Joins: {len(deps['joins'])}")
                
            except Exception as e:
                self.handle_error(e, "formatting SQL")
    
    def _show_doc_generator(self):
        """Table Documentation Generator UI"""
        st.header("📋 Table Documentation Generator")
        st.caption("Generate comprehensive documentation for your database tables")
        
        # Table input method
        input_method = st.radio("How would you like to provide table information?", 
                               ["Enter manually", "Parse CREATE TABLE statement"])
        
        if input_method == "Enter manually":
            table_name = st.text_input("Table name:")
            
            # Dynamic column input
            num_columns = st.number_input("Number of columns:", min_value=1, max_value=50, value=3)
            
            columns = []
            cols = st.columns(4)
            for i in range(int(num_columns)):
                row = i % 4
                with cols[row]:
                    with st.container():
                        st.write(f"**Column {i+1}**")
                        col_name = st.text_input("Name", key=f"col_name_{i}")
                        col_type = st.text_input("Type", key=f"col_type_{i}", 
                                               placeholder="VARCHAR(255)")
                        col_null = st.checkbox("NOT NULL", key=f"col_null_{i}")
                        col_pk = st.checkbox("PRIMARY KEY", key=f"col_pk_{i}")
                        
                        if col_name:
                            columns.append({
                                "name": col_name,
                                "type": col_type,
                                "not_null": col_null,
                                "primary_key": col_pk
                            })
        
        else:  # Parse CREATE TABLE
            create_table_sql = st.text_area(
                "Paste your CREATE TABLE statement:",
                height=200,
                placeholder="CREATE TABLE users (\n    id SERIAL PRIMARY KEY,\n    email VARCHAR(255) NOT NULL,\n    created_at TIMESTAMP DEFAULT NOW()\n);"
            )
            
            if create_table_sql:
                # Basic parsing
                table_match = re.search(r'CREATE TABLE\s+(\w+)', create_table_sql, re.IGNORECASE)
                table_name = table_match.group(1) if table_match else "unknown_table"
                
                # Parse columns (simplified)
                columns = []
                column_pattern = r'(\w+)\s+([A-Z]+(?:\([^)]+\))?)'
                for match in re.finditer(column_pattern, create_table_sql):
                    if match.group(1).upper() not in ['CREATE', 'TABLE', 'PRIMARY', 'KEY', 'CONSTRAINT']:
                        columns.append({
                            "name": match.group(1),
                            "type": match.group(2),
                            "not_null": 'NOT NULL' in create_table_sql[match.end():match.end()+20],
                            "primary_key": 'PRIMARY KEY' in create_table_sql[match.end():match.end()+20]
                        })
        
        dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS)
        
        if st.button("📋 Generate Documentation", type="primary", 
                    disabled=not (table_name and columns) if input_method == "Enter manually" else not create_table_sql):
            with st.spinner("Generating documentation..."):
                try:
                    doc = generate_table_documentation(table_name, columns, dialect.lower())
                    
                    st.success("✅ Documentation generated successfully!")
                    
                    # Display in nice format
                    st.markdown(doc)
                    
                    # Download button
                    st.download_button(
                        label="📥 Download Documentation",
                        data=doc,
                        file_name=f"{table_name}_documentation.md",
                        mime="text/markdown"
                    )
                    
                    # Save to knowledge base
                    self.show_knowledge_save_widget(
                        default_title=f"Documentation: {table_name} table",
                        default_category="Database Documentation",
                        content=doc
                    )
                    
                except Exception as e:
                    self.handle_error(e, "generating documentation")
    
    def _show_sql_to_code(self):
        """SQL to Code converter UI"""
        st.header("💻 SQL to Code Converter")
        st.caption("Convert SQL queries to code in your favorite programming language")
        
        # Input SQL
        sql_query = st.text_area(
            "Enter SQL query to convert:",
            height=150,
            placeholder="SELECT u.name, COUNT(o.id) as order_count\nFROM users u\nLEFT JOIN orders o ON u.id = o.user_id\nGROUP BY u.name"
        )
        
        col1, col2, col3 = st.columns(3)
        with col1:
            languages = ["Python", "Java", "JavaScript", "C#", "PHP", "Ruby", "Go", "Smalltalk (VisualWorks)"]
            target_lang = st.selectbox("Target Language:", languages)
        
        with col2:
            # Libraries based on language
            library_options = {
                "Python": ["psycopg2", "sqlalchemy", "pandas", "sqlite3", "pymongo"],
                "Java": ["JDBC", "Hibernate", "Spring Data", "MyBatis"],
                "JavaScript": ["pg", "mysql2", "sequelize", "knex", "prisma"],
                "C#": ["ADO.NET", "Entity Framework", "Dapper"],
                "PHP": ["PDO", "mysqli", "Laravel Eloquent"],
                "Ruby": ["pg", "ActiveRecord", "Sequel"],
                "Go": ["database/sql", "GORM", "sqlx"],
                "Smalltalk (VisualWorks)": ["EXDI Framework", "PostgreSQL", "MySQL", "Oracle", "SQLite"]
            }
            
            libs = library_options.get(target_lang, ["default"])
            library = st.selectbox("Library/Framework:", libs)
        
        with col3:
            include_connection = st.checkbox("Include connection setup", value=True)
        
        if st.button("💻 Convert to Code", type="primary", disabled=not sql_query):
            with st.spinner(f"Converting to {target_lang}..."):
                try:
                    # Special handling for Smalltalk
                    if target_lang == "Smalltalk (VisualWorks)":
                        from .processors import sql_to_smalltalk_visualworks
                        # Map library selection to connection type
                        connection_map = {
                            "EXDI Framework": "postgresql",
                            "PostgreSQL": "postgresql",
                            "MySQL": "mysql",
                            "Oracle": "oracle",
                            "SQLite": "sqlite"
                        }
                        connection_type = connection_map.get(library, "postgresql")
                        code = sql_to_smalltalk_visualworks(sql_query, connection_type)
                    else:
                        code = sql_to_code(sql_query, target_lang.lower(), library.lower())
                    
                    st.success(f"✅ Converted to {target_lang} successfully!")
                    
                    # Display code with syntax highlighting
                    lang_map = {
                        "Python": "python",
                        "Java": "java", 
                        "JavaScript": "javascript",
                        "C#": "csharp",
                        "PHP": "php",
                        "Ruby": "ruby",
                        "Go": "go",
                        "Smalltalk (VisualWorks)": "smalltalk"
                    }
                    
                    st.code(code, language=lang_map.get(target_lang, "text"))
                    
                    # Helpful tips
                    with st.expander("💡 Usage Tips"):
                        if target_lang == "Smalltalk (VisualWorks)":
                            st.info(f"""
                            **Important notes for VisualWorks Smalltalk with {library}:**
                            
                            1. Ensure EXDI (External Database Interface) is loaded in your image
                            2. Configure database connection in Settings Tool or programmatically
                            3. The generated code follows VisualWorks 8.x conventions
                            4. Remember to handle DatabaseError exceptions appropriately
                            5. Close connections in ensure: blocks for proper cleanup
                            6. Consider using connection pooling for production applications
                            7. Test in your VisualWorks environment before deploying
                            
                            **Loading EXDI:**
                            ```smalltalk
                            "Load PostgreSQL support"
                            Parcel loadParcelByName: 'PostgreSQLEXDI'.
                            ```
                            """)
                        else:
                            st.info(f"""
                            **Important notes for {target_lang} with {library}:**
                            
                            1. Remember to install required dependencies
                            2. Update connection parameters with your database credentials
                            3. Handle errors appropriately in production
                            4. Consider using environment variables for sensitive data
                            5. Test thoroughly before deploying
                            """)
                    
                    # Save to knowledge base
                    self.show_knowledge_save_widget(
                        default_title=f"SQL to {target_lang}: {library}",
                        default_category="Code Conversion",
                        content=f"**Original SQL:**\n```sql\n{sql_query}\n```\n\n**{target_lang} Code:**\n```{lang_map.get(target_lang, 'text')}\n{code}\n```"
                    )
                    
                except Exception as e:
                    self.handle_error(e, "converting SQL to code")
    
    def _show_sample_data(self):
        """Sample Data Generator UI"""
        st.header("🎲 Sample Data Generator")
        st.caption("Generate realistic test data for your tables")
        
        # Table and columns input
        table_name = st.text_input("Table name:", placeholder="users")
        
        # Column input method
        col_input = st.radio("How to specify columns?", ["Enter manually", "Paste column list"])
        
        if col_input == "Enter manually":
            columns_text = st.text_area(
                "Enter column names (one per line):",
                height=150,
                placeholder="id\nfirst_name\nlast_name\nemail\ncreated_at\nstatus"
            )
            columns = [col.strip() for col in columns_text.split('\n') if col.strip()]
        else:
            columns_raw = st.text_input(
                "Paste columns (comma-separated):",
                placeholder="id, first_name, last_name, email, created_at, status"
            )
            columns = [col.strip() for col in columns_raw.split(',') if col.strip()]
        
        col1, col2 = st.columns(2)
        with col1:
            row_count = st.slider("Number of rows:", 5, 100, 20)
        with col2:
            dialect = st.selectbox("SQL Dialect:", SQL_DIALECTS)
        
        # Preview columns
        if columns:
            st.info(f"Will generate data for {len(columns)} columns: {', '.join(columns[:5])}{'...' if len(columns) > 5 else ''}")
        
        if st.button("🎲 Generate Sample Data", type="primary", disabled=not (table_name and columns)):
            try:
                sample_sql = generate_sample_data_query(table_name, columns, row_count, dialect.lower())
                
                st.success(f"✅ Generated {row_count} rows of sample data!")
                st.code(sample_sql, language="sql")
                
                # Additional options
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.download_button(
                        label="📥 Download SQL",
                        data=sample_sql,
                        file_name=f"{table_name}_sample_data.sql",
                        mime="text/plain"
                    )
                
                with col2:
                    if st.button("🔄 Regenerate"):
                        st.rerun()
                
                with col3:
                    if st.button("📋 Copy to Clipboard"):
                        st.code(sample_sql, language="sql")
                        st.info("Sample data displayed for copying")
                
                # Tips for customization
                with st.expander("💡 Customization Tips"):
                    st.markdown("""
                    The generator uses smart heuristics based on column names:
                    - **id** → Sequential numbers
                    - **name** columns → Realistic names
                    - **email** → Valid email addresses
                    - **date/created** → Date values
                    - **status** → Rotating status values
                    - **price/amount** → Decimal values
                    
                    For more specific data, edit the generated SQL or use the Generate SQL tool 
                    with prompts like "Generate 50 rows of realistic customer data for testing".
                    """)
                
            except Exception as e:
                self.handle_error(e, "generating sample data")
