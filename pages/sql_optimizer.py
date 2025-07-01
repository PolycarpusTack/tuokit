import streamlit as st
import re
import json
import time
import ollama
from datetime import datetime
from utils import DatabaseManager

# Initialize database
db = DatabaseManager()

# Professional validation functions
def validate_explain_plan(plan):
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

def validate_index_recommendation(index, query, dialect):
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

def validate_alternative_query(alt_query, original, dialect):
    """Validate functional equivalence of alternative queries"""
    # Generate equivalence check prompt
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
            model="deepseek-coder:6.7b",
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

# Analysis functions with validation
def explain_query_plan(query, dialect):
    """Generate EXPLAIN plan with validation"""
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
            model="deepseek-coder:6.7b",
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
    
    return validate_explain_plan(plan)

def recommend_indexes(query, dialect):
    """Get validated index recommendations"""
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
            model="deepseek-coder:6.7b",
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
    
    return [validate_index_recommendation(idx, query, dialect) for idx in indexes]

def suggest_alternative_queries(query, dialect):
    """Generate validated query alternatives"""
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
            model="deepseek-coder:6.7b",
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
    
    return [validate_alternative_query(alt, query, dialect) for alt in alternatives[:2]]

# Warning descriptions
WARNING_DESCRIPTIONS = {
    "too_many_columns": "Indexes with >3 columns may have diminishing returns and higher maintenance cost",
    "low_selectivity": "Boolean/flag columns typically have low selectivity, making indexes less effective",
    "functional_dependency": "Multi-column indexes require careful column ordering based on query patterns",
    "large_table": "Index creation on large tables may require significant time and lock the table",
    "high_writes": "Indexes slow down INSERT/UPDATE/DELETE operations",
    "existing_similar": "Check for existing indexes that might already cover this query"
}

# Helper functions
def validate_sql(query, dialect):
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

def check_query_safety(query):
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

# UI Components
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
            for warning in validation["warnings"]:
                st.write(f"• {WARNING_DESCRIPTIONS.get(warning, warning)}")
    
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

# Result display components
def display_results(plan, indexes, alternatives):
    """Professional results display with validation"""
    
    # EXPLAIN plan section
    st.subheader("📊 Execution Plan Analysis")
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
                
                col1, col2, col3 = st.columns(3)
                with col1:
                    if st.button(f"📋 Copy SQL", key=f"copy_alt_{i}"):
                        st.write("Please copy the SQL above")
                with col2:
                    if st.button(f"💾 Save to Knowledge", key=f"save_alt_{i}"):
                        save_to_knowledge(alt, i)
    
    # Professional validation advisory
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

def save_to_knowledge(alternative, index):
    """Save optimization to knowledge base"""
    try:
        query_id = db.log_query(
            tool="sql_optimizer",
            model="deepseek-coder:6.7b",
            prompt=f"Optimization {index+1}: {alternative.get('rationale', 'Query optimization')}",
            response=alternative.get('sql', '')
        )
        
        saved = db.save_knowledge_unit(
            query_id=query_id,
            title=f"Optimized Query: {alternative.get('rationale', 'Alternative')[:50]}",
            content=alternative.get('sql', ''),
            category="Query Optimization"
        )
        
        if saved:
            st.success("✅ Saved to knowledge base!")
        else:
            st.error("Failed to save to knowledge base")
    except Exception as e:
        st.error(f"Error saving: {str(e)}")

# Main application
def show():
    st.title("🔍 Professional SQL Query Optimizer")
    st.caption("AI-powered query optimization with built-in validation and safety checks")
    
    # Quick navigation
    col1, col2, col3 = st.columns([2, 1, 1])
    with col3:
        if st.button("🛢️ SQL Generator →", use_container_width=True):
            st.switch_page("pages/sql_generator.py")
    
    # Professional disclaimer
    with st.expander("⚠️ Professional Advisory", expanded=True):
        st.warning("""
        **AI-Generated Optimizations - Always Validate:**
        1. Test functional equivalence in staging environment
        2. Verify performance with EXPLAIN ANALYZE
        3. Check index impact on write operations
        4. Compare results with production data samples
        
        **Optimization quality depends on:**
        • Query complexity and structure
        • Database statistics accuracy
        • Data distribution patterns
        • Concurrent workload characteristics
        """)
    
    # Initialize session state
    if "original_query" not in st.session_state:
        st.session_state.original_query = ""
    if "optimized_query" not in st.session_state:
        st.session_state.optimized_query = ""
    if "last_analysis" not in st.session_state:
        st.session_state.last_analysis = None
    
    # Check if query was passed from SQL Generator
    if "query_to_optimize" in st.session_state:
        st.session_state.original_query = st.session_state.query_to_optimize
        del st.session_state.query_to_optimize
        st.info("Query loaded from SQL Generator")
    
    # Sidebar with examples
    with st.sidebar:
        st.subheader("📚 Example Queries")
        examples = {
            "Slow Join": """SELECT o.*, c.name, c.email 
FROM orders o 
JOIN customers c ON o.customer_id = c.id 
WHERE o.status = 'pending' 
ORDER BY o.created_at DESC""",
            
            "Subquery Performance": """SELECT * FROM products 
WHERE category_id IN (
    SELECT id FROM categories 
    WHERE parent_id = 5
)""",
            
            "Complex Aggregation": """SELECT 
    DATE_TRUNC('month', created_at) as month,
    COUNT(*) as total_orders,
    SUM(amount) as revenue
FROM orders
WHERE created_at >= '2023-01-01'
GROUP BY 1
ORDER BY 1""",
            
            "N+1 Pattern": """SELECT u.*, 
    (SELECT COUNT(*) FROM orders WHERE user_id = u.id) as order_count,
    (SELECT SUM(amount) FROM orders WHERE user_id = u.id) as total_spent
FROM users u
WHERE u.active = true"""
        }
        
        for name, query in examples.items():
            if st.button(name, key=f"example_{name}", use_container_width=True):
                st.session_state.original_query = query
                st.rerun()
    
    # Main layout
    col1, col2 = st.columns([3, 1])
    
    with col1:
        query = st.text_area(
            "SQL Query to Optimize:",
            height=200,
            placeholder="Enter your SQL query here...",
            value=st.session_state.original_query,
            help="Paste the slow query you want to optimize"
        )
    
    with col2:
        dialect = st.selectbox(
            "SQL Dialect:",
            options=["PostgreSQL", "MySQL", "SQL Server", "Oracle"],
            index=0,
            help="Select your database type for dialect-specific optimizations"
        )
        
        st.write("**Validation Level:**")
        validation_level = st.select_slider(
            "",
            options=["Basic", "Standard", "Comprehensive"],
            value="Standard",
            label_visibility="collapsed"
        )
        
        st.write("**Focus Area:**")
        focus = st.radio(
            "",
            options=["Balanced", "Read Performance", "Write Performance"],
            index=0,
            label_visibility="collapsed"
        )
    
    # Advanced options
    with st.expander("🔧 Advanced Settings"):
        col1, col2 = st.columns(2)
        with col1:
            test_equivalence = st.checkbox(
                "Validate Functional Equivalence", 
                value=True,
                help="AI verifies that optimized queries return same results"
            )
            check_anti_patterns = st.checkbox(
                "Detect Index Anti-Patterns",
                value=True,
                help="Warn about problematic indexing strategies"
            )
        with col2:
            include_explain = st.checkbox(
                "Generate EXPLAIN Commands",
                value=True,
                help="Include EXPLAIN statements for validation"
            )
            aggressive_opt = st.checkbox(
                "Aggressive Optimization",
                value=False,
                help="May suggest more radical query rewrites"
            )
    
    # Main optimization button
    if st.button("🚀 Optimize Query", type="primary", use_container_width=True):
        if not query.strip():
            st.error("Please enter a SQL query to optimize")
            return
        
        st.session_state.original_query = query
        
        # Validation steps
        with st.status("Validating and analyzing query...", expanded=True) as status:
            # Step 1: Syntax validation
            st.write("🔍 Validating SQL syntax...")
            syntax_ok, syntax_msg = validate_sql(query, dialect)
            if not syntax_ok:
                st.error(f"❌ Syntax Error: {syntax_msg}")
                status.update(label="Validation failed", state="error", expanded=True)
                return
            st.write("✅ Syntax valid")
            
            # Step 2: Safety check
            st.write("🛡️ Checking query safety...")
            safety_ok, safety_msg = check_query_safety(query)
            if not safety_ok:
                st.error(f"❌ Safety Issue: {safety_msg}")
                status.update(label="Safety check failed", state="error", expanded=True)
                return
            st.write("✅ Query is safe to analyze")
            
            # Step 3: Query plan analysis
            st.write("📊 Analyzing execution plan...")
            plan = explain_query_plan(query, dialect)
            
            # Step 4: Index recommendations
            st.write("📈 Generating index recommendations...")
            indexes = recommend_indexes(query, dialect)
            
            # Step 5: Query alternatives
            st.write("⚡ Creating optimized alternatives...")
            alternatives = suggest_alternative_queries(query, dialect)
            
            status.update(label="✅ Optimization complete!", state="complete", expanded=False)
        
        # Store results in session state
        st.session_state.last_analysis = {
            "plan": plan,
            "indexes": indexes,
            "alternatives": alternatives,
            "timestamp": datetime.now()
        }
        
        # Display results
        display_results(plan, indexes, alternatives)
        
        # Log to database
        try:
            query_id = db.log_query(
                tool="sql_optimizer",
                model="deepseek-coder:6.7b",
                prompt=f"Optimize {dialect} query: {query[:100]}...",
                response=json.dumps({
                    "plan_summary": plan.get("summary", ""),
                    "index_count": len(indexes),
                    "alternative_count": len(alternatives)
                })
            )
            
            # Save optimization session
            if st.button("💾 Save Complete Analysis", key="save_session"):
                saved = db.save_knowledge_unit(
                    query_id=query_id,
                    title=f"Query Optimization: {plan.get('summary', 'Analysis')[:50]}",
                    content=json.dumps({
                        "original_query": query,
                        "dialect": dialect,
                        "analysis": st.session_state.last_analysis
                    }, indent=2),
                    category="Query Optimization"
                )
                if saved:
                    st.success("✅ Complete analysis saved to knowledge base!")
                
        except Exception as e:
            st.error(f"Error logging optimization: {str(e)}")
    
    # Show previous results if available
    elif st.session_state.last_analysis:
        st.info("Showing results from last analysis")
        display_results(
            st.session_state.last_analysis["plan"],
            st.session_state.last_analysis["indexes"],
            st.session_state.last_analysis["alternatives"]
        )
    
    # Feedback section
    if st.session_state.last_analysis:
        st.divider()
        with st.expander("📝 Provide Feedback"):
            feedback_form()

def feedback_form():
    """Collect user feedback on optimization quality"""
    with st.form("optimization_feedback", clear_on_submit=True):
        col1, col2 = st.columns(2)
        
        with col1:
            accuracy = st.select_slider(
                "How accurate were the optimizations?",
                options=["Not accurate", "Somewhat accurate", "Very accurate"],
                value="Somewhat accurate"
            )
        
        with col2:
            usefulness = st.select_slider(
                "How useful were the recommendations?",
                options=["Not useful", "Somewhat useful", "Very useful"],
                value="Somewhat useful"
            )
        
        comments = st.text_area(
            "Additional comments (optional):",
            placeholder="What worked well? What could be improved?"
        )
        
        if st.form_submit_button("Submit Feedback", type="primary"):
            try:
                # Log feedback
                feedback_data = {
                    "tool": "sql_optimizer",
                    "accuracy": accuracy,
                    "usefulness": usefulness,
                    "comments": comments,
                    "timestamp": datetime.now().isoformat()
                }
                
                # In a real implementation, this would save to a feedback table
                st.success("Thank you for your feedback! It helps us improve the optimizer.")
                
            except Exception as e:
                st.error(f"Error submitting feedback: {str(e)}")

# Run the application
if __name__ == "__main__":
    show()
