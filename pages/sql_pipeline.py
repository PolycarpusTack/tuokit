# pages/sql_pipeline.py
import streamlit as st
import streamlit.components.v1 as components
import re
import json
import ollama
from datetime import datetime
from utils import DatabaseManager, knowledge_graph as kg
from utils.sql_concepts import (
    SQL_CONCEPTS, get_concepts_in_query, get_concept_info,
    get_quiz_for_concept, get_difficulty_color, get_learning_path
)

# Initialize database
db = DatabaseManager()

# Concept card template
CONCEPT_CARD = """
<div style="
    border: 1px solid #e0e0e0;
    border-radius: 10px;
    padding: 15px;
    margin: 10px 0;
    background: white;
    box-shadow: 0 2px 6px rgba(0,0,0,0.1);
">
    <h3 style="margin-top:0; color: #2c3e50;">{name}</h3>
    <p>{description}</p>
    <div style="margin-top:10px;">
        <strong>Resources:</strong>
        <ul style="padding-left:20px; margin-top:5px;">
            {resources}
        </ul>
    </div>
    {learning_path}
</div>
"""
def display_concept_card(concept):
    """Render concept card with educational resources"""
    resources_html = ""
    for resource in concept.get("resources", []):
        resources_html += f'<li><a href="{resource["url"]}" target="_blank">{resource["title"]}</a></li>'
    
    learning_path = ""
    if concept.get("prerequisites"):
        learning_path = f"""
        <div style="margin-top:10px;">
            <strong>Prerequisites:</strong>
            <div style="display:flex; gap:5px; margin-top:5px; flex-wrap:wrap;">
                {"".join([f'<div style="padding:5px 10px; background:#e3f2fd; border-radius:5px; font-size:0.9em;">{prereq}</div>' 
                          for prereq in concept["prerequisites"]])}
            </div>
        </div>
        """
    
    return CONCEPT_CARD.format(
        name=concept["name"],
        description=concept["description"],
        resources=resources_html,
        learning_path=learning_path
    )

def show_learning_path(start_id, target_id):
    """Display personalized learning path"""
    path = kg.recommend_learning_path(start_id, target_id)
    if not path:
        st.warning("No learning path found between these concepts")
        return
    
    st.subheader(f"📚 Learning Path: {path[0]['name']} → {path[-1]['name']}")
    
    # Visual path representation
    cols = st.columns(len(path))
    for i, concept in enumerate(path):
        with cols[i]:
            color_bg = '#e1f5fe' if i == 0 else '#f1f8e9' if i == len(path)-1 else '#f3e5f5'
            st.markdown(f"""
            <div style="text-align:center; padding:10px; 
                         background:{color_bg}; 
                         border-radius:5px; height:100px; 
                         display:flex; flex-direction:column; 
                         justify-content:center; align-items:center;">
                <strong>{concept['name']}</strong>
                {f'<div style="font-size:0.8em; margin-top:5px;">Step {i+1} of {len(path)}</div>' if i > 0 and i < len(path)-1 else ''}
                {f'<div style="font-size:0.8em; margin-top:5px;">🎯 Start Here</div>' if i == 0 else ''}
                {f'<div style="font-size:0.8em; margin-top:5px;">🏁 Goal</div>' if i == len(path)-1 else ''}
            </div>
            """, unsafe_allow_html=True)
    
    st.divider()
    
    # Detailed concept cards for the path
    for i, concept in enumerate(path):
        st.markdown(f"### Step {i+1}: {concept['name']}")
        st.markdown(display_concept_card(concept), unsafe_allow_html=True)

def generate_sql_from_nl(description, dialect):
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
            model="deepseek-coder:6.7b",
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

def optimize_sql_query(query, dialect):
    """Optimize SQL query with explanations"""
    prompt = f"""
    Optimize this {dialect} SQL query:
    
    ```sql
    {query}
    ```
    
    Provide your response as JSON with these exact fields:
    {{
        "optimized_sql": "the optimized SQL query",
        "summary": "brief optimization summary",
        "improvements": ["improvement 1", "improvement 2", ...]
    }}
    
    Focus on performance, readability, and best practices.
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
            return json.loads(json_match.group())
        else:
            return {
                "optimized_sql": query,
                "summary": "Could not optimize query",
                "improvements": []
            }
    except Exception as e:
        return {
            "optimized_sql": query,
            "summary": f"Optimization error: {str(e)}",
            "improvements": []
        }

def explain_sql_query(query, dialect):
    """Explain SQL query in plain English"""
    prompt = f"""
    Explain this {dialect} SQL query in simple, plain English:
    
    ```sql
    {query}
    ```
    
    Structure your explanation:
    1. **Purpose**: What does this query do?
    2. **How it works**: Step-by-step breakdown
    3. **Expected results**: What data will be returned
    4. **Key concepts**: SQL features used
    
    Use simple language suitable for beginners.
    """
    
    try:
        response = ollama.generate(
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={"temperature": 0.3}
        )
        return response['response']
    except Exception as e:
        return f"Error explaining query: {str(e)}"

def validate_sql_safety(query):
    """Ensure SQL is safe for analysis"""
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
    
    return True, "Query is safe"
def test_query_with_sample_data(query, dialect, sample_data):
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
            model="deepseek-coder:6.7b",
            prompt=prompt,
            options={"temperature": 0.1}
        )
        return response['response']
    except Exception as e:
        return f"Error testing query: {str(e)}"

# Example data templates
EXAMPLE_DATA_TEMPLATES = {
    "customers_orders": """{
  "customers": [
    {"id": 1, "name": "Alice Johnson", "email": "alice@email.com", "join_date": "2023-01-15"},
    {"id": 2, "name": "Bob Smith", "email": "bob@email.com", "join_date": "2023-02-20"},
    {"id": 3, "name": "Carol White", "email": "carol@email.com", "join_date": "2023-03-10"}
  ],
  "orders": [
    {"id": 101, "customer_id": 1, "amount": 150.99, "order_date": "2024-01-10", "status": "completed"},
    {"id": 102, "customer_id": 1, "amount": 89.50, "order_date": "2024-02-15", "status": "completed"},
    {"id": 103, "customer_id": 2, "amount": 299.99, "order_date": "2024-02-20", "status": "pending"},
    {"id": 104, "customer_id": 3, "amount": 45.00, "order_date": "2024-03-01", "status": "completed"}
  ]
}""",
    
    "employees": """{
  "employees": [
    {"id": 1, "name": "John Doe", "department": "Sales", "salary": 60000, "hire_date": "2022-01-15"},
    {"id": 2, "name": "Jane Smith", "department": "IT", "salary": 80000, "hire_date": "2021-06-20"},
    {"id": 3, "name": "Mike Johnson", "department": "Sales", "salary": 65000, "hire_date": "2023-03-10"}
  ],
  "departments": [
    {"id": 1, "name": "Sales", "budget": 500000},
    {"id": 2, "name": "IT", "budget": 1000000},
    {"id": 3, "name": "HR", "budget": 300000}
  ]
}""",
    
    "products": """{
  "products": [
    {"id": 1, "name": "Laptop", "category": "Electronics", "price": 999.99, "stock": 50},
    {"id": 2, "name": "Mouse", "category": "Electronics", "price": 29.99, "stock": 200},
    {"id": 3, "name": "Desk Chair", "category": "Furniture", "price": 299.99, "stock": 30}
  ],
  "categories": [
    {"id": 1, "name": "Electronics", "discount": 0.10},
    {"id": 2, "name": "Furniture", "discount": 0.15}
  ]
}"""
}

# Example queries for quick start
EXAMPLE_QUERIES = {
    "Top Customers": "Show me the top 5 customers by total spending with their email addresses",
    "Sales Analysis": "Calculate monthly sales totals for the current year with running total",
    "Inactive Users": "Find customers who haven't made any orders in the last 6 months",
    "Product Inventory": "List products that are low on stock (less than 20 items)",
    "Department Stats": "Show average salary by department with employee count",
    "Recent Orders": "Get all orders from the last 30 days sorted by amount descending"
}
def show():
    st.title("🔁 SQL Pipeline: Generate → Optimize → Learn")
    st.caption("Transform natural language into optimized SQL with integrated learning")
    
    # Initialize session state
    if "pipeline_state" not in st.session_state:
        st.session_state.pipeline_state = {
            "nl_description": "",
            "generated_sql": "",
            "optimized_sql": "",
            "optimization_summary": "",
            "optimization_improvements": [],
            "current_step": 1
        }
    
    if "active_concept" not in st.session_state:
        st.session_state.active_concept = None
    
    if "learning_path" not in st.session_state:
        st.session_state.learning_path = None
    
    # Knowledge Graph Visualization in Sidebar
    with st.sidebar:
        with st.expander("🧠 Knowledge Graph", expanded=False):
            st.caption("Explore SQL concepts and their relationships")
            
            # Get concepts detected in current query
            current_concepts = []
            if st.session_state.pipeline_state.get("optimized_sql"):
                current_concepts = kg.detect_concepts_in_query(
                    st.session_state.pipeline_state["optimized_sql"]
                )
            
            # Visualize graph with highlights
            graph_img = kg.visualize_graph(highlight_concepts=current_concepts)
            st.image(graph_img, use_column_width=True)
            
            # Concept selector
            all_concepts = list(kg.graph.nodes.values())
            concept_names = [c["name"] for c in all_concepts]
            
            selected_concept_name = st.selectbox(
                "Explore Concept", 
                options=concept_names,
                index=0
            )
            
            # Find concept by name
            selected_concept = None
            for concept in all_concepts:
                if concept["name"] == selected_concept_name:
                    selected_concept = concept
                    break
            
            if selected_concept:
                st.markdown(f"**{selected_concept['name']}**")
                st.info(selected_concept["description"])
                
                # Show prerequisites
                if selected_concept.get("prerequisites"):
                    st.write("**Prerequisites:**")
                    for prereq in selected_concept["prerequisites"]:
                        prereq_concept = kg.get_concept(prereq)
                        if prereq_concept:
                            st.write(f"• {prereq_concept.get('name', prereq)}")
                
                if st.button("🎓 Learn This Concept", use_container_width=True):
                    st.session_state.active_concept = selected_concept["id"]
                    st.rerun()
        
        st.divider()
        
        # Quick examples
        st.subheader("📚 Quick Examples")
        for name, description in EXAMPLE_QUERIES.items():
            if st.button(name, key=f"example_{name}", use_container_width=True):
                st.session_state.pipeline_state["nl_description"] = description
                st.session_state.pipeline_state["current_step"] = 1
                st.rerun()
    
    # Concept learning view
    if st.session_state.active_concept:
        concept = kg.get_concept(st.session_state.active_concept)
        
        if concept:
            st.subheader(f"🧠 Learning: {concept['name']}")
            
            # Concept card
            st.markdown(display_concept_card(concept), unsafe_allow_html=True)
            
            # Related concepts
            st.subheader("📚 Related Concepts")
            related = kg.get_related_concepts(st.session_state.active_concept)
            
            if related:
                cols = st.columns(min(3, len(related)))
                for i, rel_concept in enumerate(related):
                    with cols[i % len(cols)]:
                        st.markdown(f"""
                        <div style="text-align:center; padding:15px; 
                                   background:#e3f2fd; border-radius:8px; 
                                   cursor:pointer; min-height:100px;
                                   display:flex; flex-direction:column;
                                   justify-content:center;">
                            <strong>{rel_concept['name']}</strong>
                            <div style="font-size:0.8em; margin-top:5px;">
                                {rel_concept['description'][:50]}...
                            </div>
                        </div>
                        """, unsafe_allow_html=True)
                        
                        if st.button(f"Learn {rel_concept['name']}", 
                                   key=f"learn_{rel_concept['id']}",
                                   use_container_width=True):
                            st.session_state.active_concept = rel_concept['id']
                            st.rerun()
            
            # Prerequisites tree
            prerequisites = kg.get_prerequisite_tree(st.session_state.active_concept)
            if prerequisites:
                st.subheader("📋 Prerequisites")
                st.info("Master these concepts first for better understanding:")
                
                for prereq in prerequisites:
                    with st.expander(prereq['name']):
                        st.write(prereq['description'])
                        if st.button(f"Go to {prereq['name']}", 
                                   key=f"goto_{prereq['id']}"):
                            st.session_state.active_concept = prereq['id']
                            st.rerun()
            
            # Learning path builder
            st.subheader("🗺️ Build Learning Path")
            col1, col2 = st.columns(2)
            
            with col1:
                start_concepts = [c for c in all_concepts if c["id"] != st.session_state.active_concept]
                start_concept_name = st.selectbox(
                    "Start From",
                    options=[c["name"] for c in start_concepts],
                    index=0
                )
            
            with col2:
                target_concepts = [c for c in all_concepts if c["name"] != start_concept_name]
                target_concept_name = st.selectbox(
                    "Target Concept",
                    options=[c["name"] for c in target_concepts],
                    index=0
                )
            
            if st.button("🚀 Generate Learning Path", type="primary"):
                # Find IDs
                start_id = next((c["id"] for c in all_concepts if c["name"] == start_concept_name), None)
                target_id = next((c["id"] for c in all_concepts if c["name"] == target_concept_name), None)
                
                if start_id and target_id:
                    st.session_state.learning_path = (start_id, target_id)
            
            if st.session_state.learning_path:
                show_learning_path(*st.session_state.learning_path)
            
            # Navigation
            st.divider()
            if st.button("← Back to SQL Pipeline", type="primary", use_container_width=True):
                st.session_state.active_concept = None
                st.rerun()
        
        return  # Exit early when in concept view
    
    # Main Pipeline View
    # Quick navigation
    col1, col2, col3, col4 = st.columns([2, 1, 1, 1])
    with col2:
        if st.button("🛢️ SQL Generator", use_container_width=True):
            st.switch_page("pages/sql_generator.py")
    with col3:
        if st.button("🔍 SQL Optimizer", use_container_width=True):
            st.switch_page("pages/sql_optimizer.py")
    
    # Visual Pipeline Progress
    st.subheader("Pipeline Progress")
    progress_cols = st.columns(4)
    
    steps = [
        ("1️⃣ Describe", "Natural Language", 1),
        ("2️⃣ Generate", "SQL Query", 2),
        ("3️⃣ Optimize", "Performance", 3),
        ("4️⃣ Understand", "Learn & Test", 4)
    ]
    
    for col, (title, subtitle, step_num) in zip(progress_cols, steps):
        with col:
            if st.session_state.pipeline_state["current_step"] >= step_num:
                st.success(f"**{title}**")
            else:
                st.info(f"**{title}**")
            st.caption(subtitle)
    
    # Progress bar
    progress = (st.session_state.pipeline_state["current_step"] - 1) / 4
    st.progress(progress, text=f"Step {st.session_state.pipeline_state['current_step']} of 4")
    
    # Step 1: Natural Language Input
    with st.expander("📝 STEP 1: Describe Your Data Need", 
                     expanded=st.session_state.pipeline_state["current_step"] == 1):
        st.write("Tell me what data you need in plain English:")
        
        nl_description = st.text_area(
            "Your description:",
            height=100,
            placeholder="Example: Show me the top 5 customers by total spending in the last quarter",
            value=st.session_state.pipeline_state["nl_description"],
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
                        generated_sql = generate_sql_from_nl(nl_description, dialect)
                        st.session_state.pipeline_state.update({
                            "nl_description": nl_description,
                            "generated_sql": generated_sql,
                            "current_step": 2
                        })
                        st.rerun()
    
    # Step 2: Generated SQL with Concepts
    if st.session_state.pipeline_state["current_step"] >= 2 and st.session_state.pipeline_state["generated_sql"]:
        with st.expander("⚡ STEP 2: Generated SQL", 
                        expanded=st.session_state.pipeline_state["current_step"] == 2):
            st.write("Here's your generated SQL query:")
            
            # Editable SQL
            edited_sql = st.text_area(
                "Generated SQL (you can edit):",
                value=st.session_state.pipeline_state["generated_sql"],
                height=200,
                key="sql_editor"
            )
            
            # Detect and display concepts
            detected_concepts = kg.detect_concepts_in_query(edited_sql)
            
            if detected_concepts:
                st.subheader("💡 Concepts in This Query")
                
                # Create concept cards
                concept_cols = st.columns(min(3, len(detected_concepts)))
                for i, concept_id in enumerate(detected_concepts[:6]):  # Show max 6
                    concept = kg.get_concept(concept_id)
                    if concept:
                        with concept_cols[i % 3]:
                            # Difficulty-based color
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
                                cursor: pointer;
                                min-height: 120px;
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
                            
                            if st.button(f"Learn More", key=f"learn_more_{concept_id}"):
                                st.session_state.active_concept = concept_id
                                st.rerun()
                
                # Quick learning tips
                with st.expander("💡 Quick Tips for These Concepts"):
                    for concept_id in detected_concepts[:3]:
                        concept = kg.get_concept(concept_id)
                        if concept and concept.get('resources'):
                            st.write(f"**{concept.get('name', concept_id)}:**")
                            st.write(f"• {concept.get('description', '')}")
                            st.write(f"• Learn more: [{concept['resources'][0]['title']}]({concept['resources'][0]['url']})")
                            st.write("")
            
            # Action buttons
            col1, col2, col3 = st.columns(3)
            with col1:
                if st.button("🔄 Regenerate", key="regen_btn", use_container_width=True):
                    with st.spinner("Generating alternative..."):
                        new_sql = generate_sql_from_nl(
                            st.session_state.pipeline_state["nl_description"], 
                            dialect
                        )
                        st.session_state.pipeline_state["generated_sql"] = new_sql
                        st.rerun()
            
            with col2:
                if st.button("⬅️ Back", key="back_to_1", use_container_width=True):
                    st.session_state.pipeline_state["current_step"] = 1
                    st.rerun()
            
            with col3:
                if st.button("▶️ Optimize", type="primary", key="optimize_btn", use_container_width=True):
                    # Safety check
                    is_safe, safety_msg = validate_sql_safety(edited_sql)
                    if not is_safe:
                        st.error(f"🛡️ Security Block: {safety_msg}")
                    else:
                        with st.spinner("🚀 Optimizing query..."):
                            optimization = optimize_sql_query(edited_sql, dialect)
                            st.session_state.pipeline_state.update({
                                "generated_sql": edited_sql,
                                "optimized_sql": optimization["optimized_sql"],
                                "optimization_summary": optimization["summary"],
                                "optimization_improvements": optimization["improvements"],
                                "current_step": 3
                            })
                            st.rerun()
    
    # Step 3: Optimization Results
    if st.session_state.pipeline_state["current_step"] >= 3:
        with st.expander("🚀 STEP 3: Optimization Results", 
                        expanded=st.session_state.pipeline_state["current_step"] == 3):
            st.write("**Optimization Summary:**")
            st.info(st.session_state.pipeline_state["optimization_summary"])
            
            # Show improvements
            if st.session_state.pipeline_state["optimization_improvements"]:
                st.write("**Key Improvements:**")
                for improvement in st.session_state.pipeline_state["optimization_improvements"]:
                    st.success(f"✨ {improvement}")
            
            # Before/After comparison
            st.write("**Query Comparison:**")
            col1, col2 = st.columns(2)
            
            with col1:
                st.write("Original Query:")
                st.code(st.session_state.pipeline_state["generated_sql"], language="sql")
            
            with col2:
                st.write("Optimized Query:")
                st.code(st.session_state.pipeline_state["optimized_sql"], language="sql")
            
            # Optimization concepts
            optimization_concepts = kg.detect_concepts_in_query(
                st.session_state.pipeline_state["optimized_sql"]
            )
            new_concepts = set(optimization_concepts) - set(
                kg.detect_concepts_in_query(st.session_state.pipeline_state["generated_sql"])
            )
            
            if new_concepts:
                st.subheader("🎓 New Concepts Introduced")
                st.info("The optimization added these advanced concepts:")
                
                new_concept_cols = st.columns(len(new_concepts))
                for i, concept_id in enumerate(new_concepts):
                    concept = kg.get_concept(concept_id)
                    if concept:
                        with new_concept_cols[i]:
                            st.markdown(f"""
                            <div style="text-align:center; padding:10px; 
                                       background:#e8f5e9; border-radius:5px;">
                                <strong>{concept['name']}</strong>
                            </div>
                            """, unsafe_allow_html=True)
            
            # Navigation
            col1, col2, col3 = st.columns(3)
            with col1:
                if st.button("⬅️ Back", key="back_to_2", use_container_width=True):
                    st.session_state.pipeline_state["current_step"] = 2
                    st.rerun()
            
            with col3:
                if st.button("▶️ Understand", type="primary", key="understand_btn", use_container_width=True):
                    st.session_state.pipeline_state["current_step"] = 4
                    st.rerun()
    
    # Step 4: Understanding and Testing
    if st.session_state.pipeline_state["current_step"] >= 4:
        with st.expander("🧠 STEP 4: Understand Your Query", expanded=True):
            tab1, tab2, tab3, tab4 = st.tabs([
                "📖 Explanation", 
                "🧪 Test Query", 
                "💾 Save", 
                "🎓 Concepts & Learning"
            ])
            
            with tab1:
                st.subheader("Plain English Explanation")
                with st.spinner("Generating explanation..."):
                    explanation = explain_sql_query(
                        st.session_state.pipeline_state["optimized_sql"], 
                        dialect
                    )
                st.markdown(explanation)
                
                # Concept-specific resources
                concepts = kg.detect_concepts_in_query(
                    st.session_state.pipeline_state["optimized_sql"]
                )
                
                if concepts:
                    st.subheader("📚 Deep Dive Resources")
                    
                    for concept_id in concepts[:5]:  # Top 5 concepts
                        concept = kg.get_concept(concept_id)
                        if concept:
                            with st.expander(f"{concept['name']} Resources"):
                                st.write(concept['description'])
                                
                                if concept.get('resources'):
                                    st.write("**📖 Learning Materials:**")
                                    for resource in concept['resources']:
                                        st.markdown(f"• [{resource['title']}]({resource['url']})")
                                
                                # Related concepts
                                related = kg.get_related_concepts(concept_id)
                                if related:
                                    st.write("\n**🔗 Related Concepts:**")
                                    related_names = [r['name'] for r in related[:3]]
                                    st.write(", ".join(related_names))
            
            with tab2:
                st.subheader("Test Your Query")
                st.write("Test your optimized query with sample data:")
                
                # Data template selection
                template_choice = st.selectbox(
                    "Choose a data template:",
                    options=["Custom"] + list(EXAMPLE_DATA_TEMPLATES.keys()),
                    format_func=lambda x: x.replace("_", " ").title()
                )
                
                if template_choice == "Custom":
                    sample_data = st.text_area(
                        "Sample Data (JSON format):",
                        height=200,
                        value='{\n  "table_name": [\n    {"column1": "value1", "column2": "value2"}\n  ]\n}',
                        key="sample_data_input"
                    )
                else:
                    sample_data = st.text_area(
                        "Sample Data (JSON format):",
                        height=200,
                        value=EXAMPLE_DATA_TEMPLATES[template_choice],
                        key="sample_data_template"
                    )
                
                if st.button("▶️ Run Test Query", type="primary", key="test_query_btn"):
                    with st.spinner("Executing query on sample data..."):
                        results = test_query_with_sample_data(
                            st.session_state.pipeline_state["optimized_sql"],
                            dialect,
                            sample_data
                        )
                    
                    st.subheader("Query Results")
                    st.markdown(results)
            
            with tab3:
                st.subheader("Save to Knowledge Base")
                
                # Prepare content to save
                save_title = st.text_input(
                    "Title:",
                    value=f"SQL: {st.session_state.pipeline_state['nl_description'][:50]}...",
                    key="save_title"
                )
                
                save_category = st.selectbox(
                    "Category:",
                    options=["SQL Pattern", "Optimized Query", "Business Query", "Report Query"],
                    key="save_category"
                )
                
                save_notes = st.text_area(
                    "Notes (optional):",
                    placeholder="Any additional context or notes about this query...",
                    key="save_notes"
                )
                
                # Include concepts in save
                concepts_to_save = kg.detect_concepts_in_query(
                    st.session_state.pipeline_state["optimized_sql"]
                )
                
                if st.button("💾 Save Complete Pipeline", type="primary", key="save_pipeline_btn"):
                    try:
                        # Prepare complete content
                        content = f"""-- Natural Language Description:
-- {st.session_state.pipeline_state['nl_description']}

-- Original Generated SQL:
{st.session_state.pipeline_state['generated_sql']}

-- Optimized SQL:
{st.session_state.pipeline_state['optimized_sql']}

-- Optimization Summary:
-- {st.session_state.pipeline_state['optimization_summary']}

-- Improvements:
{chr(10).join(f'-- * {imp}' for imp in st.session_state.pipeline_state['optimization_improvements'])}

-- SQL Concepts Used:
{chr(10).join(f'-- * {kg.get_concept(c).get("name", c)}' for c in concepts_to_save)}

-- Notes:
-- {save_notes if save_notes else 'No additional notes'}

-- Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M')}
-- Dialect: {dialect}
"""
                        
                        # Log to database
                        query_id = db.log_query(
                            tool="sql_pipeline",
                            model="deepseek-coder:6.7b",
                            prompt=st.session_state.pipeline_state['nl_description'],
                            response=content
                        )
                        
                        # Save to knowledge base
                        saved = db.save_knowledge_unit(
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
            
            with tab4:
                st.subheader("🎓 Concepts & Learning")
                
                # Detected concepts in the query
                concepts = kg.detect_concepts_in_query(
                    st.session_state.pipeline_state["optimized_sql"]
                )
                
                if concepts:
                    # Concept mastery overview
                    st.write("**📊 Concepts in Your Query:**")
                    
                    # Group by difficulty
                    beginner_concepts = []
                    intermediate_concepts = []
                    advanced_concepts = []
                    
                    for concept_id in concepts:
                        concept = kg.get_concept(concept_id)
                        if concept:
                            difficulty = concept.get('difficulty', 'Beginner')
                            if difficulty == 'Beginner':
                                beginner_concepts.append(concept)
                            elif difficulty == 'Intermediate':
                                intermediate_concepts.append(concept)
                            else:
                                advanced_concepts.append(concept)
                    
                    # Display by difficulty
                    col1, col2, col3 = st.columns(3)
                    
                    with col1:
                        st.markdown("**🟢 Beginner**")
                        for concept in beginner_concepts:
                            st.write(f"• {concept['name']}")
                    
                    with col2:
                        st.markdown("**🟡 Intermediate**")
                        for concept in intermediate_concepts:
                            st.write(f"• {concept['name']}")
                    
                    with col3:
                        st.markdown("**🔴 Advanced**")
                        for concept in advanced_concepts:
                            st.write(f"• {concept['name']}")
                    
                    # Learning path recommendation
                    st.divider()
                    st.subheader("📈 Recommended Learning Path")
                    
                    # Find the most advanced concept
                    most_advanced = None
                    if advanced_concepts:
                        most_advanced = advanced_concepts[0]
                    elif intermediate_concepts:
                        most_advanced = intermediate_concepts[0]
                    elif beginner_concepts:
                        most_advanced = beginner_concepts[-1]
                    
                    if most_advanced:
                        # Find a learning path from SQL basics to this concept
                        path = kg.recommend_learning_path("sql", most_advanced["id"])
                        
                        if path and len(path) > 1:
                            st.info(f"To master **{most_advanced['name']}**, follow this path:")
                            
                            path_cols = st.columns(min(5, len(path)))
                            for i, step in enumerate(path):
                                with path_cols[i % len(path_cols)]:
                                    is_current = step["id"] in [c["id"] for c in concepts]
                                    color = "#4CAF50" if is_current else "#e0e0e0"
                                    
                                    st.markdown(f"""
                                    <div style="text-align:center; padding:8px; 
                                               background:{color}; border-radius:5px;
                                               color:{'white' if is_current else 'black'};">
                                        <small>Step {i+1}</small><br>
                                        <strong>{step['name']}</strong>
                                    </div>
                                    """, unsafe_allow_html=True)
                    
                    # Interactive Quiz Section
                    st.divider()
                    st.subheader("🧩 Test Your Knowledge")
                    
                    # Select a concept to quiz on
                    if concepts:
                        quiz_concept_name = st.selectbox(
                            "Choose a concept to test:",
                            options=[kg.get_concept(c)['name'] for c in concepts if kg.get_concept(c)],
                            key="quiz_concept_select"
                        )
                        
                        # Find the concept
                        quiz_concept_id = None
                        for c_id in concepts:
                            c = kg.get_concept(c_id)
                            if c and c['name'] == quiz_concept_name:
                                quiz_concept_id = c_id
                                break
                        
                        if quiz_concept_id:
                            # Get quiz questions from sql_concepts
                            quiz_questions = get_quiz_for_concept(quiz_concept_id)
                            
                            if quiz_questions:
                                for i, question in enumerate(quiz_questions[:2]):  # Show 2 questions
                                    st.write(f"\n**Question {i+1}:** {question['question']}")
                                    
                                    answer = st.radio(
                                        "",
                                        options=question['options'],
                                        key=f"quiz_{quiz_concept_id}_{i}",
                                        label_visibility="collapsed"
                                    )
                                    
                                    if st.button(f"Check Answer", key=f"check_{quiz_concept_id}_{i}"):
                                        if question['options'].index(answer) == question['correct']:
                                            st.success(f"✅ Correct! {question['explanation']}")
                                        else:
                                            correct_answer = question['options'][question['correct']]
                                            st.error(f"❌ Incorrect. The correct answer is: {correct_answer}")
                                            st.info(f"💡 {question['explanation']}")
                            else:
                                # Generate a simple quiz based on the concept
                                st.info("Quick concept check:")
                                concept = kg.get_concept(quiz_concept_id)
                                if st.button(f"I understand what {concept['name']} means"):
                                    st.success("Great! Keep learning!")
                    
                    # Concept Deep Dive
                    st.divider()
                    st.subheader("📖 Explore Concepts in Detail")
                    
                    selected_concept_name = st.selectbox(
                        "Select a concept to explore:",
                        options=[kg.get_concept(c)['name'] for c in concepts if kg.get_concept(c)],
                        key="deep_dive_select"
                    )
                    
                    # Find selected concept
                    selected_concept_id = None
                    for c_id in concepts:
                        c = kg.get_concept(c_id)
                        if c and c['name'] == selected_concept_name:
                            selected_concept_id = c_id
                            break
                    
                    if selected_concept_id:
                        concept = kg.get_concept(selected_concept_id)
                        if concept:
                            # Display detailed concept card
                            st.markdown(display_concept_card(concept), unsafe_allow_html=True)
                            
                            # Show prerequisites
                            if concept.get('prerequisites'):
                                st.write("**📋 Prerequisites to master first:**")
                                prereq_cols = st.columns(len(concept['prerequisites']))
                                for i, prereq_id in enumerate(concept['prerequisites']):
                                    prereq = kg.get_concept(prereq_id)
                                    if prereq:
                                        with prereq_cols[i]:
                                            st.info(prereq['name'])
                            
                            # Navigate to full concept view
                            if st.button(f"🎓 Full Learning Path for {concept['name']}", 
                                       key=f"full_path_{selected_concept_id}"):
                                st.session_state.active_concept = selected_concept_id
                                st.rerun()
                
                else:
                    st.info("Generate and optimize a query first to see learning content!")
            
            # Navigation buttons
            st.divider()
            col1, col2, col3 = st.columns(3)
            with col1:
                if st.button("⬅️ Back", key="back_to_3", use_container_width=True):
                    st.session_state.pipeline_state["current_step"] = 3
                    st.rerun()
            
            with col2:
                if st.button("🏠 Start New", key="new_pipeline", use_container_width=True):
                    st.session_state.pipeline_state = {
                        "nl_description": "",
                        "generated_sql": "",
                        "optimized_sql": "",
                        "optimization_summary": "",
                        "optimization_improvements": [],
                        "current_step": 1
                    }
                    st.rerun()
            
            with col3:
                if st.button("📚 Knowledge Graph", key="view_kg", use_container_width=True):
                    st.session_state.active_concept = "sql"  # Start with SQL concept
                    st.rerun()
    
    # Footer with tips
    with st.expander("💡 Pro Tips", expanded=False):
        st.markdown("""
        ### Getting the Best Results:
        
        1. **Be Specific**: Instead of "show sales", try "show monthly sales totals for 2024 grouped by product category"
        
        2. **Include Context**: Mention important filters, time ranges, and sorting preferences
        
        3. **Learn Concepts**: Click on concept cards to understand the SQL features being used
        
        4. **Follow Learning Paths**: Use the knowledge graph to build your SQL expertise systematically
        
        5. **Test Incrementally**: Start with simple queries and add complexity gradually
        
        ### Learning Features:
        
        - **🧠 Knowledge Graph**: Visualize relationships between SQL concepts
        - **📚 Learning Paths**: Get personalized paths from beginner to advanced concepts  
        - **🧩 Interactive Quizzes**: Test your understanding of each concept
        - **📖 Resource Links**: Curated tutorials and documentation for each topic
        - **🎯 Concept Detection**: See which SQL concepts are used in your queries
        
        ### Common Patterns:
        - **Top N**: "Show top 10 customers by..."
        - **Time Series**: "Monthly/Daily/Yearly totals..."
        - **Comparisons**: "Compare this year vs last year..."
        - **Rankings**: "Rank products by sales within each category..."
        """)
    
    # JavaScript for concept navigation (hidden)
    components.html("""
    <script>
    // Handle concept card clicks
    window.addEventListener('message', function(event) {
        if (event.data && event.data.concept) {
            // Trigger concept view
            const buttons = window.parent.document.querySelectorAll('button');
            buttons.forEach(button => {
                if (button.textContent.includes('Learn This Concept')) {
                    button.click();
                }
            });
        }
    });
    
    // Add click handlers to concept cards
    document.addEventListener('DOMContentLoaded', function() {
        const conceptCards = document.querySelectorAll('[data-concept-id]');
        conceptCards.forEach(card => {
            card.addEventListener('click', function() {
                const conceptId = this.getAttribute('data-concept-id');
                window.parent.postMessage({concept: conceptId}, '*');
            });
        });
    });
    </script>
    """, height=0)

# Run the application
if __name__ == "__main__":
    show()