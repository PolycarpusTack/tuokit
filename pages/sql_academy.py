"""
🎓 TuoKit SQL Academy - Comprehensive SQL Learning Platform
Extracted from sql_pipeline.py's educational features
Integrates with EduMind for a unified learning experience
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Sql Academy - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
import streamlit.components.v1 as components
from utils import DatabaseManager, safe_ollama_generate, capture_knowledge
from utils.sql_concepts import (
    SQL_CONCEPTS, get_concepts_in_query, get_concept_info,
    get_quiz_for_concept, get_difficulty_color, get_learning_path
)
from utils import knowledge_graph as kg
import json
import time
from datetime import datetime

# Initialize database
db = DatabaseManager()

def show():
    st.title("🎓 SQL Academy")
    st.caption("Master SQL through interactive learning paths, quizzes, and real-world examples")
    
    # Quick navigation to other educational tools
    col1, col2, col3 = st.columns(3)
    with col1:
        if st.button("📚 Study Guides", use_container_width=True):
            st.switch_page("pages/study_guide_generator.py")
    with col2:
        if st.button("🧠 EduMind", use_container_width=True):
            st.switch_page("pages/edu_mind.py")
    with col3:
        if st.button("🛢️ SQL Practice", use_container_width=True):
            st.switch_page("pages/sql_suite.py")
    
    st.divider()
    
    # Main learning interface
    tab_learn, tab_analyze, tab_quiz, tab_path = st.tabs([
        "📖 Learn Concepts",
        "🔍 Analyze Query",
        "🧩 Practice Quiz",
        "🗺️ Learning Path"
    ])
    
    # TAB 1: Learn SQL Concepts
    with tab_learn:
        st.header("SQL Concept Library")
        
        # Concept difficulty filter
        col1, col2 = st.columns([2, 1])
        with col1:
            difficulty_filter = st.multiselect(
                "Filter by difficulty",
                ["Beginner", "Intermediate", "Advanced"],
                default=["Beginner", "Intermediate", "Advanced"]
            )        
        with col2:
            search_concept = st.text_input("Search concepts", placeholder="e.g., JOIN, INDEX")
        
        # Display concepts grid
        concepts_to_show = []
        for concept_id, concept in SQL_CONCEPTS.items():
            if concept['difficulty'] in difficulty_filter:
                if not search_concept or search_concept.lower() in concept['name'].lower():
                    concepts_to_show.append((concept_id, concept))
        
        # Create concept cards
        cols = st.columns(3)
        for idx, (concept_id, concept) in enumerate(concepts_to_show):
            with cols[idx % 3]:
                with st.container():
                    # Concept card with difficulty color
                    color = get_difficulty_color(concept['difficulty'])
                    st.markdown(f"""
                    <div style="
                        border: 2px solid {color};
                        border-radius: 10px;
                        padding: 15px;
                        margin-bottom: 15px;
                        background: linear-gradient(135deg, white 0%, {color}20 100%);
                    ">
                        <h4 style="margin:0; color: {color};">{concept['name']}</h4>
                        <p style="margin:10px 0; font-size:0.9em;">{concept['description'][:100]}...</p>
                        <div style="display:flex; justify-content:space-between; align-items:center;">
                            <span style="font-size:0.8em; color:#666;">
                                {concept['difficulty']} • {concept['category']}
                            </span>
                        </div>
                    </div>
                    """, unsafe_allow_html=True)
                    
                    if st.button(f"Learn →", key=f"learn_{concept_id}"):
                        st.session_state.selected_concept = concept_id
                        st.rerun()
        
        # Detailed concept view
        if 'selected_concept' in st.session_state:
            concept_id = st.session_state.selected_concept
            concept = SQL_CONCEPTS.get(concept_id)
            
            if concept:
                st.divider()
                st.subheader(f"📚 {concept['name']}")
                
                col1, col2 = st.columns([2, 1])
                with col1:
                    st.markdown(concept['description'])                    
                    # Examples
                    if concept.get('examples'):
                        st.subheader("Examples")
                        for example in concept['examples']:
                            st.code(example, language="sql")
                    
                    # Prerequisites
                    if concept.get('prerequisites'):
                        st.subheader("Prerequisites")
                        prereq_cols = st.columns(len(concept['prerequisites']))
                        for idx, prereq in enumerate(concept['prerequisites']):
                            with prereq_cols[idx]:
                                if st.button(f"📖 {prereq}", key=f"prereq_{prereq}"):
                                    # Find concept by name
                                    for cid, c in SQL_CONCEPTS.items():
                                        if c['name'] == prereq:
                                            st.session_state.selected_concept = cid
                                            st.rerun()
                
                with col2:
                    # Difficulty badge
                    color = get_difficulty_color(concept['difficulty'])
                    st.markdown(f"""
                    <div style="text-align:center; padding:20px; border-radius:10px; background:{color}20;">
                        <h3 style="color:{color}; margin:0;">{concept['difficulty']}</h3>
                        <p style="margin:5px 0;">Difficulty Level</p>
                    </div>
                    """, unsafe_allow_html=True)
                    
                    # Resources
                    if concept.get('resources'):
                        st.subheader("📚 Resources")
                        for resource in concept['resources']:
                            st.link_button(resource['title'], resource['url'])
                    
                    # Quick actions
                    st.divider()
                    if st.button("🧩 Take Quiz", key="quiz_from_concept"):
                        st.session_state.quiz_concept = concept_id
                        st.session_state.active_tab = "quiz"
                        st.rerun()
                    
                    if st.button("🗺️ Learning Path", key="path_from_concept"):
                        st.session_state.target_concept = concept_id
                        st.session_state.active_tab = "path"
                        st.rerun()    
    # TAB 2: Analyze SQL Query
    with tab_analyze:
        st.header("SQL Query Analyzer")
        st.caption("Understand which SQL concepts are used in any query")
        
        query_to_analyze = st.text_area(
            "Enter SQL query to analyze",
            height=150,
            placeholder="SELECT c.name, COUNT(o.id) as order_count\nFROM customers c\nLEFT JOIN orders o ON c.id = o.customer_id\nGROUP BY c.name\nHAVING COUNT(o.id) > 5"
        )
        
        if st.button("🔍 Analyze Concepts", type="primary", disabled=not query_to_analyze):
            # Detect concepts in query
            detected_concepts = get_concepts_in_query(query_to_analyze)
            
            if detected_concepts:
                st.success(f"Found {len(detected_concepts)} SQL concepts in your query!")
                
                # Display concept cards
                cols = st.columns(3)
                for idx, concept_id in enumerate(detected_concepts):
                    concept = SQL_CONCEPTS.get(concept_id)
                    if concept:
                        with cols[idx % 3]:
                            color = get_difficulty_color(concept['difficulty'])
                            st.markdown(f"""
                            <div style="
                                border: 2px solid {color};
                                border-radius: 8px;
                                padding: 10px;
                                margin-bottom: 10px;
                                background: {color}15;
                            ">
                                <h5 style="margin:0; color:{color};">{concept['name']}</h5>
                                <p style="margin:5px 0; font-size:0.85em;">
                                    {concept['description'][:80]}...
                                </p>
                                <span style="font-size:0.75em; color:#666;">
                                    {concept['difficulty']} Level
                                </span>
                            </div>
                            """, unsafe_allow_html=True)
                            
                            if st.button(f"Learn", key=f"analyze_learn_{concept_id}"):
                                st.session_state.selected_concept = concept_id
                                st.session_state.active_tab = "learn"
                                st.rerun()                
                # Suggest learning path
                st.divider()
                st.subheader("🎯 Suggested Learning Order")
                
                # Sort concepts by difficulty
                beginner = [c for c in detected_concepts if SQL_CONCEPTS[c]['difficulty'] == 'Beginner']
                intermediate = [c for c in detected_concepts if SQL_CONCEPTS[c]['difficulty'] == 'Intermediate']
                advanced = [c for c in detected_concepts if SQL_CONCEPTS[c]['difficulty'] == 'Advanced']
                
                learning_order = beginner + intermediate + advanced
                
                for idx, concept_id in enumerate(learning_order):
                    concept = SQL_CONCEPTS[concept_id]
                    st.write(f"{idx + 1}. **{concept['name']}** ({concept['difficulty']})")
            else:
                st.info("No specific SQL concepts detected. Try a more complex query!")
        
        # Example queries
        with st.expander("📋 Example Queries to Analyze"):
            examples = {
                "Complex JOIN": """SELECT 
    d.department_name,
    AVG(e.salary) as avg_salary,
    COUNT(DISTINCT e.employee_id) as employee_count
FROM departments d
LEFT JOIN employees e ON d.department_id = e.department_id
WHERE e.hire_date > '2020-01-01'
GROUP BY d.department_name
HAVING AVG(e.salary) > 50000
ORDER BY avg_salary DESC""",
                
                "Window Functions": """SELECT 
    product_name,
    sale_date,
    sale_amount,
    SUM(sale_amount) OVER (PARTITION BY product_name ORDER BY sale_date) as running_total,
    RANK() OVER (ORDER BY sale_amount DESC) as sales_rank
FROM sales
WHERE sale_date BETWEEN '2023-01-01' AND '2023-12-31'""",
                
                "CTE and Recursion": """WITH RECURSIVE org_hierarchy AS (
    SELECT employee_id, name, manager_id, 1 as level
    FROM employees
    WHERE manager_id IS NULL
    
    UNION ALL
    
    SELECT e.employee_id, e.name, e.manager_id, oh.level + 1
    FROM employees e
    JOIN org_hierarchy oh ON e.manager_id = oh.employee_id
)
SELECT * FROM org_hierarchy ORDER BY level, name"""
            }
            
            for name, query in examples.items():
                if st.button(f"Try: {name}", key=f"ex_analyze_{name}"):
                    st.session_state.analyze_example = query
                    st.rerun()
        
        # Load example if selected
        if 'analyze_example' in st.session_state:
            query_to_analyze = st.session_state.analyze_example
            del st.session_state.analyze_example    
    # TAB 3: Practice Quiz
    with tab_quiz:
        st.header("SQL Practice Quiz")
        st.caption("Test your knowledge with interactive quizzes")
        
        # Quiz setup
        if 'quiz_concept' not in st.session_state:
            # Select concept for quiz
            concept_options = {
                f"{c['name']} ({c['difficulty']})": cid 
                for cid, c in SQL_CONCEPTS.items()
            }
            
            selected = st.selectbox(
                "Choose a concept to practice",
                options=list(concept_options.keys())
            )
            
            if st.button("🎯 Start Quiz", type="primary"):
                st.session_state.quiz_concept = concept_options[selected]
                st.session_state.quiz_score = 0
                st.session_state.quiz_question = 0
                st.rerun()
        
        else:
            # Active quiz
            concept_id = st.session_state.quiz_concept
            concept = SQL_CONCEPTS[concept_id]
            
            # Get quiz questions
            quiz = get_quiz_for_concept(concept_id)
            
            if quiz and 'questions' in quiz:
                current_q = st.session_state.get('quiz_question', 0)
                
                if current_q < len(quiz['questions']):
                    # Display current question
                    question = quiz['questions'][current_q]
                    
                    st.subheader(f"Question {current_q + 1} of {len(quiz['questions'])}")
                    st.write(question['question'])
                    
                    # Answer options
                    answer = st.radio(
                        "Select your answer:",
                        question['options'],
                        key=f"quiz_answer_{current_q}"
                    )
                    
                    col1, col2 = st.columns(2)
                    with col1:
                        if st.button("Submit Answer", type="primary"):
                            # Check answer
                            if answer == question['correct']:
                                st.success("✅ Correct!")
                                st.session_state.quiz_score += 1
                            else:
                                st.error(f"❌ Incorrect. The correct answer is: {question['correct']}")
                            
                            # Show explanation
                            st.info(f"💡 {question['explanation']}")
                            
                            # Add navigation delay
                            time.sleep(2)
                            st.session_state.quiz_question = current_q + 1
                            st.rerun()                    
                    with col2:
                        if st.button("Skip Question"):
                            st.session_state.quiz_question = current_q + 1
                            st.rerun()
                
                else:
                    # Quiz complete
                    score = st.session_state.quiz_score
                    total = len(quiz['questions'])
                    percentage = (score / total) * 100
                    
                    st.balloons()
                    st.success(f"🎉 Quiz Complete!")
                    st.metric("Your Score", f"{score}/{total}", f"{percentage:.0f}%")
                    
                    # Performance feedback
                    if percentage >= 80:
                        st.success("Excellent work! You've mastered this concept.")
                    elif percentage >= 60:
                        st.info("Good job! Review the questions you missed.")
                    else:
                        st.warning("Keep practicing! Consider reviewing the concept again.")
                    
                    col1, col2 = st.columns(2)
                    with col1:
                        if st.button("📖 Review Concept"):
                            st.session_state.selected_concept = concept_id
                            st.session_state.active_tab = "learn"
                            del st.session_state.quiz_concept
                            st.rerun()
                    
                    with col2:
                        if st.button("🔄 Try Another Quiz"):
                            del st.session_state.quiz_concept
                            del st.session_state.quiz_score
                            del st.session_state.quiz_question
                            st.rerun()
            else:
                st.warning("No quiz available for this concept yet.")
                if st.button("Choose Another Concept"):
                    del st.session_state.quiz_concept
                    st.rerun()    
    # TAB 4: Learning Path
    with tab_path:
        st.header("Personalized Learning Path")
        st.caption("Find the optimal path between SQL concepts")
        
        col1, col2 = st.columns(2)
        
        # Starting point
        with col1:
            start_options = {"None (Absolute Beginner)": None}
            start_options.update({
                f"{c['name']} ({c['difficulty']})": cid 
                for cid, c in SQL_CONCEPTS.items()
                if c['difficulty'] in ['Beginner', 'Intermediate']
            })
            
            start_concept = st.selectbox(
                "Your current knowledge level",
                options=list(start_options.keys())
            )
            start_id = start_options[start_concept]
        
        # Target goal
        with col2:
            target_options = {
                f"{c['name']} ({c['difficulty']})": cid 
                for cid, c in SQL_CONCEPTS.items()
            }
            
            target_concept = st.selectbox(
                "What you want to learn",
                options=list(target_options.keys())
            )
            target_id = target_options[target_concept]
        
        if st.button("🗺️ Generate Learning Path", type="primary"):
            # Generate path
            if start_id is None:
                # Beginner path - start with basics
                path = []
                # Add fundamental concepts first
                for cid, concept in SQL_CONCEPTS.items():
                    if concept['difficulty'] == 'Beginner' and not concept.get('prerequisites'):
                        path.append({'id': cid, 'name': concept['name'], 
                                   'difficulty': concept['difficulty']})
                        if cid == target_id:
                            break
                
                # Add target if not already included
                if not any(p['id'] == target_id for p in path):
                    target_concept = SQL_CONCEPTS[target_id]
                    path.append({'id': target_id, 'name': target_concept['name'],
                               'difficulty': target_concept['difficulty']})
            else:
                # Use knowledge graph for path finding
                path = get_learning_path(start_id, target_id)            
            if path:
                st.success(f"Found learning path with {len(path)} steps!")
                
                # Visual path display
                st.subheader("Your Learning Journey")
                
                # Progress-style visualization
                for i, step in enumerate(path):
                    col1, col2, col3 = st.columns([1, 3, 1])
                    
                    with col1:
                        if i == 0:
                            st.write("🚀 **START**")
                        elif i == len(path) - 1:
                            st.write("🎯 **GOAL**")
                        else:
                            st.write(f"Step {i}")
                    
                    with col2:
                        concept = SQL_CONCEPTS.get(step.get('id', step.get('name', '')))
                        if concept:
                            color = get_difficulty_color(concept['difficulty'])
                            st.markdown(f"""
                            <div style="
                                border-left: 4px solid {color};
                                padding: 10px 15px;
                                margin: 5px 0;
                                background: linear-gradient(90deg, {color}15 0%, white 100%);
                            ">
                                <strong>{concept['name']}</strong><br>
                                <span style="font-size:0.85em; color:#666;">
                                    {concept['difficulty']} • {concept.get('category', 'SQL')}
                                </span>
                            </div>
                            """, unsafe_allow_html=True)
                    
                    with col3:
                        if concept:
                            if st.button("Learn", key=f"path_learn_{i}_{concept['name']}"):
                                st.session_state.selected_concept = step.get('id', '')
                                st.session_state.active_tab = "learn"
                                st.rerun()
                
                # Study time estimate
                st.divider()
                total_hours = len(path) * 2  # Rough estimate: 2 hours per concept
                st.info(f"⏱️ Estimated study time: {total_hours} hours ({total_hours//8} days at 8 hours/day)")
                
                # Save learning path
                if st.button("💾 Save This Path"):
                    # Save to knowledge base
                    path_content = "\n".join([
                        f"{i+1}. {SQL_CONCEPTS[step.get('id', '')]['name']}" 
                        for i, step in enumerate(path) if step.get('id') in SQL_CONCEPTS
                    ])
                    
                    capture_knowledge(
                        tool_name="sql_academy",
                        prompt=f"Learning path from {start_concept} to {target_concept}",
                        response=path_content,
                        metadata={"type": "learning_path", "steps": len(path)}
                    )
                    st.success("Learning path saved!")
            else:
                st.warning("Could not generate a path. Try selecting different concepts.")

# Entry point
if __name__ == "__main__":
    show()