# pages/ruby_katas.py
import streamlit as st
from utils import DatabaseManager, safe_ollama_generate
import json
import random

def generate_kata(level, topic, focus_area=None):
    """Generate coding kata with tests"""
    kata = safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Create {level} kata about {topic}" + (f" focusing on {focus_area}" if focus_area else ""),
        system=(
            "Structure:\n"
            "1. Problem Statement: Clear description\n"
            "2. Requirements: Input/output specs\n"
            "3. Examples: Test cases\n"
            "4. Starter Code: With missing implementation\n"
            "5. Tests: RSpec/minitest\n"
            "Make it engaging and educational"
        )
    )['response']
    
    solution = safe_ollama_generate(
        model="deepseek-coder:latest",
        prompt=f"Solution for kata: {kata}",
        system="Implement solution with comments explaining approach and complexity"
    )['response']
    
    hints = safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Generate 3 progressive hints for kata: {kata}",
        system="Hints should guide without revealing solution"
    )['response']
    
    return kata, solution, hints

def analyze_solution(kata, user_code):
    """Analyze user's solution"""
    return safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Analyze this solution:\nKata: {kata}\nSolution: {user_code}",
        system="Evaluate correctness, efficiency, style, and suggest improvements"
    )['response']

def show():
    st.title("🥋 Ruby Kata Trainer")
    st.caption("Practice Ruby skills with AI-generated coding challenges")
    
    # Initialize session state for kata tracking
    if 'completed_katas' not in st.session_state:
        st.session_state.completed_katas = 0
    if 'current_kata' not in st.session_state:
        st.session_state.current_kata = None
    if 'show_solution' not in st.session_state:
        st.session_state.show_solution = False
    if 'hints_shown' not in st.session_state:
        st.session_state.hints_shown = 0
    
    # Kata configuration
    col1, col2, col3 = st.columns(3)
    with col1:
        level = st.selectbox("Difficulty", ["Beginner", "Intermediate", "Advanced"])
    with col2:
        topic = st.selectbox("Topic", [
            "Algorithms", "OOP", "Metaprogramming", 
            "Functional", "Rails Patterns", "Testing",
            "Data Structures", "Refactoring"
        ])
    with col3:
        # Sub-topics based on main topic
        focus_areas = {
            "Algorithms": ["Sorting", "Searching", "Dynamic Programming", "Recursion"],
            "OOP": ["Inheritance", "Composition", "SOLID", "Design Patterns"],
            "Metaprogramming": ["define_method", "method_missing", "Class Macros", "DSLs"],
            "Functional": ["Blocks", "Procs", "Lambdas", "Enumerables"],
            "Rails Patterns": ["Scopes", "Callbacks", "Concerns", "Service Objects"],
            "Testing": ["Unit Tests", "Mocks", "Integration", "TDD"],
            "Data Structures": ["Arrays", "Hashes", "Trees", "Graphs"],
            "Refactoring": ["Extract Method", "Replace Conditional", "Simplify", "DRY"]
        }
        focus_area = st.selectbox("Focus Area", focus_areas.get(topic, ["General"]))
    
    # Kata statistics
    with st.sidebar:
        st.subheader("📊 Your Progress")
        st.metric("Katas Completed", st.session_state.completed_katas)
        st.progress(min(st.session_state.completed_katas / 10, 1.0))
        if st.session_state.completed_katas >= 10:
            st.balloons()
            st.success("🎉 Kata Master!")
        
        st.subheader("⚙️ Settings")
        show_tests = st.toggle("Show Test Cases", True)
        syntax_highlight = st.toggle("Syntax Highlighting", True)
        timer_enabled = st.toggle("Enable Timer", False)
    
    # Generate kata
    if st.button("🎲 Generate New Challenge", type="primary"):
        st.session_state.show_solution = False
        st.session_state.hints_shown = 0
        with st.spinner("Creating kata..."):
            kata, solution, hints = generate_kata(level, topic, focus_area)
            st.session_state.current_kata = {
                'kata': kata,
                'solution': solution,
                'hints': hints.split('\n'),
                'level': level,
                'topic': topic
            }
    
    # Display current kata
    if st.session_state.current_kata:
        kata_data = st.session_state.current_kata
        
        # Kata header
        st.subheader(f"{kata_data['level']} {kata_data['topic']} Challenge")
        
        # Difficulty indicators
        difficulty_stars = {"Beginner": "⭐", "Intermediate": "⭐⭐", "Advanced": "⭐⭐⭐"}
        col1, col2, col3 = st.columns([2, 1, 1])
        with col1:
            st.markdown(f"**Difficulty:** {difficulty_stars[kata_data['level']]}")
        with col2:
            estimated_time = {"Beginner": "5-10 min", "Intermediate": "15-30 min", "Advanced": "45+ min"}
            st.markdown(f"**Time:** {estimated_time[kata_data['level']]}")
        with col3:
            if timer_enabled:
                st.markdown("**Timer:** ⏱️ Active")
        
        # Display kata
        st.markdown(kata_data['kata'])
        
        # Hints system
        if st.session_state.hints_shown < len(kata_data['hints']):
            if st.button(f"💡 Show Hint ({st.session_state.hints_shown + 1}/{len(kata_data['hints'])})"):
                st.session_state.hints_shown += 1
        
        if st.session_state.hints_shown > 0:
            with st.expander("💡 Hints", expanded=True):
                for i in range(st.session_state.hints_shown):
                    st.info(f"Hint {i+1}: {kata_data['hints'][i]}")
        
        # Practice area
        st.subheader("Your Implementation")
        user_code = st.text_area("Write your solution here:", 
                               height=400,
                               key="kata_solution",
                               help="Write your Ruby code to solve the kata")
        
        # Action buttons
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("🧪 Analyze Solution", type="primary", disabled=not user_code):
                with st.spinner("Analyzing your code..."):
                    analysis = analyze_solution(kata_data['kata'], user_code)
                    st.subheader("Code Analysis")
                    st.markdown(analysis)
                    
                    # Mark as completed if solution is correct
                    if "correct" in analysis.lower() or "works" in analysis.lower():
                        st.success("✅ Well done! Challenge completed!")
                        st.session_state.completed_katas += 1
        
        with col2:
            if st.button("👁️ Show Solution"):
                st.session_state.show_solution = not st.session_state.show_solution
        
        with col3:
            if st.button("💾 Save Kata"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="ruby_katas",
                        model="deepseek-r1:latest",
                        prompt=f"{level} {topic} - {focus_area}",
                        response=kata_data['kata'],
                        metadata={
                            "tags": ["training", "ruby", topic.lower()],
                            "level": level,
                            "solution": kata_data['solution']
                        }
                    )
                    if query_id:
                        st.success("Kata saved to your training library!")
                else:
                    st.error("Could not connect to database")
        
        # Solution display
        if st.session_state.show_solution:
            with st.expander("🎯 Solution", expanded=True):
                st.code(kata_data['solution'], language="ruby")
                
                # Solution explanation
                explanation = safe_ollama_generate(
                    model="deepseek-r1:latest",
                    prompt=f"Explain the solution approach: {kata_data['solution']}",
                    system="Focus on algorithm, time/space complexity, and Ruby idioms used"
                )['response']
                st.markdown("**Explanation:**")
                st.markdown(explanation)
    
    # Learning resources
    with st.expander("📚 Ruby Learning Resources"):
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("""
            **Practice Sites:**
            - [Exercism Ruby Track](https://exercism.org/tracks/ruby)
            - [CodeWars Ruby](https://www.codewars.com/?language=ruby)
            - [Ruby Koans](http://rubykoans.com/)
            - [Ruby Quiz](http://rubyquiz.com/)
            
            **Books:**
            - Eloquent Ruby
            - Practical Object-Oriented Design
            - Metaprogramming Ruby
            - The Well-Grounded Rubyist
            """)
        
        with col2:
            st.markdown("""
            **Online Courses:**
            - [Ruby Tapas](https://www.rubytapas.com/)
            - [GoRails](https://gorails.com/)
            - [Upcase by thoughtbot](https://thoughtbot.com/upcase)
            - [Ruby Monk](https://rubymonk.com/)
            
            **Style Guides:**
            - [Ruby Style Guide](https://rubystyle.guide/)
            - [Rails Style Guide](https://rails.rubystyle.guide/)
            - [RuboCop](https://github.com/rubocop/rubocop)
            """)
    
    # Kata patterns reference
    with st.expander("💎 Common Ruby Patterns"):
        st.markdown("""
        **Enumerable Magic:**
        ```ruby
        # Chain methods for elegance
        result = items
          .select { |i| i.valid? }
          .map(&:process)
          .reduce(0, :+)
        ```
        
        **Memoization:**
        ```ruby
        def expensive_operation
          @result ||= perform_calculation
        end
        ```
        
        **Tap for Debugging:**
        ```ruby
        def create_user(attrs)
          User.new(attrs).tap do |user|
            user.generate_token
            user.send_welcome_email
          end
        end
        ```
        
        **Safe Navigation:**
        ```ruby
        # Ruby 2.3+
        user&.address&.street
        ```
        
        **Pattern Matching (Ruby 3+):**
        ```ruby
        case [status, data]
        in [:ok, result]
          process(result)
        in [:error, message]
          handle_error(message)
        end
        ```
        """)

if __name__ == "__main__":
    show()
