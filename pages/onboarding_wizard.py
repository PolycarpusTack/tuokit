import streamlit as st
from utils.model_manager import ModelManager

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate, get_available_models
import time
import json

# Page configuration
st.set_page_config(
    page_title="TuoKit - Onboarding Wizard",
    page_icon="🧙‍♂️",
    layout="wide"
)

# Initialize session state for wizard
if "wizard_step" not in st.session_state:
    st.session_state.wizard_step = 0
    st.session_state.wizard_data = {}
    st.session_state.wizard_completed = False
    
# Get available models once at startup
if "wizard_models" not in st.session_state:
    st.session_state.wizard_models = get_available_models()
    st.session_state.wizard_model = st.session_state.wizard_models[0] if st.session_state.wizard_models else "deepseek-coder:6.7b"

# Initialize database
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:
        st.session_state.db = None

# Wizard progress tracker
steps = [
    "🚀 Welcome", 
    "💻 First Code Tool", 
    "📄 Document Processing", 
    "📚 Knowledge Capture", 
    "🎯 Practice Exercise",
    "✅ Completion"
]

# Sidebar with model selection
with st.sidebar:
    st.subheader("🤖 Tutorial Settings")
    st.session_state.wizard_model = st.selectbox(
        "AI Model for Tutorial",
        st.session_state.wizard_models,
        index=0,
        help="Model to use during the tutorial"
    )
    st.divider()
    st.caption("Progress")
    for i, step in enumerate(steps):
        if i < st.session_state.wizard_step:
            st.caption(f"✅ {step}")
        elif i == st.session_state.wizard_step:
            st.caption(f"👉 **{step}**")
        else:
            st.caption(f"• {step}")

# Progress bar
if st.session_state.wizard_step < len(steps):
    progress = st.session_state.wizard_step / (len(steps) - 1)
    st.progress(progress, text=f"Step {st.session_state.wizard_step+1} of {len(steps)}: {steps[st.session_state.wizard_step]}")

# Step 0: Welcome
if st.session_state.wizard_step == 0:
    st.title("👋 Welcome to TuoKit!")
    st.subheader("Your AI-Powered Development Assistant")
    
    col1, col2 = st.columns([2, 1])
    with col1:
        st.markdown("""
        **TuoKit helps you:**
        - 🧠 Understand complex code instantly
        - 🐛 Debug errors with AI assistance
        - 📄 Process documents intelligently
        - 💾 Build a searchable knowledge base
        - 🔒 Keep everything local and private
        
        This **5-minute interactive tutorial** will guide you through:
        1. Core features with hands-on examples
        2. Best practices for each tool
        3. Building your first knowledge entries
        4. Tips from power users
        """)
        
    with col2:
        st.info("""
        **Tutorial Benefits:**
        - ✅ Hands-on practice
        - ✅ Pre-filled examples
        - ✅ Instant feedback
        - ✅ Knowledge saved
        """)
    
    with st.expander("⚙️ System Status Check", expanded=True):
        col1, col2 = st.columns(2)
        with col1:
            try:
                import ollama
                models = ollama.list()
                st.success(f"✅ Ollama: {len(models['models'])} models available")
            except:
                st.error("❌ Ollama not detected")
                st.caption("Run: `ollama serve`")
        
        with col2:
            if st.session_state.db and st.session_state.db.connected:
                st.success("✅ Database: Connected")
            else:
                st.warning("⚠️ Database: Not connected")
                st.caption("Knowledge saving disabled")
    
    st.divider()
    if st.button("🚀 Start Interactive Tutorial", type="primary", use_container_width=True):
        st.session_state.wizard_step = 1
        st.rerun()

# Step 1: Code Tools
elif st.session_state.wizard_step == 1:
    st.title("💻 Code Tools Tutorial")
    st.caption("Learn to analyze, debug, and generate code with AI")
    
    col1, col2 = st.columns([1, 1])
    with col1:
        st.subheader("🎯 Try It Yourself")
        
        # Example selection
        example = st.selectbox("Choose an example:", [
            "Buggy Discount Function",
            "Complex Algorithm",
            "Error Message"
        ])
        
        if example == "Buggy Discount Function":
            sample_code = """def calculate_discount(price, discount):
    # Bug: doesn't handle percentage discounts
    return price - discount"""
            st.code(sample_code, language="python")
            
            if st.button("🔍 Analyze This Code", type="primary"):
                with st.spinner("AI is analyzing..."):
                    response = safe_ollama_generate(
                        model=st.session_state.wizard_model,
                        prompt=f"Explain this code and identify the bug:\n```python\n{sample_code}\n```"
                    )
                    st.session_state.wizard_data["code_response"] = response['response']
                    
        elif example == "Complex Algorithm":
            sample_code = """def quicksort(arr):
    if len(arr) <= 1:
        return arr
    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)"""
            st.code(sample_code, language="python")
            
            if st.button("🔍 Explain Algorithm", type="primary"):
                with st.spinner("AI is analyzing..."):
                    response = safe_ollama_generate(
                        model=st.session_state.wizard_model,
                        prompt=f"Explain this quicksort implementation step by step:\n```python\n{sample_code}\n```"
                    )
                    st.session_state.wizard_data["code_response"] = response['response']
                    
        else:  # Error Message
            st.text_area("Error Message", 
                        value="TypeError: unsupported operand type(s) for /: 'str' and 'int'",
                        height=60)
            st.code("result = user_input / 10", language="python")
            
            if st.button("🐛 Debug This Error", type="primary"):
                with st.spinner("AI is debugging..."):
                    response = safe_ollama_generate(
                        model=st.session_state.wizard_model,
                        prompt="Fix TypeError: unsupported operand type(s) for /: 'str' and 'int' in: result = user_input / 10"
                    )
                    st.session_state.wizard_data["code_response"] = response['response']
    
    with col2:
        st.subheader("📚 Quick Reference")
        st.info("""
        **When to use Code Tools:**
        - 🔍 Understanding unfamiliar code
        - 🐛 Debugging errors
        - ✨ Generating boilerplate
        - 📊 Analyzing complexity
        
        **Pro Tips:**
        - Include full error tracebacks
        - Add comments for context
        - Specify target language
        - Use `# Focus on X` for targeted analysis
        """)
        
        if "code_response" in st.session_state.wizard_data:
            st.subheader("🤖 AI Analysis")
            st.markdown(st.session_state.wizard_data["code_response"])
            st.success("✅ Great job! You've analyzed your first code.")
    
    st.divider()
    col1, col2 = st.columns(2)
    with col1:
        if st.button("← Back", use_container_width=True):
            st.session_state.wizard_step = 0
            st.rerun()
    with col2:
        if st.button("Next: Document Tools →", type="primary", use_container_width=True):
            st.session_state.wizard_step = 2
            st.rerun()

# Step 2: Document Tools
elif st.session_state.wizard_step == 2:
    st.title("📄 Document Tools Tutorial")
    st.caption("Extract insights from any document")
    
    col1, col2 = st.columns([1, 1])
    with col1:
        st.subheader("🎯 Try It Yourself")
        
        sample_text = """Project Status Report - Q1 2025
        
Key Accomplishments:
- Deployed TuoKit to production environment
- Trained 15 team members on AI tools
- Reduced debugging time by 40%
- Built knowledge base with 500+ entries

Challenges:
- Initial Ollama setup complexity
- Database performance tuning needed
- User adoption slower than expected

Next Quarter Goals:
1. Add multi-model support
2. Implement team collaboration features
3. Create VS Code extension
4. Optimize for larger documents

Budget Status: On track ($45K of $50K spent)
Timeline: 2 weeks ahead of schedule"""
        
        st.text_area("Sample Document", value=sample_text, height=250, disabled=True)
        
        action = st.radio("Choose action:", ["Summarize", "Ask Question", "Extract Data"])
        
        if action == "Summarize" and st.button("📝 Generate Summary", type="primary"):
            with st.spinner("Creating summary..."):
                response = safe_ollama_generate(
                    model=st.session_state.wizard_model,
                    prompt=f"Create a 3-bullet executive summary:\n{sample_text}"
                )
                st.session_state.wizard_data["doc_response"] = response['response']
                
        elif action == "Ask Question":
            question = st.text_input("Your question:", "What are the budget details?")
            if st.button("❓ Get Answer", type="primary"):
                with st.spinner("Finding answer..."):
                    response = safe_ollama_generate(
                        model=st.session_state.wizard_model,
                        prompt=f"Answer based on document:\n{sample_text}\n\nQuestion: {question}"
                    )
                    st.session_state.wizard_data["doc_response"] = response['response']
                    
        elif action == "Extract Data" and st.button("🔍 Extract Structure", type="primary"):
            with st.spinner("Extracting data..."):
                response = safe_ollama_generate(
                    model=st.session_state.wizard_model,
                    prompt=f"Extract key data as JSON from:\n{sample_text}"
                )
                st.session_state.wizard_data["doc_response"] = response['response']
    
    with col2:
        st.subheader("📚 Quick Reference")
        st.info("""
        **Document Capabilities:**
        - 📝 Smart summarization
        - ❓ Context-aware Q&A
        - 📊 Data extraction
        - 🔍 Knowledge mining
        
        **Supported Formats:**
        - PDF (with text)
        - TXT files
        - Coming: DOCX, HTML
        
        **Best Practices:**
        - Summarize long docs first
        - Ask specific questions
        - Use extraction for structured data
        """)
        
        if "doc_response" in st.session_state.wizard_data:
            st.subheader("🤖 AI Response")
            st.markdown(st.session_state.wizard_data["doc_response"])
            st.success("✅ Excellent! You've processed your first document.")
    
    st.divider()
    col1, col2 = st.columns(2)
    with col1:
        if st.button("← Back", use_container_width=True):
            st.session_state.wizard_step = 1
            st.rerun()
    with col2:
        if st.button("Next: Knowledge Library →", type="primary", use_container_width=True):
            st.session_state.wizard_step = 3
            st.rerun()

# Step 3: Knowledge Library
elif st.session_state.wizard_step == 3:
    st.title("📚 Knowledge Library Tutorial")
    st.caption("Save and reuse your AI insights")
    
    col1, col2 = st.columns([1, 1])
    with col1:
        st.subheader("🎯 Save Your Progress")
        
        if st.session_state.db and st.session_state.db.connected:
            st.info("Let's save the insights from your tutorial!")
            
            # Show what will be saved
            saves_available = []
            if "code_response" in st.session_state.wizard_data:
                saves_available.append("💻 Code Analysis")
            if "doc_response" in st.session_state.wizard_data:
                saves_available.append("📄 Document Insight")
            
            if saves_available:
                st.markdown("**Ready to save:**")
                for item in saves_available:
                    st.markdown(f"- {item}")
                
                if st.button("💾 Save Tutorial Results", type="primary", use_container_width=True):
                    saved_count = 0
                    
                    # Save code explanation
                    if "code_response" in st.session_state.wizard_data:
                        query_id = st.session_state.db.log_query(
                            tool="onboarding_wizard",
                            model=ModelManager.get_default_model(),
                            prompt="Tutorial code example",
                            response=st.session_state.wizard_data["code_response"]
                        )
                        if query_id:
                            st.session_state.db.save_knowledge_unit(
                                query_id=query_id,
                                title="Tutorial: Code Analysis Example",
                                content=st.session_state.wizard_data["code_response"],
                                category="Tutorial"
                            )
                            saved_count += 1
                    
                    # Save document insight
                    if "doc_response" in st.session_state.wizard_data:
                        query_id = st.session_state.db.log_query(
                            tool="onboarding_wizard",
                            model=ModelManager.get_default_model(),
                            prompt="Tutorial document example",
                            response=st.session_state.wizard_data["doc_response"]
                        )
                        if query_id:
                            st.session_state.db.save_knowledge_unit(
                                query_id=query_id,
                                title="Tutorial: Document Processing Example",
                                content=st.session_state.wizard_data["doc_response"],
                                category="Tutorial"
                            )
                            saved_count += 1
                    
                    st.success(f"✅ Saved {saved_count} knowledge units!")
                    st.balloons()
                    time.sleep(1)
                    
                    # Show search tip
                    st.info("💡 **Try this**: Go to Knowledge Library and search for 'Tutorial'")
            else:
                st.warning("Complete previous steps to have something to save!")
        else:
            st.warning("Database not connected - saving disabled for this tutorial")
            st.caption("You can still explore the interface!")
        
        # Demo search interface
        st.divider()
        st.subheader("🔍 Search Preview")
        search_term = st.text_input("Try searching:", value="Tutorial")
        if search_term:
            st.success(f"In the real Knowledge Library, you'd find all items matching '{search_term}'")
    
    with col2:
        st.subheader("📚 Quick Reference")
        st.info("""
        **Knowledge Workflow:**
        1. **Create** - Use any tool
        2. **Save** - One click to knowledge base
        3. **Search** - Find across all content
        4. **Reuse** - Copy or reference
        
        **Organization Tips:**
        - Use descriptive titles
        - Choose appropriate categories
        - Add tags in content
        - Review and update regularly
        
        **Power Features:**
        - Full-text search
        - Category filtering
        - Export capabilities
        - Edit in place
        """)
        
        with st.expander("📊 Knowledge Statistics"):
            if st.session_state.db and st.session_state.db.connected:
                total = st.session_state.db.get_knowledge_count()
                st.metric("Total Knowledge Units", total)
            else:
                st.metric("Total Knowledge Units", "N/A")
    
    st.divider()
    col1, col2 = st.columns(2)
    with col1:
        if st.button("← Back", use_container_width=True):
            st.session_state.wizard_step = 2
            st.rerun()
    with col2:
        if st.button("Next: Practice →", type="primary", use_container_width=True):
            st.session_state.wizard_step = 4
            st.rerun()

# Step 4: Practice Exercise
elif st.session_state.wizard_step == 4:
    st.title("🎯 Practice Exercise")
    st.caption("Apply what you've learned")
    
    st.markdown("""
    ### Your Challenge:
    You're debugging a Python function that calculates the average of a list but crashes with certain inputs.
    """)
    
    col1, col2 = st.columns([1, 1])
    with col1:
        st.subheader("The Problem")
        
        problem_code = """def calculate_average(numbers):
    total = sum(numbers)
    return total / len(numbers)

# Test cases:
print(calculate_average([1, 2, 3, 4, 5]))  # Works
print(calculate_average([]))  # Crashes!"""
        
        st.code(problem_code, language="python")
        
        st.markdown("""
        **Your Tasks:**
        1. Identify the bug
        2. Get AI to fix it
        3. Save the solution
        """)
        
        user_action = st.radio("What would you like to do?", [
            "Analyze the code",
            "Debug the error",
            "Generate a fix"
        ])
        
        if st.button("🤖 Get AI Help", type="primary"):
            with st.spinner("AI is working..."):
                if user_action == "Analyze the code":
                    prompt = f"Analyze this code and identify potential issues:\n```python\n{problem_code}\n```"
                elif user_action == "Debug the error":
                    prompt = f"Debug this code that crashes with empty list:\n```python\n{problem_code}\n```"
                else:
                    prompt = f"Generate a fixed version of this average calculator:\n```python\n{problem_code}\n```"
                
                response = safe_ollama_generate(
                    model=ModelManager.get_default_model(),
                    prompt=prompt
                )
                st.session_state.wizard_data["practice_response"] = response['response']
    
    with col2:
        st.subheader("📝 Your Solution")
        
        if "practice_response" in st.session_state.wizard_data:
            st.markdown(st.session_state.wizard_data["practice_response"])
            
            st.success("🎉 Great problem-solving!")
            
            correct_solution = """def calculate_average(numbers):
    if not numbers:  # Handle empty list
        return 0
    return sum(numbers) / len(numbers)"""
            
            with st.expander("✅ See one possible solution"):
                st.code(correct_solution, language="python")
        else:
            st.info("👈 Choose an action and get AI help")
            
            st.markdown("""
            **Learning Points:**
            - Empty lists cause division by zero
            - Always validate inputs
            - Consider edge cases
            - Document assumptions
            """)
    
    st.divider()
    col1, col2 = st.columns(2)
    with col1:
        if st.button("← Back", use_container_width=True):
            st.session_state.wizard_step = 3
            st.rerun()
    with col2:
        if st.button("Complete Tutorial →", type="primary", use_container_width=True):
            st.session_state.wizard_step = 5
            st.session_state.wizard_completed = True
            st.rerun()

# Step 5: Completion
elif st.session_state.wizard_step == 5:
    st.title("🎉 Congratulations!")
    st.subheader("You've completed the TuoKit tutorial!")
    
    # Show certificate-style completion
    col1, col2, col3 = st.columns([1, 2, 1])
    with col2:
        st.success("""
        ### 🏆 Tutorial Complete!
        
        **You've learned to:**
        - ✅ Analyze code with AI
        - ✅ Process documents intelligently  
        - ✅ Save knowledge for reuse
        - ✅ Debug real problems
        
        You're now ready to use TuoKit for your daily development tasks!
        """)
    
    st.divider()
    
    # Next steps
    col1, col2 = st.columns(2)
    with col1:
        st.subheader("🚀 Quick Actions")
        if st.button("📊 Go to Dashboard", type="primary", use_container_width=True):
            st.switch_page("app.py")
        if st.button("💻 Start Coding", use_container_width=True):
            st.switch_page("pages/code_tools.py")
        if st.button("📚 Browse Knowledge", use_container_width=True):
            st.switch_page("pages/knowledge_lib.py")
    
    with col2:
        st.subheader("📖 Resources")
        st.markdown("""
        **Continue Learning:**
        - 📘 [User Guide](/) - Detailed documentation
        - 🎥 [Video Tutorials](/) - Coming soon
        - 💬 [Community Forum](/) - Share tips
        - 🐛 [Report Issues](/) - Help improve
        
        **Pro Tips:**
        1. Use keyboard shortcuts (coming)
        2. Create knowledge templates
        3. Share solutions with team
        4. Customize AI prompts
        """)
    
    # Tutorial summary
    st.divider()
    st.subheader("📋 Your Tutorial Summary")
    
    summary = {
        "Duration": "~5 minutes",
        "Tools Explored": 3,
        "Knowledge Created": 2 if st.session_state.db and st.session_state.db.connected else 0,
        "Exercises Completed": 1,
        "Status": "Expert Ready! 🎓"
    }
    
    col1, col2, col3, col4, col5 = st.columns(5)
    with col1:
        st.metric("Duration", summary["Duration"])
    with col2:
        st.metric("Tools Used", summary["Tools Explored"])
    with col3:
        st.metric("Knowledge", summary["Knowledge Created"])
    with col4:
        st.metric("Exercises", summary["Exercises Completed"])
    with col5:
        st.metric("Status", summary["Status"])
    
    # Feedback
    st.divider()
    with st.expander("💭 Share Your Feedback"):
        feedback = st.text_area("How was your tutorial experience?")
        if st.button("Submit Feedback"):
            st.success("Thank you for your feedback!")
    
    # Restart option
    st.divider()
    if st.button("🔄 Restart Tutorial", use_container_width=True):
        st.session_state.wizard_step = 0
        st.session_state.wizard_data = {}
        st.session_state.wizard_completed = False
        st.rerun()

# Add a skip option on all pages except completion
if st.session_state.wizard_step < 5:
    st.divider()
    if st.button("⏭️ Skip Tutorial", key="skip_tutorial"):
        if st.confirm("Are you sure you want to skip the tutorial?"):
            st.session_state.wizard_step = 5
            st.session_state.wizard_completed = True
            st.rerun()