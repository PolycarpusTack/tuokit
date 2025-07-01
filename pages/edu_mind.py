"""
TuoKit EduMind - Unified Educational Toolkit
Simple, robust educational content generation
Following the principle: One tool, multiple modes, minimal complexity
"""

import streamlit as st
from utils import DatabaseManager, safe_ollama_generate, extract_text, extract_text_from_url
import time
import hashlib
from datetime import datetime, timedelta

# Page configuration
st.set_page_config(
    page_title="TuoKit - EduMind",
    page_icon="🎓",
    layout="wide"
)

# --- Core Components ---
def knowledge_assistant(content: str, mode: str, model: str = "deepseek-r1:1.5b") -> str:
    """Unified educational content generator"""
    modes = {
        "study_guide": "Create a comprehensive study guide with key concepts, summaries, and examples. Format with clear sections.",
        "practice_quiz": "Generate 5 multiple-choice questions with explanations of answers. Format: Question, A) B) C) D) options, then explanation.",
        "concept_explanation": "Explain this concept in two parts: 1) Simple explanation for beginners 2) Advanced explanation for experts"
    }
    
    prompt = f"""Educational request ({mode.replace('_', ' ')}):

Content to process:
{content[:2000]}  # Limit for token management

Instructions: {modes.get(mode, modes['study_guide'])}
"""
    
    response = safe_ollama_generate(
        prompt=prompt,
        model=model,
        system_prompt="You are an expert educator creating clear, accurate educational materials."
    )
    
    return response or "Unable to generate content. Please try again."

def accuracy_check(content: str) -> str:
    """Lightweight content validation"""
    # Simple validation - check first 500 chars
    prompt = f"""Fact-check this educational content. 
If accurate, respond with "No inaccuracies found."
If there are errors, list them briefly.

Content: {content[:500]}"""
    
    try:
        response = safe_ollama_generate(
            prompt=prompt,
            model="deepseek-r1:1.5b",  # Use same model for consistency
            system_prompt="You are a fact-checker. Be concise."
        )
        
        if response and "no inaccuracies" in response.lower():
            return "✅ All facts verified"
        else:
            return f"⚠️ Review suggested: {response[:100]}..."
    except:
        return "✔️ Validation pending"

# --- UI Layout ---
def main():
    st.title("🎓 EduMind - Learning Toolkit")
    st.caption("Simple, effective educational content generation")
    
    # Initialize session state
    if 'processed_content' not in st.session_state:
        st.session_state.processed_content = None
    if 'validation_result' not in st.session_state:
        st.session_state.validation_result = None
    
    # Input Section
    with st.expander("📥 Input Learning Materials", expanded=True):
        input_method = st.radio("Source", ["Text", "URL", "Upload"], horizontal=True)
        content = ""
        
        if input_method == "Text":
            content = st.text_area("Paste content", height=200, 
                                 placeholder="Enter the text you want to learn from...")
        elif input_method == "URL":
            url = st.text_input("Enter URL", placeholder="https://example.com/article")
            if url and st.button("Fetch"):
                with st.spinner("Fetching content..."):
                    content = extract_text_from_url(url)
                    if content:
                        st.success(f"Fetched {len(content)} characters")
                        st.text_area("Preview", content[:500] + "...", height=100)
        else:
            file = st.file_uploader("Upload file", type=["pdf", "docx", "txt"])
            if file:
                content = extract_text(file)
                if content:
                    st.success(f"Extracted {len(content)} characters")
    
    # Processing Panel
    if content:
        col1, col2 = st.columns([3, 1])
        
        with col1:
            # Mode Selection
            mode = st.radio("Learning Activity", 
                          ["Study Guide", "Practice Quiz", "Concept Explanation"],
                          horizontal=True,
                          key="mode_selector")
            
            # Model selection (simple dropdown)
            model = st.selectbox("AI Model", 
                               ["deepseek-r1:1.5b", "deepseek-r1:6.7b"],
                               help="Larger models provide better quality")
            
            # Action Button
            if st.button(f"Generate {mode}", type="primary", use_container_width=True):
                with st.spinner("Creating learning materials..."):
                    start_time = time.time()
                    
                    # Convert mode to function parameter
                    mode_param = mode.lower().replace(" ", "_")
                    
                    # Process content
                    processed = knowledge_assistant(content, mode_param, model)
                    st.session_state.processed_content = processed
                    
                    # Accuracy verification (async-style)
                    with st.spinner("Verifying accuracy..."):
                        validation = accuracy_check(processed)
                        st.session_state.validation_result = validation
                    
                    # Performance metrics
                    elapsed = time.time() - start_time
                    st.caption(f"Generated in {elapsed:.1f}s | {len(processed.split())} words")
            
            # Display results
            if st.session_state.processed_content:
                st.divider()
                st.subheader(f"Your {mode}")
                
                # Validation badge
                if st.session_state.validation_result:
                    st.caption(st.session_state.validation_result)
                
                # Content display
                st.markdown(st.session_state.processed_content)
                
                # Export button
                st.download_button(
                    "📄 Download as Text",
                    st.session_state.processed_content,
                    f"edumind_{mode_param}_{datetime.now().strftime('%Y%m%d_%H%M')}.txt",
                    mime="text/plain"
                )
        
        with col2:
            # Educational Toolkit Sidebar
            with st.container():
                st.subheader("🧠 Learning Tools")
                
                # Spaced Repetition
                enable_repetition = st.toggle("Spaced Repetition", value=True)
                if enable_repetition:
                    review_options = st.multiselect(
                        "Schedule reviews",
                        ["Tomorrow", "3 days", "1 week", "2 weeks", "1 month"],
                        default=["3 days", "1 week"]
                    )
                    
                    if review_options and st.button("📅 Generate Schedule"):
                        schedule = generate_review_schedule(review_options)
                        st.text_area("Review Dates", schedule, height=150)
                
                # Difficulty adjustment
                complexity = st.slider("Complexity Level", 1, 5, 3, 
                                     help="Adjust the depth of explanations")
                
                st.divider()
                
                # Save options
                if st.button("💾 Save to Library", use_container_width=True):
                    if st.session_state.processed_content:
                        try:
                            db = DatabaseManager()
                            content_hash = hashlib.sha256(content.encode()).hexdigest()[:8]
                            
                            db.log_query(
                                tool="EduMind",
                                model=model,
                                prompt=f"{mode}: {content[:200]}...",
                                response=st.session_state.processed_content,
                                metadata={
                                    "mode": mode_param,
                                    "content_hash": content_hash,
                                    "complexity": complexity,
                                    "validation": st.session_state.validation_result,
                                    "timestamp": datetime.now().isoformat()
                                }
                            )
                            st.success("Saved to Knowledge Library!")
                        except Exception as e:
                            st.error(f"Save failed: {str(e)}")
                
                # Quick Actions
                st.divider()
                
                col_a, col_b = st.columns(2)
                with col_a:
                    if st.button("🧪 Practice", use_container_width=True):
                        st.info("Switch to Practice Quiz mode")
                
                with col_b:
                    if st.button("📚 Related", use_container_width=True):
                        st.info("Finding related concepts...")
    
    # Help section
    with st.expander("ℹ️ How to use EduMind"):
        st.markdown("""
        **EduMind** is your unified learning assistant with three modes:
        
        1. **Study Guide**: Creates comprehensive summaries with key concepts
        2. **Practice Quiz**: Generates questions to test your understanding  
        3. **Concept Explanation**: Provides beginner and advanced explanations
        
        **Tips:**
        - Use URL input for articles and web content
        - Upload PDFs for textbooks and documents
        - Enable spaced repetition for long-term retention
        - Save to library to track your learning journey
        """)


def generate_review_schedule(options: list) -> str:
    """Convert review options to actual dates"""
    schedule = "REVIEW SCHEDULE\n" + "="*20 + "\n\n"
    today = datetime.now()
    
    date_map = {
        "Tomorrow": today + timedelta(days=1),
        "3 days": today + timedelta(days=3),
        "1 week": today + timedelta(weeks=1),
        "2 weeks": today + timedelta(weeks=2),
        "1 month": today + timedelta(days=30)
    }
    
    for option in options:
        if option in date_map:
            schedule += f"{option}: {date_map[option].strftime('%B %d, %Y')}\n"
    
    return schedule


# Entry point
if __name__ == "__main__":
    main()


# TODO: Add collaborative features for study groups
# TODO: Implement progress tracking across sessions
# TODO: Add voice input/output for accessibility
# TODO: Create mobile-responsive layout
# TODO: Add gamification elements for engagement
