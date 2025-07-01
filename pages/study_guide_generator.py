"""
TuoKit Study Guide Generator
Simple, practical educational content generation
Following TuoKit Architect principles: Build fast, build smart
"""

import streamlit as st
from utils import (
    DatabaseManager, 
    safe_ollama_generate,
    extract_text,
    extract_text_from_url,
    validate_file_size,
    capture_knowledge,
    SimpleLearningStrategy,
    SimpleContentValidator,
    validate_with_ai
)
import hashlib
import json
from datetime import datetime

# Page configuration
st.set_page_config(
    page_title="TuoKit - Study Guide Generator",
    page_icon="📚",
    layout="wide"
)

def generate_educational_content(content: str, difficulty: str, objective: str, model: str = "deepseek-r1:1.5b") -> dict:
    """
    Generate study materials using Ollama
    Simplified approach - no NLTK dependency initially
    """
    # Truncate content for prompt (avoid token limits)
    content_preview = content[:2000] if len(content) > 2000 else content
    
    # Build structured prompt
    prompt = f"""Create comprehensive study materials based on this content:

Content: {content_preview}

Requirements:
- Difficulty Level: {difficulty}
- Learning Objective: {objective}

Generate the following in a structured format:

1. SUMMARY (5-7 bullet points of key concepts)

2. FLASHCARDS (5-10 Q&A pairs)
Format: Question | Answer

3. QUIZ (5 multiple choice questions)
Format: Question | A) Option | B) Option | C) Option | D) Option | Correct: Letter

4. KEY TERMS (important vocabulary with definitions)"""
    
    try:
        # Generate content with error handling
        response = safe_ollama_generate(
            prompt=prompt,
            model=model,  # Use selected model
            system_prompt="You are an expert educator creating clear, structured study materials."
        )
        
        if not response:
            return {"error": "Failed to generate content"}
            
        # Parse response into structured format
        return parse_study_materials(response)
        
    except Exception as e:
        st.error(f"Content generation error: {str(e)}")
        return {"error": str(e)}


def parse_study_materials(response: str) -> dict:
    """
    Parse Ollama response into structured study materials
    Robust parsing with fallbacks
    """
    materials = {
        "summary": [],
        "flashcards": [],
        "quiz": [],
        "key_terms": [],
        "raw_response": response
    }
    
    try:
        # Split response into sections
        sections = response.split('\n\n')
        current_section = None
        
        for section in sections:
            section_lower = section.lower()
            
            # Identify section type
            if 'summary' in section_lower:
                current_section = 'summary'
            elif 'flashcard' in section_lower:
                current_section = 'flashcards'
            elif 'quiz' in section_lower:
                current_section = 'quiz'
            elif 'key term' in section_lower or 'vocabulary' in section_lower:
                current_section = 'key_terms'
            elif current_section:
                # Parse content based on current section
                lines = [line.strip() for line in section.split('\n') if line.strip()]
                
                if current_section == 'summary':
                    # Extract bullet points
                    materials['summary'] = [
                        line.lstrip('•-*').strip() 
                        for line in lines 
                        if line and not line.lower().startswith('summary')
                    ]
                    
                elif current_section == 'flashcards':
                    # Parse Q&A pairs
                    for line in lines:
                        if '|' in line:
                            parts = line.split('|', 1)
                            if len(parts) == 2:
                                materials['flashcards'].append({
                                    'question': parts[0].strip(),
                                    'answer': parts[1].strip()
                                })
                
                elif current_section == 'quiz':
                    # Parse quiz questions
                    question_data = {}
                    for line in lines:
                        if line and line[0].isdigit():  # Question number
                            if question_data:
                                materials['quiz'].append(question_data)
                            question_data = {'question': line, 'options': [], 'correct': ''}
                        elif line.startswith(('A)', 'B)', 'C)', 'D)')):
                            question_data['options'].append(line)
                        elif 'correct:' in line.lower():
                            question_data['correct'] = line.split(':')[-1].strip()
                    
                    if question_data:
                        materials['quiz'].append(question_data)
                        
                elif current_section == 'key_terms':
                    # Parse key terms
                    for line in lines:
                        if ':' in line:
                            term, definition = line.split(':', 1)
                            materials['key_terms'].append({
                                'term': term.strip(),
                                'definition': definition.strip()
                            })
                            
    except Exception as e:
        st.warning(f"Parsing warning: {str(e)}")
        # Return materials with whatever was successfully parsed
        
    return materials


def main():
    """Main UI for Study Guide Generator"""
    st.title("📚 Study Guide Generator")
    st.markdown("Transform any content into comprehensive study materials")
    
    # Initialize session state
    if 'study_materials' not in st.session_state:
        st.session_state.study_materials = None
    
    # Input Section
    col1, col2 = st.columns([2, 1])
    
    with col1:
        input_method = st.radio(
            "Input Source",
            ["Text", "File Upload", "URL"],
            horizontal=True
        )
        
        content = ""
        source_info = ""
        
        if input_method == "Text":
            content = st.text_area(
                "Paste your content here",
                height=300,
                placeholder="Enter the text you want to create study materials from..."
            )
            source_info = "Manual text input"
            
        elif input_method == "File Upload":
            uploaded_file = st.file_uploader(
                "Choose a file",
                type=['txt', 'pdf', 'docx'],
                help="Supported formats: TXT, PDF, DOCX (max 10MB)"
            )
            if uploaded_file:
                if validate_file_size(uploaded_file):
                    with st.spinner("Extracting text..."):
                        content = extract_text(uploaded_file)
                        source_info = f"File: {uploaded_file.name}"
                    if content:
                        st.success(f"Extracted {len(content)} characters")
        
        elif input_method == "URL":
            url = st.text_input(
                "Enter URL",
                placeholder="https://example.com/article"
            )
            if url and st.button("Fetch Content"):
                with st.spinner("Fetching content from URL..."):
                    content = extract_text_from_url(url)
                    source_info = f"URL: {url}"
                if content:
                    st.success(f"Extracted {len(content)} characters")
                    st.text_area("Preview", content[:500] + "...", height=150)
    
    # Configuration sidebar
    with col2:
        st.subheader("⚙️ Settings")
        
        difficulty = st.select_slider(
            "Difficulty Level",
            options=["Beginner", "Intermediate", "Advanced"],
            value="Intermediate"
        )
        
        objective = st.radio(
            "Learning Objective",
            ["Quick Review", "Deep Understanding", "Exam Preparation"],
            help="Adjusts the depth and style of generated materials"
        )
        
        model = st.selectbox(
            "AI Model",
            ["deepseek-r1:1.5b", "deepseek-r1:6.7b", "llama3.2"],
            help="Larger models produce better results but take longer"
        )
        
        st.divider()
        
        save_to_library = st.checkbox(
            "Save to Knowledge Library",
            value=True,
            help="Store generated materials for future reference"
        )
        
        enable_spaced_repetition = st.checkbox(
            "Enable Spaced Repetition",
            help="Generate review schedule for long-term retention"
        )
        
        validate_accuracy = st.checkbox(
            "Validate Accuracy",
            value=True,
            help="Check generated content against source material"
        )
    
    # Generate button
    if st.button("🎯 Generate Study Guide", type="primary", disabled=not content):
        with st.spinner("Creating your personalized study materials..."):
            # Generate content hash for deduplication
            content_hash = hashlib.sha256(content.encode()).hexdigest()[:8]
            
            # Generate materials
            materials = generate_educational_content(content, difficulty, objective, model)
            
            if "error" not in materials:
                st.session_state.study_materials = materials
                st.session_state.content_hash = content_hash
                st.session_state.source_content = content  # Store for validation
                st.success("Study guide generated successfully!")
                
                # Validate accuracy if enabled
                if validate_accuracy and materials.get('raw_response'):
                    validator = SimpleContentValidator()
                    validation_result = validator.quick_accuracy_check(
                        materials['raw_response'], 
                        content[:5000]  # Use first 5k chars of source
                    )
                    st.session_state.validation_result = validation_result
                
                # Save to knowledge library if enabled
                if save_to_library:
                    try:
                        db = DatabaseManager()
                        query_id = db.log_query(
                            tool="study_guide_generator",
                            prompt=f"{difficulty} {objective} study guide from {source_info}",
                            response=json.dumps(materials),
                            metadata={
                                "content_hash": content_hash,
                                "difficulty": difficulty,
                                "objective": objective,
                                "source": source_info,
                                "timestamp": datetime.now().isoformat()
                            }
                        )
                        
                        # Track study session if spaced repetition enabled
                        if enable_spaced_repetition and materials.get('key_terms'):
                            strategy = SimpleLearningStrategy()
                            concepts = [term['term'] for term in materials.get('key_terms', [])]
                            strategy.track_study_session(
                                content_hash=content_hash,
                                quiz_score=0.0,  # Initial score
                                concepts=concepts,
                                difficulty=difficulty
                            )
                            
                    except Exception as e:
                        st.warning(f"Could not save to library: {str(e)}")
    
    # Display results
    if st.session_state.study_materials and "error" not in st.session_state.study_materials:
        materials = st.session_state.study_materials
        
        # Display accuracy validation if available
        if 'validation_result' in st.session_state:
            validation = st.session_state.validation_result
            col1, col2 = st.columns([3, 1])
            
            with col1:
                if validation['accuracy_score'] >= 8:
                    st.success(f"✅ Content Accuracy: {validation['accuracy_score']}/10 - High confidence")
                elif validation['accuracy_score'] >= 6:
                    st.warning(f"⚠️ Content Accuracy: {validation['accuracy_score']}/10 - Medium confidence")
                else:
                    st.error(f"❌ Content Accuracy: {validation['accuracy_score']}/10 - Low confidence")
            
            with col2:
                if validation['total_issues'] > 0:
                    with st.expander(f"View {validation['total_issues']} issues"):
                        for issue in validation['issues']:
                            st.write(f"• {issue['type']}: {issue['content']}")
                            st.caption(issue['suggestion'])
        
        # Create tabs for different material types
        tabs = st.tabs([
            "📝 Summary", 
            "🎴 Flashcards", 
            "❓ Quiz", 
            "📖 Key Terms",
            "📅 Review Schedule"
        ])
        
        tab1, tab2, tab3, tab4, tab5 = tabs
        
        with tab1:
            st.subheader("📝 Key Concepts Summary")
            if materials.get('summary'):
                for i, point in enumerate(materials['summary'], 1):
                    st.markdown(f"{i}. {point}")
            else:
                st.info("No summary generated")
        
        with tab2:
            st.subheader("🎴 Flashcards for Review")
            if materials.get('flashcards'):
                for i, card in enumerate(materials['flashcards']):
                    with st.expander(f"Card {i+1}: {card['question']}"):
                        st.write(f"**Answer:** {card['answer']}")
            else:
                st.info("No flashcards generated")
        
        with tab3:
            st.subheader("❓ Practice Quiz")
            if materials.get('quiz'):
                score = 0
                total = len(materials['quiz'])
                
                for i, q in enumerate(materials['quiz']):
                    st.markdown(f"**{q['question']}**")
                    user_answer = st.radio(
                        f"Select answer for Q{i+1}",
                        q['options'],
                        key=f"quiz_{i}",
                        label_visibility="collapsed"
                    )
                    
                    if st.button(f"Check Answer", key=f"check_{i}"):
                        if q['correct'] in user_answer:
                            st.success("Correct! ✅")
                            score += 1
                        else:
                            st.error(f"Incorrect. The correct answer is: {q['correct']}")
                    
                    st.divider()
            else:
                st.info("No quiz questions generated")
        
        with tab4:
            st.subheader("📖 Key Terms & Definitions")
            if materials.get('key_terms'):
                for term_data in materials['key_terms']:
                    st.markdown(f"**{term_data['term']}**: {term_data['definition']}")
            else:
                st.info("No key terms generated")
        
        with tab5:
            st.subheader("📅 Spaced Repetition Schedule")
            
            if enable_spaced_repetition and materials.get('key_terms'):
                strategy = SimpleLearningStrategy()
                concepts = [term['term'] for term in materials.get('key_terms', [])]
                
                # Check for existing retention data
                if 'content_hash' in st.session_state:
                    retention = strategy.get_retention_estimate(st.session_state.content_hash)
                    if retention:
                        st.metric("Estimated Retention", f"{retention:.0f}%")
                
                # Generate schedule
                schedule = strategy.generate_review_schedule(concepts, difficulty)
                
                st.write("**Optimal review dates for key concepts:**")
                
                # Display schedule in a clean format
                for concept, dates in list(schedule.items())[:5]:  # Show first 5
                    with st.container():
                        st.markdown(f"**{concept}**")
                        date_cols = st.columns(len(dates))
                        for i, date in enumerate(dates):
                            date_cols[i].caption(date.strftime("%b %d"))
                
                # Export schedule
                if st.button("📅 Export Schedule"):
                    schedule_text = "SPACED REPETITION SCHEDULE\n" + "="*30 + "\n\n"
                    for concept, dates in schedule.items():
                        schedule_text += f"{concept}:\n"
                        for date in dates:
                            schedule_text += f"  - {date.strftime('%B %d, %Y')}\n"
                        schedule_text += "\n"
                    
                    st.download_button(
                        "Download Schedule",
                        schedule_text,
                        f"review_schedule_{st.session_state.content_hash}.txt",
                        mime="text/plain"
                    )
            else:
                st.info("Enable 'Spaced Repetition' in settings to generate a review schedule")
        
        # Export options
        st.divider()
        col1, col2 = st.columns(2)
        
        with col1:
            # Export as text
            if st.button("📄 Export as Text"):
                export_text = generate_export_text(materials)
                st.download_button(
                    "Download Study Guide",
                    export_text,
                    f"study_guide_{content_hash}.txt",
                    mime="text/plain"
                )
        
        with col2:
            # Show raw response for debugging
            with st.expander("🔍 View Raw Response"):
                st.code(materials.get('raw_response', ''), language=None)


def generate_export_text(materials: dict) -> str:
    """Generate a formatted text export of study materials"""
    export_lines = ["STUDY GUIDE", "=" * 50, ""]
    
    # Summary section
    export_lines.extend(["SUMMARY", "-" * 20])
    for i, point in enumerate(materials.get('summary', []), 1):
        export_lines.append(f"{i}. {point}")
    export_lines.append("")
    
    # Flashcards section
    export_lines.extend(["FLASHCARDS", "-" * 20])
    for card in materials.get('flashcards', []):
        export_lines.append(f"Q: {card['question']}")
        export_lines.append(f"A: {card['answer']}")
        export_lines.append("")
    
    # Quiz section
    export_lines.extend(["QUIZ", "-" * 20])
    for i, q in enumerate(materials.get('quiz', []), 1):
        export_lines.append(f"{q['question']}")
        for option in q['options']:
            export_lines.append(f"  {option}")
        export_lines.append(f"  Correct: {q['correct']}")
        export_lines.append("")
    
    # Key terms section
    export_lines.extend(["KEY TERMS", "-" * 20])
    for term_data in materials.get('key_terms', []):
        export_lines.append(f"{term_data['term']}: {term_data['definition']}")
    
    return "\n".join(export_lines)


# Entry point
if __name__ == "__main__":
    main()

# TODO: Add NLTK integration for better concept extraction
# TODO: Implement spaced repetition scheduling for flashcards
# TODO: Add PDF export with proper formatting
# TODO: Create Anki deck export functionality
# TODO: Add progress tracking for study sessions
