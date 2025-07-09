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
    validate_with_ai,
    get_available_models
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
            system="You are an expert educator creating clear, structured study materials."
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
        
        # Model selection - dynamically load from Ollama
        available_models = get_available_models(["deepseek-r1:1.5b", "deepseek-r1:6.7b", "llama3.2"])
        model = st.selectbox(
            "AI Model",
            available_models,
            help="Models currently available in Ollama. Larger models produce better results but take longer"
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

def extract_concepts_with_nltk(text: str, max_concepts: int = 10) -> list:
    """
    Extract key concepts using NLTK for improved accuracy.
    Falls back to simple extraction if NLTK unavailable.
    """
    try:
        import nltk
        from nltk.tokenize import word_tokenize, sent_tokenize
        from nltk.corpus import stopwords
        from nltk.tag import pos_tag
        from collections import Counter
        
        # Download required NLTK data (cached after first download)
        try:
            nltk.data.find('tokenizers/punkt')
        except LookupError:
            nltk.download('punkt', quiet=True)
        
        try:
            nltk.data.find('corpora/stopwords')
        except LookupError:
            nltk.download('stopwords', quiet=True)
            
        try:
            nltk.data.find('taggers/averaged_perceptron_tagger')
        except LookupError:
            nltk.download('averaged_perceptron_tagger', quiet=True)
        
        # Tokenize and tag parts of speech
        sentences = sent_tokenize(text)
        stop_words = set(stopwords.words('english'))
        
        # Extract noun phrases and important terms
        concepts = []
        noun_phrases = []
        
        for sentence in sentences:
            words = word_tokenize(sentence.lower())
            # Filter and tag remaining words
            filtered_words = [w for w in words if w.isalpha() and w not in stop_words and len(w) > 2]
            tagged = pos_tag(filtered_words)
            
            # Extract nouns and noun phrases
            current_phrase = []
            for word, tag in tagged:
                if tag in ['NN', 'NNS', 'NNP', 'NNPS']:  # Nouns
                    current_phrase.append(word)
                elif current_phrase:
                    if len(current_phrase) > 1:
                        noun_phrases.append(' '.join(current_phrase))
                    elif len(current_phrase[0]) > 3:
                        concepts.append(current_phrase[0])
                    current_phrase = []
            
            # Don't forget the last phrase
            if current_phrase:
                if len(current_phrase) > 1:
                    noun_phrases.append(' '.join(current_phrase))
                elif len(current_phrase[0]) > 3:
                    concepts.append(current_phrase[0])
        
        # Combine and count frequency
        all_concepts = concepts + noun_phrases
        concept_freq = Counter(all_concepts)
        
        # Get most common concepts
        top_concepts = []
        for concept, freq in concept_freq.most_common(max_concepts):
            # Only include concepts that appear multiple times or are phrases
            if freq > 1 or ' ' in concept:
                top_concepts.append({
                    'term': concept.title(),
                    'frequency': freq,
                    'type': 'phrase' if ' ' in concept else 'term'
                })
        
        return top_concepts
        
    except ImportError:
        # Fallback to simple extraction without NLTK
        return extract_concepts_simple(text, max_concepts)
    except Exception as e:
        # Log error and use fallback
        st.warning(f"NLTK extraction failed: {str(e)}. Using simple extraction.")
        return extract_concepts_simple(text, max_concepts)


def extract_concepts_simple(text: str, max_concepts: int = 10) -> list:
    """
    Simple concept extraction without NLTK dependency.
    Uses basic heuristics and frequency analysis.
    """
    import re
    from collections import Counter
    
    # Basic preprocessing
    text_lower = text.lower()
    
    # Remove common words (basic stopwords)
    common_words = {
        'the', 'a', 'an', 'and', 'or', 'but', 'in', 'on', 'at', 'to', 'for',
        'of', 'with', 'by', 'from', 'up', 'about', 'into', 'through', 'during',
        'before', 'after', 'above', 'below', 'between', 'under', 'again',
        'further', 'then', 'once', 'is', 'are', 'was', 'were', 'been', 'be',
        'have', 'has', 'had', 'do', 'does', 'did', 'will', 'would', 'should',
        'could', 'may', 'might', 'must', 'can', 'this', 'that', 'these', 'those'
    }
    
    # Extract words (alphanumeric, length > 3)
    words = re.findall(r'\b[a-z]+\b', text_lower)
    filtered_words = [w for w in words if len(w) > 3 and w not in common_words]
    
    # Count frequency
    word_freq = Counter(filtered_words)
    
    # Extract potential phrases (capitalized sequences)
    phrases = re.findall(r'[A-Z][a-z]+(?:\s+[A-Z][a-z]+)*', text)
    phrase_freq = Counter(phrases)
    
    # Combine and sort by frequency
    concepts = []
    
    # Add top phrases
    for phrase, freq in phrase_freq.most_common(max_concepts // 2):
        if freq > 1 and len(phrase.split()) <= 3:
            concepts.append({
                'term': phrase,
                'frequency': freq,
                'type': 'phrase'
            })
    
    # Add top words
    remaining_slots = max_concepts - len(concepts)
    for word, freq in word_freq.most_common(remaining_slots):
        if freq > 2:  # Only words that appear multiple times
            concepts.append({
                'term': word.capitalize(),
                'frequency': freq,
                'type': 'term'
            })
    
    return concepts
def create_spaced_repetition_schedule(flashcards: list, initial_interval: int = 1) -> dict:
    """
    Generate spaced repetition schedule for flashcards using SM-2 algorithm.
    Returns schedule with review dates and difficulty adjustments.
    """
    from datetime import datetime, timedelta
    import json
    
    # SM-2 algorithm parameters
    schedule = {
        'flashcards': [],
        'algorithm': 'SM-2',
        'created_at': datetime.now().isoformat(),
        'settings': {
            'initial_interval': initial_interval,
            'easy_bonus': 1.3,
            'min_factor': 1.3,
            'max_interval': 365
        }
    }
    
    for idx, card in enumerate(flashcards):
        card_schedule = {
            'id': f'card_{idx}_{datetime.now().timestamp()}',
            'question': card.get('question', ''),
            'answer': card.get('answer', ''),
            'reviews': [],
            'current_interval': initial_interval,
            'ease_factor': 2.5,  # Default ease factor
            'next_review': (datetime.now() + timedelta(days=initial_interval)).isoformat(),
            'total_reviews': 0,
            'consecutive_correct': 0
        }
        schedule['flashcards'].append(card_schedule)
    
    return schedule


def update_flashcard_review(card_schedule: dict, quality: int) -> dict:
    """
    Update flashcard schedule after review based on response quality.
    Quality: 0-5 (0=complete blackout, 5=perfect recall)
    """
    from datetime import datetime, timedelta
    
    # Record review
    card_schedule['reviews'].append({
        'date': datetime.now().isoformat(),
        'quality': quality,
        'interval_before': card_schedule['current_interval']
    })
    
    card_schedule['total_reviews'] += 1
    
    # Update ease factor (EF) using SM-2 formula
    old_ef = card_schedule['ease_factor']
    if quality >= 3:  # Correct response
        card_schedule['consecutive_correct'] += 1
        # EF' = EF + (0.1 - (5 - quality) * (0.08 + (5 - quality) * 0.02))
        card_schedule['ease_factor'] = max(1.3, old_ef + (0.1 - (5 - quality) * (0.08 + (5 - quality) * 0.02)))
    else:  # Incorrect response
        card_schedule['consecutive_correct'] = 0
        card_schedule['ease_factor'] = max(1.3, old_ef - 0.2)
    
    # Calculate next interval
    if quality < 3:  # Failed recall
        card_schedule['current_interval'] = 1  # Reset to 1 day
    else:
        if card_schedule['consecutive_correct'] == 1:
            card_schedule['current_interval'] = 1
        elif card_schedule['consecutive_correct'] == 2:
            card_schedule['current_interval'] = 6
        else:
            # I(n) = I(n-1) * EF
            new_interval = card_schedule['current_interval'] * card_schedule['ease_factor']
            card_schedule['current_interval'] = min(365, round(new_interval))
    
    # Set next review date
    card_schedule['next_review'] = (
        datetime.now() + timedelta(days=card_schedule['current_interval'])
    ).isoformat()
    
    return card_schedule


def get_due_flashcards(schedule: dict, include_new: bool = True) -> list:
    """
    Get flashcards due for review, sorted by priority.
    Returns cards that need review today or are overdue.
    """
    from datetime import datetime
    
    due_cards = []
    now = datetime.now()
    
    for card in schedule['flashcards']:
        next_review = datetime.fromisoformat(card['next_review'])
        
        # Check if card is due
        if next_review <= now:
            days_overdue = (now - next_review).days
            card['days_overdue'] = days_overdue
            card['priority'] = days_overdue + (10 if card['total_reviews'] == 0 else 0)
            due_cards.append(card)
        elif include_new and card['total_reviews'] == 0:
            # Include new cards that haven't been reviewed yet
            card['days_overdue'] = 0
            card['priority'] = 5  # Medium priority for new cards
            due_cards.append(card)
    
    # Sort by priority (higher = more urgent)
    due_cards.sort(key=lambda x: x['priority'], reverse=True)
    
    return due_cards


def export_spaced_repetition_schedule(schedule: dict, format: str = 'json') -> str:
    """
    Export spaced repetition schedule in various formats.
    Supports JSON, CSV, and Anki-compatible format.
    """
    if format == 'json':
        return json.dumps(schedule, indent=2)
    
    elif format == 'csv':
        lines = ['Question,Answer,Next Review,Interval (days),Ease Factor,Total Reviews']
        for card in schedule['flashcards']:
            lines.append(
                f'"{card["question"]}","{card["answer"]}",'
                f'{card["next_review"]},{card["current_interval"]},'
                f'{card["ease_factor"]:.2f},{card["total_reviews"]}'
            )
        return '\n'.join(lines)
    
    elif format == 'anki':
        # Basic Anki import format (tab-separated)
        lines = []
        for card in schedule['flashcards']:
            # Format: Front[tab]Back[tab]Tags
            tags = f"interval:{card['current_interval']} reviews:{card['total_reviews']}"
            lines.append(f"{card['question']}\t{card['answer']}\t{tags}")
        return '\n'.join(lines)
    
    else:
        return "Unsupported format"
def export_to_pdf(materials: dict, filename: str = "study_guide.pdf", metadata: dict = None) -> bytes:
    """
    Export study materials to PDF with professional formatting.
    Uses ReportLab for PDF generation with fallback to simple PDF.
    """
    try:
        from reportlab.lib.pagesizes import letter, A4
        from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak, Table, TableStyle
        from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
        from reportlab.lib.units import inch
        from reportlab.lib import colors
        from reportlab.lib.enums import TA_CENTER, TA_JUSTIFY
        from io import BytesIO
        
        # Create PDF buffer
        buffer = BytesIO()
        
        # Create document
        doc = SimpleDocTemplate(
            buffer,
            pagesize=letter,
            rightMargin=72,
            leftMargin=72,
            topMargin=72,
            bottomMargin=18
        )
        
        # Container for the 'Flowable' objects
        elements = []
        
        # Define styles
        styles = getSampleStyleSheet()
        title_style = ParagraphStyle(
            'CustomTitle',
            parent=styles['Title'],
            fontSize=24,
            textColor=colors.HexColor('#2E4057'),
            spaceAfter=30,
            alignment=TA_CENTER
        )
        heading_style = ParagraphStyle(
            'CustomHeading',
            parent=styles['Heading1'],
            fontSize=16,
            textColor=colors.HexColor('#2E4057'),
            spaceAfter=12,
            spaceBefore=12
        )
        normal_style = styles['Normal']
        normal_style.alignment = TA_JUSTIFY
        
        # Add title
        elements.append(Paragraph("Study Guide", title_style))
        if metadata:
            date_str = metadata.get('date', datetime.now().strftime('%B %d, %Y'))
            elements.append(Paragraph(f"Generated on {date_str}", styles['Normal']))
        elements.append(Spacer(1, 0.5*inch))
        
        # Summary section
        if materials.get('summary'):
            elements.append(Paragraph("Summary", heading_style))
            for i, point in enumerate(materials['summary'], 1):
                bullet_text = f"• {point}"
                elements.append(Paragraph(bullet_text, normal_style))
                elements.append(Spacer(1, 0.1*inch))
            elements.append(Spacer(1, 0.3*inch))
        
        # Flashcards section
        if materials.get('flashcards'):
            elements.append(Paragraph("Flashcards", heading_style))
            flashcard_data = []
            for card in materials['flashcards']:
                flashcard_data.append([
                    Paragraph("<b>Q:</b>", styles['Normal']),
                    Paragraph(card['question'], styles['Normal'])
                ])
                flashcard_data.append([
                    Paragraph("<b>A:</b>", styles['Normal']),
                    Paragraph(card['answer'], styles['Normal'])
                ])
                flashcard_data.append(['', ''])  # Empty row for spacing
            
            if flashcard_data:
                flashcard_table = Table(flashcard_data, colWidths=[0.5*inch, 5*inch])
                flashcard_table.setStyle(TableStyle([
                    ('VALIGN', (0, 0), (-1, -1), 'TOP'),
                    ('LEFTPADDING', (0, 0), (-1, -1), 0),
                    ('RIGHTPADDING', (0, 0), (-1, -1), 0),
                    ('BOTTOMPADDING', (0, 0), (-1, -1), 6),
                ]))
                elements.append(flashcard_table)
            elements.append(Spacer(1, 0.3*inch))
        
        # Quiz section
        if materials.get('quiz'):
            elements.append(PageBreak())
            elements.append(Paragraph("Quiz Questions", heading_style))
            for i, q in enumerate(materials['quiz'], 1):
                elements.append(Paragraph(f"<b>{q['question']}</b>", normal_style))
                elements.append(Spacer(1, 0.1*inch))
                
                for option in q['options']:
                    elements.append(Paragraph(f"    {option}", normal_style))
                
                elements.append(Spacer(1, 0.05*inch))
                elements.append(Paragraph(f"<i>Answer: {q['correct']}</i>", styles['Italic']))
                elements.append(Spacer(1, 0.2*inch))
        
        # Key terms section
        if materials.get('key_terms'):
            elements.append(Paragraph("Key Terms", heading_style))
            terms_data = []
            for term_data in materials['key_terms']:
                terms_data.append([
                    Paragraph(f"<b>{term_data['term']}</b>", styles['Normal']),
                    Paragraph(term_data['definition'], styles['Normal'])
                ])
            
            if terms_data:
                terms_table = Table(terms_data, colWidths=[1.5*inch, 4.5*inch])
                terms_table.setStyle(TableStyle([
                    ('VALIGN', (0, 0), (-1, -1), 'TOP'),
                    ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
                    ('BACKGROUND', (0, 0), (0, -1), colors.HexColor('#F0F0F0')),
                    ('LEFTPADDING', (0, 0), (-1, -1), 6),
                    ('RIGHTPADDING', (0, 0), (-1, -1), 6),
                    ('TOPPADDING', (0, 0), (-1, -1), 3),
                    ('BOTTOMPADDING', (0, 0), (-1, -1), 3),
                ]))
                elements.append(terms_table)
        
        # Build PDF
        doc.build(elements)
        
        # Get PDF bytes
        pdf_bytes = buffer.getvalue()
        buffer.close()
        
        return pdf_bytes
        
    except ImportError:
        # Fallback to simple PDF generation without ReportLab
        return export_to_pdf_simple(materials, metadata)
    except Exception as e:
        st.error(f"PDF generation error: {str(e)}")
        # Return text version as fallback
        return export_to_text(materials).encode('utf-8')


def export_to_pdf_simple(materials: dict, metadata: dict = None) -> bytes:
    """
    Simple PDF export without external dependencies.
    Creates a basic PDF using Python's built-in capabilities.
    """
    try:
        # Create a simple PDF-like text format
        from datetime import datetime
        
        lines = []
        lines.append("%PDF-1.4")
        lines.append("% Simple Study Guide PDF")
        lines.append("")
        
        # Add content as simple text
        lines.append("STUDY GUIDE")
        lines.append("=" * 50)
        lines.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        lines.append("")
        
        # Summary
        if materials.get('summary'):
            lines.append("SUMMARY")
            lines.append("-" * 20)
            for i, point in enumerate(materials['summary'], 1):
                lines.append(f"{i}. {point}")
            lines.append("")
        
        # Flashcards
        if materials.get('flashcards'):
            lines.append("FLASHCARDS")
            lines.append("-" * 20)
            for card in materials['flashcards']:
                lines.append(f"Q: {card['question']}")
                lines.append(f"A: {card['answer']}")
                lines.append("")
        
        # Quiz
        if materials.get('quiz'):
            lines.append("QUIZ QUESTIONS")
            lines.append("-" * 20)
            for i, q in enumerate(materials['quiz'], 1):
                lines.append(f"{i}. {q['question']}")
                for option in q['options']:
                    lines.append(f"   {option}")
                lines.append(f"   Answer: {q['correct']}")
                lines.append("")
        
        # Key terms
        if materials.get('key_terms'):
            lines.append("KEY TERMS")
            lines.append("-" * 20)
            for term in materials['key_terms']:
                lines.append(f"{term['term']}: {term['definition']}")
                lines.append("")
        
        # Convert to bytes
        content = '\n'.join(lines)
        return content.encode('utf-8')
        
    except Exception as e:
        # Final fallback
        return f"Error creating PDF: {str(e)}".encode('utf-8')
def export_to_anki(materials: dict, deck_name: str = "TuoKit Study Guide", tags: list = None) -> bytes:
    """
    Export study materials as Anki deck (.apkg format).
    Creates cards from flashcards, quiz questions, and key terms.
    """
    try:
        # Try using genanki library for proper .apkg export
        import genanki
        import random
        
        # Generate unique IDs for deck and model
        deck_id = random.randrange(1 << 30, 1 << 31)
        model_id = random.randrange(1 << 30, 1 << 31)
        
        # Create Anki model (card template)
        model = genanki.Model(
            model_id,
            'TuoKit Basic Model',
            fields=[
                {'name': 'Question'},
                {'name': 'Answer'},
                {'name': 'Extra'},
            ],
            templates=[
                {
                    'name': 'Card 1',
                    'qfmt': '{{Question}}',
                    'afmt': '{{FrontSide}}<hr id="answer">{{Answer}}<br><br><small>{{Extra}}</small>',
                },
            ],
            css='''
                .card {
                    font-family: arial;
                    font-size: 20px;
                    text-align: center;
                    color: black;
                    background-color: white;
                }
                .extra {
                    font-size: 14px;
                    color: #666;
                    margin-top: 10px;
                }
            '''
        )
        
        # Create deck
        deck = genanki.Deck(deck_id, deck_name)
        
        # Default tags
        if not tags:
            tags = ['tuokit', 'study_guide']
        
        # Add flashcards
        for card in materials.get('flashcards', []):
            note = genanki.Note(
                model=model,
                fields=[
                    card['question'],
                    card['answer'],
                    'Type: Flashcard'
                ],
                tags=tags + ['flashcard']
            )
            deck.add_note(note)
        
        # Add quiz questions as cards
        for q in materials.get('quiz', []):
            # Format options
            options_text = '<br>'.join(q['options'])
            answer_text = f"{options_text}<br><br><b>Correct: {q['correct']}</b>"
            
            note = genanki.Note(
                model=model,
                fields=[
                    q['question'],
                    answer_text,
                    'Type: Quiz Question'
                ],
                tags=tags + ['quiz']
            )
            deck.add_note(note)
        
        # Add key terms
        for term in materials.get('key_terms', []):
            note = genanki.Note(
                model=model,
                fields=[
                    f"What is {term['term']}?",
                    term['definition'],
                    'Type: Key Term'
                ],
                tags=tags + ['key_term']
            )
            deck.add_note(note)
        
        # Create package and export
        package = genanki.Package(deck)
        
        # Save to bytes
        from io import BytesIO
        buffer = BytesIO()
        package.write_to_file(buffer)
        buffer.seek(0)
        return buffer.read()
        
    except ImportError:
        # Fallback to simple text format for Anki import
        return export_to_anki_text(materials, deck_name, tags)
    except Exception as e:
        st.warning(f"Anki export error: {str(e)}. Using text format instead.")
        return export_to_anki_text(materials, deck_name, tags)


def export_to_anki_text(materials: dict, deck_name: str = "TuoKit Study Guide", tags: list = None) -> bytes:
    """
    Export study materials in Anki-compatible text format.
    Tab-separated format that can be imported into Anki.
    """
    try:
        lines = []
        
        # Add deck comment
        lines.append(f"#deck:{deck_name}")
        lines.append("#separator:tab")
        lines.append("#html:true")
        lines.append("")
        
        # Default tags
        tag_str = ' '.join(tags) if tags else 'tuokit study_guide'
        
        # Add flashcards
        for card in materials.get('flashcards', []):
            # Escape tabs and newlines
            question = card['question'].replace('\t', ' ').replace('\n', '<br>')
            answer = card['answer'].replace('\t', ' ').replace('\n', '<br>')
            lines.append(f"{question}\t{answer}\t{tag_str} flashcard")
        
        # Add quiz questions
        for q in materials.get('quiz', []):
            question = q['question'].replace('\t', ' ').replace('\n', '<br>')
            # Format answer with options
            options_html = '<br>'.join(f"{opt}" for opt in q['options'])
            answer = f"{options_html}<br><br><b>Correct: {q['correct']}</b>"
            answer = answer.replace('\t', ' ')
            lines.append(f"{question}\t{answer}\t{tag_str} quiz")
        
        # Add key terms
        for term in materials.get('key_terms', []):
            question = f"What is {term['term']}?"
            answer = term['definition'].replace('\t', ' ').replace('\n', '<br>')
            lines.append(f"{question}\t{answer}\t{tag_str} key_term")
        
        # Add reverse cards for key terms (definition -> term)
        for term in materials.get('key_terms', []):
            question = term['definition'].replace('\t', ' ').replace('\n', '<br>')
            answer = term['term']
            lines.append(f"{question}\t{answer}\t{tag_str} key_term reverse")
        
        # Convert to bytes
        content = '\n'.join(lines)
        return content.encode('utf-8')
        
    except Exception as e:
        return f"Error creating Anki export: {str(e)}".encode('utf-8')


def create_anki_media_package(materials: dict, media_files: dict = None) -> bytes:
    """
    Create Anki package with media files (images, audio).
    Enhances cards with multimedia content if available.
    """
    try:
        import genanki
        import random
        from io import BytesIO
        
        # Generate unique IDs
        deck_id = random.randrange(1 << 30, 1 << 31)
        model_id = random.randrange(1 << 30, 1 << 31)
        
        # Create enhanced model with media support
        model = genanki.Model(
            model_id,
            'TuoKit Media Model',
            fields=[
                {'name': 'Question'},
                {'name': 'Answer'},
                {'name': 'Image'},
                {'name': 'Audio'},
                {'name': 'Extra'},
            ],
            templates=[
                {
                    'name': 'Card 1',
                    'qfmt': '''
                        {{Question}}
                        {{#Image}}<br><img src="{{Image}}">{{/Image}}
                        {{#Audio}}<br>[sound:{{Audio}}]{{/Audio}}
                    ''',
                    'afmt': '''
                        {{FrontSide}}
                        <hr id="answer">
                        {{Answer}}
                        <br><br>
                        <small class="extra">{{Extra}}</small>
                    ''',
                },
            ],
            css='''
                .card {
                    font-family: arial;
                    font-size: 20px;
                    text-align: center;
                    color: black;
                    background-color: white;
                }
                img {
                    max-width: 100%;
                    max-height: 400px;
                }
                .extra {
                    font-size: 14px;
                    color: #666;
                }
            '''
        )
        
        # Create deck
        deck = genanki.Deck(deck_id, "TuoKit Enhanced Study Guide")
        media_files_list = []
        
        # Process materials with media
        for card in materials.get('flashcards', []):
            # Check for associated media
            image_file = media_files.get(f"{card['question']}_image") if media_files else None
            audio_file = media_files.get(f"{card['question']}_audio") if media_files else None
            
            note = genanki.Note(
                model=model,
                fields=[
                    card['question'],
                    card['answer'],
                    image_file or '',
                    audio_file or '',
                    'Enhanced Flashcard'
                ],
                tags=['tuokit', 'enhanced', 'flashcard']
            )
            deck.add_note(note)
            
            # Add media files to package
            if image_file and media_files:
                media_files_list.append(media_files[f"{card['question']}_image_data"])
            if audio_file and media_files:
                media_files_list.append(media_files[f"{card['question']}_audio_data"])
        
        # Create package with media
        package = genanki.Package(deck, media_files=media_files_list)
        
        # Export to bytes
        buffer = BytesIO()
        package.write_to_file(buffer)
        buffer.seek(0)
        return buffer.read()
        
    except Exception as e:
        # Fallback to basic export
        return export_to_anki(materials)
# TODO: Add progress tracking for study sessions
