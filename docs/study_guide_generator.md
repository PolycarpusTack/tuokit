# Study Guide Generator

## Overview
The Study Guide Generator is a powerful educational tool within TuoKit that transforms any content into comprehensive study materials. It leverages AI to create summaries, flashcards, quizzes, and key term definitions tailored to your learning needs.

## Features

### Input Methods
- **Text Input**: Paste content directly into the text area
- **File Upload**: Support for PDF, TXT, and DOCX files (max 10MB)
- **URL Import**: Extract content from web pages automatically

### Customization Options
- **Difficulty Levels**: Beginner, Intermediate, Advanced
- **Learning Objectives**: 
  - Quick Review (for rapid content overview)
  - Deep Understanding (for comprehensive learning)
  - Exam Preparation (focused on testable knowledge)
- **AI Model Selection**: Choose between different model sizes for speed vs quality

### Generated Materials
1. **Summary**: Bullet-point overview of key concepts
2. **Flashcards**: Q&A pairs for active recall practice
3. **Quiz**: Multiple-choice questions with answers
4. **Key Terms**: Important vocabulary with definitions

### Export Options
- Text file download of all study materials
- Knowledge Library integration for future reference
- Structured format suitable for importing into other study tools

## Usage Guide

### Basic Workflow
1. Select your input method (Text/File/URL)
2. Configure difficulty and learning objective
3. Click "Generate Study Guide"
4. Review materials in organized tabs
5. Export or save to Knowledge Library

### Best Practices
- **Content Length**: Optimal results with 500-5000 words
- **Clear Structure**: Well-organized source content produces better study materials
- **Model Selection**: 
  - Use 1.5b model for quick results
  - Use 6.7b model for higher quality output

### Tips for Different Content Types
- **Technical Documentation**: Set to "Advanced" difficulty for detailed explanations
- **Course Materials**: Use "Exam Preparation" for focused study guides
- **Articles/Books**: "Deep Understanding" works best for comprehensive analysis

## Technical Details

### File Processing
- PDF extraction using PyMuPDF/PyPDF2
- DOCX parsing via XML extraction
- URL content cleaned of HTML/scripts
- Automatic text encoding detection

### AI Processing
- Context-aware prompt engineering
- Structured output parsing
- Fallback handling for malformed responses
- Token limit management (2000 char preview)

### Data Storage
- Content hash for deduplication
- Metadata tracking (difficulty, objective, source)
- PostgreSQL integration via Knowledge Library
- JSON storage of structured materials

## Troubleshooting

### Common Issues
1. **Empty Results**: Check if Ollama is running and model is loaded
2. **Parsing Errors**: Try a different AI model or simplify input
3. **File Upload Fails**: Ensure file is under 10MB and in supported format
4. **URL Extraction Issues**: Some sites may block automated access

### Error Messages
- "Failed to generate content": Ollama connection issue
- "Unsupported file format": Only PDF, TXT, DOCX supported
- "File too large": Reduce file size or extract relevant sections

## Future Enhancements
- NLTK integration for better concept extraction
- Anki deck export functionality
- Spaced repetition scheduling
- PDF export with formatting
- Multi-language support
- Image/diagram extraction from PDFs

## Integration with Other Tools
- **Knowledge Library**: All generated materials can be saved and searched
- **Document Tools**: Process documents before generating study guides
- **Code Tools**: Create programming-focused study materials

## Privacy & Security
- All processing happens locally
- No data sent to external services
- Content hashes used for deduplication only
- User data never leaves your system
