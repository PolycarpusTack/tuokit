# SmallTalk & Ruby on Rails Development Tools

This document describes the SmallTalk and Ruby on Rails development tools integrated into TuoKit.

## Overview

TuoKit now includes specialized AI-powered tools for SmallTalk and Ruby on Rails developers:

1. **SmallTalk Code Explainer** - Analyzes and explains VisualWorks SmallTalk code
2. **Rails Scaffold Generator** - Generates complete Rails scaffolds
3. **SmallTalk ↔ Ruby Converter** - Converts code between SmallTalk and Ruby
4. **Rails Debugging Assistant** - Analyzes Rails errors and provides solutions
5. **SmallTalk Snippet Finder** - Generates and manages SmallTalk code snippets

## Tool Descriptions

### 🧑‍🏫 SmallTalk Code Explainer (`pages/smalltalk_explainer.py`)

**Purpose:** Help developers understand SmallTalk code with AI-powered explanations

**Features:**
- Analyzes SmallTalk code structure and purpose
- Explains key SmallTalk concepts used in the code
- Provides step-by-step execution flow
- Includes educational resources and quick examples

**Usage:**
1. Navigate to SmallTalk Explainer from the dashboard
2. Paste your SmallTalk code
3. Click "Explain Code" to get detailed analysis

### ⚡ Rails Scaffold Generator (`pages/rails_scaffold.py`)

**Purpose:** Generate complete Rails scaffolds with models, controllers, and views

**Features:**
- Generates Rails 6.0, 6.1, and 7.0 compatible code
- Includes model validations and associations
- Creates complete CRUD controllers
- Generates all necessary views and partials
- Provides Rails commands to implement the scaffold

**Usage:**
1. Navigate to Rails Scaffold Generator
2. Describe your resource (e.g., "Blog post with title:string content:text")
3. Select Rails version and options
4. Click "Generate Scaffold"

### 🔄 SmallTalk ↔ Ruby Converter (`pages/smalltalk_ruby_converter.py`)

**Purpose:** Convert code between SmallTalk and Ruby while maintaining functionality

**Features:**
- Bidirectional conversion (SmallTalk → Ruby, Ruby → SmallTalk)
- Preserves original functionality
- Adds explanatory comments about paradigm differences
- Explains key differences between the languages
- Provides downloadable converted code

**Usage:**
1. Navigate to ST↔Ruby Converter
2. Select conversion direction
3. Paste your source code
4. Click "Convert Code"

### 🐞 Rails Debugging Assistant (`pages/rails_debugger.py`)

**Purpose:** Analyze Rails errors and provide intelligent debugging solutions

**Features:**
- Automatic error type detection
- Root cause analysis
- Step-by-step solutions
- Prevention tips
- Suggested code fixes
- Quick commands for common issues

**Usage:**
1. Navigate to Rails Debugger
2. Paste the Rails error message
3. Optionally add code context
4. Click "Debug Error"

### 📚 SmallTalk Snippet Finder (`pages/smalltalk_snippets.py`)

**Purpose:** Generate and manage reusable SmallTalk code snippets

**Features:**
- 10 predefined snippet categories
- Custom task specification
- Snippet library with search
- Quick reference patterns
- Save and retrieve snippets

**Categories:**
- Collections & Iteration
- GUI Development
- File I/O
- Database Access
- Testing & Debugging
- String Manipulation
- Date & Time
- Network & HTTP
- Exception Handling
- Design Patterns

## Technical Implementation

### Shared Utilities

All tools extend the `OllamaToolBase` class from `utils/ollama.py` which provides:
- Automatic query logging to PostgreSQL
- Error handling and retry logic
- Model management
- Response caching

### Models Used

- **SmallTalk Explainer:** `deepseek-r1:6.7b` (reasoning model)
- **Rails Scaffold:** `deepseek-coder:6.7b` (code generation)
- **Code Converter:** `deepseek-coder:6.7b` (code translation)
- **Rails Debugger:** `deepseek-r1:6.7b` (analysis) + `deepseek-coder:6.7b` (fixes)
- **Snippet Finder:** `deepseek-coder:6.7b` (code generation)

### Database Integration

All tools automatically log queries and responses to the PostgreSQL database:
- Tool name
- Model used
- User prompt
- AI response
- Metadata (timestamps, tokens, duration)

## Setup Instructions

1. **Ensure Ollama is running:**
   ```bash
   ollama serve
   ```

2. **Pull required models:**
   ```bash
   ollama pull deepseek-coder:6.7b
   ollama pull deepseek-r1:6.7b
   ```

3. **Database is configured** (already set up in TuoKit)

4. **Access the tools:**
   - From TuoKit dashboard → SmallTalk & Rails section
   - Or directly via sidebar navigation

## Best Practices

1. **SmallTalk Development:**
   - Use the Explainer for understanding legacy code
   - Generate snippets for common patterns
   - Convert Ruby examples to SmallTalk idioms

2. **Rails Development:**
   - Generate scaffolds for rapid prototyping
   - Debug errors with context for better solutions
   - Save successful debug sessions for future reference

3. **Cross-Language Work:**
   - Use the converter for porting logic between languages
   - Understand paradigm differences through explanations
   - Build a library of equivalent patterns

## Future Enhancements

- [ ] SmallTalk refactoring suggestions
- [ ] Rails performance analyzer
- [ ] SmallTalk to modern framework converter
- [ ] Rails upgrade assistant
- [ ] Integration with version control
- [ ] Collaborative snippet sharing

## Troubleshooting

**Models not found:**
```bash
ollama pull deepseek-coder:6.7b
ollama pull deepseek-r1:6.7b
```

**Database connection issues:**
- Check `.env` file configuration
- Verify PostgreSQL is running
- Check database credentials

**Generation failures:**
- Verify Ollama service is running
- Check model availability
- Review error logs in console

---

These tools follow TuoKit's core principles:
- **Local execution** - All AI processing happens locally
- **Knowledge capture** - All interactions are saved
- **Practical focus** - Real-world development tasks
- **Educational value** - Learn while coding
