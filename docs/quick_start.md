# TuoKit Quick Start Guide

## 🚀 First Time Setup (5 minutes)

### 1. Install Dependencies
```bash
cd C:\Projects\Tuokit
start_tuokit.bat  # This installs everything and starts TuoKit
```

### 2. Verify Installation
The startup script will automatically:
- ✅ Check Ollama connection
- ✅ Verify PDF libraries
- ✅ Launch the dashboard

### 3. Database Setup (Optional but Recommended)
```bash
psql -U postgres -f database_setup.sql
```

## 🎯 Your First TuoKit Session

### Step 1: Check System Status
1. Open http://localhost:8501
2. View the dashboard metrics
3. Ensure Ollama shows "✅ Running"

### Step 2: Try Code Tools
1. Click "💻 Code Tools" in sidebar
2. Paste any code and click "Analyze Code"
3. Save the explanation to Knowledge Base

### Step 3: Process a Document
1. Click "📄 Document Tools"
2. Upload the included `test_document.txt`
3. Try "Summarize" for quick overview
4. Ask "What are the action items?"

### Step 4: Browse Knowledge
1. Click "📚 Knowledge Library"
2. See your saved insights
3. Search, edit, or export as needed

## 💡 Common Workflows

### "Explain This Error" Workflow
1. Go to Code Tools → Debug Code
2. Paste problematic code
3. Enter error message
4. Get solution with fixed code
5. Save to Knowledge Base

### "Document Analysis" Workflow
1. Go to Document Tools
2. Upload PDF or TXT file
3. Generate summary first
4. Ask specific questions
5. Extract structured knowledge

### "Knowledge Reuse" Workflow
1. Go to Knowledge Library
2. Search for previous solutions
3. Click "Copy" for code reuse
4. Or "Export" for documentation

## ⚡ Pro Tips

### Ollama Models
- **deepseek-coder:6.7b** - Best for code
- **deepseek-r1:6.7b** - Best for reasoning
- **deepseek-r1:1.5b** - Faster, lighter

### Effective Prompts
- Be specific: "Fix TypeError in line 15"
- Provide context: "This is a REST API client"
- Ask for format: "Provide as a class with docstrings"

### Knowledge Organization
- Use consistent naming: "[Tool] - [Problem] - [Date]"
- Choose specific categories
- Regular cleanup sessions

## 🛠️ Troubleshooting

### Ollama Not Working
```bash
# Check if running
ollama list

# Start service
ollama serve

# Pull models
ollama pull deepseek-coder:6.7b
```

### Database Issues
```bash
# Test connection
psql -U ollama_user -d ollama_knowledge -c "SELECT 1;"

# Check credentials in .env
cat .env
```

### PDF Processing Failed
```bash
# Test libraries
python test_pdf.py

# Reinstall if needed
pip install --upgrade pypdf2 pymupdf
```

## 📚 Next Steps

1. **Customize Categories**: Edit the category lists in each tool
2. **Add Models**: Pull additional Ollama models
3. **Team Setup**: Share .env.example with your team
4. **Automation**: Create scripts using saved knowledge

## 🎉 You're Ready!
Start building your AI-powered knowledge base with TuoKit!