# 🎓 TuoKit Team Onboarding Tutorial

## Welcome to TuoKit!
Your AI-powered development assistant that runs entirely on your local machine.

---

## 🎯 Day 1: First Steps (30 minutes)

### 1. Access Your TuoKit Instance
```bash
# Your team lead will provide:
- TuoKit URL: http://localhost:8501
- Database credentials (if needed)
- Ollama model list
```

### 2. Dashboard Orientation
When you first open TuoKit:

**System Status Panel**
- ✅ Green check = Ollama is running
- Model count shows available AI models
- Knowledge units = your saved insights

**Activity Feed**
- Shows recent AI interactions
- Click any item to see full details

**Navigation**
- Sidebar has all tools
- Help button on every page

### 3. Your First AI Interaction
Try this in Code Tools:
```python
# Paste this code
def greet(name):
    return f"Hello {name}"

# Click "Analyze Code"
# The AI will explain what this function does
```

### 4. Save Your First Knowledge
After the AI explains:
1. Click "Save to Knowledge Base"
2. Give it a title: "Python Greeting Function"
3. Select category: "Code Snippet"
4. Click "Save to Library"

**Congratulations!** You've created your first knowledge unit.

---

## 📚 Day 2: Core Tools Mastery (45 minutes)

### Code Tools Exercises

#### Exercise 1: Debug Code
```python
# This has a bug - paste it
def divide_numbers(a, b):
    return a / b

# Error: ZeroDivisionError when b = 0
```
- Use "Debug Code" to get the fix
- Save the solution

#### Exercise 2: Generate Code
Ask for: "Create a function to check if a number is prime"
- Review generated code
- Test it in your IDE
- Save if useful

### Document Tools Exercises

#### Exercise 1: Document Summary
1. Upload `test_document.txt` (in project folder)
2. Click "Summarize"
3. Review key points extracted

#### Exercise 2: Document Q&A
With the same document:
1. Ask: "What are the action items?"
2. Ask: "What is the project timeline?"
3. Save useful answers

### Knowledge Library Practice

1. **Search**: Find "prime" (from earlier exercise)
2. **Filter**: Show only "Code Snippets"
3. **Edit**: Add a comment to any entry
4. **Export**: Download your knowledge

---

## 🚀 Day 3: Advanced Workflows (30 minutes)

### Workflow 1: Code Review Preparation
1. Paste complex code in Code Tools
2. Get explanation
3. Ask specific questions in a new analysis
4. Compile insights in Knowledge Library

### Workflow 2: Documentation Analysis
1. Upload technical PDF
2. Generate summary first
3. Extract structured knowledge (JSON)
4. Ask targeted questions
5. Build a knowledge document

### Workflow 3: Problem Solving
1. Start with error message in Debug tool
2. Get initial fix
3. Search Knowledge Library for similar issues
4. Combine solutions
5. Document final solution

---

## 💡 Best Practices

### 1. Effective Prompting
**Good Prompts:**
- "Explain the security implications of this code"
- "Generate a Python class for managing user sessions with proper error handling"
- "What design patterns are used in this document?"

**Poor Prompts:**
- "Fix this" (too vague)
- "Make it better" (no context)
- "Help" (no specific ask)

### 2. Knowledge Management
**Naming Convention:**
```
[Date] - [Tool] - [Brief Description]
Example: "2025-01-07 - Code Debug - Fix Async API Timeout"
```

**Categories:**
- Use specific categories consistently
- Create project-specific categories
- Review and clean up weekly

### 3. Model Selection
- **deepseek-coder:6.7b** → Best for code
- **deepseek-r1:6.7b** → Best for analysis
- **deepseek-r1:1.5b** → Faster responses

---

## 🏆 Week 1 Goals

By end of week 1, you should:
- [ ] Have 10+ knowledge units saved
- [ ] Used all three main tools
- [ ] Exported knowledge at least once
- [ ] Solved one real work problem

---

## 🤝 Getting Help

### Within TuoKit
1. Click ❓ Help button on any page
2. Check FAQ section
3. Review tool-specific guides

### From Team
1. Ask your TuoKit champion
2. Share knowledge in team meetings
3. Create shared knowledge categories

### Technical Issues
1. Check if Ollama is running
2. Verify database connection
3. Clear browser cache
4. Restart TuoKit service

---

## 📊 Success Metrics

Track your progress:
- Daily: Number of queries made
- Weekly: Knowledge units created
- Monthly: Problems solved

**Power User Indicators:**
- Uses keyboard shortcuts
- Creates custom categories
- Exports knowledge regularly
- Helps onboard others

---

## 🎯 30-Day Challenge

### Week 1: Learn the Tools
- Master each tool
- Save 20+ knowledge units
- Try all features

### Week 2: Build Workflows
- Create tool combinations
- Develop naming system
- Share with team

### Week 3: Optimize Usage
- Find your favorite models
- Create templates
- Build knowledge library

### Week 4: Become Expert
- Train someone else
- Suggest improvements
- Share success stories

---

## 🌟 Pro Tips from Power Users

1. **Morning Routine**: Check recent activity, review yesterday's knowledge
2. **Before Coding**: Search knowledge for similar problems
3. **After Debugging**: Always save the solution
4. **End of Day**: Export important knowledge
5. **Weekly**: Clean up and categorize

---

## 📝 Quick Reference Card

### Keyboard Shortcuts (Future)
- `Ctrl+K` → Quick search
- `Ctrl+S` → Save to knowledge
- `Ctrl+/` → Help
- `Esc` → Close dialog

### Essential URLs
- Dashboard: `/`
- Code Tools: `/code_tools`
- Doc Tools: `/doc_tools`
- Knowledge: `/knowledge_lib`
- Help: `/help_guide`

### Emergency Commands
```bash
# Restart Ollama
ollama serve

# Check status
ollama list

# Reset UI
Ctrl+Shift+R (browser refresh)
```

---

## 🎉 Welcome to the Team!

You're now equipped to use TuoKit effectively. Remember:
- It's your personal AI assistant
- Everything stays on your machine
- The more you save, the more valuable it becomes
- Share knowledge with your team

**Happy coding with TuoKit!** 🚀