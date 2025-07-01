# TuoKit Quick Reference

## 🚀 Starting TuoKit
```bash
# Windows
start_tuokit.bat

# Linux/Mac
./start_tuokit.sh

# Manual start
streamlit run app.py
```

## 🧰 Tool Overview

### 📊 Dashboard
- System monitoring
- Ollama status
- Recent activity
- Quick navigation

### 💻 Code Tools
| Feature | Use Case | Shortcut |
|---------|----------|----------|
| Explain | Understand code logic | Paste → Analyze |
| Debug | Fix errors | Code + Error → Diagnose |
| Generate | Create new code | Description → Generate |

### 📄 Document Tools
| Feature | Use Case | Process |
|---------|----------|---------|
| Q&A | Specific questions | Upload → Ask → Answer |
| Summarize | Quick overview | Upload → Summarize |
| Extract | Structured data | Upload → Extract JSON |

### 📚 Knowledge Library
| Action | Purpose | How |
|--------|---------|-----|
| Search | Find previous work | Type keywords |
| Filter | Narrow results | Select category |
| Copy | Reuse code | Click Copy button |
| Export | Backup/Share | Sidebar → Export |

## 🔧 Common Workflows

### 1. Debug → Save → Reuse
```
1. Code Tools → Debug Code
2. Save to Knowledge (Error Solution)
3. Knowledge Library → Search error
4. Copy → Apply fix
```

### 2. Document → Extract → Export
```
1. Document Tools → Upload PDF
2. Extract Knowledge → Get JSON
3. Save to Knowledge (JSON)
4. Export as structured data
```

### 3. Generate → Test → Save
```
1. Code Tools → Generate Code
2. Test in your IDE
3. Save working version
4. Reuse for similar tasks
```

## 💡 Pro Tips

1. **Naming Convention**: "Feature - Language - Purpose"
   - Example: "API Client - Python - REST Authentication"

2. **Category Selection**:
   - Code Snippet: Small, reusable fragments
   - Algorithm: Complete implementations
   - Error Solution: Debugged code with context

3. **Search Effectively**:
   - Use specific terms
   - Try different variations
   - Filter by category first

4. **Knowledge Management**:
   - Review weekly
   - Delete outdated items
   - Export monthly backups

## ⚡ Keyboard Shortcuts (Application-wide)
- `Ctrl+Enter`: Submit/Execute in tools
- `Esc`: Close expanded knowledge items
- `Tab`: Navigate between fields

## 🔗 Integration Points

### Ollama Models
- `deepseek-coder:6.7b` - Best for code
- `deepseek-r1:6.7b` - Best for documents
- `deepseek-r1:1.5b` - Faster, lighter option

### Database
- PostgreSQL for persistence
- Works without DB (limited features)
- Auto-saves all interactions

### File Support
- Code: `.py`, `.js`, `.sql`, `.sh`
- Docs: `.pdf`, `.txt`
- Export: `.json`, `.md`, `.csv`

---
*Keep this reference handy for maximum productivity with TuoKit!*