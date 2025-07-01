# 🎥 TuoKit Demo Script  
**Perfect for showcasing capabilities to your team**

Duration: 15 minutes  
Audience: Developers, Technical Leaders  

---

## Phase 1: Introduction (2 min)

"Today I'll showcase TuoKit - our local AI development suite. It combines:
- Code understanding/generation with DeepSeek-Coder
- Document intelligence with DeepSeek-R1  
- All running privately on our infrastructure"

**Key Points:**
- 100% local - no cloud dependencies
- Knowledge persists in PostgreSQL
- Built for developer productivity

---

## Phase 2: Core Features (10 min)

### Dashboard Walkthrough (1 min)
1. Show system status - Ollama ✅ Running
2. Highlight recent activity feed
3. Demonstrate model switching

### Code Tools Demo (3 min)
"Let's solve a real problem:"

```python
# Paste buggy code:
def calculate_discount(price, discount):
    return price - discount
    
# Error: Doesn't handle percentage discounts
```

**Actions:**
1. Click "Explain Code" → Understand the issue
2. Click "Debug Code" → Get corrected version
3. Save solution to Knowledge Base

### Document Tools Demo (3 min)
1. Upload sample PDF (research paper or technical doc)
2. Generate summary → Show key points extraction
3. Ask specific question: "What methodology was used?"
4. Extract structured knowledge → Show JSON output

### Knowledge Library Demo (3 min)
1. Search for saved discount calculation
2. Show category filtering
3. Edit to add notes
4. Export entire knowledge base

---

## Phase 3: Technical Highlights (2 min)

### Architecture
- "Streamlit for instant deployment"
- "PostgreSQL for knowledge persistence"
- "Ollama for local AI processing"

### Security & Privacy
- "All data stays local - no cloud dependencies"
- "No API keys or external services required"
- "Complete control over your AI models"

### Extensibility
- "Add new tools by creating pages/*.py files"
- "Support for any Ollama-compatible model"
- "Export knowledge for team sharing"

---

## Phase 4: Q&A (1 min)

Common questions to prepare for:
- "What models does it support?" → Any Ollama model
- "Can teams use this?" → Yes, each member runs their instance
- "How much does it cost?" → Free, just hardware costs
- "Can we customize it?" → Yes, fully open architecture

---

## 🔍 Demo Preparation Checklist

### Reset for Clean Demo
```bash
# Clear previous data (optional)
psql -U ollama_user -d ollama_knowledge -c "TRUNCATE queries, knowledge_units CASCADE;"

# Add sample knowledge
psql -U ollama_user -d ollama_knowledge -f sample_knowledge_data.sql
```

### Files to Prepare
1. **buggy_code.py** - Function with subtle bug
   ```python
   def calculate_discount(price, discount):
       # Bug: assumes discount is dollar amount, not percentage
       return price - discount
   ```

2. **sample_document.pdf** - Technical paper or meeting notes

3. **error_example.txt** - Python traceback
   ```
   Traceback (most recent call last):
     File "app.py", line 42, in process
       result = calculate_discount(100, 20)
   ValueError: Discount calculation incorrect
   ```

### Pre-Demo Checks
- [ ] Ollama running: `ollama list`
- [ ] Database connected: `psql -U ollama_user -d ollama_knowledge -c "SELECT 1;"`
- [ ] TuoKit accessible: http://localhost:8501
- [ ] Sample files ready
- [ ] Knowledge Library has some entries

---

## 📊 Demo Flow Variations

### Short Demo (5 min)
1. Dashboard status check
2. One Code Tools example
3. Knowledge Library search

### Extended Demo (30 min)
1. Full walkthrough of all tools
2. Help system exploration
3. Database backup/restore
4. Custom model addition

### Technical Deep Dive (45 min)
1. Architecture discussion
2. Database schema review
3. Extension development
4. Production deployment options

---

## 🎯 Key Messages

### For Developers
- "Accelerate your coding with AI assistance"
- "Never lose a solution - everything is saved"
- "Works with your existing workflow"

### For Managers
- "Increase team productivity"
- "Build institutional knowledge"
- "Zero cloud costs or privacy concerns"

### For Security Teams
- "Completely air-gapped if needed"
- "No data leaves your infrastructure"
- "Full audit trail in PostgreSQL"

---

## 📈 Post-Demo Actions

### Immediate
1. Share access instructions
2. Provide quick start guide
3. Schedule follow-up training

### Within 1 Week
1. Gather usage metrics
2. Collect feedback
3. Plan customizations

### Long-term
1. Build shared knowledge base
2. Integrate with CI/CD
3. Expand to other teams

---

## 🌟 Demo Success Metrics

- [ ] Audience understands local AI benefits
- [ ] At least one "wow" moment per tool
- [ ] Questions indicate interest, not confusion
- [ ] Request for installation help
- [ ] Discussion of use cases

**Remember:** The best demo shows real problems being solved!

---

*"TuoKit - Your AI pair programmer that never leaves home"*