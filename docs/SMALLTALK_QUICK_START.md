# SmallTalk Tools Quick Start Guide

Welcome to the enhanced SmallTalk development toolkit in TuoKit! This guide will help you get started with the 6 new tools quickly.

## 🚀 5-Minute Quick Start

### Prerequisites
```bash
# 1. Start Ollama
ollama serve

# 2. Pull required models (if not already done)
ollama pull deepseek-coder:6.7b
ollama pull deepseek-r1:6.7b

# 3. Start TuoKit
cd C:/Projects/Tuokit
streamlit run app.py
```

## 🎯 Tool Overview - What to Use When

| Need | Use This Tool | Quick Action |
|------|---------------|--------------|
| Create a new class | 🏗️ **Class Generator** | Describe class → Generate → Copy to image |
| Build a UI | 🎨 **Morphic UI Builder** | Describe UI → Select layout → Generate |
| Create web component | 🌊 **Seaside Generator** | Describe component → Enable features → Generate |
| Improve existing code | 🔧 **Refactoring Assistant** | Paste code → Select technique → Apply |
| Advanced techniques | ✨ **Metaprogramming Helper** | Select task → Generate solution |
| Find code in image | 🔍 **Image Browser** | Enter query → Get instructions |

## 📝 Quick Examples

### 1. Generate Your First Class (30 seconds)
```
1. Open "🏗️ ST Class Generator"
2. Type: "Person with name, age, email"
3. Click "Generate Class"
4. Copy the generated code to VisualWorks
```

### 2. Build a Simple UI (1 minute)
```
1. Open "🎨 Morphic UI Builder"
2. Type: "Simple form with name field and submit button"
3. Select "Vertical" layout
4. Click "Generate UI Code"
5. Copy code and run: MyMorphicUI new openInWorld
```

### 3. Create a Web Form (2 minutes)
```
1. Open "🌊 Seaside Generator"
2. Type: "Contact form with name, email, message"
3. Toggle "Include CSS Styling" ON
4. Click "Generate Component"
5. Register component in Seaside
```

### 4. Refactor Messy Code (1 minute)
```
1. Open "🔧 ST Refactorer"
2. Paste your problematic method
3. Click "Extract Method" button
4. Review the refactored version
5. Apply to your code
```

### 5. Add Logging to a Class (1 minute)
```
1. Open "✨ ST Metaprogramming"
2. Click "Add Logging" task
3. Enter class name: "MyDomainClass"
4. Click "Generate Metaprogramming Code"
5. Execute in workspace
```

### 6. Find All Senders of a Method (30 seconds)
```
1. Open "🔍 Image Browser"
2. Select "Find Senders" from dropdown
3. Type method name: "#add:"
4. Click "Search Image"
5. Follow the instructions
```

## 💡 Pro Tips

### For Beginners
- Start with **Class Generator** - it teaches proper SmallTalk structure
- Use **Image Browser** to learn navigation shortcuts
- Try simple examples first

### For Intermediate Users
- Combine tools: Generate class → Build UI → Create tests
- Use **Refactoring Assistant** on your existing code
- Explore **Morphic Builder** layouts

### For Advanced Users
- Master **Metaprogramming Helper** for framework building
- Create DSLs with the DSL Builder tab
- Generate browser automation scripts

## 🔗 Workflow Examples

### Complete Application Workflow
```
1. Class Generator: Create domain model
   → "Order with items, total, customer"

2. Morphic Builder: Create UI
   → "Order entry form with item list"

3. Refactoring: Clean up generated code
   → Apply "Extract Method" where needed

4. Snippets: Save reusable parts
   → Store your validated patterns
```

### Web Application Workflow
```
1. Class Generator: Create model classes
   → "User with authentication"

2. Seaside Generator: Create components
   → "Login form with validation"
   → "User dashboard"

3. Metaprogramming: Add features
   → "Add caching to queries"
```

## 🎓 Learning Path

### Week 1: Basics
- Day 1-2: Class Generator - Create 5 different classes
- Day 3-4: Morphic Builder - Build 3 different UIs
- Day 5-7: Image Browser - Master navigation

### Week 2: Intermediate
- Day 1-2: Refactoring - Try all techniques
- Day 3-4: Seaside - Build a small web app
- Day 5-7: Combine tools for a project

### Week 3: Advanced
- Day 1-3: Metaprogramming - All tasks
- Day 4-5: Create a DSL
- Day 6-7: Build something complex

## 🆘 Troubleshooting

### "Ollama not responding"
```bash
# Check if running
ollama list

# Restart if needed
ollama serve
```

### "Model not found"
```bash
# Pull the model
ollama pull deepseek-coder:6.7b
```

### "Generation failed"
- Check your description is clear
- Try simpler input first
- Check Ollama logs

### "Code doesn't work in VisualWorks"
- Verify VisualWorks version compatibility
- Check for missing dependencies
- Use Image Browser to find examples

## 📚 Resources

### In TuoKit
- Use **SmallTalk Explainer** to understand generated code
- Check **SmallTalk Snippets** for patterns
- Browse **Knowledge Library** for saved examples

### External
- VisualWorks documentation
- Pharo MOOC (free online course)
- SmallTalk books and tutorials

## 🎉 Next Steps

1. **Try each tool** at least once
2. **Save useful patterns** to your library
3. **Combine tools** for complex tasks
4. **Share feedback** on what works best

Remember: These tools are here to accelerate your SmallTalk development, not replace understanding. Use them to learn and be more productive!

---

Happy SmallTalk coding! 🚀
