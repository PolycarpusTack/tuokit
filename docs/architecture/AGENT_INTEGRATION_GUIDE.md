# TuoKit Agent Integration Guide
Quick guide to integrate awesome-llm-apps agents into TuoKit

## 🚀 Quick Start

### 1. Install Additional Dependencies
```bash
# For advanced agents
pip install agno lancedb qdrant-client duckdb

# For web research
pip install firecrawl-py

# For sandboxed execution
pip install e2b
```

### 2. Add System Architect Agent (Highest Priority)

Copy `system_architect_integration.py` to your pages folder:
```bash
cp system_architect_integration.py pages/system_architect.py
```

Then add to your navigation:
```python
# In navigation.py or app.py
pages = {
    "System Architect": "pages/system_architect.py",
    # ... other pages
}
```

### 3. Enhance Knowledge System with RAG

Replace basic knowledge capture with vector-based RAG:

```python
# In utils/knowledge_enhanced.py
from agno.embedder.ollama import OllamaEmbedder
from agno.vectordb.qdrant import Qdrant
from agno.knowledge.base import Knowledge

class EnhancedKnowledgeSystem:
    def __init__(self):
        # Use Ollama embeddings (local)
        self.embedder = OllamaEmbedder(model="nomic-embed-text")
        
        # Local Qdrant instance
        self.vector_db = Qdrant(
            collection="tuokit_knowledge",
            url="http://localhost:6333/",
            embedder=self.embedder
        )
        
        # Knowledge base
        self.knowledge = Knowledge(
            sources=[],  # Add sources dynamically
            vector_db=self.vector_db
        )
    
    def add_knowledge(self, content: str, metadata: dict):
        """Add new knowledge with vector embedding"""
        self.knowledge.add_document(
            content=content,
            metadata=metadata
        )
    
    def search(self, query: str, top_k: int = 5):
        """Vector similarity search"""
        return self.knowledge.search(query, top_k=top_k)
```

### 4. Add Multi-Agent Orchestration

Enhance agent_hub.py with multi-agent patterns:

```python
# In agent_hub_enhanced.py
class MultiAgentOrchestrator:
    """Orchestrate multiple specialist agents"""
    
    def __init__(self):
        self.agents = {
            "architect": SystemArchitectAgent(),
            "researcher": ResearchAgent(),
            "coder": EnhancedCodeAgent(),
            "data": DataAnalysisAgent()
        }
    
    def execute_complex_goal(self, goal: str):
        """Break down goal and assign to specialists"""
        # 1. Architect designs the system
        architecture = self.agents["architect"].analyze(goal)
        
        # 2. Researcher gathers additional info
        research = self.agents["researcher"].research(
            architecture["technology_stack"]
        )
        
        # 3. Coder implements based on architecture
        implementation = self.agents["coder"].generate(
            architecture=architecture,
            research=research
        )
        
        return {
            "architecture": architecture,
            "research": research,
            "implementation": implementation
        }
```

### 5. Quick Integration Snippets

#### A. Add Deep Research to Any Tool
```python
# In any tool file
from advanced_ai_agents.single_agent_apps.ai_deep_research_agent import research_topic

def enhance_with_research(topic: str):
    """Add research capability to any tool"""
    research_results = research_topic(topic)
    return research_results
```

#### B. Add Reasoning Transparency
```python
# In agent implementations
from agno.tools.reasoning import ReasoningTools

class TransparentAgent(BaseAgent):
    def __init__(self):
        super().__init__(
            name="Transparent Agent",
            tools=[ReasoningTools(add_instructions=True)]
        )
    
    def execute_with_reasoning(self, task: str):
        """Show reasoning process"""
        # Reasoning will be visible in the response
        return self.run(task, show_full_reasoning=True)
```

#### C. Add Vision Capabilities
```python
# For processing screenshots/diagrams
import google.generativeai as genai

def analyze_code_image(image_path: str):
    """Analyze code screenshots"""
    genai.configure(api_key=os.getenv("GEMINI_API_KEY"))
    
    model = genai.GenerativeModel('gemini-2.0-flash')
    image = genai.upload_file(image_path)
    
    response = model.generate_content([
        "Analyze this code image and explain what it does",
        image
    ])
    
    return response.text
```

## 📋 Integration Checklist

### Week 1: Foundation
- [ ] Install agno and vector DB dependencies
- [ ] Set up Qdrant locally (docker run -p 6333:6333 qdrant/qdrant)
- [ ] Integrate System Architect agent
- [ ] Test with sample architecture requirements

### Week 2: Knowledge Enhancement  
- [ ] Implement vector-based knowledge search
- [ ] Add reasoning transparency to agents
- [ ] Integrate research capabilities
- [ ] Test knowledge retrieval accuracy

### Week 3: Multi-Agent Features
- [ ] Implement multi-agent orchestration
- [ ] Add agent collaboration patterns
- [ ] Create complex goal workflows
- [ ] Test end-to-end scenarios

### Week 4: Advanced Features
- [ ] Add vision capabilities for screenshots
- [ ] Implement sandboxed code execution
- [ ] Add streaming responses with progress
- [ ] Performance optimization

## 🔧 Configuration

### Environment Variables
```bash
# .env file additions
QDRANT_URL=http://localhost:6333
FIRECRAWL_API_KEY=your_key_here  # For research agent
E2B_API_KEY=your_key_here  # For code execution
GEMINI_API_KEY=your_key_here  # For vision features
```

### Local Services Setup
```bash
# Start Qdrant
docker run -p 6333:6333 -v ./qdrant_storage:/qdrant/storage qdrant/qdrant

# Verify Ollama has embedding model
ollama pull nomic-embed-text
```

## 🎯 Expected Results

After integration, TuoKit will have:
1. **Professional architecture planning** with structured outputs
2. **Vector-based knowledge search** for faster retrieval  
3. **Multi-agent collaboration** for complex tasks
4. **Research capabilities** for documentation
5. **Reasoning transparency** for user trust
6. **Vision processing** for screenshots

## 💡 Pro Tips

1. **Start Small**: Begin with System Architect agent
2. **Test Locally**: All agents can work with Ollama
3. **Gradual Migration**: Don't replace everything at once
4. **Monitor Performance**: Vector search adds overhead
5. **Document Changes**: Keep track of modifications

## 🆘 Troubleshooting

### Issue: Vector DB connection fails
```bash
# Check if Qdrant is running
curl http://localhost:6333/collections

# Restart Qdrant
docker restart qdrant
```

### Issue: Embeddings too slow
```python
# Use smaller embedding model
embedder = OllamaEmbedder(model="all-minilm")  # Faster
```

### Issue: Agent responses generic
```python
# Add more specific prompts
agent.system_prompt += "\nBe specific and provide code examples"
```

## 📚 Resources

- [Agno Documentation](https://docs.agno.com)
- [Qdrant Quick Start](https://qdrant.tech/documentation/quick-start/)
- [awesome-llm-apps Repository](https://github.com/Shubhamsaboo/awesome-llm-apps)

Start with the System Architect agent and gradually add more capabilities!
