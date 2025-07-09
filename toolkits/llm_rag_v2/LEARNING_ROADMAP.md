# RAG Learning Implementation Roadmap

## 🎯 Goal: Transform Static Search into Learning System

### Phase 1: Basic Feedback Collection (1-2 days)
```python
# 1. Add feedback buttons to search results
def add_feedback_ui(result_id: str):
    col1, col2 = st.columns([1, 1])
    with col1:
        if st.button("👍", key=f"helpful_{result_id}"):
            rag.mark_helpful(result_id)
    with col2:
        if st.button("👎", key=f"not_helpful_{result_id}"):
            rag.mark_not_helpful(result_id)

# 2. Create feedback tables
CREATE TABLE rag_feedback (
    id SERIAL PRIMARY KEY,
    query TEXT NOT NULL,
    result_id TEXT NOT NULL,
    helpful BOOLEAN NOT NULL,
    timestamp TIMESTAMP DEFAULT NOW(),
    session_id TEXT
);

# 3. Store feedback
def mark_helpful(self, result_id: str):
    self.db.execute("""
        INSERT INTO rag_feedback (query, result_id, helpful, session_id)
        VALUES (?, ?, ?, ?)
    """, [self.last_query, result_id, True, self.session_id])
```

### Phase 2: Click Tracking (1 day)
```python
# Track which results users actually click/expand
def track_result_interaction(result_id: str, action: str):
    """Track: view_source, copy_code, expand_result"""
    pass

# Add to UI
if st.expander(f"Source: {result['source_path']}"):
    rag.track_interaction(result['id'], 'expand_source')
    st.code(result['content'])
```

### Phase 3: Ranking Improvements (2-3 days)
```python
class LearningReranker:
    def __init__(self, base_reranker):
        self.base_reranker = base_reranker
        self.feedback_weights = {}
    
    def rerank_with_learning(self, query: str, results: List[Dict]):
        # Get base scores
        results = self.base_reranker.rerank(query, results)
        
        # Apply learned adjustments
        for result in results:
            # Boost score if similar queries found this helpful
            boost = self.get_learned_boost(query, result)
            result['score'] *= (1 + boost)
        
        return sorted(results, key=lambda x: x['score'], reverse=True)
    
    def update_from_feedback(self):
        """Periodic job to learn from feedback"""
        # Analyze which documents are helpful for which queries
        # Update feedback_weights
        pass
```

### Phase 4: Session Context (2 days)
```python
class RAGSession:
    def __init__(self, rag_instance):
        self.rag = rag_instance
        self.history = []
        self.context = {}
    
    def ask(self, query: str):
        # Expand query with context
        expanded_query = self.expand_with_context(query)
        
        # Get answer
        answer = self.rag.generate_answer(expanded_query)
        
        # Update context
        self.history.append((query, answer))
        self.extract_entities(answer)
        
        return answer
    
    def expand_with_context(self, query: str):
        """Add context from previous queries"""
        if "it" in query.lower() and self.context.get('last_topic'):
            query = query.replace("it", self.context['last_topic'])
        return query
```

### Phase 5: Auto-Learning Patterns (3-4 days)
```python
class PatternLearner:
    def learn_user_patterns(self):
        """Learn from user's search patterns"""
        patterns = {
            'common_sequences': self.find_query_sequences(),
            'time_patterns': self.find_temporal_patterns(),
            'topic_clusters': self.cluster_related_queries(),
            'code_relationships': self.extract_code_relationships()
        }
        return patterns
    
    def suggest_next_query(self, current_query: str):
        """Predict what user might ask next"""
        patterns = self.load_patterns()
        suggestions = []
        
        # Based on common sequences
        for seq in patterns['common_sequences']:
            if current_query in seq:
                next_idx = seq.index(current_query) + 1
                if next_idx < len(seq):
                    suggestions.append(seq[next_idx])
        
        return suggestions
```

### Phase 6: Knowledge Graph Integration (1 week)
```python
class CodeKnowledgeGraph:
    def __init__(self):
        self.graph = nx.DiGraph()
    
    def build_from_codebase(self, codebase_path: str):
        """Extract relationships from code"""
        for file in self.get_python_files(codebase_path):
            # Extract imports
            imports = self.extract_imports(file)
            # Extract class definitions
            classes = self.extract_classes(file)
            # Extract function calls
            calls = self.extract_function_calls(file)
            
            # Build graph
            self.add_relationships(file, imports, classes, calls)
    
    def enhance_search(self, results: List[Dict]):
        """Add related code based on graph"""
        enhanced = []
        for result in results:
            # Find related nodes in graph
            related = self.graph.neighbors(result['source_path'])
            result['related_files'] = list(related)
            enhanced.append(result)
        return enhanced
```

## 🚀 Quick Wins (Can implement today)

### 1. Query Logging
```python
# In rag_manager.py
def search(self, query: str, **kwargs):
    # Log query
    self._log_query(query)
    # Continue with search...

def _log_query(self, query: str):
    with open("rag_queries.log", "a") as f:
        f.write(f"{datetime.now()}\t{query}\n")
```

### 2. Popular Queries Dashboard
```python
# In the sidebar
st.subheader("🔥 Popular Searches")
popular = rag.get_popular_queries(limit=5)
for query in popular:
    if st.button(query, key=f"pop_{query}"):
        st.session_state.search_query = query
```

### 3. Search Suggestions
```python
# Below search box
suggestions = rag.get_suggestions(partial_query)
if suggestions:
    st.caption("Try these searches:")
    cols = st.columns(len(suggestions))
    for col, suggestion in zip(cols, suggestions):
        with col:
            if st.button(suggestion):
                st.session_state.search_query = suggestion
```

## 📊 Metrics to Track

1. **Usage Metrics**
   - Queries per day
   - Unique users
   - Most searched topics
   - Failed queries (no results)

2. **Quality Metrics**
   - Feedback ratio (helpful/not helpful)
   - Click-through rate on results
   - Time to find answer
   - Query reformulation rate

3. **Learning Metrics**
   - Ranking improvement over time
   - Prediction accuracy
   - Pattern discovery rate
   - Knowledge graph growth

## 🎁 User Benefits

When implemented, users will experience:
1. **Better Results Over Time** - System learns what's helpful
2. **Contextual Understanding** - Follow-up questions work naturally
3. **Proactive Suggestions** - "Users who searched X also searched Y"
4. **Auto-Complete** - Based on common queries
5. **Personalization** - Results tailored to your patterns
6. **Relationship Discovery** - "This code is related to..."

## 💡 Implementation Priority

1. **High Priority** (Do First)
   - Query logging
   - Basic feedback UI
   - Popular queries display

2. **Medium Priority** (Next Sprint)
   - Click tracking
   - Session context
   - Search suggestions

3. **Low Priority** (Future)
   - Knowledge graph
   - Pattern learning
   - Full personalization