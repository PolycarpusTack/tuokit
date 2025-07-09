# Self-Learning Crash Knowledge System Design

## Vision
Transform the Crash Analyzer from a static pattern matcher into an intelligent system that learns from every crash, builds relationships, and provides predictive insights.

## Architecture Overview

```
┌─────────────────────┐     ┌──────────────────┐     ┌────────────────┐
│   Crash Analyzer    │────▶│ Pattern Learning │────▶│ Crash Knowledge│
│   (Enhanced)        │     │     Engine       │     │    Database    │
└─────────────────────┘     └──────────────────┘     └────────────────┘
           │                          │                        │
           ▼                          ▼                        ▼
┌─────────────────────┐     ┌──────────────────┐     ┌────────────────┐
│ Validation & Review │     │ Similarity Engine│     │  Crash Graph   │
│    Workflow         │     │  (Embeddings)    │     │ Visualization  │
└─────────────────────┘     └──────────────────┘     └────────────────┘
           │                          │                        │
           ▼                          ▼                        ▼
┌─────────────────────┐     ┌──────────────────┐     ┌────────────────┐
│ Knowledge Capture   │     │ Trend Analysis & │     │Business Impact │
│   Integration       │     │   Predictions    │     │   Dashboard    │
└─────────────────────┘     └──────────────────┘     └────────────────┘
```

## Core Components

### 1. Crash Pattern Learning Engine

```python
class CrashPatternLearner:
    """Learns new patterns from validated crash analyses"""
    
    def __init__(self):
        self.pattern_db = CrashPatternDatabase()
        self.similarity_engine = CrashSimilarityEngine()
        self.validator = PatternValidator()
    
    def learn_from_crash(self, crash_data, analysis, validation):
        # Generate crash fingerprint
        fingerprint = self.generate_fingerprint(crash_data)
        
        # Check if this is a new pattern
        similarity_matches = self.similarity_engine.find_similar(
            crash_data, 
            threshold=0.85
        )
        
        if not similarity_matches:
            # New pattern discovered!
            pattern = self.create_new_pattern(
                crash_data, 
                analysis, 
                validation
            )
            self.pattern_db.add_pattern(pattern)
            
        else:
            # Update existing pattern
            self.update_pattern_knowledge(
                similarity_matches[0], 
                crash_data,
                analysis
            )
```

### 2. Crash Fingerprinting System

```python
def generate_crash_fingerprint(crash_content: str) -> Dict:
    """Generate multi-level fingerprint for crash identification"""
    
    # Level 1: Exact match fingerprint
    cause = extract_wcr_cause(crash_content)
    first_frames = extract_stack_frames(crash_content, limit=3)
    exact_fingerprint = hashlib.sha256(
        f"{cause}{''.join(first_frames)}".encode()
    ).hexdigest()
    
    # Level 2: Structural fingerprint (method names only)
    methods = [extract_method_name(frame) for frame in first_frames]
    structural_fingerprint = hashlib.sha256(
        ''.join(methods).encode()
    ).hexdigest()
    
    # Level 3: Semantic embedding
    embedding = generate_semantic_embedding(crash_content)
    
    return {
        'exact': exact_fingerprint,
        'structural': structural_fingerprint,
        'semantic': embedding,
        'metadata': extract_crash_metadata(crash_content)
    }
```

### 3. Crash Knowledge Database Schema

```sql
-- Enhanced crash patterns table
CREATE TABLE crash_patterns (
    id SERIAL PRIMARY KEY,
    fingerprint_exact VARCHAR(64) UNIQUE,
    fingerprint_structural VARCHAR(64),
    pattern_name VARCHAR(255),
    error_type VARCHAR(100),
    
    -- WCR specific fields
    cause_of_dump TEXT,
    stack_signature TEXT,
    
    -- Learning metrics
    occurrence_count INTEGER DEFAULT 1,
    first_seen TIMESTAMP,
    last_seen TIMESTAMP,
    
    -- Pattern quality
    confidence_score FLOAT DEFAULT 0.5,
    validation_count INTEGER DEFAULT 0,
    false_positive_count INTEGER DEFAULT 0,
    
    -- Solution tracking
    verified_solutions JSONB,
    prevention_strategies JSONB,
    avg_resolution_time INTERVAL,
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Crash instances table
CREATE TABLE crash_instances (
    id SERIAL PRIMARY KEY,
    pattern_id INTEGER REFERENCES crash_patterns(id),
    
    -- Instance details
    filename TEXT,
    file_hash VARCHAR(32),
    crash_timestamp TIMESTAMP,
    
    -- WCR metadata
    whatson_version VARCHAR(50),
    site_name VARCHAR(100),
    user_name VARCHAR(100),
    oracle_version VARCHAR(50),
    
    -- Environment
    environment JSONB,  -- {memory, disk, process_count, etc}
    
    -- Analysis results
    analysis JSONB,
    expert_report TEXT,
    
    -- Validation
    validated_by VARCHAR(100),
    validation_timestamp TIMESTAMP,
    validation_notes TEXT,
    
    -- Business impact
    downtime_minutes INTEGER,
    affected_features TEXT[],
    severity_rating VARCHAR(20),
    
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Crash relationships table
CREATE TABLE crash_relationships (
    id SERIAL PRIMARY KEY,
    source_pattern_id INTEGER REFERENCES crash_patterns(id),
    target_pattern_id INTEGER REFERENCES crash_patterns(id),
    relationship_type VARCHAR(50), -- 'causes', 'related_to', 'evolves_from'
    confidence FLOAT DEFAULT 0.5,
    evidence JSONB,
    
    UNIQUE(source_pattern_id, target_pattern_id, relationship_type)
);

-- Pattern evolution tracking
CREATE TABLE pattern_evolution (
    id SERIAL PRIMARY KEY,
    pattern_id INTEGER REFERENCES crash_patterns(id),
    version INTEGER,
    changes JSONB,
    learned_from_instance_id INTEGER REFERENCES crash_instances(id),
    change_type VARCHAR(50), -- 'new_solution', 'pattern_update', 'merge'
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### 4. Intelligent Pattern Matching

```python
class SmartCrashMatcher:
    """Matches crashes using multiple strategies"""
    
    def match_crash(self, crash_content: str) -> CrashMatch:
        # 1. Try exact fingerprint match (fastest)
        exact_match = self.exact_match(crash_content)
        if exact_match and exact_match.confidence > 0.95:
            return exact_match
        
        # 2. Try structural match (method signatures)
        structural_match = self.structural_match(crash_content)
        if structural_match and structural_match.confidence > 0.85:
            return structural_match
        
        # 3. Semantic similarity search
        semantic_matches = self.semantic_search(crash_content, top_k=5)
        if semantic_matches:
            # Use ensemble voting
            best_match = self.ensemble_vote(semantic_matches)
            if best_match.confidence > 0.75:
                return best_match
        
        # 4. Fuzzy pattern matching
        fuzzy_match = self.fuzzy_match(crash_content)
        if fuzzy_match and fuzzy_match.confidence > 0.70:
            return fuzzy_match
        
        # No match - new pattern!
        return CrashMatch(is_new=True)
```

### 5. Crash Graph & Relationships

```python
class CrashGraphBuilder:
    """Builds relationships between crash patterns"""
    
    def discover_relationships(self):
        patterns = self.get_all_patterns()
        
        for p1, p2 in combinations(patterns, 2):
            # Check temporal relationships
            if self.occurs_together(p1, p2):
                self.add_relationship(p1, p2, 'co_occurs')
            
            # Check causal relationships
            if self.causes_pattern(p1, p2):
                self.add_relationship(p1, p2, 'causes')
            
            # Check evolution
            if self.evolves_from(p1, p2):
                self.add_relationship(p1, p2, 'evolves_from')
            
            # Check solution similarity
            if self.similar_solutions(p1, p2):
                self.add_relationship(p1, p2, 'similar_fix')
```

### 6. Business Analytics Dashboard

```python
class CrashAnalyticsDashboard:
    """Provides business insights from crash data"""
    
    def generate_insights(self):
        return {
            # Trend Analysis
            'crash_trends': self.analyze_crash_trends(),
            'emerging_patterns': self.detect_emerging_patterns(),
            'stability_score': self.calculate_stability_score(),
            
            # Impact Analysis
            'top_impacting_crashes': self.get_high_impact_crashes(),
            'downtime_by_pattern': self.calculate_downtime_metrics(),
            'affected_sites': self.analyze_site_impacts(),
            
            # Version Analysis
            'version_stability': self.analyze_version_stability(),
            'regression_alerts': self.detect_regressions(),
            
            # Predictive Insights
            'crash_forecast': self.forecast_next_week(),
            'risk_areas': self.identify_risk_areas(),
            'recommended_fixes': self.prioritize_fixes()
        }
```

## Integration with Existing Systems

### 1. Enhanced Crash Analyzer UI

```python
# In crash_analyzer.py
def show():
    # ... existing code ...
    
    # After analysis completes
    if st.session_state.analysis:
        # Check if this is a known pattern
        pattern_match = smart_crash_matcher.match_crash(content)
        
        if pattern_match.is_new:
            st.warning("🆕 New crash pattern detected!")
            st.info("This appears to be a new type of error. Your validation will help improve future analyses.")
        else:
            st.success(f"✅ Matched known pattern: {pattern_match.pattern_name}")
            st.metric("Confidence", f"{pattern_match.confidence:.1%}")
            
            # Show historical insights
            with st.expander("📊 Pattern History"):
                show_pattern_history(pattern_match.pattern_id)
            
            # Show proven solutions
            if pattern_match.verified_solutions:
                st.subheader("🔧 Proven Solutions")
                for solution in pattern_match.verified_solutions:
                    st.write(f"• {solution['description']} (worked {solution['success_count']} times)")
```

### 2. Validation Workflow Integration

```python
# Enhanced save function
def save_and_learn_from_crash(filename, content, analysis, validation_data):
    # Save to crash instances
    instance_id = save_crash_instance(
        filename=filename,
        content=content,
        analysis=analysis,
        metadata=extract_wcr_metadata(content),
        validator=validation_data['validator_name'],
        impact=validation_data['business_impact']
    )
    
    # Learn from this crash
    pattern_learner.learn_from_crash(
        crash_data=content,
        analysis=analysis,
        validation=validation_data
    )
    
    # Update relationships
    crash_graph_builder.update_relationships(instance_id)
    
    # Capture to knowledge base
    capture_crash_knowledge(
        instance_id=instance_id,
        pattern_id=pattern_id,
        quality_score=calculate_quality_score(analysis, validation_data)
    )
```

### 3. Crash Knowledge Graph Viewer

```python
# New page: crash_graph_viewer.py
def show_crash_graph():
    st.title("🕸️ Crash Pattern Network")
    
    # Load crash patterns and relationships
    patterns, relationships = load_crash_graph_data()
    
    # Create interactive graph
    fig = create_crash_network_graph(
        patterns=patterns,
        relationships=relationships,
        color_by='severity',
        size_by='occurrence_count'
    )
    
    st.plotly_chart(fig, use_container_width=True)
    
    # Pattern evolution timeline
    st.subheader("📈 Pattern Evolution")
    show_pattern_evolution_timeline()
    
    # Predictive insights
    st.subheader("🔮 Predictive Analysis")
    insights = crash_analytics.generate_insights()
    
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("7-Day Forecast", f"{insights['crash_forecast']['expected_count']} crashes")
    with col2:
        st.metric("Stability Score", f"{insights['stability_score']:.1%}")
    with col3:
        st.metric("Emerging Patterns", len(insights['emerging_patterns']))
```

## Implementation Phases

### Phase 1: Foundation (Week 1)
- [ ] Create crash pattern database schema
- [ ] Implement fingerprinting system
- [ ] Add pattern learning to save workflow
- [ ] Basic pattern matching

### Phase 2: Intelligence (Week 2)
- [ ] Implement similarity engine
- [ ] Add semantic embedding generation
- [ ] Create relationship discovery
- [ ] Enhanced pattern matching

### Phase 3: Visualization (Week 3)
- [ ] Build crash graph viewer
- [ ] Add pattern evolution tracking
- [ ] Create analytics dashboard
- [ ] Integrate with existing UI

### Phase 4: Predictive (Week 4)
- [ ] Implement trend analysis
- [ ] Add crash forecasting
- [ ] Build risk assessment
- [ ] Create alerting system

## Benefits

1. **Self-Improving**: Every validated crash makes the system smarter
2. **Pattern Discovery**: Automatically identifies new error types
3. **Relationship Mapping**: Understands how crashes relate to each other
4. **Business Intelligence**: Provides actionable insights for stability
5. **Predictive Power**: Forecasts and prevents future crashes
6. **Knowledge Preservation**: Builds institutional knowledge over time

## Success Metrics

- Pattern recognition accuracy > 90%
- New pattern discovery rate
- Average time to resolution reduction
- Crash recurrence rate decrease
- Prediction accuracy for trends

This system transforms crash analysis from reactive to proactive, building valuable knowledge with every crash!