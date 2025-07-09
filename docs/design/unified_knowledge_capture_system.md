# Unified Knowledge Capture System Design Document

**Version:** 1.0  
**Created:** 2025-01-04  
**Status:** In Development  
**Architect:** TuoKit Architect  

## Executive Summary

The Unified Knowledge Capture System automatically captures, categorizes, and connects valuable knowledge from every AI interaction across TuoKit, building a searchable institutional memory with zero manual effort.

## Design Principles

1. **Automatic Capture**: Every AI interaction is a learning opportunity
2. **Zero Friction**: No manual steps required for basic capture
3. **Progressive Enhancement**: Start simple, add features based on usage
4. **Quality Over Quantity**: Smart filtering prevents knowledge bloat
5. **Measurable Value**: Track what knowledge gets reused

## System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   TuoKit Tools Layer                     │
│  (Code Tools, Error Tool, Rails Tools, SQL Tools, etc.) │
└─────────────────┬───────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────┐
│           Unified Knowledge Capture Layer                │
│                                                         │
│  ┌─────────────┐  ┌──────────────┐  ┌───────────────┐ │
│  │  Extractor  │  │  Categorizer │  │  Relationship │ │
│  │   Engine    │  │    Engine    │  │    Builder    │ │
│  └─────────────┘  └──────────────┘  └───────────────┘ │
└─────────────────┬───────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────────────────────┐
│                  PostgreSQL Database                     │
│  ┌─────────┐  ┌────────────────┐  ┌─────────────────┐ │
│  │ queries │  │ knowledge_units │  │ knowledge_links │ │
│  └─────────┘  └────────────────┘  └─────────────────┘ │
└─────────────────────────────────────────────────────────┘
```

## Quality Gates & Technical Debt Prevention

### 1. Automatic Quality Checks

```python
class QualityGate:
    """Prevents low-quality knowledge from entering the system"""
    
    MIN_CONTENT_LENGTH = 50  # Characters
    MAX_CONTENT_LENGTH = 10000  # Prevent huge blobs
    MIN_QUALITY_SCORE = 30  # Out of 100
    
    def validate(self, content: str, metadata: Dict) -> Tuple[bool, str]:
        # Length check
        if len(content) < self.MIN_CONTENT_LENGTH:
            return False, "Content too short to be valuable"
            
        # Duplicate check (fuzzy matching)
        if self.is_duplicate(content):
            return False, "Similar knowledge already exists"
            
        # Quality scoring
        score = self.calculate_quality_score(content, metadata)
        if score < self.MIN_QUALITY_SCORE:
            return False, f"Quality score {score} below threshold"
            
        return True, "Passed all quality gates"
```

### 2. Automatic Cleanup & Maintenance

```python
class KnowledgeMaintenance:
    """Prevents knowledge bloat through automatic cleanup"""
    
    def daily_cleanup(self):
        # 1. Remove unused knowledge after 90 days
        self.remove_stale_knowledge(days=90, min_usage=0)
        
        # 2. Merge similar knowledge units
        self.merge_duplicates(similarity_threshold=0.95)
        
        # 3. Update quality scores based on usage
        self.recalculate_quality_scores()
        
        # 4. Archive old but valuable knowledge
        self.archive_old_knowledge(days=180, min_quality=70)
    
    def weekly_analysis(self):
        # Generate report on knowledge health
        return {
            "total_units": self.count_total(),
            "high_quality": self.count_quality(min_score=70),
            "unused": self.count_unused(days=30),
            "duplicates": self.find_potential_duplicates()
        }
```

### 3. Performance Safeguards

```python
class PerformanceGuard:
    """Ensures system remains fast as knowledge grows"""
    
    # Limits to prevent slowdown
    MAX_SEARCH_RESULTS = 50
    MAX_GRAPH_NODES = 100
    CACHE_TTL = 3600  # 1 hour
    
    # Automatic indexing
    INDEXES = [
        "CREATE INDEX idx_knowledge_category ON knowledge_units(category)",
        "CREATE INDEX idx_knowledge_quality ON knowledge_units(quality_score DESC)",
        "CREATE INDEX idx_knowledge_usage ON knowledge_units(usage_count DESC)",
        "CREATE INDEX idx_knowledge_search ON knowledge_units USING gin(to_tsvector('english', content))"
    ]
```

## Implementation Phases

### Phase 1: Core Capture (Week 1)
- [ ] Create `utils/knowledge_capture.py`
- [ ] Implement basic extraction engine
- [ ] Add quality gates
- [ ] Update `tool_base.py` integration
- [ ] Write tests

**Success Metrics:**
- All tools auto-capture interactions
- < 5% low-quality captures
- No performance degradation

### Phase 2: Smart Categorization (Week 2)
- [ ] Implement categorization engine
- [ ] Add pattern recognition
- [ ] Create category taxonomy
- [ ] Add manual override capability

**Success Metrics:**
- 90% accurate auto-categorization
- < 100ms categorization time
- Category distribution reports

### Phase 3: Relationship Builder (Week 3)
- [ ] Design relationship types
- [ ] Implement graph builder
- [ ] Add relationship discovery
- [ ] Create visualization component

**Success Metrics:**
- Average 3+ relationships per knowledge unit
- Graph traversal < 50ms
- Meaningful connection discovery

### Phase 4: Search & Dashboard (Week 4)
- [ ] Build knowledge dashboard page
- [ ] Implement advanced search
- [ ] Add export capabilities
- [ ] Create usage analytics

**Success Metrics:**
- Search results < 200ms
- 80% search satisfaction
- Daily dashboard usage

## Database Schema

```sql
-- Enhanced knowledge capture schema
CREATE TABLE IF NOT EXISTS knowledge_units (
    id SERIAL PRIMARY KEY,
    query_id INTEGER REFERENCES queries(id),
    title VARCHAR(500) NOT NULL,
    content TEXT NOT NULL,
    category VARCHAR(100),
    tags TEXT[],
    quality_score INTEGER DEFAULT 50,
    usage_count INTEGER DEFAULT 0,
    tool_specific_data JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_accessed TIMESTAMP,
    
    -- Quality gates
    CONSTRAINT content_length CHECK (char_length(content) >= 50),
    CONSTRAINT quality_range CHECK (quality_score >= 0 AND quality_score <= 100)
);

CREATE TABLE IF NOT EXISTS knowledge_links (
    id SERIAL PRIMARY KEY,
    source_id INTEGER REFERENCES knowledge_units(id) ON DELETE CASCADE,
    target_id INTEGER REFERENCES knowledge_units(id) ON DELETE CASCADE,
    relationship_type VARCHAR(50),
    strength FLOAT DEFAULT 1.0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    
    -- Prevent self-links and duplicates
    CONSTRAINT no_self_link CHECK (source_id != target_id),
    CONSTRAINT unique_link UNIQUE (source_id, target_id, relationship_type)
);

-- Maintenance tracking
CREATE TABLE IF NOT EXISTS knowledge_maintenance_log (
    id SERIAL PRIMARY KEY,
    action VARCHAR(100),
    details JSONB,
    units_affected INTEGER,
    execution_time_ms INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## Anti-Patterns to Avoid

1. **Knowledge Hoarding**: Capturing everything without quality filters
2. **Over-Engineering**: Adding ML before proving basic value
3. **Manual Processes**: Requiring user action for basic capture
4. **Slow Queries**: Not indexing properly from the start
5. **Lost Context**: Capturing content without tool/user context

## Monitoring & Metrics

```python
class KnowledgeMetrics:
    """Track system health and value"""
    
    def dashboard_metrics(self) -> Dict:
        return {
            # Growth metrics
            "daily_captures": self.count_captures_today(),
            "weekly_growth_rate": self.calculate_growth_rate(),
            
            # Quality metrics
            "avg_quality_score": self.average_quality_score(),
            "high_quality_percentage": self.high_quality_percentage(),
            
            # Usage metrics
            "most_accessed": self.top_accessed_knowledge(10),
            "unused_percentage": self.unused_percentage(),
            
            # Performance metrics
            "avg_search_time": self.average_search_time(),
            "avg_capture_time": self.average_capture_time(),
            
            # Health metrics
            "duplicate_candidates": self.count_potential_duplicates(),
            "stale_knowledge": self.count_stale_knowledge()
        }
```

## Rollback Plan

If issues arise, the system can be disabled without affecting core functionality:

```python
# In settings or environment
ENABLE_KNOWLEDGE_CAPTURE = False

# In tool_base.py
if settings.ENABLE_KNOWLEDGE_CAPTURE:
    self.capture_knowledge(response)
# Core functionality continues regardless
```

## Future Enhancements (Not Phase 1)

- Vector embeddings for semantic search
- ML-based quality scoring
- Automatic knowledge synthesis
- Multi-language support
- External knowledge import

## Implementation Checklist

- [ ] Create this design document
- [ ] Set up monitoring dashboards
- [ ] Create database migrations
- [ ] Implement Phase 1 core
- [ ] Add quality gate tests
- [ ] Deploy with feature flag
- [ ] Monitor for 1 week
- [ ] Proceed to Phase 2

## Change Log

- 2025-01-04: Initial design document created
- [Future dates: Track major changes here]

---

**Remember**: Build fast, build smart, build exactly what's needed. Every line of code should solve a real user problem.