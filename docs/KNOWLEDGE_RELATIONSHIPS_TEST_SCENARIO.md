# 🧪 Knowledge Relationships Testing Scenario

## 📋 Testing Strategy: Progressive Validation

### **Phase 1: Database Setup & Verification** (5 minutes)
**Goal**: Ensure all fixes are properly deployed

```bash
# 1. Start database and verify migration
python3 pages/database_health_check.py

# Expected: 95-100% completion with knowledge_links table

# 2. Run the enhanced migration (if needed)
python3 scripts/migration/create_knowledge_capture_tables.py

# Expected: All indexes and constraints added successfully
```

### **Phase 2: Basic Knowledge Capture** (10 minutes)
**Goal**: Create diverse knowledge units for relationship testing

```bash
# 3. Start TuoKit and go to Knowledge Capture Demo
streamlit run pages/knowledge_capture_demo.py
```

**Test Knowledge to Capture** (use the demo scenarios):
1. **Basic Python**: "Explain the difference between lists and tuples in Python with examples"
2. **Advanced Python**: "Implement list comprehensions with conditional logic and nested loops" 
3. **Error Solution**: "How do I fix 'ImportError: No module named pandas' in Python?"
4. **Installation Guide**: "How to install Python packages using pip and requirements.txt"
5. **SQL Basics**: "Write a SQL query to find the top 5 customers by order count"
6. **Rails Migration**: "Explain Rails migrations and provide a practical example"

**Expected Results**:
- 6 knowledge units captured (if quality meets thresholds)
- Each shows "Knowledge captured! ID: X" 
- Quality scores should be 30+

### **Phase 3: Relationship Discovery** (5 minutes)
**Goal**: Test the incremental discovery system

```bash
# 4. Run relationship discovery
python3 scripts/discover_knowledge_relationships.py
```

**Expected Results**:
```
🔗 Discovering Knowledge Relationships
Found 6 quality knowledge units
Analyzing relationships between 6 units...
✅ Discovered and stored 8 relationships!

📊 Relationship Summary:
   related: 4 connections
   prerequisite: 3 connections  
   solution: 1 connections
```

### **Phase 4: Relationship Validation** (10 minutes)
**Goal**: Verify relationships make sense and are bidirectional

```bash
# 5. Test the Knowledge Capture Demo again
streamlit run pages/knowledge_capture_demo.py
```

**Validation Steps**:
1. **Capture new knowledge**: "Advanced Python debugging with pdb and logging"
2. **Check for automatic relationships**: Should see "🔗 Related Knowledge" widget
3. **Verify relationship quality**:
   - Python debugging → Python lists (related)
   - Python debugging → ImportError solution (solution)
   - Python debugging → Basic Python (prerequisite)

**Expected Behavior**:
- Related Knowledge widget appears automatically
- Shows 3-5 related items grouped by type
- Strength scores 30-90%
- Quality scores shown for each related item

### **Phase 5: Cross-Tool Testing** (10 minutes)
**Goal**: Test relationships work across different tools

```bash
# 6. Try other tools that inherit from TuoKitToolBase
streamlit run pages/rails_model_gen.py
```

**Test Scenario**:
1. **Generate Rails code**: "Create a Rails model for User with authentication"
2. **Check for relationships**: Should connect to Rails migration knowledge
3. **Capture more Rails knowledge**: "Rails controller best practices"
4. **Verify cross-connections**: Rails topics should link together

### **Phase 6: Performance Testing** (5 minutes)
**Goal**: Verify system handles multiple knowledge units efficiently

**Test Process**:
1. Capture 10-15 more knowledge units rapidly
2. Check that each new unit triggers incremental discovery (not full re-scan)
3. Verify response times remain under 2 seconds
4. Monitor database for performance issues

**Performance Expectations**:
- Incremental discovery: < 1 second per new knowledge unit
- Relationship queries: < 500ms 
- No memory leaks or connection issues
- Logs show "Incremental discovery" not "Full discovery"

## 🎯 **Success Criteria**

### **✅ Functional Success**
- [ ] All 6 initial knowledge units captured successfully
- [ ] 5+ relationships discovered automatically  
- [ ] Bidirectional relationships work (prerequisites ↔ builds_on)
- [ ] Related Knowledge widget shows meaningful connections
- [ ] Cross-tool relationship discovery works

### **✅ Performance Success**
- [ ] Incremental discovery working (O(n) not O(n²))
- [ ] Database queries under 500ms
- [ ] No transaction failures or rollbacks
- [ ] Memory usage stable under 100MB for relationships

### **✅ Quality Success**
- [ ] Python topics link together logically
- [ ] Error solutions connect to related problems
- [ ] Basic → Advanced prerequisite chains work
- [ ] No duplicate or self-referential relationships
- [ ] Strength scores reflect actual relationship quality

## 🔍 **What to Watch For**

### **🚨 Red Flags** (Stop testing)
- Database transaction errors
- Relationships not appearing at all
- Self-referential relationships (unit linking to itself)
- Performance degradation (> 5 seconds for discovery)
- Memory leaks or connection pool exhaustion

### **⚠️ Yellow Flags** (Note but continue)
- Low relationship quality scores (all < 30%)
- Missing obvious relationships (Python basics ↔ Python advanced)
- Too many or too few relationships per unit
- UI responsiveness issues

### **✅ Green Flags** (System working well)
- Automatic relationship discovery after knowledge capture
- Logical relationship groupings (Related, Prerequisites, Solutions)
- Reasonable strength scores (30-90%)
- Fast response times (< 2 seconds)
- Cross-tool knowledge connections

## 📊 **Expected Test Results**

**Sample Relationship Graph**:
```
Python Lists (Basic) ──prerequisite──> List Comprehensions (Advanced)
       │                                      │
   related│                              related│
       │                                      │
   ImportError Fix ──solution──> pip Install Guide
       │
   related│
       │
   Rails Migration ──related──> Rails Models
```

**Sample UI Output**:
```
🔗 Related Knowledge
Knowledge connected to your recent interaction:

Related Topics:
• Python List Comprehensions (87% match, Q:92)
• SQL Query Basics (45% match, Q:78)

Prerequisites:
• Python Basics Introduction (76% match, Q:85)

Solutions:
• Fix Python Import Errors (68% match, Q:71)
```

## 📝 **Testing Log Template**

### Phase 1 Results:
- [ ] Database health check: ___% completion
- [ ] Migration status: SUCCESS / FAILED
- [ ] Issues encountered: ________________

### Phase 2 Results:
- [ ] Knowledge units captured: ___/6
- [ ] Average quality score: ___
- [ ] Issues encountered: ________________

### Phase 3 Results:
- [ ] Relationships discovered: ___
- [ ] Discovery time: ___ seconds
- [ ] Issues encountered: ________________

### Phase 4 Results:
- [ ] Related Knowledge widget: WORKING / NOT WORKING
- [ ] Relationship quality: GOOD / POOR
- [ ] Issues encountered: ________________

### Phase 5 Results:
- [ ] Cross-tool relationships: WORKING / NOT WORKING
- [ ] Response times: FAST / SLOW
- [ ] Issues encountered: ________________

### Phase 6 Results:
- [ ] Performance: GOOD / POOR
- [ ] Memory usage: STABLE / INCREASING
- [ ] Issues encountered: ________________

## 🐛 **Common Issues & Solutions**

### Database Connection Issues:
```bash
# Check if PostgreSQL is running
sudo systemctl status postgresql

# Restart if needed
sudo systemctl restart postgresql
```

### Migration Failures:
```bash
# Check database permissions
python3 fix_database_permissions.py

# Re-run migration
python3 scripts/migration/create_knowledge_capture_tables.py
```

### No Relationships Discovered:
- Check knowledge quality scores (must be 30+)
- Verify knowledge units have proper tags
- Look for validation errors in logs

### Performance Issues:
- Check database indexes are created
- Monitor memory usage during discovery
- Verify incremental vs full discovery in logs

---

**Total estimated testing time: ~45 minutes**

This testing scenario validates the entire Knowledge Relationships system end-to-end. Report any issues encountered and I'll help troubleshoot!