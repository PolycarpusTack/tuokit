# Knowledge Capture System - Deployment Guide

## 🚀 Quick Deployment

The Knowledge Capture System is now fully integrated and ready to deploy!

### 1. Database Setup

**Run the migration to create the required tables:**
```bash
# From TuoKit root directory
python scripts/migration/create_knowledge_capture_tables.py
```

**Expected output:**
```
🚀 Starting Knowledge Capture System migration...
✅ knowledge_links table created
✅ knowledge_maintenance_log table created
✅ Enhanced knowledge_units table
✅ Quality constraints added
✅ Performance indexes created
✅ Migration completed successfully!
```

### 2. Environment Configuration

**Update your .env file:**
```env
# Enable knowledge capture (default: true)
ENABLE_KNOWLEDGE_CAPTURE=true

# Existing database config
TUOKIT_PG_HOST=localhost
TUOKIT_PG_PORT=5432
TUOKIT_PG_DB=tuokit_knowledge
TUOKIT_PG_USER=tuokit_user
TUOKIT_PG_PASSWORD=your_secure_password
```

### 3. Verification

**Test the system:**
```bash
# Run the test suite
python tests/test_knowledge_capture.py

# Expected result: All tests pass ✅
```

## 🎯 What Happens Now

### For Tool Developers
**No changes required!** All existing tools that use `TuoKitToolBase` will automatically:

- ✅ Capture high-quality knowledge from AI interactions
- ✅ Filter out low-quality content
- ✅ Categorize and tag content automatically
- ✅ Make knowledge searchable across tools

### For Tool Users
**Enhanced experience immediately:**

1. **Automatic Learning**: Every valuable AI interaction builds your knowledge base
2. **Smart Search**: Find related solutions from past interactions
3. **Quality Assurance**: Only valuable content is captured
4. **Zero Maintenance**: No manual categorization required

## 🔧 Integration Examples

### Basic Tool Integration
```python
from utils.tool_base import TuoKitToolBase

class MyTool(TuoKitToolBase):
    def __init__(self):
        super().__init__("My Tool", "Tool description")
    
    def run(self):
        # This line automatically captures knowledge!
        result = self.generate_with_capture("Explain Python decorators")
        
        st.write(result['response'])
        
        # Show capture status
        self.show_knowledge_status_indicator()
```

### Enhanced Tool with Search
```python
def run(self):
    # Let users search existing knowledge first
    self.show_knowledge_search_widget()
    
    user_input = st.text_input("Your question:")
    if user_input:
        # Auto-capture happens here
        result = self.generate_with_capture(user_input)
        st.write(result['response'])
        
        # Show enhanced metrics
        with st.sidebar:
            self.show_enhanced_metrics()
```

## 📊 Monitoring

### Check System Health
```python
# Get capture statistics
if st.session_state.get('capture_manager'):
    metrics = st.session_state.capture_manager.get_metrics()
    print(f"Total knowledge units: {metrics.get('total_units', 0)}")
    print(f"Average quality: {metrics.get('avg_quality', 0):.1f}/100")
```

### Database Queries
```sql
-- Check recent captures
SELECT COUNT(*) as captures_today 
FROM knowledge_units 
WHERE created_at > CURRENT_DATE;

-- Quality distribution
SELECT 
    CASE 
        WHEN quality_score >= 80 THEN 'Excellent'
        WHEN quality_score >= 60 THEN 'Good'
        WHEN quality_score >= 40 THEN 'Fair'
        ELSE 'Needs Improvement'
    END as quality_tier,
    COUNT(*) as count
FROM knowledge_units 
GROUP BY quality_tier;

-- Most active tools
SELECT tool_specific_data->>'tool' as tool_name, COUNT(*) as captures
FROM knowledge_units
WHERE tool_specific_data->>'tool' IS NOT NULL
GROUP BY tool_name
ORDER BY captures DESC;
```

## 🎛️ Configuration Options

### Feature Flags
```env
# Enable/disable the entire system
ENABLE_KNOWLEDGE_CAPTURE=true

# Future: Configure quality thresholds
KNOWLEDGE_MIN_QUALITY=30
KNOWLEDGE_MAX_CONTENT_LENGTH=10000
```

### Quality Tuning
```python
# In utils/knowledge_capture.py, you can adjust:
MIN_CONTENT_LENGTH = 50      # Minimum chars to capture
MIN_QUALITY_SCORE = 30       # Minimum quality score
DUPLICATE_THRESHOLD = 0.85   # Similarity threshold for duplicates
```

## 🚨 Troubleshooting

### Common Issues

**1. Knowledge not being captured**
- Check: `ENABLE_KNOWLEDGE_CAPTURE=true` in .env
- Check: Database connection working
- Check: Content meets quality thresholds

**2. Database errors**
- Ensure migration ran successfully
- Check PostgreSQL is running
- Verify table permissions

**3. Performance issues**
- Monitor the `knowledge_maintenance_log` table
- Ensure indexes are created
- Check for runaway quality scoring

### Rollback Plan
```bash
# If needed, rollback the migration
python scripts/migration/create_knowledge_capture_tables.py --rollback

# Or disable via environment
ENABLE_KNOWLEDGE_CAPTURE=false
```

## 📈 Success Metrics

After deployment, monitor these metrics:

- **Capture Rate**: ~10-30% of AI interactions should result in knowledge capture
- **Quality Distribution**: 60% should score 50+ quality points
- **Search Usage**: Users should search before asking questions
- **Knowledge Growth**: Steady increase in valuable knowledge units

## 🎉 Benefits Realized

Within the first week, you should see:

1. **Reduced Repeated Questions**: Similar questions get answered from knowledge base
2. **Improved Response Quality**: Tools learn from previous high-quality interactions
3. **Team Learning**: Knowledge from one team member benefits everyone
4. **Searchable History**: Easy to find that SQL query from last month

## 🔄 Maintenance

The system is designed to be **zero-maintenance**, but you can:

- **Weekly**: Review knowledge capture metrics
- **Monthly**: Analyze quality trends and adjust thresholds
- **Quarterly**: Export valuable knowledge for documentation

---

**🎯 Ready to deploy!** The Knowledge Capture System will immediately start learning from every AI interaction, building a valuable knowledge repository with zero manual effort.