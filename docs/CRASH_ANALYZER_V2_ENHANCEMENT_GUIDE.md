# Crash Analyzer V2 Enhancement Guide
*Following TuoKit Philosophy: Build fast, build smart, build exactly what's needed*

## 🎯 Enhancement Overview
This guide provides strict, detailed prompts for implementing Crash Analyzer V2 enhancements. Each task includes clear instructions, quality gates, and technical debt prevention measures.

---

## 📚 Task 1: Knowledge Base Search Enhancement

### Prompt for Claude:
```
Enhance the Knowledge Base tab in Crash Analyzer V2 with advanced search and management capabilities.

REQUIREMENTS:
1. Add export functionality (CSV/JSON) for saved analyses
2. Implement bulk operations with confirmation dialogs
3. Create advanced search filters using Streamlit native components
4. Add a simple tagging system using the existing metadata JSONB field

TECHNICAL CONSTRAINTS:
- Use existing database schema (no new tables)
- Maximum 200 lines of new code
- Reuse existing UI components from ui/ folder
- Use st.download_button for exports
- Tags stored in metadata->tags as array

IMPLEMENTATION ORDER:
1. Add export buttons to Knowledge Base tab
2. Implement search filters (severity, date range, error type)
3. Add bulk delete with checkbox selection
4. Implement tagging in save dialog

DO NOT:
- Create new database tables
- Add complex UI frameworks
- Implement tag autocomplete (Phase 2)
- Add real-time updates
```

### Quality Gate 1:
```python
# tests/test_knowledge_base_enhancements.py
"""
Test Requirements:
1. Export generates valid CSV with all fields
2. Export generates valid JSON preserving structure  
3. Search filters return correct results
4. Bulk delete requires confirmation
5. Tags are saved in metadata field
6. Empty search returns all results
7. Combined filters work correctly
"""
```

### Technical Debt Prevention:
- [ ] Reuse existing `display_analysis_results` patterns
- [ ] Store tags in existing metadata JSONB field
- [ ] Use Streamlit session state for selections
- [ ] Add docstrings to all new methods
- [ ] Keep UI consistent with existing tabs

---

## 🔍 Task 2: Crash Pattern Recognition

### Prompt for Claude:
```
Implement a CrashPatternDetector class to identify recurring crash patterns.

REQUIREMENTS:
1. Create toolkits/crash_analyzer_v2/analytics/patterns.py
2. Detect time-based patterns (daily/weekly recurring crashes)
3. Find error sequences that appear together
4. Identify cascading failures (A leads to B leads to C)
5. Add "Pattern Analysis" section to Analytics tab

TECHNICAL CONSTRAINTS:
- Use only data from existing queries table
- Pattern detection must complete in <5 seconds
- Maximum 500 lines for entire module
- Use pandas for time series analysis
- Display results using Streamlit native charts

IMPLEMENTATION:
class CrashPatternDetector:
    def __init__(self, db: DatabaseManager):
        self.db = db
    
    def detect_time_patterns(self, time_range: str) -> Dict:
        # Group crashes by hour/day of week
        # Return pattern strength scores
        
    def find_error_sequences(self, window_minutes: int = 5) -> List:
        # Find errors that occur within time windows
        # Return sequence patterns with confidence
        
    def detect_cascading_failures(self) -> Dict:
        # Analyze error chains
        # Return root cause -> effect mappings

DO NOT:
- Use external ML libraries (sklearn, tensorflow)
- Create complex statistical models
- Store patterns in database (yet)
- Implement real-time detection
```

### Quality Gate 2:
```python
# tests/test_pattern_detection.py
"""
Test Requirements:
1. Time patterns detected for hourly recurring crashes
2. Weekly patterns identified correctly
3. Error sequences found within time windows
4. Cascading failures show cause->effect
5. Performance: 1000 crashes analyzed in <2s
6. Empty data returns empty patterns
7. Single crash returns no patterns
"""
```

### Technical Debt Prevention:
- [ ] Inherit from TuoKitToolBase for consistency
- [ ] Use existing time series methods from statistics.py
- [ ] Keep algorithms simple and explainable
- [ ] Add inline comments for pattern logic
- [ ] Create reusable pattern visualization components

---

## 📊 Task 3: Comparative Analysis

### Prompt for Claude:
```
Add period comparison functionality to Analytics dashboard.

REQUIREMENTS:
1. Add "Compare Periods" toggle to Analytics tab
2. Show side-by-side metrics for two time ranges
3. Calculate and display percentage changes
4. Highlight significant differences (>20%)
5. Export comparison report as Markdown

TECHNICAL CONSTRAINTS:
- Reuse existing statistics methods
- Use Streamlit columns for side-by-side display
- Maximum 300 lines of new code
- Keep UI simple with native Streamlit
- Use existing risk score calculation

IMPLEMENTATION LAYOUT:
[=] Compare Periods
When enabled:
┌─────────────┬─────────────┐
│ Period 1    │ Period 2    │
├─────────────┼─────────────┤
│ Metrics...  │ Metrics...  │
│ Risk: 5.2   │ Risk: 7.1↑  │
└─────────────┴─────────────┘

FEATURES:
- Period selectors (Last 7d vs Previous 7d, etc.)
- Synchronized charts with dual axes
- Change indicators (↑↓→) with percentages
- Top changes summary
- Export comparison button

DO NOT:
- Create new database queries
- Add complex date pickers
- Implement custom chart libraries
- Store comparisons
```

### Quality Gate 3:
```python
# tests/test_comparative_analysis.py
"""
Test Requirements:
1. Same period comparison shows 0% change
2. Percentage calculations are accurate
3. Significant changes highlighted correctly
4. Export includes both periods' data
5. UI elements enable/disable properly
6. Null data handled gracefully
7. Time range validation works
"""
```

### Technical Debt Prevention:
- [ ] Extend existing dashboard.py, don't create new file
- [ ] Reuse calculate_metrics_summary() method
- [ ] Use consistent color coding (red=bad, green=good)
- [ ] Keep comparison logic in statistics.py
- [ ] Document comparison algorithm

---

## 🚨 Task 4: Smart Alerts (Phase 1.5)

### Prompt for Claude:
```
Implement in-app smart alerts without email integration.

REQUIREMENTS:
1. Create alert rules based on metrics
2. Show notifications in UI when rules trigger
3. Store alert history in session state
4. Display recent alerts in sidebar
5. Add "Alert Settings" to Analytics tab

TECHNICAL CONSTRAINTS:
- Use st.session_state for alert storage
- Maximum 10 alerts in history
- Check rules only on page refresh
- Simple rule structure (metric, operator, value)
- No database storage (Phase 1.5)

ALERT RULE STRUCTURE:
{
    "name": "High Risk Alert",
    "metric": "risk_score",
    "operator": ">",
    "value": 7,
    "severity": "high"
}

UI COMPONENTS:
- Alert configuration in expander
- Active alerts in sidebar with st.error/warning/info
- Alert history with timestamps
- Mute/unmute functionality
- Test alert button

DO NOT:
- Send emails or webhooks
- Store alerts in database
- Create background processes
- Implement complex rule logic
- Add scheduling
```

### Quality Gate 4:
```python
# tests/test_smart_alerts.py
"""
Test Requirements:
1. Alert triggers when condition met
2. Multiple alerts can be active
3. Alert history maintains 10 items max
4. Muted alerts don't show
5. Test button creates sample alert
6. Invalid rules rejected
7. Alerts cleared on session reset
"""
```

### Technical Debt Prevention:
- [ ] Keep alerts in memory only (Phase 1.5)
- [ ] Use simple dict structure for rules
- [ ] Reuse existing notification patterns
- [ ] Add clear "Phase 1.5" comments
- [ ] Design for easy Phase 2 upgrade

---

## 📈 Task 5: Predictive Analytics (Basic)

### Prompt for Claude:
```
Add basic predictive analytics using simple statistical methods.

REQUIREMENTS:
1. Implement moving average prediction
2. Calculate trend projection (linear)
3. Show confidence intervals
4. Add "Predictions" sub-tab to Analytics
5. Display next 7-day forecast

TECHNICAL CONSTRAINTS:
- Use only numpy/pandas (no sklearn)
- Simple algorithms only
- Maximum prediction: 7 days
- Show prediction uncertainty
- Complete calculation in <1 second

IMPLEMENTATION:
class CrashPredictor:
    def predict_moving_average(self, days_ahead: int = 7):
        # Use 7-day moving average
        # Project forward with trend
        
    def calculate_trend_line(self):
        # Simple linear regression
        # Return slope and intercept
        
    def get_confidence_interval(self):
        # Based on historical variance
        # Return upper/lower bounds

VISUALIZATION:
- Line chart with actual vs predicted
- Shaded area for confidence interval
- Clear marking of prediction start
- Show prediction accuracy metrics

DO NOT:
- Use complex ML models
- Predict beyond 7 days
- Store predictions
- Implement ARIMA/Prophet
- Add seasonality detection
```

### Quality Gate 5:
```python
# tests/test_predictive_analytics.py
"""
Test Requirements:
1. Moving average calculated correctly
2. Trend line fits historical data
3. Predictions stay within bounds
4. Confidence interval widens over time
5. Zero crashes predicts zero
6. Insufficient data handled
7. Performance under 1 second
"""
```

### Technical Debt Prevention:
- [ ] Document statistical assumptions
- [ ] Keep algorithms transparent
- [ ] Add "EXPERIMENTAL" label
- [ ] Use existing chart patterns
- [ ] Plan for ML upgrade path

---

## 🏗️ Implementation Guidelines

### Session Management
```python
# Track enhancement status
if 'enhancement_status' not in st.session_state:
    st.session_state.enhancement_status = {
        'kb_search': False,
        'patterns': False,
        'comparison': False,
        'alerts': False,
        'predictions': False
    }
```

### Progress Tracking
After each task completion:
1. Update enhancement_status
2. Run specific quality gate tests
3. Update this document with completion date
4. Document any deviations from plan

### File Structure
```
toolkits/crash_analyzer_v2/
├── analytics/
│   ├── patterns.py       # NEW: Pattern detection
│   ├── predictor.py      # NEW: Predictions
│   ├── alerts.py         # NEW: Alert rules
│   └── dashboard.py      # MODIFY: Add features
├── ui/
│   ├── knowledge_base.py # NEW: Extract KB UI
│   └── comparisons.py    # NEW: Comparison UI
└── tests/
    └── test_enhancements.py
```

---

## 📋 Task Completion Tracker

| Task | Status | Completed Date | Quality Gate | Notes |
|------|--------|----------------|--------------|-------|
| Knowledge Base Search | ✅ Completed | 2025-07-08 | ✅ Passed (9/9) | Export CSV/JSON, filters, bulk ops, tagging |
| Pattern Recognition | ✅ Completed | 2025-07-08 | ✅ Passed (9/9) | Time patterns, sequences, cascades, insights |
| Comparative Analysis | ✅ Completed | 2025-07-08 | ✅ Passed (9/9) | Toggle, side-by-side, % changes, export |
| Smart Alerts | ✅ Completed | 2025-07-08 | ✅ Passed (9/9) | In-app alerts, mute, history, test button |
| Predictive Analytics | ✅ Completed | 2025-07-08 | ✅ Passed (9/9) | 7-day MA prediction, trend line, confidence intervals |

---

## 🚀 Quick Start for Next Session

```python
# 1. Read this guide
# 2. Check completion tracker
# 3. Start with next pending task
# 4. Follow prompt exactly
# 5. Run quality gate tests
# 6. Update tracker
# 7. Commit with message: "feat(crash-analyzer): implement [task name]"
```

---

## ⚠️ Important Reminders

1. **Each task is independent** - Can be implemented in any order
2. **Quality gates are mandatory** - Don't proceed without passing tests
3. **Keep it simple** - Resist feature creep
4. **Document everything** - Future you will thank you
5. **Test incrementally** - Run tests after each method
6. **Preserve existing functionality** - All current features must work

---

*Remember: Build fast, build smart, build exactly what's needed - The TuoKit Way!*