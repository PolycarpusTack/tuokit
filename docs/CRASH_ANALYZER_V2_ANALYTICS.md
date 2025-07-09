# Crash Analyzer V2 - Analytics Dashboard

## Overview
The Crash Analyzer V2 now includes a comprehensive Analytics Dashboard that provides business intelligence insights from crash data. This feature transforms raw crash analyses into actionable metrics, trends, and reports.

## Features

### 📊 Analytics Tab
A dedicated tab in the Crash Analyzer interface providing:

1. **Key Metrics Dashboard**
   - Total crash count with trend indicators
   - Trend percentage vs previous period
   - Risk score (0-10 scale)
   - Unique error types count

2. **Overview Tab**
   - Crash distribution by error type
   - Severity distribution breakdown
   - Analysis method usage statistics

3. **Trends Tab**
   - Time series visualization of crashes
   - Daily/weekly patterns
   - Trend direction indicators
   - Day-of-week analysis

4. **Top Issues Tab**
   - Ranked list of most frequent crashes
   - Percentage breakdown
   - Trending issues identification
   - Actionable insights

5. **Reports Tab**
   - Executive summary generation
   - Markdown report export
   - CSV data export
   - Future: Automated report scheduling

### 📚 Enhanced Knowledge Base Tab
The Knowledge Base has been moved to its own dedicated tab with:
- Search functionality
- Severity indicators
- Detailed view/collapse
- Load saved analyses back into analyzer

### 🛠️ Updated Additional Tools
The placeholder buttons now direct users to the appropriate tabs:
- "View Statistics" → Analytics tab
- "Knowledge Base" → Knowledge Base tab  
- "Trending Issues" → Analytics tab

## Architecture

### Module Structure
```
toolkits/crash_analyzer_v2/
├── analytics/
│   ├── __init__.py         # Module exports
│   ├── statistics.py       # Statistical calculations
│   └── dashboard.py        # UI components
```

### Key Components

#### CrashStatistics
- Calculates distributions and metrics
- Extracts data from saved analyses
- Time series data generation
- Trend calculations

#### CrashAnalyticsDashboard
- Streamlit-native visualization
- Tab-based interface
- Export functionality
- Real-time metrics

## Usage

### Viewing Analytics
1. Open Crash Analyzer V2
2. Click on the "📊 Analytics" tab
3. Select time range (7, 30, or 90 days)
4. Browse through sub-tabs for different views

### Understanding Metrics

**Risk Score (0-10)**
- Based on critical issue percentage and trend
- 0-3: 🟢 Low risk
- 4-6: 🟡 Medium risk  
- 7-10: 🔴 High risk

**Trend Indicators**
- Compares current period to previous
- Red (inverse) for crash increases
- Green for crash decreases

### Generating Reports
1. Go to Analytics → Reports tab
2. Review executive summary
3. Click "Download Report (Markdown)"
4. Optional: Export raw data as CSV

## Benefits

### For Development Teams
- Identify most common crashes quickly
- Track if fixes are working (trend analysis)
- Prioritize based on frequency and severity
- Build knowledge over time

### For Management
- Executive summaries with key metrics
- Risk assessment at a glance
- Trend visualization for planning
- Export reports for stakeholders

### For DevOps
- Pattern detection for system issues
- Day-of-week analysis for scheduling
- Historical data for capacity planning
- Early warning through risk scores

## Technical Implementation

### Data Source
- Reads from `query_history` table
- Filters for `crash_analyzer_v2` tool entries
- Parses saved analysis results
- No additional database tables needed (Phase 1)

### Performance
- Efficient queries with limits
- Client-side calculations
- Streamlit caching where applicable
- Responsive UI updates

### Extensibility
- Modular design for easy enhancement
- Clear separation of stats/UI
- Ready for ML integration (Phase 3)
- Database schema prepared for metrics table (Phase 2)

## Future Enhancements

### Phase 2 (Next Week)
- Dedicated metrics database table
- More sophisticated trend analysis
- Email report scheduling
- Custom alert thresholds

### Phase 3 (Coming Soon)
- ML-based anomaly detection
- Predictive crash forecasting
- Correlation analysis
- Integration with monitoring tools

### Phase 4 (Future)
- API for external dashboards
- Real-time crash streaming
- Multi-project analytics
- Cost impact calculations

## Testing

Run the test suite:
```bash
python3 -m pytest tests/test_crash_analyzer_v2_analytics.py -v
```

All 7 tests should pass, confirming:
- Statistics engine functionality
- Data extraction methods
- Dashboard initialization
- Module structure

## Conclusion

The Analytics Dashboard transforms Crash Analyzer V2 from a reactive debugging tool into a proactive crash management system. By providing trends, metrics, and insights, teams can prevent crashes before they become critical issues.

*Built fast, built smart, built exactly what's needed - The TuoKit Way!*