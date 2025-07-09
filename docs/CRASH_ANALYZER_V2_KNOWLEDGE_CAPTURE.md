# Crash Analyzer V2 - Knowledge Capture Enhancement

## Overview
The Crash Analyzer V2 has been enhanced with full knowledge capture and save functionality, allowing users to build a comprehensive knowledge base of crash analyses over time.

## New Features

### 1. Save Analysis Button 💾
After running any crash analysis, users can now save the results to the knowledge base with a single click.

**How it works:**
- Click the "💾 Save Analysis" button in the results display
- The analysis is automatically saved to the database
- A unique query ID is generated for tracking
- Results are also saved to the knowledge base for long-term reference

**What gets saved:**
- Analysis method used
- Filename analyzed
- Timestamp of analysis
- Complete analysis results
- Severity and error type metadata
- Confidence scores

### 2. Knowledge Base Button 🧠
The previously placeholder "Knowledge Base" button is now fully functional, providing access to all saved crash analyses.

**Features:**
- View all saved crash analyses
- See analysis method, filename, and timestamp
- Expand to view full results
- Quick access to reanalyze saved results
- Search and filter capabilities (coming soon)

### 3. Automatic Knowledge Capture
Every saved analysis is automatically captured in the knowledge base with intelligent categorization:
- **Critical Issue** - For CRITICAL or HIGH severity crashes
- **Error Solution** - For other crash analyses

## Technical Implementation

### Database Integration
- Uses TuoKit's centralized `DatabaseManager`
- Stores analyses in both `query_history` and `knowledge_base` tables
- Maintains relationships between queries and knowledge units

### TuoKitToolBase Integration
- Inherits knowledge capture capabilities from `TuoKitToolBase`
- Uses `save_to_knowledge_base()` method for structured storage
- Automatic quality scoring and categorization

### UI Enhancements
- Save functionality integrated into `results_display.py`
- Knowledge Base viewer in main analyzer interface
- Real-time success/error feedback

## Usage Guide

### Saving an Analysis
1. Upload a crash dump file
2. Select an analysis method
3. Run the analysis
4. Once complete, click "💾 Save Analysis"
5. See confirmation message with query ID

### Viewing Knowledge Base
1. Click "🧠 Knowledge Base" in Additional Tools
2. Browse saved analyses
3. Click on any analysis to expand details
4. Use "🔄 View Full Analysis" to reload results

### Building Knowledge Over Time
- Save important crash analyses for reference
- Build patterns of common issues
- Create a searchable repository of solutions
- Track crash trends across projects

## Benefits

### For Developers
- Quick access to previous crash solutions
- Pattern recognition across similar crashes
- Build institutional knowledge
- Reduce debugging time

### For Teams
- Share crash analysis knowledge
- Consistent analysis approach
- Historical tracking of issues
- Knowledge transfer between team members

### For Projects
- Track recurring issues
- Identify systemic problems
- Document crash resolutions
- Build crash analysis playbooks

## Future Enhancements

### Planned Features
1. **Search Functionality** - Search by error type, severity, or content
2. **Pattern Detection** - Automatically identify similar crashes
3. **Trend Analysis** - Visualize crash patterns over time
4. **Export/Import** - Share knowledge bases between teams
5. **AI Insights** - Generate insights from accumulated knowledge

### Integration Opportunities
- Connect with issue tracking systems
- Automated alerts for critical patterns
- Integration with CI/CD pipelines
- Knowledge base API for external tools

## Testing

Run the test suite to verify functionality:
```bash
python3 -m pytest tests/test_crash_analyzer_v2_save.py -v
```

All tests should pass, confirming:
- Database connection initialization
- Save functionality
- Knowledge base integration
- Data structure integrity

## Conclusion

The Crash Analyzer V2 now provides a complete knowledge management solution for crash analysis, turning every debugging session into a learning opportunity that benefits the entire team.

*Built fast, built smart, built exactly what's needed - The TuoKit Way*