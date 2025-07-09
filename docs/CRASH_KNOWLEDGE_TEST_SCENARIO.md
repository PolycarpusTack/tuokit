# Crash Knowledge System - Comprehensive Test Scenario

## Overview
This test scenario demonstrates:
1. Learning from new crash patterns
2. Recognizing known patterns
3. Discovering relationships between crashes
4. Linking crashes to general knowledge
5. Building a crash knowledge network

## Prerequisites
1. Run the migration:
```bash
python scripts/migration/create_crash_knowledge_tables.py
```

2. Ensure Knowledge Capture is enabled in `.env`:
```
ENABLE_KNOWLEDGE_CAPTURE=true
```

## Test Scenario: Building Crash Intelligence

### Phase 1: First Crash - New Pattern Learning

#### Step 1.1: Upload First "Method Not Understood" Crash
1. Go to **Crash Analyzer**
2. Upload: `crash_examples/WCR_25-11_9-42-27.txt`
3. Observe:
   - "🎯 What's On Crash Report (WCR) Detected"
   - Quick Summary shows: "Message not understood: #productInContractFor:"
   - "🆕 New Crash Pattern Detected" (first time seeing this)

#### Step 1.2: Analyze the Crash
1. Click "⚡ Basic Analysis"
2. Review the analysis:
   - Root Cause: Method called on nil object
   - Quick Fix: Check for nil values
   - Severity: High

#### Step 1.3: Save with Business Impact
1. Validator Name: "TestUser"
2. Quality Rating: 4/5
3. Business Impact:
   - Downtime: 45 minutes
   - Affected Features: "CM2 Updates, Contract Management"
   - Impact Notes: "Prevented contract updates for MTV site"
4. Click "💾 Save Validated Analysis"
5. Observe: "🧠 Pattern Learning: created pattern 'MethodNotFound_productInContractFor'"

### Phase 2: Second Similar Crash - Pattern Recognition

#### Step 2.1: Upload Similar Crash
1. Create a modified version of the crash with different details but same error type
2. Upload the modified file
3. Observe:
   - "✅ Matched Known Pattern: MethodNotFound_productInContractFor"
   - Pattern Confidence: 50% (initial confidence)
   - Match Type: Structural/Fuzzy
   - Proven Solutions: 1

#### Step 2.2: Validate Match Was Correct
1. Analyze and confirm the suggested solution worked
2. Save with validation
3. Observe: Pattern confidence increases to 60%

### Phase 3: Create Knowledge Relationships

#### Step 3.1: Generate Related Knowledge
Go to different tools and create related knowledge:

1. **Code Tools** → Generate code snippet:
   ```smalltalk
   "Safe method calling with nil check"
   object ifNotNil: [:obj | obj productInContractFor: aContract]
   ```
   Save as: "Smalltalk nil-safe method calling"

2. **Error Tool** → Analyze error:
   ```
   Message not understood errors occur when calling methods on nil
   ```
   Save as: "Understanding Smalltalk message not understood errors"

3. **Doc Tools** → Create documentation:
   ```
   Best Practices for Smalltalk Error Prevention:
   - Always check for nil before method calls
   - Use ifNotNil: blocks
   - Implement doesNotUnderstand: for debugging
   ```
   Save as: "Smalltalk error prevention guide"

#### Step 3.2: Trigger Relationship Discovery
Run the relationship discovery script:
```bash
python scripts/discover_knowledge_relationships.py
```

Expected output:
```
Discovering relationships for crash knowledge...
Found relationships:
- Crash pattern → "Smalltalk nil-safe method calling" (solution relationship)
- Crash pattern → "Understanding message not understood errors" (related concept)
- Crash pattern → "Smalltalk error prevention guide" (prevention strategy)
```

### Phase 4: Test Different Crash Types

#### Step 4.1: Low Space Crash
1. Upload: `crash_examples/WCR_3-12_9-21-32.txt`
2. Observe:
   - New pattern: "Emergency: No Space Left"
   - Different category: "System Resources"
3. Save with impact data

#### Step 4.2: Database Error
1. Upload a crash with Oracle errors
2. Creates another pattern category
3. Save and validate

### Phase 5: View Crash Intelligence

#### Step 5.1: Crash Pattern Network
1. Go to **Crash Pattern Network** page
2. Observe:
   - Visual network of crash patterns
   - Node size = occurrence frequency
   - Node color = severity (red=critical, yellow=warning, green=low)
   - Edges show relationships between patterns

#### Step 5.2: Knowledge Graph Integration
1. Go to **Knowledge Graph Viewer**
2. Filter by tool: "crash_analyzer"
3. See crash knowledge connected to:
   - Code snippets (solutions)
   - Documentation (prevention)
   - Error explanations (understanding)

#### Step 5.3: Crash Analytics
1. Return to **Crash Analyzer**
2. Open "🧠 Crash Knowledge Analytics"
3. Review:
   - Total Patterns: 3+
   - High Confidence: Patterns validated multiple times
   - Most Common: Your validated patterns
   - Recent Activity: Last 7 days

### Phase 6: Advanced Testing

#### Step 6.1: Pattern Evolution
1. Upload same crash type but with slightly different stack trace
2. System recognizes as variant of existing pattern
3. Save with new solution that worked better
4. Check pattern details - now has 2 solutions with success counts

#### Step 6.2: Cross-Site Analysis
1. Upload crashes from different What's On sites
2. Filter Crash Pattern Network by site
3. Identify site-specific vs universal patterns

#### Step 6.3: Version Regression Detection
1. Upload older crash that was fixed
2. Then upload same crash from newer version
3. System detects regression pattern

## Expected Results

### Crash Knowledge Base Contains:
- **Patterns**: 3-5 different crash patterns
- **Instances**: 6-10 individual crashes
- **Relationships**: 5+ connections to general knowledge
- **Solutions**: Multiple proven fixes with success tracking

### Knowledge Integration Shows:
- Crash analyses in knowledge search results
- Relationships between crashes and code/docs
- Quality-scored crash knowledge
- Crash patterns in knowledge graph

### Business Intelligence Provides:
- Total downtime by pattern
- Most problematic features
- Crash frequency trends
- Pattern confidence growth

## Validation Checklist

- [ ] New crashes create patterns
- [ ] Similar crashes match existing patterns
- [ ] Pattern confidence increases with validation
- [ ] Solutions accumulate and track success
- [ ] Crash knowledge appears in general knowledge base
- [ ] Relationships connect crashes to solutions
- [ ] Pattern network visualizes connections
- [ ] Analytics show trends and impacts
- [ ] Business impact data is preserved
- [ ] Evolution tracking shows pattern changes

## Troubleshooting

### Pattern Not Matching
- Check if fingerprints are too specific
- Try lowering match threshold
- Verify stack traces are similar enough

### Relationships Not Found
- Ensure knowledge quality scores > 30
- Run relationship discovery after creating knowledge
- Check if content has enough keyword overlap

### Missing from Knowledge Graph
- Verify knowledge_id is set in crash_instances
- Check knowledge capture is enabled
- Ensure quality score meets threshold

## Advanced Scenarios

### 1. Cascading Failures
Upload crashes that caused other crashes to test relationship type "causes"

### 2. Solution Effectiveness
Track which solutions have highest success rates across different sites

### 3. Predictive Analysis
After 20+ crashes, check if system predicts problem areas

This comprehensive test will validate that the Crash Knowledge System is fully operational and integrated with TuoKit's knowledge infrastructure!