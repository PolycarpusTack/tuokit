# Analysis Method 3: Strategic Sampling

## Overview
**Name:** Strategic Sampling Analysis  
**Duration:** 1-2 minutes  
**AI Required:** Optional (enhanced results with AI)  
**Best For:** Large files (1-5MB), statistical analysis, performance issues

## How It Works

Strategic Sampling intelligently analyzes large files without processing every byte. It identifies high-value sections (error clusters, time gaps, critical regions) and samples them for deep analysis, providing statistical confidence metrics.

### Key Components:
1. **Sampling Map Creation**: Identifies important regions
2. **Error Clustering**: Groups related errors
3. **Time Gap Detection**: Finds temporal anomalies
4. **Statistical Analysis**: Confidence scoring
5. **Performance Profiling**: Resource usage patterns

## Analysis Process

### 1. Sampling Strategy
```python
SAMPLING_STRATEGY = {
    "sample_size": 102400,  # 100KB per sample
    "priority_sections": [
        {"name": "file_start", "position": "start", "size": 102400},
        {"name": "file_end", "position": "end", "size": 102400},
        {"name": "error_clusters", "position": "around_errors", "size": 51200}
    ],
    "anomaly_detection": {
        "time_gap_threshold": 60,  # seconds
        "error_cluster_threshold": 3,  # min errors to form cluster
        "pattern_significance": 0.05
    }
}
```

### 2. Intelligent Sampling
- Finds error-dense regions
- Detects time discontinuities
- Samples around anomalies
- Maintains statistical validity

## AI Prompts Used (When Available)

### Section Analysis Prompt:
```
Analyze this section from a crash dump (section: {section_name}).
Focus on identifying errors, patterns, and potential issues.

Section content:
{section_content[:2000]}

Provide analysis in JSON format:
{
    "primary_issues": ["issue 1", "issue 2"],
    "error_types": ["type 1", "type 2"],
    "severity": "CRITICAL/HIGH/MEDIUM/LOW",
    "patterns": ["pattern 1", "pattern 2"],
    "recommendations": ["recommendation 1"]
}
```

## Example with WCR_2-4_16-3-4.txt

### Expected Output:

```markdown
### 🎯 Strategic Sampling Results

#### Coverage Summary
- **File Coverage**: 15.2%
- **Sections Analyzed**: 4
- **Sampled Size**: 204,800 bytes
- **Confidence**: 87%

#### 🚨 Critical Sections

**error_cluster_8355** - HIGH
• UndefinedObject(Object)>>doesNotUnderstand: cluster
• Multiple stack frames showing cascade failure
• Density: 15 error indicators in 1KB region

**file_start** - MEDIUM  
• System configuration and environment setup
• Database connection parameters exposed
• Version mismatch indicators detected

#### 🔄 Recurring Errors
• doesNotUnderstand: **12 occurrences**
• Message not understood: **8 occurrences**
• #isRelativeToEachAiring: **8 occurrences**

#### ⚡ Performance Insights
⚠️ Memory pressure detected
- Heap usage indicators suggest memory constraints
- Multiple collection operations before crash
- Thread count: 50+ active threads

🔥 Resource bottlenecks: memory, thread_contention

#### 🔒 Security Concerns
**⚠️ SECURITY CONCERNS FOUND**
- Database connection string in plain text (oracle:mgx-db-28:1521/s28db13x)
- Username visible in logs (psi)
- Consider log sanitization

#### Pattern Analysis

**Error Sequences Detected**
- In error_cluster_8355: doesNotUnderstand → containsWithoutLoading → ensure:
- Pattern suggests defensive programming failure

**Component Correlation**
- MediaGeniX.CM2LicenseWindow: [method_missing, null_reference]
- MediaGeniX.T3ChildSet: [enumeration_error, state_corruption]

#### 📊 Sampling Map Visualization
```
File: [========|===ERROR CLUSTER===|=========|=======]
       ↑        ↑                   ↑         ↑
    Start    Error Dense         Time Gap    End
   Sample      Region           Detected   Sample
```

#### Confidence Metrics
- **Coverage Score**: 0.9 (High coverage >20%)
- **Pattern Confidence**: 0.85 (Strong pattern recurrence)
- **Error Detection**: 0.85 (Error clustering detected)
- **Overall Confidence**: 87%

#### 📋 Recommended Next Steps
→ Deep dive into error_cluster_8355 for root cause
→ Review MediaGeniX.CM2LicenseWindow implementation
→ Security audit for exposed credentials
→ Memory profiling for heap pressure issues

#### Statistical Summary
- **Error Density**: 12.5 errors/KB in critical sections
- **Time Gaps**: 0 significant gaps detected
- **Pattern Significance**: p < 0.01 for doesNotUnderstand pattern
```

## Sampling Algorithm

### Error Cluster Detection:
```python
def find_error_clusters(content, threshold=3):
    error_positions = []
    for match in re.finditer(r'(?i)(error|exception|fail)', content):
        error_positions.append(match.start())
    
    clusters = []
    current_cluster = [error_positions[0]]
    
    for pos in error_positions[1:]:
        if pos - current_cluster[-1] < 1000:  # Within 1KB
            current_cluster.append(pos)
        else:
            if len(current_cluster) >= threshold:
                clusters.append({
                    "start": current_cluster[0],
                    "end": current_cluster[-1],
                    "density": len(current_cluster)
                })
            current_cluster = [pos]
```

## When to Use

### ✅ Use Strategic Sampling When:
- Files are 1-5MB in size
- Need quick insights on large dumps
- Looking for patterns across file
- Performance analysis required
- Statistical confidence needed

### ❌ Don't Use When:
- Files < 1MB (use Root Cause)
- Need 100% coverage (use Deep Forensic)
- Simple error detection (use Quick Triage)
- Real-time analysis required

## Integration Points

Strategic Sampling integrates with:
1. **Large File Processing Pipelines**
2. **Batch Analysis Systems**
3. **Pattern Detection Databases**
4. **Performance Monitoring Tools**

## Performance Characteristics

- **Speed**: 60-120 seconds typically
- **Memory**: ~100MB overhead
- **CPU**: Moderate, spike during clustering
- **Accuracy**: 85-90% with statistical confidence