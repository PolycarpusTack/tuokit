"""
Crash Analyzer Configuration
All configuration constants and templates
"""

# Main configuration
CRASH_ANALYZER_CONFIG = {
    "max_file_size_mb": 5,
    "chunk_size": 8000,
    "chunk_overlap": 400,
    "max_chunks": 700,  # Supports up to 5MB files
    "smart_sampling_threshold_mb": 1,
    "performance_warning_threshold_seconds": 30,
    "pattern_match_context_chars": 100,
    "enable_abort": True,
    "chunk_processing_delay": 0.1,
    "skip_empty_chunks": True,  # Skip chunks without error keywords
    "max_consecutive_failures": 10  # Stop after too many failures
}

# Expert analysis prompt template
EXPERT_PROMPT_TEMPLATE = """You are an expert Whats'On/VisualWorks investigator with deep knowledge of Smalltalk architecture. 
Think rigorously step-by-step, but output only the final report. Hide intermediate reasoning.

## Task
Analyse the runtime diagnostic dump below and deliver an actionable report.

## Report sections (use exactly these headings)

0. TOP 3 FINDINGS
   1) <one-sentence insight> — evidence: line/section reference
   2) <one-sentence insight> — evidence: line/section reference
   3) <one-sentence insight> — evidence: line/section reference

1. OVERVIEW  
   • Who: user, computer, site, build & DB info  
   • When: timestamp and temporal correlation
   • What: exception class & message  
   • Severity: [Critical/High/Medium/Low]

2. ELI5: WHAT HAPPENED
   • Explain in 3-5 simple sentences using everyday analogies
   • No technical jargon - think "explaining to your grandma"
   • Use a real-world metaphor

3. ROOT-CAUSE ANALYSIS  
   a. Failing call-chain summary
   b. Plain-English explanation of key frames
   c. Most likely cause with confidence level
   d. Match against known patterns or state "Unknown"

4. USER IMPACT  
   • What the user saw/lost
   • Functionality affected
   • Data loss risk

5. FIX PROPOSALS  
   • Quick operator win 🟢/🟡/🔴
   • Code hot-fix 🟢/🟡/🔴
   • Prevention strategy 🟢/🟡/🔴

6. NEXT STEPS  
   • Action items with priority
   • Questions for investigation

## Output format
Markdown with concise bullets. Quote dump lines sparingly.

## Runtime Diagnostic Dump
{crash_content}"""

# File size thresholds for analysis method selection
ANALYSIS_METHOD_THRESHOLDS = {
    "standard_max_kb": 100,
    "extraction_max_mb": 1,
    "sampling_recommended_mb": 1,
    "full_analysis_warning_mb": 2
}

# Model selection preferences (keywords to look for, not hardcoded values)
MODEL_PREFERENCE_KEYWORDS = [
    "deepseek",
    "llama",
    "mistral",
    "gpt",
    "claude"
]

# Severity levels with visual indicators
SEVERITY_INDICATORS = {
    "Critical": {"emoji": "🔴", "color": "#FF0000", "priority": 4},
    "High": {"emoji": "🟠", "color": "#FF6600", "priority": 3},
    "Medium": {"emoji": "🟡", "color": "#FFB800", "priority": 2},
    "Low": {"emoji": "🟢", "color": "#00CC00", "priority": 1},
    "Unknown": {"emoji": "⚪", "color": "#666666", "priority": 0}
}