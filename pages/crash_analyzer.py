"""
TuoKit - Crash Analyzer Pro
Enhanced crash dump analysis with expert diagnostics
Practical implementation: Basic analysis for speed, Expert mode for depth
"""
import streamlit as st
import json
import hashlib
import re
from datetime import datetime
from utils import (
    DatabaseManager, 
    safe_ollama_generate,
    capture_knowledge,
    validate_file_size
)

# Initialize database
db = DatabaseManager()

# Known crash patterns for quick identification
KNOWN_PATTERNS = {
    "NullPointerException": {
        "pattern": r"NullPointerException|NPE|null.*reference|nil.*reference",
        "quick_fix": "Add null checks before object access: if (obj != null)",
        "prevention": "Use defensive programming and Optional types"
    },
    "OutOfMemoryError": {
        "pattern": r"OutOfMemoryError|OOM|heap.*space|memory.*exhausted",
        "quick_fix": "Increase JVM heap size: -Xmx2g or -Xmx4g",
        "prevention": "Profile memory usage, fix memory leaks, optimize collections"
    },
    "StackOverflow": {
        "pattern": r"StackOverflowError|stack.*overflow|recursion.*limit",
        "quick_fix": "Check for infinite recursion or circular references",
        "prevention": "Add recursion depth limits and base cases"
    },
    "DatabaseTimeout": {
        "pattern": r"timeout|connection.*timed.*out|query.*timeout",
        "quick_fix": "Increase timeout settings or optimize query",
        "prevention": "Add indexes, optimize queries, use connection pooling"
    },
    "FileNotFound": {
        "pattern": r"FileNotFoundException|file.*not.*found|no.*such.*file",
        "quick_fix": "Verify file path and permissions",
        "prevention": "Add file existence checks before access"
    }
}

def match_known_patterns(content):
    """Match crash content against known patterns"""
    for pattern_name, pattern_info in KNOWN_PATTERNS.items():
        if re.search(pattern_info["pattern"], content, re.IGNORECASE):
            return {
                "pattern": pattern_name,
                "quick_fix": pattern_info["quick_fix"],
                "prevention": pattern_info["prevention"]
            }
    return None

def find_similar_crashes(error_type, limit=5):
    """Find crashes with similar error patterns"""
    try:
        return db.fetch_all("""
            SELECT filename, 
                   analysis->>'severity' as severity,
                   analysis->>'root_cause' as root_cause,
                   validated_by,
                   created_at
            FROM crash_analysis
            WHERE analysis->>'error_type' ILIKE %s
               OR analysis->>'root_cause' ILIKE %s
            ORDER BY created_at DESC
            LIMIT %s
        """, (f"%{error_type}%", f"%{error_type}%", limit))
    except:
        return []

def export_crash_report(filename, analysis, expert_report=None):
    """Export crash analysis as markdown"""
    severity_emoji = {
        "Critical": "🔴", "High": "🟠", 
        "Medium": "🟡", "Low": "🟢"
    }.get(analysis.get("severity", "Medium"), "⚪")
    
    report = f"""# Crash Analysis Report

**File**: {filename}  
**Date**: {datetime.now().strftime('%Y-%m-%d %H:%M')}  
**Severity**: {severity_emoji} {analysis.get('severity', 'Unknown')}

## Summary
- **Error Type**: {analysis.get('error_type', 'Unknown')}
- **Location**: {analysis.get('error_location', 'Unknown')}
- **Root Cause**: {analysis.get('root_cause', 'Not identified')}

## Quick Fix
{analysis.get('quick_fix', 'No quick fix available')}

## Prevention
{analysis.get('prevention', 'No prevention strategy defined')}
"""
    
    if expert_report:
        report += f"\n---\n\n## Expert Analysis\n\n{expert_report}"
    
    return report

# Expert prompt template for comprehensive analysis
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

def analyze_crash_dump(content):
    """Basic crash analysis - fast JSON response"""
    truncated = content[:15000] if len(content) > 15000 else content
    
    prompt = f"""Analyze this crash dump and provide a structured analysis.

CRASH DUMP:
{truncated}

Provide your analysis in this exact JSON format:
{{
    "root_cause": "Brief technical description of what caused the crash",
    "error_location": "File/method/line where crash occurred",
    "error_type": "Exception type or error category",  
    "severity": "Critical/High/Medium/Low",
    "quick_fix": "Immediate solution to resolve the crash",
    "prevention": "How to prevent this in the future"
}}

Be concise and technical. Focus only on the actual error."""
    
    try:
        response = safe_ollama_generate(
            model="deepseek-r1:latest",
            prompt=prompt,
            format="json"
        )
        
        analysis = json.loads(response)
        
        # Ensure all required fields
        required_fields = ["root_cause", "error_location", "error_type", 
                          "severity", "quick_fix", "prevention"]
        for field in required_fields:
            if field not in analysis:
                analysis[field] = "Not identified"
                
        return analysis
        
    except (json.JSONDecodeError, Exception) as e:
        return {
            "root_cause": "Analysis failed - check crash dump format",
            "error_location": "Unknown",
            "error_type": str(type(e).__name__),
            "severity": "Medium",
            "quick_fix": "Review the crash dump manually",
            "prevention": "Ensure proper error handling"
        }

def generate_expert_report(content):
    """Generate detailed expert report - comprehensive but slower"""
    truncated = content[:10000] if len(content) > 10000 else content
    prompt = EXPERT_PROMPT_TEMPLATE.format(crash_content=truncated)
    
    return safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=prompt,
        temperature=0.3  # Lower temperature for more focused analysis
    )

def extract_mermaid_diagrams(text):
    """Extract Mermaid diagrams from markdown text"""
    diagrams = []
    lines = text.split('\n')
    in_mermaid = False
    current_diagram = []
    
    for line in lines:
        if line.strip().startswith("```mermaid"):
            in_mermaid = True
            continue
        elif in_mermaid and line.strip().startswith("```"):
            in_mermaid = False
            if current_diagram:
                diagrams.append('\n'.join(current_diagram))
                current_diagram = []
        elif in_mermaid:
            current_diagram.append(line)
    
    return diagrams

def save_crash_analysis(filename, content_hash, analysis, expert_report, validated_by, include_expert):
    """Save validated analysis to database"""
    try:
        # Save to crash_analysis table
        db.execute(
            """INSERT INTO crash_analysis 
            (filename, content_hash, analysis, expert_report, validated_by, created_at) 
            VALUES (%s, %s, %s, %s, %s, %s)
            ON CONFLICT (content_hash) DO UPDATE 
            SET analysis = %s, expert_report = %s, validated_by = %s, created_at = %s""",
            (filename, content_hash, json.dumps(analysis), 
             expert_report if include_expert else None, validated_by, datetime.now(),
             json.dumps(analysis), expert_report if include_expert else None, 
             validated_by, datetime.now())
        )
        
        # Capture to general knowledge base
        capture_knowledge(
            tool_name="crash_analyzer",
            prompt=f"Analyze crash in {filename}",
            response=json.dumps(analysis, indent=2) + 
                    ("\n\n---EXPERT REPORT---\n" + expert_report if include_expert and expert_report else ""),
            metadata={
                "filename": filename,
                "severity": analysis.get("severity", "Unknown"),
                "validated_by": validated_by,
                "has_expert_report": include_expert
            }
        )
        return True
    except Exception as e:
        st.error(f"Database error: {str(e)}")
        return False

def show():
    st.title("🚨 Crash Analyzer Pro")
    st.caption("AI-powered crash analysis with optional expert diagnostics")
    
    # File upload
    uploaded_file = st.file_uploader(
        "Upload crash dump", 
        type=["txt", "log", "dmp", "wcr"],
        help="Maximum file size: 5MB. Supports .txt, .log, .dmp, .wcr formats"
    )
    
    if uploaded_file:
        # Validate file size
        if not validate_file_size(uploaded_file, max_size_mb=5):
            st.error("File too large. Maximum size is 5MB.")
            return
            
        # Read content
        try:
            content = uploaded_file.getvalue().decode("utf-8", errors='ignore')
            content_hash = hashlib.sha256(content.encode()).hexdigest()[:16]
        except Exception as e:
            st.error(f"Error reading file: {str(e)}")
            return
        
        # File info
        col1, col2, col3 = st.columns([2, 1, 1])
        with col1:
            st.info(f"📄 **File:** {uploaded_file.name}")
        with col2:
            st.info(f"**Size:** {len(content):,} bytes")
        with col3:
            st.info(f"**Hash:** {content_hash}")
        
        # Content preview
        with st.expander("📋 View Crash Dump", expanded=False):
            st.code(content[:2000] + "..." if len(content) > 2000 else content)
        
        # Analysis options
        st.subheader("🔍 Analysis Options")
        col1, col2 = st.columns(2)
        
        analysis = None
        expert_report = None
        
        with col1:
            if st.button("⚡ Basic Analysis", use_container_width=True, 
                        help="Fast JSON-based technical analysis"):
                with st.spinner("Analyzing crash dump..."):
                    analysis = analyze_crash_dump(content)
                st.success("Basic analysis complete!")
        
        with col2:
            if st.button("🕵️ Expert Diagnostics", use_container_width=True,
                        help="Comprehensive report with ELI5 explanation (1-2 min)"):
                with st.spinner("Generating expert report... This may take 1-2 minutes"):
                    expert_report = generate_expert_report(content)
                st.success("Expert report generated!")
        
        # Display results
        if analysis:
            st.subheader("📊 Basic Analysis Results")
            
            # Severity indicator
            severity_color = {
                "Critical": "🔴", "High": "🟠", 
                "Medium": "🟡", "Low": "🟢"
            }.get(analysis.get("severity", "Medium"), "⚪")
            
            st.markdown(f"**Severity:** {severity_color} {analysis.get('severity', 'Unknown')}")
            
            # Check for known patterns
            pattern_match = match_known_patterns(content)
            if pattern_match:
                st.info(f"🔍 **Known Pattern Detected:** {pattern_match['pattern']}")
                with st.expander("Pattern-based recommendations"):
                    st.markdown(f"**Quick Fix:** {pattern_match['quick_fix']}")
                    st.markdown(f"**Prevention:** {pattern_match['prevention']}")
            
            # Editable analysis fields
            col1, col2 = st.columns(2)
            
            with col1:
                analysis["root_cause"] = st.text_area(
                    "Root Cause",
                    value=analysis["root_cause"],
                    height=100
                )
                analysis["error_location"] = st.text_input(
                    "Error Location",
                    value=analysis["error_location"]
                )
                analysis["error_type"] = st.text_input(
                    "Error Type",
                    value=analysis["error_type"]
                )
                
            with col2:
                analysis["severity"] = st.selectbox(
                    "Severity",
                    ["Critical", "High", "Medium", "Low"],
                    index=["Critical", "High", "Medium", "Low"].index(
                        analysis.get("severity", "Medium"))
                )
                analysis["quick_fix"] = st.text_area(
                    "Quick Fix",
                    value=analysis["quick_fix"],
                    height=100
                )
                analysis["prevention"] = st.text_area(
                    "Prevention Strategy",
                    value=analysis["prevention"],
                    height=100
                )
        
        if expert_report:
            st.subheader("📝 Expert Diagnostic Report")
            
            # Full report in expander
            with st.expander("View Full Expert Report", expanded=True):
                st.markdown(expert_report)
            
            # Extract and display any Mermaid diagrams
            diagrams = extract_mermaid_diagrams(expert_report)
            if diagrams:
                st.subheader("📊 Visualizations")
                for i, diagram in enumerate(diagrams):
                    st.caption(f"Diagram {i+1}")
                    st.code(diagram, language="mermaid")
        
        # Show similar crashes if analysis available
        if analysis and analysis.get("error_type"):
            similar = find_similar_crashes(analysis["error_type"], limit=5)
            if similar:
                st.subheader("🔗 Similar Previous Crashes")
                for crash in similar:
                    severity_emoji = {
                        "Critical": "🔴", "High": "🟠",
                        "Medium": "🟡", "Low": "🟢"
                    }.get(crash['severity'], "⚪")
                    
                    st.markdown(f"""
                    {severity_emoji} **{crash['filename']}** - {crash['created_at'].strftime('%Y-%m-%d')}
                    - Root cause: {crash['root_cause'][:100]}...
                    - Fixed by: {crash['validated_by']}
                    """)
        
        # Save section
        if analysis or expert_report:
            st.divider()
            st.subheader("💾 Save to Knowledge Base")
            
            validator_name = st.text_input(
                "Your Name (required for validation)",
                placeholder="Enter your name"
            )
            
            include_expert = False
            if expert_report:
                include_expert = st.checkbox(
                    "Include expert report in knowledge base", 
                    value=True,
                    help="Expert reports provide valuable context for future reference"
                )
            
            col1, col2, col3 = st.columns([2, 1, 1])
            with col1:
                if st.button("💾 Save Validated Analysis", type="primary", use_container_width=True):
                    if not validator_name:
                        st.warning("⚠️ Please enter your name for validation")
                    else:
                        if save_crash_analysis(
                            uploaded_file.name,
                            content_hash,
                            analysis or {},
                            expert_report,
                            validator_name,
                            include_expert
                        ):
                            st.success("✅ Analysis saved to knowledge base!")
                            st.balloons()
            
            with col2:
                quality = st.number_input("Quality", 1, 5, 3, help="Rate analysis quality")
            
            with col3:
                if st.button("📄 Export Report", use_container_width=True):
                    report = export_crash_report(
                        uploaded_file.name,
                        analysis or {},
                        expert_report if include_expert else None
                    )
                    st.download_button(
                        label="Download Report",
                        data=report,
                        file_name=f"crash_report_{uploaded_file.name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md",
                        mime="text/markdown"
                    )
            
            # Quick navigation
            if validator_name:
                st.markdown("### 🔗 Next Steps")
                col1, col2, col3 = st.columns(3)
                with col1:
                    if st.button("📚 Knowledge Library"):
                        st.switch_page("pages/knowledge_lib.py")
                with col2:
                    if "sql" in str(analysis.get("error_type", "")).lower():
                        if st.button("🐬 SQL Pipeline"):
                            st.switch_page("pages/sql_pipeline.py")
                with col3:
                    if st.button("🧠 Code Explainer"):
                        st.switch_page("pages/code_tools.py")
    
    # Recent analyses
    with st.expander("📋 Recent Crash Analyses", expanded=False):
        try:
            recent = db.fetch_all(
                """SELECT filename, analysis->>'severity' as severity, 
                   analysis->>'root_cause' as root_cause, 
                   validated_by, created_at,
                   CASE WHEN expert_report IS NOT NULL THEN TRUE ELSE FALSE END as has_expert
                   FROM crash_analysis 
                   ORDER BY created_at DESC LIMIT 10"""
            )
            
            if recent:
                for entry in recent:
                    severity_emoji = {
                        "Critical": "🔴", "High": "🟠", 
                        "Medium": "🟡", "Low": "🟢"
                    }.get(entry['severity'], "⚪")
                    
                    expert_badge = "📝" if entry['has_expert'] else ""
                    
                    st.markdown(f"""
                    {severity_emoji} **{entry['filename']}** {expert_badge}
                    - Root cause: {entry['root_cause'][:80]}...
                    - By: {entry['validated_by']} on {entry['created_at'].strftime('%Y-%m-%d %H:%M')}
                    """)
            else:
                st.info("No crash analyses found yet")
        except:
            st.info("Initialize the database to see recent analyses")
    
    # Database setup (collapsible)
    with st.expander("🔧 Database Setup", expanded=False):
        st.markdown("### Initialize Crash Analysis Database")
        st.code("""
-- Enhanced table with expert report support
CREATE TABLE IF NOT EXISTS crash_analysis (
    id SERIAL PRIMARY KEY,
    filename TEXT NOT NULL,
    content_hash VARCHAR(32) UNIQUE,
    analysis JSONB NOT NULL,
    expert_report TEXT,
    validated_by TEXT NOT NULL,
    quality_rating INTEGER DEFAULT 3,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_crash_severity 
ON crash_analysis((analysis->>'severity'));

CREATE INDEX IF NOT EXISTS idx_crash_created 
ON crash_analysis(created_at DESC);
        """)
        
        if st.button("🚀 Initialize Database Tables"):
            try:
                db.execute("""
                    CREATE TABLE IF NOT EXISTS crash_analysis (
                        id SERIAL PRIMARY KEY,
                        filename TEXT NOT NULL,
                        content_hash VARCHAR(32) UNIQUE,
                        analysis JSONB NOT NULL,
                        expert_report TEXT,
                        validated_by TEXT NOT NULL,
                        quality_rating INTEGER DEFAULT 3,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                """)
                db.execute("""
                    CREATE INDEX IF NOT EXISTS idx_crash_severity 
                    ON crash_analysis((analysis->>'severity'))
                """)
                db.execute("""
                    CREATE INDEX IF NOT EXISTS idx_crash_created 
                    ON crash_analysis(created_at DESC)
                """)
                st.success("✅ Database initialized successfully!")
            except Exception as e:
                st.error(f"Error: {str(e)}")

# TODO: Add crash pattern recognition across multiple dumps
# TODO: Implement similarity search for finding related crashes
# TODO: Add export functionality for management reports
# TODO: Create crash statistics dashboard

if __name__ == "__main__":
    show()
