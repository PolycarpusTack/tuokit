"""
TuoKit Crash Analyzer Main Class
Inherits from TuoKitToolBase for automatic knowledge capture
"""
import streamlit as st
import hashlib
from datetime import datetime
from typing import Optional, Dict, Any

from utils.tool_base import TuoKitToolBase
from utils import DatabaseManager, validate_file_size

from .config import CRASH_ANALYZER_CONFIG, ANALYSIS_METHOD_THRESHOLDS
from .processors import (
    analyze_crash_dump,
    generate_expert_report,
    analyze_with_chunking,
    generate_strategic_samples
)
from .ui_components import (
    show_model_selection,
    show_file_analysis_status,
    show_performance_stats,
    show_pattern_matches,
    show_analysis_results,
    show_chunk_analysis_results,
    show_save_section,
    show_crash_statistics_dashboard
)
from .patterns import match_known_patterns
from .wcr_handler import is_wcr_format, get_wcr_summary, format_wcr_display
from .extractors import extract_critical_sections, extract_mermaid_diagrams

class CrashAnalyzer(TuoKitToolBase):
    """
    Professional crash dump analysis tool with AI-powered diagnostics
    """
    
    def __init__(self):
        super().__init__(
            tool_name="Crash Analyzer Pro",
            tool_description="AI-powered crash analysis with intelligent diagnostics - supports files up to 5MB"
        )
        self.db = self._get_database()
    
    def _get_database(self) -> Optional[DatabaseManager]:
        """Get database instance with proper error handling"""
        if "db" not in st.session_state:
            try:
                st.session_state.db = DatabaseManager()
            except Exception as e:
                st.error(f"Database connection failed: {e}")
                st.session_state.db = None
        return st.session_state.db
    
    def run(self):
        """Main entry point for the crash analyzer"""
        st.title("🚨 Crash Analyzer Pro")
        st.caption(self.tool_description)
        
        # Initialize session state
        self._init_session_state()
        
        # File upload
        uploaded_file = st.file_uploader(
            "Upload crash dump", 
            type=["txt", "log", "dmp", "wcr", "crash", "error"],
            help=f"Maximum file size: {CRASH_ANALYZER_CONFIG['max_file_size_mb']}MB. Full processing available for all sizes."
        )
        
        if uploaded_file:
            self._process_uploaded_file(uploaded_file)
        
        # Show additional UI sections
        self._show_additional_sections()
    
    def _init_session_state(self):
        """Initialize session state variables"""
        if 'analysis' not in st.session_state:
            st.session_state.analysis = None
        if 'expert_report' not in st.session_state:
            st.session_state.expert_report = None
        if 'chunk_analysis' not in st.session_state:
            st.session_state.chunk_analysis = None
    
    def _process_uploaded_file(self, uploaded_file):
        """Process the uploaded crash dump file"""
        # Validate file size
        if not validate_file_size(uploaded_file, max_size_mb=CRASH_ANALYZER_CONFIG['max_file_size_mb']):
            st.error(f"File too large. Maximum size is {CRASH_ANALYZER_CONFIG['max_file_size_mb']}MB.")
            return
        
        # Read content
        try:
            content = uploaded_file.getvalue().decode("utf-8", errors='ignore')
            content_hash = hashlib.sha256(content.encode()).hexdigest()[:16]
        except Exception as e:
            st.error(f"Error reading file: {str(e)}")
            return
        
        # Determine analysis method
        method, warning = self._determine_analysis_method(len(content))
        st.session_state.last_method_used = method.replace("_", " ").title()
        
        # Show file info
        self._show_file_info(uploaded_file, content, content_hash, warning)
        
        # Show file insights
        show_file_analysis_status(content)
        
        # Check for WCR format
        self._check_wcr_format(content)
        
        # Pattern detection
        self._detect_patterns(content)
        
        # Content preview
        self._show_content_preview(content)
        
        # Model selection
        selected_model = show_model_selection()
        
        # Only show analysis methods if a model is available
        if selected_model:
            # Analysis methods
            self._show_analysis_methods(content, selected_model)
        else:
            st.warning("⚠️ **Analysis unavailable** - Please start Ollama and install at least one model to proceed")
            st.code("# Start Ollama:\nollama serve\n\n# Install a model:\nollama pull deepseek-r1:latest", language="bash")
        
        # Show performance stats
        show_performance_stats()
        
        # Check for known crash patterns
        self._check_known_patterns(content)
        
        # Display results
        self._display_results()
        
        # Save section
        if any([st.session_state.analysis, st.session_state.expert_report, st.session_state.chunk_analysis]):
            analysis_to_use = (st.session_state.analysis or 
                             st.session_state.chunk_analysis or 
                             {})
            show_save_section(
                analysis_to_use,
                st.session_state.expert_report,
                uploaded_file.name,
                content_hash,
                content,
                selected_model,
                self.db
            )
    
    def _determine_analysis_method(self, content_size: int) -> tuple:
        """Determine best analysis method based on file size"""
        size_mb = content_size / (1024 * 1024)
        
        if size_mb < ANALYSIS_METHOD_THRESHOLDS["standard_max_kb"] / 1024:
            return "standard", None
        elif size_mb < ANALYSIS_METHOD_THRESHOLDS["extraction_max_mb"]:
            return "standard_with_extraction", None
        elif size_mb < ANALYSIS_METHOD_THRESHOLDS["sampling_recommended_mb"]:
            return "full_chunking", f"File is {size_mb:.1f}MB. Full analysis may take {int(size_mb * 30)} seconds."
        else:
            chunks_estimate = int(content_size / CRASH_ANALYZER_CONFIG["chunk_size"])
            time_estimate = chunks_estimate * 2  # ~2 seconds per chunk
            return "full_chunking", f"File is {size_mb:.1f}MB ({chunks_estimate} chunks, ~{time_estimate//60}m {time_estimate%60}s)"
    
    def _show_file_info(self, uploaded_file, content: str, content_hash: str, warning: Optional[str]):
        """Display file information header"""
        st.markdown(f"### 📄 Analyzing: {uploaded_file.name}")
        cols = st.columns(4)
        cols[0].info(f"**Size:** {len(content):,} bytes")
        cols[1].info(f"**Hash:** `{content_hash}`")
        cols[2].info(f"**Method:** {st.session_state.last_method_used}")
        cols[3].info(f"**Modified:** {datetime.now().strftime('%Y-%m-%d %H:%M')}")
        
        if warning:
            st.warning(f"⚠️ {warning}")
    
    def _check_wcr_format(self, content: str):
        """Check and display WCR format information"""
        if is_wcr_format(content):
            st.info("🎯 **What's On Crash Report (WCR) Detected** - Smalltalk/VisualWorks crash dump")
            
            wcr_summary = get_wcr_summary(content)
            
            with st.expander("📋 WCR Quick Summary", expanded=True):
                col1, col2, col3 = st.columns(3)
                with col1:
                    cause = wcr_summary['cause']
                    if len(cause) > 50:
                        cause = cause[:50] + "..."
                    st.metric("Primary Cause", cause)
                    st.caption(f"Category: {wcr_summary['category']}")
                with col2:
                    st.metric("User/Site", f"{wcr_summary['user']} @ {wcr_summary['site']}")
                    st.caption(f"Version: {wcr_summary['version']}")
                with col3:
                    st.metric("Timestamp", wcr_summary['timestamp'])
                    if wcr_summary['matched_patterns']:
                        st.caption(f"Patterns: {len(wcr_summary['matched_patterns'])} matched")
                
                if wcr_summary['immediate_error'].get('class'):
                    st.write("**Immediate Error Location:**")
                    st.code(f"{wcr_summary['immediate_error']['class']}>>>{wcr_summary['immediate_error']['method']}")
    
    def _detect_patterns(self, content: str):
        """Detect and display known error patterns"""
        with st.spinner("🔍 Scanning for known patterns..."):
            pattern_matches = match_known_patterns(content)
        
        if pattern_matches:
            show_pattern_matches(pattern_matches)
    
    def _show_content_preview(self, content: str):
        """Show crash dump content preview"""
        with st.expander("📋 View Crash Dump", expanded=False):
            preview_type = st.radio("Preview type", ["First 3KB", "Smart Extract", "Full (slow)"], horizontal=True)
            
            if preview_type == "First 3KB":
                st.code(content[:3000] + ("..." if len(content) > 3000 else ""))
            elif preview_type == "Smart Extract":
                extracted = extract_critical_sections(content, 5000)
                st.code(extracted)
            else:
                st.code(content)
    
    def _show_analysis_methods(self, content: str, selected_model: str):
        """Show analysis method buttons"""
        st.markdown("### 🔍 Analysis Methods")
        
        if selected_model:
            st.info(f"🤖 **Using Model:** `{selected_model}` for all analysis methods")
        
        method_cols = st.columns(4)
        
        # Basic Analysis
        with method_cols[0]:
            if st.button("⚡ Basic Analysis", use_container_width=True, 
                        help="Fast analysis using critical sections (5-15 seconds)",
                        type="primary"):
                with st.spinner("Analyzing critical sections..."):
                    # Use generate_with_capture for automatic knowledge capture
                    result = self.generate_with_capture(
                        prompt=self._build_basic_analysis_prompt(content),
                        model=selected_model,
                        temperature=0.3
                    )
                    
                    if not result['error']:
                        st.session_state.analysis = self._parse_basic_analysis(result['response'])
                    else:
                        st.session_state.analysis = analyze_crash_dump(content)
                    
                    st.session_state.expert_report = None
                    st.session_state.chunk_analysis = None
                st.success("✅ Basic analysis complete!")
                st.toast("Analysis complete!", icon="✅")
        
        # Expert Diagnostics
        with method_cols[1]:
            if st.button("🕵️ Expert Diagnostics", use_container_width=True,
                        help="Comprehensive report with actionable insights (30-60 seconds)"):
                with st.spinner("Generating expert report..."):
                    st.session_state.expert_report = generate_expert_report(content)
                    st.session_state.analysis = None
                    st.session_state.chunk_analysis = None
                st.success("✅ Expert report generated!")
                st.toast("Expert report ready!", icon="📝")
        
        # Smart Sampling
        with method_cols[2]:
            if st.button("📊 Smart Sampling", use_container_width=True,
                        help="Analyze strategic samples for quick insights"):
                with st.spinner("Analyzing strategic samples..."):
                    samples = generate_strategic_samples(content)
                    
                    st.info(f"📍 Extracted {len(samples)} strategic samples")
                    
                    combined_samples = "\n\n--- SAMPLE BOUNDARY ---\n\n".join([s['content'] for s in samples])
                    st.session_state.analysis = analyze_crash_dump(combined_samples)
                    
                    st.session_state.analysis['sample_info'] = {
                        'count': len(samples),
                        'types': [s['type'] for s in samples]
                    }
                    st.session_state.expert_report = None
                    st.session_state.chunk_analysis = None
                st.success("✅ Sample analysis complete!")
                st.toast("Sample analysis done!", icon="📊")
        
        # Full File Analysis
        with method_cols[3]:
            size_mb = len(content) / (1024 * 1024)
            help_text = f"Process entire {size_mb:.1f}MB file"
            
            if size_mb > 2:
                chunks_estimate = int(len(content) / CRASH_ANALYZER_CONFIG["chunk_size"])
                time_estimate = chunks_estimate * 2
                help_text += f" ({chunks_estimate} chunks, ~{time_estimate//60}m)"
                
            if st.button("🧩 Full File Analysis", use_container_width=True, help=help_text):
                if size_mb > 2:
                    st.warning(f"""
                    ⚠️ **Large File Processing**
                    - File size: {size_mb:.1f}MB
                    - Estimated chunks: {chunks_estimate}
                    - Estimated time: {time_estimate//60} minutes {time_estimate%60} seconds
                    - You can abort processing at any time
                    """)
                    
                    if not st.checkbox("I understand and want to proceed with full analysis"):
                        st.stop()
                
                with st.spinner("Performing full file analysis..."):
                    st.session_state.chunk_analysis = analyze_with_chunking(content)
                    st.session_state.analysis = None
                    st.session_state.expert_report = None
                
                if not st.session_state.chunk_analysis.get('aborted', False):
                    st.success("✅ Full analysis complete!")
                    st.toast("Full analysis done!", icon="🎉")
                else:
                    st.info("Analysis stopped by user")
    
    def _check_known_patterns(self, content: str):
        """Check against known crash patterns if available"""
        try:
            from utils.crash_knowledge import SmartCrashMatcher, get_crash_pattern_stats
            crash_matcher = SmartCrashMatcher(self.db)
            
            with st.spinner("🧠 Checking against known crash patterns..."):
                pattern_match = crash_matcher.match_crash(content)
                
                if not pattern_match.is_new:
                    st.success(f"✅ **Matched Known Pattern:** {pattern_match.pattern_name}")
                    
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.metric("Pattern Confidence", f"{pattern_match.confidence:.1%}")
                    with col2:
                        st.metric("Match Type", pattern_match.match_type.title())
                    with col3:
                        if pattern_match.verified_solutions:
                            st.metric("Proven Solutions", len(pattern_match.verified_solutions))
                    
                    if pattern_match.verified_solutions:
                        with st.expander("🔧 Proven Solutions from Past Incidents", expanded=True):
                            for i, solution in enumerate(pattern_match.verified_solutions, 1):
                                st.write(f"**Solution {i}** (Success count: {solution.get('success_count', 1)})")
                                st.info(solution['description'])
                                st.caption(f"Added by: {solution.get('added_by', 'Unknown')}")
                else:
                    st.info("🆕 **New Crash Pattern Detected** - This appears to be a previously unseen error type")
                    st.caption("Your validation will help the system learn and improve future analyses")
        except ImportError:
            pass  # Crash knowledge system not available
    
    def _display_results(self):
        """Display analysis results based on what's available"""
        if st.session_state.analysis:
            show_analysis_results(st.session_state.analysis, "basic")
        
        if st.session_state.expert_report:
            st.markdown("### 📝 Expert Diagnostic Report")
            
            with st.expander("View Full Expert Report", expanded=True):
                st.markdown(st.session_state.expert_report)
            
            # Extract and display any Mermaid diagrams
            diagrams = extract_mermaid_diagrams(st.session_state.expert_report)
            if diagrams:
                st.markdown("#### 📊 Visualizations")
                for i, diagram in enumerate(diagrams):
                    st.caption(f"Diagram {i+1}")
                    st.code(diagram, language="mermaid")
        
        if st.session_state.chunk_analysis:
            show_chunk_analysis_results(st.session_state.chunk_analysis)
    
    def _show_additional_sections(self):
        """Show additional UI sections"""
        # Recent analyses
        self._show_recent_analyses()
        
        # Pattern analysis dashboard
        with st.expander("📊 Crash Statistics Dashboard", expanded=False):
            if self.db:
                show_crash_statistics_dashboard(self.db)
        
        # Crash Knowledge Analytics (if enabled)
        self._show_crash_knowledge_analytics()
        
        # Database setup
        self._show_database_setup()
    
    def _show_recent_analyses(self):
        """Show recent crash analyses"""
        with st.expander("📋 Recent Crash Analyses", expanded=False):
            if self.db:
                try:
                    recent = self.db.execute_query(
                        """SELECT filename, analysis->>'severity' as severity, 
                           analysis->>'root_cause' as root_cause, 
                           validated_by, created_at,
                           CASE WHEN expert_report IS NOT NULL THEN TRUE ELSE FALSE END as has_expert
                           FROM crash_analysis 
                           ORDER BY created_at DESC LIMIT 10"""
                    )
                    
                    if recent:
                        for entry in recent:
                            from .config import SEVERITY_INDICATORS
                            severity_info = SEVERITY_INDICATORS.get(entry['severity'], SEVERITY_INDICATORS["Unknown"])
                            
                            expert_badge = "📝" if entry['has_expert'] else ""
                            
                            col1, col2, col3 = st.columns([3, 1, 1])
                            with col1:
                                st.markdown(f"{severity_info['emoji']} **{entry['filename']}** {expert_badge}")
                                st.caption(f"Root cause: {entry['root_cause'][:80]}...")
                            with col2:
                                st.caption(f"By: {entry['validated_by']}")
                            with col3:
                                st.caption(entry['created_at'].strftime('%Y-%m-%d %H:%M'))
                    else:
                        st.info("No crash analyses found yet")
                except Exception as e:
                    st.info("Initialize the database to see recent analyses")
            else:
                st.info("Database not available")
    
    def _show_crash_knowledge_analytics(self):
        """Show crash knowledge analytics if available"""
        try:
            from utils.crash_knowledge import get_crash_pattern_stats
            
            with st.expander("🧠 Crash Knowledge Analytics", expanded=False):
                if self.db:
                    stats = get_crash_pattern_stats(self.db)
                    
                    if stats.get('total_patterns', 0) > 0:
                        col1, col2, col3, col4 = st.columns(4)
                        
                        with col1:
                            st.metric("Total Patterns", stats['total_patterns'])
                        
                        with col2:
                            high_conf = stats.get('confidence_distribution', {}).get('High', 0)
                            st.metric("High Confidence", high_conf)
                        
                        with col3:
                            if stats.get('top_patterns'):
                                st.metric("Most Common", stats['top_patterns'][0]['name'][:20] + "...")
                        
                        with col4:
                            recent_count = len(stats.get('recent_patterns', []))
                            st.metric("Recent (7d)", recent_count)
                        
                        # Top patterns
                        if stats.get('top_patterns'):
                            st.subheader("🔝 Most Common Crash Patterns")
                            for pattern in stats['top_patterns']:
                                col1, col2, col3 = st.columns([3, 1, 1])
                                with col1:
                                    st.write(f"**{pattern['name']}**")
                                with col2:
                                    st.caption(f"Count: {pattern['count']}")
                                with col3:
                                    st.caption(f"Confidence: {pattern['confidence']:.1%}")
                    else:
                        st.info("No crash patterns learned yet. Start analyzing crashes to build the knowledge base!")
        except ImportError:
            pass  # Crash knowledge system not available
    
    def _show_database_setup(self):
        """Show database setup instructions"""
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
            """, language="sql")
            
            if st.button("🚀 Initialize Database Tables"):
                if self.db:
                    try:
                        self.db.execute_query("""
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
                        self.db.execute_query("""
                            CREATE INDEX IF NOT EXISTS idx_crash_severity 
                            ON crash_analysis((analysis->>'severity'))
                        """)
                        self.db.execute_query("""
                            CREATE INDEX IF NOT EXISTS idx_crash_created 
                            ON crash_analysis(created_at DESC)
                        """)
                        st.success("✅ Database initialized successfully!")
                    except Exception as e:
                        st.error(f"Error: {str(e)}")
                else:
                    st.error("Database connection not available")
    
    def _build_basic_analysis_prompt(self, content: str) -> str:
        """Build prompt for basic analysis"""
        from .extractors import extract_critical_sections
        from .wcr_handler import is_wcr_format
        
        extracted = extract_critical_sections(content, 15000)
        
        wcr_context = ""
        if is_wcr_format(content):
            try:
                from utils.wcr_patterns import extract_wcr_cause
                wcr_cause = extract_wcr_cause(content)
                if wcr_cause:
                    wcr_context = f"\nIMPORTANT: This is a What's On (Smalltalk) crash. Primary cause: {wcr_cause}\n"
            except ImportError:
                pass
        
        return f"""Analyze this crash dump and provide a structured analysis.
Focus on identifying the root cause, error types, and providing actionable solutions.
{wcr_context}
CRASH DUMP:
{extracted}

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
    
    def _parse_basic_analysis(self, response: str) -> Dict[str, Any]:
        """Parse basic analysis response"""
        import json
        import re
        
        try:
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response, re.DOTALL)
            if json_match:
                return json.loads(json_match.group(0))
        except:
            pass
        
        # Fallback to basic analysis
        from .processors import analyze_crash_dump
        return analyze_crash_dump(response)