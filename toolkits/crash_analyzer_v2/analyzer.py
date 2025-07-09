"""
Main Crash Analyzer V2 Class
Integrates all analysis methods with unified interface
"""
import streamlit as st
import time
import re
from typing import Dict, Any, Optional
from datetime import datetime

from utils.tool_base import TuoKitToolBase
from utils import validate_file_size, get_available_models
from utils.ollama import get_ollama_manager
from utils.database import DatabaseManager
from utils.annotated_text_helpers import (
    annotated_text, parse_error_message, parse_stack_trace,
    ANNOTATION_COLORS
)

from .config import ANALYSIS_METHODS, VISUAL_ELEMENTS
from .ui.results_display import display_analysis_results

class CrashAnalyzerV2(TuoKitToolBase):
    """
    Redesigned Crash Analyzer with tiered analysis methods
    """
    
    def __init__(self):
        super().__init__(
            tool_name="Crash Analyzer V2",
            tool_description="Next-generation crash analysis with Quick Triage, Root Cause Analysis, Strategic Sampling, and Deep Forensic capabilities"
        )
        
        # Initialize database connection
        try:
            self.db = DatabaseManager()
        except Exception as e:
            print(f"Database connection failed: {e}")
            self.db = None
        
        # Initialize analyzers with error handling
        self.analyzers = {}
        try:
            from .analyzers import QuickTriageAnalyzer
            self.analyzers["quick_triage"] = QuickTriageAnalyzer()
        except Exception as e:
            st.error(f"Failed to initialize Quick Triage analyzer: {e}")
            
        try:
            from .analyzers import RootCauseAnalyzer
            self.analyzers["root_cause"] = RootCauseAnalyzer()
        except Exception as e:
            st.error(f"Failed to initialize Root Cause analyzer: {e}")
            
        try:
            from .analyzers import StrategicSamplingAnalyzer
            self.analyzers["strategic_sampling"] = StrategicSamplingAnalyzer()
        except Exception as e:
            st.error(f"Failed to initialize Strategic Sampling analyzer: {e}")
            
        try:
            from .analyzers import DeepForensicAnalyzer
            self.analyzers["deep_forensic"] = DeepForensicAnalyzer()
        except Exception as e:
            st.error(f"Failed to initialize Deep Forensic analyzer: {e}")
            
        try:
            from .analyzers import EnterpriseReportAnalyzer
            self.analyzers["enterprise_report"] = EnterpriseReportAnalyzer()
        except Exception as e:
            st.error(f"Failed to initialize Enterprise Report analyzer: {e}")
    
    def run(self):
        """Main entry point for the crash analyzer"""
        st.title("Crash Analyzer V2")
        st.caption(self.tool_description)
        
        # Initialize session state
        self._init_session_state()
        
        # Create tabs for different functionalities
        tab1, tab2, tab3 = st.tabs(["🔍 Analysis", "📊 Analytics", "📚 Knowledge Base"])
        
        with tab1:
            self._render_analysis_tab()
        
        with tab2:
            self._render_analytics_tab()
        
        with tab3:
            self._render_knowledge_base_tab()
    
    def _render_analysis_tab(self):
        """Render the main analysis tab"""
        # Model selection (for AI-powered methods)
        selected_model = self._show_model_selection()
        
        # File upload
        uploaded_file = st.file_uploader(
            "Upload crash dump",
            type=["txt", "log", "dmp", "wcr", "crash", "error", "dump"],
            help="Maximum file size: 5MB. Larger files should be processed with specialized tools."
        )
        
        if uploaded_file:
            # Check file size first
            MAX_FILE_SIZE = 5 * 1024 * 1024  # 5MB
            if uploaded_file.size > MAX_FILE_SIZE:
                st.error(f"❌ File too large: {uploaded_file.size / 1024 / 1024:.1f}MB. Maximum size: {MAX_FILE_SIZE / 1024 / 1024}MB")
                st.info("💡 For larger files, consider using Strategic Sampling or Deep Forensic analysis methods which process files in chunks.")
                return
            
            # Get file content
            try:
                content = uploaded_file.getvalue().decode("utf-8", errors='ignore')
            except Exception as e:
                st.error(f"Error reading file: {str(e)}")
                return
            
            # Show file info
            self._show_file_info(uploaded_file, content)
            
            # Show file preview
            self._show_file_preview(content, uploaded_file.name)
            
            # Simpler method selection
            st.markdown("### 🎯 Select Analysis Method")
            
            # Create method options
            method_options = []
            method_keys = []
            for method_key, method_config in ANALYSIS_METHODS.items():
                if method_key == "quick_triage" or selected_model:
                    method_options.append(f"{method_config['name']} - {method_config['description']}")
                    method_keys.append(method_key)
            
            if method_options:
                selected_index = st.radio(
                    "Choose analysis type:",
                    range(len(method_options)),
                    format_func=lambda x: method_options[x],
                    key="analysis_method_radio"
                )
                
                selected_method = method_keys[selected_index]
                method_config = ANALYSIS_METHODS[selected_method]
                
                # Show method details
                st.info(f"**Selected**: {method_config['name']}")
                with st.expander("Method Details", expanded=False):
                    st.write(f"**Description**: {method_config['description']}")
                    st.write(f"**Features**:")
                    for feature in method_config.get('features', []):
                        st.write(f"- {feature}")
                
                # Analysis button
                # Add a test button for Ollama
                col1, col2 = st.columns(2)
                
                with col1:
                    if st.button(
                        f"🚀 Start {method_config['name']} Analysis", 
                        type="primary",
                        use_container_width=True
                    ):
                        self._perform_analysis(
                            selected_method,
                            content,
                            uploaded_file.name,
                            selected_model
                        )
                
                with col2:
                    if selected_model and st.button("🧪 Test Ollama", use_container_width=True):
                        with st.spinner("Testing Ollama..."):
                            try:
                                from utils.ollama import safe_ollama_generate
                                test_response = safe_ollama_generate(
                                    model=selected_model,
                                    prompt="Say 'Hello, I am working!' in 5 words or less."
                                )
                                st.write("Ollama Test Response:")
                                st.json(test_response)
                            except Exception as e:
                                st.error(f"Ollama test failed: {e}")
            else:
                st.warning("No analysis methods available. Please ensure Ollama is running with models installed.")
            
            # Display results if available
            if st.session_state.get("analysis_results"):
                display_analysis_results(
                    st.session_state.analysis_results,
                    st.session_state.analysis_method,
                    db=self.db,
                    analyzer=self
                )
        
        # Additional features
        self._show_additional_features()
    
    def _render_analytics_tab(self):
        """Render the analytics dashboard tab"""
        from .analytics.dashboard import CrashAnalyticsDashboard
        
        dashboard = CrashAnalyticsDashboard(self.db)
        dashboard.render()
    
    def _render_knowledge_base_tab(self):
        """Render the knowledge base tab"""
        from .ui.knowledge_base import KnowledgeBaseUI
        
        kb_ui = KnowledgeBaseUI(self.db)
        kb_ui.render()
    
    def _init_session_state(self):
        """Initialize session state variables"""
        if 'analysis_results' not in st.session_state:
            st.session_state.analysis_results = None
        if 'analysis_method' not in st.session_state:
            st.session_state.analysis_method = None
        if 'selected_model' not in st.session_state:
            st.session_state.selected_model = None
    
    def _show_model_selection(self) -> Optional[str]:
        """Show model selection for AI-powered methods"""
        with st.expander("🤖 AI Model Configuration", expanded=True):
            col1, col2 = st.columns([3, 1])
            
            with col1:
                # Check Ollama availability
                try:
                    ollama_mgr = get_ollama_manager()
                    status = ollama_mgr.get_status()
                    
                    if status.get("running", False):
                        available_models = get_available_models()
                        
                        if available_models:
                            selected_model = st.selectbox(
                                "Select AI Model",
                                available_models,
                                help="AI model for Root Cause, Sampling, and Forensic analysis"
                            )
                            st.session_state.selected_model = selected_model
                            return selected_model
                        else:
                            st.warning("No models installed. Quick Triage is still available.")
                            st.code("ollama pull deepseek-r1:latest", language="bash")
                    else:
                        st.info("🔌 Ollama not running. Only Quick Triage analysis available.")
                        st.caption("Start Ollama for AI-powered analysis methods")
                except Exception as e:
                    st.error(f"Error connecting to Ollama: {e}")
                    
            with col2:
                if st.button("🔄 Refresh", help="Check for models again"):
                    st.rerun()
        
        return None
    
    def _show_file_info(self, uploaded_file, content: str):
        """Display file information"""
        file_size = len(content)
        size_mb = file_size / (1024 * 1024)
        
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric("File", uploaded_file.name)
        with col2:
            if size_mb > 1:
                st.metric("Size", f"{size_mb:.1f} MB")
            else:
                st.metric("Size", f"{file_size:,} bytes")
        with col3:
            st.metric("Lines", f"{content.count(chr(10)):,}")
        with col4:
            # Quick classification
            if self._detect_production(content):
                st.metric("Environment", "Production", "⚠️ LIVE")
            else:
                st.metric("Environment", "Non-prod", "✓ SAFE")
    
    def _show_file_preview(self, content: str, filename: str):
        """Display file preview with syntax highlighting"""
        with st.expander("📄 File Preview", expanded=False):
            # Add file type detection
            file_ext = filename.split('.')[-1].lower() if '.' in filename else 'log'
            
            # Map file extensions to preview settings
            preview_config = {
                'log': {'lines': 50, 'lang': 'log', 'desc': 'Log file'},
                'txt': {'lines': 50, 'lang': 'text', 'desc': 'Text file'},
                'dmp': {'lines': 30, 'lang': 'text', 'desc': 'Dump file'},
                'wcr': {'lines': 30, 'lang': 'text', 'desc': 'Crash report'},
                'crash': {'lines': 40, 'lang': 'log', 'desc': 'Crash log'},
                'error': {'lines': 40, 'lang': 'log', 'desc': 'Error log'},
                'dump': {'lines': 30, 'lang': 'text', 'desc': 'Memory dump'}
            }
            
            config = preview_config.get(file_ext, {'lines': 50, 'lang': 'text', 'desc': 'Unknown'})
            
            # Show preview controls
            col1, col2, col3 = st.columns([2, 2, 1])
            
            with col1:
                preview_lines = st.slider(
                    "Preview lines",
                    min_value=10,
                    max_value=200,
                    value=config['lines'],
                    step=10,
                    key="preview_lines"
                )
            
            with col2:
                preview_position = st.radio(
                    "Show from",
                    ["Beginning", "End", "Errors (if any)"],
                    horizontal=True,
                    key="preview_position"
                )
            
            with col3:
                st.caption(f"Type: {config['desc']}")
            
            # Get preview content based on selection
            lines = content.split('\n')
            total_lines = len(lines)
            
            if preview_position == "Beginning":
                preview_content = '\n'.join(lines[:preview_lines])
                preview_info = f"Showing first {min(preview_lines, total_lines)} of {total_lines} lines"
            elif preview_position == "End":
                preview_content = '\n'.join(lines[-preview_lines:])
                preview_info = f"Showing last {min(preview_lines, total_lines)} of {total_lines} lines"
            else:  # Errors
                # Find lines with error indicators
                error_lines = []
                error_indices = []
                error_keywords = ['error', 'exception', 'fail', 'fatal', 'crash', 'critical']
                
                for i, line in enumerate(lines):
                    if any(keyword in line.lower() for keyword in error_keywords):
                        error_indices.append(i)
                
                if error_indices:
                    # Show context around first few errors
                    shown_lines = set()
                    for idx in error_indices[:5]:  # First 5 errors
                        # Add context: 3 lines before and after
                        for i in range(max(0, idx-3), min(total_lines, idx+4)):
                            if i not in shown_lines:
                                error_lines.append((i, lines[i]))
                                shown_lines.add(i)
                    
                    # Sort by line number and format
                    error_lines.sort(key=lambda x: x[0])
                    preview_content = ""
                    last_idx = -999
                    
                    for idx, line in error_lines:
                        if idx - last_idx > 1:
                            preview_content += "\n...\n"
                        preview_content += f"{idx+1:6d}: {line}\n"
                        last_idx = idx
                    
                    preview_info = f"Showing {len(error_indices)} error locations (with context)"
                else:
                    preview_content = '\n'.join(lines[:preview_lines])
                    preview_info = "No obvious errors found - showing beginning of file"
            
            # Display preview info
            st.caption(preview_info)
            
            # Show preview with appropriate syntax highlighting
            if preview_content.strip():
                # For error preview, use annotated text for better highlighting
                if preview_position == "Errors (if any)" and error_indices:
                    st.markdown("**Error Preview with Annotations:**")
                    # Parse and display each line with annotations
                    for line in preview_content.split('\n'):
                        if line.strip() and not line.strip() == '...':
                            # Check if this is a line number prefix
                            line_match = re.match(r'^(\s*\d+:\s*)(.*)$', line)
                            if line_match:
                                line_num, content = line_match.groups()
                                # Check if this line contains errors
                                if any(keyword in content.lower() for keyword in ['error', 'exception', 'fail', 'fatal', 'crash']):
                                    # Parse error content with annotations
                                    components = parse_error_message(content)
                                    annotated_text(
                                        (line_num, "line_number", ANNOTATION_COLORS["neutral"]),
                                        *components
                                    )
                                else:
                                    st.text(line)
                            else:
                                st.text(line)
                        elif line.strip() == '...':
                            st.text('...')
                else:
                    # Use appropriate language for syntax highlighting
                    lang = config['lang'] if config['lang'] != 'log' else None
                    st.code(preview_content, language=lang, line_numbers=True)
            else:
                st.info("File appears to be empty")
            
            # Quick analysis summary
            st.divider()
            col1, col2, col3 = st.columns(3)
            
            with col1:
                # Count error types
                error_count = sum(1 for line in lines if 'error' in line.lower())
                warn_count = sum(1 for line in lines if 'warn' in line.lower())
                st.caption(f"Errors: {error_count} | Warnings: {warn_count}")
            
            with col2:
                # Check for stack traces
                has_stack = bool(re.search(r'\s+at\s+\w+|File\s+"[^"]+",\s+line\s+\d+', content[:10000]))
                st.caption(f"Stack trace: {'Yes' if has_stack else 'No'}")
            
            with col3:
                # File encoding confidence
                try:
                    content.encode('utf-8')
                    st.caption("Encoding: ✓ UTF-8")
                except:
                    st.caption("Encoding: ⚠️ Mixed")
    
    def _detect_stack_trace(self, content: str) -> bool:
        """Quick detection of stack traces"""
        import re
        patterns = [
            r'\s+at\s+\w+',
            r'File\s+"[^"]+",\s+line\s+\d+',
            r'#\d+\s+0x[0-9a-fA-F]+\s+in\s+'
        ]
        
        for pattern in patterns:
            if re.search(pattern, content[:5000]):  # Check first 5KB
                return True
        return False
    
    def _detect_production(self, content: str) -> bool:
        """Quick detection of production environment"""
        import re
        prod_patterns = [
            r'(?i)\bprod\b',
            r'(?i)\bproduction\b',
            r'(?i)\blive\b',
            r'(?i)\.com\b'
        ]
        
        for pattern in prod_patterns:
            if re.search(pattern, content[:5000]):
                return True
        return False
    
    def _calculate_error_density(self, content: str) -> float:
        """Calculate error keyword density"""
        import re
        error_count = len(re.findall(
            r'(?i)(error|exception|fail|crash|fatal|severe)', 
            content[:10000]  # First 10KB
        ))
        return error_count / (len(content[:10000]) / 1000)  # Errors per KB
    
    def _perform_analysis(self, method: str, content: str, filename: str, model: Optional[str]):
        """Perform the selected analysis"""
        from .utils.progress_tracker import ProgressTracker, ChunkingProgressTracker, AnalysisProgressTracker
        
        # Show progress
        progress_container = st.container()
        
        with progress_container:
            # Get method config
            method_config = ANALYSIS_METHODS[method]
            
            # Progress indicator based on method
            if method == "quick_triage":
                # Quick analysis with simple spinner
                tracker = ProgressTracker(progress_container)
                tracker.start("⚡ Quick Triage Analysis", total_steps=3, show_metrics=False)
                
                try:
                    tracker.update(1, "Parsing content", "Extracting error patterns")
                    start_time = time.time()
                    
                    tracker.update(2, "Analyzing", "Running algorithmic analysis")
                    results = self.analyzers[method].analyze(content, filename)
                    
                    elapsed = time.time() - start_time
                    tracker.complete(f"Analysis completed in {elapsed:.1f}s")
                except Exception as e:
                    tracker.error(str(e))
                    st.exception(e)
                    return
                    
            elif method == "strategic_sampling":
                # Sampling with chunk progress
                chunk_tracker = ChunkingProgressTracker(progress_container)
                file_size = len(content)
                chunk_size = 8192  # 8KB chunks for sampling
                
                chunk_tracker.start_chunking(file_size, chunk_size)
                
                try:
                    # Create a progress callback
                    def sampling_callback(chunk_id: int, status: str):
                        chunk_tracker.update_chunk(chunk_id, status)
                    
                    start_time = time.time()
                    results = self.analyzers[method].analyze(
                        content, 
                        filename,
                        selected_model=model,
                        progress_callback=sampling_callback
                    )
                    
                    elapsed = time.time() - start_time
                    chunk_tracker.complete(f"Sampling analysis completed in {elapsed:.1f}s")
                except Exception as e:
                    chunk_tracker.error(str(e))
                    st.exception(e)
                    return
                    
            elif method == "deep_forensic":
                # Multi-phase progress for deep analysis
                phase_tracker = AnalysisProgressTracker(progress_container)
                
                # Define analysis phases
                phase_tracker.add_phase("chunking", "Creating analysis chunks", 0.1)
                phase_tracker.add_phase("parallel", "Parallel chunk analysis", 0.4)
                phase_tracker.add_phase("crash", "Crash analysis", 0.15)
                phase_tracker.add_phase("performance", "Performance analysis", 0.15)
                phase_tracker.add_phase("security", "Security analysis", 0.1)
                phase_tracker.add_phase("recommendations", "Generating recommendations", 0.1)
                
                phase_tracker.start()
                
                try:
                    start_time = time.time()
                    
                    # Create phase callbacks
                    def phase_callback(phase: str, progress: float):
                        phase_tracker.update_phase(phase, progress)
                    
                    # Start analysis with callbacks
                    phase_tracker.start_phase("chunking")
                    results = self.analyzers[method].analyze(
                        content, 
                        filename,
                        selected_model=model,
                        phase_callback=phase_callback
                    )
                    
                    elapsed = time.time() - start_time
                    st.success(f"✅ Deep forensic analysis completed in {elapsed:.1f}s")
                except Exception as e:
                    st.error(f"Analysis failed: {str(e)}")
                    st.exception(e)
                    return
                    
            else:
                # Standard progress for other methods
                tracker = ProgressTracker(progress_container)
                tracker.start(f"🔍 {method_config['name']}", total_steps=10)
                
                try:
                    tracker.update(1, "Initializing", "Setting up analysis environment")
                    start_time = time.time()
                    
                    tracker.update(3, "Processing", "Analyzing crash dump content")
                    
                    # Create progress callback for analyzer
                    def progress_callback(progress: float, message: str = None):
                        step = int(progress * 10)
                        tracker.update(step, "Processing", message)
                    
                    if method in ["root_cause", "enterprise_report"]:
                        tracker.add_detail("Using AI model: " + (model or "Algorithmic fallback"), "info")
                        results = self.analyzers[method].analyze(
                            content, 
                            filename,
                            selected_model=model,
                            progress_callback=progress_callback
                        )
                    else:
                        results = self.analyzers[method].analyze(content, filename)
                    
                    elapsed = time.time() - start_time
                    tracker.complete(f"{method_config['name']} completed in {elapsed:.1f}s")
                except Exception as e:
                    tracker.error(str(e))
                    st.exception(e)
                    import traceback
                    st.code(traceback.format_exc())
                    return
        
        # Store results
        # Ensure filename is in results
        if 'filename' not in results:
            results['filename'] = filename
        st.session_state.analysis_results = results
        st.session_state.analysis_method = method
        
        # Show success message
        st.success(f"✅ Analysis complete! ({elapsed:.1f} seconds)")
        
        # Auto-expand results
        st.rerun()
    
    def _show_additional_features(self):
        """Show additional features and options"""
        with st.expander("🛠️ Additional Tools", expanded=False):
            col1, col2, col3 = st.columns(3)
            
            with col1:
                if st.button("📊 View Statistics"):
                    st.info("📊 Switch to the Analytics tab to view detailed statistics!")
            
            with col2:
                if st.button("🧠 Knowledge Base"):
                    st.info("📚 Switch to the Knowledge Base tab to browse saved analyses!")
            
            with col3:
                if st.button("📈 Trending Issues"):
                    st.info("📊 Switch to the Analytics tab to view trending issues!")
        
        # Help section
        with st.expander("❓ Help & Documentation", expanded=False):
            st.markdown("""
            ### Analysis Methods Guide
            
            **⚡ Quick Triage** (< 10 seconds)
            - Best for: Emergency response, initial assessment
            - No AI required, pure algorithmic analysis
            - Provides: Severity, error type, immediate fix suggestions
            
            **🔍 Root Cause Analysis** (30-60 seconds)  
            - Best for: Standard investigation with AI insights
            - Requires: Ollama with installed model
            - Provides: Business impact, error chain, actionable recommendations
            
            **🎯 Strategic Sampling** (1-2 minutes)
            - Best for: Large files (1-5MB) when full analysis isn't needed
            - Smart extraction of relevant sections
            - Coming soon!
            
            **🧬 Deep Forensic Analysis** (5-30 minutes)
            - Best for: Complete investigation including non-crash issues
            - Analyzes: Performance, security, code quality
            - Coming soon!
            """)
        
        # Footer
        st.divider()
        st.caption("🚀 Crash Analyzer V2 - Built with TuoKit principles")


# Create a page wrapper for Streamlit
def show():
    """Streamlit page entry point"""
    analyzer = CrashAnalyzerV2()
    analyzer.run()

if __name__ == "__main__":
    show()