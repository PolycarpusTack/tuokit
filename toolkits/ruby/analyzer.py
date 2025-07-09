"""
Ruby Toolkit Analyzer
Comprehensive Ruby development and optimization suite
"""

import streamlit as st
from typing import Dict, Any, List, Optional
from datetime import datetime

from utils.tool_base import TuoKitToolBase
from utils.ollama import get_ollama_manager, safe_ollama_generate

class RubyToolkit(TuoKitToolBase):
    """Advanced Ruby development toolkit with optimization and learning tools"""
    
    def __init__(self):
        super().__init__(
            tool_name="Ruby Toolkit",
            tool_description="Advanced Ruby development suite - optimization, profiling, patterns, and learning tools"
        )
        
        # Import tool modules
        from .optimization import MemoryOptimizer, PerformanceProfiler, CodeOptimizer
        from .advanced import CExtensionBuilder, RactorGuide, PatternMatcher
        from .learning import KataGenerator, BestPracticesAnalyzer
        from .config import RUBY_CONFIG
        
        # Initialize tools
        self.memory_optimizer = MemoryOptimizer()
        self.performance_profiler = PerformanceProfiler()
        self.code_optimizer = CodeOptimizer()
        self.c_extension_builder = CExtensionBuilder()
        self.ractor_guide = RactorGuide()
        self.pattern_matcher = PatternMatcher()
        self.kata_generator = KataGenerator()
        self.best_practices = BestPracticesAnalyzer()
        
        self.config = RUBY_CONFIG
    
    def run(self):
        """Main entry point for Ruby toolkit"""
        st.title("💎 Ruby Toolkit")
        st.markdown("Advanced Ruby development and optimization suite")
        
        # Sidebar navigation
        with st.sidebar:
            st.markdown("## 🗂️ Tool Categories")
            
            category = st.radio(
                "Select Category",
                [
                    "🚀 Optimization",
                    "🔬 Advanced Ruby",
                    "🎓 Learning",
                    "🔍 Analysis",
                    "🛠️ Utilities"
                ],
                key="ruby_category"
            )
            
            st.divider()
            
            # Model selector
            models = self._get_available_models()
            if models:
                selected_model = st.selectbox(
                    "🤖 AI Model",
                    models,
                    help="Select model for code generation"
                )
                st.session_state['selected_model'] = selected_model
            
            # Ruby version selector
            st.selectbox(
                "💎 Ruby Version",
                self.config['ruby_versions'],
                key="ruby_version"
            )
        
        # Main content based on category
        if category == "🚀 Optimization":
            self._show_optimization_tools()
        elif category == "🔬 Advanced Ruby":
            self._show_advanced_tools()
        elif category == "🎓 Learning":
            self._show_learning_tools()
        elif category == "🔍 Analysis":
            self._show_analysis_tools()
        elif category == "🛠️ Utilities":
            self._show_utility_tools()
    
    def _show_optimization_tools(self):
        """Show performance and memory optimization tools"""
        st.subheader("🚀 Ruby Optimization Tools")
        
        tool_tabs = st.tabs([
            "Memory Optimizer",
            "Performance Profiler",
            "Code Optimizer",
            "Benchmark Suite"
        ])
        
        with tool_tabs[0]:
            self._show_memory_optimizer()
        
        with tool_tabs[1]:
            self._show_performance_profiler()
        
        with tool_tabs[2]:
            self._show_code_optimizer()
        
        with tool_tabs[3]:
            self._show_benchmark_suite()
    
    def _show_memory_optimizer(self):
        """Memory optimization interface"""
        st.markdown("### 🧠 Memory Optimizer")
        st.caption("Reduce memory footprint and prevent leaks")
        
        code_input = st.text_area(
            "Ruby Code to Optimize",
            height=300,
            placeholder="""def process_data
  data = []
  10000.times { data << 'x' * 1024 }
  data.map { |x| x.upcase }
end""",
            key="memory_code"
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            analysis_depth = st.select_slider(
                "Analysis Depth",
                ["Quick", "Standard", "Deep"],
                value="Standard"
            )
        
        with col2:
            include_gc_tuning = st.checkbox("Include GC Tuning", value=True)
            include_benchmarks = st.checkbox("Add Benchmarks", value=True)
        
        if st.button("🔍 Analyze Memory", type="primary", use_container_width=True):
            if code_input:
                with st.spinner("Analyzing memory usage..."):
                    result = self.memory_optimizer.analyze(
                        code_input,
                        depth=analysis_depth,
                        gc_tuning=include_gc_tuning,
                        benchmarks=include_benchmarks
                    )
                    
                    if result.get('success'):
                        # Quick detection results
                        if result.get('antipatterns'):
                            st.warning("⚠️ Memory Issues Detected:")
                            for pattern in result['antipatterns']:
                                st.write(f"• {pattern['name']}: {pattern['description']}")
                        
                        # Results in tabs
                        tabs = st.tabs(["Analysis", "Optimized Code", "Benchmarks", "GC Config"])
                        
                        with tabs[0]:
                            st.markdown("**Memory Analysis Report**")
                            st.markdown(result.get('analysis', ''))
                            
                            # Memory metrics
                            if result.get('metrics'):
                                col1, col2, col3 = st.columns(3)
                                with col1:
                                    st.metric("Allocations", result['metrics'].get('allocations', 'N/A'))
                                with col2:
                                    st.metric("Memory Usage", result['metrics'].get('memory_usage', 'N/A'))
                                with col3:
                                    st.metric("GC Runs", result['metrics'].get('gc_runs', 'N/A'))
                        
                        with tabs[1]:
                            st.markdown("**Optimized Code**")
                            st.code(result.get('optimized_code', ''), language='ruby')
                            
                            # Savings estimate
                            if result.get('savings_estimate'):
                                st.success(f"💰 Estimated Memory Savings: {result['savings_estimate']}")
                        
                        with tabs[2]:
                            if include_benchmarks and result.get('benchmark_code'):
                                st.markdown("**Benchmark Code**")
                                st.code(result['benchmark_code'], language='ruby')
                        
                        with tabs[3]:
                            if include_gc_tuning and result.get('gc_config'):
                                st.markdown("**Recommended GC Configuration**")
                                st.code(result['gc_config'], language='ruby')
                    else:
                        st.error(f"Analysis failed: {result.get('error', 'Unknown error')}")
            else:
                st.warning("Please provide Ruby code to analyze")
    
    def _show_advanced_tools(self):
        """Show advanced Ruby features and tools"""
        st.subheader("🔬 Advanced Ruby Tools")
        
        tool_tabs = st.tabs([
            "C Extensions",
            "Ractors",
            "Pattern Matching",
            "Metaprogramming",
            "Fiber Scheduler"
        ])
        
        with tool_tabs[0]:
            self._show_c_extension_builder()
        
        with tool_tabs[1]:
            self._show_ractor_guide()
        
        with tool_tabs[2]:
            self._show_pattern_matching()
        
        with tool_tabs[3]:
            self._show_metaprogramming()
        
        with tool_tabs[4]:
            self._show_fiber_scheduler()
    
    def _show_c_extension_builder(self):
        """C extension builder interface"""
        st.markdown("### ⚡ C Extension Builder")
        st.caption("Build high-performance Ruby C extensions")
        
        extension_type = st.selectbox(
            "Extension Type",
            ["Numeric Computation", "String Processing", "Data Structure", "System Call", "Custom"]
        )
        
        if extension_type == "Custom":
            description = st.text_area(
                "Describe your C extension",
                placeholder="Fast matrix multiplication for 2D arrays",
                height=100
            )
        else:
            # Pre-defined templates
            templates = {
                "Numeric Computation": "Fast mathematical operations",
                "String Processing": "High-speed string manipulation",
                "Data Structure": "Efficient custom data structure",
                "System Call": "Direct system call wrapper"
            }
            description = templates[extension_type]
            st.info(f"Building: {description}")
        
        col1, col2 = st.columns(2)
        
        with col1:
            include_tests = st.checkbox("Include Tests", value=True)
            include_benchmarks = st.checkbox("Include Benchmarks", value=True)
        
        with col2:
            include_makefile = st.checkbox("Generate Makefile", value=True)
            include_docs = st.checkbox("Add Documentation", value=True)
        
        if st.button("🔨 Generate C Extension", type="primary", use_container_width=True):
            with st.spinner("Generating C extension..."):
                result = self.c_extension_builder.generate(
                    extension_type=extension_type,
                    description=description if extension_type == "Custom" else None,
                    include_tests=include_tests,
                    include_benchmarks=include_benchmarks,
                    include_makefile=include_makefile,
                    include_docs=include_docs
                )
                
                if result.get('success'):
                    st.success("✅ C Extension generated successfully!")
                    
                    # Show files in tabs
                    tabs = st.tabs(["C Source", "Ruby Wrapper", "Tests", "Makefile", "Usage"])
                    
                    with tabs[0]:
                        st.markdown("**C Source Code**")
                        st.code(result.get('c_source', ''), language='c')
                    
                    with tabs[1]:
                        st.markdown("**Ruby Wrapper**")
                        st.code(result.get('ruby_wrapper', ''), language='ruby')
                    
                    with tabs[2]:
                        if include_tests:
                            st.markdown("**Test Suite**")
                            st.code(result.get('tests', ''), language='ruby')
                    
                    with tabs[3]:
                        if include_makefile:
                            st.markdown("**Makefile**")
                            st.code(result.get('makefile', ''), language='makefile')
                    
                    with tabs[4]:
                        st.markdown("**Usage Example**")
                        st.code(result.get('usage_example', ''), language='ruby')
                        
                        st.markdown("**Build Instructions**")
                        st.code("""# Build the extension
ruby extconf.rb
make

# Run tests
ruby test_extension.rb

# Use in Ruby
require './extension'
# Your code here""", language='bash')
                else:
                    st.error(f"Generation failed: {result.get('error')}")
    
    def _show_learning_tools(self):
        """Show Ruby learning and practice tools"""
        st.subheader("🎓 Ruby Learning Tools")
        
        tool_tabs = st.tabs([
            "Ruby Katas",
            "Best Practices",
            "Code Challenges",
            "Interview Prep",
            "Style Guide"
        ])
        
        with tool_tabs[0]:
            self._show_kata_generator()
        
        with tool_tabs[1]:
            self._show_best_practices()
        
        with tool_tabs[2]:
            self._show_code_challenges()
        
        with tool_tabs[3]:
            self._show_interview_prep()
        
        with tool_tabs[4]:
            self._show_style_guide()
    
    def _show_kata_generator(self):
        """Ruby kata generator interface"""
        st.markdown("### 🥋 Ruby Kata Generator")
        st.caption("Practice Ruby with coding katas")
        
        col1, col2 = st.columns(2)
        
        with col1:
            difficulty = st.select_slider(
                "Difficulty Level",
                ["Beginner", "Intermediate", "Advanced", "Expert"],
                value="Intermediate"
            )
            
            category = st.selectbox(
                "Kata Category",
                ["Algorithms", "Data Structures", "String Manipulation", 
                 "Array Operations", "Functional Programming", "OOP Design",
                 "Metaprogramming", "Performance", "Random"]
            )
        
        with col2:
            time_limit = st.selectbox(
                "Time Limit",
                ["15 minutes", "30 minutes", "45 minutes", "1 hour", "No limit"],
                index=1
            )
            
            include_hints = st.checkbox("Include Hints", value=True)
            include_solution = st.checkbox("Include Solution", value=False)
        
        if st.button("🎯 Generate Kata", type="primary", use_container_width=True):
            with st.spinner("Generating kata..."):
                result = self.kata_generator.generate(
                    difficulty=difficulty,
                    category=category,
                    time_limit=time_limit,
                    include_hints=include_hints,
                    include_solution=include_solution
                )
                
                if result.get('success'):
                    # Display kata
                    st.markdown(f"## {result.get('title', 'Ruby Kata')}")
                    st.markdown(f"**Difficulty:** {difficulty} | **Category:** {category} | **Time:** {time_limit}")
                    
                    st.divider()
                    
                    # Problem description
                    st.markdown("### 📋 Problem Description")
                    st.markdown(result.get('description', ''))
                    
                    # Examples
                    if result.get('examples'):
                        st.markdown("### 📝 Examples")
                        st.code(result['examples'], language='ruby')
                    
                    # Starter code
                    st.markdown("### 💻 Starter Code")
                    starter_code = st.text_area(
                        "Write your solution here:",
                        value=result.get('starter_code', ''),
                        height=200,
                        key="kata_solution"
                    )
                    
                    # Test cases
                    if result.get('test_cases'):
                        st.markdown("### 🧪 Test Cases")
                        st.code(result['test_cases'], language='ruby')
                    
                    # Hints
                    if include_hints and result.get('hints'):
                        with st.expander("💡 Hints"):
                            for i, hint in enumerate(result['hints'], 1):
                                st.write(f"{i}. {hint}")
                    
                    # Solution
                    if include_solution and result.get('solution'):
                        with st.expander("✅ Solution", expanded=False):
                            st.code(result['solution'], language='ruby')
                            
                            if result.get('explanation'):
                                st.markdown("**Explanation:**")
                                st.markdown(result['explanation'])
                    
                    # Run tests button
                    if st.button("🏃 Run Tests"):
                        with st.spinner("Running tests..."):
                            test_result = self.kata_generator.run_tests(
                                starter_code,
                                result.get('test_cases', '')
                            )
                            
                            if test_result.get('passed'):
                                st.success("✅ All tests passed! Great job!")
                            else:
                                st.error("❌ Some tests failed")
                                st.code(test_result.get('output', ''), language='text')
                else:
                    st.error(f"Failed to generate kata: {result.get('error')}")
    
    def _show_analysis_tools(self):
        """Show code analysis tools"""
        st.subheader("🔍 Ruby Analysis Tools")
        
        tool_tabs = st.tabs([
            "Code Review",
            "Complexity Analysis",
            "Security Scan",
            "Dependency Check",
            "Coverage Report"
        ])
        
        with tool_tabs[0]:
            self._show_code_review()
        
        with tool_tabs[1]:
            self._show_complexity_analysis()
        
        with tool_tabs[2]:
            self._show_security_scan()
        
        with tool_tabs[3]:
            self._show_dependency_check()
        
        with tool_tabs[4]:
            self._show_coverage_report()
    
    def _show_utility_tools(self):
        """Show Ruby utility tools"""
        st.subheader("🛠️ Ruby Utilities")
        
        tool_tabs = st.tabs([
            "Gem Builder",
            "Documentation",
            "Formatter",
            "Converter",
            "Scaffolder"
        ])
        
        with tool_tabs[0]:
            st.info("Gem builder - Create Ruby gems with best practices")
        
        with tool_tabs[1]:
            st.info("Documentation generator - YARD docs and README")
        
        with tool_tabs[2]:
            st.info("Code formatter - RuboCop integration")
        
        with tool_tabs[3]:
            st.info("Code converter - Ruby version migration")
        
        with tool_tabs[4]:
            st.info("Project scaffolder - Ruby project templates")
    
    # Placeholder methods for other tools
    def _show_performance_profiler(self):
        self.performance_profiler.show_interface()
    
    def _show_code_optimizer(self):
        self.code_optimizer.show_interface()
    
    def _show_benchmark_suite(self):
        st.info("Benchmark suite - Coming soon!")
    
    def _show_ractor_guide(self):
        self.ractor_guide.show_interface()
    
    def _show_pattern_matching(self):
        self.pattern_matcher.show_interface()
    
    def _show_metaprogramming(self):
        st.info("Metaprogramming tools - Coming soon!")
    
    def _show_fiber_scheduler(self):
        st.info("Fiber scheduler guide - Coming soon!")
    
    def _show_best_practices(self):
        self.best_practices.show_interface()
    
    def _show_code_challenges(self):
        st.info("Code challenges - Coming soon!")
    
    def _show_interview_prep(self):
        st.info("Interview preparation - Coming soon!")
    
    def _show_style_guide(self):
        st.info("Ruby style guide - Coming soon!")
    
    def _show_code_review(self):
        st.info("Code review tool - Coming soon!")
    
    def _show_complexity_analysis(self):
        st.info("Complexity analyzer - Coming soon!")
    
    def _show_security_scan(self):
        st.info("Security scanner - Coming soon!")
    
    def _show_dependency_check(self):
        st.info("Dependency checker - Coming soon!")
    
    def _show_coverage_report(self):
        st.info("Coverage reporter - Coming soon!")
    
    def _get_available_models(self) -> List[str]:
        """Get list of available Ollama models"""
        try:
            from utils.ollama import get_available_models
            return get_available_models()
        except:
            return []

# Create a page wrapper for Streamlit
def show():
    """Streamlit page entry point"""
    toolkit = RubyToolkit()
    toolkit.run()

if __name__ == "__main__":
    show()