# pages/ruby_profiler.py
import streamlit as st
from utils import DatabaseManager, safe_ollama_generate
import re

def analyze_performance(code):
    """Comprehensive performance analysis with optimization suggestions"""
    # Complexity analysis
    complexity_report = safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Analyze computational complexity:\n```ruby\n{code}\n```",
        system="Identify time/space complexity (Big O), highlight bottlenecks, and suggest algorithmic improvements"
    )['response']
    
    # Optimization suggestions
    optimized_code = safe_ollama_generate(
        model="deepseek-coder:latest",
        prompt=f"Optimize performance of:\n```ruby\n{code}\n```",
        system="Apply: Lazy loading, memoization, database batching, algorithmic improvements. Preserve functionality."
    )['response']
    
    # Memory analysis
    memory_report = safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Analyze memory usage:\n```ruby\n{code}\n```",
        system="Identify object allocation hotspots, memory retention issues, and GC pressure points"
    )['response']
    
    return complexity_report, optimized_code, memory_report

def estimate_complexity(code):
    """Quick complexity estimation using heuristics"""
    # Count decision points
    decisions = len(re.findall(r'\b(if|unless|case|while|until|for|&&|\|\|)\b', code))
    
    # Count loops
    loops = len(re.findall(r'\b(each|map|select|reduce|times)\b', code))
    
    # Count nested blocks
    nested_blocks = len(re.findall(r'\bdo\b|\{', code))
    
    # Heuristic complexity score
    score = decisions + (loops * 2) + (nested_blocks * 3)
    
    if score < 10: return "O(1) - Constant"
    elif score < 30: return "O(n) - Linear"
    elif score < 60: return "O(n log n) - Log-linear"
    elif score < 100: return "O(n²) - Quadratic"
    return "O(n!) - Factorial (dangerous)"

def show():
    st.title("⚡ Ruby Performance Profiler")
    st.caption("Identify and fix performance bottlenecks in Ruby code")
    
    # Code input
    code = st.text_area("Paste Ruby Code", 
                       height=300,
                       placeholder="def process_data\n  Data.all.each do |d|\n    # ...\n  end\nend",
                       key="perf_code")
    
    # Quick analysis
    if st.button("Quick Analysis", type="secondary"):
        st.subheader("Complexity Estimation")
        complexity = estimate_complexity(code)
        st.metric("Estimated Complexity", complexity)
        
        if "Quadratic" in complexity or "Factorial" in complexity:
            st.warning("Potential performance issues detected!")
    
    # Full analysis
    if st.button("Run Full Analysis", type="primary") and code:
        with st.spinner("Profiling code..."):
            complexity_report, optimized_code, memory_report = analyze_performance(code)
            
            # Results in tabs
            tab1, tab2, tab3 = st.tabs(["Complexity Report", "Optimized Code", "Memory Analysis"])
            
            with tab1:
                st.subheader("Performance Analysis")
                st.markdown(complexity_report)
                
            with tab2:
                st.subheader("Optimized Implementation")
                st.code(optimized_code, language="ruby")
                st.download_button("Download Optimized Code", optimized_code, "optimized.rb")
                
            with tab3:
                st.subheader("Memory Usage Report")
                st.markdown(memory_report)
            
            # Performance patterns
            with st.expander("🚀 Ruby Performance Patterns", expanded=True):
                st.markdown("""
                **Common Optimizations:**
                - N+1 Queries → Eager Loading
                - Loop Inefficiencies → Map/Reduce
                - Memory Bloat → Lazy Evaluation
                - Algorithmic Bottlenecks → Better Data Structures
                
                **Tools:**
                - `benchmark-ips` for microbenchmarks
                - `memory_profiler` for memory analysis
                - `stackprof` for flamegraphs
                """)
                st.link_button("Ruby Performance Guide", "https://github.com/ruby-prof/ruby-prof")
            
            # Save to knowledge base
            if st.button("💾 Save Analysis"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="ruby_profiler",
                        model="deepseek-r1:latest",
                        prompt=code[:500],
                        response=f"{complexity_report}\n\n{optimized_code}",
                        metadata={"tags": ["ruby", "performance"]}
                    )
                    if query_id:
                        st.success("Analysis saved to knowledge library!")
                else:
                    st.error("Could not connect to database")

if __name__ == "__main__":
    show()
