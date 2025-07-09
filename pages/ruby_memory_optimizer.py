# pages/ruby_memory_optimizer.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Ruby Memory Optimizer - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate
import re

def analyze_memory(code):
    """Comprehensive memory analysis with optimization suggestions"""
    # Memory report
    report = safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Analyze memory usage in:\n```ruby\n{code}\n```",
        system=(
            "Identify:\n"
            "- Object allocation hotspots\n"
            "- Memory retention issues\n"
            "- GC pressure points\n"
            "- Potential memory leaks\n"
            "Suggest specific optimizations with estimated impact"
        )
    )['response']
    
    # Optimized code
    optimized = safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Optimize memory usage for:\n```ruby\n{code}\n```",
        system=(
            "Apply:\n"
            "- Lazy loading\n"
            "- Object pooling\n"
            "- Memory-efficient data structures\n"
            "- String freezing\n"
            "- GC tuning\n"
            "Preserve functionality while reducing memory footprint"
        )
    )['response']
    
    # Memory saving estimate
    estimate = safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Estimate memory savings after optimization:\nOriginal:\n{code}\nOptimized:\n{optimized}",
        system="Provide percentage estimate of memory reduction"
    )['response']
    
    return report, optimized, estimate

def detect_memory_antipatterns(code):
    """Quick detection of common memory issues"""
    antipatterns = {
        "String Duplication": r"(\+=|<<)\s*['\"]",
        "Unbounded Growth": r"(@@|\$)[a-z]+\s*(\+|\-|\*|\/)\s*=",
        "N+1 Caching": r"Rails\.cache\.fetch[^{]*\{[^}]*\.each",
        "Leaky Constants": r"([A-Z][A-Z0-9_]+)\s*=\s*\[|\{",
        "Large Object Creation": r"\.times\s*\{.*new|\.map\s*\{.*new",
        "Memory Bloat": r"@\w+\s*<<.*\*\s*\d{3,}"
    }
    found = []
    for name, pattern in antipatterns.items():
        if re.search(pattern, code, re.IGNORECASE):
            found.append(name)
    return found

def show():
    st.title("🧠 Ruby Memory Optimizer")
    st.caption("Reduce memory footprint and prevent leaks in Ruby applications")
    
    # Code input
    code = st.text_area("Paste Ruby Code", 
                       height=300,
                       placeholder="def process_data\n  data = []; 10000.times { data << 'x' * 1024 }\nend",
                       key="memory_code")
    
    # Quick scan
    if st.button("Quick Scan", type="secondary") and code:
        antipatterns = detect_memory_antipatterns(code)
        if antipatterns:
            st.warning(f"⚠️ Potential issues detected:")
            for pattern in antipatterns:
                st.caption(f"• {pattern}")
        else:
            st.success("✅ No common antipatterns detected")
    
    # Full optimization
    if st.button("Optimize Memory", type="primary") and code:
        with st.spinner("Analyzing memory usage..."):
            report, optimized, estimate = analyze_memory(code)
            
            # Results in tabs
            tab1, tab2, tab3, tab4 = st.tabs(["Analysis Report", "Optimized Code", "Savings Estimate", "Best Practices"])
            
            with tab1:
                st.subheader("Memory Usage Analysis")
                st.markdown(report)
                
            with tab2:
                st.subheader("Optimized Implementation")
                st.code(optimized, language="ruby")
                st.download_button("Download Optimized Code", optimized, "memory_optimized.rb")
                
            with tab3:
                st.subheader("Memory Savings")
                # Try to extract percentage from estimate
                percentage_match = re.search(r"(\d+)%", estimate)
                if percentage_match:
                    percentage = int(percentage_match.group(1))
                    st.metric("Estimated Reduction", f"{percentage}%")
                    st.progress(percentage / 100)
                else:
                    st.info(estimate)
                    
            with tab4:
                st.markdown("""
                **Memory Optimization Checklist:**
                
                ✅ **String Optimization**
                - Use `String#freeze` for constants
                - Prefer `<<` over `+=` for concatenation
                - Use symbols for hash keys
                
                ✅ **Collection Management**
                - Use `lazy` enumerators for large datasets
                - Clear collections when done: `array.clear`
                - Consider `Set` instead of `Array` for uniqueness
                
                ✅ **Object Pooling**
                - Reuse objects instead of creating new ones
                - Use connection pools for database/Redis
                - Implement object factories with pooling
                
                ✅ **GC Tuning**
                ```bash
                export RUBY_GC_HEAP_GROWTH_FACTOR=1.1
                export RUBY_GC_MALLOC_LIMIT=90000000
                export RUBY_GC_OLDMALLOC_LIMIT=90000000
                ```
                """)
            
            # Memory optimization techniques
            with st.expander("💡 Memory Optimization Patterns", expanded=True):
                col1, col2 = st.columns(2)
                
                with col1:
                    st.markdown("""
                    **Object Reuse:**
                    ```ruby
                    # Bad
                    results = []
                    data.each { |d| results << process(d) }
                    
                    # Good
                    results = data.map { |d| process(d) }
                    ```
                    
                    **Lazy Loading:**
                    ```ruby
                    # Bad
                    @all_users = User.all
                    
                    # Good
                    def users
                      @users ||= User.all
                    end
                    ```
                    """)
                
                with col2:
                    st.markdown("""
                    **String Optimization:**
                    ```ruby
                    # Bad
                    str = ""
                    items.each { |i| str += i }
                    
                    # Good
                    str = ""
                    items.each { |i| str << i }
                    ```
                    
                    **Data Structures:**
                    ```ruby
                    # Use Set for uniqueness
                    require 'set'
                    unique = Set.new(items)
                    
                    # Use symbols as keys
                    { name: 'John' } # not { 'name' => 'John' }
                    ```
                    """)
                
                st.markdown("**Memory Profiling Tools:**")
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.link_button("memory_profiler", "https://github.com/SamSaffron/memory_profiler")
                with col2:
                    st.link_button("derailed", "https://github.com/schneems/derailed_benchmarks")
                with col3:
                    st.link_button("jemalloc", "https://github.com/jemalloc/jemalloc")
            
            # Save to knowledge base
            if st.button("💾 Save Optimization"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="memory_optimizer",
                        model=ModelManager.get_default_model(),
                        prompt=code[:500],
                        response=f"{report}\n\n{optimized}",
                        metadata={"tags": ["ruby", "performance", "memory"]}
                    )
                    if query_id:
                        st.success("Analysis saved to knowledge library!")
                else:
                    st.error("Could not connect to database")

if __name__ == "__main__":
    show()
