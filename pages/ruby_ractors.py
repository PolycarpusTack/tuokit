# pages/ruby_ractors.py
import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Ruby Ractors - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils import DatabaseManager, safe_ollama_generate
import re

def ractor_implementation(task):
    """Generate thread-safe Ractor implementation"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Implement Ractors for: {task}",
        system=(
            "Create complete solution with:\n"
            "- Ractor initialization\n"
            "- Message passing\n"
            "- Error handling\n"
            "- Resource sharing precautions\n"
            "- Performance considerations\n"
            "Use Ruby 3.1+ with comments explaining concurrency model"
        )
    )['response']

def concurrency_advice(code):
    """Provide concurrency optimization advice"""
    return safe_ollama_generate(
        model=ModelManager.get_default_model(),
        prompt=f"Optimize concurrency for:\n```ruby\n{code}\n```",
        system=(
            "Suggest:\n"
            "1. Where to use Ractors vs Threads\n"
            "2. Thread safety improvements\n"
            "3. Shared resource management\n"
            "4. Alternative approaches (Fibers, Async)"
        )
    )['response']

def detect_concurrency_issues(code):
    """Quick check for potential thread safety issues"""
    issues = []
    
    # Check for shared state
    if re.search(r'@@\w+', code):
        issues.append("Class variables detected - potential race condition")
    if re.search(r'\$\w+', code):
        issues.append("Global variables detected - thread safety risk")
    if re.search(r'@\w+\s*\+=', code) or re.search(r'@\w+\s*<<', code):
        issues.append("Mutable instance variable modification - consider mutex")
    
    return issues

def show():
    st.title("⚡ Ruby Concurrency Advisor")
    st.caption("Implement parallel processing with Ruby's Ractors and concurrency models")
    
    # Input options
    task_type = st.radio("Task Type", 
                        ["Generate New Implementation", "Optimize Existing Code"])
    
    if task_type == "Generate New Implementation":
        task = st.text_area("Describe Parallel Task", 
                           height=150,
                           placeholder="e.g., Process 10,000 images in parallel")
        
        # Concurrency options
        with st.sidebar:
            st.subheader("Concurrency Options")
            worker_count = st.slider("Worker Count", 1, 16, 4)
            comms_model = st.radio("Communication", ["Message Passing", "Shared Channel"])
            fault_tolerance = st.toggle("Fault Tolerance", True)
            use_supervisor = st.toggle("Add Supervisor", False)
        
        if st.button("Generate Ractor Code", type="primary") and task:
            with st.spinner("Building parallel solution..."):
                full_task = f"{task} | Workers: {worker_count} | Communication: {comms_model}"
                if fault_tolerance:
                    full_task += " | With fault tolerance"
                if use_supervisor:
                    full_task += " | With supervisor pattern"
                    
                code = ractor_implementation(full_task)
                st.subheader("Concurrent Implementation")
                st.code(code, language="ruby")
                
                # Performance estimate
                st.metric("Estimated Speedup", f"~{worker_count * 0.8:.1f}x", "vs sequential")
                st.caption("Actual speedup depends on task parallelizability")
                
                # Concurrency concepts
                with st.expander("🧠 Ractor Fundamentals", expanded=True):
                    st.markdown("""
                    **Ractor Model:**
                    - Actor-based concurrency
                    - Isolated object spaces
                    - Copy/Move semantics for messages
                    - No GVL (Global VM Lock) contention
                    
                    **Best Practices:**
                    - Use for CPU-bound tasks
                    - Avoid sharing mutable state
                    - Prefer message passing over shared memory
                    - Handle Ractor::RemoteError exceptions
                    
                    **Message Types:**
                    - **Copy**: Most objects are deep-copied
                    - **Move**: Transfer ownership (Ractor.make_shareable)
                    - **Shareable**: Immutable objects (frozen strings, numbers)
                    """)
                    st.link_button("Ruby Ractor Docs", "https://docs.ruby-lang.org/en/master/Ractor.html")
    
    else:
        code = st.text_area("Paste Ruby Code", 
                           height=300,
                           placeholder="def process_data\n  # ...\nend")
        
        if code:
            # Quick safety check
            issues = detect_concurrency_issues(code)
            if issues:
                st.warning("Potential concurrency issues detected:")
                for issue in issues:
                    st.caption(f"⚠️ {issue}")
        
        if st.button("Optimize Concurrency", type="primary") and code:
            with st.spinner("Analyzing concurrency..."):
                advice = concurrency_advice(code)
                st.subheader("Concurrency Recommendations")
                st.markdown(advice)
                
                # Concurrency patterns
                with st.expander("🔧 Ruby Concurrency Patterns"):
                    st.markdown("""
                    **Thread Pool Pattern:**
                    ```ruby
                    require 'concurrent-ruby'
                    pool = Concurrent::FixedThreadPool.new(5)
                    ```
                    
                    **Fiber Scheduler (Ruby 3+):**
                    ```ruby
                    require 'async'
                    Async do |task|
                      task.async { perform_io }
                    end
                    ```
                    
                    **Ractor Pipeline:**
                    ```ruby
                    pipe = Ractor.new do
                      loop { Ractor.yield(process(Ractor.receive)) }
                    end
                    ```
                    """)
                
                # Save to knowledge base
                if st.button("💾 Save Analysis"):
                    db = DatabaseManager()
                    if db.connected:
                        query_id = db.log_query(
                            tool="ruby_concurrency",
                            model=ModelManager.get_default_model(),
                            prompt=code[:500],
                            response=advice,
                            metadata={"tags": ["ruby", "concurrency", "ractors"]}
                        )
                        if query_id:
                            st.success("Analysis saved!")
                    else:
                        st.error("Could not connect to database")
    
    # Reference section
    with st.expander("📊 Concurrency Model Comparison"):
        comparison_data = {
            "Model": ["Threads", "Ractors", "Fibers", "Processes"],
            "Best For": ["I/O-bound", "CPU-bound", "Cooperative", "Isolation"],
            "GVL": ["Yes", "No", "Yes", "No"],
            "Memory": ["Shared", "Isolated", "Shared", "Isolated"],
            "Overhead": ["Medium", "High", "Low", "Highest"]
        }
        st.table(comparison_data)

if __name__ == "__main__":
    show()
