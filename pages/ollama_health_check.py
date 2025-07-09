"""
TuoKit - Ollama Health Check Dashboard
Practical monitoring for local LLM infrastructure
No overengineering: Direct API calls, clear status indicators
"""
import streamlit as st
import json
from datetime import datetime
import time
from typing import Dict, List, Optional, Tuple
import sys
import os
import subprocess
import shutil
import psutil

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import TuoKit's existing Ollama utilities
from utils.ollama import get_ollama_manager, get_available_models, safe_ollama_generate
from utils.database import DatabaseManager

# Configuration at top of file (easy to modify)
REFRESH_INTERVAL = 5  # seconds
CONNECTION_TIMEOUT = 3  # seconds
BENCHMARK_CACHE_HOURS = 24  # How long to cache benchmark results

def check_ollama_connection() -> Tuple[bool, str]:
    """
    Simple connection check with meaningful error messages
    Returns: (is_connected, error_message)
    """
    manager = get_ollama_manager()
    status = manager.get_status()
    
    if status["running"]:
        return True, ""
    elif status["error"]:
        return False, f"Connection error: {status['error']}"
    else:
        return False, "Ollama not running (connection refused)"

def get_installed_models() -> List[Dict]:
    """
    Fetch list of installed models with size and modification info
    """
    manager = get_ollama_manager()
    
    # First try the direct API call for detailed info
    try:
        import requests
        response = requests.get(f"{manager.host}/api/tags", timeout=CONNECTION_TIMEOUT)
        if response.status_code == 200:
            data = response.json()
            models = data.get('models', [])
            if models:
                return models
    except Exception:
        # Silent fallback to other methods
        pass
    
    # Fallback to manager's status
    status = manager.get_status()
    model_names = status.get("models", [])
    
    if model_names:
        # Convert simple names to dict format
        return [{"name": model} for model in model_names]
    
    # Final fallback - try to get from available_models function
    from utils.ollama import get_available_models as utils_get_models
    available = utils_get_models()
    if available:
        return [{"name": model} for model in available]
    
    return []

def get_running_models() -> List[Dict]:
    """
    Check which models are currently loaded in memory
    """
    manager = get_ollama_manager()
    try:
        import requests
        response = requests.get(f"{manager.host}/api/ps", timeout=CONNECTION_TIMEOUT)
        if response.status_code == 200:
            data = response.json()
            return data.get('models', [])
    except:
        pass
    return []

def test_model_response(model_name: str) -> Tuple[bool, float, str]:
    """
    Quick test of model responsiveness
    Returns: (success, response_time, error_message)
    """
    test_prompt = "Say 'OK' if you're working"
    start_time = time.time()
    
    # Use TuoKit's safe generation
    response = safe_ollama_generate(
        model_name, 
        test_prompt,
        options={"num_predict": 10}  # Limit response for quick test
    )
    
    response_time = time.time() - start_time
    
    if "error" in response and response["error"]:
        return False, 0, response["response"]
    else:
        return True, response_time, ""

def format_size(size_bytes: int) -> str:
    """Simple size formatting"""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024.0:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024.0
    return f"{size_bytes:.1f} TB"

def get_cached_benchmark(model_name: str) -> Optional[Dict]:
    """Retrieve cached benchmark results if fresh"""
    if "db" not in st.session_state or not st.session_state.db:
        return None
    
    db = st.session_state.db
    
    try:
        with db.conn.cursor() as cursor:
            cursor.execute("""
                SELECT ai_response, created_at 
                FROM queries 
                WHERE tool = 'ollama_health_check' 
                AND user_prompt = %s 
                ORDER BY created_at DESC 
                LIMIT 1
            """, (f"benchmark_{model_name}",))
            
            result = cursor.fetchone()
            
            if result:
                response, timestamp = result
                # Check if cache is fresh
                if (datetime.now() - timestamp).total_seconds() < (BENCHMARK_CACHE_HOURS * 3600):
                    return json.loads(response)
    except Exception as e:
        st.error(f"Database error: {e}")
    
    return None

def save_benchmark_results(model_name: str, results: Dict):
    """Save benchmark results to knowledge base"""
    if "db" not in st.session_state or not st.session_state.db:
        return
    
    db = st.session_state.db
    
    try:
        query_id = db.log_query(
            tool="ollama_health_check",
            model="system",
            prompt=f"benchmark_{model_name}",
            response=json.dumps(results),
            metadata={"type": "benchmark", "model": model_name}
        )
    except Exception as e:
        st.error(f"Failed to save benchmark: {e}")

def benchmark_model(model_name: str) -> Dict:
    """Run performance benchmarks on a model"""
    # Check cache first
    cached = get_cached_benchmark(model_name)
    if cached:
        cached['from_cache'] = True
        return cached
    
    benchmarks = {
        "simple_math": {
            "prompt": "What is 15 + 27? Reply with just the number.",
            "category": "Simple"
        },
        "reasoning": {
            "prompt": "In one sentence, why does ice float on water?",
            "category": "Reasoning"  
        },
        "code_generation": {
            "prompt": "Write a Python one-liner to reverse a string. Code only.",
            "category": "Code"
        }
    }
    
    results = {
        "model": model_name,
        "timestamp": datetime.now().isoformat(),
        "benchmarks": {},
        "from_cache": False
    }
    
    for test_name, test_data in benchmarks.items():
        start_time = time.time()
        
        response = safe_ollama_generate(
            model_name,
            test_data["prompt"],
            options={"num_predict": 100}  # Limit tokens for fair comparison
        )
        
        elapsed = time.time() - start_time
        
        results["benchmarks"][test_name] = {
            "category": test_data["category"],
            "time_seconds": round(elapsed, 2),
            "success": "error" not in response or not response.get("error", False),
            "response_preview": response.get("response", "")[:100]
        }
    
    # Calculate average response time
    times = [b["time_seconds"] for b in results["benchmarks"].values() if b["success"]]
    results["avg_response_time"] = round(sum(times) / len(times), 2) if times else 0
    
    # Save to cache
    save_benchmark_results(model_name, results)
    
    return results

def get_system_resources() -> Dict:
    """Get current system resource usage"""
    resources = {
        "cpu_percent": psutil.cpu_percent(interval=1),
        "memory": psutil.virtual_memory()._asdict(),
        "disk": {}
    }
    
    # Get Ollama models directory size
    try:
        # Try common Ollama model locations
        ollama_dirs = [
            os.path.expanduser("~/.ollama/models"),
            "/usr/share/ollama/.ollama/models",
            os.path.expanduser("~/Library/Application Support/Ollama/models")  # macOS
        ]
        
        for dir_path in ollama_dirs:
            if os.path.exists(dir_path):
                disk_usage = shutil.disk_usage(os.path.dirname(dir_path))
                resources["disk"] = {
                    "total": disk_usage.total,
                    "used": disk_usage.used,
                    "free": disk_usage.free,
                    "percent": (disk_usage.used / disk_usage.total) * 100,
                    "models_path": dir_path
                }
                break
    except:
        pass
    
    # Check for GPU if nvidia-smi is available
    if shutil.which("nvidia-smi"):
        try:
            result = subprocess.run(
                ["nvidia-smi", "--query-gpu=name,memory.used,memory.total,utilization.gpu", "--format=csv,noheader"],
                capture_output=True,
                text=True,
                timeout=2
            )
            if result.returncode == 0:
                gpu_info = result.stdout.strip().split(', ')
                resources["gpu"] = {
                    "name": gpu_info[0],
                    "memory_used": gpu_info[1],
                    "memory_total": gpu_info[2],
                    "utilization": gpu_info[3]
                }
        except:
            pass
    
    return resources

def get_active_pulls() -> List[Dict]:
    """Check for models currently being pulled"""
    active_pulls = []
    
    try:
        # Check running processes for ollama pull commands
        for proc in psutil.process_iter(['pid', 'name', 'cmdline', 'create_time']):
            try:
                if proc.info['name'] and 'ollama' in proc.info['name'].lower():
                    cmdline = proc.info.get('cmdline', [])
                    if cmdline and 'pull' in cmdline:
                        # Extract model name from command
                        model_idx = cmdline.index('pull') + 1
                        if model_idx < len(cmdline):
                            model_name = cmdline[model_idx]
                            
                            # Calculate runtime
                            create_time = proc.info['create_time']
                            runtime = time.time() - create_time
                            
                            active_pulls.append({
                                "model": model_name,
                                "pid": proc.info['pid'],
                                "runtime_seconds": int(runtime),
                                "runtime_display": f"{int(runtime//60)}m {int(runtime%60)}s"
                            })
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
    except:
        pass
    
    return active_pulls

def compare_models(model_a: str, model_b: str, prompt: str) -> Dict:
    """Compare two models side by side"""
    comparison = {
        "prompt": prompt,
        "timestamp": datetime.now().isoformat(),
        "results": {}
    }
    
    for model in [model_a, model_b]:
        start_time = time.time()
        
        response = safe_ollama_generate(
            model,
            prompt,
            options={"num_predict": 500}  # Reasonable limit for comparison
        )
        
        elapsed = time.time() - start_time
        
        comparison["results"][model] = {
            "response": response.get("response", "Error generating response"),
            "time_seconds": round(elapsed, 2),
            "success": "error" not in response or not response.get("error", False),
            "response_length": len(response.get("response", ""))
        }
    
    # Save comparison to knowledge base
    if "db" in st.session_state and st.session_state.db:
        try:
            db = st.session_state.db
            query_id = db.log_query(
                tool="ollama_health_check",
                model="system",
                prompt=f"comparison_{model_a}_vs_{model_b}",
                response=json.dumps(comparison),
                metadata={"type": "model_comparison"}
            )
        except Exception as e:
            st.error(f"Failed to save comparison: {e}")
    
    return comparison

def init_database():
    """Initialize database connection in session state"""
    if "db" not in st.session_state:
        try:
            st.session_state.db = DatabaseManager()
        except Exception as e:
            st.error(f"Database connection failed: {e}")
            st.session_state.db = None

def main():
    st.set_page_config(
        page_title="Ollama Health Check",
        page_icon="🏥",
        layout="wide"
    )
    
    # Initialize database
    init_database()
    
    st.title("🏥 Ollama Health Check Dashboard")
    st.markdown("Monitor your local LLM infrastructure")
    
    # Auto-refresh option
    col1, col2, col3 = st.columns([2, 1, 1])
    with col1:
        auto_refresh = st.checkbox("Auto-refresh", value=False)
    with col2:
        if st.button("🔄 Refresh Now"):
            st.rerun()
    with col3:
        st.caption(f"Last update: {datetime.now().strftime('%H:%M:%S')}")
    
    # Get manager instance
    manager = get_ollama_manager()
    
    # Connection Status Card
    st.subheader("🔌 Connection Status")
    is_connected, error_msg = check_ollama_connection()
    
    if is_connected:
        st.success("✅ Ollama is running and accessible")
    else:
        st.error(f"❌ Connection Failed: {error_msg}")
        st.info("""
        **Troubleshooting steps:**
        1. Check if Ollama is installed: `ollama --version`
        2. Start Ollama service: `ollama serve`
        3. Verify it's running on port 11434
        4. Check firewall settings if needed
        """)
        # Don't show other sections if not connected
        return
    
    # System Overview
    col1, col2 = st.columns(2)
    
    with col1:
        st.subheader("📦 Installed Models")
        models = get_installed_models()
        
        
        if models:
            for model in models:
                with st.expander(f"**{model['name']}**", expanded=False):
                    col_a, col_b = st.columns(2)
                    with col_a:
                        st.metric("Size", format_size(model.get('size', 0)))
                    with col_b:
                        modified = model.get('modified_at', '')
                        if modified:
                            # Parse and format the date nicely
                            try:
                                dt = datetime.fromisoformat(modified.replace('Z', '+00:00'))
                                st.metric("Modified", dt.strftime('%Y-%m-%d'))
                            except:
                                st.metric("Modified", "Unknown")
                    
                    # Quick test button for each model
                    if st.button(f"🧪 Test {model['name']}", key=f"test_{model['name']}"):
                        with st.spinner(f"Testing {model['name']}..."):
                            success, response_time, error = test_model_response(model['name'])
                            if success:
                                st.success(f"✅ Response in {response_time:.2f}s")
                            else:
                                st.error(f"❌ {error}")
        else:
            st.warning("No models installed. Run `ollama pull <model>` to install one.")
    
    with col2:
        st.subheader("🏃 Running Models")
        running = get_running_models()
        
        if running:
            for model in running:
                st.info(f"**{model['name']}**")
                # Display memory usage if available
                size = model.get('size', 0)
                if size:
                    st.metric("Memory Usage", format_size(size))
                # Show when it expires from memory
                expires = model.get('expires_at', '')
                if expires:
                    st.caption(f"Expires: {expires}")
        else:
            st.info("No models currently loaded in memory")
    
    # Quick Actions
    st.subheader("⚡ Quick Actions")
    col1, col2, col3 = st.columns(3)
    
    with col1:
        model_to_pull = st.text_input("Pull new model:", placeholder="llama2")
        if st.button("📥 Pull Model"):
            if model_to_pull:
                st.info(f"Run in terminal: `ollama pull {model_to_pull}`")
                st.code(f"ollama pull {model_to_pull}", language="bash")
    
    with col2:
        if models:
            model_to_run = st.selectbox("Run model:", [m['name'] for m in models])
            if st.button("▶️ Start Model"):
                st.info(f"Run in terminal: `ollama run {model_to_run}`")
                st.code(f"ollama run {model_to_run}", language="bash")
    
    with col3:
        if st.button("📊 Show System Info"):
            with st.spinner("Gathering system info..."):
                resources = get_system_resources()
                
                # CPU and Memory
                st.metric("CPU Usage", f"{resources['cpu_percent']}%")
                mem = resources['memory']
                st.metric("Memory", f"{mem['percent']:.1f}%", 
                         f"{format_size(mem['used'])} / {format_size(mem['total'])}")
                
                # Disk space for models
                if resources['disk']:
                    st.metric("Disk Free", format_size(resources['disk']['free']),
                             f"{resources['disk']['percent']:.1f}% used")
                
                # GPU info if available
                if 'gpu' in resources:
                    gpu = resources['gpu']
                    st.info(f"GPU: {gpu['name']}")
                    st.metric("GPU Memory", f"{gpu['memory_used']} / {gpu['memory_total']}")
                    st.metric("GPU Utilization", gpu['utilization'])
    
    # Health History with knowledge capture
    with st.expander("📈 Health Check History", expanded=False):
        # Capture current health status
        if st.button("📊 Save Current Status"):
            if st.session_state.db:
                health_data = {
                    "timestamp": datetime.now().isoformat(),
                    "connected": is_connected,
                    "model_count": len(models),
                    "running_models": len(running),
                    "models": [m['name'] for m in models],
                    "host": manager.host
                }
                
                try:
                    query_id = st.session_state.db.log_query(
                        tool="ollama_health_check",
                        model="system",
                        prompt="health_status_snapshot",
                        response=json.dumps(health_data),
                        metadata={"type": "health_check"}
                    )
                    st.success("✅ Health status saved to knowledge base")
                except Exception as e:
                    st.error(f"Failed to save status: {e}")
            else:
                st.warning("Database not connected")
        
        # Display recent health history
        history = []
        if st.session_state.db:
            try:
                with st.session_state.db.conn.cursor() as cursor:
                    cursor.execute("""
                        SELECT created_at, ai_response 
                        FROM queries 
                        WHERE tool = 'ollama_health_check' 
                        AND user_prompt = 'health_status_snapshot'
                        ORDER BY created_at DESC 
                        LIMIT 10
                    """)
                    history = cursor.fetchall()
            except Exception as e:
                st.error(f"Failed to load history: {e}")
        
        if history:
            st.subheader("Recent Health Checks")
            for timestamp, data_json in history:
                data = json.loads(data_json)
                with st.container():
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.caption(f"**{timestamp}**")
                    with col2:
                        st.caption(f"Models: {data.get('model_count', 0)}")
                    with col3:
                        status_icon = "✅" if data.get('connected') else "❌"
                        st.caption(f"Status: {status_icon}")
        else:
            st.info("No health history yet. Click 'Save Current Status' to start tracking.")
    
    # Model Performance Benchmarking
    st.subheader("⚡ Model Performance Benchmarking")
    
    if models:
        col1, col2 = st.columns([3, 1])
        with col1:
            selected_model = st.selectbox("Select model to benchmark:", 
                                        [m['name'] for m in models],
                                        key="benchmark_model")
        with col2:
            if st.button("🏃 Run Benchmark"):
                with st.spinner(f"Benchmarking {selected_model}..."):
                    results = benchmark_model(selected_model)
                    
                    if results.get('from_cache'):
                        st.info("📦 Results from cache (less than 24h old)")
                    
                    # Display results
                    st.subheader(f"Results for {selected_model}")
                    
                    # Average response time as main metric
                    col1, col2, col3 = st.columns(3)
                    with col1:
                        st.metric("Avg Response Time", f"{results['avg_response_time']}s")
                    with col2:
                        successful = sum(1 for b in results['benchmarks'].values() if b['success'])
                        st.metric("Tests Passed", f"{successful}/3")
                    with col3:
                        st.metric("Tested At", results['timestamp'].split('T')[1].split('.')[0])
                    
                    # Individual benchmark results
                    for test_name, test_result in results['benchmarks'].items():
                        with st.expander(f"{test_result['category']} Test - {test_result['time_seconds']}s"):
                            if test_result['success']:
                                st.success(f"✅ Completed in {test_result['time_seconds']}s")
                                st.caption("Response preview:")
                                st.code(test_result['response_preview'])
                            else:
                                st.error("❌ Test failed")
    else:
        st.info("No models found. Pull a model first using the Quick Actions above.")
    
    # Active Downloads Monitor
    with st.expander("📥 Active Model Downloads", expanded=False):
        active_pulls = get_active_pulls()
        
        if active_pulls:
            st.warning(f"🔄 {len(active_pulls)} model(s) currently downloading")
            
            for pull in active_pulls:
                col1, col2, col3 = st.columns([2, 1, 1])
                with col1:
                    st.info(f"**{pull['model']}**")
                with col2:
                    st.caption(f"Runtime: {pull['runtime_display']}")
                with col3:
                    st.caption(f"PID: {pull['pid']}")
                
                # Progress indication (since we can't get actual progress)
                st.progress(0.5, text="Progress unknown - check terminal for details")
        else:
            st.info("No active downloads detected")
            st.caption("Start a download with: `ollama pull model_name`")
    
    # Model Comparison Tool
    with st.expander("🔍 Model Comparison", expanded=False):
        if len(models) >= 2:
            col1, col2 = st.columns(2)
            
            with col1:
                model_a = st.selectbox("Model A", [m['name'] for m in models], key="compare_a")
            with col2:
                model_b = st.selectbox("Model B", [m['name'] for m in models], 
                                     index=1 if len(models) > 1 else 0, key="compare_b")
            
            test_prompt = st.text_area("Test prompt:", 
                                      value="Explain the concept of recursion in programming",
                                      height=100)
            
            if st.button("⚔️ Compare Models"):
                if model_a != model_b:
                    with st.spinner(f"Comparing {model_a} vs {model_b}..."):
                        comparison = compare_models(model_a, model_b, test_prompt)
                        
                        # Display results side by side
                        col1, col2 = st.columns(2)
                        
                        for idx, (model, result) in enumerate(comparison['results'].items()):
                            col = col1 if idx == 0 else col2
                            
                            with col:
                                st.subheader(model)
                                
                                if result['success']:
                                    st.metric("Response Time", f"{result['time_seconds']}s")
                                    st.metric("Response Length", f"{result['response_length']} chars")
                                    
                                    st.caption("Response:")
                                    st.text_area("", result['response'], height=300, 
                                               key=f"response_{model}")
                                else:
                                    st.error("❌ Failed to generate response")
                        
                        # Winner declaration
                        if all(r['success'] for r in comparison['results'].values()):
                            times = {m: r['time_seconds'] for m, r in comparison['results'].items()}
                            faster_model = min(times, key=times.get)
                            speed_diff = max(times.values()) - min(times.values())
                            st.success(f"🏆 {faster_model} was {speed_diff:.1f}s faster!")
                else:
                    st.warning("Please select different models to compare")
        else:
            st.info("Need at least 2 models installed for comparison")
    
    # System Alerts
    if is_connected:
        resources = get_system_resources()
        
        # Check for issues and display alerts
        alerts = []
        
        if resources['cpu_percent'] > 90:
            alerts.append(("⚠️ High CPU Usage", f"CPU at {resources['cpu_percent']}% - may slow responses"))
        
        if resources['memory']['percent'] > 85:
            alerts.append(("⚠️ High Memory Usage", f"Memory at {resources['memory']['percent']:.1f}% - consider closing other apps"))
        
        if resources['disk'] and resources['disk']['free'] < 5_000_000_000:  # 5GB
            free_gb = resources['disk']['free'] / (1024**3)
            alerts.append(("⚠️ Low Disk Space", f"Only {free_gb:.1f}GB free - may prevent new model downloads"))
        
        if alerts:
            st.divider()
            st.subheader("⚠️ System Alerts")
            for title, message in alerts:
                st.warning(f"{title}: {message}")
    
    # Auto-refresh logic
    if auto_refresh:
        time.sleep(REFRESH_INTERVAL)
        st.rerun()

# TODO: Add GPU memory monitoring for CUDA-enabled systems
# TODO: Implement model download progress tracking
# TODO: Add alert system for model crashes
# TODO: Create model performance benchmarking tool

if __name__ == "__main__":
    main()