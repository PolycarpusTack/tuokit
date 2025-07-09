"""
Ollama Health Checker for TuoKit
Monitors Ollama service and model performance
"""

import streamlit as st
import json
from datetime import datetime
import time
from typing import Dict, List, Optional, Tuple
import subprocess
import shutil
import psutil
import os

from utils.ollama import get_ollama_manager, get_available_models, safe_ollama_generate
from .config import OLLAMA_DEFAULT_HOST, REFRESH_INTERVAL, CONNECTION_TIMEOUT, BENCHMARK_CACHE_HOURS

class OllamaHealthChecker:
    """Ollama service health monitoring"""
    
    def __init__(self):
        self.manager = get_ollama_manager()
        self._status_cache = None
        self._last_check = None
        self._cache_duration = 30  # seconds
    
    def get_quick_status(self) -> Dict[str, any]:
        """Get quick status for overview dashboard"""
        # Use cache if fresh
        if self._status_cache and self._last_check:
            if time.time() - self._last_check < self._cache_duration:
                return self._status_cache
        
        status = {
            'connected': False,
            'model_count': 0,
            'running_models': 0,
            'error': None
        }
        
        try:
            manager_status = self.manager.get_status()
            
            if manager_status["running"]:
                status['connected'] = True
                status['model_count'] = len(manager_status.get("models", []))
                
                # Check running models
                running = self._get_running_models()
                status['running_models'] = len(running)
            else:
                status['error'] = manager_status.get('error', 'Ollama not running')
                
        except Exception as e:
            status['error'] = str(e)
        
        # Cache the result
        self._status_cache = status
        self._last_check = time.time()
        
        return status
    
    def get_detailed_status(self) -> Dict[str, any]:
        """Get detailed status for reporting"""
        status = self.get_quick_status()
        
        if not status['connected']:
            return status
        
        # Get installed models with details
        models = self._get_installed_models()
        status['models'] = []
        
        for model in models:
            model_info = {
                'name': model.get('name', 'Unknown'),
                'size': model.get('size', 0),
                'modified': model.get('modified_at', '')
            }
            status['models'].append(model_info)
        
        # Get system resources
        resources = self._get_system_resources()
        status['resources'] = {
            'cpu_percent': resources['cpu_percent'],
            'memory_percent': resources['memory']['percent'],
            'gpu': resources.get('gpu')
        }
        
        # Get active pulls
        status['active_pulls'] = self._get_active_pulls()
        
        return status
    
    def run_diagnostics(self) -> Dict[str, Dict[str, any]]:
        """Run diagnostic tests"""
        diagnostics = {}
        
        # Test 1: Service availability
        status = self.manager.get_status()
        if status["running"]:
            diagnostics['service'] = {
                'passed': True,
                'message': 'Ollama service is running'
            }
        else:
            diagnostics['service'] = {
                'passed': False,
                'message': f"Service not running: {status.get('error', 'Connection refused')}"
            }
            return diagnostics  # Can't continue without service
        
        # Test 2: Model availability
        models = get_available_models()
        if models:
            diagnostics['models'] = {
                'passed': True,
                'message': f'{len(models)} models available'
            }
        else:
            diagnostics['models'] = {
                'passed': False,
                'message': 'No models installed'
            }
        
        # Test 3: Model responsiveness
        if models:
            test_model = models[0]
            success, response_time, error = self._test_model_response(test_model)
            
            if success:
                diagnostics['responsiveness'] = {
                    'passed': True,
                    'message': f'Model {test_model} responding ({response_time:.2f}s)'
                }
            else:
                diagnostics['responsiveness'] = {
                    'passed': False,
                    'message': f'Model {test_model} error: {error}'
                }
        
        # Test 4: Resource availability
        resources = self._get_system_resources()
        if resources['memory']['percent'] < 90:
            diagnostics['resources'] = {
                'passed': True,
                'message': f"Memory usage: {resources['memory']['percent']:.1f}%"
            }
        else:
            diagnostics['resources'] = {
                'passed': False,
                'message': f"High memory usage: {resources['memory']['percent']:.1f}%"
            }
        
        return diagnostics
    
    def _get_installed_models(self) -> List[Dict]:
        """Fetch list of installed models with details"""
        try:
            import requests
            response = requests.get(f"{self.manager.host}/api/tags", timeout=CONNECTION_TIMEOUT)
            if response.status_code == 200:
                data = response.json()
                return data.get('models', [])
        except Exception:
            pass
        
        # Fallback to simple list
        model_names = get_available_models()
        return [{"name": model} for model in model_names]
    
    def _get_running_models(self) -> List[Dict]:
        """Check which models are currently loaded"""
        try:
            import requests
            response = requests.get(f"{self.manager.host}/api/ps", timeout=CONNECTION_TIMEOUT)
            if response.status_code == 200:
                data = response.json()
                return data.get('models', [])
        except:
            pass
        return []
    
    def _test_model_response(self, model_name: str) -> Tuple[bool, float, str]:
        """Quick test of model responsiveness"""
        test_prompt = "Say 'OK' if you're working"
        start_time = time.time()
        
        response = safe_ollama_generate(
            model_name, 
            test_prompt,
            options={"num_predict": 10}
        )
        
        response_time = time.time() - start_time
        
        if "error" in response and response["error"]:
            return False, 0, response["response"]
        else:
            return True, response_time, ""
    
    def _get_system_resources(self) -> Dict:
        """Get current system resource usage"""
        resources = {
            "cpu_percent": psutil.cpu_percent(interval=1),
            "memory": psutil.virtual_memory()._asdict(),
            "disk": {}
        }
        
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
    
    def _get_active_pulls(self) -> List[Dict]:
        """Check for models currently being pulled"""
        active_pulls = []
        
        try:
            for proc in psutil.process_iter(['pid', 'name', 'cmdline', 'create_time']):
                try:
                    if proc.info['name'] and 'ollama' in proc.info['name'].lower():
                        cmdline = proc.info.get('cmdline', [])
                        if cmdline and 'pull' in cmdline:
                            model_idx = cmdline.index('pull') + 1
                            if model_idx < len(cmdline):
                                model_name = cmdline[model_idx]
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
    
    def run(self):
        """Run the Ollama health check interface"""
        st.subheader("🤖 Ollama Health Check")
        
        # Auto-refresh option
        col1, col2, col3 = st.columns([2, 1, 1])
        with col1:
            auto_refresh = st.checkbox("Auto-refresh", value=False, key="ollama_auto_refresh")
        with col2:
            if st.button("🔄 Refresh", key="ollama_refresh"):
                self._status_cache = None  # Clear cache
                st.rerun()
        with col3:
            st.caption(f"Last: {datetime.now().strftime('%H:%M:%S')}")
        
        # Connection status
        status = self.get_quick_status()
        
        if status['connected']:
            st.success("✅ Ollama service running")
            
            # Show metrics
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Installed Models", status['model_count'])
            with col2:
                st.metric("Running Models", status['running_models'])
            with col3:
                resources = self._get_system_resources()
                st.metric("Memory Usage", f"{resources['memory']['percent']:.1f}%")
            
            # Model list
            models = self._get_installed_models()
            if models:
                st.markdown("**Installed Models:**")
                
                for model in models:
                    with st.expander(f"📦 {model.get('name', 'Unknown')}", expanded=False):
                        col1, col2, col3 = st.columns(3)
                        
                        with col1:
                            size = model.get('size', 0)
                            if size:
                                st.caption(f"Size: {self._format_size(size)}")
                        
                        with col2:
                            modified = model.get('modified_at', '')
                            if modified:
                                try:
                                    dt = datetime.fromisoformat(modified.replace('Z', '+00:00'))
                                    st.caption(f"Modified: {dt.strftime('%Y-%m-%d')}")
                                except:
                                    pass
                        
                        with col3:
                            if st.button(f"🧪 Test", key=f"test_{model.get('name')}"):
                                with st.spinner("Testing..."):
                                    success, resp_time, error = self._test_model_response(model['name'])
                                    if success:
                                        st.success(f"✅ Response in {resp_time:.2f}s")
                                    else:
                                        st.error(f"❌ {error}")
            
            # Active downloads
            active_pulls = self._get_active_pulls()
            if active_pulls:
                st.warning(f"🔄 {len(active_pulls)} model(s) downloading")
                for pull in active_pulls:
                    st.info(f"Downloading {pull['model']} ({pull['runtime_display']})")
            
            # Quick actions
            with st.expander("⚡ Quick Actions", expanded=False):
                col1, col2 = st.columns(2)
                
                with col1:
                    model_to_pull = st.text_input("Pull model:", placeholder="llama2")
                    if st.button("📥 Pull"):
                        if model_to_pull:
                            st.code(f"ollama pull {model_to_pull}", language="bash")
                
                with col2:
                    if models:
                        model_to_run = st.selectbox("Run model:", [m['name'] for m in models])
                        if st.button("▶️ Run"):
                            st.code(f"ollama run {model_to_run}", language="bash")
        
        else:
            st.error("❌ Ollama service not running")
            if status['error']:
                st.error(f"Error: {status['error']}")
            
            st.info("""
            **To start Ollama:**
            1. Install: Visit https://ollama.ai
            2. Start service: `ollama serve`
            3. Pull a model: `ollama pull llama2`
            """)
        
        # Auto-refresh
        if auto_refresh:
            time.sleep(REFRESH_INTERVAL)
            st.rerun()
    
    def _format_size(self, size_bytes: int) -> str:
        """Format bytes into human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size_bytes < 1024.0:
                return f"{size_bytes:.1f} {unit}"
            size_bytes /= 1024.0
        return f"{size_bytes:.1f} TB"