"""
TuoKit Unified Ollama Utilities
Consolidates all Ollama functionality into a single, reliable module
"""

import os
import sys
import subprocess
import json
import time
import platform
from typing import Dict, List, Optional, Tuple, Any
from pathlib import Path

# Try importing requests, but make it optional
try:
    import requests
    REQUESTS_AVAILABLE = True
except ImportError:
    REQUESTS_AVAILABLE = False

# Try importing ollama package
try:
    import ollama
    OLLAMA_PACKAGE_AVAILABLE = True
except ImportError:
    OLLAMA_PACKAGE_AVAILABLE = False


class OllamaManager:
    """Unified Ollama manager with all functionality consolidated"""
    
    def __init__(self):
        self.host = self._detect_ollama_host()
        self._set_environment()
        
    def _detect_ollama_host(self) -> str:
        """Auto-detect Ollama host with multiple fallback strategies"""
        # Priority 1: Environment variable (check both TuoKit and standard)
        if os.getenv("TUOKIT_OLLAMA_HOST"):
            return os.getenv("TUOKIT_OLLAMA_HOST")
        if os.getenv("OLLAMA_HOST"):
            return os.getenv("OLLAMA_HOST")
            
        # Priority 2: Check localhost
        localhost_url = "http://localhost:11434"
        if self._test_connection(localhost_url):
            return localhost_url
            
        # Priority 3: WSL2 auto-detection (Windows host)
        if platform.system() == "Linux" and "microsoft" in platform.release().lower():
            wsl_host = self._detect_wsl_host()
            if wsl_host:
                wsl_url = f"http://{wsl_host}:11434"
                if self._test_connection(wsl_url):
                    print(f"[INFO] Auto-detected Ollama on Windows host: {wsl_url}")
                    return wsl_url
                    
        # Priority 4: Docker host
        docker_url = "http://host.docker.internal:11434"
        if self._test_connection(docker_url):
            return docker_url
            
        # Default fallback
        return localhost_url
        
    def _detect_wsl_host(self) -> Optional[str]:
        """Detect Windows host IP from WSL2"""
        try:
            # Method 1: /etc/resolv.conf
            if os.path.exists("/etc/resolv.conf"):
                with open("/etc/resolv.conf", "r") as f:
                    for line in f:
                        if line.startswith("nameserver"):
                            return line.split()[1]
                            
            # Method 2: ip route command
            result = subprocess.run(
                ["ip", "route", "show", "default"],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                parts = result.stdout.split()
                if len(parts) > 2:
                    return parts[2]
                    
        except Exception:
            pass
            
        return None
        
    def _test_connection(self, url: str) -> bool:
        """Test if Ollama is accessible at given URL"""
        if REQUESTS_AVAILABLE:
            try:
                response = requests.get(f"{url}/api/tags", timeout=2)
                return response.status_code == 200
            except:
                return False
        else:
            # Fallback to subprocess curl
            try:
                result = subprocess.run(
                    ["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", f"{url}/api/tags"],
                    capture_output=True,
                    text=True,
                    timeout=2
                )
                return result.stdout.strip() == "200"
            except:
                return False
                
    def _set_environment(self):
        """Set Ollama environment variables"""
        os.environ["OLLAMA_HOST"] = self.host
        os.environ["TUOKIT_OLLAMA_HOST"] = self.host
        
    def get_status(self) -> Dict[str, Any]:
        """Get comprehensive Ollama status"""
        status = {
            "running": False,
            "host": self.host,
            "model_count": 0,
            "models": [],
            "error": None,
            "version": None,
            "memory": {}
        }
        
        try:
            if OLLAMA_PACKAGE_AVAILABLE:
                # Use ollama package
                models_response = ollama.list()
                if models_response and hasattr(models_response, 'models'):
                    status["running"] = True
                    status["models"] = [m.model for m in models_response.models]
                    status["model_count"] = len(status["models"])
                    
                # Try to get version
                try:
                    version_data = ollama.show("llama2")  # Any model to get version
                    if "version" in version_data:
                        status["version"] = version_data["version"]
                except:
                    pass
                    
            elif REQUESTS_AVAILABLE:
                # Use requests
                response = requests.get(f"{self.host}/api/tags", timeout=5)
                if response.status_code == 200:
                    data = response.json()
                    status["running"] = True
                    status["models"] = [m["name"] for m in data.get("models", [])]
                    status["model_count"] = len(status["models"])
                    
            else:
                # Use subprocess
                result = subprocess.run(
                    ["curl", "-s", f"{self.host}/api/tags"],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0:
                    data = json.loads(result.stdout)
                    status["running"] = True
                    status["models"] = [m["name"] for m in data.get("models", [])]
                    status["model_count"] = len(status["models"])
                    
        except Exception as e:
            status["error"] = str(e)
            
        return status
        
    def pull_model(self, model_name: str) -> bool:
        """Pull a model with progress tracking"""
        print(f"Pulling model: {model_name}")
        
        try:
            if OLLAMA_PACKAGE_AVAILABLE:
                # Use ollama package with progress
                stream = ollama.pull(model_name, stream=True)
                for chunk in stream:
                    if "status" in chunk:
                        print(f"  {chunk['status']}", end="\r")
                print(f"\n[OK] Model {model_name} pulled successfully")
                return True
            else:
                # Use subprocess
                result = subprocess.run(
                    ["ollama", "pull", model_name],
                    capture_output=True,
                    text=True
                )
                if result.returncode == 0:
                    print(f"[OK] Model {model_name} pulled successfully")
                    return True
                else:
                    print(f"[ERROR] Failed to pull {model_name}: {result.stderr}")
                    return False
                    
        except Exception as e:
            print(f"[ERROR] Error pulling model: {e}")
            return False
            
    def generate(self, model: str, prompt: str, **kwargs) -> Dict[str, Any]:
        """Generate response with automatic fallbacks"""
        try:
            if OLLAMA_PACKAGE_AVAILABLE:
                # Extract temperature if provided
                temperature = kwargs.pop('temperature', None)
                
                # Build options dict for Ollama
                options = {}
                if temperature is not None:
                    options['temperature'] = temperature
                
                # Generate response with options
                response = ollama.generate(
                    model=model, 
                    prompt=prompt, 
                    options=options if options else None,
                    **kwargs
                )
                
                # Normalize to dict format
                if hasattr(response, 'response'):
                    # Ollama package returns object with response attribute
                    return {"response": response.response}
                elif hasattr(response, 'model_dump'):
                    # New ollama package returns objects
                    return response.model_dump()
                elif isinstance(response, dict):
                    return response
                else:
                    # Convert response object to dict
                    return {"response": str(response)}
            else:
                # Fallback to subprocess
                data = {
                    "model": model,
                    "prompt": prompt,
                    **kwargs
                }
                
                result = subprocess.run(
                    ["curl", "-X", "POST", f"{self.host}/api/generate",
                     "-H", "Content-Type: application/json",
                     "-d", json.dumps(data)],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode == 0:
                    return json.loads(result.stdout)
                else:
                    return {"error": result.stderr}
                    
        except Exception as e:
            return {"error": str(e)}
            
    def start_service(self) -> bool:
        """Start Ollama service if not running"""
        if self.get_status()["running"]:
            print("[OK] Ollama already running")
            return True
            
        print("Starting Ollama service...")
        
        try:
            # Try different methods based on platform
            if platform.system() == "Windows":
                subprocess.Popen(["ollama", "serve"], shell=True)
            elif platform.system() == "Darwin":  # macOS
                subprocess.Popen(["ollama", "serve"])
            else:  # Linux
                subprocess.Popen(["ollama", "serve"], stdout=subprocess.DEVNULL)
                
            # Wait for service to start
            for i in range(10):
                time.sleep(1)
                if self.get_status()["running"]:
                    print("[OK] Ollama service started")
                    return True
                    
            print("[ERROR] Ollama service failed to start")
            return False
            
        except Exception as e:
            print(f"[ERROR] Error starting service: {e}")
            return False


# Convenience functions
_manager = None

def get_ollama_manager() -> OllamaManager:
    """Get singleton OllamaManager instance"""
    global _manager
    if _manager is None:
        _manager = OllamaManager()
    return _manager

def get_available_models(preferred_models: List[str] = None) -> List[str]:
    """
    Get list of available models
    
    Args:
        preferred_models: Optional list of preferred model names to prioritize
                         If provided, returns these models first if they're available
    
    Returns:
        List of available model names
    """
    manager = get_ollama_manager()
    status = manager.get_status()
    available = status.get("models", [])
    
    if preferred_models and available:
        # Create a list with preferred models first, then others
        result = []
        
        # Add preferred models that are actually available
        for model in preferred_models:
            if model in available:
                result.append(model)
        
        # Add remaining available models
        for model in available:
            if model not in result:
                result.append(model)
        
        return result
    
    return available

# Track failed model pulls to avoid infinite retry loops
import threading
_failed_model_pulls = set()
_failed_model_pulls_lock = threading.Lock()

def safe_ollama_generate(model: str, prompt: str, **kwargs) -> Dict[str, Any]:
    """Safe generation with automatic error handling"""
    global _failed_model_pulls
    
    manager = get_ollama_manager()
    
    # Ensure service is running
    if not manager.get_status()["running"]:
        if not manager.start_service():
            return {"response": "Error: Ollama service not available", "error": "Service not available"}
    
    # Check if model exists
    available_models = get_available_models()
    if model not in available_models:
        # Check if we've already failed to pull this model
        with _failed_model_pulls_lock:
            if model in _failed_model_pulls:
                return {"response": f"Error: Model {model} not available (pull previously failed)", "error": "Model pull failed"}
        
        print(f"Model {model} not found, attempting to pull...")
        if not manager.pull_model(model):
            # Mark this model as failed to avoid retry loops
            with _failed_model_pulls_lock:
                _failed_model_pulls.add(model)
            return {"response": f"Error: Could not pull model {model}", "error": "Could not pull model"}
        
        # If pull succeeded, remove from failed set (in case it was there from before)
        with _failed_model_pulls_lock:
            _failed_model_pulls.discard(model)
            
    # Generate response
    response = manager.generate(model, prompt, **kwargs)
    
    # Check if response is valid
    if not response:
        return {"response": "Error: Empty response from Ollama", "error": "Empty response"}
    
    if "error" in response:
        error_msg = str(response.get('error', 'Unknown error'))
        # Consistent error format - always string
        return {"response": f"Error: {error_msg}", "error": error_msg}
    
    # Check if response has the expected structure
    if 'response' not in response:
        # Log to stderr instead of stdout
        import sys
        print(f"Warning: Unexpected Ollama response structure", file=sys.stderr)
        return {"response": "Error: Invalid response format from Ollama", "error": "Invalid format"}
    
    return response

def clear_failed_model_cache():
    """Clear the cache of failed model pulls"""
    global _failed_model_pulls
    with _failed_model_pulls_lock:
        _failed_model_pulls.clear()

def get_failed_models():
    """Get list of models that failed to pull"""
    global _failed_model_pulls
    return list(_failed_model_pulls)

def check_model_availability(model_name: str) -> Dict[str, Any]:
    """Check if a model is available or can be pulled"""
    manager = get_ollama_manager()
    available_models = get_available_models()
    
    result = {
        "available": model_name in available_models,
        "can_attempt_pull": model_name not in _failed_model_pulls,
        "status": "unknown"
    }
    
    if result["available"]:
        result["status"] = "ready"
    elif not result["can_attempt_pull"]:
        result["status"] = "pull_failed"
    else:
        result["status"] = "needs_pull"
    
    return result

def test_ollama_setup() -> Tuple[bool, str]:
    """Comprehensive Ollama setup test"""
    print("[TEST] Testing Ollama Setup")
    print("=" * 60)
    
    manager = get_ollama_manager()
    status = manager.get_status()
    
    print(f"\n1. Connection Test:")
    print(f"   Host: {status['host']}")
    print(f"   Running: {status['running']}")
    
    if not status['running']:
        print("\n2. Attempting to start service...")
        if manager.start_service():
            status = manager.get_status()
        else:
            return False, "Could not start Ollama service"
            
    print(f"\n3. Models Available: {status['model_count']}")
    if status['models']:
        for model in status['models'][:5]:
            print(f"   - {model}")
        if len(status['models']) > 5:
            print(f"   ... and {len(status['models']) - 5} more")
            
    print("\n4. Test Generation:")
    response = safe_ollama_generate(
        "deepseek-r1:1.5b",
        "Say 'Hello, TuoKit!' in exactly 3 words"
    )
    
    if "error" in response and response["error"]:
        print(f"   [ERROR] Generation failed: {response['response']}")
        return False, response['response']
    else:
        print(f"   [OK] Response: {response.get('response', '')[:50]}...")
        
    print("\n[OK] All tests passed!")
    return True, "Ollama is working correctly"


# For backward compatibility
class OllamaToolBase:
    """Base class for tools using Ollama"""
    
    def __init__(self, tool_name: str, default_model: str = "deepseek-r1:1.5b"):
        self.tool_name = tool_name
        self.default_model = default_model
        self.manager = get_ollama_manager()
        
    def generate(self, prompt: str, model: str = None) -> str:
        """Generate response using Ollama"""
        model = model or self.default_model
        response = safe_ollama_generate(model, prompt)
        
        if "error" in response and response["error"]:
            return f"Error: {response['response']}"
            
        return response.get("response", "")


if __name__ == "__main__":
    # Run test when executed directly
    success, message = test_ollama_setup()
    if not success:
        sys.exit(1)
