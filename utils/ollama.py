"""
Ollama integration utilities for TuoKit
Handles model management and safe generation
"""

import subprocess
from typing import Dict, Optional, List
import json

class OllamaManager:
    """Manages Ollama service and models"""
    
    @staticmethod
    def get_status() -> Dict[str, any]:
        """Check Ollama service status"""
        try:
            result = subprocess.run(
                ["ollama", "list", "--json"], 
                capture_output=True, 
                text=True, 
                timeout=5
            )
            
            # Parse JSON output if available
            models = []
            if result.stdout:
                try:
                    models = json.loads(result.stdout)
                except:
                    # Fallback to line parsing
                    models = result.stdout.splitlines()
                    
            return {
                "running": True,
                "model_count": len(models),
                "models": models,
                "error": None
            }
        except subprocess.TimeoutExpired:
            return {
                "running": False,
                "model_count": 0,
                "models": [],
                "error": "Ollama service timeout"
            }
        except Exception as e:
            return {
                "running": False,
                "model_count": 0,
                "models": [],
                "error": str(e)
            }
    
    @staticmethod
    def list_models() -> List[str]:
        """Get list of available models"""
        status = OllamaManager.get_status()
        if status["running"] and status["models"]:
            # Extract model names depending on format
            if isinstance(status["models"][0], dict):
                return [m.get("name", "") for m in status["models"]]
            else:
                return status["models"]
        return []
    
    @staticmethod
    def pull_model(model_name: str) -> bool:
        """Pull a model from Ollama registry"""
        try:
            result = subprocess.run(
                ["ollama", "pull", model_name],
                capture_output=True,
                text=True,
                timeout=300  # 5 minutes for large models
            )
            return result.returncode == 0
        except:
            return False

def safe_ollama_generate(model: str, prompt: str, 
                        temperature: float = 0.7,
                        max_tokens: Optional[int] = None,
                        system: Optional[str] = None) -> Dict:
    """Safely call Ollama generate with error handling and retries"""
    try:
        import ollama
        
        # Build options
        options = {"temperature": temperature}
        if max_tokens:
            options["num_predict"] = max_tokens
            
        # Add system prompt if provided
        if system:
            full_prompt = f"{system}\n\n{prompt}"
        else:
            full_prompt = prompt
            
        response = ollama.generate(
            model=model, 
            prompt=full_prompt,
            options=options
        )
        
        return {
            "response": response.get("response", ""),
            "error": False,
            "model": model,
            "total_duration": response.get("total_duration"),
            "eval_count": response.get("eval_count")
        }
        
    except Exception as e:
        error_msg = str(e)
        
        # Provide helpful error messages
        if "model not found" in error_msg.lower():
            suggestion = f"Model '{model}' not found. Run: ollama pull {model}"
        elif "connection" in error_msg.lower():
            suggestion = "Ollama service not running. Start with: ollama serve"
        else:
            suggestion = "Check Ollama installation and try again"
            
        return {
            "response": f"Error: {error_msg}\n\nSuggestion: {suggestion}",
            "error": True,
            "model": model,
            "error_type": type(e).__name__
        }

class OllamaToolBase:
    """Base class for Ollama-powered tools with automatic logging"""
    
    def __init__(self, tool_name: str, default_model: str = "deepseek-coder:6.7b"):
        self.tool_name = tool_name
        self.default_model = default_model
        
        # Lazy import to avoid circular dependencies
        self._db = None
        
    @property
    def db(self):
        """Lazy load database manager"""
        if self._db is None:
            from .database import DatabaseManager
            self._db = DatabaseManager()
        return self._db
    
    def generate_with_logging(self, prompt: str, model: Optional[str] = None,
                            **kwargs) -> Dict:
        """Generate response and automatically log to knowledge base"""
        model = model or self.default_model
        
        # Generate response
        result = safe_ollama_generate(model, prompt, **kwargs)
        
        # Log if successful and database connected
        if not result["error"] and self.db.connected:
            self.db.log_query(
                tool=self.tool_name,
                model=model,
                prompt=prompt,
                response=result["response"],
                metadata={
                    "duration_ms": result.get("total_duration", 0) / 1_000_000,
                    "tokens": result.get("eval_count", 0)
                }
            )
            
        return result
