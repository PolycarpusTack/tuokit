"""
TuoKit Ollama Client
Handles connections to local or remote Ollama servers
"""
import requests
import json
from typing import Optional, Dict, List, Generator
from config.connection_config import config
import streamlit as st

class RemoteOllamaClient:
    """Client for connecting to remote Ollama servers"""
    
    def __init__(self, host: str = None, api_key: str = None):
        self.host = host or config.ollama_host
        self.api_key = api_key or config.ollama_api_key
        self.headers = {"Authorization": f"Bearer {self.api_key}"} if self.api_key else {}
        
    def _make_request(self, endpoint: str, method: str = "GET", 
                     data: Dict = None, stream: bool = False) -> requests.Response:
        """Make HTTP request to Ollama server"""
        url = f"{self.host}/api/{endpoint}"
        
        try:
            if method == "GET":
                response = requests.get(url, headers=self.headers, timeout=30)
            elif method == "POST":
                response = requests.post(
                    url, 
                    json=data, 
                    headers=self.headers, 
                    stream=stream,
                    timeout=300  # Longer timeout for generation
                )
            
            response.raise_for_status()
            return response
            
        except requests.exceptions.ConnectionError:
            st.error(f"❌ Cannot connect to Ollama server at {self.host}")
            st.info("Make sure the Ollama server is running and accessible")
            raise
        except requests.exceptions.Timeout:
            st.error("⏱️ Request to Ollama server timed out")
            raise
        except Exception as e:
            st.error(f"Error communicating with Ollama: {str(e)}")
            raise
    
    def list_models(self) -> List[Dict]:
        """List available models on the server"""
        response = self._make_request("tags")
        return response.json().get("models", [])
    
    def generate(self, model: str, prompt: str, 
                stream: bool = True, **kwargs) -> Generator[str, None, None]:
        """Generate response from model"""
        data = {
            "model": model,
            "prompt": prompt,
            "stream": stream,
            **kwargs
        }
        
        response = self._make_request("generate", "POST", data, stream=stream)
        
        if stream:
            for line in response.iter_lines():
                if line:
                    chunk = json.loads(line)
                    if "response" in chunk:
                        yield chunk["response"]
        else:
            yield response.json()["response"]
    
    def chat(self, model: str, messages: List[Dict], 
             stream: bool = True, **kwargs) -> Generator[str, None, None]:
        """Chat with model using conversation history"""
        data = {
            "model": model,
            "messages": messages,
            "stream": stream,
            **kwargs
        }
        
        response = self._make_request("chat", "POST", data, stream=stream)
        
        if stream:
            for line in response.iter_lines():
                if line:
                    chunk = json.loads(line)
                    if "message" in chunk and "content" in chunk["message"]:
                        yield chunk["message"]["content"]
        else:
            yield response.json()["message"]["content"]
    
    def pull_model(self, model_name: str) -> bool:
        """Pull a model from Ollama registry (if server supports it)"""
        try:
            data = {"name": model_name}
            response = self._make_request("pull", "POST", data, stream=True)
            
            progress_placeholder = st.empty()
            for line in response.iter_lines():
                if line:
                    chunk = json.loads(line)
                    status = chunk.get("status", "")
                    
                    if "total" in chunk and "completed" in chunk:
                        progress = chunk["completed"] / chunk["total"]
                        progress_placeholder.progress(progress, f"Pulling {model_name}: {status}")
                    else:
                        progress_placeholder.info(f"Pulling {model_name}: {status}")
            
            progress_placeholder.success(f"✅ Successfully pulled {model_name}")
            return True
            
        except Exception as e:
            st.error(f"Failed to pull model: {str(e)}")
            return False

# Convenience function for backward compatibility
def get_ollama_client() -> RemoteOllamaClient:
    """Get configured Ollama client instance"""
    return RemoteOllamaClient()

# Example usage function
def query_ollama(prompt: str, model: str = "deepseek-r1", 
                temperature: float = 0.7, max_tokens: int = 2000) -> str:
    """
    Simple function to query Ollama and get response
    
    Args:
        prompt: The prompt to send to the model
        model: Model name (default: deepseek-r1)
        temperature: Generation temperature (0-1)
        max_tokens: Maximum tokens to generate
        
    Returns:
        Generated text response
    """
    client = get_ollama_client()
    
    # Check if model exists
    models = [m["name"] for m in client.list_models()]
    if model not in models:
        st.warning(f"Model {model} not found. Available models: {', '.join(models)}")
        return ""
    
    # Generate response
    response_parts = []
    for chunk in client.generate(
        model=model,
        prompt=prompt,
        temperature=temperature,
        num_predict=max_tokens
    ):
        response_parts.append(chunk)
    
    return "".join(response_parts)

# Test function for setup verification
def test_ollama_setup() -> bool:
    """Test if Ollama connection is properly configured"""
    try:
        client = get_ollama_client()
        models = client.list_models()
        st.success(f"✅ Connected to Ollama at {client.host}")
        st.info(f"Available models: {', '.join([m['name'] for m in models])}")
        return True
    except Exception as e:
        st.error(f"❌ Ollama connection failed: {str(e)}")
        return False
