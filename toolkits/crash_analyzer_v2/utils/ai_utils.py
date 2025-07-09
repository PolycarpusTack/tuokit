"""
AI utilities for Crash Analyzer V2
Provides timeout handling and circuit breaker functionality
"""
import time
import logging
import functools
from typing import Optional, Dict, Any, Callable
from threading import Timer
import concurrent.futures

from utils import safe_ollama_generate

logger = logging.getLogger(__name__)

class AIServiceError(Exception):
    """Raised when AI service fails"""
    pass

class AITimeout(AIServiceError):
    """Raised when AI call times out"""
    pass

def with_timeout(timeout_seconds: float = 7200.0):  # 2 hours default for local testing
    """
    Decorator to add timeout to functions
    
    Args:
        timeout_seconds: Maximum time allowed for function execution
    """
    def decorator(func: Callable) -> Callable:
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
                future = executor.submit(func, *args, **kwargs)
                try:
                    return future.result(timeout=timeout_seconds)
                except concurrent.futures.TimeoutError:
                    logger.error(f"{func.__name__} timed out after {timeout_seconds}s")
                    raise AITimeout(f"AI call timed out after {timeout_seconds} seconds")
                except Exception as e:
                    logger.error(f"{func.__name__} failed: {e}")
                    raise AIServiceError(f"AI service error: {e}") from e
        return wrapper
    return decorator

class CircuitBreaker:
    """
    Circuit breaker pattern for AI calls
    
    Prevents repeated calls to a failing service
    """
    def __init__(self, failure_threshold: int = 3, recovery_timeout: float = 60.0):
        self.failure_threshold = failure_threshold
        self.recovery_timeout = recovery_timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.is_open = False
        
    def call(self, func: Callable, *args, **kwargs):
        """Execute function with circuit breaker protection"""
        # Check if circuit is open
        if self.is_open:
            if time.time() - self.last_failure_time < self.recovery_timeout:
                raise AIServiceError("Circuit breaker is open - AI service unavailable")
            else:
                # Try to reset
                self.is_open = False
                self.failure_count = 0
                logger.info("Circuit breaker reset - retrying AI service")
        
        try:
            result = func(*args, **kwargs)
            # Success - reset failure count
            self.failure_count = 0
            return result
        except Exception as e:
            self.failure_count += 1
            self.last_failure_time = time.time()
            
            if self.failure_count >= self.failure_threshold:
                self.is_open = True
                logger.error(f"Circuit breaker opened after {self.failure_count} failures")
            
            raise

# Global circuit breaker instance
_circuit_breaker = CircuitBreaker()

@with_timeout(7200.0)  # 2 hours for deep analysis on local hardware
def safe_ai_generate(prompt: str, model: str, temperature: float = 0.3) -> Optional[Dict[str, Any]]:
    """
    Generate AI response with timeout and circuit breaker
    
    Args:
        prompt: The prompt to send
        model: Model name
        temperature: Generation temperature
        
    Returns:
        AI response or None
    """
    def _generate():
        response = safe_ollama_generate(
            model=model,
            prompt=prompt,
            temperature=temperature
        )
        
        if response and response.get("error"):
            error_detail = str(response.get("error", "Unknown error"))
            # Use logger instead of print
            logger.error(f"AI generation error: {error_detail}")
            raise AIServiceError(f"AI generation error: {error_detail}")
            
        return response
    
    try:
        return _circuit_breaker.call(_generate)
    except AIServiceError as e:
        logger.error(f"AI service error: {e}")
        return None
    except Exception as e:
        logger.error(f"Unexpected error in AI generation: {e}")
        return None

def extract_json_from_response(response: str) -> Optional[Dict[str, Any]]:
    """
    Extract JSON from AI response text
    
    Args:
        response: Raw AI response text
        
    Returns:
        Parsed JSON or None
    """
    import re
    import json
    
    if not response:
        return None
    
    # Try to find JSON block
    json_match = re.search(r'\{.*\}', response, re.DOTALL)
    if not json_match:
        logger.warning("No JSON found in AI response")
        return None
    
    try:
        return json.loads(json_match.group(0))
    except json.JSONDecodeError as e:
        logger.error(f"Failed to parse JSON from AI response: {e}")
        # Log the problematic JSON for debugging
        logger.debug(f"Problematic JSON: {json_match.group(0)[:200]}...")
        return None

def batch_ai_analysis(items: list, prompt_template: str, model: str, 
                     max_concurrent: int = 3, timeout_per_item: float = 10.0) -> list:
    """
    Perform AI analysis on multiple items concurrently
    
    Args:
        items: List of items to analyze
        prompt_template: Template with {item} placeholder
        model: Model to use
        max_concurrent: Maximum concurrent requests
        timeout_per_item: Timeout per item
        
    Returns:
        List of results (None for failed items)
    """
    results = []
    
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_concurrent) as executor:
        # Create futures
        futures = []
        for item in items:
            prompt = prompt_template.format(item=item)
            future = executor.submit(
                safe_ai_generate,
                prompt=prompt,
                model=model,
                temperature=0.3
            )
            futures.append(future)
        
        # Collect results
        for future in concurrent.futures.as_completed(futures, timeout=timeout_per_item * len(items)):
            try:
                result = future.result()
                results.append(result)
            except Exception as e:
                logger.error(f"Batch AI analysis failed for item: {e}")
                results.append(None)
    
    return results