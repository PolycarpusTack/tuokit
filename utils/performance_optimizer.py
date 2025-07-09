"""
Performance Optimizer for TuoKit
Provides memory and performance optimizations for low-resource hardware
"""

import os
import gc
import psutil
from typing import Optional, Dict, Any, Generator, List
import logging

logger = logging.getLogger(__name__)


class PerformanceOptimizer:
    """Central performance optimization manager"""
    
    def __init__(self):
        self.memory_monitor = MemoryMonitor()
        self.model_selector = AdaptiveModelSelector()
        self.chunk_optimizer = ChunkOptimizer()
        
    def get_optimization_profile(self) -> Dict[str, Any]:
        """Determine optimization profile based on system resources"""
        memory_gb = self.memory_monitor.get_available_memory_gb()
        cpu_count = psutil.cpu_count()
        
        if memory_gb < 4:
            profile = "low_memory"
        elif memory_gb < 8:
            profile = "medium_memory"
        else:
            profile = "high_memory"
            
        return {
            "profile": profile,
            "memory_gb": memory_gb,
            "cpu_count": cpu_count,
            "recommendations": self._get_recommendations(profile)
        }
    
    def _get_recommendations(self, profile: str) -> Dict[str, Any]:
        """Get specific recommendations for profile"""
        profiles = {
            "low_memory": {
                "chunk_size": 4096,
                "parallel_workers": 1,
                "model": "tinyllama:latest",
                "streaming": True,
                "cache_enabled": False
            },
            "medium_memory": {
                "chunk_size": 8192,
                "parallel_workers": 2,
                "model": "deepseek-r1:1.5b",
                "streaming": True,
                "cache_enabled": True
            },
            "high_memory": {
                "chunk_size": 16384,
                "parallel_workers": 4,
                "model": "deepseek-r1:latest",
                "streaming": False,
                "cache_enabled": True
            }
        }
        return profiles.get(profile, profiles["medium_memory"])


class MemoryMonitor:
    """Monitor and manage memory usage"""
    
    def __init__(self, low_memory_threshold_mb: int = 500):
        self.low_memory_threshold_mb = low_memory_threshold_mb
        self.gc_triggered_count = 0
        
    def check_memory(self) -> bool:
        """Check if memory is low and trigger GC if needed"""
        available_mb = self.get_available_memory_mb()
        
        if available_mb < self.low_memory_threshold_mb:
            logger.warning(f"Low memory detected: {available_mb}MB available")
            self.force_garbage_collection()
            return False
            
        return True
    
    def get_available_memory_mb(self) -> float:
        """Get available memory in MB"""
        return psutil.virtual_memory().available / (1024 * 1024)
    
    def get_available_memory_gb(self) -> float:
        """Get available memory in GB"""
        return psutil.virtual_memory().available / (1024 * 1024 * 1024)
    
    def get_memory_stats(self) -> Dict[str, Any]:
        """Get detailed memory statistics"""
        vm = psutil.virtual_memory()
        return {
            "total_gb": round(vm.total / (1024**3), 2),
            "available_gb": round(vm.available / (1024**3), 2),
            "used_gb": round(vm.used / (1024**3), 2),
            "percent_used": vm.percent,
            "gc_triggered": self.gc_triggered_count
        }
    
    def force_garbage_collection(self):
        """Force garbage collection"""
        gc.collect()
        self.gc_triggered_count += 1
        logger.info("Forced garbage collection")


class AdaptiveModelSelector:
    """Select appropriate model based on system resources"""
    
    # Model requirements (approximate memory in GB)
    MODEL_REQUIREMENTS = {
        "deepseek-r1:latest": 8.0,
        "deepseek-r1:8b": 6.0,
        "deepseek-r1:1.5b": 2.0,
        "mistral:latest": 4.0,
        "mistral:7b-instruct-v0.2-q4_0": 3.0,
        "codellama:7b-instruct-q4_0": 3.5,
        "tinyllama:latest": 1.0,
        "phi:2.7b": 2.0
    }
    
    def select_model(self, preferred_model: str, available_memory_gb: float) -> str:
        """Select best model that fits in available memory"""
        # Leave some memory for the application
        usable_memory = available_memory_gb * 0.7
        
        # Check if preferred model fits
        if preferred_model in self.MODEL_REQUIREMENTS:
            if self.MODEL_REQUIREMENTS[preferred_model] <= usable_memory:
                return preferred_model
        
        # Find best alternative
        suitable_models = [
            (model, req) for model, req in self.MODEL_REQUIREMENTS.items()
            if req <= usable_memory
        ]
        
        if not suitable_models:
            logger.warning("No models fit in available memory, using smallest")
            return "tinyllama:latest"
        
        # Sort by requirement (prefer larger models that fit)
        suitable_models.sort(key=lambda x: x[1], reverse=True)
        selected = suitable_models[0][0]
        
        logger.info(f"Selected model '{selected}' for {available_memory_gb:.1f}GB available memory")
        return selected
    
    def get_model_fallback_chain(self, start_model: str) -> List[str]:
        """Get fallback chain of progressively smaller models"""
        chain = []
        start_req = self.MODEL_REQUIREMENTS.get(start_model, 8.0)
        
        # Add all models smaller than start model
        for model, req in sorted(self.MODEL_REQUIREMENTS.items(), key=lambda x: x[1], reverse=True):
            if req <= start_req:
                chain.append(model)
        
        # Always end with pattern matching as final fallback
        chain.append("no_ai_fallback")
        
        return chain


class ChunkOptimizer:
    """Optimize chunk sizes based on available resources"""
    
    def get_optimal_chunk_size(self, file_size: int, available_memory_mb: float) -> int:
        """Calculate optimal chunk size"""
        # Base chunk sizes
        if available_memory_mb < 1024:  # < 1GB
            base_chunk = 2048  # 2KB
        elif available_memory_mb < 2048:  # < 2GB
            base_chunk = 4096  # 4KB
        elif available_memory_mb < 4096:  # < 4GB
            base_chunk = 8192  # 8KB
        else:
            base_chunk = 16384  # 16KB
        
        # Adjust based on file size
        if file_size < 1024 * 1024:  # < 1MB
            return base_chunk
        elif file_size < 10 * 1024 * 1024:  # < 10MB
            return base_chunk * 2
        else:
            return base_chunk * 4
    
    def calculate_chunk_count(self, file_size: int, chunk_size: int) -> int:
        """Calculate number of chunks for progress tracking"""
        return (file_size + chunk_size - 1) // chunk_size


class StreamingFileProcessor:
    """Process files in streaming fashion to minimize memory usage"""
    
    def __init__(self, chunk_size: int = 4096):
        self.chunk_size = chunk_size
        self.memory_monitor = MemoryMonitor()
        
    def stream_file(self, filepath: str) -> Generator[str, None, None]:
        """Stream file contents in chunks"""
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            chunk_count = 0
            while True:
                chunk = f.read(self.chunk_size)
                if not chunk:
                    break
                    
                yield chunk
                chunk_count += 1
                
                # Check memory every 10 chunks
                if chunk_count % 10 == 0:
                    self.memory_monitor.check_memory()
    
    def process_file_with_callback(self, filepath: str, process_func, progress_callback=None):
        """Process file with progress callback"""
        file_size = os.path.getsize(filepath)
        processed_size = 0
        
        for chunk in self.stream_file(filepath):
            # Process chunk
            result = process_func(chunk)
            
            # Update progress
            processed_size += len(chunk)
            if progress_callback:
                progress = processed_size / file_size
                progress_callback(progress, result)
            
            # Free memory
            del chunk
            
        return True


# Singleton instance
_optimizer = None

def get_performance_optimizer() -> PerformanceOptimizer:
    """Get singleton performance optimizer instance"""
    global _optimizer
    if _optimizer is None:
        _optimizer = PerformanceOptimizer()
    return _optimizer


# Utility functions
def optimize_for_hardware(func):
    """Decorator to automatically optimize function based on hardware"""
    def wrapper(*args, **kwargs):
        optimizer = get_performance_optimizer()
        profile = optimizer.get_optimization_profile()
        
        # Inject optimization settings
        kwargs['_optimization_profile'] = profile
        
        # Monitor memory during execution
        memory_before = optimizer.memory_monitor.get_available_memory_mb()
        
        try:
            result = func(*args, **kwargs)
            return result
        finally:
            memory_after = optimizer.memory_monitor.get_available_memory_mb()
            memory_used = memory_before - memory_after
            
            if memory_used > 100:  # Used more than 100MB
                logger.info(f"{func.__name__} used {memory_used:.1f}MB of memory")
                optimizer.memory_monitor.check_memory()
    
    return wrapper


def get_hardware_profile() -> Dict[str, Any]:
    """Get current hardware profile for display"""
    optimizer = get_performance_optimizer()
    profile = optimizer.get_optimization_profile()
    memory_stats = optimizer.memory_monitor.get_memory_stats()
    
    return {
        "profile": profile,
        "memory": memory_stats,
        "cpu": {
            "count": psutil.cpu_count(),
            "percent": psutil.cpu_percent(interval=1)
        },
        "recommendations": profile["recommendations"]
    }