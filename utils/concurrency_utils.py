# utils/concurrency_utils.py
"""
Concurrency analysis utilities for Ruby code
"""
import re
from typing import Dict, List, Tuple

class ConcurrencyAnalyzer:
    """Ruby concurrency and thread safety analysis"""
    
    @staticmethod
    def detect_shared_state(code: str) -> List[Dict[str, str]]:
        """Identify potential thread-safety issues"""
        issues = []
        
        # Class variables
        class_vars = re.findall(r'(@@\w+)', code)
        for var in class_vars:
            issues.append({
                "type": "class_variable",
                "name": var,
                "risk": "high",
                "description": "Class variables are shared across all threads"
            })
        
        # Global variables
        global_vars = re.findall(r'(\$\w+)', code)
        for var in global_vars:
            issues.append({
                "type": "global_variable", 
                "name": var,
                "risk": "high",
                "description": "Global variables are shared across entire application"
            })
        
        # Instance variable mutations
        mutations = re.findall(r'(@\w+)\s*(?:\+=|-=|\*=|\/=|<<=|>>=)', code)
        for var in mutations:
            issues.append({
                "type": "mutable_instance_var",
                "name": var,
                "risk": "medium",
                "description": "Instance variable mutation may need synchronization"
            })
        
        return issues
    
    @staticmethod
    def suggest_concurrency_model(code: str) -> str:
        """Suggest appropriate concurrency model based on code analysis"""
        # Check for I/O operations
        io_operations = bool(re.search(r'File\.|Net::|HTTP\.|IO\.|gets|puts|print', code))
        
        # Check for CPU-intensive operations
        cpu_intensive = bool(re.search(r'\.times\s*\{|\.\each\s*\{.*\*|Matrix|calculate|compute', code))
        
        # Check for shared state
        shared_state = ConcurrencyAnalyzer.detect_shared_state(code)
        
        if io_operations and not cpu_intensive:
            return "Fibers or Async (I/O-bound tasks)"
        elif cpu_intensive and not shared_state:
            return "Ractors (CPU-bound with no shared state)"
        elif cpu_intensive and shared_state:
            return "Threads with proper synchronization"
        else:
            return "Sequential execution may be sufficient"
    
    @staticmethod
    def ractor_compatibility_check(code: str) -> Tuple[bool, List[str]]:
        """Check if code is Ractor-compatible"""
        issues = []
        
        # Check for non-shareable objects
        if re.search(r'StringIO|Tempfile|Socket', code):
            issues.append("Contains non-shareable I/O objects")
        
        # Check for class/module definitions
        if re.search(r'class\s+\w+|module\s+\w+', code):
            issues.append("Dynamic class/module definitions not allowed in Ractors")
        
        # Check for global state access
        if re.search(r'\$\w+|@@\w+', code):
            issues.append("Global/class variables not accessible in Ractors")
        
        # Check for require/load
        if re.search(r'require|load|autoload', code):
            issues.append("Dynamic loading not allowed in Ractors")
        
        is_compatible = len(issues) == 0
        return is_compatible, issues
    
    @staticmethod
    def generate_thread_pool_size(task_type: str, cpu_count: int = 4) -> int:
        """Recommend thread pool size based on task type"""
        if "io" in task_type.lower():
            return cpu_count * 2  # I/O bound can benefit from more threads
        elif "cpu" in task_type.lower():
            return cpu_count  # CPU bound should match CPU count
        else:
            return cpu_count + 1  # General purpose
    
    @staticmethod
    def identify_synchronization_needs(code: str) -> List[Dict[str, str]]:
        """Identify where synchronization primitives are needed"""
        needs = []
        
        # Shared collection modifications
        if re.search(r'(@\w+)\s*<<|(@\w+)\.push|(@\w+)\.delete', code):
            needs.append({
                "location": "Collection modifications",
                "primitive": "Mutex",
                "reason": "Thread-safe collection access"
            })
        
        # Counter increments
        if re.search(r'@\w+\s*\+=\s*1|@counter|@count', code):
            needs.append({
                "location": "Counter operations",
                "primitive": "Concurrent::AtomicFixnum",
                "reason": "Atomic counter operations"
            })
        
        # Resource pooling
        if re.search(r'connection|pool|cache', code, re.IGNORECASE):
            needs.append({
                "location": "Resource management",
                "primitive": "ConnectionPool or Queue",
                "reason": "Thread-safe resource sharing"
            })
        
        return needs
