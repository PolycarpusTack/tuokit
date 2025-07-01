# utils/memory_utils.py
"""
Memory optimization utilities for Ruby code
"""
import re
from typing import Dict, List, Tuple

class MemoryPatterns:
    """Ruby memory optimization patterns and antipatterns"""
    
    ANTIPATTERNS = {
        "String Duplication": {
            "pattern": r"(\+=)\s*['\"]",
            "solution": "Use << instead of += for string concatenation",
            "severity": 3,
            "example": "str += 'text' → str << 'text'"
        },
        "Unbounded Growth": {
            "pattern": r"(@@|\$)\w+\s*<<",
            "solution": "Use bounded data structures or pagination",
            "severity": 4,
            "example": "Limit array size or use lazy enumerators"
        },
        "N+1 Caching": {
            "pattern": r"Rails\.cache\.fetch.*\.each",
            "solution": "Cache entire collections instead of per-element",
            "severity": 2,
            "example": "Cache the full result set, not individual items"
        },
        "Leaky Constants": {
            "pattern": r"^[A-Z][A-Z0-9_]*\s*=\s*\[",
            "solution": "Use class methods instead of top-level constants",
            "severity": 3,
            "example": "CACHE = [] → def self.cache; @cache ||= []; end"
        },
        "Frozen String Missing": {
            "pattern": r"^(?!.*frozen_string_literal).*\.rb",
            "solution": "Add # frozen_string_literal: true",
            "severity": 1,
            "example": "Reduces string allocation overhead"
        }
    }
    
    @staticmethod
    def detect_issues(code: str) -> List[Dict[str, any]]:
        """Detect memory-related issues in code"""
        issues = []
        
        for name, details in MemoryPatterns.ANTIPATTERNS.items():
            if re.search(details["pattern"], code, re.MULTILINE):
                issues.append({
                    "type": name,
                    "severity": details["severity"],
                    "solution": details["solution"],
                    "example": details["example"]
                })
        
        return sorted(issues, key=lambda x: x["severity"], reverse=True)
    
    @staticmethod
    def estimate_memory_impact(code: str) -> Dict[str, int]:
        """Estimate memory usage patterns"""
        impact = {
            "string_allocations": len(re.findall(r"['\"].*['\"]", code)),
            "array_allocations": len(re.findall(r"\[\s*\]|\w+\.to_a", code)),
            "hash_allocations": len(re.findall(r"\{\s*\}|\w+\.to_h", code)),
            "object_allocations": len(re.findall(r"\.new\b", code)),
            "potential_leaks": len(re.findall(r"@@\w+|CONSTANT\s*=", code))
        }
        
        # Calculate severity score
        impact["severity_score"] = (
            impact["string_allocations"] * 1 +
            impact["array_allocations"] * 2 +
            impact["hash_allocations"] * 2 +
            impact["object_allocations"] * 3 +
            impact["potential_leaks"] * 5
        )
        
        return impact
    
    @staticmethod
    def suggest_optimizations(code: str) -> List[str]:
        """Suggest specific memory optimizations"""
        suggestions = []
        
        # String optimizations
        if "+" in code and "'" in code:
            suggestions.append("Consider using string interpolation instead of concatenation")
        
        # Collection optimizations
        if ".map" in code and ".select" in code:
            suggestions.append("Chain .select before .map to reduce intermediate arrays")
        
        # Lazy evaluation
        if re.search(r"\.select.*\.map", code) and ".lazy" not in code:
            suggestions.append("Use .lazy for large collection transformations")
        
        # Object pooling
        if code.count(".new") > 5:
            suggestions.append("Consider object pooling for frequently created objects")
        
        # Frozen strings
        if "freeze" not in code and code.count('"') > 10:
            suggestions.append("Freeze string literals to reduce allocations")
        
        return suggestions


class MemoryProfiler:
    """Memory profiling utilities"""
    
    @staticmethod
    def generate_profiling_code(original_code: str) -> str:
        """Wrap code with memory profiling"""
        return f"""require 'memory_profiler'

report = MemoryProfiler.report do
{original_code}
end

puts "Total allocated: #{{report.total_allocated_memsize / 1024}} KB"
puts "Total retained: #{{report.total_retained_memsize / 1024}} KB"

report.pretty_print(to_file: 'memory_report.txt')
"""
    
    @staticmethod
    def gc_tuning_suggestions(app_type: str) -> Dict[str, str]:
        """GC tuning suggestions based on app type"""
        tuning = {
            "web_app": {
                "RUBY_GC_HEAP_GROWTH_FACTOR": "1.1",
                "RUBY_GC_MALLOC_LIMIT": "90000000",
                "RUBY_GC_OLDMALLOC_LIMIT": "90000000",
                "description": "Balanced for request/response cycle"
            },
            "background_job": {
                "RUBY_GC_HEAP_GROWTH_FACTOR": "1.5",
                "RUBY_GC_MALLOC_LIMIT": "128000000",
                "RUBY_GC_HEAP_FREE_SLOTS": "100000",
                "description": "Optimized for long-running processes"
            },
            "data_processing": {
                "RUBY_GC_HEAP_INIT_SLOTS": "1000000",
                "RUBY_GC_HEAP_GROWTH_MAX_SLOTS": "300000",
                "RUBY_GC_MALLOC_LIMIT_MAX": "256000000",
                "description": "Large heap for data-intensive work"
            }
        }
        
        return tuning.get(app_type, tuning["web_app"])
