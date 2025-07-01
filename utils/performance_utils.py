# utils/performance_utils.py
"""
Performance analysis utilities for Ruby code
"""
import re
from typing import Dict, List

class RubyPerformance:
    """Ruby-specific performance analysis utilities"""
    
    @staticmethod
    def detect_n_plus_1(code: str) -> bool:
        """Detect common N+1 query patterns"""
        patterns = [
            r"\.each\s*{\s*[^}]*\.[a-z_]+\.where",
            r"\.map\s*{\s*[^}]*\.[a-z_]+\s*}",
            r"\.select\s*{\s*[^}]*\.[a-z_]+\.exists\?"
        ]
        return any(re.search(p, code) for p in patterns)
    
    @staticmethod
    def detect_performance_issues(code: str) -> List[Dict[str, str]]:
        """Detect various performance issues in Ruby code"""
        issues = []
        
        # N+1 queries
        if RubyPerformance.detect_n_plus_1(code):
            issues.append({
                "type": "N+1 Query",
                "severity": "high",
                "description": "Potential N+1 database query detected",
                "solution": "Use includes() or eager loading"
            })
        
        # Array concatenation in loops
        if re.search(r"\.each.*\+\=\s*\[", code):
            issues.append({
                "type": "Array Concatenation",
                "severity": "medium",
                "description": "Array concatenation in loop",
                "solution": "Use << or concat() instead of +="
            })
        
        # Missing indexes
        if re.search(r"\.where\s*\(\s*['\"](?!id)", code):
            issues.append({
                "type": "Missing Index",
                "severity": "medium",
                "description": "Possible unindexed query",
                "solution": "Consider adding database indexes"
            })
        
        # Large data loading
        if re.search(r"\.all\s*\.", code) and not re.search(r"\.limit\s*\(", code):
            issues.append({
                "type": "Unbounded Query",
                "severity": "high",
                "description": "Loading all records without limit",
                "solution": "Add pagination or batch processing"
            })
        
        return issues
    
    @staticmethod
    def complexity_metrics(code: str) -> Dict[str, int]:
        """Calculate complexity metrics for Ruby code"""
        return {
            "lines": len(code.splitlines()),
            "methods": len(re.findall(r"\bdef\s+\w+", code)),
            "classes": len(re.findall(r"\bclass\s+\w+", code)),
            "conditionals": len(re.findall(r"\b(if|unless|case)\b", code)),
            "loops": len(re.findall(r"\b(each|while|for|loop|times)\b", code)),
            "blocks": len(re.findall(r"\bdo\b|\{", code))
        }
