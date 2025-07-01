# utils/pattern_utils.py
"""
Pattern matching utilities for Ruby code analysis
"""
import re
from typing import Dict, List

class PatternMatcher:
    """Ruby pattern matching analysis utilities"""
    
    @staticmethod
    def complexity_score(code: str) -> int:
        """Estimate pattern matching complexity"""
        patterns = [
            r"in\s*\[.*\]",  # Array pattern
            r"in\s*\{.*\}",  # Hash pattern
            r"in\s*\w+\s*if",  # Guard clause
            r"in\s*[^|]+\|[^|]+",  # Alternative pattern
            r"=>\s*\w+",  # As pattern
            r"in\s*\*",  # Splat pattern
        ]
        return sum(1 for p in patterns if re.search(p, code))
    
    @staticmethod
    def extract_patterns(code: str) -> List[Dict[str, str]]:
        """Extract all pattern matching constructs"""
        patterns = []
        
        # Find case expressions with pattern matching
        case_matches = re.finditer(r'case\s+(\w+)(.+?)end', code, re.DOTALL)
        for match in case_matches:
            var_name = match.group(1)
            case_body = match.group(2)
            
            # Extract individual patterns
            in_patterns = re.findall(r'in\s+(.+?)(?=\n|then)', case_body)
            for pattern in in_patterns:
                patterns.append({
                    "variable": var_name,
                    "pattern": pattern.strip(),
                    "type": PatternMatcher._classify_pattern(pattern)
                })
        
        return patterns
    
    @staticmethod
    def _classify_pattern(pattern: str) -> str:
        """Classify the type of pattern"""
        if pattern.startswith('['):
            return "array"
        elif pattern.startswith('{'):
            return "hash"
        elif '|' in pattern:
            return "alternative"
        elif 'if' in pattern:
            return "guard"
        elif '=>' in pattern:
            return "as_pattern"
        else:
            return "value"
    
    @staticmethod
    def suggest_improvements(code: str) -> List[str]:
        """Suggest pattern matching improvements"""
        suggestions = []
        
        # Check for if/elsif chains that could be pattern matching
        if re.search(r'if\s+\w+\.is_a\?\(', code) and 'elsif' in code:
            suggestions.append("Consider using pattern matching instead of if/elsif type checks")
        
        # Check for nested conditionals
        if code.count('if') > 3:
            suggestions.append("Complex conditional logic could be simplified with pattern matching")
        
        # Check for manual hash/array destructuring
        if re.search(r'\[\s*0\s*\]|\[\s*1\s*\]', code):
            suggestions.append("Array indexing could be replaced with pattern matching destructuring")
        
        return suggestions
