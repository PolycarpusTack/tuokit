# utils/kata_utils.py
"""
Ruby kata generation and training utilities
"""
import re
from typing import Dict, List, Tuple
import random

class KataGenerator:
    """Ruby kata generation and difficulty management"""
    
    # Kata templates by topic and difficulty
    KATA_PATTERNS = {
        "Algorithms": {
            "Beginner": [
                "Array manipulation (reverse, rotate, unique)",
                "String processing (palindrome, anagram)",
                "Basic sorting (bubble, selection)",
                "Simple math (fibonacci, factorial)"
            ],
            "Intermediate": [
                "Binary search variations",
                "Dynamic programming basics",
                "Graph traversal (DFS/BFS)",
                "Sliding window problems"
            ],
            "Advanced": [
                "Advanced DP (knapsack, LCS)",
                "Complex graph algorithms",
                "String matching algorithms",
                "Optimization problems"
            ]
        },
        "OOP": {
            "Beginner": [
                "Class design (Person, Animal)",
                "Inheritance basics",
                "Attr_accessor usage",
                "Simple composition"
            ],
            "Intermediate": [
                "Design patterns (Factory, Observer)",
                "SOLID principles application",
                "Module mixins",
                "Interface segregation"
            ],
            "Advanced": [
                "Complex inheritance hierarchies",
                "Metaprogramming with OOP",
                "Abstract classes simulation",
                "Domain modeling"
            ]
        },
        "Metaprogramming": {
            "Beginner": [
                "define_method basics",
                "send and public_send",
                "attr_* implementations",
                "Simple DSL creation"
            ],
            "Intermediate": [
                "method_missing magic",
                "Class macros",
                "Hook methods",
                "Code generation"
            ],
            "Advanced": [
                "Complex DSLs",
                "Aspect-oriented programming",
                "Runtime class modification",
                "Framework building"
            ]
        }
    }
    
    @staticmethod
    def difficulty_factors(level: str) -> Dict[str, any]:
        """Get difficulty parameters"""
        return {
            "Beginner": {
                "complexity": 1,
                "edge_cases": 0,
                "lines_of_code": "5-20",
                "time_minutes": "5-10",
                "concepts": 1
            },
            "Intermediate": {
                "complexity": 3,
                "edge_cases": 2,
                "lines_of_code": "20-50",
                "time_minutes": "15-30",
                "concepts": 2
            },
            "Advanced": {
                "complexity": 5,
                "edge_cases": 4,
                "lines_of_code": "50+",
                "time_minutes": "45+",
                "concepts": 3
            }
        }[level]
    
    @staticmethod
    def generate_test_cases(kata_type: str, difficulty: str) -> List[str]:
        """Generate appropriate test cases"""
        base_tests = [
            "handles basic input",
            "returns expected output",
            "handles edge cases"
        ]
        
        if difficulty == "Intermediate":
            base_tests.extend([
                "handles nil/empty input",
                "performs efficiently",
                "maintains immutability"
            ])
        elif difficulty == "Advanced":
            base_tests.extend([
                "handles concurrent access",
                "scales with large input",
                "follows design patterns",
                "handles all edge cases"
            ])
        
        return base_tests
    
    @staticmethod
    def scoring_rubric(level: str) -> Dict[str, int]:
        """Scoring rubric for kata solutions"""
        rubrics = {
            "Beginner": {
                "correctness": 70,
                "style": 20,
                "efficiency": 10
            },
            "Intermediate": {
                "correctness": 50,
                "style": 25,
                "efficiency": 25
            },
            "Advanced": {
                "correctness": 40,
                "style": 20,
                "efficiency": 30,
                "elegance": 10
            }
        }
        
        return rubrics.get(level, rubrics["Intermediate"])
    
    @staticmethod
    def generate_starter_code(topic: str, difficulty: str) -> str:
        """Generate starter code template"""
        templates = {
            "Algorithms": """class Solution
  def solve(input)
    # TODO: Implement your solution
    
  end
end

# Example usage:
# solution = Solution.new
# result = solution.solve(input_data)""",
            
            "OOP": """class YourClass
  # TODO: Define attributes
  
  def initialize
    # TODO: Initialize state
  end
  
  # TODO: Implement methods
end""",
            
            "Metaprogramming": """module YourModule
  def self.included(base)
    base.extend(ClassMethods)
  end
  
  module ClassMethods
    # TODO: Implement class methods
  end
  
  # TODO: Implement instance methods
end"""
        }
        
        return templates.get(topic, templates["Algorithms"])


class KataAnalyzer:
    """Analyze kata solutions"""
    
    @staticmethod
    def check_ruby_idioms(code: str) -> List[str]:
        """Check for Ruby idioms and best practices"""
        suggestions = []
        
        # Check for non-idiomatic patterns
        patterns = {
            r"for\s+\w+\s+in": "Use .each instead of for..in loops",
            r"if\s+.*\s*==\s*true": "Remove redundant == true comparisons",
            r"return\s+nil": "Implicit nil return is more idiomatic",
            r"Array\.new\(\)": "Use [] instead of Array.new()",
            r"Hash\.new\(\)": "Use {} instead of Hash.new()",
            r"if\s+!\s*\w+": "Use unless instead of if !condition",
            r"\.length\s*==\s*0": "Use .empty? instead of .length == 0"
        }
        
        for pattern, suggestion in patterns.items():
            if re.search(pattern, code):
                suggestions.append(suggestion)
        
        # Check for good practices
        good_practices = {
            r"\.map\s*\{": "Good use of .map",
            r"&:\w+": "Nice use of symbol-to-proc",
            r"\|\|=": "Good memoization pattern",
            r"\.tap\s*\{": "Elegant use of .tap"
        }
        
        practices_found = []
        for pattern, praise in good_practices.items():
            if re.search(pattern, code):
                practices_found.append(praise)
        
        return suggestions, practices_found
    
    @staticmethod
    def estimate_complexity(code: str) -> str:
        """Estimate time complexity"""
        loop_count = len(re.findall(r"\.each|\.map|\.select|for\s+|while\s+", code))
        nested_loops = len(re.findall(r"\.each.*\.each|\.map.*\.map", code))
        
        if nested_loops > 0:
            return "O(n²) or higher - nested iterations detected"
        elif loop_count > 2:
            return "O(n) - multiple iterations"
        elif loop_count > 0:
            return "O(n) - linear time"
        else:
            return "O(1) - constant time"
    
    @staticmethod
    def check_edge_cases(code: str) -> List[str]:
        """Check if code handles edge cases"""
        handled = []
        missing = []
        
        # Check for nil handling
        if "nil?" in code or "if" in code and "nil" in code:
            handled.append("Nil handling")
        else:
            missing.append("Consider nil input handling")
        
        # Check for empty collection handling
        if "empty?" in code or "any?" in code:
            handled.append("Empty collection handling")
        else:
            missing.append("Consider empty array/hash cases")
        
        # Check for boundary conditions
        if "zero?" in code or "<=" in code or ">=" in code:
            handled.append("Boundary conditions")
        
        return handled, missing
