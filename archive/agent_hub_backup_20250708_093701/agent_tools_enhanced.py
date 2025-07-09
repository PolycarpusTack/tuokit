"""
Enhanced Tool Implementations for TuoKit Agent Hub
Add these to your agent_hub.py to replace generic implementations
"""

import ast
import re
import json
from typing import Dict, Any, List

def enhanced_code_analyzer(code: str, language: str = "python") -> Dict[str, Any]:
    """
    Advanced code analysis with AST parsing and pattern detection
    """
    analysis = {
        "complexity": {},
        "patterns": [],
        "issues": [],
        "metrics": {}
    }
    
    if language.lower() == "python":
        try:
            tree = ast.parse(code)
            
            # Complexity analysis
            class ComplexityAnalyzer(ast.NodeVisitor):
                def __init__(self):
                    self.loops = 0
                    self.conditionals = 0
                    self.functions = 0
                    self.classes = 0
                    self.max_depth = 0
                    self.current_depth = 0
                
                def visit_For(self, node):
                    self.loops += 1
                    self.current_depth += 1
                    self.max_depth = max(self.max_depth, self.current_depth)
                    self.generic_visit(node)
                    self.current_depth -= 1
                
                def visit_While(self, node):
                    self.loops += 1
                    self.current_depth += 1
                    self.max_depth = max(self.max_depth, self.current_depth)
                    self.generic_visit(node)
                    self.current_depth -= 1
                
                def visit_If(self, node):
                    self.conditionals += 1
                    self.generic_visit(node)
                
                def visit_FunctionDef(self, node):
                    self.functions += 1
                    self.generic_visit(node)
                
                def visit_ClassDef(self, node):
                    self.classes += 1
                    self.generic_visit(node)
            
            analyzer = ComplexityAnalyzer()
            analyzer.visit(tree)
            
            # Calculate cyclomatic complexity
            cyclomatic = 1 + analyzer.conditionals + analyzer.loops
            
            analysis["complexity"] = {
                "cyclomatic": cyclomatic,
                "loops": analyzer.loops,
                "conditionals": analyzer.conditionals,
                "max_nesting": analyzer.max_depth,
                "functions": analyzer.functions,
                "classes": analyzer.classes
            }
            
            # Pattern detection
            patterns = []
            
            # Check for common patterns
            if "import pandas" in code or "import numpy" in code:
                patterns.append("Data Science Libraries")
            
            if re.search(r'async\s+def', code):
                patterns.append("Async/Await Pattern")
            
            if re.search(r'@\w+', code):
                patterns.append("Decorators Used")
            
            if re.search(r'class.*\(.*\):', code):
                patterns.append("Object-Oriented Design")
            
            if re.search(r'yield\s+', code):
                patterns.append("Generator Pattern")
            
            analysis["patterns"] = patterns
            
            # Code smell detection
            issues = []
            
            # Long functions
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    func_lines = node.end_lineno - node.lineno if hasattr(node, 'end_lineno') else 0
                    if func_lines > 50:
                        issues.append(f"Long function '{node.name}' ({func_lines} lines)")
            
            # Deep nesting
            if analyzer.max_depth > 3:
                issues.append(f"Deep nesting detected (depth: {analyzer.max_depth})")
            
            # Too many parameters
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    if len(node.args.args) > 5:
                        issues.append(f"Function '{node.name}' has too many parameters ({len(node.args.args)})")
            
            analysis["issues"] = issues
            
            # Code metrics
            lines = code.split('\n')
            analysis["metrics"] = {
                "total_lines": len(lines),
                "code_lines": len([l for l in lines if l.strip() and not l.strip().startswith('#')]),
                "comment_lines": len([l for l in lines if l.strip().startswith('#')]),
                "blank_lines": len([l for l in lines if not l.strip()])
            }
            
        except SyntaxError as e:
            analysis["issues"].append(f"Syntax Error: {str(e)}")
        except Exception as e:
            analysis["issues"].append(f"Analysis Error: {str(e)}")
    
    return analysis

def enhanced_sql_analyzer(query: str, dialect: str = "postgresql") -> Dict[str, Any]:
    """
    Advanced SQL query analysis
    """
    analysis = {
        "type": "unknown",
        "tables": [],
        "joins": [],
        "conditions": [],
        "performance_hints": [],
        "security_warnings": []
    }
    
    query_upper = query.upper()
    
    # Determine query type
    if query_upper.strip().startswith("SELECT"):
        analysis["type"] = "SELECT"
    elif query_upper.strip().startswith("INSERT"):
        analysis["type"] = "INSERT"
    elif query_upper.strip().startswith("UPDATE"):
        analysis["type"] = "UPDATE"
    elif query_upper.strip().startswith("DELETE"):
        analysis["type"] = "DELETE"
    elif query_upper.strip().startswith("CREATE"):
        analysis["type"] = "DDL"
    
    # Extract tables (simplified)
    table_pattern = r'FROM\s+(\w+)|JOIN\s+(\w+)|UPDATE\s+(\w+)|INSERT\s+INTO\s+(\w+)'
    tables = re.findall(table_pattern, query, re.IGNORECASE)
    analysis["tables"] = list(set([t for group in tables for t in group if t]))
    
    # Find JOINs
    join_pattern = r'(INNER|LEFT|RIGHT|FULL|CROSS)\s+JOIN'
    joins = re.findall(join_pattern, query, re.IGNORECASE)
    analysis["joins"] = joins
    
    # Performance hints
    if "SELECT *" in query_upper:
        analysis["performance_hints"].append("Avoid SELECT * - specify only needed columns")
    
    if not re.search(r'LIMIT\s+\d+', query_upper) and analysis["type"] == "SELECT":
        analysis["performance_hints"].append("Consider adding LIMIT clause for large result sets")
    
    if len(joins) > 3:
        analysis["performance_hints"].append("Multiple JOINs detected - ensure proper indexes exist")
    
    if "NOT IN" in query_upper:
        analysis["performance_hints"].append("NOT IN can be slow - consider using NOT EXISTS")
    
    if "LIKE '%'" in query_upper or "LIKE '%%" in query:
        analysis["performance_hints"].append("Leading wildcard in LIKE prevents index usage")
    
    # Security warnings
    if re.search(r'["\'].*\+.*["\']', query):
        analysis["security_warnings"].append("Potential SQL injection risk - use parameterized queries")
    
    if "EXEC" in query_upper or "EXECUTE" in query_upper:
        analysis["security_warnings"].append("Dynamic SQL execution detected - validate inputs carefully")
    
    return analysis

def enhanced_error_analyzer(error_message: str, context: str = "") -> Dict[str, Any]:
    """
    Intelligent error analysis with solutions
    """
    analysis = {
        "error_type": "Unknown",
        "likely_cause": "",
        "solutions": [],
        "related_docs": [],
        "severity": "medium"
    }
    
    error_lower = error_message.lower()
    
    # Python errors
    if "syntaxerror" in error_lower:
        analysis["error_type"] = "Syntax Error"
        analysis["likely_cause"] = "Invalid Python syntax"
        analysis["solutions"] = [
            "Check for missing colons after if/for/def statements",
            "Verify parentheses and brackets are balanced",
            "Look for invalid indentation",
            "Check for missing quotes in strings"
        ]
        analysis["severity"] = "high"
    
    elif "nameerror" in error_lower:
        analysis["error_type"] = "Name Error"
        analysis["likely_cause"] = "Variable or function not defined"
        
        # Extract the undefined name
        match = re.search(r"name '(\w+)' is not defined", error_message)
        if match:
            undefined_name = match.group(1)
            analysis["solutions"] = [
                f"Ensure '{undefined_name}' is defined before use",
                f"Check spelling of '{undefined_name}'",
                f"Import '{undefined_name}' if it's from a module",
                "Check variable scope - might be defined in different scope"
            ]
    
    elif "typeerror" in error_lower:
        analysis["error_type"] = "Type Error"
        analysis["likely_cause"] = "Operation on incompatible types"
        analysis["solutions"] = [
            "Check data types of variables",
            "Ensure function arguments match expected types",
            "Convert types explicitly if needed (int(), str(), etc.)",
            "Verify object has the method/attribute you're accessing"
        ]
    
    elif "indentationerror" in error_lower:
        analysis["error_type"] = "Indentation Error"
        analysis["likely_cause"] = "Inconsistent indentation"
        analysis["solutions"] = [
            "Use consistent indentation (4 spaces recommended)",
            "Don't mix tabs and spaces",
            "Check indentation after control structures",
            "Use an IDE that shows whitespace characters"
        ]
        analysis["severity"] = "high"
    
    elif "importerror" in error_lower or "modulenotfounderror" in error_lower:
        analysis["error_type"] = "Import Error"
        analysis["likely_cause"] = "Module not installed or not found"
        
        # Extract module name
        match = re.search(r"No module named '(\w+)'", error_message)
        if match:
            module_name = match.group(1)
            analysis["solutions"] = [
                f"Install the module: pip install {module_name}",
                "Check if you're in the correct virtual environment",
                "Verify the module name spelling",
                "Ensure the module is in your Python path"
            ]
    
    # SQL errors
    elif "syntax error" in error_lower and ("sql" in context.lower() or "query" in context.lower()):
        analysis["error_type"] = "SQL Syntax Error"
        analysis["likely_cause"] = "Invalid SQL syntax"
        analysis["solutions"] = [
            "Check for missing commas between columns",
            "Verify table and column names exist",
            "Ensure quotes are used correctly for strings",
            "Check SQL dialect-specific syntax"
        ]
    
    # Add context-specific hints
    if context:
        if "pandas" in context.lower():
            analysis["related_docs"].append("https://pandas.pydata.org/docs/")
        if "numpy" in context.lower():
            analysis["related_docs"].append("https://numpy.org/doc/stable/")
        if "sql" in context.lower():
            analysis["related_docs"].append("https://www.postgresql.org/docs/")
    
    return analysis

def generate_unit_tests(code: str, framework: str = "pytest") -> str:
    """
    Generate comprehensive unit tests for given code
    """
    # Parse the code to find functions
    try:
        tree = ast.parse(code)
        functions = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                func_info = {
                    "name": node.name,
                    "args": [arg.arg for arg in node.args.args],
                    "has_return": any(isinstance(n, ast.Return) for n in ast.walk(node))
                }
                functions.append(func_info)
        
        if framework == "pytest":
            test_code = "import pytest\n"
            test_code += "# Import the module containing your functions\n"
            test_code += "# from your_module import "
            test_code += ", ".join([f["name"] for f in functions]) + "\n\n"
            
            for func in functions:
                # Generate test class
                test_code += f"class Test{func['name'].title()}:\n"
                
                # Basic test
                test_code += f"    def test_{func['name']}_basic(self):\n"
                test_code += f"        # Test basic functionality\n"
                
                if func['args']:
                    test_code += f"        # Prepare test arguments\n"
                    for arg in func['args']:
                        test_code += f"        {arg} = None  # TODO: Set appropriate test value\n"
                    
                    args_str = ", ".join(func['args'])
                    test_code += f"        \n        result = {func['name']}({args_str})\n"
                else:
                    test_code += f"        result = {func['name']}()\n"
                
                if func['has_return']:
                    test_code += f"        \n        # Assert expected result\n"
                    test_code += f"        assert result is not None  # TODO: Add specific assertion\n"
                
                test_code += "\n"
                
                # Edge cases
                test_code += f"    def test_{func['name']}_edge_cases(self):\n"
                test_code += f"        # Test edge cases\n"
                
                if func['args']:
                    test_code += f"        # Test with None values\n"
                    none_args = ", ".join(["None" for _ in func['args']])
                    test_code += f"        with pytest.raises(Exception):  # Adjust exception type\n"
                    test_code += f"            {func['name']}({none_args})\n"
                
                test_code += "\n"
                
                # Error cases
                test_code += f"    def test_{func['name']}_error_handling(self):\n"
                test_code += f"        # Test error conditions\n"
                test_code += f"        pass  # TODO: Implement error tests\n"
                test_code += "\n"
            
            # Add fixture
            test_code += "\n@pytest.fixture\n"
            test_code += "def test_data():\n"
            test_code += "    \"\"\"Provide test data for all tests\"\"\"\n"
            test_code += "    return {\n"
            test_code += "        # Add your test data here\n"
            test_code += "    }\n"
            
            return test_code
            
        elif framework == "unittest":
            test_code = "import unittest\n"
            test_code += "# Import the module containing your functions\n\n"
            
            test_code += "class TestFunctions(unittest.TestCase):\n"
            
            for func in functions:
                test_code += f"\n    def test_{func['name']}(self):\n"
                test_code += f"        # Test {func['name']} function\n"
                test_code += f"        pass  # TODO: Implement test\n"
            
            test_code += "\n\nif __name__ == '__main__':\n"
            test_code += "    unittest.main()\n"
            
            return test_code
            
    except Exception as e:
        return f"# Error generating tests: {str(e)}\n# Please check your code syntax"

# Export all enhanced functions
__all__ = [
    'enhanced_code_analyzer',
    'enhanced_sql_analyzer', 
    'enhanced_error_analyzer',
    'generate_unit_tests'
]
