"""
Unified SQL Tools - Single source of truth for SQL operations
Consolidates functionality from sql_generator, sql_optimizer, and sql_pipeline
Following TuoKit principle: One tool, one purpose, no duplication
"""

from utils import safe_ollama_generate, capture_knowledge
import re

class SQLTools:
    """Unified SQL operations - generate, optimize, explain"""
    
    # Dangerous SQL patterns for validation
    DANGEROUS_PATTERNS = [
        r'\b(DROP|TRUNCATE|DELETE)\s+(TABLE|DATABASE)\b',
        r'\bEXEC(UTE)?\s*\(',
        r'\bxp_cmdshell\b',
        '--.*',  # SQL comments that might hide malicious code
        r'/\*.*\*/',  # Block comments
    ]
    
    @staticmethod
    def generate(query, dialect="postgresql", schema_info=None):
        """Convert natural language to SQL - single implementation"""
        schema_context = ""
        if schema_info:
            schema_context = f"\nSchema: {schema_info}"
        
        prompt = f"""Convert this request to {dialect} SQL.
Request: {query}{schema_context}

Return ONLY the SQL query, no explanations."""
        
        try:
            sql = safe_ollama_generate(
                model="deepseek-coder:6.7b",
                prompt=prompt,
                temperature=0.1  # Low temperature for consistency
            )
            
            # Basic safety check
            if SQLTools._is_dangerous(sql):
                return "-- BLOCKED: Query contains potentially dangerous operations"
            
            # Capture to knowledge base
            capture_knowledge(
                tool_name="sql_tools",
                prompt=query,
                response=sql,
                metadata={"operation": "generate", "dialect": dialect}
            )
            
            return sql.strip()
            
        except Exception as e:
            return f"-- Error: {str(e)}"
    
    @staticmethod
    def optimize(sql, schema_info=None):
        """Optimize SQL query - unified implementation"""
        schema_context = ""
        if schema_info:
            schema_context = f"\nAvailable indexes: {schema_info}"
        
        prompt = f"""Optimize this SQL query for better performance.
{sql}{schema_context}

Provide:
1. Optimized query
2. Brief explanation of changes
3. Performance impact estimate"""
        
        try:
            result = safe_ollama_generate(
                model="deepseek-coder:6.7b",
                prompt=prompt,
                temperature=0.2
            )
            
            return result.strip()
            
        except Exception as e:
            return f"Optimization error: {str(e)}"
    
    @staticmethod
    def explain(sql):
        """Explain SQL in plain English - simple and clear"""
        prompt = f"""Explain this SQL query in simple terms:
{sql}

Explain:
1. What data it retrieves
2. Any filters or conditions
3. Expected result in plain English"""
        
        try:
            explanation = safe_ollama_generate(
                model="deepseek-r1:latest",
                prompt=prompt,
                temperature=0.3
            )
            
            return explanation.strip()
            
        except Exception as e:
            return f"Explanation error: {str(e)}"
    
    @staticmethod
    def validate(sql):
        """Validate SQL for safety and syntax - returns (is_valid, message)"""
        if not sql or not sql.strip():
            return False, "Empty query"
        
        # Check dangerous patterns
        if SQLTools._is_dangerous(sql):
            return False, "Query contains potentially dangerous operations"
        
        # Basic syntax checks
        sql_upper = sql.upper().strip()
        if sql_upper.startswith(('SELECT', 'WITH', 'INSERT', 'UPDATE', 'DELETE')):
            return True, "Query appears valid"
        
        return False, "Query must start with SELECT, WITH, INSERT, UPDATE, or DELETE"
    
    @staticmethod
    def _is_dangerous(sql):
        """Check if SQL contains dangerous patterns"""
        for pattern in SQLTools.DANGEROUS_PATTERNS:
            if re.search(pattern, sql, re.IGNORECASE):
                return True
        return False
    
    @staticmethod
    def format(sql):
        """Basic SQL formatting for readability"""
        # Simple formatting - add newlines before major keywords
        keywords = ['SELECT', 'FROM', 'WHERE', 'JOIN', 'LEFT JOIN', 
                   'RIGHT JOIN', 'INNER JOIN', 'GROUP BY', 'ORDER BY', 
                   'HAVING', 'LIMIT']
        
        formatted = sql
        for keyword in keywords:
            formatted = re.sub(f'\\b{keyword}\\b', f'\n{keyword}', 
                             formatted, flags=re.IGNORECASE)
        
        # Clean up extra newlines
        formatted = re.sub(r'\n\s*\n', '\n', formatted)
        return formatted.strip()

# Convenience functions for backward compatibility
def generate_sql(query, dialect="postgresql", schema_info=None):
    """Legacy wrapper - use SQLTools.generate() instead"""
    return SQLTools.generate(query, dialect, schema_info)

def optimize_sql(sql, schema_info=None):
    """Legacy wrapper - use SQLTools.optimize() instead"""
    return SQLTools.optimize(sql, schema_info)

def explain_sql(sql):
    """Legacy wrapper - use SQLTools.explain() instead"""
    return SQLTools.explain(sql)

# TODO: Remove legacy wrappers after updating all pages