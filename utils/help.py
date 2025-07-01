"""
Help and documentation utilities for TuoKit
Provides contextual help and usage guidance
"""

from typing import Dict, Optional
from .database import DatabaseManager

# Built-in help documentation
HELP_DATABASE = {
    "code_explainer": {
        "default": "Explain code functionality in bullet points. Add comments like '# Focus on security' for targeted analysis.",
        "security": "Analyzing for security vulnerabilities. Check for SQL injection, XSS, authentication issues.",
        "performance": "Analyzing for performance bottlenecks. Look for O(n²) algorithms, unnecessary loops, memory leaks.",
        "best_practices": "Checking against language best practices and common patterns."
    },
    "code_debugger": {
        "default": "Debug errors by providing the full error message and problematic code.",
        "performance": "Include profiling data or slow operation details for performance debugging.",
        "memory": "For memory issues, include heap dumps or memory usage patterns.",
        "async": "For async/concurrent issues, include thread states and race conditions."
    },
    "code_generator": {
        "default": "Generate code by describing what you need. Be specific about requirements.",
        "api": "For API clients, specify authentication method, endpoints, and error handling needs.",
        "cli": "For CLI tools, specify commands, arguments, and output format.",
        "data": "For data processing, specify input/output formats and transformation rules."
    },
    "sql_generator": {
        "default": "Generate SQL queries from natural language. Specify target database (PostgreSQL, MySQL, etc.)",
        "complex": "For complex queries, break down requirements: tables, joins, conditions, grouping.",
        "optimization": "Include current query performance metrics for optimization suggestions.",
        "migration": "For schema migrations, provide current and desired table structures."
    },
    "doc_summary": {
        "default": "Generate 3-5 key point summaries. Use '// Technical summary' for jargon-heavy docs.",
        "technical": "Creating technical summary with preservation of key terminology and specifications.",
        "executive": "High-level summary for non-technical audience, focusing on impact and outcomes.",
        "academic": "Summarize research papers with methodology, findings, and limitations."
    },
    "doc_qa": {
        "default": "Ask specific questions about the document content.",
        "research": "For research papers, ask about methodology, findings, limitations.",
        "technical": "For technical docs, ask about implementation details, requirements, constraints.",
        "comparison": "Compare multiple documents or sections within a document."
    },
    "error_decoder": {
        "default": "Decode error messages with code context. Include full stack trace when available.",
        "python": "Python errors: Include traceback, local variables, and import statements.",
        "javascript": "JS errors: Include console output, browser info, and async stack traces.",
        "database": "DB errors: Include query, schema, and connection details."
    },
    "regex_generator": {
        "default": "Generate regex patterns from descriptions. Specify language/flavor (Python, JS, etc.)",
        "validation": "For validation patterns, provide valid and invalid examples.",
        "extraction": "For data extraction, show sample text and desired captures.",
        "replacement": "For replacements, show before/after examples."
    }
}

def get_contextual_help(tool: str, context: str = "") -> str:
    """Fetch help documentation based on current context"""
    
    # Try to fetch from knowledge base first
    db = None
    try:
        db = DatabaseManager()
        if db.connected:
            with db.conn.cursor() as cur:
                cur.execute("""
                    SELECT content FROM knowledge_units 
                    WHERE category = 'Documentation' 
                    AND title ILIKE %s
                    ORDER BY created_at DESC
                    LIMIT 1
                """, (f"%{tool}%help%",))
                if result := cur.fetchone():
                    return result[0]
    except:
        pass
    
    # Fallback to built-in help
    tool_help = HELP_DATABASE.get(tool, {})
    if context and context in tool_help:
        return tool_help[context]
    return tool_help.get("default", f"Use this {tool} to process your content with AI assistance.")

def get_tool_examples(tool: str) -> Dict[str, str]:
    """Get example usage for a tool"""
    examples = {
        "code_explainer": {
            "basic": "def factorial(n):\n    return 1 if n <= 1 else n * factorial(n-1)",
            "complex": "# Complex async code with decorators and type hints"
        },
        "sql_generator": {
            "basic": "Find all customers who made purchases last month",
            "complex": "Calculate customer lifetime value with cohort analysis"
        },
        "regex_generator": {
            "email": "Match valid email addresses",
            "phone": "Match US phone numbers in any format"
        }
    }
    
    return examples.get(tool, {})

def get_all_tools_summary() -> Dict[str, str]:
    """Get a summary of all available tools"""
    return {
        "code_explainer": "Explain code functionality, identify patterns and potential issues",
        "code_debugger": "Debug errors with detailed analysis and fixes",
        "code_generator": "Generate code from natural language descriptions",
        "sql_generator": "Create SQL queries from plain English",
        "sql_optimizer": "Optimize SQL queries for better performance", 
        "sql_pipeline": "Build ETL pipelines and data workflows",
        "doc_summary": "Summarize documents into key points",
        "doc_qa": "Answer questions about document content",
        "error_decoder": "Decode and explain error messages",
        "regex_generator": "Create regex patterns from descriptions",
        "agent_portal": "Orchestrate multiple tools for complex tasks"
    }
