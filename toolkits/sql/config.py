"""
SQL Toolkit Configuration
"""

# SQL dialects supported
SQL_DIALECTS = ["PostgreSQL", "MySQL", "SQLite", "SQL Server", "Oracle", "MariaDB"]

# Default models for different tasks - dynamically determined
def get_sql_models():
    """Get appropriate models for SQL tasks"""
    from utils.model_manager import ModelManager
    return {
        "generation": ModelManager.get_model_for_task("sql"),
        "optimization": ModelManager.get_model_for_task("code"),
        "explanation": ModelManager.get_model_for_task("explanation"),
    }

# For backward compatibility
DEFAULT_MODELS = get_sql_models()

# Query type categories
QUERY_TYPES = [
    "SELECT - Data Retrieval",
    "INSERT - Add Data",
    "UPDATE - Modify Data",
    "DELETE - Remove Data",
    "CREATE - Schema Definition",
    "ALTER - Schema Modification",
    "DROP - Remove Objects",
]

# Common optimization patterns
OPTIMIZATION_PATTERNS = {
    "select_star": "Avoid SELECT * for better performance",
    "not_in": "Consider NOT EXISTS instead of NOT IN",
    "leading_wildcard": "Leading wildcards prevent index usage",
    "missing_limit": "Consider adding LIMIT to prevent large result sets",
    "correlated_subquery": "Correlated subqueries can be slow",
    "implicit_conversion": "Type mismatches cause implicit conversions",
}

# Sample schemas for examples
SAMPLE_SCHEMAS = {
    "E-commerce": """
    An e-commerce database with:
    - Users table (id, email, name, created_at)
    - Products table (id, name, price, stock)
    - Orders table (id, user_id, total, status, created_at)
    - Order_items table (id, order_id, product_id, quantity, price)
    """,
    "Blog": """
    A blog platform database with:
    - Authors table (id, username, email, bio)
    - Posts table (id, author_id, title, content, published_at)
    - Comments table (id, post_id, user_name, comment, created_at)
    - Tags table (id, name)
    - Post_tags table (post_id, tag_id)
    """,
    "HR System": """
    An HR management database with:
    - Employees table (id, first_name, last_name, email, hire_date, department_id)
    - Departments table (id, name, manager_id)
    - Salaries table (id, employee_id, amount, effective_date)
    - Positions table (id, title, department_id, level)
    """,
}
