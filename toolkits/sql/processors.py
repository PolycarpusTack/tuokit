"""
SQL Processing Functions
Core logic for SQL generation, optimization, and analysis
"""

import re
from typing import Any, Dict, List, Optional, Union

try:
    from utils import safe_ollama_generate
except ImportError:
    # Fallback for when running standalone tests
    import sys

    sys.path.append("..")
    from utils import safe_ollama_generate


def _clean_sql_response(sql: str) -> str:
    """
    Clean common formatting issues from AI-generated SQL
    
    Args:
        sql: Raw SQL response from AI
        
    Returns:
        Cleaned SQL string
    """
    # Remove code block markers
    sql = sql.replace("```sql", "").replace("```", "").strip()
    
    # Remove leading/trailing whitespace
    sql = sql.strip()
    
    # Ensure it ends with semicolon if not present
    if sql and not sql.rstrip().endswith(";"):
        sql = sql.rstrip() + ";"
    
    return sql


def generate_sql_query(natural_language: str, dialect: str = "postgresql", model: str = "deepseek-coder:6.7b") -> str:
    """
    Generate SQL query from natural language description

    Args:
        natural_language: Description of what the query should do
        dialect: SQL dialect (postgresql, mysql, sqlite, etc.)
        model: Ollama model to use

    Returns:
        Generated SQL query
        
    Raises:
        ValueError: If natural_language is empty or dialect is invalid
    """
    
    # Input validation
    if not natural_language or not natural_language.strip():
        raise ValueError("Natural language description cannot be empty")
    
    if not dialect:
        dialect = "postgresql"

    # Build comprehensive prompt
    prompt = f"""
Generate a {dialect} SQL query for the following request:
{natural_language}

Requirements:
1. Use proper {dialect} syntax
2. Include appropriate JOINs if multiple tables are mentioned
3. Add WHERE clauses for any conditions
4. Use meaningful table and column aliases
5. Include ORDER BY and LIMIT where appropriate
6. Follow best practices for the {dialect} dialect

Return ONLY the SQL query without explanation.
"""

    response = safe_ollama_generate(model, prompt)
    
    # Check for errors in response
    if response.get("error") or not response.get("response"):
        raise RuntimeError(f"Failed to generate SQL: {response.get('error', 'Empty response')}")

    # Extract and clean SQL from response
    sql = _clean_sql_response(response["response"])

    # Validate basic SQL structure
    if not sql.upper().startswith(("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE", "ALTER", "DROP")):
        # Try to extract SQL from explanation
        sql_match = re.search(r"(SELECT|INSERT|UPDATE|DELETE|CREATE|ALTER|DROP).*?;", sql, re.IGNORECASE | re.DOTALL)
        if sql_match:
            sql = sql_match.group(0)
        else:
            # If still no valid SQL, return cleaned response
            sql = _clean_sql_response(sql)

    return sql


def optimize_sql_query(query: str, dialect: str = "postgresql", model: str = "deepseek-coder:6.7b") -> str:
    """
    Optimize an existing SQL query for better performance

    Args:
        query: SQL query to optimize
        dialect: SQL dialect
        model: Ollama model to use

    Returns:
        Optimized SQL query
        
    Raises:
        ValueError: If query is empty
    """
    
    # Input validation
    if not query or not query.strip():
        raise ValueError("Query cannot be empty")

    prompt = f"""
Optimize this {dialect} SQL query for better performance:

```sql
{query}
```

Optimization guidelines:
1. Use specific column names instead of SELECT *
2. Add appropriate indexes hints if supported by {dialect}
3. Optimize JOIN order (smaller tables first)
4. Use EXISTS instead of IN for better performance where applicable
5. Add LIMIT if not present for SELECT queries
6. Use CTEs (WITH clause) for complex queries if supported
7. Avoid correlated subqueries where possible

Return the optimized query with comments explaining key optimizations.
"""

    response = safe_ollama_generate(model, prompt)

    return response["response"]


def explain_sql_query(query: str, dialect: str = "postgresql", model: str = "deepseek-coder:6.7b") -> str:
    """
    Explain what a SQL query does in plain English

    Args:
        query: SQL query to explain
        dialect: SQL dialect
        model: Ollama model to use

    Returns:
        Plain English explanation
    """

    prompt = f"""
Explain this {dialect} SQL query in plain English:

```sql
{query}
```

Provide:
1. Overall purpose of the query
2. Step-by-step breakdown of what it does
3. Any potential issues or improvements
4. Expected output format

Make the explanation clear for someone who understands the business logic but not SQL.
"""

    response = safe_ollama_generate(model, prompt)

    return response["response"]


def validate_sql_syntax(query: str, dialect: str = "postgresql") -> Dict[str, Any]:
    """
    Basic SQL syntax validation

    Args:
        query: SQL query to validate
        dialect: SQL dialect

    Returns:
        Dict with validation results
    """

    validation = {"is_valid": True, "errors": [], "warnings": [], "query_type": None}

    query_upper = query.upper().strip()

    # Determine query type
    if query_upper.startswith("SELECT"):
        validation["query_type"] = "SELECT"
    elif query_upper.startswith("INSERT"):
        validation["query_type"] = "INSERT"
    elif query_upper.startswith("UPDATE"):
        validation["query_type"] = "UPDATE"
    elif query_upper.startswith("DELETE"):
        validation["query_type"] = "DELETE"
    elif query_upper.startswith("CREATE"):
        validation["query_type"] = "CREATE"
    else:
        validation["is_valid"] = False
        validation["errors"].append("Query must start with SELECT, INSERT, UPDATE, DELETE, or CREATE")

    # Basic syntax checks
    if validation["query_type"] == "SELECT":
        if "FROM" not in query_upper:
            validation["errors"].append("SELECT query missing FROM clause")
            validation["is_valid"] = False

    elif validation["query_type"] == "INSERT":
        if "INTO" not in query_upper:
            validation["errors"].append("INSERT query missing INTO keyword")
            validation["is_valid"] = False
        if "VALUES" not in query_upper and "SELECT" not in query_upper:
            validation["errors"].append("INSERT query must have VALUES or SELECT clause")
            validation["is_valid"] = False

    elif validation["query_type"] == "UPDATE":
        if "SET" not in query_upper:
            validation["errors"].append("UPDATE query missing SET clause")
            validation["is_valid"] = False

    elif validation["query_type"] == "DELETE":
        if "FROM" not in query_upper:
            validation["errors"].append("DELETE query missing FROM clause")
            validation["is_valid"] = False

    # Common warnings
    if "SELECT *" in query_upper:
        validation["warnings"].append("Avoid SELECT * for better performance")

    if validation["query_type"] in ["UPDATE", "DELETE"] and "WHERE" not in query_upper:
        validation["warnings"].append(f"{validation['query_type']} without WHERE clause will affect all rows")

    # Check for common SQL injection patterns
    if re.search(r'["\'].*\+.*["\']', query):
        validation["warnings"].append("Potential SQL injection risk - use parameterized queries")

    # Parentheses balance
    if query.count("(") != query.count(")"):
        validation["errors"].append("Unbalanced parentheses")
        validation["is_valid"] = False

    # Quote balance
    single_quotes = query.count("'") % 2
    double_quotes = query.count('"') % 2
    if single_quotes != 0:
        validation["errors"].append("Unbalanced single quotes")
        validation["is_valid"] = False
    if double_quotes != 0:
        validation["errors"].append("Unbalanced double quotes")
        validation["is_valid"] = False

    return validation


def generate_schema_from_description(
    description: str, dialect: str = "postgresql", model: str = "deepseek-coder:6.7b"
) -> str:
    """
    Generate CREATE TABLE statements from natural language description

    Args:
        description: Description of the database schema
        dialect: SQL dialect
        model: Ollama model to use

    Returns:
        CREATE TABLE statements
    """

    prompt = f"""
Generate {dialect} CREATE TABLE statements for the following database description:
{description}

Requirements:
1. Use appropriate data types for {dialect}
2. Include PRIMARY KEY constraints
3. Add FOREIGN KEY constraints where relationships are mentioned
4. Include NOT NULL constraints for required fields
5. Add CHECK constraints for data validation where appropriate
6. Include indexes for commonly queried fields
7. Add comments to explain the purpose of tables and columns

Return complete, executable CREATE TABLE statements.
"""

    response = safe_ollama_generate(model, prompt)

    return response["response"]


def convert_sql_dialect(query: str, from_dialect: str, to_dialect: str, model: str = "deepseek-coder:6.7b") -> str:
    """
    Convert SQL query from one dialect to another

    Args:
        query: SQL query to convert
        from_dialect: Source SQL dialect
        to_dialect: Target SQL dialect
        model: Ollama model to use

    Returns:
        Converted SQL query
        
    Raises:
        ValueError: If query is empty or dialects are the same
    """
    
    # Input validation
    if not query or not query.strip():
        raise ValueError("Query cannot be empty")
        
    if from_dialect.lower() == to_dialect.lower():
        return query  # No conversion needed

    prompt = f"""
Convert this {from_dialect} SQL query to {to_dialect}:

```sql
{query}
```

Conversion requirements:
1. Use proper {to_dialect} data types
2. Convert functions to {to_dialect} equivalents
3. Adjust syntax for {to_dialect} specific features
4. Handle differences in:
   - String concatenation
   - Date/time functions
   - Limit/offset syntax
   - Quote characters
   - Case sensitivity

Return only the converted query.
"""

    response = safe_ollama_generate(model, prompt)

    return response["response"]


def analyze_query_performance(query: str, dialect: str = "postgresql") -> Dict[str, Any]:
    """
    Analyze query performance characteristics
    """

    analysis = {
        "complexity": "low",
        "estimated_cost": "low",
        "optimization_suggestions": [],
        "index_recommendations": [],
    }

    query_upper = query.upper()

    # Count operations
    join_count = len(re.findall(r"\bJOIN\b", query_upper))
    subquery_count = query_upper.count("(SELECT")

    # Determine complexity
    if join_count > 3 or subquery_count > 1:
        analysis["complexity"] = "high"
        analysis["estimated_cost"] = "high"
    elif join_count > 1 or subquery_count > 0:
        analysis["complexity"] = "medium"
        analysis["estimated_cost"] = "medium"

    # Optimization suggestions
    if "SELECT *" in query_upper:
        analysis["optimization_suggestions"].append("Replace SELECT * with specific columns")

    if "NOT IN" in query_upper:
        analysis["optimization_suggestions"].append("Consider using NOT EXISTS instead of NOT IN")

    if join_count > 2:
        analysis["optimization_suggestions"].append("Review JOIN order - place smaller tables first")

    if "LIKE '%'" in query_upper:
        analysis["optimization_suggestions"].append("Leading wildcard prevents index usage")

    # Index recommendations
    where_match = re.search(r"WHERE\s+(\w+)", query, re.IGNORECASE)
    if where_match:
        analysis["index_recommendations"].append(f"Consider index on {where_match.group(1)}")

    join_matches = re.findall(r"ON\s+\w+\.(\w+)\s*=\s*\w+\.(\w+)", query, re.IGNORECASE)
    for col1, col2 in join_matches:
        analysis["index_recommendations"].append(f"Consider indexes on {col1} and {col2}")

    return analysis


def format_sql_query(query: str, indent_width: int = 4) -> str:
    """
    Format and beautify SQL query for better readability
    
    Args:
        query: SQL query to format
        indent_width: Number of spaces for indentation
        
    Returns:
        Formatted SQL query
    """
    if not query or not query.strip():
        return query
    
    # Clean the query first
    query = _clean_sql_response(query)
    
    # Keywords that should be on new lines
    major_keywords = [
        'SELECT', 'FROM', 'WHERE', 'GROUP BY', 'HAVING', 
        'ORDER BY', 'LIMIT', 'OFFSET', 'UNION', 'INTERSECT', 'EXCEPT',
        'INSERT INTO', 'VALUES', 'UPDATE', 'SET', 'DELETE FROM',
        'CREATE TABLE', 'ALTER TABLE', 'DROP TABLE', 'CREATE INDEX',
        'LEFT JOIN', 'RIGHT JOIN', 'INNER JOIN', 'OUTER JOIN', 'JOIN',
        'ON', 'AND', 'OR', 'CASE', 'WHEN', 'THEN', 'ELSE', 'END'
    ]
    
    # Format the query
    formatted = query
    indent = ' ' * indent_width
    
    # Add newlines before major keywords
    for keyword in major_keywords:
        # Add newline before keyword (case-insensitive)
        pattern = rf'\s+(?={keyword}\b)'
        formatted = re.sub(pattern, f'\n', formatted, flags=re.IGNORECASE)
    
    # Handle nested queries
    formatted = formatted.replace('(SELECT', f'(\n{indent}SELECT')
    
    # Format commas in SELECT statements
    formatted = re.sub(r',\s*', ',\n       ', formatted)
    
    # Clean up extra newlines
    formatted = re.sub(r'\n\s*\n', '\n', formatted)
    
    # Ensure proper spacing around operators
    formatted = re.sub(r'\s*=\s*', ' = ', formatted)
    formatted = re.sub(r'\s*>\s*', ' > ', formatted)
    formatted = re.sub(r'\s*<\s*', ' < ', formatted)
    
    return formatted.strip()


def generate_table_documentation(table_name: str, columns: List[Dict[str, str]], 
                               dialect: str = "postgresql", model: str = "deepseek-coder:6.7b") -> str:
    """
    Generate comprehensive table documentation
    
    Args:
        table_name: Name of the table
        columns: List of column definitions with name, type, constraints
        dialect: SQL dialect
        model: Ollama model to use
        
    Returns:
        Markdown documentation for the table
    """
    
    # Build column info string
    column_info = "\n".join([
        f"- {col.get('name', 'unknown')}: {col.get('type', 'unknown')} "
        f"{'NOT NULL' if col.get('not_null') else 'NULL'} "
        f"{'PRIMARY KEY' if col.get('primary_key') else ''}"
        for col in columns
    ])
    
    prompt = f"""
Generate comprehensive documentation for this {dialect} database table:

Table Name: {table_name}
Columns:
{column_info}

Please provide:
1. Table purpose and description
2. Column descriptions with business meaning
3. Common query examples
4. Best practices for using this table
5. Potential indexes to improve performance

Format the output as clean Markdown documentation.
"""
    
    response = safe_ollama_generate(model, prompt)
    
    if response.get("error"):
        return f"# {table_name}\n\nError generating documentation: {response.get('error')}"
    
    return response["response"]


def sql_to_code(query: str, target_language: str = "python", 
                library: str = "default", model: str = "deepseek-coder:6.7b") -> str:
    """
    Convert SQL query to code in target programming language
    
    Args:
        query: SQL query to convert
        target_language: Target programming language (python, java, javascript, etc.)
        library: Specific library to use (e.g., psycopg2, sqlalchemy for Python)
        model: Ollama model to use
        
    Returns:
        Code snippet in target language
    """
    
    # Library mappings
    library_map = {
        "python": {
            "default": "psycopg2",
            "options": ["psycopg2", "sqlalchemy", "pandas", "sqlite3"]
        },
        "java": {
            "default": "jdbc",
            "options": ["jdbc", "hibernate", "spring"]
        },
        "javascript": {
            "default": "pg",
            "options": ["pg", "mysql2", "sequelize", "knex"]
        },
        "csharp": {
            "default": "ado.net",
            "options": ["ado.net", "entity-framework", "dapper"]
        },
        "smalltalk": {
            "default": "visualworks",
            "options": ["visualworks", "pharo", "squeak"]
        }
    }
    
    # Get appropriate library
    if library == "default" and target_language.lower() in library_map:
        library = library_map[target_language.lower()]["default"]
    
    prompt = f"""
Convert this SQL query to {target_language} code using {library}:

```sql
{query}
```

Requirements:
1. Include necessary imports
2. Use parameterized queries to prevent SQL injection
3. Include proper error handling
4. Add connection setup if needed
5. Return results in appropriate data structure
6. Add helpful comments

Return clean, production-ready code.
"""
    
    response = safe_ollama_generate(model, prompt)
    
    if response.get("error"):
        return f"# Error converting SQL to {target_language}: {response.get('error')}"
    
    return response["response"]


def generate_sample_data_query(table_name: str, columns: List[str], 
                             row_count: int = 10, dialect: str = "postgresql") -> str:
    """
    Generate SQL to create sample data for testing
    
    Args:
        table_name: Table to generate data for
        columns: List of column names
        row_count: Number of sample rows to generate
        dialect: SQL dialect
        
    Returns:
        SQL INSERT statements with sample data
    """
    
    if not columns:
        return f"-- Error: No columns specified for table {table_name}"
    
    # Generate sample data based on column names (heuristic approach)
    sample_values = []
    
    for i in range(min(row_count, 100)):  # Cap at 100 rows
        row_values = []
        for col in columns:
            col_lower = col.lower()
            
            # Heuristic value generation based on column name
            if 'id' in col_lower:
                row_values.append(str(i + 1))
            elif 'name' in col_lower:
                if 'first' in col_lower:
                    row_values.append(f"'FirstName{i + 1}'")
                elif 'last' in col_lower:
                    row_values.append(f"'LastName{i + 1}'")
                else:
                    row_values.append(f"'Name{i + 1}'")
            elif 'email' in col_lower:
                row_values.append(f"'user{i + 1}@example.com'")
            elif 'phone' in col_lower:
                row_values.append(f"'555-{1000 + i:04d}'")
            elif 'date' in col_lower or 'created' in col_lower or 'updated' in col_lower:
                row_values.append("CURRENT_DATE")
            elif 'time' in col_lower:
                row_values.append("CURRENT_TIMESTAMP")
            elif 'price' in col_lower or 'amount' in col_lower or 'cost' in col_lower:
                row_values.append(f"{(i + 1) * 10.99:.2f}")
            elif 'quantity' in col_lower or 'count' in col_lower:
                row_values.append(str((i % 10) + 1))
            elif 'status' in col_lower:
                statuses = ['active', 'inactive', 'pending']
                row_values.append(f"'{statuses[i % len(statuses)]}'")
            elif 'description' in col_lower or 'comment' in col_lower:
                row_values.append(f"'Sample description for row {i + 1}'")
            else:
                row_values.append(f"'Value{i + 1}'")
        
        sample_values.append(f"    ({', '.join(row_values)})")
    
    # Build INSERT statement
    insert_sql = f"""-- Sample data for {table_name}
INSERT INTO {table_name} ({', '.join(columns)})
VALUES
{',\n'.join(sample_values)};"""
    
    return insert_sql


def analyze_query_dependencies(query: str) -> Dict[str, List[str]]:
    """
    Analyze which tables and columns a query depends on
    
    Args:
        query: SQL query to analyze
        
    Returns:
        Dictionary with tables, columns, and joins used
    """
    
    query_upper = query.upper()
    
    # Extract table names (basic pattern matching)
    table_pattern = r'(?:FROM|JOIN)\s+(\w+)(?:\s+(?:AS\s+)?(\w+))?'
    table_matches = re.findall(table_pattern, query, re.IGNORECASE)
    
    tables = []
    aliases = {}
    for match in table_matches:
        table = match[0]
        alias = match[1] if match[1] else match[0]
        tables.append(table)
        aliases[alias] = table
    
    # Extract column references
    column_pattern = r'(?:SELECT|WHERE|ON|GROUP BY|ORDER BY)\s+.*?(\w+\.\w+|\w+)'
    column_matches = re.findall(column_pattern, query, re.IGNORECASE)
    
    columns = []
    for col in column_matches:
        if '.' in col:
            columns.append(col)
        else:
            # Try to infer table from context
            columns.append(col)
    
    # Extract join conditions
    join_pattern = r'JOIN\s+\w+.*?ON\s+(.*?)(?:WHERE|GROUP|ORDER|LIMIT|$)'
    join_matches = re.findall(join_pattern, query, re.IGNORECASE | re.DOTALL)
    
    return {
        "tables": list(set(tables)),
        "columns": list(set(columns)),
        "aliases": aliases,
        "joins": join_matches,
        "has_subquery": '(SELECT' in query_upper,
        "has_union": 'UNION' in query_upper,
        "has_aggregation": any(func in query_upper for func in ['COUNT', 'SUM', 'AVG', 'MAX', 'MIN'])
    }


def sql_to_smalltalk_visualworks(query: str, connection_type: str = "postgresql", 
                                 model: str = "deepseek-coder:6.7b") -> str:
    """
    Convert SQL query to Cincom VisualWorks Smalltalk code
    
    Args:
        query: SQL query to convert
        connection_type: Database type (postgresql, mysql, oracle, etc.)
        model: Ollama model to use
        
    Returns:
        Smalltalk code for VisualWorks with database access
    """
    
    prompt = f"""
Convert this SQL query to Cincom VisualWorks Smalltalk code:

```sql
{query}
```

Requirements:
1. Use VisualWorks database framework (EXDI - External Database Interface)
2. Create a proper connection for {connection_type}
3. Use prepared statements/bind variables where appropriate
4. Include proper error handling with exception blocks
5. Return results as appropriate Smalltalk collections
6. Add class and method structure following Smalltalk conventions
7. Include comments explaining the code

Generate complete, working Smalltalk code that would execute in VisualWorks 8.x or later.

The code should:
- Define a class method for database connection
- Define an instance method that executes the query
- Handle result sets appropriately
- Close connections properly
- Use Smalltalk naming conventions (camelCase for methods, PascalCase for classes)

Example structure:
```smalltalk
"Class definition"
DatabaseAccessor subclass: #MyDatabaseQuery
    instanceVariableNames: 'connection'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyApp-Database'

"Instance methods..."
```
"""
    
    response = safe_ollama_generate(model, prompt)
    
    if response.get("error"):
        return f"\"Error converting SQL to Smalltalk: {response.get('error')}\""
    
    return response["response"]
