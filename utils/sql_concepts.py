# SQL Concepts and Learning Resources

# Core SQL concepts with descriptions and resources
SQL_CONCEPTS = {
    "SELECT": {
        "name": "SELECT Statement",
        "description": "Retrieves data from database tables",
        "difficulty": "Beginner",
        "resources": [
            {"title": "SELECT Basics", "url": "https://www.w3schools.com/sql/sql_select.asp"},
            {"title": "Advanced SELECT", "url": "https://www.postgresql.org/docs/current/sql-select.html"}
        ],
        "tips": [
            "Always specify columns instead of using SELECT *",
            "Use aliases for better readability",
            "Consider the order of columns for clarity"
        ]
    },
    "JOIN": {
        "name": "JOIN Operations",
        "description": "Combines rows from two or more tables",
        "difficulty": "Intermediate",
        "resources": [
            {"title": "Visual Guide to JOINs", "url": "https://blog.codinghorror.com/a-visual-explanation-of-sql-joins/"},
            {"title": "JOIN Performance", "url": "https://use-the-index-luke.com/sql/join"}
        ],
        "tips": [
            "Start with INNER JOIN for matching records",
            "Use LEFT JOIN to include all records from the first table",
            "Always specify join conditions to avoid cartesian products"
        ]
    },
    "WHERE": {
        "name": "WHERE Clause",
        "description": "Filters records based on conditions",
        "difficulty": "Beginner",
        "resources": [
            {"title": "WHERE Clause Guide", "url": "https://www.sqltutorial.org/sql-where/"},
            {"title": "Complex Conditions", "url": "https://www.postgresql.org/docs/current/sql-expressions.html"}
        ],
        "tips": [
            "Use indexes on columns in WHERE clauses",
            "Be careful with NULL comparisons",
            "Combine conditions with AND/OR carefully"
        ]
    },
    "GROUP BY": {
        "name": "GROUP BY Clause",
        "description": "Groups rows with same values into summary rows",
        "difficulty": "Intermediate",
        "resources": [
            {"title": "GROUP BY Tutorial", "url": "https://www.sqltutorial.org/sql-group-by/"},
            {"title": "Aggregation Functions", "url": "https://www.postgresql.org/docs/current/functions-aggregate.html"}
        ],
        "tips": [
            "All non-aggregated columns must be in GROUP BY",
            "Use HAVING to filter grouped results",
            "Consider performance with large datasets"
        ]
    },
    "ORDER BY": {
        "name": "ORDER BY Clause",
        "description": "Sorts the result set by one or more columns",
        "difficulty": "Beginner",
        "resources": [
            {"title": "Sorting Data", "url": "https://www.w3schools.com/sql/sql_orderby.asp"},
            {"title": "Multi-column Sorting", "url": "https://www.postgresql.org/docs/current/queries-order.html"}
        ],
        "tips": [
            "Default is ascending order (ASC)",
            "NULL values sort differently in databases",
            "Consider indexes for frequently sorted columns"
        ]
    },
    "Aggregation": {
        "name": "Aggregate Functions",
        "description": "Performs calculations on sets of values",
        "difficulty": "Intermediate",
        "resources": [
            {"title": "Common Aggregates", "url": "https://www.sqltutorial.org/sql-aggregate-functions/"},
            {"title": "Window Functions", "url": "https://www.postgresql.org/docs/current/tutorial-window.html"}
        ],
        "tips": [
            "COUNT(*) counts all rows, COUNT(column) excludes NULLs",
            "Use DISTINCT within aggregates for unique values",
            "Consider window functions for running totals"
        ]
    },
    "Subquery": {
        "name": "Subqueries",
        "description": "Queries nested within other queries",
        "difficulty": "Advanced",
        "resources": [
            {"title": "Subquery Basics", "url": "https://www.sqlshack.com/sql-subquery-examples/"},
            {"title": "Correlated Subqueries", "url": "https://www.postgresql.org/docs/current/sql-expressions.html#SQL-SYNTAX-SCALAR-SUBQUERIES"}
        ],
        "tips": [
            "Consider JOINs instead of subqueries for better performance",
            "Use EXISTS for existence checks",
            "Be aware of NULL handling in NOT IN subqueries"
        ]
    },
    "CTE": {
        "name": "Common Table Expressions",
        "description": "Named temporary result sets",
        "difficulty": "Advanced",
        "resources": [
            {"title": "CTE Introduction", "url": "https://www.essentialsql.com/introduction-common-table-expressions-ctes/"},
            {"title": "Recursive CTEs", "url": "https://www.postgresql.org/docs/current/queries-with.html"}
        ],
        "tips": [
            "CTEs improve readability for complex queries",
            "Can be recursive for hierarchical data",
            "May have performance implications vs subqueries"
        ]
    },
    "Window Functions": {
        "name": "Window Functions",
        "description": "Calculations across sets of rows",
        "difficulty": "Advanced",
        "resources": [
            {"title": "Window Functions Guide", "url": "https://www.postgresql.org/docs/current/tutorial-window.html"},
            {"title": "Practical Examples", "url": "https://mode.com/sql-tutorial/sql-window-functions/"}
        ],
        "tips": [
            "Use for running totals and rankings",
            "PARTITION BY creates windows within results",
            "ORDER BY in OVER clause affects calculations"
        ]
    },
    "Index": {
        "name": "Database Indexes",
        "description": "Structures that improve query performance",
        "difficulty": "Intermediate",
        "resources": [
            {"title": "Index Basics", "url": "https://use-the-index-luke.com/"},
            {"title": "Index Types", "url": "https://www.postgresql.org/docs/current/indexes-types.html"}
        ],
        "tips": [
            "Index columns used in WHERE and JOIN conditions",
            "Too many indexes slow down writes",
            "Consider composite indexes for multi-column queries"
        ]
    }
}

# Quiz questions for different concepts
CONCEPT_QUIZZES = {
    "SELECT": [
        {
            "question": "What is the purpose of the SELECT statement?",
            "options": [
                "To insert data into a table",
                "To retrieve data from a table",
                "To delete data from a table",
                "To create a new table"
            ],
            "correct": 1,
            "explanation": "SELECT is used to retrieve (query) data from database tables."
        },
        {
            "question": "Which is better practice?",
            "options": [
                "SELECT * FROM users",
                "SELECT id, name, email FROM users",
                "Both are equally good",
                "It depends on the database"
            ],
            "correct": 1,
            "explanation": "Specifying columns is better for performance and clarity."
        }
    ],
    "JOIN": [
        {
            "question": "What does an INNER JOIN do?",
            "options": [
                "Returns all rows from both tables",
                "Returns only matching rows from both tables",
                "Returns all rows from the left table",
                "Returns all rows from the right table"
            ],
            "correct": 1,
            "explanation": "INNER JOIN returns only rows that have matching values in both tables."
        },
        {
            "question": "When should you use a LEFT JOIN?",
            "options": [
                "When you need all records from both tables",
                "When you need only matching records",
                "When you need all records from the first table",
                "When performance is critical"
            ],
            "correct": 2,
            "explanation": "LEFT JOIN returns all records from the left table and matching records from the right."
        }
    ],
    "WHERE": [
        {
            "question": "How do you check for NULL values in a WHERE clause?",
            "options": [
                "WHERE column = NULL",
                "WHERE column IS NULL",
                "WHERE column == NULL",
                "WHERE column <> NULL"
            ],
            "correct": 1,
            "explanation": "Use IS NULL or IS NOT NULL to check for NULL values."
        }
    ],
    "GROUP BY": [
        {
            "question": "What must be true about SELECT columns when using GROUP BY?",
            "options": [
                "All columns must be numeric",
                "All columns must be in the GROUP BY or be aggregated",
                "Only one column can be selected",
                "Columns must be indexed"
            ],
            "correct": 1,
            "explanation": "Non-aggregated columns in SELECT must appear in GROUP BY."
        }
    ],
    "Aggregation": [
        {
            "question": "What's the difference between COUNT(*) and COUNT(column)?",
            "options": [
                "No difference",
                "COUNT(*) is faster",
                "COUNT(column) excludes NULL values",
                "COUNT(*) only works with indexed tables"
            ],
            "correct": 2,
            "explanation": "COUNT(*) counts all rows, COUNT(column) excludes NULL values in that column."
        }
    ]
}

# Learning paths between concepts
LEARNING_PATHS = {
    "beginner": ["SELECT", "WHERE", "ORDER BY"],
    "intermediate": ["SELECT", "JOIN", "GROUP BY", "Aggregation"],
    "advanced": ["Subquery", "CTE", "Window Functions", "Index"],
    "optimization": ["Index", "JOIN", "Subquery", "CTE"]
}

def get_concept_info(concept_key):
    """Get information about a SQL concept"""
    return SQL_CONCEPTS.get(concept_key, {})

def get_concepts_in_query(sql_query):
    """Detect SQL concepts used in a query"""
    query_upper = sql_query.upper()
    detected = []
    
    # Check for each concept
    if "SELECT" in query_upper:
        detected.append("SELECT")
    if any(join in query_upper for join in ["JOIN", "LEFT JOIN", "RIGHT JOIN", "INNER JOIN"]):
        detected.append("JOIN")
    if "WHERE" in query_upper:
        detected.append("WHERE")
    if "GROUP BY" in query_upper:
        detected.append("GROUP BY")
    if "ORDER BY" in query_upper:
        detected.append("ORDER BY")
    if any(agg in query_upper for agg in ["COUNT(", "SUM(", "AVG(", "MAX(", "MIN("]):
        detected.append("Aggregation")
    if "(" in query_upper and "SELECT" in query_upper[query_upper.find("("):]:
        detected.append("Subquery")
    if "WITH" in query_upper and " AS " in query_upper:
        detected.append("CTE")
    if "OVER" in query_upper and "(" in query_upper:
        detected.append("Window Functions")
    
    return detected

def get_learning_path(current_level="beginner"):
    """Get a suggested learning path based on level"""
    return LEARNING_PATHS.get(current_level, LEARNING_PATHS["beginner"])

def get_quiz_for_concept(concept_key):
    """Get quiz questions for a specific concept"""
    return CONCEPT_QUIZZES.get(concept_key, [])

def get_difficulty_color(difficulty):
    """Get color coding for difficulty levels"""
    colors = {
        "Beginner": "#4CAF50",
        "Intermediate": "#FF9800",
        "Advanced": "#F44336"
    }
    return colors.get(difficulty, "#757575")
