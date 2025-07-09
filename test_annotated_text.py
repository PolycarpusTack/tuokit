"""
Test script for annotated text functionality
Run this to see examples of annotated text in action
"""

import streamlit as st
from utils.annotated_text_helpers import (
    annotated_text, parse_error_message, parse_stack_trace,
    highlight_severity, display_annotated_error, annotate_sql_query,
    create_code_snippet_annotation, ANNOTATION_COLORS
)

st.set_page_config(page_title="Annotated Text Demo", layout="wide")

st.title("🎨 Annotated Text Demo")
st.caption("Demonstrating the new streamlit-extras integration")

# Demo 1: Error Messages
st.header("1. Error Message Annotations")

error_examples = [
    "TypeError: undefined method 'capitalize' for nil:NilClass at line 42",
    "SyntaxError: unexpected token '}' in /app/controllers/users_controller.rb:15",
    "RuntimeError: Connection timeout after 30 seconds to database server",
    "ValueError: invalid literal for int() with base 10: 'abc'"
]

for error in error_examples:
    components = parse_error_message(error)
    annotated_text(*components)
    st.write("")  # Add spacing

# Demo 2: Stack Trace
st.header("2. Stack Trace Annotations")

stack_trace = """
TypeError: undefined method 'capitalize' for nil:NilClass
    /app/models/user.rb:42:in `format_name'
    /app/controllers/users_controller.rb:15:in `create'
    /lib/middleware/error_handler.rb:8:in `call'
    /vendor/bundle/ruby/3.0.0/gems/rack-2.2.3/lib/rack/method_override.rb:24:in `call'
"""

st.text("Original stack trace:")
st.code(stack_trace)

st.text("Annotated stack trace:")
parsed_stack = parse_stack_trace(stack_trace)
for line_components in parsed_stack:
    if line_components:
        annotated_text(*line_components)

# Demo 3: Severity Levels
st.header("3. Severity Level Highlighting")

severities = ["CRITICAL", "HIGH", "MEDIUM", "LOW"]
for severity in severities:
    st.write("Severity level:")
    annotated_text(highlight_severity(severity))
    st.write("")

# Demo 4: SQL Query
st.header("4. SQL Query Annotations")

sql_query = """
SELECT u.name, u.email, COUNT(o.id) as order_count
FROM users u
LEFT JOIN orders o ON u.id = o.user_id
WHERE u.created_at > '2024-01-01'
GROUP BY u.id, u.name, u.email
HAVING COUNT(o.id) > 5
ORDER BY order_count DESC
LIMIT 10
"""

st.text("SQL Query with syntax highlighting:")
sql_components = annotate_sql_query(sql_query)
annotated_text(*sql_components)

# Demo 5: Complete Error Display
st.header("5. Complete Error Display")

error_data = {
    'severity': 'CRITICAL',
    'error_type': 'DatabaseConnectionError',
    'message': "Could not connect to PostgreSQL server on host 'localhost' port 5432",
    'file': '/app/config/database.rb',
    'line': 15,
    'stack_trace': stack_trace
}

display_annotated_error(error_data)

# Demo 6: Code Snippet with Error
st.header("6. Code Snippet with Error Line")

code_snippet = """
def calculate_average(numbers)
  return 0 if numbers.empty?
  sum = numbers.reduce(:+)
  sum / numbers.length  # Error: division by zero if numbers is nil
end
"""

st.text("Code with error on line 4:")
code_lines = create_code_snippet_annotation(code_snippet, error_line=4)
for line_components in code_lines:
    annotated_text(*line_components)

st.success("✅ Annotated text integration complete! These examples show how error messages, stack traces, and code are now more readable with color-coded annotations.")