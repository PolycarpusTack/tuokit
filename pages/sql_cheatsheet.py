"""
TuoKit - SQL Cheat Sheet
Comprehensive SQL reference for developers, analysts, and support staff
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="SQL Cheat Sheet - TuoKit",
    page_icon="🗃️",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="sql_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🗃️ SQL Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Standard SQL reference for developers, analysts, and support staff
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search SQL commands...", placeholder="Try: JOIN, GROUP BY, DATE functions...")

# Create role-based tabs
role_tab1, role_tab2, role_tab3, role_tab4 = st.tabs([
    "👨‍💻 Developer", 
    "📊 Analyst", 
    "🛠️ Support", 
    "📚 Reference"
])

# Helper function
def show_sql_example(title, description, code, show=True):
    if show:
        st.markdown(f"### {title}")
        st.caption(description)
        st.code(code, language="sql")
        if st.button(f"📋 Copy", key=f"copy_{hash(title)}"):
            st.toast(f"Copied {title} to clipboard!")
        st.markdown("---")

def matches_search(text, query):
    if not query:
        return True
    return query.lower() in text.lower()

# Developer Tab
with role_tab1:
    st.header("👨‍💻 Developer Reference")
    
    dev_tabs = st.tabs([
        "Queries", "Joins", "Functions", "Performance", "Transactions", "Advanced"
    ])
    
    with dev_tabs[0]:  # Queries
        st.subheader("📝 Query Fundamentals")
        
        show_sql_example(
            "SELECT with All Clauses",
            "Complete SELECT statement structure in execution order",
            """-- Execution order: FROM → WHERE → GROUP BY → HAVING → SELECT → ORDER BY → LIMIT
SELECT 
    column1,
    column2,
    AGG_FUNCTION(column3) as aggregated_value
FROM table1 t1
    JOIN table2 t2 ON t1.id = t2.foreign_id
WHERE t1.status = 'active'
    AND t2.created_date >= '2024-01-01'
GROUP BY column1, column2
HAVING COUNT(*) > 5
ORDER BY aggregated_value DESC
LIMIT 10 OFFSET 20;""",
            matches_search("SELECT WHERE GROUP HAVING ORDER", search_query)
        )
        
        show_sql_example(
            "Common Table Expressions (CTEs)",
            "Modular queries with WITH clause",
            """-- Single CTE
WITH high_value_customers AS (
    SELECT customer_id, SUM(order_total) as lifetime_value
    FROM orders
    GROUP BY customer_id
    HAVING SUM(order_total) > 10000
)
SELECT c.*, hvc.lifetime_value
FROM customers c
JOIN high_value_customers hvc ON c.id = hvc.customer_id;

-- Multiple CTEs
WITH 
order_stats AS (
    SELECT customer_id, 
           COUNT(*) as order_count,
           AVG(total) as avg_order_value
    FROM orders
    GROUP BY customer_id
),
customer_segments AS (
    SELECT customer_id,
           CASE 
               WHEN order_count > 10 THEN 'Frequent'
               WHEN order_count > 5 THEN 'Regular'
               ELSE 'Occasional'
           END as segment
    FROM order_stats
)
SELECT * FROM customer_segments;""",
            matches_search("CTE WITH Common Table Expression", search_query)
        )
        
    with dev_tabs[1]:  # Joins
        st.subheader("🔗 JOIN Operations")
        
        # Visual guide
        st.markdown("""
        <div style="background: #1e1e2e; padding: 1rem; border-radius: 8px; margin-bottom: 1rem;">
        <h4>Visual JOIN Guide</h4>
        <pre>
        INNER JOIN: Returns matching rows from both tables
        A: [1,2,3]  B: [2,3,4]  Result: [2,3]
        
        LEFT JOIN: All from left + matching from right
        A: [1,2,3]  B: [2,3,4]  Result: [1,2,3] (1 has NULL for B)
        
        RIGHT JOIN: All from right + matching from left
        A: [1,2,3]  B: [2,3,4]  Result: [2,3,4] (4 has NULL for A)
        
        FULL OUTER: All from both tables
        A: [1,2,3]  B: [2,3,4]  Result: [1,2,3,4] (1,4 have NULLs)
        </pre>
        </div>
        """, unsafe_allow_html=True)
        
        show_sql_example(
            "All JOIN Types",
            "Comprehensive JOIN examples",
            """-- INNER JOIN (most common)
SELECT o.order_id, c.customer_name, o.total
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;

-- LEFT JOIN (keep all left records)
SELECT c.customer_name, COUNT(o.order_id) as order_count
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
GROUP BY c.customer_name;

-- RIGHT JOIN (keep all right records)
SELECT p.product_name, COALESCE(SUM(oi.quantity), 0) as total_sold
FROM order_items oi
RIGHT JOIN products p ON oi.product_id = p.id
GROUP BY p.product_name;

-- FULL OUTER JOIN (keep all records)
SELECT 
    COALESCE(c.customer_name, 'No Customer') as customer,
    COALESCE(o.order_id, 'No Order') as order_ref
FROM customers c
FULL OUTER JOIN orders o ON c.id = o.customer_id;

-- CROSS JOIN (Cartesian product)
SELECT p.product_name, s.size_name
FROM products p
CROSS JOIN sizes s;

-- Self JOIN
SELECT e1.name as employee, e2.name as manager
FROM employees e1
LEFT JOIN employees e2 ON e1.manager_id = e2.id;""",
            matches_search("JOIN INNER LEFT RIGHT FULL OUTER CROSS", search_query)
        )
        
    with dev_tabs[2]:  # Functions
        st.subheader("🧮 SQL Functions")
        
        col1, col2 = st.columns(2)
        
        with col1:
            show_sql_example(
                "String Functions",
                "Text manipulation functions",
                """-- Concatenation
SELECT first_name || ' ' || last_name AS full_name  -- Standard
SELECT CONCAT(first_name, ' ', last_name) AS full_name  -- MySQL

-- Case manipulation
SELECT UPPER(name), LOWER(email), INITCAP(title)
FROM users;

-- Substring operations
SELECT 
    SUBSTRING(description, 1, 100) AS preview,
    LENGTH(description) AS char_count,
    POSITION('error' IN log_message) AS error_position,
    REPLACE(phone, '-', '') AS cleaned_phone
FROM data;

-- Trimming and padding
SELECT 
    TRIM(both ' ' FROM name) AS trimmed,
    LTRIM(code, '0') AS no_leading_zeros,
    LPAD(id::text, 5, '0') AS padded_id
FROM items;""",
                matches_search("STRING CONCAT SUBSTRING TRIM", search_query)
            )
            
        with col2:
            show_sql_example(
                "Date/Time Functions",
                "Working with temporal data",
                """-- Current date/time
SELECT 
    CURRENT_DATE,
    CURRENT_TIME,
    CURRENT_TIMESTAMP,
    NOW();  -- PostgreSQL/MySQL

-- Date arithmetic
SELECT 
    order_date + INTERVAL '7 days' AS week_later,
    order_date - INTERVAL '1 month' AS month_ago,
    AGE(CURRENT_DATE, birth_date) AS age  -- PostgreSQL
FROM orders;

-- Date extraction
SELECT 
    EXTRACT(YEAR FROM order_date) AS year,
    EXTRACT(MONTH FROM order_date) AS month,
    EXTRACT(DOW FROM order_date) AS day_of_week,
    DATE_PART('quarter', order_date) AS quarter
FROM orders;

-- Date formatting
SELECT 
    TO_CHAR(order_date, 'YYYY-MM-DD') AS iso_date,
    TO_CHAR(order_date, 'Month DD, YYYY') AS long_date,
    DATE_FORMAT(order_date, '%Y-%m-%d')  -- MySQL
FROM orders;""",
                matches_search("DATE TIME CURRENT EXTRACT FORMAT", search_query)
            )
            
    with dev_tabs[3]:  # Performance
        st.subheader("⚡ Performance Optimization")
        
        show_sql_example(
            "Query Optimization Techniques",
            "Best practices for faster queries",
            """-- Use EXPLAIN to understand query plans
EXPLAIN (ANALYZE, BUFFERS)  -- PostgreSQL
SELECT * FROM large_table WHERE column = 'value';

-- Index usage hints
SELECT /*+ INDEX(orders idx_customer_date) */ *  -- Oracle
FROM orders
WHERE customer_id = 123 AND order_date > '2024-01-01';

-- Avoid SELECT * in production
-- Bad:
SELECT * FROM customers;

-- Good: Select only needed columns
SELECT customer_id, name, email FROM customers;

-- Use EXISTS instead of IN for better performance
-- Slower:
SELECT * FROM orders 
WHERE customer_id IN (SELECT id FROM vip_customers);

-- Faster:
SELECT * FROM orders o
WHERE EXISTS (
    SELECT 1 FROM vip_customers v 
    WHERE v.id = o.customer_id
);

-- Optimize GROUP BY with indexes
CREATE INDEX idx_sales_date_product 
ON sales(sale_date, product_id);

-- Use UNION ALL instead of UNION when possible
SELECT id FROM table1
UNION ALL  -- No duplicate removal, faster
SELECT id FROM table2;""",
            matches_search("EXPLAIN PERFORMANCE INDEX OPTIMIZE", search_query)
        )

# Analyst Tab
with role_tab2:
    st.header("📊 Analyst Toolkit")
    
    analyst_tabs = st.tabs([
        "Reports", "Aggregations", "Time Series", "Data Quality", "Export"
    ])
    
    with analyst_tabs[0]:  # Reports
        st.subheader("📈 Report Templates")
        
        show_sql_example(
            "Monthly Summary Report",
            "Template for monthly business metrics",
            """-- Monthly summary with comparisons
WITH monthly_metrics AS (
    SELECT 
        DATE_TRUNC('month', order_date) AS month,
        COUNT(DISTINCT customer_id) AS unique_customers,
        COUNT(*) AS total_orders,
        SUM(order_total) AS revenue,
        AVG(order_total) AS avg_order_value
    FROM orders
    WHERE order_date >= DATE_TRUNC('year', CURRENT_DATE)
    GROUP BY 1
)
SELECT 
    TO_CHAR(month, 'YYYY-MM') AS month_label,
    unique_customers,
    total_orders,
    revenue,
    ROUND(avg_order_value, 2) AS avg_order_value,
    -- Month-over-month calculations
    LAG(revenue) OVER (ORDER BY month) AS prev_month_revenue,
    ROUND((revenue - LAG(revenue) OVER (ORDER BY month)) / 
          NULLIF(LAG(revenue) OVER (ORDER BY month), 0) * 100, 2) AS revenue_growth_pct
FROM monthly_metrics
ORDER BY month;""",
            matches_search("MONTHLY REPORT SUMMARY", search_query)
        )
        
        show_sql_example(
            "Customer Cohort Analysis",
            "Track customer behavior by cohort",
            """-- Customer retention by cohort
WITH cohort_data AS (
    -- Get first purchase date for each customer
    SELECT 
        customer_id,
        DATE_TRUNC('month', MIN(order_date)) AS cohort_month
    FROM orders
    GROUP BY customer_id
),
cohort_size AS (
    -- Count customers in each cohort
    SELECT 
        cohort_month,
        COUNT(DISTINCT customer_id) AS customers
    FROM cohort_data
    GROUP BY cohort_month
),
retention_data AS (
    -- Calculate retention for each cohort/period
    SELECT 
        c.cohort_month,
        DATE_TRUNC('month', o.order_date) AS order_month,
        COUNT(DISTINCT o.customer_id) AS retained_customers
    FROM orders o
    JOIN cohort_data c ON o.customer_id = c.customer_id
    GROUP BY c.cohort_month, DATE_TRUNC('month', o.order_date)
)
SELECT 
    cs.cohort_month,
    rd.order_month,
    cs.customers AS cohort_size,
    rd.retained_customers,
    ROUND(100.0 * rd.retained_customers / cs.customers, 2) AS retention_rate,
    DATEDIFF('month', cs.cohort_month, rd.order_month) AS months_since_first
FROM cohort_size cs
JOIN retention_data rd ON cs.cohort_month = rd.cohort_month
ORDER BY cs.cohort_month, rd.order_month;""",
            matches_search("COHORT RETENTION ANALYSIS", search_query)
        )
        
    with analyst_tabs[1]:  # Aggregations
        st.subheader("📊 Aggregation Functions")
        
        show_sql_example(
            "Advanced Aggregations",
            "Beyond basic COUNT, SUM, AVG",
            """-- Statistical aggregations
SELECT 
    category,
    COUNT(*) AS count,
    SUM(amount) AS total,
    AVG(amount) AS average,
    MIN(amount) AS minimum,
    MAX(amount) AS maximum,
    STDDEV(amount) AS std_deviation,
    VARIANCE(amount) AS variance,
    -- Percentiles (PostgreSQL)
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY amount) AS q1,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY amount) AS median,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY amount) AS q3,
    -- Mode (most frequent value)
    MODE() WITHIN GROUP (ORDER BY amount) AS mode_value
FROM sales
GROUP BY category;

-- Conditional aggregations
SELECT 
    DATE_TRUNC('month', order_date) AS month,
    COUNT(*) AS total_orders,
    COUNT(CASE WHEN status = 'completed' THEN 1 END) AS completed_orders,
    COUNT(CASE WHEN status = 'cancelled' THEN 1 END) AS cancelled_orders,
    SUM(CASE WHEN status = 'completed' THEN total ELSE 0 END) AS revenue,
    ROUND(100.0 * COUNT(CASE WHEN status = 'completed' THEN 1 END) / 
          COUNT(*), 2) AS completion_rate
FROM orders
GROUP BY 1
ORDER BY 1;""",
            matches_search("AGGREGATION PERCENTILE MEDIAN STDDEV", search_query)
        )
        
    with analyst_tabs[2]:  # Time Series
        st.subheader("📅 Time Series Analysis")
        
        show_sql_example(
            "Moving Averages & Trends",
            "Smooth out fluctuations and identify trends",
            """-- 7-day moving average
SELECT 
    date,
    daily_sales,
    AVG(daily_sales) OVER (
        ORDER BY date 
        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
    ) AS ma_7_day,
    AVG(daily_sales) OVER (
        ORDER BY date 
        ROWS BETWEEN 29 PRECEDING AND CURRENT ROW
    ) AS ma_30_day
FROM daily_metrics
ORDER BY date;

-- Year-over-year comparison
WITH sales_by_date AS (
    SELECT 
        DATE(order_date) AS sale_date,
        SUM(total) AS daily_sales
    FROM orders
    GROUP BY DATE(order_date)
)
SELECT 
    sale_date,
    daily_sales,
    LAG(daily_sales, 365) OVER (ORDER BY sale_date) AS same_day_last_year,
    ROUND((daily_sales - LAG(daily_sales, 365) OVER (ORDER BY sale_date)) / 
          NULLIF(LAG(daily_sales, 365) OVER (ORDER BY sale_date), 0) * 100, 2) AS yoy_growth
FROM sales_by_date
WHERE sale_date >= CURRENT_DATE - INTERVAL '2 years'
ORDER BY sale_date DESC;

-- Seasonality detection
SELECT 
    EXTRACT(DOW FROM order_date) AS day_of_week,
    TO_CHAR(order_date, 'Day') AS day_name,
    COUNT(*) AS orders,
    ROUND(AVG(total), 2) AS avg_order_value,
    ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 2) AS pct_of_total
FROM orders
GROUP BY 1, 2
ORDER BY 1;""",
            matches_search("MOVING AVERAGE TIME SERIES YOY", search_query)
        )
        
    with analyst_tabs[3]:  # Data Quality
        st.subheader("🔍 Data Quality Checks")
        
        show_sql_example(
            "Data Validation Queries",
            "Identify data quality issues",
            """-- Find duplicates
SELECT 
    email,
    COUNT(*) as duplicate_count
FROM customers
GROUP BY email
HAVING COUNT(*) > 1;

-- Check for NULL values
SELECT 
    COUNT(*) AS total_rows,
    COUNT(column1) AS column1_non_null,
    COUNT(*) - COUNT(column1) AS column1_null_count,
    ROUND(100.0 * (COUNT(*) - COUNT(column1)) / COUNT(*), 2) AS column1_null_pct
FROM your_table;

-- Data freshness check
SELECT 
    MAX(created_at) AS latest_record,
    CURRENT_TIMESTAMP - MAX(created_at) AS time_since_last_record,
    CASE 
        WHEN MAX(created_at) > CURRENT_TIMESTAMP - INTERVAL '1 hour' THEN 'Fresh'
        WHEN MAX(created_at) > CURRENT_TIMESTAMP - INTERVAL '1 day' THEN 'Stale'
        ELSE 'Very Stale'
    END AS freshness_status
FROM transactions;

-- Outlier detection
WITH stats AS (
    SELECT 
        AVG(amount) AS mean,
        STDDEV(amount) AS std_dev
    FROM orders
)
SELECT o.*
FROM orders o, stats s
WHERE ABS(o.amount - s.mean) > 3 * s.std_dev;  -- 3 sigma rule""",
            matches_search("DUPLICATE NULL VALIDATION QUALITY", search_query)
        )

# Support Tab
with role_tab3:
    st.header("🛠️ Support Toolkit")
    
    support_tabs = st.tabs([
        "Troubleshooting", "User Management", "System Health", "Quick Fixes"
    ])
    
    with support_tabs[0]:  # Troubleshooting
        st.subheader("🔧 Troubleshooting Queries")
        
        show_sql_example(
            "Active Connections & Queries",
            "Monitor database activity",
            """-- PostgreSQL: View active connections
SELECT 
    pid,
    usename,
    application_name,
    client_addr,
    state,
    query_start,
    NOW() - query_start AS duration,
    query
FROM pg_stat_activity
WHERE state != 'idle'
ORDER BY query_start;

-- MySQL: Show processlist
SHOW FULL PROCESSLIST;

-- Oracle: Active sessions
SELECT 
    s.sid,
    s.serial#,
    s.username,
    s.status,
    s.program,
    q.sql_text
FROM v$session s
LEFT JOIN v$sql q ON s.sql_id = q.sql_id
WHERE s.type != 'BACKGROUND'
ORDER BY s.logon_time;

-- SQL Server: Current requests
SELECT 
    r.session_id,
    r.status,
    r.command,
    r.wait_type,
    t.text AS query_text
FROM sys.dm_exec_requests r
CROSS APPLY sys.dm_exec_sql_text(r.sql_handle) t
WHERE r.session_id != @@SPID;""",
            matches_search("CONNECTIONS PROCESSLIST ACTIVITY", search_query)
        )
        
        show_sql_example(
            "Find Blocking Queries",
            "Identify queries causing locks",
            """-- PostgreSQL: Find blocking queries
SELECT 
    blocked_locks.pid AS blocked_pid,
    blocked_activity.usename AS blocked_user,
    blocking_locks.pid AS blocking_pid,
    blocking_activity.usename AS blocking_user,
    blocked_activity.query AS blocked_statement,
    blocking_activity.query AS blocking_statement
FROM pg_catalog.pg_locks blocked_locks
JOIN pg_catalog.pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
JOIN pg_catalog.pg_locks blocking_locks 
    ON blocking_locks.locktype = blocked_locks.locktype
    AND blocking_locks.database IS NOT DISTINCT FROM blocked_locks.database
    AND blocking_locks.relation IS NOT DISTINCT FROM blocked_locks.relation
    AND blocking_locks.page IS NOT DISTINCT FROM blocked_locks.page
    AND blocking_locks.tuple IS NOT DISTINCT FROM blocked_locks.tuple
    AND blocking_locks.virtualxid IS NOT DISTINCT FROM blocked_locks.virtualxid
    AND blocking_locks.transactionid IS NOT DISTINCT FROM blocked_locks.transactionid
    AND blocking_locks.classid IS NOT DISTINCT FROM blocked_locks.classid
    AND blocking_locks.objid IS NOT DISTINCT FROM blocked_locks.objid
    AND blocking_locks.objsubid IS NOT DISTINCT FROM blocked_locks.objsubid
    AND blocking_locks.pid != blocked_locks.pid
JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
WHERE NOT blocked_locks.granted;

-- MySQL: Show locked tables
SHOW OPEN TABLES WHERE In_use > 0;

-- Kill blocking query (use with caution!)
-- PostgreSQL: SELECT pg_terminate_backend(pid);
-- MySQL: KILL process_id;
-- Oracle: ALTER SYSTEM KILL SESSION 'sid,serial#';""",
            matches_search("BLOCKING LOCKS DEADLOCK", search_query)
        )
        
    with support_tabs[1]:  # User Management
        st.subheader("👥 User & Permission Management")
        
        show_sql_example(
            "User Account Management",
            "Create, modify, and check user accounts",
            """-- Create user (PostgreSQL)
CREATE USER new_user WITH PASSWORD 'secure_password';
ALTER USER new_user VALID UNTIL '2024-12-31';

-- Create user (MySQL)
CREATE USER 'new_user'@'localhost' IDENTIFIED BY 'secure_password';
CREATE USER 'new_user'@'%' IDENTIFIED BY 'secure_password';  -- Any host

-- Check user permissions (PostgreSQL)
SELECT 
    grantee,
    table_schema,
    table_name,
    privilege_type
FROM information_schema.role_table_grants
WHERE grantee = 'username'
ORDER BY table_schema, table_name;

-- Check user permissions (MySQL)
SHOW GRANTS FOR 'username'@'localhost';

-- Reset password
-- PostgreSQL
ALTER USER username WITH PASSWORD 'new_password';

-- MySQL
ALTER USER 'username'@'localhost' IDENTIFIED BY 'new_password';

-- Oracle
ALTER USER username IDENTIFIED BY new_password;

-- Lock/Unlock account
-- PostgreSQL (use VALID UNTIL)
ALTER USER username VALID UNTIL 'infinity';  -- Unlock
ALTER USER username VALID UNTIL 'today';     -- Lock

-- MySQL
ALTER USER 'username'@'localhost' ACCOUNT LOCK;
ALTER USER 'username'@'localhost' ACCOUNT UNLOCK;""",
            matches_search("USER CREATE PASSWORD PERMISSION GRANT", search_query)
        )
        
    with support_tabs[2]:  # System Health
        st.subheader("💊 System Health Checks")
        
        show_sql_example(
            "Database Size & Growth",
            "Monitor storage usage",
            """-- Database sizes (PostgreSQL)
SELECT 
    datname AS database_name,
    pg_size_pretty(pg_database_size(datname)) AS size
FROM pg_database
ORDER BY pg_database_size(datname) DESC;

-- Table sizes with indexes (PostgreSQL)
SELECT 
    schemaname,
    tablename,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS total_size,
    pg_size_pretty(pg_relation_size(schemaname||'.'||tablename)) AS table_size,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename) - 
                   pg_relation_size(schemaname||'.'||tablename)) AS indexes_size
FROM pg_tables
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
LIMIT 20;

-- MySQL database sizes
SELECT 
    table_schema AS database_name,
    ROUND(SUM(data_length + index_length) / 1024 / 1024, 2) AS size_mb
FROM information_schema.tables
GROUP BY table_schema
ORDER BY size_mb DESC;

-- Growth tracking
SELECT 
    DATE(created_at) AS date,
    COUNT(*) AS records_added,
    SUM(COUNT(*)) OVER (ORDER BY DATE(created_at)) AS total_records
FROM your_table
WHERE created_at >= CURRENT_DATE - INTERVAL '30 days'
GROUP BY DATE(created_at)
ORDER BY date;""",
            matches_search("SIZE DATABASE TABLE GROWTH", search_query)
        )

# Reference Tab
with role_tab4:
    st.header("📚 Quick Reference")
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        st.subheader("📅 Date Shortcuts")
        st.code("""
-- Common date calculations
TODAY:          CURRENT_DATE
NOW:            CURRENT_TIMESTAMP
YESTERDAY:      CURRENT_DATE - 1
TOMORROW:       CURRENT_DATE + 1

-- Period starts
THIS_WEEK:      DATE_TRUNC('week', CURRENT_DATE)
THIS_MONTH:     DATE_TRUNC('month', CURRENT_DATE)
THIS_QUARTER:   DATE_TRUNC('quarter', CURRENT_DATE)
THIS_YEAR:      DATE_TRUNC('year', CURRENT_DATE)

-- Intervals
LAST_7_DAYS:    CURRENT_DATE - INTERVAL '7 days'
LAST_30_DAYS:   CURRENT_DATE - INTERVAL '30 days'
LAST_YEAR:      CURRENT_DATE - INTERVAL '1 year'

-- Date parts
YEAR:           EXTRACT(YEAR FROM date_column)
MONTH:          EXTRACT(MONTH FROM date_column)
DAY:            EXTRACT(DAY FROM date_column)
WEEKDAY:        EXTRACT(DOW FROM date_column)
QUARTER:        EXTRACT(QUARTER FROM date_column)
""", language="sql")
    
    with col2:
        st.subheader("🔤 String Functions")
        st.code("""
-- Concatenation
str1 || str2                    -- Standard
CONCAT(str1, str2)              -- MySQL
str1 + str2                     -- SQL Server

-- Case
UPPER(str)      -- UPPERCASE
LOWER(str)      -- lowercase
INITCAP(str)    -- Title Case

-- Trimming
TRIM(str)                       -- Both sides
LTRIM(str)                      -- Left only
RTRIM(str)                      -- Right only
TRIM(LEADING '0' FROM str)      -- Specific char

-- Substring
SUBSTRING(str, start, length)
LEFT(str, n)                    -- First n chars
RIGHT(str, n)                   -- Last n chars

-- Search
POSITION(substr IN str)         -- Position
REPLACE(str, old, new)          -- Replace
LENGTH(str)                     -- Length
""", language="sql")
    
    with col3:
        st.subheader("🔢 NULL Handling")
        st.code("""
-- NULL checks
IS NULL
IS NOT NULL

-- NULL replacement
COALESCE(col1, col2, 'default')   -- First non-null
NULLIF(col1, col2)                 -- NULL if equal
IFNULL(col, 'default')             -- MySQL
ISNULL(col, 'default')             -- SQL Server
NVL(col, 'default')                -- Oracle

-- NULL in aggregates
COUNT(*)           -- Counts all rows
COUNT(column)      -- Counts non-NULL only
SUM(column)        -- Ignores NULL
AVG(column)        -- Ignores NULL

-- NULL arithmetic
NULL + 1 = NULL
NULL * 0 = NULL
NULL / 1 = NULL

-- NULL comparisons
NULL = NULL  → NULL (not TRUE!)
NULL != NULL → NULL (not TRUE!)
""", language="sql")

# Data Type Reference
st.markdown("---")
st.header("📊 Data Type Reference")

data_types_col1, data_types_col2 = st.columns(2)

with data_types_col1:
    st.subheader("Common Data Types")
    st.markdown("""
    | Type | PostgreSQL | MySQL | Oracle | SQL Server |
    |------|------------|-------|--------|------------|
    | **Integer** | INTEGER, BIGINT | INT, BIGINT | NUMBER(10) | INT, BIGINT |
    | **Decimal** | NUMERIC(p,s) | DECIMAL(p,s) | NUMBER(p,s) | DECIMAL(p,s) |
    | **String** | VARCHAR(n), TEXT | VARCHAR(n), TEXT | VARCHAR2(n) | VARCHAR(n) |
    | **Date** | DATE | DATE | DATE | DATE |
    | **DateTime** | TIMESTAMP | DATETIME | TIMESTAMP | DATETIME |
    | **Boolean** | BOOLEAN | TINYINT(1) | NUMBER(1) | BIT |
    | **JSON** | JSON, JSONB | JSON | CLOB + constraint | NVARCHAR(MAX) |
    """)

with data_types_col2:
    st.subheader("Type Conversion")
    st.code("""
-- Standard CAST
CAST(column AS INTEGER)
CAST(column AS VARCHAR(50))
CAST(column AS DATE)

-- PostgreSQL shortcuts
column::INTEGER
column::TEXT
column::DATE

-- Conversion functions
TO_CHAR(date, 'YYYY-MM-DD')     -- Date to string
TO_DATE(string, 'YYYY-MM-DD')    -- String to date
TO_NUMBER(string)                -- String to number

-- MySQL conversions
CONVERT(column, UNSIGNED)
STR_TO_DATE(string, '%Y-%m-%d')
DATE_FORMAT(date, '%Y-%m-%d')
""", language="sql")

# Performance Tips
st.markdown("---")
st.header("⚡ Performance Quick Tips")

perf_col1, perf_col2 = st.columns(2)

with perf_col1:
    st.markdown("""
    **Query Optimization**
    - ✅ Use specific column names, not SELECT *
    - ✅ Filter early with WHERE clauses
    - ✅ Use appropriate indexes
    - ✅ Avoid functions in WHERE clauses
    - ✅ Use EXISTS instead of IN for subqueries
    - ✅ UNION ALL is faster than UNION
    
    **Index Best Practices**
    - Create indexes on JOIN columns
    - Create indexes on WHERE columns
    - Consider composite indexes for multiple columns
    - Don't over-index (slows INSERT/UPDATE)
    """)

with perf_col2:
    st.markdown("""
    **Common Anti-patterns**
    - ❌ SELECT * in production code
    - ❌ Implicit type conversions
    - ❌ OR conditions (use UNION instead)
    - ❌ NOT IN with nullable columns
    - ❌ Correlated subqueries in SELECT
    - ❌ Missing JOIN conditions (cartesian)
    
    **Monitoring Commands**
    ```sql
    -- PostgreSQL
    EXPLAIN (ANALYZE, BUFFERS) query;
    
    -- MySQL
    EXPLAIN query;
    SHOW PROFILES;
    ```
    """)

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use the search box to quickly find specific SQL commands and examples!")