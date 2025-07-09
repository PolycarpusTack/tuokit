"""
TuoKit - Snowflake Cheat Sheet
A comprehensive reference guide for Snowflake SQL commands and best practices
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="Snowflake Cheat Sheet - TuoKit",
    page_icon="❄️",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="snowflake_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        ❄️ Snowflake Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Quick reference for Snowflake SQL commands, functions, and best practices
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search Snowflake commands...", placeholder="Try: CREATE TABLE, COPY INTO, WAREHOUSE...")

# Create tabs for different categories
tab1, tab2, tab3, tab4, tab5, tab6, tab7, tab8 = st.tabs([
    "📊 Data Definition", 
    "📝 Data Manipulation", 
    "🏢 Warehouses", 
    "🔐 Security", 
    "⚡ Performance", 
    "🔄 Data Loading",
    "📈 Analytics",
    "💡 Best Practices"
])

# Helper function to display code with description
def show_command(title, description, code, show=True):
    """Display a command with description and code"""
    if show:
        st.markdown(f"### {title}")
        st.caption(description)
        st.code(code, language="sql")
        st.markdown("---")

# Helper function to filter by search
def matches_search(text, query):
    """Check if text matches search query"""
    if not query:
        return True
    return query.lower() in text.lower()

# Data Definition Language (DDL)
with tab1:
    st.header("📊 Data Definition Language (DDL)")
    
    # Create Database
    show_command(
        "Create Database",
        "Create a new database with optional parameters",
        """-- Basic database creation
CREATE DATABASE my_database;

-- With data retention
CREATE DATABASE my_database
    DATA_RETENTION_TIME_IN_DAYS = 30
    COMMENT = 'Production database';

-- Create or replace
CREATE OR REPLACE DATABASE my_database;""",
        matches_search("CREATE DATABASE", search_query)
    )
    
    # Create Schema
    show_command(
        "Create Schema",
        "Create schemas within a database",
        """-- Basic schema
CREATE SCHEMA my_schema;

-- With managed access
CREATE SCHEMA my_schema WITH MANAGED ACCESS;

-- Transient schema (no fail-safe)
CREATE TRANSIENT SCHEMA temp_schema
    DATA_RETENTION_TIME_IN_DAYS = 1;""",
        matches_search("CREATE SCHEMA", search_query)
    )
    
    # Create Table
    show_command(
        "Create Table",
        "Create tables with various options",
        """-- Standard table
CREATE TABLE customers (
    customer_id NUMBER AUTOINCREMENT,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE,
    created_at TIMESTAMP_NTZ DEFAULT CURRENT_TIMESTAMP(),
    PRIMARY KEY (customer_id)
);

-- Table with clustering
CREATE TABLE sales (
    sale_date DATE,
    product_id NUMBER,
    amount NUMBER(10,2),
    region VARCHAR(50)
) CLUSTER BY (sale_date, region);

-- External table
CREATE EXTERNAL TABLE ext_logs (
    log_date DATE AS TO_DATE(SUBSTR(METADATA$FILENAME, 7, 10), 'YYYY-MM-DD'),
    log_data VARIANT AS (PARSE_JSON($1))
) 
LOCATION = @my_stage/logs/
FILE_FORMAT = (TYPE = JSON);

-- Table as select (CTAS)
CREATE TABLE summary AS
SELECT region, SUM(amount) as total_sales
FROM sales
GROUP BY region;""",
        matches_search("CREATE TABLE", search_query)
    )
    
    # Create View
    show_command(
        "Create View",
        "Create views for data abstraction",
        """-- Standard view
CREATE VIEW active_customers AS
SELECT * FROM customers
WHERE status = 'ACTIVE';

-- Secure view (hides definition)
CREATE SECURE VIEW customer_summary AS
SELECT 
    customer_id,
    name,
    COUNT(order_id) as order_count
FROM customers c
JOIN orders o ON c.customer_id = o.customer_id
GROUP BY 1, 2;

-- Materialized view
CREATE MATERIALIZED VIEW daily_sales_mv AS
SELECT 
    sale_date,
    SUM(amount) as daily_total,
    COUNT(*) as transaction_count
FROM sales
GROUP BY sale_date;""",
        matches_search("CREATE VIEW", search_query)
    )
    
    # Alter Table
    show_command(
        "Alter Table",
        "Modify existing table structure",
        """-- Add column
ALTER TABLE customers 
ADD COLUMN phone VARCHAR(20);

-- Modify column
ALTER TABLE customers 
MODIFY COLUMN email VARCHAR(500);

-- Rename column
ALTER TABLE customers 
RENAME COLUMN name TO customer_name;

-- Add constraint
ALTER TABLE orders 
ADD CONSTRAINT fk_customer 
FOREIGN KEY (customer_id) 
REFERENCES customers(customer_id);

-- Enable/disable clustering
ALTER TABLE sales 
SUSPEND RECLUSTER;

ALTER TABLE sales 
RESUME RECLUSTER;""",
        matches_search("ALTER TABLE", search_query)
    )

# Data Manipulation Language (DML)
with tab2:
    st.header("📝 Data Manipulation Language (DML)")
    
    # Insert
    show_command(
        "Insert Data",
        "Insert data into tables",
        """-- Single row insert
INSERT INTO customers (name, email) 
VALUES ('John Doe', 'john@example.com');

-- Multi-row insert
INSERT INTO customers (name, email) VALUES
    ('Jane Smith', 'jane@example.com'),
    ('Bob Johnson', 'bob@example.com'),
    ('Alice Brown', 'alice@example.com');

-- Insert from select
INSERT INTO customer_archive
SELECT * FROM customers 
WHERE created_at < DATEADD(year, -2, CURRENT_DATE());

-- Insert overwrite (replace all data)
INSERT OVERWRITE INTO temp_data
SELECT * FROM source_data 
WHERE date = CURRENT_DATE();""",
        matches_search("INSERT", search_query)
    )
    
    # Update
    show_command(
        "Update Data",
        "Update existing records",
        """-- Basic update
UPDATE customers 
SET status = 'INACTIVE' 
WHERE last_login < DATEADD(day, -90, CURRENT_DATE());

-- Update with join
UPDATE orders o
SET o.customer_name = c.name
FROM customers c
WHERE o.customer_id = c.customer_id;

-- Conditional update
UPDATE products
SET 
    price = CASE 
        WHEN category = 'PREMIUM' THEN price * 1.10
        WHEN category = 'CLEARANCE' THEN price * 0.70
        ELSE price
    END,
    updated_at = CURRENT_TIMESTAMP();""",
        matches_search("UPDATE", search_query)
    )
    
    # Delete
    show_command(
        "Delete Data",
        "Remove records from tables",
        """-- Basic delete
DELETE FROM customers 
WHERE status = 'DELETED';

-- Delete with subquery
DELETE FROM orders
WHERE customer_id IN (
    SELECT customer_id 
    FROM customers 
    WHERE country = 'INVALID'
);

-- Delete duplicates
DELETE FROM customers c1
USING customers c2
WHERE c1.email = c2.email
AND c1.customer_id > c2.customer_id;""",
        matches_search("DELETE", search_query)
    )
    
    # Merge
    show_command(
        "Merge (Upsert)",
        "Insert or update based on match condition",
        """-- Basic merge
MERGE INTO target_table t
USING source_table s
ON t.id = s.id
WHEN MATCHED THEN
    UPDATE SET t.value = s.value, t.updated_at = CURRENT_TIMESTAMP()
WHEN NOT MATCHED THEN
    INSERT (id, value, created_at) 
    VALUES (s.id, s.value, CURRENT_TIMESTAMP());

-- Merge with delete
MERGE INTO inventory i
USING daily_changes c
ON i.product_id = c.product_id
WHEN MATCHED AND c.action = 'DELETE' THEN DELETE
WHEN MATCHED AND c.action = 'UPDATE' THEN
    UPDATE SET i.quantity = c.new_quantity
WHEN NOT MATCHED AND c.action = 'INSERT' THEN
    INSERT (product_id, quantity) VALUES (c.product_id, c.new_quantity);""",
        matches_search("MERGE", search_query)
    )

# Warehouses
with tab3:
    st.header("🏢 Virtual Warehouses")
    
    # Create Warehouse
    show_command(
        "Create Warehouse",
        "Create and configure virtual warehouses",
        """-- Basic warehouse
CREATE WAREHOUSE my_warehouse
    WAREHOUSE_SIZE = 'MEDIUM'
    AUTO_SUSPEND = 300
    AUTO_RESUME = TRUE
    INITIALLY_SUSPENDED = TRUE;

-- Multi-cluster warehouse
CREATE WAREHOUSE analytics_wh
    WAREHOUSE_SIZE = 'LARGE'
    MAX_CLUSTER_COUNT = 10
    MIN_CLUSTER_COUNT = 1
    SCALING_POLICY = 'STANDARD'
    AUTO_SUSPEND = 60
    AUTO_RESUME = TRUE;

-- Dedicated warehouse for specific workload
CREATE WAREHOUSE etl_warehouse
    WAREHOUSE_SIZE = 'X-LARGE'
    MAX_CONCURRENCY_LEVEL = 1
    STATEMENT_QUEUED_TIMEOUT_IN_SECONDS = 0
    COMMENT = 'Dedicated for ETL processes';""",
        matches_search("CREATE WAREHOUSE", search_query)
    )
    
    # Manage Warehouses
    show_command(
        "Manage Warehouses",
        "Start, stop, and modify warehouses",
        """-- Resume/suspend warehouse
ALTER WAREHOUSE my_warehouse RESUME;
ALTER WAREHOUSE my_warehouse SUSPEND;

-- Resize warehouse
ALTER WAREHOUSE my_warehouse 
SET WAREHOUSE_SIZE = 'LARGE';

-- Modify auto-suspend
ALTER WAREHOUSE my_warehouse 
SET AUTO_SUSPEND = 60;

-- Change clustering
ALTER WAREHOUSE analytics_wh 
SET MAX_CLUSTER_COUNT = 20;

-- Monitor warehouse
SHOW WAREHOUSES LIKE 'my_%';

-- Warehouse usage
SELECT * FROM TABLE(INFORMATION_SCHEMA.WAREHOUSE_LOAD_HISTORY(
    DATE_RANGE_START=>DATEADD('hour',-1,CURRENT_TIMESTAMP()),
    WAREHOUSE_NAME=>'MY_WAREHOUSE'
));""",
        matches_search("WAREHOUSE", search_query)
    )

# Security
with tab4:
    st.header("🔐 Security & Access Control")
    
    # Users and Roles
    show_command(
        "Users and Roles",
        "Manage users and role-based access",
        """-- Create user
CREATE USER john_doe
    PASSWORD = 'StrongPassword123!'
    DEFAULT_ROLE = 'ANALYST'
    DEFAULT_WAREHOUSE = 'ANALYTICS_WH'
    MUST_CHANGE_PASSWORD = TRUE;

-- Create custom role
CREATE ROLE data_engineer;
CREATE ROLE analyst;

-- Grant roles to users
GRANT ROLE analyst TO USER john_doe;
GRANT ROLE data_engineer TO ROLE analyst; -- Role hierarchy

-- Database privileges
GRANT USAGE ON DATABASE my_db TO ROLE analyst;
GRANT CREATE SCHEMA ON DATABASE my_db TO ROLE data_engineer;

-- Schema privileges
GRANT USAGE ON SCHEMA my_db.public TO ROLE analyst;
GRANT ALL ON SCHEMA my_db.etl TO ROLE data_engineer;

-- Table privileges
GRANT SELECT ON ALL TABLES IN SCHEMA my_db.public TO ROLE analyst;
GRANT INSERT, UPDATE, DELETE ON TABLE my_db.public.customers TO ROLE data_engineer;

-- Future grants (automatic for new objects)
GRANT SELECT ON FUTURE TABLES IN SCHEMA my_db.public TO ROLE analyst;""",
        matches_search("GRANT ROLE USER SECURITY", search_query)
    )
    
    # Row Access Policies
    show_command(
        "Row Access Policies",
        "Implement row-level security",
        """-- Create row access policy
CREATE ROW ACCESS POLICY customer_policy
AS (customer_region VARCHAR) 
RETURNS BOOLEAN ->
    CASE
        WHEN CURRENT_ROLE() IN ('ADMIN') THEN TRUE
        WHEN CURRENT_ROLE() IN ('REGIONAL_MANAGER') THEN 
            customer_region = CURRENT_USER_REGION()
        ELSE FALSE
    END;

-- Apply policy to table
ALTER TABLE customers 
ADD ROW ACCESS POLICY customer_policy 
ON (region);

-- Create dynamic data masking
CREATE MASKING POLICY email_mask AS (val STRING) 
RETURNS STRING ->
    CASE
        WHEN CURRENT_ROLE() IN ('ADMIN', 'SUPPORT') THEN val
        ELSE CONCAT(LEFT(val, 2), '*****', '@', SPLIT_PART(val, '@', 2))
    END;

-- Apply masking policy
ALTER TABLE customers 
MODIFY COLUMN email 
SET MASKING POLICY email_mask;""",
        matches_search("ROW ACCESS POLICY MASKING SECURITY", search_query)
    )

# Performance
with tab5:
    st.header("⚡ Performance Optimization")
    
    # Query Optimization
    show_command(
        "Query Optimization Tips",
        "Best practices for query performance",
        """-- Use result caching
-- Results are cached for 24 hours
SELECT /* RESULT_CACHE=TRUE */ 
    COUNT(*) 
FROM large_table;

-- Clustering for better partition pruning
-- Check clustering information
SELECT SYSTEM$CLUSTERING_INFORMATION('sales');

-- Optimize with CTEs
WITH monthly_totals AS (
    SELECT 
        DATE_TRUNC('month', sale_date) as month,
        SUM(amount) as total
    FROM sales
    WHERE sale_date >= DATEADD('year', -1, CURRENT_DATE())
    GROUP BY 1
)
SELECT * FROM monthly_totals
ORDER BY month;

-- Use SEARCH OPTIMIZATION
ALTER TABLE large_table 
ADD SEARCH OPTIMIZATION;

-- Query profile analysis
-- Run this after your query
SELECT * FROM TABLE(GET_QUERY_OPERATOR_STATS(LAST_QUERY_ID()));

-- Avoid SELECT *
-- Be specific with columns
SELECT customer_id, name, email  -- Good
FROM customers
-- Instead of SELECT * FROM customers  -- Bad""",
        matches_search("OPTIMIZATION PERFORMANCE CLUSTERING", search_query)
    )
    
    # Materialized Views
    show_command(
        "Materialized Views",
        "Pre-computed results for better performance",
        """-- Create materialized view
CREATE MATERIALIZED VIEW sales_summary_mv AS
SELECT 
    DATE_TRUNC('day', sale_date) as sale_day,
    region,
    product_category,
    SUM(amount) as daily_sales,
    COUNT(*) as transaction_count,
    AVG(amount) as avg_sale
FROM sales
GROUP BY 1, 2, 3;

-- Monitor materialized view
SHOW MATERIALIZED VIEWS;

-- Check refresh history
SELECT * 
FROM TABLE(INFORMATION_SCHEMA.MATERIALIZED_VIEW_REFRESH_HISTORY(
    DATE_RANGE_START => DATEADD('day', -7, CURRENT_DATE())
));

-- Manual refresh
ALTER MATERIALIZED VIEW sales_summary_mv REFRESH;""",
        matches_search("MATERIALIZED VIEW PERFORMANCE", search_query)
    )

# Data Loading
with tab6:
    st.header("🔄 Data Loading & Unloading")
    
    # COPY INTO
    show_command(
        "COPY INTO (Loading)",
        "Load data from stages into tables",
        """-- Load from internal stage
COPY INTO customers
FROM @my_stage/customers/
FILE_FORMAT = (TYPE = CSV SKIP_HEADER = 1)
ON_ERROR = 'CONTINUE';

-- Load from S3
COPY INTO sales
FROM 's3://mybucket/sales/'
CREDENTIALS = (AWS_KEY_ID='xxx' AWS_SECRET_KEY='yyy')
FILE_FORMAT = (TYPE = PARQUET)
PATTERN = '.*sales_2024.*\.parquet';

-- Load with transformation
COPY INTO orders (order_id, customer_id, amount, order_date)
FROM (
    SELECT 
        $1::NUMBER,
        $2::NUMBER,
        $3::NUMBER(10,2),
        TO_DATE($4, 'MM/DD/YYYY')
    FROM @my_stage/orders/
)
FILE_FORMAT = (TYPE = CSV);

-- Load JSON with parsing
COPY INTO events
FROM @my_stage/events/
FILE_FORMAT = (TYPE = JSON)
MATCH_BY_COLUMN_NAME = CASE_INSENSITIVE;

-- Validation mode (dry run)
COPY INTO customers
FROM @my_stage/customers/
FILE_FORMAT = (TYPE = CSV)
VALIDATION_MODE = 'RETURN_ERRORS';""",
        matches_search("COPY INTO LOAD", search_query)
    )
    
    # COPY INTO (Unloading)
    show_command(
        "COPY INTO (Unloading)",
        "Export data from tables to stages",
        """-- Unload to internal stage
COPY INTO @my_stage/exports/customers_
FROM customers
FILE_FORMAT = (TYPE = CSV COMPRESSION = GZIP)
SINGLE = FALSE
MAX_FILE_SIZE = 104857600;  -- 100MB

-- Unload to S3
COPY INTO 's3://mybucket/exports/'
FROM (
    SELECT * FROM sales 
    WHERE sale_date = CURRENT_DATE()
)
CREDENTIALS = (AWS_KEY_ID='xxx' AWS_SECRET_KEY='yyy')
FILE_FORMAT = (TYPE = PARQUET)
HEADER = TRUE;

-- Unload with partitioning
COPY INTO @my_stage/sales/
FROM sales
FILE_FORMAT = (TYPE = PARQUET)
PARTITION BY ('year=' || YEAR(sale_date) || '/month=' || MONTH(sale_date))
INCLUDE_QUERY_ID = TRUE;""",
        matches_search("COPY INTO UNLOAD EXPORT", search_query)
    )
    
    # Stages
    show_command(
        "Stages",
        "Create and manage stages for data loading",
        """-- Create internal stage
CREATE STAGE my_stage
    ENCRYPTION = (TYPE = 'SNOWFLAKE_SSE')
    DIRECTORY = (ENABLE = TRUE);

-- Create external stage (S3)
CREATE STAGE s3_stage
    URL = 's3://mybucket/data/'
    CREDENTIALS = (AWS_KEY_ID='xxx' AWS_SECRET_KEY='yyy')
    FILE_FORMAT = (TYPE = PARQUET);

-- Create external stage (Azure)
CREATE STAGE azure_stage
    URL = 'azure://myaccount.blob.core.windows.net/mycontainer'
    CREDENTIALS = (AZURE_SAS_TOKEN='xxx')
    FILE_FORMAT = (TYPE = CSV);

-- List files in stage
LIST @my_stage/path/;

-- Remove files from stage
REMOVE @my_stage/old_files/;

-- Upload files (using SnowSQL)
PUT file:///local/path/data.csv @my_stage/;""",
        matches_search("STAGE CREATE", search_query)
    )

# Analytics Functions
with tab7:
    st.header("📈 Analytics & Window Functions")
    
    # Window Functions
    show_command(
        "Window Functions",
        "Advanced analytics with window functions",
        """-- Ranking functions
SELECT 
    customer_id,
    order_date,
    amount,
    ROW_NUMBER() OVER (PARTITION BY customer_id ORDER BY order_date) as order_num,
    RANK() OVER (PARTITION BY customer_id ORDER BY amount DESC) as amount_rank,
    DENSE_RANK() OVER (ORDER BY amount DESC) as global_rank,
    PERCENT_RANK() OVER (PARTITION BY customer_id ORDER BY amount) as pct_rank
FROM orders;

-- Moving aggregates
SELECT 
    sale_date,
    amount,
    AVG(amount) OVER (ORDER BY sale_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) as moving_avg_7d,
    SUM(amount) OVER (ORDER BY sale_date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) as running_total,
    MAX(amount) OVER (PARTITION BY MONTH(sale_date) ORDER BY sale_date) as month_max_to_date
FROM sales;

-- Lead/Lag functions
SELECT 
    customer_id,
    order_date,
    amount,
    LAG(amount, 1) OVER (PARTITION BY customer_id ORDER BY order_date) as prev_order_amount,
    LEAD(order_date, 1) OVER (PARTITION BY customer_id ORDER BY order_date) as next_order_date,
    amount - LAG(amount, 1, 0) OVER (PARTITION BY customer_id ORDER BY order_date) as amount_change
FROM orders;

-- First/Last value
SELECT 
    department,
    employee_name,
    salary,
    FIRST_VALUE(employee_name) OVER (PARTITION BY department ORDER BY salary DESC) as highest_paid,
    LAST_VALUE(employee_name) OVER (PARTITION BY department ORDER BY salary DESC 
        ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) as lowest_paid
FROM employees;""",
        matches_search("WINDOW FUNCTION OVER PARTITION", search_query)
    )
    
    # Common Table Expressions
    show_command(
        "CTEs and Recursive Queries",
        "Complex queries with CTEs",
        """-- Basic CTE
WITH regional_sales AS (
    SELECT region, SUM(amount) as total_sales
    FROM sales
    GROUP BY region
),
top_regions AS (
    SELECT region
    FROM regional_sales
    WHERE total_sales > 1000000
)
SELECT s.*
FROM sales s
JOIN top_regions t ON s.region = t.region;

-- Multiple CTEs
WITH 
customer_metrics AS (
    SELECT 
        customer_id,
        COUNT(*) as order_count,
        SUM(amount) as total_spent,
        AVG(amount) as avg_order_value
    FROM orders
    GROUP BY customer_id
),
customer_segments AS (
    SELECT 
        customer_id,
        CASE 
            WHEN total_spent > 10000 THEN 'VIP'
            WHEN total_spent > 5000 THEN 'Premium'
            ELSE 'Standard'
        END as segment
    FROM customer_metrics
)
SELECT c.*, cs.segment
FROM customers c
JOIN customer_segments cs ON c.customer_id = cs.customer_id;

-- Recursive CTE (hierarchy)
WITH RECURSIVE employee_hierarchy AS (
    -- Anchor: top-level employees
    SELECT employee_id, name, manager_id, 1 as level
    FROM employees
    WHERE manager_id IS NULL
    
    UNION ALL
    
    -- Recursive: employees with managers
    SELECT e.employee_id, e.name, e.manager_id, h.level + 1
    FROM employees e
    JOIN employee_hierarchy h ON e.manager_id = h.employee_id
)
SELECT * FROM employee_hierarchy
ORDER BY level, name;""",
        matches_search("CTE WITH RECURSIVE", search_query)
    )
    
    # Advanced Analytics
    show_command(
        "Advanced Analytics Functions",
        "Statistical and analytical functions",
        """-- Statistical aggregates
SELECT 
    product_category,
    COUNT(*) as count,
    AVG(price) as avg_price,
    MEDIAN(price) as median_price,
    STDDEV(price) as std_dev,
    VARIANCE(price) as variance,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY price) as q1,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY price) as q3,
    MODE(price) as mode_price,
    APPROX_TOP_K(product_name, 5) as top_5_products
FROM products
GROUP BY product_category;

-- Time series functions
SELECT 
    DATE_TRUNC('hour', timestamp) as hour,
    COUNT(*) as events,
    -- Interpolate missing hours
    COUNT(*) OVER (ORDER BY DATE_TRUNC('hour', timestamp) 
        ROWS BETWEEN 23 PRECEDING AND CURRENT ROW) as rolling_24h_count
FROM events
GROUP BY 1;

-- Array and JSON analytics
SELECT 
    customer_id,
    ARRAY_AGG(DISTINCT product_id) as purchased_products,
    ARRAY_SIZE(purchased_products) as unique_product_count,
    ARRAY_INTERSECTION(
        ARRAY_AGG(product_id),
        (SELECT ARRAY_AGG(product_id) FROM featured_products)
    ) as featured_purchases
FROM orders
GROUP BY customer_id;""",
        matches_search("STATISTICAL ANALYTICS MEDIAN PERCENTILE", search_query)
    )

# Best Practices
with tab8:
    st.header("💡 Best Practices & Tips")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.subheader("🎯 Query Best Practices")
        st.markdown("""
        **1. Use Appropriate Warehouse Sizes**
        - X-Small: Dev/test workloads
        - Small/Medium: Regular queries
        - Large/X-Large: Complex analytics
        - 2X-Large+: Heavy ETL/ML workloads
        
        **2. Optimize for Clustering**
        - Cluster on frequently filtered columns
        - Use date columns for time-series data
        - Monitor clustering with SYSTEM$CLUSTERING_INFORMATION
        
        **3. Leverage Caching**
        - Result cache: 24-hour automatic caching
        - Warehouse cache: Suspend after inactivity
        - Metadata cache: Automatic
        
        **4. Partition Pruning**
        - Filter on clustered columns
        - Use date ranges effectively
        - Avoid functions on filter columns
        
        **5. Query Optimization**
        - Use CTEs for readability
        - Avoid SELECT * in production
        - Push filters down in subqueries
        - Use appropriate JOIN types
        """)
        
        st.subheader("🔧 Data Loading Best Practices")
        st.markdown("""
        **1. File Sizing**
        - Compress files (gzip, brotli)
        - Target 100-250 MB compressed
        - Split large files
        
        **2. File Formats**
        - Parquet: Best for analytics
        - CSV: Simple data exchange
        - JSON: Semi-structured data
        - Avro: Schema evolution
        
        **3. Error Handling**
        - Use ON_ERROR = 'CONTINUE'
        - Validate with VALIDATION_MODE
        - Monitor with COPY_HISTORY
        
        **4. Performance**
        - Use multiple files for parallelism
        - Match warehouse size to load
        - Consider Snowpipe for continuous loading
        """)
    
    with col2:
        st.subheader("🛡️ Security Best Practices")
        st.markdown("""
        **1. Access Control**
        - Use role-based access (RBAC)
        - Principle of least privilege
        - Regular access reviews
        - Separate roles by function
        
        **2. Data Protection**
        - Enable encryption at rest
        - Use secure views for sensitive data
        - Implement row access policies
        - Apply dynamic data masking
        
        **3. Network Security**
        - Use network policies
        - Enable MFA for users
        - Audit login attempts
        - Monitor suspicious activity
        
        **4. Compliance**
        - Enable object tagging
        - Track data lineage
        - Regular audit reviews
        - Document data governance
        """)
        
        st.subheader("💰 Cost Optimization")
        st.markdown("""
        **1. Warehouse Management**
        - Auto-suspend unused warehouses
        - Right-size for workload
        - Use multi-cluster for concurrency
        - Schedule heavy jobs off-peak
        
        **2. Storage Optimization**
        - Set appropriate retention
        - Use transient tables for temp data
        - Implement data lifecycle policies
        - Monitor storage growth
        
        **3. Query Efficiency**
        - Monitor long-running queries
        - Use resource monitors
        - Set statement timeouts
        - Review query history regularly
        
        **4. Data Sharing**
        - Use secure data sharing vs copying
        - Leverage reader accounts
        - Monitor consumer usage
        """)

# Quick Reference Card
st.markdown("---")
st.header("📋 Quick Reference Card")

col1, col2, col3 = st.columns(3)

with col1:
    st.markdown("""
    **Common Commands**
    ```sql
    -- Show current context
    SELECT CURRENT_DATABASE();
    SELECT CURRENT_SCHEMA();
    SELECT CURRENT_WAREHOUSE();
    SELECT CURRENT_ROLE();
    
    -- Use context
    USE DATABASE my_db;
    USE SCHEMA my_schema;
    USE WAREHOUSE my_wh;
    USE ROLE my_role;
    
    -- Show objects
    SHOW DATABASES;
    SHOW SCHEMAS;
    SHOW TABLES;
    SHOW VIEWS;
    ```
    """)

with col2:
    st.markdown("""
    **Date/Time Functions**
    ```sql
    -- Current date/time
    CURRENT_DATE()
    CURRENT_TIMESTAMP()
    
    -- Date arithmetic
    DATEADD(day, 7, date_col)
    DATEDIFF(day, start, end)
    DATE_TRUNC('month', date)
    
    -- Extract parts
    YEAR(date_col)
    MONTH(date_col)
    DAYOFWEEK(date_col)
    ```
    """)

with col3:
    st.markdown("""
    **String Functions**
    ```sql
    -- Manipulation
    UPPER(str)
    LOWER(str)
    TRIM(str)
    
    -- Substring
    SUBSTR(str, start, len)
    LEFT(str, n)
    RIGHT(str, n)
    
    -- Combine
    CONCAT(str1, str2)
    str1 || str2
    ```
    """)

# Resources
st.markdown("---")
st.header("📚 Additional Resources")

col1, col2 = st.columns(2)

with col1:
    st.markdown("""
    **Official Documentation**
    - [Snowflake Documentation](https://docs.snowflake.com)
    - [SQL Reference](https://docs.snowflake.com/en/sql-reference.html)
    - [Best Practices](https://docs.snowflake.com/en/user-guide/intro-best-practices.html)
    - [Tutorial](https://docs.snowflake.com/en/user-guide-getting-started.html)
    """)

with col2:
    st.markdown("""
    **Community Resources**
    - [Snowflake Community](https://community.snowflake.com)
    - [Stack Overflow](https://stackoverflow.com/questions/tagged/snowflake-cloud-data-platform)
    - [Reddit r/snowflake](https://www.reddit.com/r/snowflake/)
    - [YouTube Tutorials](https://www.youtube.com/c/SnowflakeInc)
    """)

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use Ctrl+F to search for specific commands on this page!")