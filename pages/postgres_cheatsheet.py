"""
TuoKit - PostgreSQL Cheat Sheet
Comprehensive PostgreSQL reference for developers, analysts, and DBAs
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="PostgreSQL Cheat Sheet - TuoKit",
    page_icon="🐘",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="postgres_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🐘 PostgreSQL Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        PostgreSQL commands, functions, and best practices for all roles
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search PostgreSQL topics...", placeholder="Try: JSONB, indexes, VACUUM, window functions...")

# Create role-based tabs
role_tab1, role_tab2, role_tab3, role_tab4 = st.tabs([
    "👨‍💻 Developer", 
    "📊 Analyst", 
    "🛠️ DBA/Support", 
    "📚 Reference"
])

# Helper function
def show_postgres_example(title, description, code, show=True):
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
        "Basic SQL", "Data Types", "JSON/JSONB", "Arrays", "Advanced SQL", "Transactions", "Functions", "Performance"
    ])
    
    with dev_tabs[0]:  # Basic SQL
        st.subheader("📝 Basic SQL Operations")
        
        show_postgres_example(
            "Database and Table Operations",
            "Essential DDL commands for database management",
            """-- Database operations
CREATE DATABASE mydb
    WITH OWNER = myuser
    ENCODING = 'UTF8'
    LC_COLLATE = 'en_US.UTF-8'
    LC_CTYPE = 'en_US.UTF-8'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1;

DROP DATABASE IF EXISTS mydb;

-- Create table with constraints
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    username VARCHAR(50) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT true,
    age INTEGER CHECK (age >= 18),
    CONSTRAINT unique_email_username UNIQUE (email, username)
);

-- Create table with foreign key
CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    title VARCHAR(200) NOT NULL,
    content TEXT,
    published_at TIMESTAMPTZ,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- Alter table operations
ALTER TABLE users ADD COLUMN phone VARCHAR(20);
ALTER TABLE users DROP COLUMN phone;
ALTER TABLE users ALTER COLUMN email TYPE VARCHAR(500);
ALTER TABLE users RENAME COLUMN username TO user_name;
ALTER TABLE users ADD CONSTRAINT check_age CHECK (age >= 18 AND age <= 120);
ALTER TABLE users DROP CONSTRAINT check_age;

-- Create indexes
CREATE INDEX idx_users_email ON users(email);
CREATE UNIQUE INDEX idx_users_username ON users(user_name);
CREATE INDEX idx_posts_user_published ON posts(user_id, published_at DESC);
CREATE INDEX idx_users_email_partial ON users(email) WHERE is_active = true;

-- Drop objects
DROP TABLE IF EXISTS posts CASCADE;
DROP INDEX IF EXISTS idx_users_email;""",
            matches_search("CREATE TABLE ALTER INDEX constraint foreign key", search_query)
        )
        
        show_postgres_example(
            "Data Manipulation (DML)",
            "INSERT, UPDATE, DELETE operations",
            """-- INSERT operations
-- Single row insert
INSERT INTO users (email, username, password_hash)
VALUES ('user@example.com', 'john_doe', 'hashed_password');

-- Multiple rows insert
INSERT INTO users (email, username, password_hash) VALUES
    ('alice@example.com', 'alice', 'hash1'),
    ('bob@example.com', 'bob', 'hash2'),
    ('charlie@example.com', 'charlie', 'hash3');

-- Insert with RETURNING
INSERT INTO users (email, username, password_hash)
VALUES ('new@example.com', 'newuser', 'hash')
RETURNING id, created_at;

-- Insert from SELECT
INSERT INTO archived_users (id, email, username)
SELECT id, email, username FROM users
WHERE created_at < CURRENT_DATE - INTERVAL '2 years';

-- UPDATE operations
-- Simple update
UPDATE users
SET is_active = false
WHERE last_login < CURRENT_DATE - INTERVAL '6 months';

-- Update with multiple columns
UPDATE users
SET email = LOWER(email),
    updated_at = CURRENT_TIMESTAMP
WHERE email != LOWER(email);

-- Update with JOIN
UPDATE posts p
SET published_at = CURRENT_TIMESTAMP
FROM users u
WHERE p.user_id = u.id
AND u.is_active = true
AND p.published_at IS NULL;

-- Update with RETURNING
UPDATE users
SET age = age + 1
WHERE DATE_PART('month', created_at) = DATE_PART('month', CURRENT_DATE)
AND DATE_PART('day', created_at) = DATE_PART('day', CURRENT_DATE)
RETURNING id, username, age;

-- DELETE operations
-- Simple delete
DELETE FROM users
WHERE is_active = false
AND last_login < CURRENT_DATE - INTERVAL '1 year';

-- Delete with JOIN
DELETE FROM posts p
USING users u
WHERE p.user_id = u.id
AND u.is_active = false;

-- Delete with RETURNING
DELETE FROM users
WHERE email LIKE '%@oldomain.com'
RETURNING *;

-- TRUNCATE (faster than DELETE for all rows)
TRUNCATE TABLE posts RESTART IDENTITY CASCADE;""",
            matches_search("INSERT UPDATE DELETE TRUNCATE RETURNING DML", search_query)
        )
        
        show_postgres_example(
            "Basic Queries",
            "SELECT operations and filtering",
            """-- Basic SELECT
SELECT * FROM users;
SELECT id, email, username FROM users;
SELECT DISTINCT is_active FROM users;

-- WHERE conditions
SELECT * FROM users
WHERE is_active = true
AND age >= 21
AND email LIKE '%@gmail.com'
AND created_at BETWEEN '2024-01-01' AND '2024-12-31';

-- IN and NOT IN
SELECT * FROM posts
WHERE user_id IN (1, 2, 3, 4, 5);

SELECT * FROM users
WHERE email NOT IN (
    SELECT email FROM blacklisted_emails
);

-- NULL handling
SELECT * FROM posts
WHERE published_at IS NULL;

SELECT * FROM users
WHERE phone IS NOT NULL;

-- ORDER BY
SELECT * FROM users
ORDER BY created_at DESC, username ASC;

-- LIMIT and OFFSET
SELECT * FROM posts
ORDER BY published_at DESC
LIMIT 10 OFFSET 20;  -- Page 3 with 10 items per page

-- GROUP BY and HAVING
SELECT user_id, COUNT(*) as post_count, 
       MAX(published_at) as latest_post
FROM posts
WHERE published_at IS NOT NULL
GROUP BY user_id
HAVING COUNT(*) >= 5
ORDER BY post_count DESC;

-- Aggregate functions
SELECT 
    COUNT(*) as total_users,
    COUNT(DISTINCT email) as unique_emails,
    AVG(age) as average_age,
    MIN(created_at) as first_user,
    MAX(created_at) as latest_user,
    SUM(CASE WHEN is_active THEN 1 ELSE 0 END) as active_users
FROM users;

-- CASE expressions
SELECT 
    username,
    CASE 
        WHEN age < 18 THEN 'Minor'
        WHEN age BETWEEN 18 AND 65 THEN 'Adult'
        ELSE 'Senior'
    END as age_group,
    CASE is_active
        WHEN true THEN 'Active'
        WHEN false THEN 'Inactive'
    END as status
FROM users;""",
            matches_search("SELECT WHERE GROUP BY HAVING ORDER LIMIT aggregate", search_query)
        )
        
        show_postgres_example(
            "Joins and Set Operations",
            "Combining data from multiple tables",
            """-- INNER JOIN
SELECT u.username, p.title, p.published_at
FROM users u
INNER JOIN posts p ON u.id = p.user_id
WHERE p.published_at IS NOT NULL;

-- LEFT JOIN (LEFT OUTER JOIN)
SELECT u.username, COUNT(p.id) as post_count
FROM users u
LEFT JOIN posts p ON u.id = p.user_id
GROUP BY u.id, u.username;

-- RIGHT JOIN
SELECT u.username, p.title
FROM posts p
RIGHT JOIN users u ON p.user_id = u.id;

-- FULL OUTER JOIN
SELECT u.username, p.title
FROM users u
FULL OUTER JOIN posts p ON u.id = p.user_id
WHERE u.username IS NULL OR p.title IS NULL;

-- CROSS JOIN
SELECT u.username, c.category_name
FROM users u
CROSS JOIN categories c;

-- Self JOIN
SELECT e1.name as employee, e2.name as manager
FROM employees e1
LEFT JOIN employees e2 ON e1.manager_id = e2.id;

-- Multiple JOINs
SELECT 
    u.username,
    p.title,
    c.comment_text,
    c.created_at as comment_date
FROM users u
INNER JOIN posts p ON u.id = p.user_id
INNER JOIN comments c ON p.id = c.post_id
WHERE u.is_active = true
ORDER BY c.created_at DESC;

-- UNION operations
-- UNION (removes duplicates)
SELECT email FROM users
UNION
SELECT email FROM newsletter_subscribers;

-- UNION ALL (keeps duplicates)
SELECT 'User' as type, email FROM users
UNION ALL
SELECT 'Subscriber' as type, email FROM newsletter_subscribers;

-- INTERSECT
SELECT email FROM users
INTERSECT
SELECT email FROM premium_members;

-- EXCEPT (MINUS in other databases)
SELECT email FROM users
EXCEPT
SELECT email FROM banned_emails;""",
            matches_search("JOIN INNER LEFT RIGHT FULL UNION INTERSECT EXCEPT", search_query)
        )
        
    with dev_tabs[1]:  # Data Types
        st.subheader("📊 PostgreSQL Data Types")
        
        show_postgres_example(
            "Comprehensive Data Types",
            "PostgreSQL's rich type system",
            """-- Numeric types
CREATE TABLE numeric_examples (
    id SERIAL PRIMARY KEY,              -- Auto-incrementing integer
    small_int SMALLINT,                 -- -32768 to +32767
    normal_int INTEGER,                 -- -2147483648 to +2147483647
    big_int BIGINT,                     -- -9223372036854775808 to +9223372036854775807
    decimal_num DECIMAL(10,2),          -- Exact numeric with precision
    numeric_val NUMERIC(15,4),          -- Same as DECIMAL
    real_num REAL,                      -- 6 decimal digits precision
    double_num DOUBLE PRECISION,        -- 15 decimal digits precision
    serial_id SERIAL,                   -- Auto-incrementing int
    big_serial_id BIGSERIAL            -- Auto-incrementing bigint
);

-- String types
CREATE TABLE string_examples (
    id SERIAL PRIMARY KEY,
    fixed_char CHAR(10),                -- Fixed-length, blank padded
    var_char VARCHAR(255),              -- Variable length with limit
    text_field TEXT,                    -- Variable unlimited length
    char_field "char",                  -- Single character
    binary_data BYTEA                   -- Binary data
);

-- Date/Time types
CREATE TABLE datetime_examples (
    id SERIAL PRIMARY KEY,
    date_only DATE,                     -- Date without time
    time_only TIME,                     -- Time without date
    time_with_tz TIME WITH TIME ZONE,   -- Time with timezone
    timestamp_no_tz TIMESTAMP,          -- Date and time without timezone
    timestamp_tz TIMESTAMPTZ,           -- Date and time with timezone
    interval_period INTERVAL            -- Time interval
);

-- Boolean and enumerated types
CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');
CREATE TABLE boolean_enum_examples (
    id SERIAL PRIMARY KEY,
    is_active BOOLEAN DEFAULT true,
    is_verified BOOL DEFAULT false,     -- Same as BOOLEAN
    current_mood mood
);

-- Network address types
CREATE TABLE network_examples (
    id SERIAL PRIMARY KEY,
    ip_address INET,                    -- IPv4 or IPv6 address
    network_cidr CIDR,                  -- IPv4 or IPv6 network
    mac_address MACADDR,                -- MAC address
    mac_address8 MACADDR8               -- EUI-64 MAC address
);

-- Geometric types
CREATE TABLE geometric_examples (
    id SERIAL PRIMARY KEY,
    location POINT,                     -- (x,y)
    line_segment LINE,                  -- Infinite line
    line_seg LSEG,                      -- Line segment
    box_area BOX,                       -- Rectangular box
    path_open PATH,                     -- Open path
    path_closed PATH,                   -- Closed path
    polygon_shape POLYGON,              -- Polygon
    circle_shape CIRCLE                 -- Circle
);

-- UUID type
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE uuid_example (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    reference_id UUID NOT NULL
);

-- Range types
CREATE TABLE range_examples (
    id SERIAL PRIMARY KEY,
    int_range INT4RANGE,                -- Integer range
    bigint_range INT8RANGE,             -- Bigint range
    numeric_range NUMRANGE,             -- Numeric range
    timestamp_range TSRANGE,            -- Timestamp range
    timestamptz_range TSTZRANGE,        -- Timestamptz range
    date_range DATERANGE                -- Date range
);

-- Domain types (custom constraints)
CREATE DOMAIN email AS VARCHAR(255)
    CHECK (VALUE ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$');

CREATE DOMAIN positive_integer AS INTEGER
    CHECK (VALUE > 0);

-- Composite types
CREATE TYPE address AS (
    street VARCHAR(100),
    city VARCHAR(50),
    state CHAR(2),
    zip VARCHAR(10)
);

CREATE TABLE customers (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    home_address address,
    work_address address
);""",
            matches_search("data types numeric string date boolean uuid range", search_query)
        )
        
    with dev_tabs[2]:  # JSON/JSONB
        st.subheader("📄 JSON & JSONB Operations")
        
        show_postgres_example(
            "JSONB Operations",
            "Working with JSON data in PostgreSQL",
            """-- Creating tables with JSON/JSONB
CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    details JSONB NOT NULL,
    metadata JSON,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Inserting JSON data
INSERT INTO products (name, details) VALUES
    ('Laptop', '{"brand": "Dell", "specs": {"cpu": "i7", "ram": "16GB", "storage": "512GB SSD"}, "price": 999.99}'),
    ('Phone', '{"brand": "Apple", "model": "iPhone 13", "colors": ["black", "white", "blue"], "price": 799}'),
    ('Tablet', '{"brand": "Samsung", "features": {"screen": "10.5 inch", "battery": "7040 mAh"}, "price": 449.99}');

-- Querying JSON data
-- Get specific field
SELECT name, details->>'brand' AS brand
FROM products;

-- Get nested field
SELECT name, details#>>'{specs,cpu}' AS cpu
FROM products
WHERE details ? 'specs';

-- Query with operators
SELECT name, details
FROM products
WHERE details @> '{"brand": "Dell"}';  -- Contains

SELECT name, details
FROM products
WHERE details ? 'colors';  -- Has key

SELECT name, details
FROM products
WHERE details ?& array['brand', 'price'];  -- Has all keys

SELECT name, details
FROM products
WHERE details ?| array['colors', 'features'];  -- Has any key

-- JSON array operations
SELECT name, jsonb_array_elements_text(details->'colors') AS color
FROM products
WHERE details ? 'colors';

-- Aggregating JSON
SELECT jsonb_agg(details) AS all_details
FROM products;

SELECT jsonb_object_agg(name, details) AS products_by_name
FROM products;

-- Updating JSON
-- Add or update field
UPDATE products
SET details = details || '{"warranty": "2 years"}'
WHERE name = 'Laptop';

-- Update nested field
UPDATE products
SET details = jsonb_set(details, '{specs,ram}', '"32GB"')
WHERE name = 'Laptop';

-- Remove field
UPDATE products
SET details = details - 'warranty'
WHERE name = 'Laptop';

-- Remove nested field
UPDATE products
SET details = details #- '{specs,storage}'
WHERE name = 'Laptop';

-- Building JSON
SELECT jsonb_build_object(
    'product_id', id,
    'product_name', name,
    'brand', details->>'brand',
    'price', (details->>'price')::numeric,
    'has_specs', details ? 'specs'
) AS product_summary
FROM products;

-- JSON path queries (PostgreSQL 12+)
SELECT name, jsonb_path_query(details, '$.specs.*') AS spec_values
FROM products;

SELECT name, details
FROM products
WHERE jsonb_path_exists(details, '$.specs.cpu ? (@ == "i7")');

-- Indexing JSON
CREATE INDEX idx_products_details_gin ON products USING gin (details);
CREATE INDEX idx_products_brand ON products ((details->>'brand'));
CREATE INDEX idx_products_price ON products (((details->>'price')::numeric));

-- Full text search on JSON
ALTER TABLE products ADD COLUMN search_vector tsvector
    GENERATED ALWAYS AS (to_tsvector('english', details::text)) STORED;

CREATE INDEX idx_products_search ON products USING gin (search_vector);

SELECT name, details
FROM products
WHERE search_vector @@ to_tsquery('english', 'Dell & laptop');""",
            matches_search("JSON JSONB operators path query index", search_query)
        )
        
    with dev_tabs[3]:  # Arrays
        st.subheader("📚 Array Operations")
        
        show_postgres_example(
            "PostgreSQL Arrays",
            "Working with array data types",
            """-- Creating tables with arrays
CREATE TABLE events (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    tags TEXT[],                        -- Text array
    attendee_ids INTEGER[],             -- Integer array
    schedule TIME[],                    -- Time array
    ratings NUMERIC(3,2)[],             -- Numeric array
    metadata JSONB[]                    -- JSONB array
);

-- Inserting arrays
INSERT INTO events (name, tags, attendee_ids, ratings) VALUES
    ('Tech Conference', ARRAY['technology', 'innovation', 'networking'], ARRAY[1,2,3,4,5], ARRAY[4.5, 4.8, 4.2]),
    ('Workshop', '{"education", "hands-on", "practical"}', '{10,20,30}', '{4.9,4.7}'),
    ('Meetup', ARRAY['social', 'casual']::TEXT[], ARRAY[100,200]::INTEGER[], ARRAY[4.0]::NUMERIC(3,2)[]);

-- Array operations
-- Access elements (1-based indexing)
SELECT name, tags[1] AS first_tag, tags[2] AS second_tag
FROM events;

-- Array slicing
SELECT name, tags[1:2] AS first_two_tags, tags[2:] AS tags_from_second
FROM events;

-- Array length
SELECT name, array_length(tags, 1) AS tag_count, cardinality(attendee_ids) AS attendee_count
FROM events;

-- Array contains
SELECT name, tags
FROM events
WHERE tags @> ARRAY['technology']::TEXT[];  -- Contains

SELECT name, tags
FROM events
WHERE ARRAY['social'] <@ tags;  -- Is contained by

SELECT name, tags
FROM events
WHERE tags && ARRAY['education', 'innovation'];  -- Overlaps

-- Unnesting arrays
SELECT e.name, unnest(e.tags) AS tag
FROM events e;

-- With ordinality (get position)
SELECT e.name, tag.item AS tag_name, tag.position
FROM events e, unnest(e.tags) WITH ORDINALITY AS tag(item, position);

-- Array aggregation
SELECT array_agg(DISTINCT tag) AS all_unique_tags
FROM events, unnest(tags) AS tag;

SELECT name, array_agg(rating ORDER BY rating DESC) AS sorted_ratings
FROM events, unnest(ratings) AS rating
GROUP BY name;

-- Array functions
SELECT 
    name,
    array_append(tags, 'new-tag') AS tags_with_new,
    array_prepend('first-tag', tags) AS tags_with_first,
    array_remove(tags, 'casual') AS tags_without_casual,
    array_replace(tags, 'social', 'community') AS tags_replaced
FROM events;

-- Array concatenation
SELECT 
    name,
    tags || ARRAY['additional', 'tags'] AS extended_tags,
    tags || '{more,tags}'::TEXT[] AS extended_tags2,
    array_cat(tags, ARRAY['extra']) AS concatenated_tags
FROM events;

-- Array to string and back
SELECT 
    name,
    array_to_string(tags, ', ') AS tags_string,
    string_to_array('one,two,three', ',') AS string_to_array_example
FROM events;

-- Multi-dimensional arrays
CREATE TABLE matrix_data (
    id SERIAL PRIMARY KEY,
    matrix INTEGER[][],
    cube INTEGER[][][]
);

INSERT INTO matrix_data (matrix) VALUES
    ('{{1,2,3},{4,5,6},{7,8,9}}'),
    (ARRAY[[10,20],[30,40]]);

-- ANY and ALL operators
SELECT name
FROM events
WHERE 4.5 = ANY(ratings);  -- Any rating equals 4.5

SELECT name
FROM events
WHERE 4.0 < ALL(ratings);  -- All ratings greater than 4.0

-- GIN index for arrays
CREATE INDEX idx_events_tags ON events USING gin (tags);
CREATE INDEX idx_events_attendees ON events USING gin (attendee_ids);

-- Querying with index
SELECT name FROM events WHERE tags @> ARRAY['technology'];""",
            matches_search("array unnest aggregate append contains GIN", search_query)
        )
        
    with dev_tabs[4]:  # Advanced SQL
        st.subheader("🚀 Advanced PostgreSQL Features")
        
        show_postgres_example(
            "CTEs and Recursive Queries",
            "Common Table Expressions and recursive patterns",
            """-- Basic CTE
WITH regional_sales AS (
    SELECT region, SUM(sales_amount) AS total_sales
    FROM sales
    WHERE sale_date >= CURRENT_DATE - INTERVAL '30 days'
    GROUP BY region
),
top_regions AS (
    SELECT region
    FROM regional_sales
    WHERE total_sales > 100000
)
SELECT s.*
FROM sales s
JOIN top_regions t ON s.region = t.region;

-- Multiple CTEs
WITH 
customer_orders AS (
    SELECT customer_id, COUNT(*) AS order_count, SUM(total) AS total_spent
    FROM orders
    GROUP BY customer_id
),
customer_categories AS (
    SELECT 
        customer_id,
        CASE 
            WHEN total_spent > 10000 THEN 'VIP'
            WHEN total_spent > 5000 THEN 'Premium'
            ELSE 'Regular'
        END AS category
    FROM customer_orders
)
SELECT c.*, cc.category
FROM customers c
JOIN customer_categories cc ON c.id = cc.customer_id;

-- Recursive CTE for hierarchical data
WITH RECURSIVE employee_hierarchy AS (
    -- Anchor member: top-level employees
    SELECT 
        employee_id,
        name,
        manager_id,
        1 AS level,
        name AS path,
        ARRAY[employee_id] AS path_array
    FROM employees
    WHERE manager_id IS NULL
    
    UNION ALL
    
    -- Recursive member
    SELECT 
        e.employee_id,
        e.name,
        e.manager_id,
        h.level + 1,
        h.path || ' > ' || e.name,
        h.path_array || e.employee_id
    FROM employees e
    JOIN employee_hierarchy h ON e.manager_id = h.employee_id
    WHERE NOT e.employee_id = ANY(h.path_array)  -- Prevent cycles
)
SELECT * FROM employee_hierarchy
ORDER BY path;

-- LATERAL joins
SELECT 
    c.name AS customer_name,
    c.created_at AS customer_since,
    recent_orders.*
FROM customers c
CROSS JOIN LATERAL (
    SELECT 
        COUNT(*) AS order_count,
        SUM(total) AS total_spent,
        MAX(order_date) AS last_order_date
    FROM orders o
    WHERE o.customer_id = c.id
    AND o.order_date >= CURRENT_DATE - INTERVAL '90 days'
) AS recent_orders
WHERE recent_orders.order_count > 0;

-- VALUES lists as tables
SELECT *
FROM (VALUES 
    (1, 'Active', true),
    (2, 'Inactive', false),
    (3, 'Pending', null)
) AS statuses(id, name, is_enabled);

-- INSERT with RETURNING
INSERT INTO products (name, price, category)
VALUES ('New Product', 29.99, 'Electronics')
RETURNING id, name, created_at;

-- UPDATE with FROM clause
UPDATE orders o
SET status = 'shipped',
    shipped_date = CURRENT_DATE
FROM (
    SELECT order_id
    FROM orders
    WHERE status = 'processing'
    AND created_at < CURRENT_DATE - INTERVAL '2 days'
    LIMIT 100
) AS orders_to_ship
WHERE o.order_id = orders_to_ship.order_id
RETURNING o.order_id;

-- UPSERT (INSERT ON CONFLICT)
INSERT INTO user_settings (user_id, setting_key, setting_value)
VALUES (123, 'theme', 'dark')
ON CONFLICT (user_id, setting_key) 
DO UPDATE SET 
    setting_value = EXCLUDED.setting_value,
    updated_at = CURRENT_TIMESTAMP;

-- DISTINCT ON
SELECT DISTINCT ON (customer_id) 
    customer_id,
    order_id,
    order_date,
    total
FROM orders
ORDER BY customer_id, order_date DESC;  -- Most recent order per customer

-- GROUPING SETS, ROLLUP, CUBE
SELECT 
    region,
    product_category,
    EXTRACT(YEAR FROM order_date) AS year,
    SUM(amount) AS total_sales,
    COUNT(*) AS order_count,
    GROUPING(region, product_category, EXTRACT(YEAR FROM order_date)) AS grouping_level
FROM sales
GROUP BY GROUPING SETS (
    (region, product_category, EXTRACT(YEAR FROM order_date)),
    (region, product_category),
    (region),
    ()
)
ORDER BY grouping_level, region, product_category, year;""",
            matches_search("CTE recursive LATERAL UPSERT RETURNING DISTINCT ON", search_query)
        )
        
    with dev_tabs[5]:  # Transactions
        st.subheader("🔄 Transaction Control")
        
        show_postgres_example(
            "Transaction Management",
            "ACID compliance and transaction control",
            """-- Basic transaction
BEGIN;  -- or START TRANSACTION
    INSERT INTO accounts (user_id, balance) VALUES (1, 1000.00);
    UPDATE accounts SET balance = balance - 100 WHERE user_id = 1;
    UPDATE accounts SET balance = balance + 100 WHERE user_id = 2;
COMMIT;

-- Transaction with rollback
BEGIN;
    DELETE FROM orders WHERE order_date < '2020-01-01';
    -- Oops, wrong date!
ROLLBACK;

-- Savepoints
BEGIN;
    INSERT INTO users (email, username) VALUES ('test1@example.com', 'test1');
    SAVEPOINT my_savepoint;
    
    INSERT INTO users (email, username) VALUES ('test2@example.com', 'test2');
    -- This might fail due to constraint
    
    ROLLBACK TO my_savepoint;
    -- Only the second insert is rolled back
    
    INSERT INTO users (email, username) VALUES ('test3@example.com', 'test3');
COMMIT;

-- Transaction isolation levels
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;  -- Dirty reads possible
SET TRANSACTION ISOLATION LEVEL READ COMMITTED;    -- Default
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;   -- Phantom reads possible
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;      -- Strictest

-- Read-only transactions
BEGIN READ ONLY;
    SELECT * FROM sensitive_data;
    -- Any write operation will fail
COMMIT;

-- Deferrable transactions
BEGIN DEFERRABLE;
    -- For read-only serializable transactions
    -- Waits until it can run without conflicts
    SELECT * FROM large_report_table;
COMMIT;

-- Advisory locks
-- Session-level lock
SELECT pg_advisory_lock(12345);
-- Do some work
SELECT pg_advisory_unlock(12345);

-- Transaction-level lock (auto-released at commit/rollback)
BEGIN;
    SELECT pg_advisory_xact_lock(12345);
    -- Do some work
COMMIT;  -- Lock automatically released

-- Lock timeout
SET lock_timeout = '2s';
BEGIN;
    -- If lock cannot be acquired in 2 seconds, transaction fails
    UPDATE heavily_used_table SET value = value + 1;
COMMIT;

-- Deadlock detection
-- PostgreSQL automatically detects and resolves deadlocks
-- One transaction will be aborted with error

-- Check current transactions
SELECT pid, age(clock_timestamp(), query_start), usename, query, state
FROM pg_stat_activity
WHERE query != '<IDLE>'
AND query NOT ILIKE '%pg_stat_activity%'
ORDER BY query_start;

-- Kill a long-running transaction
SELECT pg_cancel_backend(pid);  -- Gentle cancel
SELECT pg_terminate_backend(pid);  -- Force terminate""",
            matches_search("transaction BEGIN COMMIT ROLLBACK SAVEPOINT isolation lock", search_query)
        )
        
        show_postgres_example(
            "Concurrency Control",
            "Managing concurrent access and locking",
            """-- Row-level locking
-- FOR UPDATE - locks for update, blocks other transactions
SELECT * FROM accounts
WHERE user_id = 123
FOR UPDATE;

-- FOR UPDATE NOWAIT - fails immediately if locked
SELECT * FROM accounts
WHERE user_id = 123
FOR UPDATE NOWAIT;

-- FOR UPDATE SKIP LOCKED - skips locked rows
SELECT * FROM job_queue
WHERE status = 'pending'
ORDER BY created_at
LIMIT 1
FOR UPDATE SKIP LOCKED;

-- FOR SHARE - allows reading but not updating
SELECT * FROM accounts
WHERE user_id = 123
FOR SHARE;

-- Table-level locks
LOCK TABLE accounts IN ACCESS SHARE MODE;        -- Allows reads
LOCK TABLE accounts IN ROW SHARE MODE;           -- Allows reads and row locks
LOCK TABLE accounts IN ROW EXCLUSIVE MODE;       -- Allows reads, blocks writes
LOCK TABLE accounts IN SHARE MODE;               -- Blocks writes and row locks
LOCK TABLE accounts IN SHARE ROW EXCLUSIVE MODE; -- Blocks writes and row locks
LOCK TABLE accounts IN EXCLUSIVE MODE;           -- Blocks reads and writes
LOCK TABLE accounts IN ACCESS EXCLUSIVE MODE;    -- Blocks everything

-- Check locks
SELECT 
    locktype,
    database,
    relation::regclass,
    mode,
    granted,
    pid
FROM pg_locks
WHERE relation::regclass::text LIKE '%your_table%';

-- Optimistic locking pattern
-- Add version column to table
ALTER TABLE products ADD COLUMN version INTEGER DEFAULT 1;

-- Update with version check
UPDATE products
SET name = 'New Name',
    price = 29.99,
    version = version + 1
WHERE id = 123
AND version = 5;  -- Only updates if version hasn't changed

-- Check if update succeeded
GET DIAGNOSTICS rows_affected = ROW_COUNT;
IF rows_affected = 0 THEN
    RAISE EXCEPTION 'Product was modified by another user';
END IF;""",
            matches_search("locking FOR UPDATE SKIP LOCKED concurrency optimistic", search_query)
        )

# Analyst Tab
with role_tab2:
    st.header("📊 Analyst Toolkit")
    
    analyst_tabs = st.tabs([
        "Window Functions", "Time Series", "Statistics", "Pivot & Crosstab", "Export"
    ])
    
    with analyst_tabs[0]:  # Window Functions
        st.subheader("🪟 Window Functions")
        
        show_postgres_example(
            "Comprehensive Window Functions",
            "Advanced analytics with window functions",
            """-- Ranking functions
SELECT 
    employee_name,
    department,
    salary,
    -- Different ranking methods
    ROW_NUMBER() OVER (ORDER BY salary DESC) AS salary_row_num,
    RANK() OVER (ORDER BY salary DESC) AS salary_rank,
    DENSE_RANK() OVER (ORDER BY salary DESC) AS salary_dense_rank,
    PERCENT_RANK() OVER (ORDER BY salary DESC) AS salary_percent_rank,
    CUME_DIST() OVER (ORDER BY salary) AS cumulative_distribution,
    NTILE(4) OVER (ORDER BY salary DESC) AS salary_quartile,
    -- Department-specific ranking
    ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
    -- Running calculations
    SUM(salary) OVER (ORDER BY salary) AS running_total,
    AVG(salary) OVER (ORDER BY salary ROWS BETWEEN 3 PRECEDING AND CURRENT ROW) AS moving_avg_4,
    -- Comparison with department
    salary - AVG(salary) OVER (PARTITION BY department) AS diff_from_dept_avg,
    salary / SUM(salary) OVER () AS salary_percentage
FROM employees;

-- LAG and LEAD functions
WITH monthly_sales AS (
    SELECT 
        DATE_TRUNC('month', order_date) AS month,
        SUM(total) AS monthly_total
    FROM orders
    GROUP BY 1
)
SELECT 
    month,
    monthly_total,
    -- Previous period values
    LAG(monthly_total, 1) OVER (ORDER BY month) AS prev_month,
    LAG(monthly_total, 12) OVER (ORDER BY month) AS same_month_last_year,
    -- Next period values
    LEAD(monthly_total, 1) OVER (ORDER BY month) AS next_month,
    -- Growth calculations
    monthly_total - LAG(monthly_total, 1) OVER (ORDER BY month) AS month_over_month_change,
    ROUND((monthly_total - LAG(monthly_total, 1) OVER (ORDER BY month)) / 
          NULLIF(LAG(monthly_total, 1) OVER (ORDER BY month), 0) * 100, 2) AS mom_growth_rate,
    -- Multiple lags
    LAG(monthly_total, 1, 0) OVER (ORDER BY month) AS prev_month_default_0
FROM monthly_sales;

-- FIRST_VALUE and LAST_VALUE
SELECT 
    product_name,
    sale_date,
    quantity,
    price,
    -- First and last in partition
    FIRST_VALUE(price) OVER (PARTITION BY product_name ORDER BY sale_date) AS initial_price,
    LAST_VALUE(price) OVER (PARTITION BY product_name ORDER BY sale_date 
        ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS final_price,
    -- NTH_VALUE
    NTH_VALUE(price, 2) OVER (PARTITION BY product_name ORDER BY sale_date) AS second_price
FROM product_sales;

-- Frame clause examples
SELECT 
    transaction_date,
    amount,
    -- Different window frames
    SUM(amount) OVER (ORDER BY transaction_date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS cumulative_sum,
    AVG(amount) OVER (ORDER BY transaction_date ROWS BETWEEN 3 PRECEDING AND 3 FOLLOWING) AS centered_avg_7,
    MAX(amount) OVER (ORDER BY transaction_date RANGE BETWEEN INTERVAL '7 days' PRECEDING AND CURRENT ROW) AS max_last_7_days,
    COUNT(*) OVER (ORDER BY transaction_date RANGE BETWEEN INTERVAL '1 hour' PRECEDING AND INTERVAL '1 hour' FOLLOWING) AS transactions_2hr_window
FROM transactions;

-- Array aggregation with window functions
SELECT 
    customer_id,
    order_date,
    product_id,
    ARRAY_AGG(product_id) OVER (PARTITION BY customer_id ORDER BY order_date) AS products_purchased_so_far,
    STRING_AGG(product_id::TEXT, ',') OVER (PARTITION BY customer_id ORDER BY order_date) AS product_history
FROM orders;

-- Window function with FILTER clause
SELECT 
    department,
    COUNT(*) OVER (PARTITION BY department) AS dept_total,
    COUNT(*) FILTER (WHERE salary > 50000) OVER (PARTITION BY department) AS high_earners,
    AVG(salary) FILTER (WHERE years_experience > 5) OVER (PARTITION BY department) AS avg_salary_experienced
FROM employees;""",
            matches_search("window function rank lag lead frame FILTER", search_query)
        )
        
    with analyst_tabs[2]:  # Statistics
        st.subheader("📈 Statistical Functions")
        
        show_postgres_example(
            "Statistical Aggregates",
            "Advanced statistical analysis functions",
            """-- Basic statistical functions
SELECT 
    COUNT(*) AS total_count,
    COUNT(DISTINCT customer_id) AS unique_customers,
    AVG(amount) AS average_amount,
    MEDIAN(amount) AS median_amount,  -- Custom or use percentile_cont
    MODE() WITHIN GROUP (ORDER BY category) AS most_common_category,
    STDDEV(amount) AS standard_deviation,
    STDDEV_POP(amount) AS population_stddev,
    STDDEV_SAMP(amount) AS sample_stddev,
    VARIANCE(amount) AS variance,
    VAR_POP(amount) AS population_variance,
    VAR_SAMP(amount) AS sample_variance
FROM sales;

-- Correlation and regression
SELECT 
    CORR(price, quantity) AS price_quantity_correlation,
    COVAR_POP(price, quantity) AS population_covariance,
    COVAR_SAMP(price, quantity) AS sample_covariance,
    REGR_SLOPE(quantity, price) AS regression_slope,
    REGR_INTERCEPT(quantity, price) AS regression_intercept,
    REGR_R2(quantity, price) AS r_squared,
    REGR_COUNT(quantity, price) AS regression_count,
    REGR_AVGX(quantity, price) AS avg_independent,
    REGR_AVGY(quantity, price) AS avg_dependent
FROM sales_data;

-- Percentiles and quantiles
SELECT 
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY amount) AS median,
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY amount) AS q1,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY amount) AS q3,
    PERCENTILE_CONT(ARRAY[0.25, 0.5, 0.75]) WITHIN GROUP (ORDER BY amount) AS quartiles,
    PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY amount) AS median_discrete
FROM transactions;

-- Distribution analysis
WITH value_distribution AS (
    SELECT 
        WIDTH_BUCKET(amount, 0, 1000, 10) AS bucket,
        COUNT(*) AS frequency,
        MIN(amount) AS bucket_min,
        MAX(amount) AS bucket_max
    FROM transactions
    WHERE amount BETWEEN 0 AND 1000
    GROUP BY bucket
)
SELECT 
    bucket,
    frequency,
    ROUND(bucket_min, 2) AS range_start,
    ROUND(bucket_max, 2) AS range_end,
    REPEAT('█', (frequency * 50 / MAX(frequency) OVER ())::INT) AS histogram
FROM value_distribution
ORDER BY bucket;

-- Sampling techniques
-- Random sample
SELECT * FROM large_table
TABLESAMPLE SYSTEM (1);  -- 1% sample

-- Repeatable random sample
SELECT * FROM large_table
TABLESAMPLE BERNOULLI (10) REPEATABLE (42);  -- 10% sample with seed

-- Stratified sampling
WITH stratified AS (
    SELECT *,
        ROW_NUMBER() OVER (PARTITION BY category ORDER BY RANDOM()) AS rn,
        COUNT(*) OVER (PARTITION BY category) AS category_count
    FROM products
)
SELECT * FROM stratified
WHERE rn <= GREATEST(1, category_count * 0.1);  -- 10% from each category""",
            matches_search("statistics correlation regression percentile distribution sampling", search_query)
        )
        
    with analyst_tabs[3]:  # Pivot & Crosstab
        st.subheader("📊 Pivot Tables and Crosstab")
        
        show_postgres_example(
            "Pivot Operations",
            "Transform rows to columns for analysis",
            """-- Manual pivot with CASE
SELECT 
    product_category,
    SUM(CASE WHEN EXTRACT(YEAR FROM order_date) = 2023 THEN amount ELSE 0 END) AS sales_2023,
    SUM(CASE WHEN EXTRACT(YEAR FROM order_date) = 2024 THEN amount ELSE 0 END) AS sales_2024,
    SUM(CASE WHEN EXTRACT(YEAR FROM order_date) = 2025 THEN amount ELSE 0 END) AS sales_2025,
    SUM(amount) AS total_sales
FROM sales
GROUP BY product_category
ORDER BY total_sales DESC;

-- Using FILTER clause (PostgreSQL 9.4+)
SELECT 
    department,
    COUNT(*) AS total_employees,
    COUNT(*) FILTER (WHERE gender = 'M') AS male_count,
    COUNT(*) FILTER (WHERE gender = 'F') AS female_count,
    AVG(salary) FILTER (WHERE years_experience > 5) AS avg_salary_experienced,
    AVG(salary) FILTER (WHERE years_experience <= 5) AS avg_salary_junior
FROM employees
GROUP BY department;

-- Dynamic pivot with JSON
SELECT 
    product_category,
    jsonb_object_agg(
        EXTRACT(YEAR FROM order_date)::TEXT,
        total_amount
    ) AS yearly_sales
FROM (
    SELECT 
        product_category,
        EXTRACT(YEAR FROM order_date) AS year,
        SUM(amount) AS total_amount
    FROM sales
    GROUP BY product_category, EXTRACT(YEAR FROM order_date)
) AS yearly_data
GROUP BY product_category;

-- Using crosstab function (requires tablefunc extension)
CREATE EXTENSION IF NOT EXISTS tablefunc;

-- Basic crosstab
SELECT * FROM crosstab(
    'SELECT region, product_category, SUM(sales_amount)
     FROM sales
     GROUP BY region, product_category
     ORDER BY 1, 2',
    'SELECT DISTINCT product_category FROM sales ORDER BY 1'
) AS ct(
    region TEXT,
    electronics NUMERIC,
    clothing NUMERIC,
    food NUMERIC,
    books NUMERIC
);

-- Crosstab with custom categories
SELECT * FROM crosstab(
    $$SELECT 
        TO_CHAR(order_date, 'YYYY-MM') AS month,
        customer_segment,
        COUNT(*)
    FROM orders
    WHERE order_date >= CURRENT_DATE - INTERVAL '6 months'
    GROUP BY 1, 2
    ORDER BY 1, 2$$,
    $$VALUES ('Premium'), ('Standard'), ('Basic')$$
) AS ct(
    month TEXT,
    premium_orders INTEGER,
    standard_orders INTEGER,
    basic_orders INTEGER
);

-- Multi-dimensional pivot
WITH sales_data AS (
    SELECT 
        region,
        product_category,
        EXTRACT(QUARTER FROM order_date) AS quarter,
        EXTRACT(YEAR FROM order_date) AS year,
        SUM(amount) AS total_sales
    FROM sales
    WHERE order_date >= '2024-01-01'
    GROUP BY 1, 2, 3, 4
)
SELECT 
    region,
    product_category,
    jsonb_object_agg(
        year || '-Q' || quarter,
        total_sales
    ) AS quarterly_sales
FROM sales_data
GROUP BY region, product_category
ORDER BY region, product_category;""",
            matches_search("pivot crosstab FILTER CASE transform columns", search_query)
        )
        
    with analyst_tabs[4]:  # Export
        st.subheader("📤 Data Export and Import")
        
        show_postgres_example(
            "Export Operations",
            "Various ways to export data from PostgreSQL",
            """-- COPY to CSV
COPY (SELECT * FROM users WHERE is_active = true) 
TO '/tmp/active_users.csv' 
WITH (FORMAT CSV, HEADER true, DELIMITER ',', QUOTE '"');

-- COPY with custom options
COPY products 
TO '/tmp/products.txt' 
WITH (
    FORMAT CSV,
    HEADER true,
    DELIMITER E'\\t',  -- Tab-delimited
    NULL '\\N',        -- NULL representation
    QUOTE '"',
    ESCAPE '\\',
    ENCODING 'UTF8'
);

-- Export as INSERT statements
COPY (
    SELECT format(
        'INSERT INTO users (id, email, username) VALUES (%L, %L, %L);',
        id, email, username
    )
    FROM users
) TO '/tmp/users_insert.sql';

-- psql \\copy (client-side, no superuser required)
\\copy users TO 'users.csv' WITH CSV HEADER

-- Export query results
\\copy (SELECT * FROM orders WHERE order_date >= '2024-01-01') 
TO 'orders_2024.csv' WITH CSV HEADER

-- Export to stdout (for piping)
COPY users TO STDOUT WITH CSV HEADER;

-- Import from CSV
COPY users (email, username, created_at)
FROM '/tmp/users.csv'
WITH (FORMAT CSV, HEADER true);

-- Import with data transformation
COPY users (email, username, age)
FROM STDIN WITH (FORMAT CSV, HEADER true);
john@example.com,johndoe,25
jane@example.com,janedoe,30
\\.

-- Import handling errors
COPY users FROM '/tmp/users.csv'
WITH (
    FORMAT CSV,
    HEADER true,
    NULL '\\N',
    QUOTE '"',
    ESCAPE '\\'
);

-- Generate export script
SELECT 
    'COPY ' || schemaname || '.' || tablename || 
    ' TO ''/backup/' || tablename || '.csv'' WITH CSV HEADER;' AS export_command
FROM pg_tables
WHERE schemaname = 'public'
ORDER BY tablename;

-- Export to different formats using pg_dump
-- CSV-like format (use command line)
/*
pg_dump -h localhost -U username -d dbname \
    --table=users \
    --data-only \
    --column-inserts > users_data.sql

pg_dump -h localhost -U username -d dbname \
    --table=users \
    --data-only \
    --inserts > users_inserts.sql
*/

-- JSON export
COPY (
    SELECT jsonb_agg(row_to_json(users))
    FROM users
) TO '/tmp/users.json';

-- XML export
COPY (
    SELECT xmlelement(
        name users,
        xmlagg(
            xmlelement(
                name user,
                xmlforest(id, email, username, created_at)
            )
        )
    )
    FROM users
) TO '/tmp/users.xml';""",
            matches_search("COPY export import CSV JSON XML pg_dump", search_query)
        )
        
    with analyst_tabs[1]:  # Time Series
        st.subheader("📅 Time Series Analysis")
        
        show_postgres_example(
            "Time Series Operations",
            "Working with temporal data and time series analysis",
            """-- Generate time series
WITH time_series AS (
    SELECT generate_series(
        DATE_TRUNC('month', CURRENT_DATE - INTERVAL '12 months'),
        DATE_TRUNC('month', CURRENT_DATE),
        INTERVAL '1 month'
    )::DATE AS month
)
SELECT month FROM time_series;

-- Fill missing dates in data
WITH date_range AS (
    SELECT generate_series(
        MIN(order_date)::DATE,
        MAX(order_date)::DATE,
        INTERVAL '1 day'
    )::DATE AS date
    FROM orders
),
daily_sales AS (
    SELECT 
        DATE(order_date) AS date,
        COUNT(*) AS order_count,
        SUM(total) AS daily_total
    FROM orders
    GROUP BY DATE(order_date)
)
SELECT 
    dr.date,
    COALESCE(ds.order_count, 0) AS order_count,
    COALESCE(ds.daily_total, 0) AS daily_total,
    -- Fill methods
    AVG(ds.daily_total) OVER (ORDER BY dr.date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS ma7_total,
    -- Forward fill
    LAST_VALUE(ds.daily_total) IGNORE NULLS OVER (ORDER BY dr.date) AS forward_filled,
    -- Interpolation
    CASE 
        WHEN ds.daily_total IS NULL THEN 
            (LAG(ds.daily_total) IGNORE NULLS OVER (ORDER BY dr.date) + 
             LEAD(ds.daily_total) IGNORE NULLS OVER (ORDER BY dr.date)) / 2
        ELSE ds.daily_total
    END AS interpolated
FROM date_range dr
LEFT JOIN daily_sales ds ON dr.date = ds.date;

-- Time-based bucketing
SELECT 
    -- Different time buckets
    DATE_TRUNC('hour', created_at) AS hour,
    DATE_TRUNC('day', created_at) AS day,
    DATE_TRUNC('week', created_at) AS week,
    DATE_TRUNC('month', created_at) AS month,
    DATE_TRUNC('quarter', created_at) AS quarter,
    DATE_TRUNC('year', created_at) AS year,
    -- Custom buckets
    TO_CHAR(created_at, 'YYYY-WW') AS year_week,
    TO_CHAR(created_at, 'YYYY-MM-DD HH24:00:00') AS hour_bucket,
    -- Count and aggregate
    COUNT(*) AS event_count,
    AVG(value) AS avg_value
FROM events
GROUP BY 1, 2, 3, 4, 5, 6, 7, 8;

-- Sessionization (identify user sessions)
WITH user_events AS (
    SELECT 
        user_id,
        event_time,
        LAG(event_time) OVER (PARTITION BY user_id ORDER BY event_time) AS prev_event_time,
        event_time - LAG(event_time) OVER (PARTITION BY user_id ORDER BY event_time) AS time_since_last
    FROM user_activity
),
session_starts AS (
    SELECT 
        user_id,
        event_time,
        CASE 
            WHEN prev_event_time IS NULL OR time_since_last > INTERVAL '30 minutes' 
            THEN 1 
            ELSE 0 
        END AS is_session_start
    FROM user_events
),
sessions AS (
    SELECT 
        user_id,
        event_time,
        SUM(is_session_start) OVER (PARTITION BY user_id ORDER BY event_time) AS session_id
    FROM session_starts
)
SELECT 
    user_id,
    session_id,
    MIN(event_time) AS session_start,
    MAX(event_time) AS session_end,
    COUNT(*) AS events_in_session,
    MAX(event_time) - MIN(event_time) AS session_duration
FROM sessions
GROUP BY user_id, session_id;

-- Time zone handling
SELECT 
    created_at AT TIME ZONE 'UTC' AS utc_time,
    created_at AT TIME ZONE 'America/New_York' AS ny_time,
    created_at AT TIME ZONE 'Europe/London' AS london_time,
    -- Convert and format
    TO_CHAR(created_at AT TIME ZONE 'America/Los_Angeles', 'YYYY-MM-DD HH24:MI:SS TZ') AS la_time_formatted
FROM events;

-- Period-over-period comparison
WITH sales_by_period AS (
    SELECT 
        DATE_TRUNC('month', order_date) AS period,
        EXTRACT(YEAR FROM order_date) AS year,
        EXTRACT(MONTH FROM order_date) AS month,
        SUM(total) AS period_total
    FROM orders
    GROUP BY 1, 2, 3
)
SELECT 
    period,
    period_total AS current_period,
    LAG(period_total, 12) OVER (ORDER BY period) AS same_period_last_year,
    period_total - LAG(period_total, 12) OVER (ORDER BY period) AS yoy_absolute_change,
    ROUND((period_total - LAG(period_total, 12) OVER (ORDER BY period)) / 
          NULLIF(LAG(period_total, 12) OVER (ORDER BY period), 0) * 100, 2) AS yoy_percent_change
FROM sales_by_period
ORDER BY period;""",
            matches_search("time series generate_series date_trunc session period", search_query)
        )

# DBA/Support Tab
with role_tab3:
    st.header("🛠️ DBA & Support Toolkit")
    
    dba_tabs = st.tabs([
        "Performance", "Maintenance", "Monitoring", "Backup", "Troubleshooting"
    ])
    
    with dba_tabs[0]:  # Performance
        st.subheader("⚡ Performance Tuning")
        
        show_postgres_example(
            "Query Performance Analysis",
            "Identify and optimize slow queries",
            """-- Enable query timing
\\timing on

-- Explain plan with execution statistics
EXPLAIN (ANALYZE, BUFFERS, VERBOSE, SETTINGS) 
SELECT c.name, COUNT(o.id) AS order_count
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
GROUP BY c.name;

-- Find slow queries
SELECT 
    query,
    mean_exec_time,
    calls,
    total_exec_time,
    min_exec_time,
    max_exec_time,
    stddev_exec_time,
    rows
FROM pg_stat_statements
WHERE query NOT LIKE '%pg_stat_statements%'
ORDER BY mean_exec_time DESC
LIMIT 20;

-- Index usage statistics
SELECT 
    schemaname,
    tablename,
    indexname,
    idx_scan AS index_scans,
    idx_tup_read AS tuples_read,
    idx_tup_fetch AS tuples_fetched,
    pg_size_pretty(pg_relation_size(indexrelid)) AS index_size
FROM pg_stat_user_indexes
ORDER BY idx_scan;

-- Find missing indexes
SELECT 
    schemaname,
    tablename,
    seq_scan,
    seq_tup_read,
    idx_scan,
    seq_tup_read / GREATEST(seq_scan, 1) AS avg_tuples_per_scan
FROM pg_stat_user_tables
WHERE seq_scan > 0
ORDER BY seq_tup_read DESC;

-- Table bloat estimation
WITH constants AS (
    SELECT current_setting('block_size')::numeric AS bs, 23 AS hdr, 8 AS ma
),
no_stats AS (
    SELECT table_schema, table_name, 
        n_live_tup::numeric as est_rows,
        pg_table_size(relid)::numeric as table_size
    FROM information_schema.columns
        JOIN pg_stat_user_tables as psut
           ON table_schema=psut.schemaname
           AND table_name=psut.relname
        LEFT OUTER JOIN pg_stats
        ON table_schema=pg_stats.schemaname
            AND table_name=pg_stats.tablename
            AND column_name=attname 
    WHERE attname IS NULL
        AND table_schema NOT IN ('pg_catalog', 'information_schema')
    GROUP BY table_schema, table_name, relid, n_live_tup
),
null_headers AS (
    SELECT
        hdr+1+(sum(case when null_frac <> 0 THEN 1 else 0 END)/8) as nullhdr,
        SUM((1-null_frac)*avg_width) as datawidth,
        MAX(null_frac) as maxfracsum,
        schemaname,
        tablename,
        hdr, ma, bs
    FROM pg_stats CROSS JOIN constants
        LEFT OUTER JOIN no_stats
            ON schemaname=no_stats.table_schema
            AND tablename=no_stats.table_name
    WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
        AND no_stats.table_name IS NULL
        AND EXISTS ( SELECT 1
            FROM information_schema.columns
                WHERE schemaname = columns.table_schema
                    AND tablename = columns.table_name )
    GROUP BY schemaname, tablename, hdr, ma, bs
),
data_headers AS (
    SELECT
        ma, bs, hdr, schemaname, tablename,
        (datawidth+(hdr+ma-(case when hdr%ma=0 THEN ma ELSE hdr%ma END)))::numeric AS datahdr,
        (maxfracsum*(nullhdr+ma-(case when nullhdr%ma=0 THEN ma ELSE nullhdr%ma END))) AS nullhdr2
    FROM null_headers
),
table_estimates AS (
    SELECT schemaname, tablename, bs,
        reltuples::numeric as est_rows, relpages * bs as table_bytes,
    CEIL((reltuples*
            (datahdr + nullhdr2 + 4 + ma -
                (CASE WHEN datahdr%ma=0
                    THEN ma ELSE datahdr%ma END)
                )/(bs-20))) * bs AS expected_bytes,
        reltoastrelid
    FROM data_headers
        JOIN pg_class ON tablename = relname
        JOIN pg_namespace ON relnamespace = pg_namespace.oid
            AND schemaname = nspname
    WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
),
estimates_with_toast AS (
    SELECT schemaname, tablename, 
        TRUE AS can_estimate,
        est_rows,
        table_bytes + ( coalesce(toast.relpages, 0) * bs ) as table_bytes,
        expected_bytes + ( ceil( coalesce(toast.reltuples, 0) / 4 ) * bs ) as expected_bytes
    FROM table_estimates LEFT OUTER JOIN pg_class as toast
        ON table_estimates.reltoastrelid = toast.oid
            AND toast.relkind = 't'
),
table_estimates_plus AS (
    SELECT current_database() as databasename,
            schemaname, tablename, can_estimate, 
            est_rows,
            CASE WHEN table_bytes > 0
                THEN table_bytes::NUMERIC
                ELSE NULL::NUMERIC END
                AS table_bytes,
            CASE WHEN expected_bytes > 0 
                THEN expected_bytes::NUMERIC
                ELSE NULL::NUMERIC END
                    AS expected_bytes,
            CASE WHEN expected_bytes > 0 AND table_bytes > 0
                AND expected_bytes <= table_bytes
                THEN (table_bytes - expected_bytes)::NUMERIC
                ELSE 0::NUMERIC END AS bloat_bytes
    FROM estimates_with_toast
    UNION ALL
    SELECT current_database() as databasename, 
        table_schema, table_name, FALSE, 
        est_rows, table_size,
        NULL::NUMERIC, NULL::NUMERIC
    FROM no_stats
),
bloat_data AS (
    select current_database() as databasename,
        schemaname, tablename, can_estimate, 
        table_bytes, expected_bytes, bloat_bytes,
        round(bloat_bytes*100/table_bytes) as bloat_ratio
    FROM table_estimates_plus
)
SELECT databasename, schemaname, tablename,
    can_estimate,
    pg_size_pretty(table_bytes) as table_size,
    CASE WHEN expected_bytes IS NOT NULL THEN pg_size_pretty(expected_bytes) ELSE NULL END AS expected_size,
    CASE WHEN bloat_bytes IS NOT NULL AND bloat_bytes > 0 THEN pg_size_pretty(bloat_bytes) ELSE NULL END AS bloat_size,
    bloat_ratio
FROM bloat_data
WHERE ( bloat_ratio > 30 AND bloat_bytes > 1024*100 )
ORDER BY bloat_bytes DESC;

-- Cache hit ratio
SELECT 
    'index hit rate' AS name,
    (sum(idx_blks_hit)) / NULLIF(sum(idx_blks_hit + idx_blks_read),0) AS ratio
FROM pg_statio_user_indexes
UNION ALL
SELECT 
    'table hit rate' AS name,
    sum(heap_blks_hit) / NULLIF(sum(heap_blks_hit) + sum(heap_blks_read),0) AS ratio
FROM pg_statio_user_tables;""",
            matches_search("performance explain analyze slow query index bloat cache", search_query)
        )
        
    with dba_tabs[2]:  # Monitoring
        st.subheader("📊 Database Monitoring")
        
        show_postgres_example(
            "System Monitoring",
            "Monitor database health and activity",
            """-- Database size information
SELECT 
    pg_database.datname,
    pg_size_pretty(pg_database_size(pg_database.datname)) AS size
FROM pg_database
ORDER BY pg_database_size(pg_database.datname) DESC;

-- Table sizes with indexes and toast
SELECT
    schemaname AS schema,
    tablename AS table,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS total_size,
    pg_size_pretty(pg_relation_size(schemaname||'.'||tablename)) AS table_size,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename) - pg_relation_size(schemaname||'.'||tablename)) AS indexes_size
FROM pg_tables
WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
LIMIT 20;

-- Connection information
SELECT 
    pid,
    usename,
    application_name,
    client_addr,
    backend_start,
    state,
    wait_event_type,
    wait_event,
    query
FROM pg_stat_activity
WHERE state != 'idle'
ORDER BY backend_start;

-- Lock monitoring
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

-- Replication lag (for replicas)
SELECT 
    client_addr,
    state,
    sent_lsn,
    write_lsn,
    flush_lsn,
    replay_lsn,
    pg_wal_lsn_diff(sent_lsn, replay_lsn) AS replication_lag_bytes,
    replay_lag
FROM pg_stat_replication;

-- Background writer statistics
SELECT 
    checkpoints_timed,
    checkpoints_req,
    checkpoint_write_time,
    checkpoint_sync_time,
    buffers_checkpoint,
    buffers_clean,
    maxwritten_clean,
    buffers_backend,
    buffers_backend_fsync,
    buffers_alloc
FROM pg_stat_bgwriter;

-- Long running queries
SELECT 
    pid,
    now() - pg_stat_activity.query_start AS duration,
    query,
    state
FROM pg_stat_activity
WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes'
AND state != 'idle';

-- Table I/O statistics
SELECT 
    schemaname,
    tablename,
    heap_blks_read,
    heap_blks_hit,
    CASE 
        WHEN heap_blks_read + heap_blks_hit = 0 THEN 0
        ELSE round(100.0 * heap_blks_hit / (heap_blks_read + heap_blks_hit), 2)
    END AS cache_hit_ratio,
    idx_blks_read,
    idx_blks_hit
FROM pg_statio_user_tables
ORDER BY heap_blks_read + heap_blks_hit DESC
LIMIT 20;

-- Unused indexes
SELECT 
    schemaname,
    tablename,
    indexname,
    idx_scan,
    pg_size_pretty(pg_relation_size(indexrelid)) AS index_size
FROM pg_stat_user_indexes
WHERE idx_scan = 0
AND indexrelname NOT LIKE '%_pkey'
ORDER BY pg_relation_size(indexrelid) DESC;""",
            matches_search("monitoring pg_stat activity replication locks size", search_query)
        )
        
    with dba_tabs[3]:  # Backup
        st.subheader("💾 Backup and Recovery")
        
        show_postgres_example(
            "Backup Strategies",
            "Different backup methods and recovery procedures",
            """-- Logical backup with pg_dump
-- Full database backup
pg_dump -h localhost -U postgres -d mydb -f mydb_backup.sql

-- Compressed backup
pg_dump -h localhost -U postgres -d mydb -Fc -f mydb_backup.dump

-- Directory format (parallel)
pg_dump -h localhost -U postgres -d mydb -Fd -j 4 -f mydb_backup_dir

-- Specific schemas
pg_dump -h localhost -U postgres -d mydb -n public -n app_schema -f schemas_backup.sql

-- Exclude tables
pg_dump -h localhost -U postgres -d mydb -T logs -T temp_* -f mydb_no_logs.sql

-- Data only
pg_dump -h localhost -U postgres -d mydb -a -f mydb_data_only.sql

-- Schema only
pg_dump -h localhost -U postgres -d mydb -s -f mydb_schema_only.sql

-- Global objects (roles, tablespaces)
pg_dumpall -h localhost -U postgres -g -f globals.sql

-- All databases
pg_dumpall -h localhost -U postgres -f all_databases.sql

-- Restore operations
-- From SQL file
psql -h localhost -U postgres -d mydb -f mydb_backup.sql

-- From custom format
pg_restore -h localhost -U postgres -d mydb mydb_backup.dump

-- Create database and restore
pg_restore -h localhost -U postgres -C -d postgres mydb_backup.dump

-- Parallel restore
pg_restore -h localhost -U postgres -d mydb -j 4 mydb_backup_dir

-- Selective restore
pg_restore -h localhost -U postgres -d mydb -t users -t orders mydb_backup.dump

-- Physical backup with pg_basebackup
-- Full cluster backup
pg_basebackup -h localhost -U replicator -D /backup/base -Fp -Xs -P

-- Compressed tar format
pg_basebackup -h localhost -U replicator -D /backup/base -Ft -z -Xs -P

-- Include WAL files
pg_basebackup -h localhost -U replicator -D /backup/base -Fp -Xf -P

-- Point-in-time recovery setup
-- In postgresql.conf:
archive_mode = on
archive_command = 'cp %p /archive/%f'
wal_level = replica

-- Continuous archiving script
#!/bin/bash
# Archive WAL files with compression
archive_wal() {
    local wal_file=$1
    local archive_dir="/archive"
    gzip < "$wal_file" > "$archive_dir/$(basename $wal_file).gz"
}

-- Recovery configuration (recovery.conf or postgresql.auto.conf)
restore_command = 'gunzip < /archive/%f.gz > %p'
recovery_target_time = '2024-01-15 14:30:00'
recovery_target_action = 'promote'

-- Backup validation
-- Check backup file
pg_restore -l mydb_backup.dump

-- Verify backup integrity
pg_dump -h localhost -U postgres -d mydb | md5sum > backup.md5
pg_restore -h localhost -U postgres -d test_restore mydb_backup.dump
pg_dump -h localhost -U postgres -d test_restore | md5sum -c backup.md5

-- Backup automation script
#!/bin/bash
BACKUP_DIR="/backup/postgres"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="mydb"

# Create backup
pg_dump -h localhost -U postgres -d $DB_NAME -Fc -f "$BACKUP_DIR/${DB_NAME}_${DATE}.dump"

# Keep only last 7 days
find $BACKUP_DIR -name "${DB_NAME}_*.dump" -mtime +7 -delete

# Test restore to verify
pg_restore --list "$BACKUP_DIR/${DB_NAME}_${DATE}.dump" > /dev/null 2>&1
if [ $? -eq 0 ]; then
    echo "Backup verified successfully"
else
    echo "Backup verification failed"
    exit 1
fi""",
            matches_search("backup restore pg_dump pg_restore pg_basebackup recovery", search_query)
        )
        
    with dba_tabs[4]:  # Troubleshooting
        st.subheader("🔍 Troubleshooting")
        
        show_postgres_example(
            "Common Issues and Solutions",
            "Diagnose and fix common PostgreSQL problems",
            """-- Connection issues
-- Check PostgreSQL is running
SELECT version();

-- Check connection limits
SELECT 
    setting AS max_connections,
    (SELECT count(*) FROM pg_stat_activity) AS current_connections,
    (SELECT count(*) FROM pg_stat_activity) / setting::float * 100 AS percentage_used
FROM pg_settings
WHERE name = 'max_connections';

-- Find connection hogs
SELECT 
    usename,
    application_name,
    count(*) AS connection_count
FROM pg_stat_activity
GROUP BY usename, application_name
ORDER BY connection_count DESC;

-- Performance issues
-- Find slow queries without pg_stat_statements
SELECT 
    pid,
    now() - query_start AS duration,
    substring(query, 1, 100) AS query_preview,
    state,
    wait_event_type,
    wait_event
FROM pg_stat_activity
WHERE state != 'idle'
AND query_start < now() - interval '1 minute'
ORDER BY duration DESC;

-- Identify blocking queries
WITH blocking AS (
    SELECT 
        pid,
        query,
        now() - query_start AS blocking_duration
    FROM pg_stat_activity
    WHERE pid IN (
        SELECT DISTINCT blocking_locks.pid
        FROM pg_locks blocked_locks
        JOIN pg_locks blocking_locks 
            ON blocking_locks.locktype = blocked_locks.locktype
            AND blocking_locks.database IS NOT DISTINCT FROM blocked_locks.database
            AND blocking_locks.relation IS NOT DISTINCT FROM blocked_locks.relation
            AND blocking_locks.pid != blocked_locks.pid
        WHERE NOT blocked_locks.granted
    )
)
SELECT * FROM blocking;

-- Disk space issues
-- Check database sizes
SELECT 
    datname,
    pg_size_pretty(pg_database_size(datname)) AS size
FROM pg_database
WHERE datname NOT IN ('template0', 'template1')
ORDER BY pg_database_size(datname) DESC;

-- Find large tables
SELECT 
    schemaname || '.' || tablename AS table_full_name,
    pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE schemaname NOT IN ('pg_catalog', 'information_schema')
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
LIMIT 10;

-- Check for table bloat
SELECT 
    schemaname,
    tablename,
    n_dead_tup,
    n_live_tup,
    round(n_dead_tup * 100.0 / NULLIF(n_live_tup + n_dead_tup, 0), 2) AS dead_percentage
FROM pg_stat_user_tables
WHERE n_dead_tup > 1000
ORDER BY n_dead_tup DESC;

-- Transaction ID wraparound prevention
SELECT 
    datname,
    age(datfrozenxid) AS age,
    2^31 - age(datfrozenxid) AS transactions_until_wraparound
FROM pg_database
ORDER BY age DESC;

-- Check autovacuum is working
SELECT 
    schemaname,
    tablename,
    last_vacuum,
    last_autovacuum,
    last_analyze,
    last_autoanalyze
FROM pg_stat_user_tables
WHERE last_autovacuum IS NULL
OR last_autovacuum < CURRENT_DATE - INTERVAL '7 days'
ORDER BY n_live_tup DESC;

-- Memory issues
-- Check shared buffer usage
SELECT 
    pg_size_pretty(pg_database_size(current_database())) AS database_size,
    pg_size_pretty(current_setting('shared_buffers')::bigint * 8192) AS shared_buffers_size,
    round(100.0 * pg_database_size(current_database()) / 
          (current_setting('shared_buffers')::bigint * 8192), 2) AS percentage_of_shared_buffers
FROM pg_database
WHERE datname = current_database();

-- Configuration issues
-- Show non-default settings
SELECT 
    name,
    setting,
    unit,
    source
FROM pg_settings
WHERE source != 'default'
ORDER BY name;

-- Check for configuration errors
SELECT 
    name,
    setting,
    pending_restart
FROM pg_settings
WHERE pending_restart = true;

-- Emergency fixes
-- Cancel long-running query
SELECT pg_cancel_backend(pid)
FROM pg_stat_activity
WHERE pid = 12345;

-- Terminate connection (more forceful)
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE pid = 12345;

-- Emergency VACUUM
SET vacuum_cost_delay = 0;
VACUUM (VERBOSE, ANALYZE) large_table;

-- Reset statistics
SELECT pg_stat_reset();

-- Clear shared buffers (requires superuser)
SELECT pg_prewarm('table_name', 'buffer');""",
            matches_search("troubleshooting performance connection disk memory fix", search_query)
        )
        
    with dba_tabs[1]:  # Maintenance
        st.subheader("🔧 Database Maintenance")
        
        show_postgres_example(
            "VACUUM and Maintenance",
            "Keep your database healthy and performant",
            """-- Manual VACUUM commands
VACUUM;                          -- Basic vacuum all databases
VACUUM FULL;                     -- Full vacuum (locks table)
VACUUM FREEZE;                   -- Aggressive freezing
VACUUM ANALYZE;                  -- Vacuum and update statistics
VACUUM (VERBOSE, ANALYZE) tablename;  -- Verbose output with stats

-- Check autovacuum status
SELECT 
    schemaname,
    tablename,
    last_vacuum,
    last_autovacuum,
    last_analyze,
    last_autoanalyze,
    vacuum_count,
    autovacuum_count,
    analyze_count,
    autoanalyze_count,
    n_live_tup,
    n_dead_tup,
    n_dead_tup::float / NULLIF(n_live_tup, 0) AS dead_ratio
FROM pg_stat_user_tables
ORDER BY n_dead_tup DESC;

-- Tables needing vacuum
SELECT 
    schemaname,
    tablename,
    n_dead_tup,
    n_live_tup,
    round(n_dead_tup::numeric / NULLIF(n_live_tup, 0), 4) AS dead_tuple_ratio,
    last_autovacuum,
    CURRENT_TIMESTAMP - last_autovacuum AS time_since_vacuum
FROM pg_stat_user_tables
WHERE n_dead_tup > 1000
AND n_dead_tup::float / NULLIF(n_live_tup, 0) > 0.1
ORDER BY dead_tuple_ratio DESC;

-- Configure autovacuum per table
ALTER TABLE large_table SET (
    autovacuum_vacuum_scale_factor = 0.01,
    autovacuum_vacuum_threshold = 1000,
    autovacuum_analyze_scale_factor = 0.005,
    autovacuum_analyze_threshold = 500
);

-- REINDEX operations
REINDEX TABLE tablename;         -- Rebuild all indexes on table
REINDEX INDEX indexname;         -- Rebuild specific index
REINDEX DATABASE dbname;         -- Rebuild all indexes in database
REINDEX SCHEMA schemaname;       -- Rebuild all indexes in schema
REINDEX (VERBOSE) TABLE tablename;  -- With progress

-- Concurrent index creation (no table lock)
CREATE INDEX CONCURRENTLY idx_name ON table_name (column_name);
DROP INDEX CONCURRENTLY idx_name;

-- Update table statistics
ANALYZE;                         -- Analyze all tables
ANALYZE tablename;               -- Analyze specific table
ANALYZE tablename (column1, column2);  -- Specific columns

-- Find tables with stale statistics
SELECT 
    schemaname,
    tablename,
    last_analyze,
    last_autoanalyze,
    GREATEST(last_analyze, last_autoanalyze) AS last_stats_update,
    CURRENT_TIMESTAMP - GREATEST(last_analyze, last_autoanalyze) AS time_since_analyze
FROM pg_stat_user_tables
WHERE GREATEST(last_analyze, last_autoanalyze) < CURRENT_TIMESTAMP - INTERVAL '7 days'
OR (last_analyze IS NULL AND last_autoanalyze IS NULL)
ORDER BY time_since_analyze DESC NULLS FIRST;

-- Reset statistics
SELECT pg_stat_reset();          -- Reset all statistics
SELECT pg_stat_reset_single_table_counters('schema.table'::regclass);
SELECT pg_stat_reset_single_function_counters('schema.function'::regproc);

-- Table maintenance script
DO $$
DECLARE
    r RECORD;
BEGIN
    FOR r IN 
        SELECT schemaname, tablename 
        FROM pg_stat_user_tables 
        WHERE n_dead_tup > 10000
    LOOP
        RAISE NOTICE 'Vacuuming %.%', r.schemaname, r.tablename;
        EXECUTE format('VACUUM ANALYZE %I.%I', r.schemaname, r.tablename);
    END LOOP;
END$$;""",
            matches_search("VACUUM REINDEX ANALYZE maintenance autovacuum statistics", search_query)
        )

# Reference Tab
with role_tab4:
    st.header("📚 Quick Reference")
    
    ref_tabs = st.tabs(["psql Commands", "Functions", "Config", "Extensions", "Best Practices"])
    
    with ref_tabs[0]:  # psql Commands
        st.subheader("🖥️ psql Meta-Commands")
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.code("""
-- Database navigation
\\l or \\list              -- List databases
\\c dbname                -- Connect to database
\\dt                      -- List tables
\\dt+                     -- List tables with size
\\d tablename             -- Describe table
\\d+ tablename            -- Describe table (verbose)
\\di                      -- List indexes
\\dv                      -- List views
\\df                      -- List functions
\\du                      -- List users/roles
\\dn                      -- List schemas
\\dx                      -- List extensions

-- Query helpers
\\x                       -- Toggle expanded display
\\a                       -- Toggle aligned output
\\t                       -- Tuples only (no headers)
\\timing                  -- Toggle timing
\\watch 2                 -- Re-run query every 2 seconds

-- Output control
\\o filename              -- Send output to file
\\o                       -- Stop sending to file
\\copy (SELECT ...) TO 'file.csv' CSV HEADER
\\g filename              -- Execute and save to file
\\gx                      -- Execute with expanded output
""", language="text")
            
        with col2:
            st.code("""
-- Editing and history
\\e                       -- Edit in external editor
\\ef function_name        -- Edit function
\\ev view_name            -- Edit view
\\p                       -- Show current query buffer
\\r                       -- Reset query buffer
\\s                       -- Show command history
\\s filename              -- Save history to file

-- Information
\\? or \\h                 -- Help on psql commands
\\h CREATE TABLE          -- Help on SQL command
\\conninfo                -- Connection information
\\encoding                -- Show client encoding
\\! command               -- Execute shell command
\\cd directory            -- Change directory
\\q                       -- Quit psql

-- Variables
\\set AUTOCOMMIT off      -- Turn off autocommit
\\set QUIET on            -- Suppress messages
\\set ON_ERROR_ROLLBACK on -- Automatic savepoints
\\set HISTSIZE 2000       -- History size
\\unset varname           -- Unset variable

-- Shortcuts
\\gset                    -- Store query result in variables
\\gdesc                   -- Describe result columns
\\crosstabview           -- Pivot results
""", language="text")
            
    with ref_tabs[1]:  # Functions
        st.subheader("🧮 PostgreSQL Functions")
        
        func_tabs = st.tabs(["String", "Date/Time", "Math", "Array", "JSON"])
        
        with func_tabs[0]:  # String
            st.code("""
-- String manipulation
LENGTH(string)                    -- String length
CHAR_LENGTH(string)              -- Character count
LOWER(string)                    -- Lowercase
UPPER(string)                    -- Uppercase
INITCAP(string)                  -- Title case
TRIM(string)                     -- Remove spaces
LTRIM(string, chars)             -- Left trim
RTRIM(string, chars)             -- Right trim

-- Substring operations
SUBSTRING(string FROM start FOR length)
LEFT(string, n)                  -- First n characters
RIGHT(string, n)                 -- Last n characters
SPLIT_PART(string, delimiter, n) -- Split and get nth part

-- Pattern matching
POSITION(substring IN string)     -- Find position
STRPOS(string, substring)        -- Same as POSITION
REPLACE(string, from, to)        -- Replace all occurrences
TRANSLATE(string, from, to)      -- Character mapping
OVERLAY(string PLACING new FROM start FOR length)

-- Concatenation
string1 || string2               -- Concatenate
CONCAT(str1, str2, ...)         -- Concatenate (NULL safe)
CONCAT_WS(sep, str1, str2, ...) -- Concatenate with separator

-- Regular expressions
string ~ 'pattern'               -- Match
string !~ 'pattern'              -- No match
string ~* 'pattern'              -- Match case-insensitive
REGEXP_MATCH(string, pattern)    -- Return match
REGEXP_MATCHES(string, pattern, 'g')  -- All matches
REGEXP_REPLACE(string, pattern, replacement, 'g')
REGEXP_SPLIT_TO_ARRAY(string, pattern)
""", language="sql")
            
        with func_tabs[1]:  # Date/Time
            st.code("""
-- Current date/time
CURRENT_DATE                     -- Date
CURRENT_TIME                     -- Time with timezone
CURRENT_TIMESTAMP                -- Timestamp with timezone
NOW()                           -- Same as CURRENT_TIMESTAMP
LOCALTIME                       -- Time without timezone
LOCALTIMESTAMP                  -- Timestamp without timezone

-- Date/time construction
DATE '2024-01-15'               -- Date literal
TIME '14:30:00'                 -- Time literal
TIMESTAMP '2024-01-15 14:30:00' -- Timestamp literal
INTERVAL '1 day 2 hours'        -- Interval literal

-- Extraction
EXTRACT(field FROM timestamp)    -- Extract part
DATE_PART('field', timestamp)    -- Same as EXTRACT
DATE_TRUNC('field', timestamp)   -- Truncate to field

-- Fields: year, month, day, hour, minute, second,
-- week, quarter, dow, doy, epoch, century, decade

-- Arithmetic
date + integer                   -- Add days
date - integer                   -- Subtract days
date - date                      -- Days between
timestamp + interval             -- Add interval
timestamp - interval             -- Subtract interval

-- Formatting
TO_CHAR(timestamp, 'format')     -- Format as string
TO_DATE(string, 'format')        -- Parse date
TO_TIMESTAMP(string, 'format')   -- Parse timestamp

-- Common formats:
-- YYYY-MM-DD HH24:MI:SS
-- Mon DD, YYYY
-- Day, DD Month YYYY

-- Other functions
AGE(timestamp)                   -- Age from now
AGE(timestamp1, timestamp2)      -- Age between
MAKE_DATE(year, month, day)      -- Construct date
MAKE_TIME(hour, min, sec)        -- Construct time
""", language="sql")
            
    with ref_tabs[2]:  # Config
        st.subheader("⚙️ Configuration")
        
        st.code("""
-- Key configuration parameters
-- Memory settings
shared_buffers = 256MB           -- 25% of RAM for dedicated server
effective_cache_size = 1GB       -- 50-75% of RAM
work_mem = 4MB                   -- Per operation memory
maintenance_work_mem = 64MB      -- For VACUUM, CREATE INDEX, etc.

-- Write-ahead logging
wal_level = replica              -- Minimal, replica, or logical
max_wal_size = 1GB              -- Maximum WAL size between checkpoints
min_wal_size = 80MB             -- Minimum WAL size
checkpoint_timeout = 5min        -- Time between checkpoints
checkpoint_completion_target = 0.7  -- Spread checkpoint I/O

-- Connection settings
max_connections = 100            -- Maximum concurrent connections
superuser_reserved_connections = 3  -- Reserved for superusers

-- Query planner
random_page_cost = 4.0          -- Cost of random disk access (1.1 for SSD)
seq_page_cost = 1.0             -- Cost of sequential disk access
cpu_tuple_cost = 0.01           -- Cost of processing each row
cpu_index_tuple_cost = 0.005    -- Cost of processing each index entry
cpu_operator_cost = 0.0025      -- Cost of processing each operator

-- Autovacuum settings
autovacuum = on                 -- Enable autovacuum
autovacuum_max_workers = 3      -- Number of autovacuum workers
autovacuum_naptime = 1min       -- Time between autovacuum runs
autovacuum_vacuum_threshold = 50     -- Minimum number of row updates
autovacuum_vacuum_scale_factor = 0.2 -- Fraction of table size
autovacuum_analyze_threshold = 50    -- Minimum number of row updates
autovacuum_analyze_scale_factor = 0.1 -- Fraction of table size

-- Logging
log_destination = 'stderr'       -- Where to log
logging_collector = on           -- Enable log collection
log_directory = 'pg_log'         -- Log directory
log_filename = 'postgresql-%Y-%m-%d_%H%M%S.log'
log_rotation_age = 1d            -- Rotate logs daily
log_rotation_size = 100MB        -- Rotate logs at size
log_min_duration_statement = 1000  -- Log queries slower than 1s
log_checkpoints = on             -- Log checkpoints
log_connections = on             -- Log connections
log_disconnections = on          -- Log disconnections
log_lock_waits = on              -- Log lock waits
log_temp_files = 0               -- Log all temp files

-- Statement behavior
statement_timeout = 0            -- Cancel queries after duration (0 = off)
lock_timeout = 0                 -- Cancel if can't acquire lock
idle_in_transaction_session_timeout = 0  -- Kill idle transactions

-- Runtime statistics
track_activities = on            -- Track command execution
track_counts = on                -- Track table/index access
track_io_timing = on             -- Track I/O timing (small overhead)
track_functions = all            -- Track function execution

-- View current settings
SELECT name, setting, unit, category, short_desc
FROM pg_settings
WHERE name IN ('shared_buffers', 'work_mem', 'max_connections')
ORDER BY category, name;

-- Change settings at runtime
SET work_mem = '8MB';                    -- Session only
SET LOCAL work_mem = '8MB';              -- Transaction only
ALTER SYSTEM SET work_mem = '8MB';       -- Persistent (requires reload)
ALTER DATABASE mydb SET work_mem = '8MB'; -- Database default
ALTER ROLE myuser SET work_mem = '8MB';   -- User default

-- Reload configuration
SELECT pg_reload_conf();         -- Reload postgresql.conf

-- Check which settings need restart
SELECT name, pending_restart
FROM pg_settings
WHERE pending_restart = true;
""", language="sql")
        
    with ref_tabs[3]:  # Extensions
        st.subheader("🔌 Popular Extensions")
        
        st.code("""
-- View available extensions
SELECT * FROM pg_available_extensions ORDER BY name;

-- View installed extensions
SELECT * FROM pg_extension;

-- Core extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";      -- UUID generation
CREATE EXTENSION IF NOT EXISTS "pgcrypto";       -- Cryptographic functions
CREATE EXTENSION IF NOT EXISTS "hstore";         -- Key-value store
CREATE EXTENSION IF NOT EXISTS "citext";         -- Case-insensitive text
CREATE EXTENSION IF NOT EXISTS "btree_gist";     -- GiST operator classes
CREATE EXTENSION IF NOT EXISTS "btree_gin";      -- GIN operator classes
CREATE EXTENSION IF NOT EXISTS "intarray";       -- Integer array functions
CREATE EXTENSION IF NOT EXISTS "tablefunc";      -- Crosstab and more
CREATE EXTENSION IF NOT EXISTS "pg_trgm";        -- Trigram matching
CREATE EXTENSION IF NOT EXISTS "fuzzystrmatch";  -- Fuzzy string matching
CREATE EXTENSION IF NOT EXISTS "unaccent";       -- Text search dictionary

-- PostGIS (spatial/geographic)
CREATE EXTENSION IF NOT EXISTS "postgis";
CREATE EXTENSION IF NOT EXISTS "postgis_topology";
CREATE EXTENSION IF NOT EXISTS "postgis_raster";

-- Full text search
CREATE EXTENSION IF NOT EXISTS "pg_trgm";        -- Trigram search
CREATE EXTENSION IF NOT EXISTS "unaccent";       -- Remove accents

-- Performance
CREATE EXTENSION IF NOT EXISTS "pg_stat_statements";  -- Query statistics
CREATE EXTENSION IF NOT EXISTS "pg_buffercache";      -- Buffer cache inspection
CREATE EXTENSION IF NOT EXISTS "pgstattuple";         -- Tuple statistics
CREATE EXTENSION IF NOT EXISTS "pg_prewarm";          -- Preload relations

-- Foreign data wrappers
CREATE EXTENSION IF NOT EXISTS "postgres_fdw";   -- PostgreSQL FDW
CREATE EXTENSION IF NOT EXISTS "file_fdw";       -- CSV/text files
CREATE EXTENSION IF NOT EXISTS "dblink";         -- Database links

-- Time series
CREATE EXTENSION IF NOT EXISTS "timescaledb";    -- Time series database

-- JSON enhancements
CREATE EXTENSION IF NOT EXISTS "jsquery";        -- JSON query language

-- Monitoring
CREATE EXTENSION IF NOT EXISTS "pg_stat_statements";
ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements';
-- Requires restart

-- Example usage: UUID
SELECT uuid_generate_v4();
SELECT uuid_generate_v1mc();

-- Example usage: pgcrypto
SELECT encode(digest('password', 'sha256'), 'hex');
SELECT crypt('password', gen_salt('bf'));

-- Example usage: hstore
CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name TEXT,
    attributes hstore
);
INSERT INTO products (name, attributes) VALUES 
    ('Laptop', 'brand=>Dell, cpu=>i7, ram=>16GB'::hstore);
SELECT name, attributes->'cpu' AS cpu FROM products;

-- Example usage: pg_trgm
CREATE INDEX idx_users_name_trgm ON users USING gin (name gin_trgm_ops);
SELECT * FROM users WHERE name % 'john';  -- Fuzzy match

-- Example usage: citext
CREATE TABLE users_ci (
    email citext PRIMARY KEY,
    username citext UNIQUE
);
-- Case-insensitive comparisons
SELECT * FROM users_ci WHERE email = 'John@Example.Com';

-- Example usage: intarray
SELECT ARRAY[1,2,3] & ARRAY[2,3,4];     -- Intersection
SELECT ARRAY[1,2,3] | ARRAY[4,5,6];     -- Union
SELECT ARRAY[1,2,3] @> ARRAY[2];        -- Contains
SELECT uniq(ARRAY[1,2,2,3,3,3]);        -- Unique elements

-- Drop extension
DROP EXTENSION IF EXISTS extension_name CASCADE;
""", language="sql")
        
    with ref_tabs[4]:  # Best Practices
        st.subheader("💡 PostgreSQL Best Practices")
        
        bp_col1, bp_col2 = st.columns(2)
        
        with bp_col1:
            st.markdown("""
            **Performance Best Practices**
            - ✅ Use EXPLAIN ANALYZE to understand queries
            - ✅ Create indexes on foreign keys
            - ✅ Use partial indexes for filtered queries
            - ✅ Consider BRIN indexes for time-series data
            - ✅ Use JSONB instead of JSON for better performance
            - ✅ Partition large tables by date or key
            - ✅ Regular VACUUM and ANALYZE
            - ✅ Monitor and tune autovacuum settings
            
            **Query Optimization**
            - ✅ Use EXISTS instead of IN for subqueries
            - ✅ Avoid SELECT * in production
            - ✅ Use UNION ALL instead of UNION when possible
            - ✅ Be careful with CTEs (not always optimized)
            - ✅ Use proper data types (don't use TEXT for everything)
            - ✅ Consider materialized views for complex queries
            """)
            
        with bp_col2:
            st.markdown("""
            **Database Design**
            - ✅ Use UUID for distributed systems
            - ✅ Normalize data but consider read performance
            - ✅ Use CHECK constraints for data validation
            - ✅ Implement proper foreign key constraints
            - ✅ Use schemas for logical separation
            - ✅ Consider using ENUM types for fixed values
            
            **Security & Maintenance**
            - ✅ Use roles for permission management
            - ✅ Enable SSL for connections
            - ✅ Regular backups with pg_dump or streaming
            - ✅ Monitor disk space and table bloat
            - ✅ Use connection pooling (pgBouncer/PgPool)
            - ✅ Keep PostgreSQL version updated
            """)

# Common Patterns
st.markdown("---")
st.header("🎯 Common PostgreSQL Patterns")

pattern_col1, pattern_col2 = st.columns(2)

with pattern_col1:
    st.subheader("Upsert Pattern")
    st.code("""
-- Insert or update
INSERT INTO users (email, name, last_seen)
VALUES ('user@example.com', 'John Doe', NOW())
ON CONFLICT (email) 
DO UPDATE SET 
    name = EXCLUDED.name,
    last_seen = EXCLUDED.last_seen,
    update_count = users.update_count + 1;

-- Insert or ignore
INSERT INTO users (email, name)
VALUES ('user@example.com', 'John Doe')
ON CONFLICT (email) DO NOTHING;

-- Conditional upsert
INSERT INTO inventory (product_id, quantity)
VALUES (123, 10)
ON CONFLICT (product_id) 
DO UPDATE SET quantity = inventory.quantity + EXCLUDED.quantity
WHERE inventory.quantity + EXCLUDED.quantity >= 0;
""", language="sql")

with pattern_col2:
    st.subheader("Audit Trail Pattern")
    st.code("""
-- Audit trigger function
CREATE OR REPLACE FUNCTION audit_trigger_function()
RETURNS TRIGGER AS $$
BEGIN
    INSERT INTO audit_log (
        table_name,
        operation,
        user_name,
        changed_at,
        old_data,
        new_data
    ) VALUES (
        TG_TABLE_NAME,
        TG_OP,
        current_user,
        current_timestamp,
        to_jsonb(OLD),
        to_jsonb(NEW)
    );
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Apply to table
CREATE TRIGGER audit_trigger
AFTER INSERT OR UPDATE OR DELETE ON your_table
FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();
""", language="sql")

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use the search box to quickly find PostgreSQL-specific features, functions, and optimization tips!")