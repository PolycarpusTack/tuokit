# Multi-Role Cheat Sheet Analysis

## Overview
This analysis structures cheat sheets to serve three key audiences: Developers, Support Staff, and Analysts. Each section focuses on practical, everyday tasks with quick wins and reusable templates.

## SQL Cheat Sheet Structure

### For Support Staff

#### 1. Common Troubleshooting Queries
```sql
-- Check running queries
SELECT pid, query, state, query_start 
FROM pg_stat_activity 
WHERE state != 'idle' 
ORDER BY query_start;

-- Find blocked queries
SELECT blocked_locks.pid AS blocked_pid,
       blocking_locks.pid AS blocking_pid,
       blocked_activity.query AS blocked_query
FROM pg_locks blocked_locks
JOIN pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
JOIN pg_locks blocking_locks ON blocking_locks.locktype = blocked_locks.locktype
WHERE NOT blocked_locks.granted;

-- Database size check
SELECT datname, pg_size_pretty(pg_database_size(datname)) as size
FROM pg_database
ORDER BY pg_database_size(datname) DESC;
```

#### 2. User Issues Resolution
```sql
-- Check user permissions
SELECT * FROM information_schema.role_table_grants 
WHERE grantee = 'username';

-- Reset user password (PostgreSQL)
ALTER USER username WITH PASSWORD 'new_password';

-- Unlock account (Oracle)
ALTER USER username ACCOUNT UNLOCK;
```

#### 3. System Health Checks
```sql
-- Connection count
SELECT count(*) FROM pg_stat_activity;

-- Table bloat check
SELECT schemaname, tablename, 
       pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
LIMIT 10;
```

### For Analysts

#### 1. Report Generation Patterns
```sql
-- Monthly summary template
WITH monthly_data AS (
    SELECT 
        DATE_TRUNC('month', created_date) as month,
        COUNT(*) as total_count,
        SUM(amount) as total_amount
    FROM transactions
    WHERE created_date >= CURRENT_DATE - INTERVAL '12 months'
    GROUP BY 1
)
SELECT 
    TO_CHAR(month, 'YYYY-MM') as month_label,
    total_count,
    total_amount,
    LAG(total_amount) OVER (ORDER BY month) as prev_month,
    ROUND(((total_amount - LAG(total_amount) OVER (ORDER BY month)) / 
           LAG(total_amount) OVER (ORDER BY month)) * 100, 2) as growth_pct
FROM monthly_data
ORDER BY month;
```

#### 2. Data Export Techniques
```sql
-- CSV export with headers (PostgreSQL)
COPY (
    SELECT column1, column2, column3
    FROM your_table
    WHERE conditions
) TO '/tmp/export.csv' WITH CSV HEADER;

-- Excel-friendly date format
SELECT TO_CHAR(date_column, 'MM/DD/YYYY') as formatted_date
FROM your_table;
```

#### 3. Common Business Metrics
```sql
-- Customer retention rate
WITH cohorts AS (
    SELECT 
        DATE_TRUNC('month', first_purchase_date) as cohort_month,
        customer_id
    FROM (
        SELECT customer_id, MIN(purchase_date) as first_purchase_date
        FROM purchases
        GROUP BY customer_id
    ) first_purchases
)
SELECT 
    cohort_month,
    COUNT(DISTINCT c.customer_id) as cohort_size,
    COUNT(DISTINCT p.customer_id) as retained,
    ROUND(COUNT(DISTINCT p.customer_id)::NUMERIC / COUNT(DISTINCT c.customer_id) * 100, 2) as retention_rate
FROM cohorts c
LEFT JOIN purchases p ON c.customer_id = p.customer_id 
    AND p.purchase_date >= c.cohort_month + INTERVAL '1 month'
    AND p.purchase_date < c.cohort_month + INTERVAL '2 months'
GROUP BY cohort_month
ORDER BY cohort_month;
```

### For All Roles

#### Quick Wins
```sql
-- Explain plan shortcut
EXPLAIN (ANALYZE, BUFFERS) your_query_here;

-- Quick row count estimate
SELECT reltuples::BIGINT FROM pg_class WHERE relname='table_name';

-- Date shortcuts
SELECT CURRENT_DATE - INTERVAL '7 days' as week_ago;
SELECT DATE_TRUNC('week', CURRENT_DATE) as week_start;
SELECT DATE_TRUNC('month', CURRENT_DATE) as month_start;
```

## XML Cheat Sheet Structure

### For Support Staff

#### 1. Common XML Validation
```xml
<!-- Check well-formedness -->
xmllint --noout file.xml

<!-- Validate against XSD -->
xmllint --schema schema.xsd file.xml --noout

<!-- Pretty print for debugging -->
xmllint --format file.xml
```

#### 2. Error Interpretation
```
Common XML Errors:
- "Premature end of data" → Missing closing tag
- "Invalid character" → Check encoding (UTF-8 vs ISO-8859-1)
- "Element not expected" → Schema validation error
- "Entity not defined" → Missing DTD/entity declaration
```

### For Analysts

#### 1. Data Extraction with XPath
```xpath
<!-- Extract all customer names -->
//customer/name/text()

<!-- Get orders over $100 -->
//order[amount > 100]

<!-- Count elements -->
count(//product)

<!-- Extract with conditions -->
//product[@category='electronics']/price
```

#### 2. XML to CSV Conversion
```python
import xml.etree.ElementTree as ET
import csv

# Parse XML
tree = ET.parse('data.xml')
root = tree.getroot()

# Extract to CSV
with open('output.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['ID', 'Name', 'Value'])  # Headers
    
    for item in root.findall('.//item'):
        writer.writerow([
            item.get('id'),
            item.find('name').text,
            item.find('value').text
        ])
```

## Python Cheat Sheet Structure

### For Support Staff

#### 1. Debugging Commands
```python
# Check Python version
import sys
print(sys.version)

# List installed packages
pip list

# Check module location
import module_name
print(module_name.__file__)

# Memory usage
import psutil
print(f"Memory: {psutil.virtual_memory().percent}%")
```

#### 2. Common Error Solutions
```python
# FileNotFoundError
import os
if os.path.exists(filepath):
    with open(filepath) as f:
        # process file
else:
    print(f"File not found: {filepath}")

# Permission Error handling
try:
    with open('/restricted/file', 'w') as f:
        f.write('data')
except PermissionError:
    print("No write permission. Check file permissions.")
```

### For Analysts

#### 1. Data Processing Templates
```python
import pandas as pd

# Read various formats
df_csv = pd.read_csv('file.csv')
df_excel = pd.read_excel('file.xlsx', sheet_name='Sheet1')
df_json = pd.read_json('file.json')

# Quick analysis
print(df.info())           # Column types and null counts
print(df.describe())       # Statistical summary
print(df.head())          # First 5 rows

# Common transformations
df['date'] = pd.to_datetime(df['date'])  # Convert to datetime
df['amount'] = df['amount'].astype(float)  # Convert to float
df_monthly = df.groupby(pd.Grouper(key='date', freq='M')).sum()  # Monthly aggregation
```

#### 2. Report Generation
```python
# Excel report with formatting
with pd.ExcelWriter('report.xlsx', engine='xlsxwriter') as writer:
    df.to_excel(writer, sheet_name='Data', index=False)
    
    # Get workbook and worksheet
    workbook = writer.book
    worksheet = writer.sheets['Data']
    
    # Add formats
    money_format = workbook.add_format({'num_format': '$#,##0.00'})
    percent_format = workbook.add_format({'num_format': '0.00%'})
    
    # Apply formats
    worksheet.set_column('B:B', 12, money_format)
    worksheet.set_column('C:C', 10, percent_format)
```

## Oracle Cheat Sheet Structure

### For Support Staff

#### 1. Session Management
```sql
-- View all sessions
SELECT sid, serial#, username, status, program
FROM v$session
WHERE username IS NOT NULL;

-- Kill a session
ALTER SYSTEM KILL SESSION 'sid,serial#' IMMEDIATE;

-- Check locked objects
SELECT l.session_id, o.object_name, o.object_type
FROM v$locked_object l
JOIN dba_objects o ON l.object_id = o.object_id;
```

#### 2. Tablespace Management
```sql
-- Check tablespace usage
SELECT tablespace_name,
       ROUND(SUM(bytes)/1024/1024/1024, 2) AS "Size GB",
       ROUND(SUM(bytes - NVL(free_space, 0))/1024/1024/1024, 2) AS "Used GB"
FROM (
    SELECT tablespace_name, SUM(bytes) bytes
    FROM dba_data_files
    GROUP BY tablespace_name
) d
LEFT JOIN (
    SELECT tablespace_name, SUM(bytes) free_space
    FROM dba_free_space
    GROUP BY tablespace_name
) f USING (tablespace_name)
GROUP BY tablespace_name;
```

### For Analysts

#### 1. Analytical Functions
```sql
-- Running totals
SELECT order_date,
       amount,
       SUM(amount) OVER (ORDER BY order_date) as running_total
FROM orders;

-- Ranking
SELECT customer_name,
       total_sales,
       RANK() OVER (ORDER BY total_sales DESC) as sales_rank,
       NTILE(4) OVER (ORDER BY total_sales DESC) as quartile
FROM customer_summary;

-- Period-over-period comparison
SELECT 
    month,
    sales,
    LAG(sales, 12) OVER (ORDER BY month) as same_month_last_year,
    ROUND((sales - LAG(sales, 12) OVER (ORDER BY month)) / 
          LAG(sales, 12) OVER (ORDER BY month) * 100, 2) as yoy_growth
FROM monthly_sales;
```

## PostgreSQL Cheat Sheet Structure

### For Support Staff

#### 1. Performance Troubleshooting
```sql
-- Slow query identification
SELECT query, mean_exec_time, calls
FROM pg_stat_statements
ORDER BY mean_exec_time DESC
LIMIT 10;

-- Index usage
SELECT schemaname, tablename, indexname, idx_scan
FROM pg_stat_user_indexes
ORDER BY idx_scan;

-- Vacuum status
SELECT schemaname, tablename, last_vacuum, last_autovacuum
FROM pg_stat_user_tables
WHERE last_autovacuum < CURRENT_DATE - INTERVAL '7 days';
```

#### 2. Backup and Recovery
```bash
# Backup database
pg_dump -h localhost -U username -d database_name -f backup.sql

# Backup with compression
pg_dump -h localhost -U username -d database_name -Fc -f backup.dump

# Restore
pg_restore -h localhost -U username -d database_name backup.dump
```

### For Analysts

#### 1. Window Functions
```sql
-- Moving averages
SELECT date,
       value,
       AVG(value) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) as ma7
FROM daily_metrics;

-- Cumulative percentages
SELECT category,
       count,
       SUM(count) OVER (ORDER BY count DESC) as cumulative_count,
       ROUND(100.0 * SUM(count) OVER (ORDER BY count DESC) / SUM(count) OVER (), 2) as cumulative_pct
FROM category_counts;
```

#### 2. JSON Data Handling
```sql
-- Extract JSON fields
SELECT 
    data->>'customer_name' as customer,
    (data->>'order_total')::numeric as total
FROM orders_json
WHERE data->>'status' = 'completed';

-- Aggregate JSON arrays
SELECT 
    customer_id,
    jsonb_agg(
        jsonb_build_object(
            'date', order_date,
            'total', order_total
        ) ORDER BY order_date
    ) as order_history
FROM orders
GROUP BY customer_id;
```

## Universal Quick Reference Card

### Time Savers for All Roles

#### SQL Date Shortcuts
```sql
-- Common date calculations
TODAY:           CURRENT_DATE
YESTERDAY:       CURRENT_DATE - 1
THIS_WEEK:       DATE_TRUNC('week', CURRENT_DATE)
THIS_MONTH:      DATE_TRUNC('month', CURRENT_DATE)
THIS_YEAR:       DATE_TRUNC('year', CURRENT_DATE)
LAST_30_DAYS:    CURRENT_DATE - INTERVAL '30 days'
```

#### Python One-Liners
```python
# Read file to list
lines = [line.strip() for line in open('file.txt')]

# Quick CSV to dict
import csv
data = list(csv.DictReader(open('file.csv')))

# Format numbers
f"{1234567.89:,.2f}"  # 1,234,567.89
f"{0.1234:.2%}"       # 12.34%
```

#### Performance Quick Checks
```sql
-- PostgreSQL
EXPLAIN (ANALYZE, BUFFERS) your_query;

-- Oracle
SET AUTOTRACE ON
your_query;

-- MySQL
EXPLAIN your_query;
SHOW PROFILE FOR QUERY 1;
```

## Visual Guide Templates

### For Complex Joins (All Roles)
```
INNER JOIN: [A ∩ B]
LEFT JOIN:  [A ∪ (A ∩ B)]
RIGHT JOIN: [(A ∩ B) ∪ B]
FULL JOIN:  [A ∪ B]

Example:
customers (c)          orders (o)
+----+-------+        +----+-----+--------+
| id | name  |        | id | c_id| amount |
+----+-------+        +----+-----+--------+
| 1  | Alice |        | 1  | 1   | 100    |
| 2  | Bob   |        | 2  | 1   | 200    |
| 3  | Carol |        | 3  | 2   | 150    |
+----+-------+        +----+-----+--------+

LEFT JOIN Result:
+-------+--------+
| name  | amount |
+-------+--------+
| Alice | 100    |
| Alice | 200    |
| Bob   | 150    |
| Carol | NULL   |  ← No orders
+-------+--------+
```

## Implementation Recommendations

1. **Format**: Create laminated reference cards for physical desks
2. **Digital**: Interactive HTML with copy buttons for code snippets
3. **Organization**: Tab-separated sections for each role
4. **Updates**: Quarterly reviews based on support tickets and user feedback
5. **Customization**: Department-specific versions with relevant examples

## Success Metrics

- Reduction in support tickets for common issues
- Increased self-service problem resolution
- Faster report generation turnaround
- Improved query performance across teams
- Higher adoption of best practices