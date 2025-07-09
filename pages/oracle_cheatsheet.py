"""
TuoKit - Oracle Cheat Sheet
Comprehensive Oracle reference for developers, DBAs, and analysts
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="Oracle Cheat Sheet - TuoKit",
    page_icon="🔶",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="oracle_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🔶 Oracle Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Oracle SQL, PL/SQL, and administration reference for all roles
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search Oracle topics...", placeholder="Try: PL/SQL, tablespace, performance, MERGE...")

# Create role-based tabs
role_tab1, role_tab2, role_tab3, role_tab4 = st.tabs([
    "👨‍💻 Developer", 
    "📊 Analyst", 
    "🛠️ DBA/Support", 
    "📚 Reference"
])

# Helper function
def show_oracle_example(title, description, code, show=True):
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
        "PL/SQL", "Advanced SQL", "Packages", "Performance", "Error Handling"
    ])
    
    with dev_tabs[0]:  # PL/SQL
        st.subheader("📝 PL/SQL Fundamentals")
        
        show_oracle_example(
            "PL/SQL Block Structure",
            "Basic anonymous block and stored procedures",
            """-- Anonymous PL/SQL block
DECLARE
    -- Variable declarations
    v_employee_name VARCHAR2(100);
    v_salary NUMBER(10,2);
    v_bonus NUMBER(10,2) := 0;
    c_tax_rate CONSTANT NUMBER := 0.3;
    
    -- Cursor declaration
    CURSOR c_employees IS
        SELECT employee_id, first_name, last_name, salary
        FROM employees
        WHERE department_id = 50;
        
    -- Exception declaration
    e_salary_too_high EXCEPTION;
    PRAGMA EXCEPTION_INIT(e_salary_too_high, -20001);
    
BEGIN
    -- Main logic
    FOR emp_rec IN c_employees LOOP
        v_employee_name := emp_rec.first_name || ' ' || emp_rec.last_name;
        v_salary := emp_rec.salary;
        
        -- Calculate bonus
        IF v_salary > 10000 THEN
            v_bonus := v_salary * 0.15;
        ELSIF v_salary > 5000 THEN
            v_bonus := v_salary * 0.10;
        ELSE
            v_bonus := v_salary * 0.05;
        END IF;
        
        -- Output
        DBMS_OUTPUT.PUT_LINE(v_employee_name || ' - Bonus: $' || v_bonus);
    END LOOP;
    
    -- Commit changes
    COMMIT;
    
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE('No employees found');
    WHEN TOO_MANY_ROWS THEN
        DBMS_OUTPUT.PUT_LINE('Query returned multiple rows');
    WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error: ' || SQLERRM);
        ROLLBACK;
END;
/

-- Stored procedure
CREATE OR REPLACE PROCEDURE update_salary(
    p_employee_id IN NUMBER,
    p_percentage IN NUMBER,
    p_new_salary OUT NUMBER
) AS
    v_current_salary NUMBER;
BEGIN
    -- Get current salary
    SELECT salary INTO v_current_salary
    FROM employees
    WHERE employee_id = p_employee_id;
    
    -- Calculate new salary
    p_new_salary := v_current_salary * (1 + p_percentage/100);
    
    -- Update salary
    UPDATE employees
    SET salary = p_new_salary,
        last_update_date = SYSDATE
    WHERE employee_id = p_employee_id;
    
    COMMIT;
    
EXCEPTION
    WHEN NO_DATA_FOUND THEN
        RAISE_APPLICATION_ERROR(-20001, 'Employee not found');
END update_salary;
/

-- Function
CREATE OR REPLACE FUNCTION calculate_tax(
    p_salary IN NUMBER,
    p_tax_rate IN NUMBER DEFAULT 0.3
) RETURN NUMBER AS
    v_tax NUMBER;
BEGIN
    v_tax := p_salary * p_tax_rate;
    RETURN v_tax;
END calculate_tax;
/""",
            matches_search("PL/SQL procedure function block cursor", search_query)
        )
        
        show_oracle_example(
            "Collections and Bulk Operations",
            "Working with arrays and bulk processing",
            """-- Define collection types
DECLARE
    -- Associative array (index by)
    TYPE t_emp_names IS TABLE OF VARCHAR2(100) INDEX BY PLS_INTEGER;
    v_names t_emp_names;
    
    -- Nested table
    TYPE t_numbers IS TABLE OF NUMBER;
    v_numbers t_numbers := t_numbers(10, 20, 30, 40, 50);
    
    -- VARRAY
    TYPE t_cities IS VARRAY(5) OF VARCHAR2(50);
    v_cities t_cities := t_cities('New York', 'London', 'Tokyo');
    
    -- Record type
    TYPE t_employee IS RECORD (
        id NUMBER,
        name VARCHAR2(100),
        salary NUMBER
    );
    v_employee t_employee;
    
BEGIN
    -- Populate associative array
    v_names(1) := 'John Doe';
    v_names(2) := 'Jane Smith';
    v_names(100) := 'Bob Johnson';  -- Sparse array
    
    -- Loop through associative array
    DECLARE
        v_index PLS_INTEGER;
    BEGIN
        v_index := v_names.FIRST;
        WHILE v_index IS NOT NULL LOOP
            DBMS_OUTPUT.PUT_LINE(v_index || ': ' || v_names(v_index));
            v_index := v_names.NEXT(v_index);
        END LOOP;
    END;
    
    -- Bulk collect
    DECLARE
        TYPE t_emp_array IS TABLE OF employees%ROWTYPE;
        v_employees t_emp_array;
    BEGIN
        SELECT * BULK COLLECT INTO v_employees
        FROM employees
        WHERE department_id = 50;
        
        -- Process collected data
        FOR i IN 1..v_employees.COUNT LOOP
            DBMS_OUTPUT.PUT_LINE(v_employees(i).first_name);
        END LOOP;
    END;
    
    -- FORALL bulk DML
    DECLARE
        TYPE t_id_array IS TABLE OF NUMBER;
        TYPE t_salary_array IS TABLE OF NUMBER;
        v_ids t_id_array := t_id_array(100, 101, 102, 103);
        v_salaries t_salary_array := t_salary_array(5000, 5500, 6000, 6500);
    BEGIN
        FORALL i IN 1..v_ids.COUNT
            UPDATE employees
            SET salary = v_salaries(i)
            WHERE employee_id = v_ids(i);
            
        DBMS_OUTPUT.PUT_LINE(SQL%ROWCOUNT || ' rows updated');
    END;
END;
/

-- Bulk collect with limit
DECLARE
    TYPE t_emp_array IS TABLE OF employees%ROWTYPE;
    v_employees t_emp_array;
    CURSOR c_all_employees IS SELECT * FROM employees;
BEGIN
    OPEN c_all_employees;
    LOOP
        FETCH c_all_employees BULK COLLECT INTO v_employees LIMIT 100;
        
        -- Process batch
        FOR i IN 1..v_employees.COUNT LOOP
            -- Process each employee
            NULL;
        END LOOP;
        
        EXIT WHEN c_all_employees%NOTFOUND;
    END LOOP;
    CLOSE c_all_employees;
END;
/""",
            matches_search("collection array bulk collect FORALL", search_query)
        )
        
    with dev_tabs[1]:  # Advanced SQL
        st.subheader("🚀 Oracle-Specific SQL Features")
        
        show_oracle_example(
            "Hierarchical Queries",
            "Query hierarchical data with CONNECT BY",
            """-- Basic hierarchical query
SELECT employee_id, first_name, last_name, manager_id, LEVEL
FROM employees
START WITH manager_id IS NULL  -- Start with top-level managers
CONNECT BY PRIOR employee_id = manager_id
ORDER SIBLINGS BY last_name;

-- With hierarchy path
SELECT 
    LPAD(' ', 2 * (LEVEL - 1)) || first_name || ' ' || last_name AS employee_hierarchy,
    LEVEL as org_level,
    SYS_CONNECT_BY_PATH(last_name, '/') AS path,
    CONNECT_BY_ROOT last_name AS top_manager,
    CONNECT_BY_ISLEAF AS is_leaf_node
FROM employees
START WITH manager_id IS NULL
CONNECT BY PRIOR employee_id = manager_id;

-- Detect cycles in hierarchical data
SELECT employee_id, manager_id, LEVEL
FROM employees
START WITH employee_id = 100
CONNECT BY NOCYCLE PRIOR employee_id = manager_id;

-- MERGE statement (UPSERT)
MERGE INTO target_table t
USING (
    SELECT employee_id, salary, department_id
    FROM source_table
    WHERE update_date = TRUNC(SYSDATE)
) s
ON (t.employee_id = s.employee_id)
WHEN MATCHED THEN
    UPDATE SET 
        t.salary = s.salary,
        t.department_id = s.department_id,
        t.last_update = SYSDATE
    WHERE t.salary != s.salary  -- Only update if changed
    DELETE WHERE s.department_id = 999  -- Delete terminated employees
WHEN NOT MATCHED THEN
    INSERT (employee_id, salary, department_id, created_date)
    VALUES (s.employee_id, s.salary, s.department_id, SYSDATE);

-- PIVOT and UNPIVOT
-- Convert rows to columns
SELECT * FROM (
    SELECT department_name, job_id, salary
    FROM employees e
    JOIN departments d ON e.department_id = d.department_id
)
PIVOT (
    AVG(salary) AS avg_salary,
    COUNT(*) AS emp_count
    FOR job_id IN ('IT_PROG' AS programmer, 'SA_REP' AS sales_rep, 'ST_CLERK' AS clerk)
);

-- MODEL clause for spreadsheet-like calculations
SELECT employee_id, year, sales, projected_sales
FROM sales_data
MODEL
    PARTITION BY (employee_id)
    DIMENSION BY (year)
    MEASURES (sales, 0 AS projected_sales)
    RULES (
        projected_sales[2024] = sales[2023] * 1.1,
        projected_sales[2025] = sales[2023] * 1.2
    );""",
            matches_search("hierarchical CONNECT BY MERGE PIVOT MODEL", search_query)
        )
        
    with dev_tabs[2]:  # Packages
        st.subheader("📦 PL/SQL Packages")
        
        show_oracle_example(
            "Package Creation and Usage",
            "Creating modular code with packages",
            """-- Package specification
CREATE OR REPLACE PACKAGE employee_pkg AS
    -- Constants
    c_max_salary CONSTANT NUMBER := 100000;
    c_min_salary CONSTANT NUMBER := 30000;
    
    -- Types
    TYPE t_employee_record IS RECORD (
        employee_id NUMBER,
        full_name VARCHAR2(200),
        salary NUMBER
    );
    
    TYPE t_employee_table IS TABLE OF t_employee_record;
    
    -- Cursors
    CURSOR c_high_earners IS
        SELECT * FROM employees
        WHERE salary > 10000
        ORDER BY salary DESC;
    
    -- Procedures
    PROCEDURE hire_employee(
        p_first_name IN VARCHAR2,
        p_last_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_job_id IN VARCHAR2,
        p_salary IN NUMBER,
        p_employee_id OUT NUMBER
    );
    
    PROCEDURE adjust_salary(
        p_employee_id IN NUMBER,
        p_adjustment_pct IN NUMBER
    );
    
    -- Functions
    FUNCTION get_employee_details(
        p_employee_id IN NUMBER
    ) RETURN t_employee_record;
    
    FUNCTION calculate_bonus(
        p_employee_id IN NUMBER,
        p_performance_rating IN NUMBER
    ) RETURN NUMBER;
    
END employee_pkg;
/

-- Package body
CREATE OR REPLACE PACKAGE BODY employee_pkg AS
    
    -- Private variables
    g_last_error VARCHAR2(1000);
    
    -- Private procedures
    PROCEDURE log_error(p_error_msg IN VARCHAR2) IS
        PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
        INSERT INTO error_log (error_date, error_message, package_name)
        VALUES (SYSDATE, p_error_msg, 'EMPLOYEE_PKG');
        COMMIT;
    END log_error;
    
    -- Public procedure implementations
    PROCEDURE hire_employee(
        p_first_name IN VARCHAR2,
        p_last_name IN VARCHAR2,
        p_email IN VARCHAR2,
        p_job_id IN VARCHAR2,
        p_salary IN NUMBER,
        p_employee_id OUT NUMBER
    ) IS
    BEGIN
        -- Validate salary
        IF p_salary < c_min_salary OR p_salary > c_max_salary THEN
            RAISE_APPLICATION_ERROR(-20001, 
                'Salary must be between ' || c_min_salary || ' and ' || c_max_salary);
        END IF;
        
        -- Get next employee ID
        SELECT employees_seq.NEXTVAL INTO p_employee_id FROM dual;
        
        -- Insert employee
        INSERT INTO employees (
            employee_id, first_name, last_name, email, 
            job_id, salary, hire_date
        ) VALUES (
            p_employee_id, p_first_name, p_last_name, p_email,
            p_job_id, p_salary, SYSDATE
        );
        
        COMMIT;
        
    EXCEPTION
        WHEN OTHERS THEN
            log_error(SQLERRM);
            RAISE;
    END hire_employee;
    
    FUNCTION calculate_bonus(
        p_employee_id IN NUMBER,
        p_performance_rating IN NUMBER
    ) RETURN NUMBER IS
        v_salary NUMBER;
        v_bonus NUMBER;
    BEGIN
        SELECT salary INTO v_salary
        FROM employees
        WHERE employee_id = p_employee_id;
        
        v_bonus := v_salary * (p_performance_rating / 100);
        
        RETURN v_bonus;
        
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            RETURN 0;
    END calculate_bonus;
    
    -- Package initialization
BEGIN
    DBMS_OUTPUT.PUT_LINE('Employee package initialized');
END employee_pkg;
/

-- Using the package
DECLARE
    v_emp_id NUMBER;
    v_bonus NUMBER;
BEGIN
    -- Hire new employee
    employee_pkg.hire_employee(
        p_first_name => 'John',
        p_last_name => 'Doe',
        p_email => 'john.doe@company.com',
        p_job_id => 'IT_PROG',
        p_salary => 75000,
        p_employee_id => v_emp_id
    );
    
    -- Calculate bonus
    v_bonus := employee_pkg.calculate_bonus(v_emp_id, 15);
    
    DBMS_OUTPUT.PUT_LINE('Employee ' || v_emp_id || ' hired with bonus: $' || v_bonus);
END;
/""",
            matches_search("package specification body procedure function", search_query)
        )

# Analyst Tab
with role_tab2:
    st.header("📊 Analyst Toolkit")
    
    analyst_tabs = st.tabs([
        "Analytics", "Window Functions", "Reports", "Data Export"
    ])
    
    with analyst_tabs[0]:  # Analytics
        st.subheader("📈 Analytical Queries")
        
        show_oracle_example(
            "Advanced Analytics Functions",
            "Oracle's powerful analytical capabilities",
            """-- Ranking and percentiles
SELECT 
    employee_id,
    first_name,
    last_name,
    salary,
    department_id,
    -- Ranking functions
    RANK() OVER (ORDER BY salary DESC) AS salary_rank,
    DENSE_RANK() OVER (ORDER BY salary DESC) AS salary_dense_rank,
    ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
    PERCENT_RANK() OVER (ORDER BY salary) AS salary_percentile,
    NTILE(4) OVER (ORDER BY salary DESC) AS salary_quartile,
    -- Department-specific ranking
    RANK() OVER (PARTITION BY department_id ORDER BY salary DESC) AS dept_salary_rank,
    -- Cumulative distribution
    CUME_DIST() OVER (ORDER BY salary) AS cumulative_dist,
    -- Ratio to report
    RATIO_TO_REPORT(salary) OVER () AS salary_ratio,
    RATIO_TO_REPORT(salary) OVER (PARTITION BY department_id) AS dept_salary_ratio
FROM employees
WHERE department_id IN (50, 60, 80);

-- Statistical functions
SELECT 
    department_id,
    COUNT(*) AS emp_count,
    AVG(salary) AS avg_salary,
    MEDIAN(salary) AS median_salary,
    STDDEV(salary) AS salary_stddev,
    VARIANCE(salary) AS salary_variance,
    -- Percentiles
    PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY salary) AS salary_q1,
    PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY salary) AS salary_q3,
    PERCENTILE_DISC(0.5) WITHIN GROUP (ORDER BY salary) AS salary_median_disc,
    -- Mode
    STATS_MODE(salary) AS mode_salary,
    -- Correlation
    CORR(salary, commission_pct) AS salary_commission_corr
FROM employees
GROUP BY department_id
HAVING COUNT(*) > 5;

-- Time-based analytics with LAG/LEAD
WITH monthly_sales AS (
    SELECT 
        TO_CHAR(order_date, 'YYYY-MM') AS month,
        SUM(order_total) AS total_sales,
        COUNT(DISTINCT customer_id) AS unique_customers,
        COUNT(*) AS order_count
    FROM orders
    GROUP BY TO_CHAR(order_date, 'YYYY-MM')
)
SELECT 
    month,
    total_sales,
    unique_customers,
    order_count,
    -- Previous period comparisons
    LAG(total_sales, 1) OVER (ORDER BY month) AS prev_month_sales,
    LAG(total_sales, 12) OVER (ORDER BY month) AS same_month_last_year,
    -- Growth calculations
    ROUND((total_sales - LAG(total_sales, 1) OVER (ORDER BY month)) / 
          NULLIF(LAG(total_sales, 1) OVER (ORDER BY month), 0) * 100, 2) AS mom_growth,
    ROUND((total_sales - LAG(total_sales, 12) OVER (ORDER BY month)) / 
          NULLIF(LAG(total_sales, 12) OVER (ORDER BY month), 0) * 100, 2) AS yoy_growth,
    -- Moving averages
    AVG(total_sales) OVER (ORDER BY month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS ma3_sales,
    AVG(total_sales) OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) AS ma12_sales
FROM monthly_sales
ORDER BY month;""",
            matches_search("analytics rank percentile median statistics", search_query)
        )
        
    with analyst_tabs[1]:  # Window Functions
        st.subheader("🪟 Window Functions")
        
        show_oracle_example(
            "Complex Window Functions",
            "Advanced windowing techniques",
            """-- Running totals and averages
SELECT 
    order_date,
    customer_id,
    order_total,
    -- Running totals
    SUM(order_total) OVER (ORDER BY order_date) AS running_total_all,
    SUM(order_total) OVER (PARTITION BY customer_id ORDER BY order_date) AS customer_running_total,
    -- Range-based windows
    SUM(order_total) OVER (ORDER BY order_date RANGE BETWEEN INTERVAL '7' DAY PRECEDING AND CURRENT ROW) AS sales_last_7_days,
    COUNT(*) OVER (ORDER BY order_date RANGE BETWEEN INTERVAL '30' DAY PRECEDING AND CURRENT ROW) AS orders_last_30_days,
    -- Row-based windows
    AVG(order_total) OVER (ORDER BY order_date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS avg_last_7_orders,
    -- First and last values
    FIRST_VALUE(order_total) OVER (PARTITION BY customer_id ORDER BY order_date) AS first_order_amount,
    LAST_VALUE(order_total) OVER (PARTITION BY customer_id ORDER BY order_date 
        ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_order_amount,
    -- Nth value
    NTH_VALUE(order_total, 2) OVER (PARTITION BY customer_id ORDER BY order_total DESC) AS second_highest_order
FROM orders
WHERE order_date >= ADD_MONTHS(SYSDATE, -12);

-- Window clause reuse
SELECT 
    employee_id,
    department_id,
    salary,
    hire_date,
    -- Using named windows
    AVG(salary) OVER dept_window AS dept_avg_salary,
    COUNT(*) OVER dept_window AS dept_employee_count,
    MIN(hire_date) OVER dept_window AS dept_first_hire,
    MAX(hire_date) OVER dept_window AS dept_last_hire,
    LISTAGG(first_name, ', ') WITHIN GROUP (ORDER BY hire_date) OVER dept_window AS dept_employees
FROM employees
WINDOW dept_window AS (PARTITION BY department_id);

-- Pattern matching (MATCH_RECOGNIZE)
SELECT * FROM orders
MATCH_RECOGNIZE (
    PARTITION BY customer_id
    ORDER BY order_date
    MEASURES 
        FIRST(order_date) AS start_date,
        LAST(order_date) AS end_date,
        COUNT(*) AS order_count,
        SUM(order_total) AS total_spent
    ONE ROW PER MATCH
    PATTERN (initial increase+ stable* decline+)
    DEFINE
        initial AS order_total > 0,
        increase AS order_total > PREV(order_total) * 1.1,
        stable AS order_total BETWEEN PREV(order_total) * 0.9 AND PREV(order_total) * 1.1,
        decline AS order_total < PREV(order_total) * 0.9
) MR
WHERE order_count > 5;""",
            matches_search("window function running total MATCH_RECOGNIZE pattern", search_query)
        )

# DBA/Support Tab
with role_tab3:
    st.header("🛠️ DBA & Support Toolkit")
    
    dba_tabs = st.tabs([
        "Performance", "Sessions", "Storage", "Backup", "Troubleshooting"
    ])
    
    with dba_tabs[0]:  # Performance
        st.subheader("⚡ Performance Monitoring")
        
        show_oracle_example(
            "Performance Diagnostics",
            "Monitor and tune database performance",
            """-- Top SQL by elapsed time
SELECT * FROM (
    SELECT 
        sql_id,
        sql_text,
        elapsed_time/1000000 AS elapsed_seconds,
        executions,
        ROUND(elapsed_time/NULLIF(executions,0)/1000000, 4) AS avg_elapsed_sec,
        cpu_time/1000000 AS cpu_seconds,
        buffer_gets,
        disk_reads,
        rows_processed,
        last_active_time
    FROM v$sql
    WHERE elapsed_time > 0
    ORDER BY elapsed_time DESC
) WHERE ROWNUM <= 20;

-- Current wait events
SELECT 
    event,
    COUNT(*) AS session_count,
    SUM(time_waited) AS total_time_waited,
    AVG(time_waited) AS avg_time_waited
FROM v$session_wait
WHERE wait_class != 'Idle'
GROUP BY event
ORDER BY total_time_waited DESC;

-- AWR top events (requires diagnostic pack)
SELECT * FROM (
    SELECT 
        event_name,
        total_waits,
        time_waited_micro/1000000 AS time_waited_sec,
        ROUND(time_waited_micro/total_waits/1000, 2) AS avg_wait_ms
    FROM dba_hist_system_event
    WHERE snap_id = (SELECT MAX(snap_id) FROM dba_hist_snapshot)
    AND wait_class != 'Idle'
    ORDER BY time_waited_micro DESC
) WHERE ROWNUM <= 10;

-- Execution plan for a SQL_ID
SELECT * FROM table(DBMS_XPLAN.DISPLAY_CURSOR('sql_id_here', NULL, 'ALLSTATS LAST'));

-- SQL tuning advice
SELECT 
    sql_id,
    type,
    message,
    more_info
FROM v$sql_monitor_sesstat
WHERE sql_id = 'your_sql_id';

-- Index usage monitoring
SELECT 
    owner,
    index_name,
    table_name,
    monitoring,
    used,
    start_monitoring,
    end_monitoring
FROM v$object_usage
WHERE used = 'NO'
ORDER BY owner, table_name, index_name;

-- Missing index suggestions
SELECT 
    d.sql_id,
    d.object_owner,
    d.object_name,
    d.object_type,
    d.partition_start,
    d.partition_stop,
    d.object_alias,
    d.qblock_name,
    d.remarks
FROM v$sql_plan d
WHERE d.operation = 'TABLE ACCESS'
AND d.options = 'FULL'
AND d.object_owner NOT IN ('SYS', 'SYSTEM')
AND EXISTS (
    SELECT 1 FROM v$sql s
    WHERE s.sql_id = d.sql_id
    AND s.elapsed_time > 1000000  -- More than 1 second
);""",
            matches_search("performance AWR SQL tuning wait events", search_query)
        )
        
    with dba_tabs[1]:  # Sessions
        st.subheader("👥 Session Management")
        
        show_oracle_example(
            "Session Monitoring and Management",
            "Monitor and manage database sessions",
            """-- Active sessions with current SQL
SELECT 
    s.sid,
    s.serial#,
    s.username,
    s.status,
    s.osuser,
    s.machine,
    s.program,
    s.logon_time,
    s.last_call_et AS seconds_since_last_call,
    s.sql_id,
    s.sql_child_number,
    q.sql_text,
    s.blocking_session,
    s.event,
    s.wait_class,
    s.state
FROM v$session s
LEFT JOIN v$sql q ON s.sql_id = q.sql_id AND s.sql_child_number = q.child_number
WHERE s.type = 'USER'
AND s.status = 'ACTIVE'
ORDER BY s.last_call_et DESC;

-- Blocking sessions tree
WITH blocking_tree AS (
    SELECT 
        LEVEL AS lvl,
        s.sid,
        s.serial#,
        s.username,
        s.blocking_session,
        s.event,
        s.seconds_in_wait,
        s.sql_id
    FROM v$session s
    START WITH s.blocking_session IS NULL AND s.sid IN (
        SELECT DISTINCT blocking_session FROM v$session WHERE blocking_session IS NOT NULL
    )
    CONNECT BY PRIOR s.sid = s.blocking_session
)
SELECT 
    LPAD(' ', 2 * (lvl - 1)) || 'SID: ' || sid AS blocking_tree,
    serial#,
    username,
    event,
    seconds_in_wait,
    sql_id
FROM blocking_tree
ORDER BY lvl, sid;

-- Kill session commands
-- Generate kill commands for blocking sessions
SELECT 
    'ALTER SYSTEM KILL SESSION ''' || sid || ',' || serial# || ''' IMMEDIATE;' AS kill_command
FROM v$session
WHERE sid IN (
    SELECT DISTINCT blocking_session 
    FROM v$session 
    WHERE blocking_session IS NOT NULL
);

-- Resource consumption by session
SELECT 
    s.sid,
    s.serial#,
    s.username,
    s.program,
    ss.value AS cpu_used,
    sm.value/1024/1024 AS memory_mb,
    si.block_gets + si.consistent_gets AS logical_reads,
    si.physical_reads
FROM v$session s
JOIN v$sesstat ss ON s.sid = ss.sid
JOIN v$statname sn ON ss.statistic# = sn.statistic#
JOIN v$sess_io si ON s.sid = si.sid
LEFT JOIN v$sesstat sm ON s.sid = sm.sid AND sm.statistic# = (
    SELECT statistic# FROM v$statname WHERE name = 'session pga memory'
)
WHERE s.type = 'USER'
AND sn.name = 'CPU used by this session'
ORDER BY ss.value DESC;

-- Long running operations
SELECT 
    sid,
    serial#,
    opname,
    target,
    sofar,
    totalwork,
    ROUND(sofar/totalwork*100, 2) AS percent_complete,
    elapsed_seconds,
    time_remaining,
    message
FROM v$session_longops
WHERE sofar < totalwork
ORDER BY time_remaining DESC;""",
            matches_search("session blocking kill resource longops", search_query)
        )

# Reference Tab
with role_tab4:
    st.header("📚 Quick Reference")
    
    ref_tabs = st.tabs(["Functions", "Data Types", "System Views", "Hints", "Best Practices"])
    
    with ref_tabs[0]:  # Functions
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.subheader("📅 Date Functions")
            st.code("""
-- Current date/time
SYSDATE                    -- Current date+time
CURRENT_DATE              -- Session date
CURRENT_TIMESTAMP         -- With timezone
SYSTIMESTAMP              -- System timestamp

-- Date arithmetic
date_col + 1              -- Add 1 day
date_col - 7              -- Subtract 7 days
date_col + 1/24           -- Add 1 hour
date_col + 1/1440         -- Add 1 minute

-- Date functions
ADD_MONTHS(date, n)       -- Add n months
LAST_DAY(date)           -- Last day of month
NEXT_DAY(date, 'MONDAY') -- Next Monday
MONTHS_BETWEEN(d1, d2)   -- Months between
ROUND(date, 'MONTH')     -- Round to month
TRUNC(date, 'YEAR')      -- Truncate to year

-- Extract parts
EXTRACT(YEAR FROM date)   -- Get year
EXTRACT(MONTH FROM date)  -- Get month
EXTRACT(DAY FROM date)    -- Get day
TO_CHAR(date, 'DY')      -- Day name
TO_CHAR(date, 'Q')       -- Quarter

-- Common formats
TO_CHAR(date, 'YYYY-MM-DD')
TO_CHAR(date, 'DD-MON-YYYY')
TO_CHAR(date, 'MM/DD/YYYY HH24:MI:SS')
TO_DATE('2024-01-15', 'YYYY-MM-DD')
""", language="sql")
            
        with col2:
            st.subheader("🔤 String Functions")
            st.code("""
-- Case manipulation
UPPER(str)                -- UPPERCASE
LOWER(str)                -- lowercase
INITCAP(str)              -- Title Case

-- Trimming
TRIM(str)                 -- Both sides
LTRIM(str)                -- Left trim
RTRIM(str)                -- Right trim
TRIM(LEADING '0' FROM str) -- Specific char

-- Substring
SUBSTR(str, start, length)
SUBSTR(str, -3)           -- Last 3 chars
INSTR(str, substr)        -- Position
INSTR(str, substr, 1, 2)  -- 2nd occurrence

-- Padding
LPAD(str, length, char)   -- Left pad
RPAD(str, length, char)   -- Right pad

-- Other functions
LENGTH(str)               -- String length
REPLACE(str, old, new)    -- Replace text
TRANSLATE(str, from, to)  -- Character map
CONCAT(str1, str2)        -- Concatenate
str1 || str2 || str3      -- Concatenate
ASCII(char)               -- ASCII value
CHR(number)               -- Character

-- Regular expressions
REGEXP_LIKE(str, pattern)
REGEXP_REPLACE(str, pattern, replace)
REGEXP_SUBSTR(str, pattern)
REGEXP_INSTR(str, pattern)
REGEXP_COUNT(str, pattern)
""", language="sql")
            
        with col3:
            st.subheader("🔢 Numeric & Conversion")
            st.code("""
-- Math functions
ABS(n)                    -- Absolute value
CEIL(n)                   -- Ceiling
FLOOR(n)                  -- Floor
ROUND(n, decimals)        -- Round
TRUNC(n, decimals)        -- Truncate
MOD(n, divisor)          -- Modulo
POWER(n, exponent)       -- Power
SQRT(n)                  -- Square root
SIGN(n)                  -- Sign (-1,0,1)

-- Trigonometric
SIN(n), COS(n), TAN(n)
ASIN(n), ACOS(n), ATAN(n)

-- Logarithmic
LN(n)                    -- Natural log
LOG(base, n)             -- Log base
EXP(n)                   -- e^n

-- Conversion functions
TO_NUMBER(str)           -- String to number
TO_CHAR(number, format)  -- Number to string
TO_DATE(str, format)     -- String to date
TO_TIMESTAMP(str, fmt)   -- String to timestamp
CAST(expr AS datatype)   -- Type casting

-- Number formats
TO_CHAR(1234.5, '9999.99')     -- 1234.50
TO_CHAR(1234.5, '09999.99')    -- 01234.50
TO_CHAR(1234567, '9,999,999')  -- 1,234,567
TO_CHAR(0.15, '99%')           -- 15%
TO_CHAR(1234, '$9,999')        -- $1,234

-- NULL handling
NVL(expr, default)       -- Replace NULL
NVL2(expr, if_not_null, if_null)
NULLIF(expr1, expr2)     -- NULL if equal
COALESCE(expr1, expr2, ...) -- First non-NULL
""", language="sql")
            
    with ref_tabs[1]:  # Data Types
        st.subheader("📊 Oracle Data Types")
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("""
            **Character Data Types**
            ```sql
            VARCHAR2(size)      -- Variable length (1-4000)
            NVARCHAR2(size)     -- Unicode variable
            CHAR(size)          -- Fixed length (1-2000)
            NCHAR(size)         -- Unicode fixed
            CLOB                -- Character large object (4GB)
            NCLOB               -- Unicode CLOB
            LONG                -- Legacy, use CLOB instead
            ```
            
            **Numeric Data Types**
            ```sql
            NUMBER              -- Any number
            NUMBER(p)           -- Precision p
            NUMBER(p,s)         -- Precision p, scale s
            BINARY_FLOAT        -- 32-bit floating
            BINARY_DOUBLE       -- 64-bit floating
            INTEGER             -- Alias for NUMBER(38)
            FLOAT               -- Floating point
            ```
            
            **Date/Time Data Types**
            ```sql
            DATE                -- Date and time
            TIMESTAMP           -- Date, time, fractional seconds
            TIMESTAMP WITH TIME ZONE
            TIMESTAMP WITH LOCAL TIME ZONE
            INTERVAL YEAR TO MONTH
            INTERVAL DAY TO SECOND
            ```
            """)
            
        with col2:
            st.markdown("""
            **Binary Data Types**
            ```sql
            RAW(size)           -- Binary data (1-2000)
            LONG RAW            -- Legacy, use BLOB
            BLOB                -- Binary large object (4GB)
            BFILE               -- External file pointer
            ```
            
            **Other Data Types**
            ```sql
            ROWID               -- Physical row address
            UROWID              -- Universal rowid
            XMLType             -- XML data
            JSON                -- JSON data (21c+)
            SDO_GEOMETRY        -- Spatial data
            ```
            
            **Collection Types**
            ```sql
            -- Nested table
            CREATE TYPE phone_list AS TABLE OF VARCHAR2(20);
            
            -- VARRAY
            CREATE TYPE colors AS VARRAY(7) OF VARCHAR2(20);
            
            -- Associative array (PL/SQL only)
            TYPE assoc_array IS TABLE OF VARCHAR2(100) 
                INDEX BY PLS_INTEGER;
            ```
            """)
            
    with ref_tabs[2]:  # System Views
        st.subheader("🔍 Important System Views")
        
        st.code("""
-- Session and performance views
V$SESSION              -- Current sessions
V$SESSION_WAIT         -- Session wait events
V$SQL                  -- SQL statements in cache
V$SQL_PLAN             -- Execution plans
V$LOCKED_OBJECT        -- Locked objects
V$LOCK                 -- All locks
V$PROCESS              -- Oracle processes
V$TRANSACTION          -- Active transactions

-- Storage views
DBA_TABLESPACES        -- All tablespaces
DBA_DATA_FILES         -- Data files
DBA_SEGMENTS           -- Storage segments
DBA_EXTENTS            -- Storage extents
DBA_FREE_SPACE         -- Free space

-- Schema objects
DBA_TABLES             -- All tables
DBA_INDEXES            -- All indexes
DBA_VIEWS              -- All views
DBA_SEQUENCES          -- All sequences
DBA_CONSTRAINTS        -- All constraints
DBA_TRIGGERS           -- All triggers
DBA_PROCEDURES         -- Stored procedures
DBA_SOURCE             -- PL/SQL source code

-- User and privilege views
DBA_USERS              -- All users
DBA_ROLES              -- All roles
DBA_SYS_PRIVS          -- System privileges
DBA_TAB_PRIVS          -- Table privileges
DBA_ROLE_PRIVS         -- Role grants

-- Note: Replace DBA_ with ALL_ for accessible objects
--       Replace DBA_ with USER_ for owned objects
""", language="sql")

# Best Practices
st.markdown("---")
st.header("💡 Oracle Best Practices")

bp_col1, bp_col2 = st.columns(2)

with bp_col1:
    st.markdown("""
    **Performance Best Practices**
    - ✅ Use bind variables to avoid hard parsing
    - ✅ Gather statistics regularly (DBMS_STATS)
    - ✅ Create indexes on foreign keys
    - ✅ Use appropriate data types (VARCHAR2 vs CHAR)
    - ✅ Partition large tables
    - ✅ Use bulk operations (BULK COLLECT, FORALL)
    - ✅ Monitor and tune SGA/PGA sizes
    
    **PL/SQL Best Practices**
    - ✅ Use packages for modularity
    - ✅ Handle exceptions properly
    - ✅ Use bulk operations for large data sets
    - ✅ Avoid row-by-row processing
    - ✅ Use native compilation for CPU-intensive code
    - ✅ Instrument code with DBMS_APPLICATION_INFO
    """)

with bp_col2:
    st.markdown("""
    **Security Best Practices**
    - ✅ Follow least privilege principle
    - ✅ Use roles for privilege management
    - ✅ Enable auditing for sensitive operations
    - ✅ Encrypt sensitive data (TDE)
    - ✅ Use VPD for row-level security
    - ✅ Regular security patches
    
    **Development Best Practices**
    - ✅ Use consistent naming conventions
    - ✅ Document code with comments
    - ✅ Version control database code
    - ✅ Test in non-production first
    - ✅ Use EXPLAIN PLAN before production
    - ✅ Implement proper error logging
    """)

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use the search box to quickly find Oracle-specific syntax, PL/SQL examples, and DBA commands!")