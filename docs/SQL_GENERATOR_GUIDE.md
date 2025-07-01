# 🛢️ TuoKit SQL Generator - Quick Reference

## Overview
The SQL Generator provides AI-powered SQL development with two modes:
- **Basic Mode** (Default): All AI features without database connectivity
- **Enterprise Mode** (Optional): Adds live database integration

## Features at a Glance

### Basic Mode (Always Available)
1. **Natural Language to SQL** - Describe queries in plain English
2. **SQL Optimization** - Performance recommendations and indexing
3. **Dialect Translation** - Oracle ↔ PostgreSQL conversion
4. **Security Scanner** - Vulnerability detection and remediation

### Enterprise Mode (Optional Installation)
5. **Live Database Connection** - Connect to PostgreSQL/Oracle
6. **Schema Discovery** - Auto-complete with real table/column names
7. **Query Execution** - Test queries with automatic safety limits
8. **EXPLAIN Plans** - Analyze actual execution plans

## Installation Status Check
```python
# In the SQL Generator, look for:
- Green checkmark: SQLAlchemy installed (DB connections available)
- Warning message: Feature not installed (still fully functional for AI features)
```

## Quick Start Examples

### Generate a Report Query
```
Description: "Show monthly sales by region with running totals and YoY growth"
Database: PostgreSQL
Result: Optimized query with window functions
```

### Optimize Existing SQL
```sql
-- Input
SELECT * FROM orders WHERE customer_id IN 
  (SELECT id FROM customers WHERE country = 'USA')

-- Output
- Convert to JOIN for better performance
- Add index on customers.country
- Select only needed columns
```

### Translate Between Dialects
```sql
-- Oracle
SELECT * FROM employees WHERE ROWNUM <= 10

-- PostgreSQL
SELECT * FROM employees LIMIT 10
```

### Security Audit
```sql
-- Vulnerable
WHERE username = '" + user_input + "'

-- Secure
WHERE username = $1  -- Parameterized query
```

## Pro Tips

1. **Schema Hints**: Always provide table structures for better accuracy
2. **Stored Procedures**: Use the checkbox for complete procedure generation
3. **Security Mode**: Enable for production-ready code with validation
4. **Examples**: Use the sidebar examples as starting points

## Common Use Cases

- **Data Migration**: Convert Oracle queries to PostgreSQL
- **Performance Tuning**: Identify and fix slow queries
- **Report Generation**: Create complex analytical queries
- **Security Audit**: Scan legacy code for vulnerabilities
- **Learning**: Understand SQL best practices by example

## Keyboard Shortcuts
- `Ctrl+Enter`: Generate SQL (when in text area)
- `Tab`: Switch between tabs
- `Esc`: Close expandable sections

## Integration with Knowledge Base
All generated queries can be saved to the knowledge base for:
- Reuse across projects
- Building a query library
- Team knowledge sharing
- Pattern recognition

---
*For detailed documentation, visit the Help Guide (❓) in TuoKit*
