# 🔄 SQL Pipeline - User Guide

## Overview
The SQL Pipeline is a beginner-friendly tool that guides you through creating optimized SQL queries in 4 simple steps. Perfect for those learning SQL or wanting a quick way to generate professional queries.

## The 4-Step Process

### Step 1: Describe 💬
**What to do:** Tell us what data you need in plain English

**Tips for better results:**
- Be specific about what you want to see
- Include filters (e.g., "last 30 days", "active customers")
- Mention sorting preferences (e.g., "highest to lowest")
- Specify limits (e.g., "top 10", "first 5")

**Good examples:**
- ✅ "Show me the top 5 customers by total spending in Q4 2024"
- ✅ "List all products with less than 20 items in stock, sorted by category"
- ✅ "Calculate monthly sales totals for 2024 with running total"

**Poor examples:**
- ❌ "Customer data" (too vague)
- ❌ "Sales info" (not specific enough)

### Step 2: Generate ⚡
**What happens:** AI creates a SQL query based on your description

**Features:**
- View the generated SQL with syntax highlighting
- See which SQL concepts are used (JOIN, GROUP BY, etc.)
- Edit the SQL directly if needed
- Regenerate for alternative versions
- Safety validation prevents dangerous operations

### Step 3: Optimize 🚀
**What happens:** The query is automatically optimized for performance

**What you'll see:**
- Optimization summary explaining improvements
- Side-by-side comparison of original vs optimized
- List of specific improvements made
- Performance impact estimates

**Common optimizations:**
- Converting subqueries to JOINs
- Adding efficient filtering
- Improving index usage
- Simplifying complex logic

### Step 4: Understand 🧠
**Three ways to explore your query:**

#### 📖 Explanation Tab
- Plain English breakdown of what the query does
- Step-by-step logic explanation
- Learning resources based on concepts used
- Links to relevant SQL tutorials

#### 🧪 Test Tab
- Run your query on sample data
- Three built-in data templates:
  - Customers & Orders
  - Employees & Departments  
  - Products & Categories
- Paste your own JSON data
- See results immediately

#### 💾 Save Tab
- Save the complete pipeline to knowledge base
- Includes all versions and explanations
- Categorize for easy retrieval
- Add notes for future reference

## Sample Data Templates

### Customers & Orders
```json
{
  "customers": [
    {"id": 1, "name": "Alice Johnson", "email": "alice@email.com"},
    {"id": 2, "name": "Bob Smith", "email": "bob@email.com"}
  ],
  "orders": [
    {"id": 101, "customer_id": 1, "amount": 150.99, "order_date": "2024-01-10"}
  ]
}
```
Perfect for: Sales analysis, customer segmentation, order tracking

### Employees & Departments
```json
{
  "employees": [
    {"id": 1, "name": "John Doe", "department": "Sales", "salary": 60000}
  ],
  "departments": [
    {"id": 1, "name": "Sales", "budget": 500000}
  ]
}
```
Perfect for: HR analytics, salary analysis, department comparisons

### Products & Categories
```json
{
  "products": [
    {"id": 1, "name": "Laptop", "category": "Electronics", "price": 999.99, "stock": 50}
  ],
  "categories": [
    {"id": 1, "name": "Electronics", "discount": 0.10}
  ]
}
```
Perfect for: Inventory management, pricing analysis, stock alerts

## Quick Examples

Click any example in the sidebar to instantly load it:

### Top Customers
**Description:** "Show me the top 5 customers by total spending with their email addresses"
**Generates:** Customer ranking query with aggregation

### Sales Analysis
**Description:** "Calculate monthly sales totals for the current year with running total"
**Generates:** Time-series analysis with window functions

### Inactive Users
**Description:** "Find customers who haven't made any orders in the last 6 months"
**Generates:** Query using LEFT JOIN or NOT EXISTS

### Product Inventory
**Description:** "List products that are low on stock (less than 20 items)"
**Generates:** Simple filtering query with sorting

## Pro Tips

### 1. Start Simple
Begin with basic queries and add complexity gradually. The pipeline handles complex requests but starting simple helps you understand the process.

### 2. Use the Examples
The sidebar examples demonstrate good description patterns. Study them to learn how to phrase your requests.

### 3. Test Before Production
Always test with sample data before running queries on production databases. The test feature helps catch issues early.

### 4. Learn from Explanations
The explanation tab breaks down complex SQL into understandable parts. Use it to improve your SQL knowledge.

### 5. Save Everything
Save successful queries to the knowledge base. Over time, you'll build a library of tested, optimized queries.

## Integration with Other Tools

### Need More Control?
- Use **SQL Generator** for complex queries requiring fine-tuning
- Use **SQL Optimizer** for deep performance analysis

### Workflow Example:
1. Start with Pipeline for quick query creation
2. If too complex → SQL Generator
3. If performance critical → SQL Optimizer
4. Save all versions to Knowledge Library

## Common Patterns

### Top N Queries
"Show top 10 [items] by [metric]"
- Generates: ORDER BY with LIMIT

### Time-Based Analysis
"Monthly/Daily/Yearly [metric] for [time period]"
- Generates: DATE_TRUNC with GROUP BY

### Comparisons
"Compare [metric] between [group A] and [group B]"
- Generates: CASE statements or multiple aggregations

### Running Totals
"Show [metric] with running total"
- Generates: Window functions with SUM() OVER()

## Troubleshooting

### Query Not Working?
1. Check your description is specific enough
2. Verify table/column names in schema hints
3. Test with simpler version first
4. Use regenerate for alternatives

### Optimization Not Helpful?
- Some queries are already optimal
- Focus on the explanation to understand why
- Consider data model changes for better performance

### Test Results Unexpected?
- Verify your sample data structure
- Check for NULL values
- Ensure date formats match
- Test with minimal data first

---

The SQL Pipeline makes SQL accessible to everyone. Start with Step 1 and let the pipeline guide you to professional, optimized queries!
