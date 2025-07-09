# 🦆 DuckDB + 🐘 PostgreSQL Integration in TuoKit

## Overview

TuoKit now uses **two databases** that work together perfectly:
- **PostgreSQL**: Your permanent knowledge base (existing)
- **DuckDB**: Lightning-fast analytics engine (new)

## Installation

```bash
pip install duckdb
# or
pip install -r requirements.txt  # (already updated)
```

## How It Works

### 1. PostgreSQL (Main Database) - The Filing Cabinet 🗄️
- **Purpose**: Permanent storage for all TuoKit data
- **What it stores**:
  - Knowledge base entries
  - User settings and preferences
  - Agent execution history
  - Saved pipelines
  - **NEW**: Analytics results from DuckDB
  - **NEW**: Discovered data patterns
  - **NEW**: Natural language query history
  - **NEW**: Data quality profiles

### 2. DuckDB (Analytics Engine) - The Calculator 🧮
- **Purpose**: Super-fast data analysis on uploaded files
- **What it does**:
  - Analyzes CSV/Excel files in-memory
  - Converts natural language to SQL
  - Performs complex aggregations instantly
  - Finds patterns and correlations
  - No server setup needed!

## The Enhanced Data Analysis Flow

```python
# 1. User uploads a CSV file
df = pd.read_csv("uploaded_file.csv")

# 2. DuckDB analyzes it (FAST!)
agent = DataAnalysisAgentEnhanced()
results = agent.analyze_dataframe_enhanced(df, "my_dataset")

# 3. Valuable insights automatically saved to PostgreSQL
# - Data profile (quality score, column stats)
# - Discovered patterns (correlations, anomalies)
# - Generated insights (with confidence scores)
# - Query history (natural language → SQL)

# 4. Query historical insights anytime
past_insights = query_analytics_history(db, "insights")
```

## What Gets Saved to PostgreSQL

### 1. Data Profiles (`data_profiles` table)
```sql
- Profile name and source file
- Row/column counts
- Complete profile data (JSON)
- Data quality score
- Timestamp
```

### 2. Analytics Queries (`analytics_queries` table)
```sql
- Natural language query
- Generated SQL
- Result summary
- Execution time
- Success/failure status
```

### 3. Data Insights (`data_insights` table)
```sql
- Dataset name
- Insight type and description
- Confidence score
- Detailed insight data (JSON)
```

### 4. Data Patterns (`data_patterns` table)
```sql
- Pattern type (correlation, anomaly, etc.)
- Affected columns
- Pattern details
- Discovery timestamp
```

## Usage Examples

### Basic Analysis with Storage
```python
# Analyze and save to knowledge base
agent = DataAnalysisAgentEnhanced()
results = agent.analyze_dataframe_enhanced(
    df, 
    dataset_name="sales_q4_2024"
)
```

### Natural Language Queries
```python
# Ask questions in plain English
result = agent.natural_language_query_enhanced(
    "What's the average sales by region?",
    table_schema=dict(df.dtypes),
    dataset_name="sales_q4_2024"
)

# Query is saved with results for future reference!
```

### Query Past Analytics
```python
# Get all saved insights
insights = query_analytics_history(db, "insights")

# Get successful queries
queries = query_analytics_history(db, "queries")

# Get data quality profiles
profiles = query_analytics_history(db, "profiles")
```

## Benefits of This Architecture

### 1. ⚡ Speed
- DuckDB is 10-100x faster than PostgreSQL for analytics
- Analyzes millions of rows in seconds
- No need to import data into PostgreSQL first

### 2. 💾 Knowledge Persistence
- Every analysis builds your knowledge base
- Learn from past queries and insights
- Track data quality over time

### 3. 🔍 Searchability
- Find patterns across multiple datasets
- Query history helps refine future analyses
- Build a library of proven SQL queries

### 4. 🚀 Zero Configuration
- DuckDB runs in-memory, no server needed
- Works immediately with pandas DataFrames
- Automatic cleanup after analysis

## Quick Start

1. **Upload any CSV/Excel file** in the Agent Hub
2. **Select "Data Analysis"** quick action
3. **Ask questions** in natural language
4. **View saved insights** in the Analytics Dashboard

All results are automatically saved to PostgreSQL!

## Advanced Features

### Custom Queries
```python
# Run custom DuckDB queries
conn = duckdb.connect(':memory:')
conn.register('my_table', df)
result = conn.execute("SELECT * FROM my_table WHERE sales > 1000").fetchall()
```

### Batch Analysis
```python
# Analyze multiple files
for file in files:
    df = pd.read_csv(file)
    agent.analyze_dataframe_enhanced(df, file.stem)
```

### Analytics Dashboard
```python
# View all saved analytics in Streamlit
streamlit run demo_data_analysis_storage.py
```

## Summary

- **PostgreSQL** = Permanent knowledge storage (unchanged)
- **DuckDB** = Temporary but ultra-fast analytics (new)
- **Together** = Fast analysis + permanent insights

No changes to existing TuoKit functionality - this just makes data analysis incredibly powerful while building your knowledge base automatically!
