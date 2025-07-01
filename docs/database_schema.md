# TuoKit Database Schema

## Tables Overview

### 1. `queries` Table
Stores all AI interactions across tools.

| Column | Type | Description |
|--------|------|-------------|
| id | SERIAL PRIMARY KEY | Unique identifier |
| tool | VARCHAR(100) | Tool used (code_explainer, doc_qa, etc.) |
| model | VARCHAR(100) | AI model used |
| user_prompt | TEXT | User's input/question |
| ai_response | TEXT | AI's complete response |
| created_at | TIMESTAMP | When query was made |

### 2. `knowledge_units` Table
Stores verified knowledge extracted from queries.

| Column | Type | Description |
|--------|------|-------------|
| id | SERIAL PRIMARY KEY | Unique identifier |
| query_id | INTEGER | Reference to source query |
| title | VARCHAR(255) | Descriptive title |
| content | TEXT | Knowledge content |
| category | VARCHAR(100) | Classification |
| created_at | TIMESTAMP | When saved |

## Relationships

```
queries (1) ──────< (many) knowledge_units
   │                            │
   └── query_id ───────────────┘
```

## Sample Queries

### Find all code snippets
```sql
SELECT * FROM knowledge_units 
WHERE category = 'Code Snippet' 
ORDER BY created_at DESC;
```

### Get query statistics
```sql
SELECT tool, COUNT(*) as usage_count 
FROM queries 
GROUP BY tool 
ORDER BY usage_count DESC;
```

### Recent activity
```sql
SELECT k.title, k.category, q.tool, q.created_at
FROM knowledge_units k
JOIN queries q ON k.query_id = q.id
ORDER BY k.created_at DESC
LIMIT 10;
```

## Indexes
For better performance:
```sql
CREATE INDEX idx_queries_tool ON queries(tool);
CREATE INDEX idx_queries_created ON queries(created_at);
CREATE INDEX idx_knowledge_category ON knowledge_units(category);
CREATE INDEX idx_knowledge_title ON knowledge_units(title);
```