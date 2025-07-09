# TuoKit Broken Tools Analysis

## Summary of Issues Found

### 1. **Syntax Errors**
- `pages/smalltalk_toolkit.py`: F-string expression error at line 424 (backslash in f-string)

### 2. **Missing Page References**
- `pages/sql_academy.py` references non-existent `pages/sql_suite.py`
- `pages/tool_finder.py` has template variable references that need proper formatting

### 3. **Database Dependencies**
Several tools depend on specific database tables that may not exist:
- `pages/crash_graph_viewer.py`: Requires tables `crash_patterns`, `crash_relationships`, `pattern_evolution`, `crash_instances`
- `pages/knowledge_graph_viewer.py`: Requires tables `knowledge_units`, `knowledge_links`
- `pages/database_health_check.py`: Checks for multiple tables including `queries`, `knowledge_units`, `knowledge_links`, etc.

### 4. **Potentially Non-Functional Tools**
Based on analysis, these tools are likely to have issues:

#### **Definitely Broken:**
1. **smalltalk_toolkit.py** - Syntax error prevents execution
2. **sql_academy.py** - References non-existent `sql_suite.py` page

#### **Likely Broken (Database Dependencies):**
1. **crash_graph_viewer.py** - Requires crash analysis database tables
2. **knowledge_graph_viewer.py** - Requires knowledge graph database tables
3. **migration_dashboard.py** - Looks for `feature_analysis_*.json` files that may not exist

#### **Functional but Limited:**
1. **database_health_check.py** - Works but will report missing tables as issues
2. **ollama_health_check.py** - Should work if Ollama is installed
3. **tool_finder.py** - Works but has minor formatting issues in page references

### 5. **Tools Requiring External Services:**
- All tools using Ollama require Ollama to be running
- Database tools require PostgreSQL to be configured

## Recommendations

### Tools to Remove from Navigation:
1. **smalltalk_toolkit.py** - Has syntax error
2. **sql_academy.py** - References missing page
3. **crash_graph_viewer.py** - Requires specialized database setup
4. **knowledge_graph_viewer.py** - Requires knowledge graph database
5. **migration_dashboard.py** - Temporary migration tool, likely obsolete

### Tools to Keep with Warnings:
1. **database_health_check.py** - Useful for database setup validation
2. **ollama_health_check.py** - Useful for Ollama setup validation

### Quick Fixes Needed:
1. Fix syntax error in `smalltalk_toolkit.py` line 424
2. Remove or update the reference to `sql_suite.py` in `sql_academy.py`
3. Fix template variables in `tool_finder.py`

## Navigation Update Suggestion

Remove or comment out these entries from `utils/navigation.py`:
- smalltalk_toolkit (syntax error)
- sql_academy (missing dependency)
- crash_graph_viewer (database dependency)
- knowledge_graph_viewer (database dependency)
- migration_dashboard (obsolete)