# TuoKit Technical Debt - Quick Fixes Checklist

## 🚨 Day 1: Critical Security Fixes (4-6 hours)

### Fix Hardcoded Credentials
- [ ] Create `.env.example` file with all required environment variables
- [ ] Update `utils/database.py` to use environment variables
- [ ] Search and replace all hardcoded connection strings
- [ ] Update README with environment setup instructions

### Create .env.example:
```bash
# Database Configuration
DB_HOST=localhost
DB_PORT=5432
DB_NAME=tuokit
DB_USER=your_username
DB_PASSWORD=your_password

# Ollama Configuration
OLLAMA_HOST=http://localhost:11434
OLLAMA_DEFAULT_MODEL=llama2

# Application Settings
SECRET_KEY=generate-a-secure-key-here
DEBUG=False
```

### Update database.py:
```python
import os
from dotenv import load_dotenv

load_dotenv()

class DatabaseManager:
    def __init__(self):
        self.connection_string = (
            f"postgresql://{os.getenv('DB_USER')}:"
            f"{os.getenv('DB_PASSWORD')}@"
            f"{os.getenv('DB_HOST', 'localhost')}:"
            f"{os.getenv('DB_PORT', '5432')}/"
            f"{os.getenv('DB_NAME', 'tuokit')}"
        )
```

## 🔴 Day 2-3: SQL Injection Fixes

### Fix Knowledge Search
- [ ] Update `utils/database.py` search methods to use parameterized queries
- [ ] Fix `search_knowledge_safe()` method
- [ ] Audit all dynamic SQL construction
- [ ] Add SQL injection tests

### Example Fix:
```python
# UNSAFE - Current code
query = f"SELECT * FROM knowledge WHERE content LIKE '%{search_term}%'"

# SAFE - Fixed code
query = "SELECT * FROM knowledge WHERE content LIKE %s"
params = (f"%{search_term}%",)
cursor.execute(query, params)
```

## 🟡 Day 4-5: Error Handling Framework

### Add Global Error Handler
Create `utils/error_handler.py`:
```python
import streamlit as st
import logging
from functools import wraps

def handle_errors(func):
    """Decorator for consistent error handling"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            logging.error(f"Error in {func.__name__}: {str(e)}")
            st.error(f"An error occurred: {str(e)}")
            return None
    return wrapper
```

### Priority Files to Add Error Handling:
- [ ] `utils/database.py` - All database operations
- [ ] `utils/ollama.py` - LLM calls
- [ ] `pages/crash_analyzer_v2.py` - File operations
- [ ] `utils/knowledge_capture.py` - Knowledge storage

## 📋 Week 2: Architecture Quick Wins

### Refactor Largest File (crash_graph_viewer.py)
- [ ] Create `toolkits/crash_graph_viewer/` directory
- [ ] Split into modules:
  - `analyzer.py` - Main class (200 lines)
  - `visualizations.py` - Graph generation (600 lines)
  - `data_processing.py` - Data preparation (400 lines)
  - `ui_components.py` - UI elements (500 lines)
  - `config.py` - Configuration (100 lines)

### Extract Common Utilities
- [ ] Create `utils/model_selector.py` for model selection UI
- [ ] Create `utils/ui_components.py` for common UI patterns
- [ ] Update all tools to use shared components

## 🧪 Week 3: Testing Infrastructure

### Set Up Testing Framework
- [ ] Create `pytest.ini` configuration
- [ ] Add GitHub Actions workflow for tests
- [ ] Create test templates for each tool type
- [ ] Write tests for critical paths

### pytest.ini:
```ini
[pytest]
testpaths = tests
python_files = test_*.py
python_classes = Test*
python_functions = test_*
addopts = -v --cov=. --cov-report=html
```

## 🚀 Quick Performance Wins

### Add Connection Pooling (1 hour)
```python
from psycopg2 import pool

class DatabaseManager:
    def __init__(self):
        self.connection_pool = psycopg2.pool.SimpleConnectionPool(
            1, 20,  # min and max connections
            host=os.getenv('DB_HOST'),
            database=os.getenv('DB_NAME'),
            user=os.getenv('DB_USER'),
            password=os.getenv('DB_PASSWORD')
        )
```

### Add Basic Caching (2 hours)
```python
from functools import lru_cache
import hashlib

@lru_cache(maxsize=128)
def get_cached_analysis(content_hash):
    return perform_analysis(content_hash)

def analyze_with_cache(content):
    content_hash = hashlib.md5(content.encode()).hexdigest()
    return get_cached_analysis(content_hash)
```

## 📝 Documentation Quick Start

### Create CONTRIBUTING.md:
```markdown
# Contributing to TuoKit

## Development Setup
1. Clone the repository
2. Copy `.env.example` to `.env` and configure
3. Install dependencies: `pip install -r requirements.txt`
4. Run tests: `pytest`
5. Start app: `streamlit run app.py`

## Code Standards
- Follow toolkit pattern (see CLAUDE.md)
- Add tests for new features
- Use type hints
- Add docstrings to all functions
```

## ✅ Success Criteria

After completing these quick fixes:
- [ ] No hardcoded credentials in codebase
- [ ] No SQL injection vulnerabilities
- [ ] Critical paths have error handling
- [ ] Largest file is under 1000 lines
- [ ] Basic test infrastructure in place
- [ ] Development setup documented

## 🎯 Next Steps

1. Continue systematic refactoring (1-2 tools per week)
2. Increase test coverage incrementally
3. Add monitoring and observability
4. Create UI component library
5. Implement performance optimizations

Remember: **Fix security first, then architecture, then nice-to-haves**