# TuoKit Test Infrastructure

This directory contains the unified test infrastructure for TuoKit, consolidating all test functionality from 30+ test files.

## Quick Start

```bash
# Run all tests
python run_tests.py

# Run specific category
python test_runner.py --category sql

# Run specific test file
python test_runner.py --file test_sql_generator.py

# List all available tests
python test_runner.py --list
```

## Test Categories

### 1. **SQL Tests** (`sql`)
- SQL generation, optimization, and pipeline tests
- Files: `test_sql_generator.py`, `test_sql_optimizer.py`, `test_sql_pipeline.py`, etc.

### 2. **Agent Tests** (`agent`)
- Agent system and integration tests
- Files: `test_agent_lite.py`, `test_agent_system.py`

### 3. **Ollama Tests** (`ollama`)
- Ollama integration and connection tests
- Files: `test_ollama.py`, `test_ollama_connection.py`, etc.

### 4. **Tool Tests** (`tools`)
- Individual tool tests
- Files: `test_unified_tools.py`, `test_enhanced_tools.py`, etc.

### 5. **UI Tests** (`ui`)
- UI and app functionality tests
- Files: `test_app.py`, `test_crash_analyzer_enhanced.py`, etc.

### 6. **Knowledge Tests** (`knowledge`)
- Knowledge graph and educational feature tests
- Files: `test_knowledge_graph.py`, `test_edu_mind.py`, etc.

### 7. **Utility Tests** (`utilities`)
- Utility and helper function tests
- Files: `test_pdf.py`, `test_json_parsing_fix.py`, etc.

## Test Runner Options

```bash
Options:
  -h, --help            Show help message
  -c, --category        Run tests from specific category
  -f, --file            Run specific test files
  -l, --list            List all available tests
  --config              Path to test configuration file
  -p, --parallel        Run tests in parallel
  -s, --sequential      Run tests sequentially
  -v, --verbose         Increase verbosity
  --no-html             Skip HTML report generation
  --no-json             Skip JSON report generation
  --timeout             Test timeout in seconds
  --workers             Number of parallel workers
```

## Configuration

Test configuration can be customized using `test_config.json`:

```json
{
  "parallel": true,
  "max_workers": 4,
  "timeout": 300,
  "verbose": 1,
  "coverage": true,
  "html_report": true,
  "json_report": true,
  "mock_ollama": true,
  "database": {
    "use_test_db": true,
    "test_db_path": "test_tuokit.db"
  }
}
```

## Test Utilities

The `test_utils.py` module provides common helpers:

### Mock Objects
- `MockStreamlit`: Mock Streamlit for non-UI tests
- `MockOllama`: Mock Ollama client for testing
- `TestDatabase`: Isolated test database

### Context Managers
```python
from test_utils import mock_streamlit, mock_ollama, temp_directory

# Mock Streamlit
with mock_streamlit() as st:
    st.title("Test")

# Mock Ollama
with mock_ollama() as ollama:
    response = ollama.generate(prompt="test")

# Temporary directory
with temp_directory() as temp_dir:
    # Use temp_dir for test files
```

### Test Fixtures
```python
from test_utils import TestFixtures

# Get test SQL schema
schema = TestFixtures.get_test_sql_schema()

# Get test queries
queries = TestFixtures.get_test_queries()

# Get code samples
code = TestFixtures.get_test_code_samples()
```

## Writing Tests

### Simple Test Script
```python
#!/usr/bin/env python3
"""Test for my feature"""

def test_my_feature():
    print("Testing my feature...")
    
    # Test logic
    result = my_function()
    assert result == expected
    
    print("✅ Test passed!")

if __name__ == '__main__':
    test_my_feature()
```

### Unit Test with unittest
```python
import unittest
from my_module import MyClass

class TestMyClass(unittest.TestCase):
    def setUp(self):
        self.obj = MyClass()
    
    def test_method(self):
        result = self.obj.method()
        self.assertEqual(result, expected)

if __name__ == '__main__':
    unittest.main()
```

### Using Test Utilities
```python
from test_utils import mock_streamlit, MockOllama, TestDatabase

def test_with_mocks():
    # Mock Streamlit
    with mock_streamlit() as st:
        from pages.my_page import show
        show()  # Will use mocked streamlit
    
    # Mock Ollama
    mock = MockOllama({'default': 'test response'})
    response = mock.generate(prompt="test")
    
    # Test database
    with TestDatabase() as db:
        db.insert_test_data()
        # Run tests with isolated database
```

## CI/CD Integration

The test runner generates reports suitable for CI/CD:

### JSON Report
```json
{
  "passed": 45,
  "failed": 2,
  "errors": 1,
  "skipped": 3,
  "duration": 120.5,
  "timestamp": "2024-01-15T10:30:00",
  "details": {
    "test_file.py": {
      "passed": 5,
      "failed": 0,
      "errors": 0,
      "duration": 2.3
    }
  }
}
```

### GitHub Actions
The `.github/workflows/tests.yml` file runs tests automatically on:
- Push to main/develop branches
- Pull requests
- Daily schedule

## Performance Testing

For performance-sensitive tests:

```python
import time

def test_performance():
    start = time.time()
    
    # Run operation
    result = expensive_operation()
    
    duration = time.time() - start
    assert duration < 1.0, f"Operation too slow: {duration}s"
```

## Debugging Failed Tests

1. **Run with verbose output:**
   ```bash
   python test_runner.py --file failing_test.py -vv
   ```

2. **Check test reports:**
   - HTML report: `test_report_TIMESTAMP.html`
   - JSON report: `test_report_TIMESTAMP.json`

3. **Run individual test:**
   ```bash
   python failing_test.py
   ```

4. **Use test utilities for isolation:**
   ```python
   from test_utils import mock_streamlit, TestDatabase
   
   # Isolate the failing component
   with TestDatabase() as db:
       # Test with clean database
   ```

## Best Practices

1. **Use descriptive test names**
2. **Test one thing at a time**
3. **Use mocks for external dependencies**
4. **Clean up test data after tests**
5. **Use assertions with helpful messages**
6. **Group related tests in categories**
7. **Keep tests fast and independent**

## Troubleshooting

### Import Errors
```bash
# Ensure Python path includes project root
export PYTHONPATH=/path/to/tuokit:$PYTHONPATH
```

### Ollama Connection Issues
```bash
# Use mock Ollama for tests
export MOCK_OLLAMA=1
```

### Database Errors
```bash
# Use in-memory test database
export TUOKIT_TEST_DB=:memory:
```

### Timeout Issues
```bash
# Increase timeout for slow tests
python test_runner.py --timeout 600
```