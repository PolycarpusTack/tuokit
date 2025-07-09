# SQL Tools Migration Guide

## Quick Migration Steps

### 1. Update Imports
Replace in all SQL-related pages:
```python
# OLD - Multiple implementations
def generate_sql(query):
    # Local implementation
    
def optimize_sql(sql):
    # Another local implementation

# NEW - Single source of truth
from utils import SQLTools
```

### 2. Update Function Calls
```python
# OLD
sql = generate_sql(user_query)
optimized = optimize_sql(sql)

# NEW  
sql = SQLTools.generate(user_query)
optimized = SQLTools.optimize(sql)
```

### 3. Files to Update
- [ ] pages/sql_generator.py
- [ ] pages/sql_optimizer.py  
- [ ] pages/sql_pipeline.py
- [ ] pages/agent_unified.py

### 4. Benefits
- Single implementation to maintain
- Consistent behavior across tools
- Automatic knowledge capture
- Unified validation

### 5. Testing
```python
# Test the new unified tools
from utils import SQLTools

# Test generation
sql = SQLTools.generate("show all users")
print(sql)

# Test validation  
is_valid, msg = SQLTools.validate(sql)
print(f"Valid: {is_valid}, Message: {msg}")

# Test explanation
explanation = SQLTools.explain(sql)
print(explanation)
```

## Note
Legacy wrapper functions are provided for backward compatibility but should be removed after migration is complete.