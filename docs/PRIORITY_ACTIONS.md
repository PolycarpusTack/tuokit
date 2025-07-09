# 🎯 TuoKit Priority Action List

## This Week - Must Do

### Day 1: Emergency Cleanup
```bash
# 1. Run organization script in dry-run mode first
python smart_organize.py --dry-run

# 2. Review what will change, then run for real
python smart_organize.py

# 3. Delete obvious duplicates (keep backups)
rm app_modern_backup.py
rm utils_old.py
rm -rf backup_before_consolidation_*  # After verifying current code works
```

### Day 2: Fix Database Chaos
1. Keep ONLY `setup_database.py` 
2. Delete: `setup_unified_database.py`, `quick_db_setup.py`
3. Update `setup_database.py` to be the single source of truth
4. Test with fresh database

### Day 3: Complete SQL Consolidation
1. Verify new `pages/sql_toolkit.py` has all features
2. Archive old SQL files:
   - `mv pages/sql_generator.py archive/`
   - `mv pages/sql_optimizer.py archive/`
   - `mv pages/sql_pipeline.py archive/`
3. Update navigation to use only sql_toolkit.py

### Day 4: Fix Ollama Connection
1. Keep ONLY `utils/ollama.py` and `test_ollama.py`
2. Delete all other test_ollama_*.py files
3. Add robust connection handling to utils/ollama.py:
```python
def ensure_ollama_running():
    """Single function to check/start Ollama"""
    # All connection logic here
```

### Day 5: Simplify Navigation
1. Group pages into categories
2. Create `pages/0_🏠_Home.py` as landing page
3. Implement tool categories in sidebar

## Next Week - Nice to Have

1. **Ruby/Rails Consolidation** - Use the adapter pattern
2. **Test Suite Organization** - Move all tests to tests/ directory  
3. **Knowledge Capture** - Ensure ALL tools save to knowledge base
4. **Remove .bat files** - Use Python launcher instead

## Success Metrics

Before cleanup:
- [ ] Root directory files: 194
- [ ] Duplicate Ollama tests: 8
- [ ] SQL tool files: 5
- [ ] Database setup files: 5

After cleanup:
- [ ] Root directory files: <50
- [ ] Duplicate Ollama tests: 1
- [ ] SQL tool files: 1
- [ ] Database setup files: 1

## Remember

> "Build fast, build smart, build exactly what's needed"
> 
> NOT: "Keep everything just in case" 😅

Focus on FINISHING existing work before starting new features!
