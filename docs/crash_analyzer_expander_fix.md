# Crash Analyzer - Nested Expander Fix

## 🐛 Issue Fixed

**Error**: `StreamlitAPIException: Expanders may not be nested inside other expanders`

**Location**: Line 1131 in `crash_analyzer.py`

## ✅ Solution Applied

Changed the nested expander to a checkbox toggle:

**Before**:
```python
with st.expander("Context", expanded=False):
    st.code(match['context'], language="text")
```

**After**:
```python
if st.checkbox(f"Show context for {match['pattern']}", key=f"context_{match['pattern']}_{match['position']}"):
    st.code(match['context'], language="text")
```

## 🚀 How to Test

1. The crash analyzer should now load without errors
2. Upload the test file: `test_crash.log`
3. Look for the "Detected Known Patterns" section
4. Click the checkboxes to show/hide context for each pattern

## 📝 Notes

- Streamlit doesn't allow expanders inside expanders
- Using checkboxes provides a similar collapsible UI experience
- Each checkbox has a unique key to avoid conflicts

The crash analyzer should now work properly! Try uploading a crash dump file to test all the new features.
