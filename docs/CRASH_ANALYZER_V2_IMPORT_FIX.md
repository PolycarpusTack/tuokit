# Crash Analyzer V2 - Import Fix Guide

## Issue
When running the crash analyzer v2 page, you might encounter:
```
ModuleNotFoundError: No module named 'toolkits'
```

## Solution Applied
The issue has been fixed by adding the parent directory to the Python path in the page file.

### Fix Details
In `pages/crash_analyzer_v2.py`, added:
```python
import sys
from pathlib import Path

# Add parent directory to path to find toolkits module
sys.path.insert(0, str(Path(__file__).parent.parent))
```

This ensures that Python can find the `toolkits` module regardless of where Streamlit is executed from.

## Running the Application

### Option 1: Using the batch file (Recommended)
```bash
run_crash_v2.bat
```

### Option 2: Direct streamlit command
```bash
# From project root
streamlit run pages/crash_analyzer_v2.py --server.port 8502
```

### Option 3: Using Python module syntax
```bash
# From project root with activated environment
python -m streamlit run pages/crash_analyzer_v2.py
```

## Testing the Progress Demo
```bash
streamlit run test_progress_demo.py
```

## Verifying the Fix
You can verify the import works:
```bash
# From project root
python -c "from toolkits.crash_analyzer_v2 import CrashAnalyzerV2; print('Import OK')"
```

## Notes
- The fix ensures compatibility regardless of the current working directory
- All pages that import from toolkits should use this pattern
- The path manipulation is done before any toolkits imports