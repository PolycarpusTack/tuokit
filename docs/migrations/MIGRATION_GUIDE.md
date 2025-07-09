# Migration Guide: From Enhanced Scanner v1 to v2

This guide helps you migrate from `enhanced_scanner_full.py` to `enhanced_scanner_v2.py`.

## 🚀 Quick Migration (5 minutes)

1. **Install new dependencies**:
   ```bash
   pip install -r requirements_scanner_v2.txt
   ```

2. **Run the new scanner**:
   ```bash
   python start_scanner.py
   # OR
   streamlit run enhanced_scanner_v2.py
   ```

3. **Use your existing project path** - the new scanner is fully backward compatible!

## 📊 Feature Mapping

### Features That Work The Same Way

| Feature | v1 Location | v2 Location | Notes |
|---------|-------------|-------------|-------|
| Syntax Error Detection | Main scan | Quality tab | Enhanced with better error messages |
| TODO/FIXME Detection | Main scan | Quality tab | Now categorized by type |
| Large File Detection | Main scan | Summary tab | With size recommendations |
| Fix Suggestions | Fix buttons | Fix buttons | Now includes AI prompts |
| Backup System | Same directory | Same directory | Compatible format |

### New Features to Explore

1. **Security Tab** (🔒)
   - Automatically scans for API keys, passwords, secrets
   - No configuration needed - just check the results!

2. **Dependencies Tab** (📦)
   - Shows all package managers in your project
   - Highlights vulnerable dependencies
   - Warns about missing lock files

3. **Technology Detection**
   - Automatically identifies your tech stack
   - Shows confidence levels
   - No manual configuration needed

4. **Scan Profiles**
   - Quick Scan = Similar to v1 default behavior
   - Security Focus = New security features
   - Full Analysis = Everything enabled

## 🔄 Configuration Changes

### Old Way (v1)
```python
# In enhanced_scanner_full.py
SCAN_EXTENSIONS = ['.py']
IGNORE_DIRS = ['__pycache__', '.git', 'venv']
MAX_FILE_SIZE = 1024 * 1024  # 1MB
```

### New Way (v2)
```python
# In enhanced_scanner_v2.py
SCAN_EXTENSIONS = ['.py', '.js', '.ts', '.jsx', '.tsx', '.java', '.cs', '.go', '.rs', '.rb', '.php']
IGNORE_DIRS = ['__pycache__', '.git', 'venv', 'env', 'node_modules', 'vendor', 'target', 'bin', 'obj']
# File size limit now configurable per profile!
```

**No action needed** - v2 automatically handles more file types!

## 🎯 Common Use Cases

### "I just want my syntax errors fixed"
1. Select **Quick Scan** profile
2. Go to **Quality** tab after scan
3. Click **Get Fix** buttons - works the same as v1!

### "I want to check security before deployment"
1. Select **Security Focus** profile
2. Review **Security** tab for critical issues
3. Check **Dependencies** tab for vulnerable packages

### "I want everything v1 had plus new features"
1. Select **Full Analysis** profile
2. All v1 features are in **Quality** tab
3. New features in other tabs

## 📁 File Structure Changes

### v1 Files
```
enhanced_scanner_full.py
clipboard_utils.py (if available)
```

### v2 Files
```
enhanced_scanner_v2.py      # Main scanner
requirements_scanner_v2.txt # Dependencies
start_scanner.py           # Quick start script
README_scanner_v2.md       # Documentation
clipboard_utils.py         # Still supported!
```

## 🔧 API Changes (For Developers)

### Class Name Changes
```python
# v1
scanner = EnhancedCodeScanner(root_path)

# v2
scanner = EnhancedCodeScannerV2(root_path, profile)
```

### New Required Parameter
```python
# v2 requires a profile
profile = ScanProfile().get_profile('quick')  # or 'security', 'quality', 'full'
scanner = EnhancedCodeScannerV2(root_path, profile)
```

### Issue Access
```python
# v1
syntax_errors = scanner.issues['syntax_errors']
todos = scanner.issues['todo_items']

# v2 (same structure, more categories)
syntax_errors = scanner.issues['syntax_errors']
todos = scanner.issues['technical_debt']  # includes TODO, FIXME, HACK, XXX
security = scanner.issues['security']      # NEW!
performance = scanner.issues['performance'] # NEW!
```

## 📈 Score Interpretation

### v1 Scoring
- Single quality score (0-100)
- Based on TODOs and large files

### v2 Scoring
- **Overall Health**: Weighted average of all scores
- **Security Score**: Based on vulnerabilities (40% weight)
- **Quality Score**: Similar to v1 (35% weight)
- **Performance Score**: New performance patterns (25% weight)

**Your v1 quality score ≈ v2 quality score** (similar calculation)

## 🚨 Breaking Changes

1. **Backup Directory Name**
   - v1: `backups/enhanced_scanner`
   - v2: `backups/enhanced_scanner_v2`
   - **Action**: Old backups remain untouched, new backups in new location

2. **Session State Keys** (Streamlit internals)
   - Some internal state keys changed
   - **Action**: None needed - just restart the app

3. **Export Formats**
   - v1: Text reports only
   - v2: Markdown, JSON, SARIF
   - **Action**: Use new export buttons for richer formats

## ✅ Migration Checklist

- [ ] Install new dependencies: `pip install -r requirements_scanner_v2.txt`
- [ ] Try Quick Scan profile first (most similar to v1)
- [ ] Check Security tab for any critical issues
- [ ] Review Dependencies tab if you use package managers
- [ ] Explore new export options (SARIF for CI/CD!)
- [ ] Optional: Install external tools for deeper analysis

## 🤝 Getting Help

### If Something Doesn't Work

1. **"Import error on startup"**
   ```bash
   pip install plotly streamlit --upgrade
   ```

2. **"Can't find my v1 features"**
   - Syntax errors → Quality tab
   - TODOs → Quality tab
   - All v1 features are preserved!

3. **"Scan is slower"**
   - v2 does more analysis by default
   - Use Quick Scan for v1-like speed
   - Limit files in Advanced Options

### Feature Comparison

| Task | v1 | v2 |
|------|----|----|
| Find syntax errors | ✅ Main results | ✅ Quality tab |
| Fix syntax errors | ✅ Fix button | ✅ Fix button + AI prompt |
| Find TODOs | ✅ Main results | ✅ Quality tab (enhanced) |
| Security scan | ❌ | ✅ Security tab |
| Dependency check | ❌ | ✅ Dependencies tab |
| Tech detection | ❌ | ✅ Automatic |
| Visual charts | ❌ | ✅ Dashboard |
| CI/CD export | ❌ | ✅ SARIF format |

## 🎉 Benefits of Upgrading

1. **Find security issues before they reach production**
2. **Identify vulnerable dependencies automatically**
3. **Get visual insights into code health**
4. **Export reports for team sharing**
5. **Integrate with CI/CD pipelines**
6. **Support for 10+ programming languages**

The new scanner includes everything v1 had, plus professional-grade features from enterprise tools - all with the same easy-to-use interface!

---

**Remember**: Your existing workflow still works! The new features are additions, not replacements.
