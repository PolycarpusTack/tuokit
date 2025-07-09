# TuoKit Enhanced Scanner v2.0 - Professional Edition

A comprehensive code analysis tool that combines the best features from enterprise-grade analyzers with a user-friendly Streamlit interface.

## 🚀 Features Implemented

### Phase 1 - Core Features
- **🔒 Security Analysis**
  - API key detection (AWS, GitHub, Google, generic)
  - Password and credential scanning
  - JWT token detection
  - Private key detection
  - SQL injection vulnerability detection
  - Weak cryptography detection

- **🔍 Technology Detection**
  - Automatic language and framework detection
  - Support for Python, JavaScript/TypeScript, Java, C#/.NET, Go, Rust, Ruby, PHP
  - Confidence scoring based on file patterns and directory structure

- **📊 Enhanced Dashboard**
  - Visual health score gauge
  - Issue distribution charts
  - Language breakdown pie chart
  - Severity distribution

- **📁 SARIF Export**
  - CI/CD ready format
  - Compatible with GitHub, GitLab, Azure DevOps

### Phase 2 - Advanced Features
- **📦 Dependency Analysis**
  - Package manager detection (npm, pip, maven, gradle, cargo, composer)
  - Vulnerability checking
  - Lock file validation
  - Dependency counting

- **🛠️ External Tool Integration**
  - Flake8 (Python linting)
  - Pylint (Python analysis)
  - ESLint (JavaScript/TypeScript)
  - Bandit (Python security)

- **⚙️ Scan Profiles**
  - Quick Scan: Fast analysis of critical issues
  - Security Focus: Deep security analysis
  - Code Quality: Comprehensive quality checks
  - Full Analysis: Everything enabled

- **📈 Performance Analysis**
  - Nested loop detection
  - Database N+1 query problems
  - Large list comprehensions
  - Synchronous file operations

## 🛠️ Installation

1. **Clone or download the scanner files**:
   - `enhanced_scanner_v2.py` - Main application
   - `requirements_scanner_v2.txt` - Dependencies

2. **Install dependencies**:
   ```bash
   pip install -r requirements_scanner_v2.txt
   ```

3. **Install optional external tools**:
   ```bash
   # Python tools
   pip install flake8 pylint bandit safety

   # JavaScript tools (requires Node.js)
   npm install -g eslint
   ```

## 🎯 Usage

1. **Start the scanner**:
   ```bash
   streamlit run enhanced_scanner_v2.py
   ```

2. **Configure your scan**:
   - Enter the project path
   - Select a scan profile (Quick, Security, Quality, or Full)
   - Configure advanced options if needed

3. **Run the scan**:
   - Click "🚀 Start Scan"
   - Monitor progress in real-time
   - Review results in the dashboard

## 📋 Scan Profiles

### Quick Scan
- **Purpose**: Fast analysis for immediate feedback
- **Max files**: 200
- **Features**: Security scan, basic quality checks
- **Best for**: Pre-commit checks, quick reviews

### Security Focus
- **Purpose**: Deep security vulnerability analysis
- **Max files**: 1000
- **Features**: All security patterns, dependency vulnerabilities, Bandit tool
- **Best for**: Security audits, pre-deployment checks

### Code Quality
- **Purpose**: Comprehensive code quality analysis
- **Max files**: 500
- **Features**: Linting tools, code smells, technical debt
- **Best for**: Code reviews, refactoring planning

### Full Analysis
- **Purpose**: Complete analysis with all features
- **Max files**: Unlimited
- **Features**: Everything enabled
- **Best for**: Periodic health checks, major releases

## 📊 Understanding Results

### Health Scores
- **Overall Health**: Weighted average (Security 40%, Quality 35%, Performance 25%)
- **Security Score**: Based on vulnerability count and severity
- **Quality Score**: Based on syntax errors and technical debt
- **Performance Score**: Based on performance anti-patterns

### Issue Severities
- 🔴 **Critical**: Immediate action required (e.g., exposed private keys)
- 🟠 **High**: Should be fixed soon (e.g., hardcoded passwords)
- 🟡 **Medium**: Plan to fix (e.g., weak cryptography)
- 🟢 **Low**: Nice to fix (e.g., TODOs)

## 🔧 Configuration

### Custom File Extensions
Edit `SCAN_EXTENSIONS` in the code to add more file types:
```python
SCAN_EXTENSIONS = ['.py', '.js', '.ts', '.jsx', '.tsx', '.java', '.cs', '.go', '.rs', '.rb', '.php']
```

### Ignore Directories
Edit `IGNORE_DIRS` to skip specific folders:
```python
IGNORE_DIRS = ['__pycache__', '.git', 'venv', 'env', 'node_modules', 'vendor']
```

### Security Patterns
Add custom security patterns in `SECURITY_PATTERNS`:
```python
SECURITY_PATTERNS = {
    'Custom Category': {
        'pattern_name': {
            'pattern': r'your_regex_here',
            'severity': 'high',
            'description': 'Pattern description'
        }
    }
}
```

## 🚀 CI/CD Integration

### GitHub Actions
```yaml
- name: Run TuoKit Scanner
  run: |
    pip install -r requirements_scanner_v2.txt
    python -m streamlit run enhanced_scanner_v2.py --server.headless true
    
- name: Upload SARIF
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: tuokit_scan_results.sarif
```

### GitLab CI
```yaml
scan:
  script:
    - pip install -r requirements_scanner_v2.txt
    - streamlit run enhanced_scanner_v2.py --server.headless true
  artifacts:
    reports:
      sast: tuokit_scan_results.sarif
```

## 📈 Comparison with Original Scanner

| Feature | Original Scanner | Enhanced Scanner v2 |
|---------|-----------------|-------------------|
| Security Analysis | ❌ | ✅ Comprehensive patterns |
| Technology Detection | ❌ | ✅ 8+ languages |
| Dependency Analysis | ❌ | ✅ 6 package managers |
| External Tools | ❌ | ✅ 4+ tools integrated |
| Visual Dashboard | Basic | Advanced with Plotly |
| Scan Profiles | ❌ | ✅ 4 predefined profiles |
| SARIF Export | ❌ | ✅ CI/CD ready |
| Performance Analysis | Limited | Enhanced patterns |

## 🐛 Troubleshooting

### Common Issues

1. **"Module not found" errors**:
   ```bash
   pip install -r requirements_scanner_v2.txt
   ```

2. **External tools not detected**:
   - Install the tools as shown in the installation section
   - Make sure they're in your system PATH

3. **Scan taking too long**:
   - Use Quick Scan profile
   - Reduce max files in advanced options
   - Add large directories to IGNORE_DIRS

4. **Memory issues with large projects**:
   - Use file size limits
   - Scan subdirectories separately
   - Use Quick Scan profile

## 🤝 Contributing

To add new features:

1. **New Security Patterns**: Add to `SECURITY_PATTERNS`
2. **New Languages**: Update `TechnologyDetector.detectors`
3. **New Package Managers**: Update `DependencyAnalyzer.package_managers`
4. **New External Tools**: Update `ExternalToolRunner.tools`

## 📄 Export Formats

- **Markdown Report**: Human-readable summary
- **JSON Metrics**: Machine-readable data
- **SARIF**: Security analysis results for CI/CD
- **CSV** (coming in v3): Spreadsheet-friendly format

## 🔮 Future Enhancements (Phase 3)

- Interactive code viewer with inline fixes
- Historical trend analysis
- AI-powered fix suggestions
- Team collaboration features
- Custom rule creation UI
- Integration with issue trackers

## 📞 Support

- Report issues in the project repository
- Check logs in the Streamlit console for debugging
- Use verbose mode for detailed output

---

**Version**: 2.0  
**License**: MIT  
**Author**: TuoKit Team  
**Last Updated**: 2024
