# TuoKit Development Setup

## Python Dependencies

Install Python dependencies using pip:

```bash
pip install -r requirements.txt
```

## Security Scanning

TuoKit uses Bandit for Python security scanning:

```bash
# Install (already in requirements.txt)
pip install bandit

# Run security scan
bandit -r . -f json -o security_report.json

# Run with specific severity
bandit -r . -ll  # Only high severity issues
```

## JavaScript/Frontend Tools (Future Development)

While TuoKit is primarily a Python/Streamlit application, future development may include JavaScript components for enhanced UI features. If you're developing JavaScript components:

### ESLint Setup

```bash
# Install ESLint globally
npm install -g eslint

# Or install locally in project (if package.json exists)
npm install --save-dev eslint

# Initialize ESLint configuration
eslint --init

# Run ESLint
eslint your-javascript-file.js
```

### When to use ESLint

ESLint would be needed if:
- Adding custom JavaScript components to Streamlit
- Creating browser extensions for TuoKit
- Building a separate frontend application
- Adding JavaScript-based visualizations

## Python Code Quality Tools

For Python code quality, use these tools (commented in requirements.txt):

```bash
# Code formatting
pip install black
black .

# Linting
pip install flake8
flake8 .

# Type checking
pip install mypy
mypy .

# Security scanning
pip install bandit
bandit -r .
```

## Recommended Development Workflow

1. **Before committing code:**
   ```bash
   # Format code
   black .
   
   # Check for issues
   flake8 .
   
   # Security scan
   bandit -r .
   ```

2. **For JavaScript components (if any):**
   ```bash
   eslint src/**/*.js
   ```

## IDE Setup

### VS Code Extensions
- Python
- Pylance
- Black Formatter
- Bandit
- ESLint (if working with JavaScript)

### PyCharm
- Enable Bandit inspections
- Configure Black as formatter
- Install ESLint plugin (if needed)