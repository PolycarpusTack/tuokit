# Diagnostic Toolkit Consolidation

## Overview

The Diagnostic Toolkit consolidates six diagnostic and fix tools into a comprehensive system health checker and auto-fixer for TuoKit. This unified toolkit provides both GUI and command-line interfaces for diagnosing and resolving common issues.

## Consolidated Tools

### Original Files Consolidated:
1. **fix_ollama_auto.py** - Automatic Ollama host detection and fixes
2. **fix_ollama_detection.py** - Ollama connection detection logic
3. **fix_model_selection.py** - Model reference fixes in codebase
4. **check_before_migration.py** - Pre-migration validation
5. **check_ollama_integration.py** - Comprehensive Ollama checks
6. **detect_ollama_host.py** - WSL-aware host detection

## Key Features

### 1. Ollama Diagnostics
- **Auto-detection**: Tests multiple host configurations automatically
- **WSL Support**: Detects Windows host IP from WSL environment
- **Connection Testing**: Validates API endpoint with timeout
- **Model Verification**: Checks installed vs recommended models
- **Generation Testing**: Validates actual text generation capability

### 2. System Diagnostics
- **Python Version Check**: Ensures minimum Python 3.8
- **Package Validation**: Checks all required packages with version constraints
- **Disk Space Monitoring**: Validates sufficient free space
- **File Permissions**: Verifies read/write access to critical paths

### 3. Database Diagnostics
- **Multi-Database Support**: PostgreSQL, SQLite, MySQL
- **Connection Testing**: Validates database connectivity
- **Authentication Checks**: Identifies credential issues
- **Server Status**: Detects if database service is running

### 4. Migration Readiness
- **Script Verification**: Ensures all migration scripts present
- **Target File Inventory**: Counts files to be migrated
- **Dependency Validation**: Checks migration prerequisites
- **Backup Space**: Ensures sufficient space for backups

### 5. Auto-Fix Capabilities
- **Ollama Host Update**: Automatically updates .env with working host
- **Model Reference Fixes**: Updates hardcoded models to use session state
- **Cache Clearing**: Removes Streamlit cache to fix stuck states
- **Configuration Backup**: Always backs up before making changes

## Platform-Specific Features

### WSL Detection and Support
```python
def is_wsl() -> bool:
    return 'microsoft-standard' in platform.uname().release.lower()

def get_windows_host_ip() -> Optional[str]:
    # Method 1: From /etc/resolv.conf
    # Method 2: From ip route command
```

### Host Priority Order
1. Current configured host (from .env)
2. WSL Windows host (if in WSL)
3. localhost:11434
4. 127.0.0.1:11434
5. host.docker.internal:11434 (Docker)

## Diagnostic Result Structure

Each diagnostic returns a structured result:
```python
class DiagnosticResult:
    name: str              # Diagnostic name
    status: str            # success, warning, error
    message: str           # Summary message
    details: List[str]     # Detailed findings
    fix_available: bool    # If auto-fix exists
    fix_command: str       # Fix command/function
```

## Usage Modes

### 1. Streamlit GUI Mode
```bash
streamlit run diagnostic_toolkit.py
```

Features:
- Interactive diagnostic selection
- Real-time results display
- One-click fixes
- Export diagnostic reports
- Visual status indicators

### 2. Command Line Mode
```bash
python diagnostic_toolkit.py --cli
```

Features:
- Runs all diagnostics automatically
- Console output with color coding
- Saves report to markdown file
- Non-interactive operation

## Diagnostic Categories

### Ollama Diagnostics
1. Host detection and connectivity
2. Model availability check
3. Generation capability test

### System Diagnostics
1. Python version compatibility
2. Required package installation
3. Available disk space
4. File/directory permissions

### Database Diagnostics
1. Connection validation
2. Authentication verification
3. Service availability

### Migration Diagnostics
1. Script presence verification
2. Target file counting
3. Prerequisite validation

## Auto-Fix Functions

### 1. Update Ollama Host
```python
update_ollama_host(new_host: str) -> bool
```
- Backs up current .env
- Updates TUOKIT_OLLAMA_HOST
- Preserves other settings

### 2. Fix Model References
```python
fix_model_references() -> DiagnosticResult
```
- Scans all Python files
- Replaces hardcoded models
- Uses session state references

### 3. Clear Streamlit Cache
```python
clear_streamlit_cache() -> DiagnosticResult
```
- Removes session state cache
- Clears disk cache
- Fixes stuck UI states

## Configuration

The toolkit uses centralized configuration:
```python
DIAGNOSTIC_CONFIG = {
    "timeout": 2.0,
    "test_hosts": [...],
    "recommended_models": [...],
    "required_packages": [...],
    "migration_scripts": [...],
    "min_python_version": "3.8",
    "min_disk_space_gb": 2.0
}
```

## Benefits

1. **Comprehensive Coverage**: All diagnostic needs in one tool
2. **Auto-Fix Capability**: Resolves common issues automatically
3. **Platform Awareness**: Special handling for WSL, Docker
4. **Dual Interface**: Both GUI and CLI modes
5. **Export Reports**: Markdown reports for documentation
6. **Safe Operations**: Always backs up before changes

## Migration from Individual Tools

Users of individual diagnostic scripts can now:
1. Use the unified diagnostic toolkit for all checks
2. Access auto-fix capabilities not available before
3. Get comprehensive reports in one run
4. Benefit from improved WSL detection

The diagnostic toolkit serves as the primary health check and maintenance tool for TuoKit installations.