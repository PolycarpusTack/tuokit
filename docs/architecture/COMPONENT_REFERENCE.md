# TuoKit Component Reference Guide

## Table of Contents
1. [Core Components](#core-components)
2. [Consolidated Toolkits](#consolidated-toolkits)
3. [Individual Tools](#individual-tools)
4. [Support Systems](#support-systems)
5. [Database Schema](#database-schema)
6. [API Reference](#api-reference)

## Core Components

### tuokit_launcher.py
**Purpose**: Universal launcher providing setup, diagnostics, and application launching

**Key Classes**:
- `TuoKitLauncher`: Main launcher class

**Key Methods**:
```python
check_python_version()      # Verify Python 3.8+
check_venv()               # Check virtual environment
create_venv()              # Create virtual environment
install_requirements()      # Install dependencies
check_ollama()             # Verify Ollama installation
check_database()           # Validate database config
run_diagnostics()          # Execute diagnostic toolkit
launch_app(app_choice)     # Launch TuoKit variant
quick_start()              # Fast launch with checks
full_setup()               # Complete setup wizard
interactive_menu()         # Display interactive menu
```

**Usage**:
```bash
python tuokit_launcher.py              # Interactive menu
python tuokit_launcher.py --quick      # Quick start
python tuokit_launcher.py --setup      # Full setup
python tuokit_launcher.py --launch     # Direct launch
```

### tuokit_unified.py
**Purpose**: Single unified entry point consolidating all TuoKit applications

**Key Data Structures**:
```python
TOOL_REGISTRY = {
    "category_id": {
        "name": str,
        "description": str,
        "tools": {
            "tool_id": {
                "name": str,
                "icon": str,
                "description": str,
                "type": "page|toolkit|internal",
                "page": str,        # for page type
                "function": str     # for toolkit/internal
            }
        }
    }
}
```

**Key Functions**:
```python
get_tool_stats()           # Retrieve usage statistics
show_home_dashboard()      # Display main dashboard
show_analytics()           # Show analytics dashboard
show_activity_feed()       # Display recent activity
show_settings()            # Configuration interface
load_tool(tool_info)       # Dynamic tool loader
```

## Consolidated Toolkits

### sql_toolkit.py
**Consolidates**: sql_generator.py, sql_optimizer.py, sql_pipeline.py

**Features**:
- Natural language to SQL conversion
- Query optimization
- Pipeline execution
- Multi-dialect support (PostgreSQL, MySQL, SQLite)

**Key Functions**:
```python
generate_sql(prompt, dialect)      # NL to SQL
optimize_query(query, db_type)     # Optimize SQL
execute_pipeline(operations)       # Run SQL pipeline
analyze_query_plan(query)          # Performance analysis
```

### error_analysis_toolkit.py
**Consolidates**: error_tool.py, exception_advisor.py, crash_analyzer.py

**Enhanced Features**:
- Support for 5MB files (200 chunks)
- Pattern synthesis across chunks
- Multi-language error analysis

**Key Functions**:
```python
decode_error(error_message)        # Decode error messages
advise_exception(exception_data)   # Provide solutions
analyze_crash_dump(file_path)      # Analyze crash files
analyze_with_chunking(content)     # Handle large files
synthesize_chunk_results(results)  # Combine analyses
```

### smalltalk_toolkit.py
**Consolidates**: 7 SmallTalk development tools

**Tool Registry**:
```python
TOOL_REGISTRY = {
    "class_generator": {...},
    "explainer": {...},
    "converter": {...},
    "refactorer": {...},
    "meta_programmer": {...},
    "snippet_generator": {...},
    "seaside_generator": {...}
}
```

**Key Functions**:
```python
generate_class(requirements)       # Create SmallTalk classes
explain_code(code)                # Explain SmallTalk code
convert_to_ruby(smalltalk_code)   # Convert to Ruby
refactor_code(code, patterns)     # Apply refactoring
generate_meta_programs(spec)       # Meta-programming
```

### learning_toolkit.py
**Consolidates**: edu_mind.py, study_guide_generator.py, sql_academy.py

**Features**:
- Spaced repetition (SM-2 algorithm)
- Adaptive learning paths
- Progress tracking
- Interactive exercises

**Key Functions**:
```python
calculate_spaced_repetition(item)  # SM-2 algorithm
generate_study_guide(topic)        # Create guides
track_progress(user_id, metric)    # Monitor learning
create_flashcard(content)          # Generate cards
run_sql_exercise(difficulty)       # SQL practice
```

### documentation_toolkit.py
**Consolidates**: doc_tools.py, knowledge_lib.py, help_guide.py

**Features**:
- Document chunking
- Knowledge graph integration
- Search capabilities
- Help system

**Key Functions**:
```python
chunk_document(text, size)         # Split documents
save_to_knowledge(content)         # Store knowledge
search_knowledge(query)            # Search database
generate_documentation(code)       # Auto-document
provide_help(topic)               # Context help
```

### diagnostic_toolkit.py
**Consolidates**: 6 diagnostic and auto-fix tools

**Key Classes**:
```python
class DiagnosticResult:
    name: str
    status: str  # "success", "warning", "error"
    message: str
    details: List[str]
    fix_available: bool
    fix_command: str
```

**Diagnostic Functions**:
```python
detect_ollama_host()              # Auto-detect Ollama
check_ollama_models()             # Verify models
test_ollama_generation()          # Test generation
check_python_version()            # Python version
check_required_packages()         # Package validation
check_database_connection()       # DB connectivity
check_migration_readiness()       # Migration status
```

**Auto-Fix Functions**:
```python
update_ollama_host(new_host)      # Update .env
fix_model_references()            # Fix hardcoded models
clear_streamlit_cache()           # Clear cache
```

## Individual Tools

### Code Intelligence Tools
Located in `pages/` directory:

- **code_explainer.py**: Explain code in any language
- **enhanced_scanner_v2.py**: Deep code analysis
- **code_debugger.py**: AI-assisted debugging
- **code_generator.py**: Generate code from descriptions
- **code_formatter.py**: Format and beautify code

### Specialized Tools
- **crash_analyzer.py**: Standalone crash analysis
- **agent_hub.py**: Unified agent management
- **migration_dashboard.py**: Migration monitoring
- **rails_toolkit.py**: Rails development tools
- **ruby_toolkit.py**: Ruby development suite

## Support Systems

### migration_manager.py
**Purpose**: Manage database migrations and tool consolidations

**Key Class**:
```python
class MigrationManager:
    def __init__(self, db_type: str = None)
    def initialize_migration_tracking()
    def get_current_version()
    def get_pending_migrations()
    def backup_database(reason: str)
    def apply_sql_migration(version: str, dry_run: bool)
    def rollback_migration(version: str)
    def migrate_to_version(target: str, dry_run: bool)
    def consolidate_tools(key: str, dry_run: bool)
    def verify_integrity()
```

**Migration Registry**:
```python
sql_migrations = {
    'v0.1.0': {'file': 'database_setup.sql', ...},
    'v0.4.0': {'file': 'database_migration_v0.4.sql', ...},
    # ... more versions
}
```

### setup_manager.py
**Purpose**: System setup and configuration management

**Key Features**:
- Interactive setup wizard
- Database initialization
- Environment configuration
- Platform detection

### DatabaseManager (utils/database.py)
**Purpose**: Database abstraction layer

**Key Methods**:
```python
execute_query(query, params)      # Execute SQL
save_query(tool, prompt, response)# Log queries
save_knowledge(content, metadata) # Store knowledge
search_knowledge(query)           # Search data
get_connection()                  # Get DB connection
```

## Database Schema

### Core Tables

#### queries
```sql
CREATE TABLE queries (
    id SERIAL PRIMARY KEY,
    tool VARCHAR(100),
    model VARCHAR(100),
    user_prompt TEXT,
    ai_response TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

#### knowledge_units
```sql
CREATE TABLE knowledge_units (
    id SERIAL PRIMARY KEY,
    query_id INTEGER REFERENCES queries(id),
    title VARCHAR(255),
    content TEXT,
    category VARCHAR(100),
    tags TEXT[],
    verified BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

#### migration_history
```sql
CREATE TABLE migration_history (
    id SERIAL PRIMARY KEY,
    version VARCHAR(20) UNIQUE,
    migration_type VARCHAR(20),
    description TEXT,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    rollback_at TIMESTAMP,
    status VARCHAR(20) DEFAULT 'applied'
);
```

## API Reference

### Ollama Integration
```python
# utils/ollama.py
def get_available_models() -> List[str]
def safe_ollama_generate(prompt: str, model: str) -> str
def test_ollama_connection() -> bool
```

### Session State Management
```python
# Streamlit session state keys
st.session_state.selected_model      # Current AI model
st.session_state.selected_tool       # Active tool
st.session_state.selected_category   # Tool category
st.session_state.diagnostic_results  # Diagnostic data
```

### Environment Variables
```bash
# Database
TUOKIT_DB_TYPE          # postgresql|sqlite|mysql
TUOKIT_PG_HOST         # PostgreSQL host
TUOKIT_PG_PORT         # PostgreSQL port
TUOKIT_PG_DB           # Database name
TUOKIT_PG_USER         # Database user
TUOKIT_PG_PASSWORD     # Database password

# Ollama
TUOKIT_OLLAMA_HOST     # Ollama API endpoint

# Features
TUOKIT_ENABLE_ANALYTICS      # Enable usage tracking
TUOKIT_ENABLE_EXPERIMENTAL   # Show experimental features
```

## Usage Examples

### Basic Tool Usage
```python
# Launch unified app
streamlit run tuokit_unified.py

# Direct toolkit access
from learning_toolkit import main as learning_main
learning_main()

# Database operations
from utils import DatabaseManager
db = DatabaseManager()
results = db.execute_query("SELECT * FROM queries LIMIT 10")
```

### Migration Operations
```bash
# Check status
python migration_manager.py --status

# Run migrations
python migration_manager.py --migrate

# Specific version
python migration_manager.py --version v2.0.0

# Tool consolidation
python migration_manager.py --consolidate sql
```

### Diagnostic Operations
```bash
# Run all diagnostics
python diagnostic_toolkit.py --cli

# Streamlit interface
streamlit run diagnostic_toolkit.py
```

## Error Handling

### Common Patterns
```python
try:
    result = operation()
    if result:
        st.success("Operation completed")
    else:
        st.warning("Operation returned no results")
except Exception as e:
    st.error(f"Operation failed: {e}")
    # Log to database
    db.save_query(tool_name, prompt, f"ERROR: {e}")
```

### Diagnostic Results
```python
result = DiagnosticResult("Check Name")
if condition_met:
    result.set_success("All good", ["Detail 1", "Detail 2"])
else:
    result.set_error("Problem found", ["Error detail"], 
                     fix_command="command_to_fix()")
```

## Best Practices

### 1. Tool Development
- Follow the consolidation pattern
- Use session state for configuration
- Implement error handling
- Add to TOOL_REGISTRY

### 2. Database Operations
- Use parameterized queries
- Handle connection failures
- Implement transaction support
- Add appropriate indexes

### 3. UI Development
- Use Streamlit components consistently
- Implement loading states
- Provide clear feedback
- Follow the established CSS theme

### 4. Testing
- Test with multiple database types
- Verify Ollama connectivity
- Check platform compatibility
- Validate migration scripts