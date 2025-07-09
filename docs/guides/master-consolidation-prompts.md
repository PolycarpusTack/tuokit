# 🎯 TuoKit Master Consolidation Prompts
*Each prompt captures 100% of functionality from all files to be consolidated*

## 1. 🚂 Ruby/Rails Development Suite Consolidation

### Files to Analyze and Consolidate:
```
Rails Tools (8 files):
1. rails_controller_gen.py - RESTful controller generation with API support
2. rails_model_gen.py - Model generation with validations and associations
3. rails_scaffold.py - Full scaffold generation
4. rails_graphql.py - GraphQL schema and resolver generation
5. rails_system_tests.py - System test generation
6. rails_upgrader.py - Rails version upgrade assistant
7. rails_debugger.py - Debugging helpers and error analysis
8. rspec_generator.py - RSpec test generation
9. view_components.py - ViewComponent generation

Ruby Tools (6 files):
1. ruby_profiler.py - Performance profiling
2. ruby_memory_optimizer.py - Memory usage optimization
3. ruby_pattern_matching.py - Pattern matching helpers
4. ruby_c_extensions.py - C extension helpers
5. ruby_ractors.py - Concurrency with Ractors
6. ruby_katas.py - Practice kata generation
```

### Master Consolidation Prompt:
"Create a consolidated Ruby/Rails toolkit that preserves ALL functionality from the 15 Ruby/Rails files. The consolidated solution should have:

1. **rails_toolkit.py** containing:
   - RESTful controller generator with: standard actions (index, show, new, create, edit, update, destroy), API mode support, authentication (JWT, devise, basic), strong parameters, before_actions, response formats (JSON, HTML, XML)
   - Model generator with: validations (presence, uniqueness, format, custom), associations (belongs_to, has_many, has_one, HABTM), scopes, callbacks, concerns, database indexes, migrations
   - Scaffold generator combining: controller, model, views, routes, tests, with customizable templates
   - GraphQL generator for: types, queries, mutations, subscriptions, with relay support
   - System test generator for: Capybara tests, JavaScript testing, screenshot support
   - Rails upgrader analyzing: deprecations, gem compatibility, configuration changes, providing upgrade paths
   - Debugging tools for: error analysis, performance bottlenecks, N+1 queries, memory leaks
   - RSpec generator for: model specs, controller specs, request specs, feature specs, with factory support
   - ViewComponent generator with: component classes, templates, previews, tests

2. **ruby_toolkit.py** containing:
   - Performance profiler using: CPU profiling, memory profiling, allocation tracking, flame graphs, call stack analysis
   - Memory optimizer detecting: memory leaks, object allocation patterns, GC optimization, heap analysis
   - Pattern matching helpers for: case patterns, array patterns, hash patterns, custom matchers
   - C extension helpers for: FFI integration, native extension building, type conversions
   - Ractor concurrency tools for: parallel processing, actor model, message passing, isolation
   - Kata generator creating: algorithm challenges, data structure exercises, with test suites

Ensure each tool maintains its original functionality, configuration options, and output formats. Include all error handling, validation, and edge cases from the original implementations."

## 2. 🧪 Test Infrastructure Consolidation

### Files to Analyze:
```
Test files (20+):
- test_agent_lite.py, test_agent_system.py
- test_sql_generator.py, test_sql_optimizer.py, test_sql_pipeline.py
- test_ollama.py, test_ollama_autodetect.py, test_ollama_connection.py
- test_knowledge_graph.py
- test_app.py
- Various other test_*.py files
```

### Master Consolidation Prompt:
"Create a unified test runner (test_runner.py) that consolidates ALL test functionality from 20+ test files. The runner should:

1. **Test Discovery**: Automatically find all tests, support multiple test frameworks (unittest, pytest), handle both unit and integration tests
2. **Test Categories**: SQL tests, Agent tests, Ollama integration tests, UI tests, Knowledge graph tests, Tool-specific tests
3. **Test Execution**: Run all tests, run by category, run specific test files, parallel execution support, timeout handling
4. **Reporting**: Console output with colors, HTML reports, JSON reports for CI/CD, coverage reports, performance metrics
5. **Special Features**: Setup/teardown for database tests, mock Ollama responses, fixture management, test data generation
6. **Configuration**: Test settings, environment variables, database connections, skip conditions

Preserve all existing test logic, assertions, and test data from original files."

## 3. 🔧 Setup & Migration Consolidation

### Files to Analyze:
```
Setup Scripts:
- setup_database.py - Database initialization
- setup_local.py - Local environment setup
- setup_unified_database.py - Unified DB setup
- quick_db_setup.py - Quick setup
- Various .bat and .sh scripts

Migration Files:
- database_migration_*.sql (7+ files)
- migrate_tuokit.py
- migration guides and SQL files
```

### Master Consolidation Prompt:
"Create a consolidated setup and migration management system preserving ALL functionality:

1. **setup_manager.py** with:
   - Database setup for: PostgreSQL, SQLite, MySQL, with connection testing
   - Environment setup: Python venv, dependencies, Ollama configuration
   - Quick setup mode: One-command setup with defaults
   - Advanced setup: Custom configurations, multiple databases
   - Verification: Check all components, diagnose issues, fix common problems
   - OS support: Windows (.bat), Linux/Mac (.sh), cross-platform Python

2. **migration_manager.py** with:
   - Version tracking: Current version, available migrations, migration history
   - Migration execution: Up/down migrations, specific version targeting, dry-run mode
   - Rollback support: Undo migrations, backup before migration
   - Migration types: Schema changes, data migrations, index creation
   - All existing migrations from: agents, knowledge graph, Ruby tools, SQL tools

Include all error handling, rollback logic, and safety checks from original implementations."

## 4. 🐛 Error Analysis Suite Consolidation

### Files to Analyze:
```
1. error_tool.py - Error decoder with pattern matching
2. exception_advisor.py - Exception handling best practices
3. crash_analyzer.py - Crash dump analysis (with 5MB file support)
```

### Master Consolidation Prompt:
"Consolidate all error analysis tools into error_analysis_toolkit.py preserving:

1. **Error Decoder** from error_tool.py:
   - Pattern matching for: MessageNotUnderstood, NullPointerException, OutOfMemoryError, IndexError, etc.
   - Educational content with: explanations, analogies, causes, fixes
   - Multi-language support: Python, Ruby, Java, JavaScript, Smalltalk
   - Context-aware analysis

2. **Exception Advisor** from exception_advisor.py:
   - Exception handling analysis
   - Best practices recommendations
   - Error recovery strategies
   - Logging and monitoring advice

3. **Crash Analyzer** from crash_analyzer.py:
   - Large file support (up to 5MB)
   - Smart chunking for large dumps
   - Pattern recognition with context
   - Java, Python, Ruby crash formats
   - Memory dump analysis
   - Stack trace parsing
   - Root cause identification

Include all configuration options, chunk processing logic, and pattern databases."

## 5. 🦆 Smalltalk Development Suite Consolidation

### Files to Analyze:
```
1. smalltalk_class_gen.py - Class generation
2. smalltalk_explainer.py - Code explanation
3. smalltalk_meta.py - Metaprogramming tools
4. smalltalk_refactorer.py - Refactoring assistance
5. smalltalk_ruby_converter.py - Ruby conversion
6. smalltalk_snippets.py - Code snippet library
7. seaside_generator.py - Seaside web framework
```

### Master Consolidation Prompt:
"Create smalltalk_toolkit.py consolidating ALL Smalltalk tools:

1. **Class Generator**: Instance/class variables, methods with categories, inheritance, traits, metaclass programming
2. **Code Explainer**: Message sending, blocks, collections, reflection, syntax explanation
3. **Metaprogramming**: Dynamic method creation, class modification, doesNotUnderstand:, method wrappers
4. **Refactoring Tools**: Extract method, rename, move method, inline, extract class
5. **Ruby Converter**: Syntax conversion, idiom mapping, block translation, message conversion
6. **Snippet Library**: Common patterns, collection operations, control structures, best practices
7. **Seaside Generator**: Component creation, rendering methods, callbacks, AJAX support

Maintain all original features, examples, and pattern databases."

## 6. 📚 Learning & Documentation Consolidation

### Files to Analyze:
```
Learning:
1. edu_mind.py - Adaptive learning system
2. study_guide_generator.py - Study guide creation
3. sql_academy.py - SQL learning platform

Documentation:
1. doc_tools.py - Document Q&A
2. knowledge_lib.py - Knowledge base interface
3. help_guide.py - Help system
```

### Master Consolidation Prompt:
"Consolidate into learning_toolkit.py and documentation_toolkit.py:

1. **learning_toolkit.py**:
   - EduMind adaptive learning: skill tracking, personalized paths, quiz generation, progress analytics
   - Study guide generator: topic extraction, summary creation, practice problems, cheat sheets
   - SQL Academy: interactive lessons, query challenges, concept explanation, progress tracking

2. **documentation_toolkit.py**:
   - Document Q&A: PDF/text extraction, context retrieval, question answering, source citations
   - Knowledge library: search, categorization, version tracking, collaborative editing
   - Help system: contextual help, tool documentation, examples, troubleshooting

Preserve all algorithms, UI components, and data structures."

## 7. 🔍 Diagnostic Tools Consolidation

### Files to Analyze:
```
- fix_ollama_auto.py
- fix_ollama_detection.py
- fix_model_selection.py
- check_before_migration.py
- check_ollama_integration.py
- detect_ollama_host.py
```

### Master Consolidation Prompt:
"Create diagnostic_toolkit.py combining ALL diagnostic and fix tools:

1. **Ollama Diagnostics**: Auto-detection, WSL support, connection testing, model availability
2. **Model Management**: Model selection fixes, model downloading, model verification
3. **Migration Checks**: Pre-migration validation, dependency checks, compatibility verification
4. **System Diagnostics**: Python version, package conflicts, database connectivity, file permissions
5. **Auto-fix Capabilities**: Common issue resolution, configuration updates, environment fixes

Include all detection logic, fix strategies, and platform-specific handling."

## 🚀 Implementation Strategy

### For Each Consolidation:
1. **Analyze** all source files for complete functionality
2. **Design** unified interface preserving all features
3. **Implement** with backward compatibility
4. **Test** against original functionality
5. **Document** the consolidated API
6. **Migrate** existing code to use new structure

### Priority Order:
1. Ruby/Rails (highest impact)
2. Test Infrastructure
3. Setup & Migration
4. Error Analysis
5. Others as needed

### Safety Measures:
- Full backup before each consolidation
- Side-by-side testing
- Gradual migration
- Rollback capability