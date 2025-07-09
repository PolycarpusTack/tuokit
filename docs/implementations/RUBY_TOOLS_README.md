# Ruby Performance & Testing Tools for TuoKit

## Overview
This implementation adds two powerful Ruby/Rails development tools to TuoKit:

1. **Ruby Performance Profiler** - Comprehensive performance analysis and optimization
2. **Rails System Test Generator** - Production-ready system tests with accessibility checks

## Files Added/Modified

### New Pages
- `pages/ruby_profiler.py` - Ruby Performance Profiler interface
- `pages/rails_system_tests.py` - Rails System Test Generator interface

### New Utilities
- `utils/performance_utils.py` - Performance analysis utilities for Ruby code
- `utils/testing_utils.py` - Test generation utilities for Rails

### Database Migration
- `database_migration_ruby_tools.sql` - Schema updates for performance findings and test cases

### Modified Files
- `app.py` - Added navigation links and buttons for new tools
- `utils/__init__.py` - Exported new utility classes

## Features

### Ruby Performance Profiler
- **Quick Analysis**: Instant complexity estimation
- **Full Analysis**: 
  - Computational complexity (Big O) analysis
  - Automatic code optimization suggestions
  - Memory usage analysis
- **Performance Pattern Detection**:
  - N+1 query detection
  - Array concatenation in loops
  - Missing database indexes
  - Unbounded queries
- **Knowledge Library Integration**: Save analyses for future reference

### Rails System Test Generator
- **Comprehensive Test Generation**:
  - Page object model implementation
  - User journey scenarios
  - Accessibility (a11y) checks
  - JavaScript interaction testing
  - Screenshot on failure
  - Database cleaning strategies
- **Framework Support**: RSpec and Minitest
- **Browser Support**: Chrome, Firefox, Safari
- **JavaScript Drivers**: Selenium, Cuprite, Apparition
- **WCAG 2.1 Compliance**: Built-in accessibility standards

## Installation

1. **Run Database Migration**:
   ```bash
   psql -U ollama_user -d ollama_knowledge -f database_migration_ruby_tools.sql
   ```

2. **Ensure Ollama Models**:
   ```bash
   ollama pull deepseek-r1:latest
   ollama pull deepseek-coder:latest
   ```

3. **Restart TuoKit**:
   ```bash
   # Windows
   ./start_tuokit.bat
   
   # Linux/Mac
   ./start_tuokit.sh
   ```

## Usage

### Ruby Performance Profiler
1. Navigate to "⚡ Ruby Profiler" from the dashboard or sidebar
2. Paste your Ruby code
3. Click "Quick Analysis" for instant complexity estimation
4. Click "Run Full Analysis" for comprehensive performance review
5. Review complexity report, optimized code, and memory analysis
6. Save to knowledge library for future reference

### Rails System Test Generator
1. Navigate to "🧪 System Tests" from the dashboard or sidebar
2. Describe the feature you want to test
3. Configure test settings in the sidebar:
   - Framework (RSpec/Minitest)
   - Browser and JavaScript driver
   - Accessibility and visual testing options
4. Click "Generate Tests" to create comprehensive test suite
5. Download or save to test suite

## TuoKit Integration

Both tools follow TuoKit's core principles:
- **Local-First**: All processing happens locally using Ollama
- **Knowledge-Centric**: Analyses and tests are saved to PostgreSQL
- **Practical Focus**: Real-world Ruby/Rails development needs
- **Educational**: Includes performance patterns and testing best practices

## Future Enhancements

Potential additions to complete the Ruby/Rails toolkit:
- Ruby Memory Profiler with heap dumps
- Rails API Test Generator
- Ruby Gem Dependency Analyzer
- Rails Database Performance Analyzer
- Ruby Style Guide Enforcer

## Technical Notes

- Uses existing TuoKit infrastructure (DatabaseManager, safe_ollama_generate)
- Maintains consistent UI patterns with other TuoKit tools
- Includes comprehensive error handling
- Supports knowledge library integration
- Provides downloadable artifacts

## Troubleshooting

If you encounter issues:
1. Ensure PostgreSQL is running and accessible
2. Verify Ollama models are installed
3. Check database migration was applied successfully
4. Review logs for any error messages

For support, refer to the main TuoKit documentation.
