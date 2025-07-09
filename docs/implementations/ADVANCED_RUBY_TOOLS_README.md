# Advanced Ruby Tools for TuoKit

## Overview
This implementation adds three advanced Ruby/Rails development tools to TuoKit:

1. **Ruby Pattern Matching Explorer** - Master Ruby 3's pattern matching features
2. **Ractor & Concurrency Advisor** - Implement parallel processing with Ractors
3. **GraphQL API Builder** - Create production-ready GraphQL APIs

## Files Added/Modified

### New Pages
- `pages/ruby_pattern_matching.py` - Pattern matching analysis and generation
- `pages/ruby_ractors.py` - Concurrency implementation advisor
- `pages/rails_graphql.py` - GraphQL API schema generator

### New Utilities
- `utils/pattern_utils.py` - Pattern matching analysis utilities
- `utils/concurrency_utils.py` - Concurrency and thread safety analysis
- `utils/graphql_utils.py` - GraphQL schema and query utilities

### Database Migration
- `database_migration_advanced_ruby.sql` - Schema for advanced features

### Modified Files
- `app.py` - Added navigation and dashboard buttons
- `utils/__init__.py` - Exported new utility classes

## Features

### Ruby Pattern Matching Explorer
- **Pattern Analysis**: 
  - Deconstructs existing pattern matching code
  - Explains how values are matched and bound
  - Shows equivalent non-pattern matching alternatives
- **Pattern Generation**:
  - Creates examples based on use cases
  - Supports multiple pattern types (array, hash, guard, alternative)
  - Complexity levels: Simple, Intermediate, Advanced
- **Educational Resources**:
  - Pattern matching fundamentals
  - Common use case examples
  - Ruby 3+ syntax guide

### Ractor & Concurrency Advisor
- **Ractor Implementation**:
  - Generates complete Ractor-based solutions
  - Configurable worker count and communication models
  - Fault tolerance and supervisor patterns
- **Concurrency Analysis**:
  - Detects thread safety issues
  - Recommends appropriate concurrency models
  - Ractor compatibility checking
- **Performance Guidance**:
  - Estimates speedup potential
  - Compares concurrency models (Threads, Ractors, Fibers, Async)
  - Synchronization primitive recommendations

### GraphQL API Builder
- **Complete API Generation**:
  - Type definitions with fields
  - Query, Mutation, and Subscription resolvers
  - N+1 prevention with BatchLoader
  - Authentication integration
- **Configuration Options**:
  - Multiple authentication methods (JWT, Devise, API Key)
  - Pagination strategies (Cursor, Offset, Relay)
  - Tracing and rate limiting
  - Field-level authorization
- **Testing & Documentation**:
  - RSpec test generation
  - Example queries and mutations
  - Setup instructions
  - Best practices guide

## Installation

1. **Run Database Migrations** (in order):
   ```bash
   # First ensure base schema is installed
   psql -U ollama_user -d ollama_knowledge -f database_setup.sql
   
   # Then the Ruby tools migration
   psql -U ollama_user -d ollama_knowledge -f database_migration_ruby_tools.sql
   
   # Finally the advanced Ruby tools migration
   psql -U ollama_user -d ollama_knowledge -f database_migration_advanced_ruby.sql
   ```

2. **Ensure Ollama Models**:
   ```bash
   ollama pull deepseek-r1:latest
   ollama pull deepseek-coder:latest
   ```

3. **Install Ruby Gems** (for reference examples):
   ```bash
   gem install graphql batch-loader graphiql-rails
   gem install concurrent-ruby async
   ```

4. **Restart TuoKit**:
   ```bash
   # Windows
   ./start_tuokit.bat
   
   # Linux/Mac
   ./start_tuokit.sh
   ```

## Usage Examples

### Pattern Matching Explorer
```ruby
# Analyze complex pattern matching
case response
in {status: 200, data: {users: [*, {admin: true} => admin, *]}}
  process_admin(admin)
in {status: 404}
  handle_not_found
end
```

### Ractor Implementation
```ruby
# Generate parallel image processor
workers = 4.times.map do
  Ractor.new do
    loop do
      img = Ractor.receive
      processed = ImageProcessor.process(img)
      Ractor.yield processed
    end
  end
end
```

### GraphQL API
```ruby
# Generate complete CRUD API
# Resource: Post
# Fields: title, content, author, comments
# Operations: Query, Mutation, Subscription
# Auth: JWT
# Pagination: Relay
```

## Advanced Features

### Pattern Matching
- **Supported Patterns**:
  - Array destructuring: `in [first, *rest]`
  - Hash patterns: `in {name:, age: 18..}`
  - Guard clauses: `in [x, y] if x > y`
  - As patterns: `in [x, y] => point`
  - Alternative patterns: `in 0 | 1 | 2`
  - Find patterns: `in [*, target, *]`

### Concurrency Models
- **Ractors**: True parallelism without GVL
- **Threads**: Traditional threading with GVL
- **Fibers**: Lightweight cooperative concurrency
- **Async**: Event-driven I/O with Fiber scheduler
- **Processes**: Full isolation with fork

### GraphQL Features
- **Performance**:
  - BatchLoader for N+1 prevention
  - Query complexity analysis
  - Caching strategies
  - Connection-based pagination
- **Security**:
  - Field-level authorization
  - Rate limiting by complexity
  - Introspection control
  - Input validation

## Troubleshooting

### Common Issues

1. **Pattern Matching Syntax Errors**:
   - Ensure Ruby 3.0+ syntax
   - Check for proper case/in structure
   - Verify pattern syntax validity

2. **Ractor Compatibility**:
   - No global variables or class variables
   - Use shareable objects only
   - Avoid dynamic class definitions

3. **GraphQL Schema Issues**:
   - Validate type definitions
   - Check resolver method names
   - Ensure proper authentication setup

### Performance Tips

1. **Pattern Matching**:
   - Order patterns from most to least specific
   - Use guard clauses sparingly
   - Consider performance vs readability

2. **Ractors**:
   - Use for CPU-bound tasks only
   - Minimize message passing overhead
   - Pre-process data before distribution

3. **GraphQL**:
   - Always use DataLoader/BatchLoader
   - Implement query depth limits
   - Cache resolver results when possible

## Future Enhancements

Potential additions to the Ruby toolkit:
- Ruby Memory Profiler with visual heap dumps
- Rails Upgrade Compatibility Checker
- Ruby C Extension Generator
- Metaprogramming Pattern Library
- Performance Regression Detector

## Integration with TuoKit

All tools follow TuoKit's principles:
- **Local-First**: All processing via local Ollama models
- **Knowledge-Centric**: Examples and patterns saved to PostgreSQL
- **Educational**: Comprehensive guides and best practices
- **Practical**: Real-world use cases and production-ready code

For more information, see the main TuoKit documentation.
