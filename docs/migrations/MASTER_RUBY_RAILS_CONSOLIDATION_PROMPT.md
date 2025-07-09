# Master Ruby/Rails Consolidation Prompt

## Comprehensive Consolidation Instructions

### 🎯 Objective
Create two unified toolkit modules (`rails_unified_toolkit.py` and `ruby_unified_toolkit.py`) that consolidate ALL functionality from 15 separate Ruby/Rails tools while maintaining 100% feature parity.

### 📋 Complete Feature Requirements

## Rails Unified Toolkit (`rails_unified_toolkit.py`)

### 1. Controller Generator Requirements
From `rails_controller_gen.py`, preserve:
- **RESTful Actions**: All 7 standard actions (index, show, new, create, edit, update, destroy)
- **API Mode Support**: 
  - JSON responses with proper status codes
  - API versioning (v1, v2, etc.)
  - Token-based authentication
- **Authentication Types**:
  - JWT with token refresh
  - Devise integration with all modules
  - Basic HTTP authentication
  - Custom authentication hooks
- **Strong Parameters**:
  - Automatic parameter whitelisting
  - Nested attributes support
  - Array parameters handling
- **Before Actions**:
  - Authentication checks
  - Authorization (CanCanCan/Pundit)
  - Rate limiting
  - Request logging
- **Response Formats**:
  - HTML with layouts
  - JSON with JBuilder
  - XML responses
  - Custom formats
- **Advanced Features**:
  - Nested resources support
  - Concern inclusion
  - Error handling
  - CORS configuration
  - Pagination support

### 2. Model Generator Requirements
From `rails_model_gen.py`, preserve:
- **Validations**:
  - presence, uniqueness, format, length
  - numericality with options
  - custom validations
  - conditional validations
  - validation contexts
- **Associations**:
  - belongs_to with optional/required
  - has_many with dependent options
  - has_one
  - has_and_belongs_to_many
  - has_many :through
  - polymorphic associations
  - self-referential associations
- **Scopes**:
  - Named scopes
  - Default scopes
  - Lambda scopes with parameters
  - Scope chaining
- **Callbacks**:
  - All lifecycle callbacks
  - Conditional callbacks
  - Callback classes
- **Database Features**:
  - Indexes (unique, composite, partial)
  - Foreign keys with constraints
  - Check constraints
  - Database views
  - Triggers
- **Advanced Features**:
  - STI (Single Table Inheritance)
  - Concerns/modules
  - Enum attributes
  - Store accessors
  - Virtual attributes

### 3. Scaffold Generator Requirements
From `rails_scaffold.py`, preserve:
- **Complete CRUD Stack**:
  - Model with all features
  - Controller with all actions
  - Views (index, show, new, edit, _form)
  - Routes with constraints
  - Tests for all components
- **View Features**:
  - Responsive layouts
  - Form helpers with error handling
  - Partial templates
  - AJAX support
  - Turbo integration
- **Customization**:
  - Template engine selection (ERB, Haml, Slim)
  - CSS framework integration
  - JavaScript framework setup
  - I18n support

### 4. GraphQL Generator Requirements
From `rails_graphql.py`, preserve:
- **Type Definitions**:
  - Object types with all fields
  - Input types for mutations
  - Interface types
  - Union types
  - Enum types
  - Custom scalar types
- **Query Support**:
  - Single record queries
  - Collection queries with filtering
  - Nested queries
  - Query complexity analysis
  - N+1 prevention
- **Mutations**:
  - Create mutations with validation
  - Update mutations with partial updates
  - Delete mutations with soft delete
  - Bulk mutations
  - File upload mutations
- **Subscriptions**:
  - Real-time updates
  - ActionCable integration
  - Subscription filters
  - Authorization for subscriptions
- **Relay Support**:
  - Node interface
  - Connection types
  - Cursor-based pagination
  - Global ID support
- **Advanced Features**:
  - Query batching
  - Persisted queries
  - Schema stitching
  - Federation support
  - Rate limiting
  - Query depth limiting

### 5. System Test Generator Requirements
From `rails_system_tests.py`, preserve:
- **Test Scenarios**:
  - User journey tests
  - Form interaction tests
  - JavaScript interaction tests
  - Responsive design tests
  - Accessibility tests (WCAG 2.1)
  - Performance tests
- **Capybara Features**:
  - Multiple driver support (Selenium, Cuprite, Webkit)
  - Screenshot on failure
  - Video recording
  - Network traffic inspection
  - Console log capture
- **Page Objects**:
  - Page object pattern
  - Component objects
  - Shared examples
  - Custom matchers
- **Test Data**:
  - Factory setup
  - Fixtures integration
  - Database cleaning strategies
  - Seed data management

### 6. Rails Upgrader Requirements
From `rails_upgrader.py`, preserve:
- **Compatibility Analysis**:
  - Gem compatibility checking
  - Deprecation scanning
  - Breaking change detection
  - Configuration migration
- **Upgrade Planning**:
  - Step-by-step upgrade path
  - Dual boot configuration
  - Gradual migration strategy
  - Rollback procedures
- **Code Transformation**:
  - Automated code updates
  - Syntax migration
  - API changes adaptation
  - Configuration updates

### 7. Debugger Requirements
From `rails_debugger.py`, preserve:
- **Error Analysis**:
  - Stack trace parsing
  - Error categorization
  - Root cause analysis
  - Similar error detection
- **Performance Issues**:
  - N+1 query detection
  - Slow query analysis
  - Memory leak detection
  - Cache hit rate analysis
- **Debugging Tools**:
  - Breakpoint suggestions
  - Variable inspection
  - Method tracing
  - SQL query logging

### 8. RSpec Generator Requirements
From `rspec_generator.py`, preserve:
- **Test Types**:
  - Model specs with all validations
  - Controller specs with all actions
  - Request specs for APIs
  - Feature specs for integration
  - View specs
  - Helper specs
  - Mailer specs
  - Job specs
- **Testing Patterns**:
  - Shared examples
  - Custom matchers
  - Test doubles (mocks, stubs)
  - Factory integration
  - Database transaction management

### 9. ViewComponent Requirements
From `view_components.py`, preserve:
- **Component Types**:
  - Standard components
  - Slots support
  - Collections rendering
  - Conditional rendering
  - Nested components
- **Stimulus Integration**:
  - Controller generation
  - Action definitions
  - Target management
  - Value properties
- **Testing**:
  - Component unit tests
  - Preview generation
  - Visual regression tests
  - Accessibility tests

## Ruby Unified Toolkit (`ruby_unified_toolkit.py`)

### 1. Performance Profiler Requirements
From `ruby_profiler.py`, preserve:
- **Profiling Modes**:
  - CPU profiling with sampling
  - Memory profiling with allocations
  - Method call profiling
  - Block profiling
  - GC profiling
- **Analysis Features**:
  - Big O complexity detection
  - Hotspot identification
  - Call graph generation
  - Flame graph support
  - Time complexity analysis
- **Optimization Suggestions**:
  - Algorithm improvements
  - Data structure recommendations
  - Caching opportunities
  - Parallel processing options

### 2. Memory Optimizer Requirements
From `ruby_memory_optimizer.py`, preserve:
- **Memory Analysis**:
  - Heap dump analysis
  - Object allocation tracking
  - Memory leak detection
  - Reference analysis
  - GC pressure points
- **Optimization Strategies**:
  - String freezing and pooling
  - Symbol vs string usage
  - Lazy enumeration
  - Object recycling
  - Memory-efficient data structures
- **GC Tuning**:
  - Generation configuration
  - Heap size optimization
  - Collection frequency tuning
  - Compaction strategies

### 3. Pattern Matching Requirements
From `ruby_pattern_matching.py`, preserve:
- **Pattern Types**:
  - Case patterns (in/else)
  - Array patterns with splats
  - Hash patterns with rest
  - Find patterns
  - Alternative patterns (|)
  - Guard clauses
  - Variable binding
  - As patterns (=>)
- **Advanced Patterns**:
  - Nested patterns
  - Custom pattern objects
  - Pin operators (^)
  - Type checking patterns

### 4. C Extension Requirements
From `ruby_c_extensions.py`, preserve:
- **Extension Types**:
  - Pure C extensions
  - C++ extensions
  - FFI integration
  - Rust integration via FFI
- **Code Generation**:
  - Extconf.rb generation
  - Type conversion helpers
  - Memory management
  - Exception handling
  - Ruby API usage
- **Build Configuration**:
  - Platform detection
  - Compiler flags
  - Library linking
  - Cross-compilation support

### 5. Ractor Requirements
From `ruby_ractors.py`, preserve:
- **Concurrency Patterns**:
  - Actor model implementation
  - Message passing protocols
  - Shared state handling
  - Pipeline processing
  - MapReduce patterns
- **Ractor Features**:
  - Ractor creation and management
  - Message sending (blocking/non-blocking)
  - Move semantics
  - Shareable objects
  - Ractor-local storage
- **Error Handling**:
  - Isolation violations
  - Deadlock detection
  - Timeout handling
  - Supervision trees

### 6. Kata Generator Requirements
From `ruby_katas.py`, preserve:
- **Kata Categories**:
  - Algorithm challenges (sorting, searching)
  - Data structure implementation
  - String manipulation
  - Mathematical problems
  - Design patterns
  - Refactoring exercises
- **Difficulty Levels**:
  - Beginner (syntax, basic operations)
  - Intermediate (algorithms, OOP)
  - Advanced (metaprogramming, optimization)
  - Expert (complex algorithms, system design)
- **Test Generation**:
  - RSpec test suites
  - Minitest alternatives
  - Edge case coverage
  - Performance benchmarks
  - Example solutions

## Implementation Guidelines

### Architecture Requirements

1. **Class Structure**:
```python
class RailsUnifiedToolkit:
    def __init__(self, ollama_model: str = "deepseek-coder:6.7b"):
        self.model = ollama_model
        self.generators = {
            RailsToolType.CONTROLLER: self._generate_controller,
            # ... all other tool types
        }
    
    def generate(self, config: RailsGenerationConfig) -> Dict[str, Any]:
        # Unified entry point for all generation
        pass
```

2. **Configuration Management**:
```python
@dataclass
class RailsGenerationConfig:
    resource_name: str
    tool_type: RailsToolType
    api_mode: bool = False
    authentication: str = "none"
    test_framework: str = "rspec"
    database: str = "postgresql"
    features: List[str] = None
    # All other configuration options
```

3. **Error Handling**:
- Comprehensive validation for all inputs
- Detailed error messages with suggestions
- Graceful degradation for missing features
- Logging for debugging

4. **Output Format**:
```python
{
    "files": {
        "path/to/file": "content",
        # All generated files
    },
    "instructions": "Setup and usage instructions",
    "components": {
        # Individual components for display
    },
    "metadata": {
        "tool_type": "controller",
        "timestamp": "2025-01-03T10:00:00Z",
        "model_used": "deepseek-coder:6.7b"
    }
}
```

### Quality Requirements

1. **Code Quality**:
- Type hints for all methods
- Comprehensive docstrings
- PEP 8 compliance
- No code duplication
- Modular design

2. **Testing**:
- Unit tests for each component
- Integration tests for tool combinations
- Edge case coverage
- Performance benchmarks

3. **Documentation**:
- README for each toolkit
- API documentation
- Usage examples
- Migration guide

### Performance Requirements

1. **Efficiency**:
- Lazy loading of components
- Caching of common patterns
- Batch processing support
- Async operations where applicable

2. **Scalability**:
- Handle large codebases
- Support multiple resources
- Concurrent generation
- Memory-efficient operations

### Integration Requirements

1. **Backward Compatibility**:
- Adapter pattern for existing pages
- Same output format
- Preserved functionality
- Gradual migration path

2. **Extensibility**:
- Plugin architecture
- Custom tool additions
- Template overrides
- Hook system

---

## Final Implementation Checklist

- [ ] All 15 tools fully consolidated
- [ ] 100% feature parity maintained
- [ ] Comprehensive test coverage
- [ ] Performance optimizations applied
- [ ] Documentation completed
- [ ] Migration guide written
- [ ] Adapter layer implemented
- [ ] Cross-tool features added
- [ ] Error handling comprehensive
- [ ] Output formats consistent

This consolidation will transform TuoKit's Ruby/Rails toolkit into a powerful, maintainable, and extensible system while preserving every feature from the original 15 separate tools.