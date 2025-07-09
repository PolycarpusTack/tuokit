"""
Configuration for Rails Toolkit
"""

# Rails versions
RAILS_VERSIONS = ["7.0", "7.1", "6.1", "6.0"]

# Standard RESTful actions
RESTFUL_ACTIONS = {
    "index": {"method": "GET", "path": "/resources", "desc": "List all resources"},
    "show": {"method": "GET", "path": "/resources/:id", "desc": "Show one resource"},
    "new": {"method": "GET", "path": "/resources/new", "desc": "New resource form"},
    "create": {"method": "POST", "path": "/resources", "desc": "Create resource"},
    "edit": {"method": "GET", "path": "/resources/:id/edit", "desc": "Edit form"},
    "update": {"method": "PUT/PATCH", "path": "/resources/:id", "desc": "Update resource"},
    "destroy": {"method": "DELETE", "path": "/resources/:id", "desc": "Delete resource"}
}

# Test frameworks
TEST_FRAMEWORKS = ["RSpec", "Minitest", "None"]

# Database adapters
DATABASE_ADAPTERS = ["postgresql", "mysql2", "sqlite3"]

# Authentication libraries
AUTH_LIBRARIES = ["Devise", "Clearance", "Sorcery", "None"]

# API formats
API_FORMATS = ["JSON", "JSON:API", "GraphQL"]

# Common Rails patterns
RAILS_PATTERNS = {
    "service_object": "Encapsulate business logic",
    "form_object": "Complex form handling",
    "query_object": "Complex database queries",
    "decorator": "Presentation logic",
    "policy_object": "Authorization rules",
    "value_object": "Immutable domain concepts"
}

# Error patterns for debugging
ERROR_PATTERNS = {
    "routing": {
        "pattern": r"(No route matches|RoutingError|ActionController::UrlGenerationError)",
        "icon": "🛣️",
        "category": "Routing Error"
    },
    "database": {
        "pattern": r"(ActiveRecord::|PG::|Mysql2::|SQLite3::|migration)",
        "icon": "🗄️",
        "category": "Database Error"
    },
    "validation": {
        "pattern": r"(Validation failed|RecordInvalid|RecordNotSaved)",
        "icon": "✅",
        "category": "Validation Error"
    },
    "authentication": {
        "pattern": r"(Unauthorized|Devise|authenticate|CanCan|Pundit)",
        "icon": "🔐",
        "category": "Authentication/Authorization Error"
    },
    "view": {
        "pattern": r"(ActionView::|undefined method.*nil|NoMethodError.*nil)",
        "icon": "🎨",
        "category": "View Error"
    },
    "asset": {
        "pattern": r"(Sprockets::|Asset.*not found|Webpacker)",
        "icon": "📦",
        "category": "Asset Pipeline Error"
    },
    "job": {
        "pattern": r"(ActiveJob::|Sidekiq|DelayedJob|Resque)",
        "icon": "⚙️",
        "category": "Background Job Error"
    }
}

# Performance thresholds
PERFORMANCE_THRESHOLDS = {
    "slow_query_ms": 100,
    "n_plus_one_threshold": 10,
    "memory_leak_mb": 100,
    "request_timeout_s": 30
}

# Rails configuration
RAILS_CONFIG = {
    "versions": RAILS_VERSIONS,
    "actions": RESTFUL_ACTIONS,
    "test_frameworks": TEST_FRAMEWORKS,
    "databases": DATABASE_ADAPTERS,
    "auth_libraries": AUTH_LIBRARIES,
    "api_formats": API_FORMATS,
    "patterns": RAILS_PATTERNS,
    "error_patterns": ERROR_PATTERNS,
    "performance": PERFORMANCE_THRESHOLDS
}