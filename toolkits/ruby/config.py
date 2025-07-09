"""
Configuration for Ruby Toolkit
"""

# Ruby versions
RUBY_VERSIONS = ["3.3", "3.2", "3.1", "3.0", "2.7"]

# Memory optimization patterns
MEMORY_ANTIPATTERNS = {
    "String Duplication": {
        "pattern": r"(\+=|<<)\s*['\"]",
        "description": "String concatenation in loops creates new objects",
        "fix": "Use String#<< or join array"
    },
    "Unbounded Growth": {
        "pattern": r"(@@|\$)[a-z]+\s*(\+|\-|\*|\/)\s*=",
        "description": "Global/class variables growing without bounds",
        "fix": "Implement size limits or use weak references"
    },
    "N+1 Caching": {
        "pattern": r"Rails\.cache\.fetch[^{]*\{[^}]*\.each",
        "description": "Caching inside loops causes memory bloat",
        "fix": "Cache outside the loop or use bulk operations"
    },
    "Leaky Constants": {
        "pattern": r"([A-Z][A-Z0-9_]+)\s*=\s*\[|\{",
        "description": "Large constants hold memory permanently",
        "fix": "Use methods with memoization instead"
    },
    "Large Object Creation": {
        "pattern": r"\.times\s*\{.*new|\.map\s*\{.*new",
        "description": "Creating many objects in tight loops",
        "fix": "Use object pooling or reuse objects"
    },
    "Memory Bloat": {
        "pattern": r"@\w+\s*<<.*\*\s*\d{3,}",
        "description": "Appending large data to instance variables",
        "fix": "Use streaming or pagination"
    }
}

# Performance optimization patterns
PERFORMANCE_PATTERNS = {
    "slow_loops": {
        "pattern": r"\.each.*\.each|\.map.*\.map",
        "description": "Nested iterations",
        "optimization": "Use flat_map or single pass algorithms"
    },
    "string_interpolation": {
        "pattern": r"['\"].*#\{.*\}.*['\"].*\+",
        "description": "String interpolation with concatenation",
        "optimization": "Use single interpolation or join"
    },
    "repeated_calculations": {
        "pattern": r"(Math\.|\.length|\.size).*\1",
        "description": "Repeated expensive calculations",
        "optimization": "Cache results in variables"
    }
}

# Ruby idioms and best practices
RUBY_IDIOMS = {
    "nil_check": {
        "bad": "if x != nil",
        "good": "if x",
        "description": "Use truthiness"
    },
    "empty_check": {
        "bad": "array.length == 0",
        "good": "array.empty?",
        "description": "Use predicate methods"
    },
    "map_compact": {
        "bad": "array.map{}.compact",
        "good": "array.filter_map{}",
        "description": "Use filter_map (Ruby 2.7+)"
    },
    "hash_fetch": {
        "bad": "hash[:key] || default",
        "good": "hash.fetch(:key, default)",
        "description": "Use fetch for defaults"
    }
}

# C Extension templates
C_EXTENSION_TEMPLATES = {
    "Numeric Computation": {
        "description": "Fast mathematical operations",
        "includes": ["ruby.h", "math.h"],
        "example": "Matrix multiplication, Vector operations"
    },
    "String Processing": {
        "description": "High-speed string manipulation",
        "includes": ["ruby.h", "string.h"],
        "example": "Pattern matching, Encoding conversion"
    },
    "Data Structure": {
        "description": "Efficient custom data structure",
        "includes": ["ruby.h", "stdlib.h"],
        "example": "Priority queue, Trie, B-tree"
    },
    "System Call": {
        "description": "Direct system call wrapper",
        "includes": ["ruby.h", "unistd.h", "sys/types.h"],
        "example": "File operations, Process management"
    }
}

# Kata categories and difficulty
KATA_CATEGORIES = {
    "Algorithms": ["Sorting", "Searching", "Graph traversal", "Dynamic programming"],
    "Data Structures": ["Arrays", "Hashes", "Trees", "Linked lists"],
    "String Manipulation": ["Parsing", "Pattern matching", "Encoding"],
    "Array Operations": ["Transformation", "Filtering", "Aggregation"],
    "Functional Programming": ["Map/Reduce", "Currying", "Composition"],
    "OOP Design": ["SOLID principles", "Design patterns", "Refactoring"],
    "Metaprogramming": ["Dynamic methods", "DSLs", "Class macros"],
    "Performance": ["Optimization", "Caching", "Lazy evaluation"]
}

DIFFICULTY_LEVELS = {
    "Beginner": {
        "time_estimate": "5-15 minutes",
        "concepts": ["Basic syntax", "Simple loops", "Conditionals"]
    },
    "Intermediate": {
        "time_estimate": "15-30 minutes",
        "concepts": ["Complex data structures", "OOP", "Modules"]
    },
    "Advanced": {
        "time_estimate": "30-60 minutes",
        "concepts": ["Metaprogramming", "Performance", "Design patterns"]
    },
    "Expert": {
        "time_estimate": "60+ minutes",
        "concepts": ["Complex algorithms", "System design", "Optimization"]
    }
}

# GC tuning configurations
GC_CONFIGS = {
    "web_app": {
        "RUBY_GC_HEAP_GROWTH_FACTOR": "1.8",
        "RUBY_GC_HEAP_INIT_SLOTS": "600000",
        "RUBY_GC_HEAP_FREE_SLOTS": "600000",
        "description": "Optimized for web applications"
    },
    "batch_processing": {
        "RUBY_GC_HEAP_GROWTH_FACTOR": "1.5",
        "RUBY_GC_HEAP_INIT_SLOTS": "1000000",
        "RUBY_GC_MALLOC_LIMIT": "90000000",
        "description": "Optimized for batch processing"
    },
    "low_memory": {
        "RUBY_GC_HEAP_GROWTH_FACTOR": "1.2",
        "RUBY_GC_HEAP_FREE_SLOTS_MIN_RATIO": "0.40",
        "RUBY_GC_HEAP_FREE_SLOTS_MAX_RATIO": "0.65",
        "description": "Optimized for low memory environments"
    }
}

# Configuration dictionary
RUBY_CONFIG = {
    "ruby_versions": RUBY_VERSIONS,
    "memory_antipatterns": MEMORY_ANTIPATTERNS,
    "performance_patterns": PERFORMANCE_PATTERNS,
    "idioms": RUBY_IDIOMS,
    "c_extension_templates": C_EXTENSION_TEMPLATES,
    "kata_categories": KATA_CATEGORIES,
    "difficulty_levels": DIFFICULTY_LEVELS,
    "gc_configs": GC_CONFIGS
}