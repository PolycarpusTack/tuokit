# toolkits/error_decoder/knowledge_base.py
"""Predefined knowledge base for common errors"""

EDUCATIONAL_CONTENT = {
    # SmallTalk Errors
    "MessageNotUnderstood": {
        "title": "Message Not Understood Error",
        "categories": ["SmallTalk", "Object-Oriented"],
        "explanation": "Occurs when an object receives a message it doesn't implement",
        "analogy": "Like asking a fish to climb a tree - the receiver lacks the capability",
        "causes": [
            "Method not implemented in receiver's class",
            "Typo in method name",
            "Incorrect receiver type"
        ],
        "fixes": [
            "Implement the missing method",
            "Check spelling of message",
            "Verify receiver class hierarchy"
        ],
        "best_practices": [
            "Use #respondsTo: before sending messages",
            "Implement #doesNotUnderstand: for custom handling",
            "Use protocols to organize methods"
        ],
        "case_study": "In the SmallTalk image, all objects inherit from Object which implements #doesNotUnderstand:. This default implementation signals a MessageNotUnderstood exception.",
        "resources": [
            {"title": "SmallTalk Message Handling", "url": "https://wiki.squeak.org/squeak/194"},
            {"title": "Object Protocol Design", "url": "https://wiki.squeak.org/squeak/195"}
        ]
    },
    "SubscriptOutOfBounds": {
        "title": "Subscript Out of Bounds",
        "categories": ["SmallTalk", "Collections"],
        "explanation": "Occurs when accessing an index beyond collection size",
        "analogy": "Like trying to read page 100 in a 50-page book",
        "causes": [
            "Off-by-one errors",
            "Invalid index calculation",
            "Empty collection access"
        ],
        "fixes": [
            "Check collection size before access",
            "Use #ifEmpty: block",
            "Validate index ranges"
        ],
        "best_practices": [
            "Use #at:ifAbsent: instead of #at:",
            "Prefer iterators over index access",
            "Use #first, #last instead of indexes"
        ],
        "case_study": "The SmallTalk collection hierarchy implements #errorSubscriptBounds: to handle this error consistently across all collection types.",
        "resources": [
            {"title": "Collection Protocols", "url": "https://wiki.squeak.org/squeak/220"},
            {"title": "Error Handling Patterns", "url": "https://wiki.squeak.org/squeak/223"}
        ]
    },
    
    # Ruby/Rails Errors
    "NoMethodError": {
        "title": "No Method Error",
        "categories": ["Ruby", "Rails"],
        "explanation": "Occurs when calling undefined method on object",
        "analogy": "Like trying to drive a car with a steering wheel that doesn't exist",
        "causes": [
            "Method name typo",
            "Missing require/include",
            "Wrong object type"
        ],
        "fixes": [
            "Check method spelling",
            "Verify required files are loaded",
            "Use respond_to? before calling"
        ],
        "best_practices": [
            "Use method_missing for dynamic handling",
            "Implement null object pattern",
            "Use safe navigation operator (&.)"
        ],
        "case_study": "In Rails, this often occurs in views when referencing undefined helper methods. The solution is to define the method in the appropriate helper module.",
        "resources": [
            {"title": "Ruby Method Lookup", "url": "https://ruby-doc.org/core-3.1.2/doc/method_lookup_rdoc.html"},
            {"title": "Rails Helper Patterns", "url": "https://guides.rubyonrails.org/action_view_overview.html"}
        ]
    },
    "ActiveRecord::RecordNotFound": {
        "title": "Record Not Found",
        "categories": ["Rails", "Database"],
        "explanation": "Occurs when database query returns no results",
        "analogy": "Like looking for a book in an empty library",
        "causes": [
            "Invalid ID parameter",
            "Deleted record",
            "Scoped query with no matches"
        ],
        "fixes": [
            "Validate params before query",
            "Use find_by instead of find",
            "Implement rescue_from handler"
        ],
        "best_practices": [
            "Use find_or_initialize_by for safe access",
            "Implement null object pattern",
            "Use policy objects for authorization"
        ],
        "case_study": "Rails controllers rescue this error by default, returning a 404 response. This can be customized in ApplicationController.",
        "resources": [
            {"title": "ActiveRecord Querying", "url": "https://guides.rubyonrails.org/active_record_querying.html"},
            {"title": "Error Handling in Controllers", "url": "https://guides.rubyonrails.org/action_controller_overview.html#rescue-from"}
        ]
    }
}

# Language-specific guidance
LANGUAGE_GUIDANCE = {
    "ruby": "Consider Ruby idioms and Rails conventions",
    "rails": "Focus on MVC structure, database interactions, and Rails conventions",
    "smalltalk": "Consider Smalltalk's object-oriented nature and image-based environment",
    "python": "Focus on Pythonic patterns and exception hierarchy",
    "javascript": "Consider async patterns and prototype chain",
    "java": "Focus on strong typing and exception hierarchy",
    "c++": "Consider memory management and template errors"
}

# Learning resources by language
LEARNING_RESOURCES = {
    "SmallTalk": """
    **SmallTalk Resources:**
    - [Message Handling](https://wiki.squeak.org/squeak/194)
    - [Exception Hierarchy](https://wiki.squeak.org/squeak/223)
    - [doesNotUnderstand:](https://wiki.squeak.org/squeak/195)
    """,
    "Ruby": """
    **Ruby/Rails Resources:**
    - [Exception Handling](https://ruby-doc.org/core-3.1.2/Exception.html)
    - [Rails Error Handling](https://guides.rubyonrails.org/action_controller_overview.html#rescue-from)
    - [Metaprogramming](https://ruby-doc.org/core-3.1.2/doc/metaprogramming_rdoc.html)
    """,
    "Rails": """
    **Ruby/Rails Resources:**
    - [Exception Handling](https://ruby-doc.org/core-3.1.2/Exception.html)
    - [Rails Error Handling](https://guides.rubyonrails.org/action_controller_overview.html#rescue-from)
    - [Metaprogramming](https://ruby-doc.org/core-3.1.2/doc/metaprogramming_rdoc.html)
    """,
    "default": """
    **General Resources:**
    - Language documentation
    - Stack Overflow
    - Official tutorials
    """
}