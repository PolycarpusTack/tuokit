"""
TuoKit Navigation Registry
Central registry for all tools with categories and metadata
Enhanced with modern theme colors and emojis
"""

NAVIGATION_CATEGORIES = {
    "🤖 AI & Agents": {
        "description": "AI-powered assistants and automation",
        "icon": "🤖",
        "color": "#9c27b0",
        "tools": {
            "agent_hub": {
                "name": "Agent Hub",
                "description": "Unified AI agent system with goal orchestration",
                "icon": "🤖",
                "file": "agent_hub.py"
            }
        }
    },
    
    "💻 Code Tools": {
        "description": "Code generation, analysis, and debugging",
        "icon": "💻",
        "color": "#4caf50",
        "tools": {
            "code_tools": {
                "name": "Code Explainer",
                "description": "Explain and understand code in any language",
                "icon": "💡",
                "file": "code_tools.py"
            },
            "crash_analyzer": {
                "name": "Crash Analyzer",
                "description": "Analyze crash logs and stack traces",
                "icon": "🔍",
                "file": "crash_analyzer.py"
            },
            "crash_analyzer_v2": {
                "name": "Crash Analyzer V2",
                "description": "Enhanced crash analysis with multiple analysis methods",
                "icon": "🚨",
                "file": "crash_analyzer_v2.py"
            },
            "error_tool": {
                "name": "Error Decoder",
                "description": "Decode and fix error messages",
                "icon": "🐛",
                "file": "error_tool.py"
            },
            "exception_advisor": {
                "name": "Exception Advisor",
                "description": "Smart exception handling recommendations",
                "icon": "⚠️",
                "file": "exception_advisor.py"
            },
            "regex_tool": {
                "name": "Regex Builder",
                "description": "Build and test regular expressions",
                "icon": "🔤",
                "file": "regex_tool.py"
            }
        }
    },
    
    "🗄️ SQL & Database": {
        "description": "Database tools and SQL utilities",
        "icon": "🗄️",
        "color": "#1976d2",
        "tools": {
            "sql_toolkit": {
                "name": "SQL Toolkit Pro",
                "description": "11 SQL tools: Generate, Format, Optimize, Convert to Code (Python/Java/Smalltalk), Documentation, Sample Data & more",
                "icon": "🛢️",
                "file": "sql_toolkit_modern.py"
            },
            # Temporarily disabled - references missing sql_suite.py
            # "sql_academy": {
            #     "name": "SQL Academy",
            #     "description": "Interactive SQL learning platform",
            #     "icon": "🎓",
            #     "file": "sql_academy.py"
            # },
            # Temporarily disabled - requires migration files
            # "migration_dashboard": {
            #     "name": "Migration Dashboard",
            #     "description": "Database migration planning and execution",
            #     "icon": "🚀",
            #     "file": "migration_dashboard.py"
            # }
        }
    },
    
    "💎 Ruby & Rails": {
        "description": "Ruby development and Rails framework tools",
        "icon": "💎",
        "color": "#d32f2f",
        "tools": {
            "ruby_toolkit": {
                "name": "Ruby Toolkit",
                "description": "Ruby code optimization and best practices",
                "icon": "💎",
                "file": "ruby_toolkit.py"
            },
            "rails_ultimate_toolkit": {
                "name": "Rails Ultimate Toolkit",
                "description": "Complete Rails development suite - 25+ tools for models, controllers, APIs, testing, and more",
                "icon": "🛤️",
                "file": "rails_ultimate_toolkit.py"
            },
            "rspec_generator": {
                "name": "RSpec Generator",
                "description": "Generate RSpec tests with best practices",
                "icon": "✅",
                "file": "rspec_generator.py"
            },
            "view_components": {
                "name": "View Components",
                "description": "Build reusable view components",
                "icon": "🎨",
                "file": "view_components.py"
            }
        }
    },
    
    "🚀 Ruby Advanced": {
        "description": "Advanced Ruby programming tools",
        "icon": "🚀",
        "color": "#f44336",
        "tools": {
            "ruby_c_extensions": {
                "name": "C Extensions",
                "description": "Build Ruby C extensions",
                "icon": "⚡",
                "file": "ruby_c_extensions.py"
            },
            "ruby_katas": {
                "name": "Ruby Katas",
                "description": "Practice Ruby with coding katas",
                "icon": "🥋",
                "file": "ruby_katas.py"
            },
            "ruby_memory_optimizer": {
                "name": "Memory Optimizer",
                "description": "Optimize Ruby memory usage",
                "icon": "💾",
                "file": "ruby_memory_optimizer.py"
            },
            "ruby_pattern_matching": {
                "name": "Pattern Matching",
                "description": "Master Ruby pattern matching",
                "icon": "🎯",
                "file": "ruby_pattern_matching.py"
            },
            "ruby_profiler": {
                "name": "Performance Profiler",
                "description": "Profile Ruby application performance",
                "icon": "📈",
                "file": "ruby_profiler.py"
            },
            "ruby_ractors": {
                "name": "Ractor Guide",
                "description": "Master Ruby Ractors for parallelism",
                "icon": "🔀",
                "file": "ruby_ractors.py"
            }
        }
    },
    
    # Temporarily disabled - syntax error in file
    # "🎈 Smalltalk Tools": {
    #     "description": "Comprehensive SmallTalk development suite",
    #     "icon": "🎈",
    #     "color": "#ff9800",
    #     "tools": {
    #         "smalltalk_toolkit": {
    #             "name": "SmallTalk Toolkit",
    #             "description": "All-in-one SmallTalk development suite - classes, refactoring, Seaside, Ruby conversion, and more",
    #             "icon": "🚀",
    #             "file": "smalltalk_toolkit.py"
    #         }
    #     }
    # },
    
    "📚 Knowledge & Docs": {
        "description": "Documentation and knowledge management",
        "icon": "📚",
        "color": "#7b1fa2",
        "tools": {
            "doc_tools": {
                "name": "Document Analyzer",
                "description": "Analyze and understand documents",
                "icon": "📄",
                "file": "doc_tools.py"
            },
            "knowledge_lib": {
                "name": "Knowledge Library",
                "description": "Browse AI-generated knowledge base",
                "icon": "📚",
                "file": "knowledge_lib.py"
            },
            "rag_knowledge_base": {
                "name": "RAG Knowledge Base",
                "description": "AI-powered search across codebase and documentation",
                "icon": "🔍",
                "file": "rag_knowledge_base.py"
            },
            "knowledge_explorer": {
                "name": "Knowledge Explorer",
                "description": "Chat with your knowledge base using semantic search and AI",
                "icon": "🔮",
                "file": "knowledge_explorer.py"
            },
            "snowflake_cheatsheet": {
                "name": "Snowflake Cheat Sheet",
                "description": "Comprehensive Snowflake SQL reference with examples",
                "icon": "❄️",
                "file": "snowflake_cheatsheet.py"
            },
            "sql_cheatsheet": {
                "name": "SQL Cheat Sheet",
                "description": "Standard SQL reference for developers, analysts, and support",
                "icon": "🗃️",
                "file": "sql_cheatsheet.py"
            },
            "xml_cheatsheet": {
                "name": "XML Cheat Sheet",
                "description": "XML, XPath, XSLT reference and troubleshooting guide",
                "icon": "📄",
                "file": "xml_cheatsheet.py"
            },
            "python_cheatsheet": {
                "name": "Python Cheat Sheet",
                "description": "Python syntax, patterns, and best practices for all levels",
                "icon": "🐍",
                "file": "python_cheatsheet.py"
            },
            "oracle_cheatsheet": {
                "name": "Oracle Cheat Sheet",
                "description": "Oracle SQL, PL/SQL, and DBA reference guide",
                "icon": "🔶",
                "file": "oracle_cheatsheet.py"
            },
            "postgres_cheatsheet": {
                "name": "PostgreSQL Cheat Sheet",
                "description": "PostgreSQL commands, JSONB, arrays, and optimization",
                "icon": "🐘",
                "file": "postgres_cheatsheet.py"
            },
            "bpmn_cheatsheet": {
                "name": "BPMN 2.0 Training Path",
                "description": "Complete BPMN 2.0 learning path with CMMN and DMN introductions",
                "icon": "🔄",
                "file": "bpmn_cheatsheet.py"
            },
            "help_guide": {
                "name": "Help Guide",
                "description": "TuoKit user guide and documentation",
                "icon": "❓",
                "file": "help_guide.py"
            },
            "vector_search_demo": {
                "name": "Vector Search Demo",
                "description": "Test and explore intelligent vector search capabilities",
                "icon": "🔍",
                "file": "vector_search_demo.py"
            }
        }
    },
    
    "🎓 Learning Tools": {
        "description": "Educational and learning utilities",
        "icon": "🎓",
        "color": "#00897b",
        "tools": {
            "edu_mind": {
                "name": "EduMind Assistant",
                "description": "AI-powered educational assistant",
                "icon": "🧠",
                "file": "edu_mind.py"
            },
            "study_guide_generator": {
                "name": "Study Guide Generator",
                "description": "Create personalized study guides",
                "icon": "📖",
                "file": "study_guide_generator.py"
            }
        }
    },
    
    "🔧 Utilities": {
        "description": "General development utilities",
        "icon": "🔧",
        "color": "#546e7a",
        "tools": {
            "image_browser": {
                "name": "Image Browser",
                "description": "Browse and manage images",
                "icon": "🖼️",
                "file": "image_browser.py"
            },
            "morphic_builder": {
                "name": "Morphic Builder",
                "description": "Build interactive components",
                "icon": "🏗️",
                "file": "morphic_builder.py"
            },
            "tool_finder": {
                "name": "Tool Finder",
                "description": "Find the right tool for your task",
                "icon": "🔎",
                "file": "tool_finder.py"
            }
        }
    },
    
    "🏥 System Health": {
        "description": "System monitoring and health checks",
        "icon": "🏥",
        "color": "#e91e63",
        "tools": {
            "health_monitor": {
                "name": "Health Monitor",
                "description": "Comprehensive system health monitoring - Database, Ollama, and system resources",
                "icon": "🏥",
                "file": "health_monitor.py"
            },
            "database_health_check": {
                "name": "Database Health Check",
                "description": "Check database connection and table status",
                "icon": "🗄️",
                "file": "database_health_check.py"
            },
            "ollama_health_check": {
                "name": "Ollama Health Check",
                "description": "Verify Ollama installation and model availability",
                "icon": "🤖",
                "file": "ollama_health_check.py"
            }
        }
    },
    
    "🏭 Enterprise Integrations": {
        "description": "Integration tools for enterprise systems",
        "icon": "🏭",
        "color": "#795548",
        "tools": {
            "camunda_manager": {
                "name": "Camunda Manager",
                "description": "AI-powered Camunda BPM management - monitor processes, generate BPMN, analyze workflows",
                "icon": "🏭",
                "file": "camunda_manager.py"
            },
            "camunda_practical": {
                "name": "Camunda Practical",
                "description": "Real Camunda tools that actually work - deploy, monitor, and manage BPMN processes",
                "icon": "🔧",
                "file": "camunda_practical.py"
            }
        }
    },
    
    # Advanced tools that require specific database setup
    # "📊 Advanced Analytics": {
    #     "description": "Advanced visualization and analytics (requires database setup)",
    #     "icon": "📊",
    #     "color": "#ff5722",
    #     "tools": {
    #         "crash_graph_viewer": {
    #             "name": "Crash Graph Viewer",
    #             "description": "Visualize crash patterns and relationships (requires crash analysis database)",
    #             "icon": "📊",
    #             "file": "crash_graph_viewer.py"
    #         },
    #         "knowledge_graph_viewer": {
    #             "name": "Knowledge Graph Viewer",
    #             "description": "Explore knowledge relationships (requires knowledge graph database)",
    #             "icon": "🌐",
    #             "file": "knowledge_graph_viewer.py"
    #         }
    #     }
    # }
}

def get_tool_count():
    """Get total number of tools"""
    count = 0
    for category in NAVIGATION_CATEGORIES.values():
        count += len(category["tools"])
    return count

def get_category_count():
    """Get number of categories"""
    return len(NAVIGATION_CATEGORIES)

def search_tools(query):
    """Search tools by name or description"""
    query = query.lower()
    results = []
    
    for category_name, category_data in NAVIGATION_CATEGORIES.items():
        for tool_id, tool in category_data["tools"].items():
            # Search in name and description
            if (query in tool["name"].lower() or 
                query in tool["description"].lower() or
                query in tool_id.lower()):
                results.append({
                    "id": tool_id,
                    "name": tool["name"],
                    "description": tool["description"],
                    "icon": tool["icon"],
                    "file": tool["file"],
                    "category": category_name
                })
    
    return results

def get_tool_by_id(tool_id):
    """Get tool info by ID"""
    for category_data in NAVIGATION_CATEGORIES.values():
        if tool_id in category_data["tools"]:
            return category_data["tools"][tool_id]
    return None

def get_category_for_tool(tool_id):
    """Get category name for a tool"""
    for category_name, category_data in NAVIGATION_CATEGORIES.items():
        if tool_id in category_data["tools"]:
            return category_name
    return None
