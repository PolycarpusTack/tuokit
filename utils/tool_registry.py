"""
Complete Tool Registry for TuoKit
All tools from the pages directory properly categorized
"""

COMPLETE_TOOL_REGISTRY = {
    "🧠 Code Intelligence": {
        "description": "AI-powered code analysis, generation, and debugging",
        "icon": "🧠",
        "tools": [
            {"id": "code_assistant", "name": "Code Assistant", "icon": "💡", "desc": "Explain, debug, and optimize code", "page": "pages/code_tools.py", "tab": "assistant"},
            {"id": "code_debugger", "name": "Code Debugger", "icon": "🐛", "desc": "Fix errors with AI assistance", "page": "pages/code_tools.py", "tab": "debugger"},
            {"id": "code_generator", "name": "Code Generator", "icon": "🚀", "desc": "Generate code from descriptions", "page": "pages/code_tools.py", "tab": "generator"},
            {"id": "error_decoder", "name": "Error Decoder", "icon": "🔍", "desc": "Understand complex error messages", "page": "pages/error_tool.py"},
            {"id": "exception_advisor", "name": "Exception Advisor", "icon": "🛡️", "desc": "Exception handling strategies", "page": "pages/exception_advisor.py"},
            {"id": "regex_generator", "name": "Regex Generator", "icon": "🎯", "desc": "Create and test regex patterns", "page": "pages/regex_tool.py"},
            {"id": "crash_analyzer", "name": "Crash Analyzer", "icon": "💥", "desc": "Analyze crash dumps and stack traces", "page": "pages/crash_analyzer.py"}
        ]
    },
    
    "🗄️ Data & SQL": {
        "description": "Database query generation, optimization, and learning",
        "icon": "🗄️",
        "tools": [
            {"id": "sql_generator", "name": "SQL Generator", "icon": "✨", "desc": "Natural language to SQL", "page": "pages/sql_generator.py"},
            {"id": "sql_optimizer", "name": "SQL Optimizer", "icon": "⚡", "desc": "Optimize query performance", "page": "pages/sql_optimizer.py"},
            {"id": "sql_pipeline", "name": "SQL Pipeline", "icon": "🔄", "desc": "End-to-end SQL workflow", "page": "pages/sql_pipeline.py"},
            {"id": "sql_suite", "name": "SQL Suite", "icon": "🎯", "desc": "Complete SQL toolkit", "page": "pages/sql_suite.py"},
            {"id": "sql_academy", "name": "SQL Academy", "icon": "🎓", "desc": "Learn SQL interactively", "page": "pages/sql_academy.py"}
        ]
    },
    
    "💎 Ruby & Rails": {
        "description": "Ruby and Rails development tools",
        "icon": "💎",
        "tools": [
            {"id": "rails_controller", "name": "Rails Controller Gen", "icon": "🎮", "desc": "Generate Rails controllers", "page": "pages/rails_controller_gen.py"},
            {"id": "rails_model", "name": "Rails Model Gen", "icon": "📊", "desc": "Create Rails models", "page": "pages/rails_model_gen.py"},
            {"id": "rails_scaffold", "name": "Rails Scaffold", "icon": "🏗️", "desc": "Generate complete resources", "page": "pages/rails_scaffold.py"},
            {"id": "rails_debugger", "name": "Rails Debugger", "icon": "🐞", "desc": "Debug Rails applications", "page": "pages/rails_debugger.py"},
            {"id": "rails_graphql", "name": "Rails GraphQL", "icon": "📡", "desc": "GraphQL for Rails", "page": "pages/rails_graphql.py"},
            {"id": "rails_tests", "name": "Rails System Tests", "icon": "🧪", "desc": "Generate system tests", "page": "pages/rails_system_tests.py"},
            {"id": "rails_upgrader", "name": "Rails Upgrader", "icon": "⬆️", "desc": "Upgrade Rails versions", "page": "pages/rails_upgrader.py"},
            {"id": "rspec_generator", "name": "RSpec Generator", "icon": "✅", "desc": "Create RSpec tests", "page": "pages/rspec_generator.py"},
            {"id": "ruby_profiler", "name": "Ruby Profiler", "icon": "📈", "desc": "Profile Ruby performance", "page": "pages/ruby_profiler.py"},
            {"id": "ruby_memory", "name": "Memory Optimizer", "icon": "💾", "desc": "Optimize Ruby memory", "page": "pages/ruby_memory_optimizer.py"},
            {"id": "ruby_patterns", "name": "Pattern Matching", "icon": "🎨", "desc": "Ruby pattern matching", "page": "pages/ruby_pattern_matching.py"},
            {"id": "ruby_ractors", "name": "Ruby Ractors", "icon": "🔀", "desc": "Parallel execution with Ractors", "page": "pages/ruby_ractors.py"},
            {"id": "ruby_c_ext", "name": "Ruby C Extensions", "icon": "⚙️", "desc": "Build C extensions", "page": "pages/ruby_c_extensions.py"},
            {"id": "ruby_katas", "name": "Ruby Katas", "icon": "🥋", "desc": "Practice Ruby katas", "page": "pages/ruby_katas.py"}
        ]
    },
    
    "🦆 SmallTalk Tools": {
        "description": "SmallTalk development and conversion tools",
        "icon": "🦆",
        "tools": [
            {"id": "smalltalk_class", "name": "SmallTalk Class Gen", "icon": "📦", "desc": "Generate SmallTalk classes", "page": "pages/smalltalk_class_gen.py"},
            {"id": "smalltalk_explain", "name": "SmallTalk Explainer", "icon": "💡", "desc": "Explain SmallTalk code", "page": "pages/smalltalk_explainer.py"},
            {"id": "smalltalk_meta", "name": "SmallTalk Meta", "icon": "🔮", "desc": "Metaprogramming tools", "page": "pages/smalltalk_meta.py"},
            {"id": "smalltalk_refactor", "name": "SmallTalk Refactorer", "icon": "🔧", "desc": "Refactor SmallTalk code", "page": "pages/smalltalk_refactorer.py"},
            {"id": "smalltalk_convert", "name": "SmallTalk↔Ruby", "icon": "🔄", "desc": "Convert between languages", "page": "pages/smalltalk_ruby_converter.py"},
            {"id": "smalltalk_snippets", "name": "SmallTalk Snippets", "icon": "📝", "desc": "Code snippet library", "page": "pages/smalltalk_snippets.py"},
            {"id": "seaside_gen", "name": "Seaside Generator", "icon": "🌊", "desc": "Generate Seaside components", "page": "pages/seaside_generator.py"}
        ]
    },
    
    "🤖 Agent Systems": {
        "description": "AI agent orchestration and automation",
        "icon": "🤖",
        "tools": [
            {"id": "agent_hub", "name": "Agent Hub", "icon": "🎯", "desc": "Central agent management", "page": "pages/agent_hub.py"},
            {"id": "agent_lite", "name": "Agent Lite", "icon": "🪶", "desc": "Lightweight agent system", "page": "pages/agent_lite.py"},
            {"id": "agent_portal", "name": "Agent Portal", "icon": "🌐", "desc": "Agent deployment portal", "page": "pages/agent_portal.py"},
            {"id": "agent_unified", "name": "Unified Agents", "icon": "🔗", "desc": "Unified agent framework", "page": "pages/agent_unified.py"}
        ]
    },
    
    "🎨 UI & Components": {
        "description": "User interface and component tools",
        "icon": "🎨",
        "tools": [
            {"id": "view_components", "name": "View Components", "icon": "🧩", "desc": "Rails ViewComponent builder", "page": "pages/view_components.py"},
            {"id": "morphic_builder", "name": "Morphic Builder", "icon": "🏗️", "desc": "Build Morphic UIs", "page": "pages/morphic_builder.py"},
            {"id": "image_browser", "name": "Image Browser", "icon": "🖼️", "desc": "Browse and manage images", "page": "pages/image_browser.py"}
        ]
    },
    
    "📚 Learning & Documents": {
        "description": "Document processing and educational tools",
        "icon": "📚",
        "tools": [
            {"id": "document_qa", "name": "Document Q&A", "icon": "💬", "desc": "Chat with your documents", "page": "pages/doc_tools.py", "tab": "qa"},
            {"id": "summarizer", "name": "Summarizer", "icon": "📝", "desc": "Get key points from texts", "page": "pages/doc_tools.py", "tab": "summarize"},
            {"id": "knowledge_extractor", "name": "Knowledge Extractor", "icon": "🔎", "desc": "Extract structured data", "page": "pages/doc_tools.py", "tab": "extract"},
            {"id": "study_guide", "name": "Study Guide Generator", "icon": "📖", "desc": "Create comprehensive study materials", "page": "pages/study_guide_generator.py"},
            {"id": "edumind", "name": "EduMind", "icon": "🧩", "desc": "Interactive learning assistant", "page": "pages/edu_mind.py"}
        ]
    },
    
    "💾 Knowledge & Help": {
        "description": "Knowledge management and support tools",
        "icon": "💾",
        "tools": [
            {"id": "knowledge_library", "name": "Knowledge Library", "icon": "📚", "desc": "Browse and manage saved insights", "page": "pages/knowledge_lib.py"},
            {"id": "help_guide", "name": "Help Guide", "icon": "📋", "desc": "Documentation and tutorials", "page": "pages/help_guide.py"},
            {"id": "onboarding", "name": "Onboarding Wizard", "icon": "🎯", "desc": "Interactive getting started guide", "page": "pages/onboarding_wizard.py"}
        ]
    }
}

# Tool count summary
def get_tool_count():
    total = 0
    for category in COMPLETE_TOOL_REGISTRY.values():
        total += len(category['tools'])
    return total

# Get tool by ID
def get_tool_by_id(tool_id):
    for category in COMPLETE_TOOL_REGISTRY.values():
        for tool in category['tools']:
            if tool['id'] == tool_id:
                return tool
    return None

# Get tools by category
def get_tools_by_category(category_name):
    return COMPLETE_TOOL_REGISTRY.get(category_name, {}).get('tools', [])

# Search tools
def search_tools(query):
    results = []
    query_lower = query.lower()
    
    for category_name, category_info in COMPLETE_TOOL_REGISTRY.items():
        for tool in category_info['tools']:
            if (query_lower in tool['name'].lower() or 
                query_lower in tool['desc'].lower() or
                query_lower in tool['id']):
                results.append({
                    **tool,
                    'category': category_name
                })
    
    return results
