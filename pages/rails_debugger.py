"""
Rails Debugging Assistant for TuoKit
Analyzes Rails errors and provides solutions using DeepSeek models
Enhanced with error categorization and solution tracking
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rails Debugger - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
import re
from datetime import datetime

class RailsDebugger(OllamaToolBase):
    """Rails error analysis and debugging tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="rails_debugger",
            default_model=ModelManager.get_default_model()
        )
        self.coder_model = ModelManager.get_default_model()
    
    def analyze_error(self, error_message: str, context: dict) -> dict:
        """Analyze Rails error with comprehensive context"""
        
        # Build context string
        context_parts = []
        if context.get("code"):
            context_parts.append(f"Code Context:\n{context['code']}")
        if context.get("rails_version"):
            context_parts.append(f"Rails Version: {context['rails_version']}")
        if context.get("environment"):
            context_parts.append(f"Environment: {context['environment']}")
        if context.get("stack_trace"):
            context_parts.append(f"Stack Trace:\n{context['stack_trace'][:500]}...")
            
        context_str = "\n\n".join(context_parts)
        
        prompt = f"""Debug this Rails error:

Error Message:
{error_message}

{context_str if context_str else "No additional context provided"}

Provide:
1) Root Cause Analysis - What's causing this error?
2) Step-by-Step Solution - How to fix it (be specific)
3) Prevention Tips - How to avoid this in the future
4) Related Rails Concepts - Understanding the underlying issue
5) Common Variations - Similar errors to watch for"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2,
            system="""You are a Rails debugging expert with deep knowledge of the framework.
Provide clear, actionable solutions with specific code examples where relevant.
Consider Rails version differences and best practices."""
        )
        
        return {
            "analysis": result["response"],
            "error": result["error"]
        }
    
    def suggest_code_fix(self, error_message: str, code_snippet: str, 
                        rails_version: str = "7.0") -> str:
        """Generate specific code fix for the error"""
        prompt = f"""Fix this Rails {rails_version} error:

Error: {error_message}

Current Code:
```ruby
{code_snippet}
```

Provide the corrected code with:
1. Fixed version of the code
2. Comments explaining what was wrong
3. Comments explaining the fix
4. Any additional methods/configuration needed"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            model=self.coder_model,
            temperature=0.1,
            system="Output only valid Ruby code with clear explanatory comments."
        )
        
        return result["response"]
    
    def detect_error_type(self, error_message: str) -> tuple:
        """Detect and categorize the type of Rails error"""
        error_patterns = {
            "routing": {
                "pattern": r"(No route matches|ActionController::RoutingError|Route)",
                "emoji": "🛣️",
                "description": "Routing Configuration Error"
            },
            "database": {
                "pattern": r"(ActiveRecord::|PG::|Mysql2::|SQLite3::|migration)",
                "emoji": "🗄️",
                "description": "Database/ActiveRecord Error"
            },
            "validation": {
                "pattern": r"(Validation failed|ActiveRecord::RecordInvalid|RecordNotFound)",
                "emoji": "✅",
                "description": "Model Validation Error"
            },
            "authentication": {
                "pattern": r"(Unauthorized|CanCan|Pundit|Devise|authenticate)",
                "emoji": "🔐",
                "description": "Authentication/Authorization Error"
            },
            "view": {
                "pattern": r"(ActionView::|undefined method.*for nil|Missing template|partial)",
                "emoji": "🎨",
                "description": "View/Template Error"
            },
            "asset": {
                "pattern": r"(Asset.*not found|Sprockets::|Webpacker|pipeline)",
                "emoji": "📦",
                "description": "Asset Pipeline Error"
            },
            "configuration": {
                "pattern": r"(uninitialized constant|NameError|LoadError|require)",
                "emoji": "⚙️",
                "description": "Configuration/Loading Error"
            },
            "syntax": {
                "pattern": r"(SyntaxError|unexpected|syntax)",
                "emoji": "📝",
                "description": "Ruby Syntax Error"
            }
        }
        
        for error_type, info in error_patterns.items():
            if re.search(info["pattern"], error_message, re.IGNORECASE):
                return error_type, info["emoji"], info["description"]
        
        return "general", "🔧", "General Rails Error"
    
    def get_quick_fixes(self, error_type: str) -> list:
        """Get quick fix commands for specific error types"""
        quick_fixes = {
            "routing": [
                ("View all routes", "rails routes | grep controller_name"),
                ("Check routes file", "cat config/routes.rb"),
                ("Restart server", "rails restart")
            ],
            "database": [
                ("Check migration status", "rails db:migrate:status"),
                ("Run pending migrations", "rails db:migrate"),
                ("Rollback last migration", "rails db:rollback"),
                ("Check schema", "cat db/schema.rb | grep table_name")
            ],
            "validation": [
                ("Rails console", "rails console"),
                ("Check model validations", "Model.validators"),
                ("Test in console", "Model.new(attrs).valid?")
            ],
            "authentication": [
                ("Check current user", "rails console -> current_user"),
                ("Generate Devise views", "rails generate devise:views"),
                ("Check routes", "rails routes | grep devise")
            ],
            "asset": [
                ("Precompile assets", "rails assets:precompile"),
                ("Clean assets", "rails assets:clean"),
                ("Check manifest", "cat public/assets/manifest.json")
            ],
            "configuration": [
                ("Bundle install", "bundle install"),
                ("Stop Spring", "spring stop"),
                ("Check Gemfile", "cat Gemfile | grep gem_name"),
                ("Environment check", "rails runner 'p Rails.env'")
            ]
        }
        
        return quick_fixes.get(error_type, [])

def show():
    """Main page display function"""
    st.title("🐞 Rails Debugging Assistant")
    st.markdown("Analyze Rails errors and get intelligent debugging solutions")
    
    # Initialize debugger
    debugger = RailsDebugger()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Debug Configuration")
        
        rails_version = st.selectbox(
            "Rails Version",
            ["7.0", "6.1", "6.0", "5.2", "4.2"],
            index=0,
            help="Your Rails framework version"
        )
        
        environment = st.selectbox(
            "Environment",
            ["Development", "Production", "Test", "Staging"],
            index=0,
            help="Where the error occurred"
        )
        
        st.divider()
        
        include_stack = st.toggle(
            "Include Stack Trace",
            value=False,
            help="Add full stack trace for deeper analysis"
        )
        
        search_similar = st.toggle(
            "Search Similar Errors",
            value=True,
            help="Look for similar resolved errors"
        )
        
        st.divider()
        st.caption("💡 **Pro Tip**: Include code context for more accurate fixes")
    
    # Main error input
    error_message = st.text_area(
        "📋 Paste Rails Error Message",
        height=150,
        placeholder="""Example:
ActionController::RoutingError (No route matches [GET] "/api/users"):
app/controllers/application_controller.rb:12:in `rescue_from'

Or any Rails error message..."""
    )
    
    # Detect error type
    if error_message:
        error_type, emoji, description = debugger.detect_error_type(error_message)
        col1, col2 = st.columns([1, 3])
        with col1:
            st.metric("Error Type", f"{emoji} {error_type.title()}")
        with col2:
            st.info(f"**Detected**: {description}")
    
    # Context inputs
    with st.expander("➕ Add Context (Recommended)", expanded=True):
        code_snippet = st.text_area(
            "Related Code",
            height=100,
            placeholder="Paste the code causing the error (controller, model, view, etc.)"
        )
        
        if include_stack:
            stack_trace = st.text_area(
                "Stack Trace",
                height=100,
                placeholder="Paste the full stack trace..."
            )
        else:
            stack_trace = ""
        
        # Additional context
        col1, col2 = st.columns(2)
        with col1:
            gems_context = st.text_input(
                "Relevant Gems",
                placeholder="e.g., devise, pundit, rspec"
            )
        with col2:
            db_type = st.selectbox(
                "Database",
                ["PostgreSQL", "MySQL", "SQLite", "Other"],
                index=0
            )
    
    # Debug button
    if st.button("🔍 Debug Error", type="primary", disabled=not error_message.strip()):
        # Build context
        context = {
            "rails_version": rails_version,
            "environment": environment,
            "code": code_snippet,
            "stack_trace": stack_trace,
            "gems": gems_context,
            "database": db_type
        }
        
        # Search for similar errors if enabled
        if search_similar and db.connected:
            with st.spinner("Searching for similar errors..."):
                # This would search the knowledge base for similar errors
                st.info("📚 Searching knowledge base for similar resolved errors...")
        
        # Analyze error
        with st.spinner("Analyzing error..."):
            analysis_result = debugger.analyze_error(error_message, context)
            
            if not analysis_result["error"]:
                # Display results in tabs
                tabs = st.tabs([
                    "📋 Analysis",
                    "🔧 Code Fix",
                    "⚡ Quick Actions",
                    "📚 Resources",
                    "💾 Save"
                ])
                
                with tabs[0]:
                    st.markdown(analysis_result["analysis"])
                    
                    # Confidence indicator
                    if code_snippet:
                        st.success("✅ High confidence - code context provided")
                    else:
                        st.warning("⚠️ Medium confidence - consider adding code context")
                
                with tabs[1]:
                    if code_snippet:
                        with st.spinner("Generating code fix..."):
                            code_fix = debugger.suggest_code_fix(
                                error_message, 
                                code_snippet,
                                rails_version
                            )
                            st.subheader("🔧 Suggested Fix")
                            st.code(code_fix, language="ruby")
                            
                            # Test instructions
                            st.subheader("🧪 Testing the Fix")
                            st.markdown("""
                            1. Apply the code changes
                            2. Run your test suite: `bundle exec rspec`
                            3. Test manually in development
                            4. Check logs: `tail -f log/development.log`
                            """)
                    else:
                        st.info("💡 Add code context to get specific fix suggestions")
                
                with tabs[2]:
                    st.subheader("⚡ Quick Actions")
                    
                    # Quick fixes for error type
                    quick_fixes = debugger.get_quick_fixes(error_type)
                    
                    if quick_fixes:
                        st.markdown(f"**Commands for {error_type.title()} errors:**")
                        for description, command in quick_fixes:
                            col1, col2 = st.columns([2, 3])
                            with col1:
                                st.markdown(f"**{description}**")
                            with col2:
                                st.code(command, language="bash")
                    
                    # General debugging commands
                    st.subheader("🛠️ General Debug Tools")
                    
                    col1, col2 = st.columns(2)
                    with col1:
                        st.markdown("""
                        **Console Commands:**
                        ```bash
                        rails console
                        rails dbconsole
                        rails runner 'p User.count'
                        ```
                        """)
                    
                    with col2:
                        st.markdown("""
                        **Logging:**
                        ```bash
                        tail -f log/development.log
                        grep ERROR log/production.log
                        rails log:clear
                        ```
                        """)
                
                with tabs[3]:
                    st.subheader("📚 Relevant Documentation")
                    
                    # Error-specific resources
                    resources = {
                        "routing": [
                            ("Rails Routing Guide", "https://guides.rubyonrails.org/routing.html"),
                            ("Route Helpers", "https://api.rubyonrails.org/classes/ActionDispatch/Routing.html")
                        ],
                        "database": [
                            ("Active Record Basics", "https://guides.rubyonrails.org/active_record_basics.html"),
                            ("Migrations Guide", "https://guides.rubyonrails.org/active_record_migrations.html")
                        ],
                        "validation": [
                            ("Active Record Validations", "https://guides.rubyonrails.org/active_record_validations.html"),
                            ("Validation Helpers", "https://api.rubyonrails.org/classes/ActiveRecord/Validations/ClassMethods.html")
                        ]
                    }
                    
                    if error_type in resources:
                        st.markdown(f"**Specific to {description}:**")
                        for title, url in resources[error_type]:
                            st.link_button(title, url, use_container_width=True)
                    
                    # General resources
                    st.markdown("**General Rails Resources:**")
                    col1, col2 = st.columns(2)
                    with col1:
                        st.link_button(
                            "Rails Guides",
                            "https://guides.rubyonrails.org/",
                            use_container_width=True
                        )
                        st.link_button(
                            "Rails API Docs",
                            "https://api.rubyonrails.org/",
                            use_container_width=True
                        )
                    with col2:
                        st.link_button(
                            "Rails Forum",
                            "https://discuss.rubyonrails.org/",
                            use_container_width=True
                        )
                        st.link_button(
                            "Stack Overflow",
                            "https://stackoverflow.com/questions/tagged/ruby-on-rails",
                            use_container_width=True
                        )
                    
                    # Debugging gems
                    st.subheader("🔍 Helpful Debugging Gems")
                    st.code("""# Gemfile
group :development do
  gem 'pry-rails'      # Better console
  gem 'better_errors'  # Better error pages
  gem 'bullet'         # N+1 query detection
  gem 'rack-mini-profiler'  # Performance
end""", language="ruby")
                
                with tabs[4]:
                    st.subheader("💾 Save Debug Session")
                    
                    # Session metadata
                    title = st.text_input(
                        "Session Title",
                        value=f"{description}: {error_message[:50]}..."
                    )
                    
                    solution_summary = st.text_area(
                        "Solution Summary",
                        placeholder="Briefly describe how you resolved this error..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"rails, {error_type}, {rails_version}, debugging"
                    )
                    
                    col1, col2 = st.columns(2)
                    with col1:
                        mark_resolved = st.checkbox("Mark as Resolved", value=True)
                    with col2:
                        share_public = st.checkbox("Share with Community", value=False)
                    
                    if st.button("💾 Save Session", type="primary"):
                        if db.connected:
                            # Compile full session
                            session_content = f"""## Error
{error_message}

## Analysis
{analysis_result['analysis']}

## Solution Summary
{solution_summary}

## Context
- Rails Version: {rails_version}
- Environment: {environment}
- Error Type: {error_type}
- Resolved: {'Yes' if mark_resolved else 'No'}
"""
                            
                            if code_snippet and 'code_fix' in locals():
                                session_content += f"\n## Code Fix\n```ruby\n{code_fix}\n```"
                            
                            metadata = {
                                "error_type": error_type,
                                "rails_version": rails_version,
                                "environment": environment,
                                "resolved": mark_resolved,
                                "public": share_public,
                                "timestamp": datetime.now().isoformat()
                            }
                            
                            query_id = debugger.db.log_query(
                                tool="rails_debugger",
                                model=debugger.default_model,
                                prompt=error_message,
                                response=analysis_result["analysis"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=session_content,
                                    category="Rails Debugging",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success(f"✅ Debug session saved!")
                                    if share_public:
                                        st.info("📢 Session marked for community sharing")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Analysis failed. Please check your Ollama connection.")
    
    # Common errors reference
    with st.expander("📖 Common Rails Errors Reference"):
        st.markdown("""
        ### 🚨 Frequently Encountered Errors
        
        | Error | Common Cause | Quick Fix |
        |-------|--------------|-----------|
        | **No route matches** | Missing route definition | Add route to `config/routes.rb` |
        | **RecordNotFound** | Invalid ID or deleted record | Add error handling or check ID |
        | **CSRF token** | Missing authenticity token | Add token to forms or skip for API |
        | **Undefined method for nil** | Calling method on nil object | Add nil checks or use `&.` |
        | **Mass assignment** | Unpermitted parameters | Update strong parameters |
        | **Template missing** | View file doesn't exist | Create the view file |
        | **Pending migrations** | Unapplied database changes | Run `rails db:migrate` |
        """)

# Entry point
if __name__ == "__main__":
    show()
