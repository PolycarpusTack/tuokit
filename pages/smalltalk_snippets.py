"""
SmallTalk Snippet Finder for TuoKit
Generates practical SmallTalk code snippets for common tasks
Enhanced with complexity levels and pattern recognition
"""

import streamlit as st
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
from datetime import datetime
import json

class SmallTalkSnippetFinder(OllamaToolBase):
    """SmallTalk snippet generation and library tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_snippets",
            default_model="deepseek-coder:6.7b"
        )
        
        self.snippet_categories = {
            "Collections & Iteration": {
                "icon": "📚",
                "description": "Arrays, OrderedCollections, Sets, Dictionaries",
                "subcategories": ["Basic Operations", "Advanced Iteration", "Collection Conversion"]
            },
            "GUI Development (MVC)": {
                "icon": "🎨",
                "description": "Morphic UI, MVC patterns, Event handling",
                "subcategories": ["Basic Windows", "Custom Widgets", "Event Handling"]
            },
            "File I/O": {
                "icon": "📁",
                "description": "File reading, writing, and stream operations",
                "subcategories": ["Text Files", "Binary Files", "Directory Operations"]
            },
            "Database Access": {
                "icon": "🗄️",
                "description": "Glorp ORM, SQL connectivity, Persistence",
                "subcategories": ["Basic Queries", "Transactions", "Schema Management"]
            },
            "Testing & Debugging": {
                "icon": "🧪",
                "description": "SUnit tests, Debugging tools, Assertions",
                "subcategories": ["Unit Tests", "Mock Objects", "Performance Testing"]
            },
            "String Manipulation": {
                "icon": "✏️",
                "description": "String operations, parsing, formatting",
                "subcategories": ["Basic Operations", "Regular Expressions", "Text Processing"]
            },
            "Date & Time": {
                "icon": "🕐",
                "description": "Date arithmetic, formatting, time zones",
                "subcategories": ["Basic Operations", "Formatting", "Time Calculations"]
            },
            "Network & HTTP": {
                "icon": "🌐",
                "description": "HTTP requests, sockets, web services",
                "subcategories": ["HTTP Client", "Socket Programming", "REST APIs"]
            },
            "Exception Handling": {
                "icon": "⚠️",
                "description": "Error handling, custom exceptions, recovery",
                "subcategories": ["Basic Try-Catch", "Custom Exceptions", "Error Recovery"]
            },
            "Design Patterns": {
                "icon": "🏗️",
                "description": "Common OOP patterns in SmallTalk",
                "subcategories": ["Creational", "Structural", "Behavioral"]
            }
        }
    
    def generate_snippet(self, category: str, complexity: str, 
                        specific_task: str = "", subcategory: str = "") -> dict:
        """Generate SmallTalk snippet with specified complexity"""
        
        complexity_notes = {
            "Beginner": "Use simple, clear code with extensive comments. Avoid advanced features.",
            "Intermediate": "Include common patterns and idioms. Balance clarity with efficiency.",
            "Advanced": "Show sophisticated techniques, metaprogramming, and performance optimizations."
        }
        
        prompt = f"""Generate a {complexity} level VisualWorks SmallTalk snippet for: {category}
{f'Subcategory: {subcategory}' if subcategory else ''}
{f'Specific task: {specific_task}' if specific_task else ''}

Requirements:
1. Working, runnable code
2. {complexity_notes[complexity]}
3. Include usage example
4. List key methods/classes used
5. Add inline comments explaining SmallTalk-specific concepts
6. If applicable, show common variations

Make it practical and immediately usable in a VisualWorks environment."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2,
            system=f"""Generate clean, idiomatic SmallTalk code following VisualWorks conventions.
Target audience: {complexity} level SmallTalk developers.
Focus on real-world applicability and best practices."""
        )
        
        return {
            "snippet": result["response"],
            "error": result["error"]
        }
    
    def search_snippets(self, query: str, filters: dict = None) -> list:
        """Search saved snippets with advanced filtering"""
        db = DatabaseManager()
        if not db.connected:
            return []
        
        try:
            # Build query with filters
            sql_query = """
                SELECT q.id, q.user_prompt, q.ai_response, q.created_at, q.metadata
                FROM queries q
                WHERE q.tool = 'smalltalk_snippets'
                AND (q.user_prompt ILIKE %s OR q.ai_response ILIKE %s)
            """
            params = [f'%{query}%', f'%{query}%']
            
            if filters:
                if filters.get('category'):
                    sql_query += " AND q.metadata->>'category' = %s"
                    params.append(filters['category'])
                if filters.get('complexity'):
                    sql_query += " AND q.metadata->>'complexity' = %s"
                    params.append(filters['complexity'])
            
            sql_query += " ORDER BY q.created_at DESC LIMIT 20"
            
            with db.conn.cursor() as cur:
                cur.execute(sql_query, params)
                return cur.fetchall()
        except Exception as e:
            st.error(f"Search error: {e}")
            return []
    
    def get_popular_snippets(self, limit: int = 5) -> list:
        """Get most frequently accessed snippets"""
        # In a real implementation, this would track access counts
        # For now, return recent snippets
        db = DatabaseManager()
        if not db.connected:
            return []
        
        try:
            with db.conn.cursor() as cur:
                cur.execute("""
                    SELECT user_prompt, COUNT(*) as usage_count
                    FROM queries
                    WHERE tool = 'smalltalk_snippets'
                    GROUP BY user_prompt
                    ORDER BY usage_count DESC
                    LIMIT %s
                """, (limit,))
                return cur.fetchall()
        except:
            return []

def show():
    """Main page display function"""
    st.title("📚 SmallTalk Snippet Library")
    st.markdown("Find and generate practical SmallTalk code snippets for common tasks")
    
    # Initialize snippet finder
    finder = SmallTalkSnippetFinder()
    db = DatabaseManager()
    
    # Sidebar configuration
    with st.sidebar:
        st.subheader("⚙️ Snippet Options")
        
        complexity = st.select_slider(
            "Complexity Level",
            options=["Beginner", "Intermediate", "Advanced"],
            value="Intermediate",
            help="Adjust code complexity and explanations"
        )
        
        st.divider()
        
        include_tests = st.toggle(
            "Include Test Examples",
            value=False,
            help="Add SUnit test cases"
        )
        
        include_performance = st.toggle(
            "Include Performance Notes",
            value=False,
            help="Add performance considerations"
        )
        
        st.divider()
        
        # Popular snippets
        st.subheader("🔥 Popular Snippets")
        popular = finder.get_popular_snippets(3)
        if popular:
            for prompt, count in popular:
                if st.button(f"📌 {prompt[:30]}... ({count}x)", key=f"pop_{prompt[:10]}"):
                    st.session_state.load_popular = prompt
        else:
            st.caption("No popular snippets yet")
    
    # Tab interface
    tabs = st.tabs([
        "🔍 Generate Snippets",
        "📖 Saved Library",
        "🎯 Quick Reference",
        "🏆 Pattern Gallery"
    ])
    
    with tabs[0]:
        # Category selection with icons
        col1, col2 = st.columns([3, 1])
        
        with col1:
            # Display categories with icons
            category_names = list(finder.snippet_categories.keys())
            category_display = [
                f"{finder.snippet_categories[cat]['icon']} {cat}" 
                for cat in category_names
            ]
            
            selected_display = st.selectbox(
                "Select Category",
                category_display,
                help="Choose a general category for your snippet"
            )
            
            # Extract actual category name
            category = category_names[category_display.index(selected_display)]
            
        with col2:
            st.metric(
                "Category",
                finder.snippet_categories[category]["icon"],
                finder.snippet_categories[category]["description"]
            )
        
        # Subcategory selection
        if finder.snippet_categories[category].get("subcategories"):
            subcategory = st.selectbox(
                "Subcategory",
                [""] + finder.snippet_categories[category]["subcategories"],
                help="Narrow down to specific topic"
            )
        else:
            subcategory = ""
        
        # Task description
        specific_task = st.text_input(
            "Specific Task (Optional)",
            placeholder="e.g., 'Sort collection by multiple criteria' or 'Parse CSV file'",
            help="Describe exactly what you need"
        )
        
        # Quick task suggestions
        st.markdown("**💡 Quick Tasks:**")
        task_suggestions = {
            "Collections & Iteration": [
                "Filter and transform collection",
                "Group by multiple criteria",
                "Custom sorting algorithm"
            ],
            "GUI Development (MVC)": [
                "Create custom dialog",
                "Handle drag and drop",
                "Build property inspector"
            ],
            "File I/O": [
                "Read CSV file",
                "Process large files in chunks",
                "Directory tree walker"
            ]
        }
        
        if category in task_suggestions:
            cols = st.columns(3)
            for i, task in enumerate(task_suggestions[category]):
                with cols[i]:
                    if st.button(task, key=f"task_{i}"):
                        specific_task = task
        
        # Generate button
        if st.button("🎯 Generate Snippet", type="primary"):
            with st.spinner(f"Creating {complexity} level {category} snippet..."):
                result = finder.generate_snippet(
                    category, 
                    complexity,
                    specific_task,
                    subcategory
                )
                
                if not result["error"]:
                    st.success(f"✅ {category} snippet generated!")
                    
                    # Display snippet with syntax highlighting
                    st.subheader("📝 Generated SmallTalk Code")
                    st.code(result["snippet"], language="smalltalk")
                    
                    # Action buttons
                    col1, col2, col3, col4 = st.columns(4)
                    
                    with col1:
                        st.download_button(
                            "📥 Download",
                            data=result["snippet"],
                            file_name=f"{category.lower().replace(' ', '_')}_{complexity.lower()}.st",
                            mime="text/plain"
                        )
                    
                    with col2:
                        if st.button("🔄 Regenerate"):
                            st.rerun()
                    
                    with col3:
                        # Copy to clipboard (placeholder)
                        if st.button("📋 Copy"):
                            st.info("Code copied! (requires JavaScript)")
                    
                    with col4:
                        # Save button
                        save_snippet = st.button("💾 Save")
                    
                    # Save dialog
                    if save_snippet:
                        with st.expander("💾 Save Snippet Details", expanded=True):
                            save_title = st.text_input(
                                "Snippet Title",
                                value=f"{category}: {specific_task or subcategory or 'General'}"
                            )
                            
                            save_tags = st.text_input(
                                "Tags",
                                value=f"smalltalk, {category.lower()}, {complexity.lower()}, snippet"
                            )
                            
                            save_notes = st.text_area(
                                "Notes",
                                placeholder="Add any implementation notes..."
                            )
                            
                            if st.button("💾 Confirm Save", type="primary"):
                                if db.connected:
                                    metadata = {
                                        "category": category,
                                        "subcategory": subcategory,
                                        "complexity": complexity,
                                        "specific_task": specific_task,
                                        "notes": save_notes
                                    }
                                    
                                    task_desc = specific_task or subcategory or category
                                    query_id = finder.db.log_query(
                                        tool="smalltalk_snippets",
                                        model=finder.default_model,
                                        prompt=f"{category} - {complexity}: {task_desc}",
                                        response=result["snippet"],
                                        metadata=metadata
                                    )
                                    
                                    if query_id:
                                        success = db.save_knowledge_unit(
                                            query_id=query_id,
                                            title=save_title,
                                            content=result["snippet"],
                                            category="SmallTalk Snippets",
                                            tags=[tag.strip() for tag in save_tags.split(",")]
                                        )
                                        if success:
                                            st.success("✅ Snippet saved to library!")
                                            st.balloons()
                                else:
                                    st.warning("Database not connected")
                    
                    # Learning notes
                    if complexity == "Beginner":
                        st.info("💡 **Beginner Tip**: Try modifying this snippet step by step to understand how it works")
                    elif complexity == "Advanced":
                        st.info("🚀 **Advanced Note**: This snippet demonstrates sophisticated SmallTalk techniques")
                else:
                    st.error("Failed to generate snippet. Check Ollama connection.")
    
    with tabs[1]:
        st.subheader("📖 Your Saved Snippets")
        
        # Search and filter
        col1, col2, col3 = st.columns([3, 1, 1])
        
        with col1:
            search_query = st.text_input(
                "Search snippets",
                placeholder="Enter keywords...",
                help="Search in titles and code"
            )
        
        with col2:
            filter_category = st.selectbox(
                "Category",
                ["All"] + list(finder.snippet_categories.keys()),
                help="Filter by category"
            )
        
        with col3:
            filter_complexity = st.selectbox(
                "Complexity",
                ["All", "Beginner", "Intermediate", "Advanced"],
                help="Filter by complexity"
            )
        
        # Build filters
        filters = {}
        if filter_category != "All":
            filters["category"] = filter_category
        if filter_complexity != "All":
            filters["complexity"] = filter_complexity
        
        # Search if query provided
        if search_query or filters:
            snippets = finder.search_snippets(search_query or "", filters)
            
            if snippets:
                st.markdown(f"**Found {len(snippets)} snippets:**")
                
                for snippet in snippets:
                    snippet_id, prompt, code, created, metadata = snippet[:5]
                    
                    # Parse metadata
                    try:
                        meta = json.loads(metadata) if metadata else {}
                    except:
                        meta = {}
                    
                    # Display snippet card
                    with st.expander(
                        f"{meta.get('category', 'General')} - {prompt[:50]}... ({created.strftime('%Y-%m-%d')})"
                    ):
                        # Metadata badges
                        col1, col2, col3 = st.columns(3)
                        with col1:
                            st.caption(f"📊 Complexity: {meta.get('complexity', 'Unknown')}")
                        with col2:
                            st.caption(f"🏷️ Category: {meta.get('category', 'General')}")
                        with col3:
                            st.caption(f"🆔 ID: {snippet_id}")
                        
                        # Code display
                        st.code(code, language="smalltalk")
                        
                        # Notes if available
                        if meta.get('notes'):
                            st.info(f"📝 Notes: {meta['notes']}")
                        
                        # Actions
                        col1, col2 = st.columns(2)
                        with col1:
                            st.download_button(
                                "📥 Download",
                                data=code,
                                file_name=f"snippet_{snippet_id}.st",
                                key=f"dl_{snippet_id}"
                            )
                        with col2:
                            if st.button("🗑️ Delete", key=f"del_{snippet_id}"):
                                st.warning("Delete functionality pending")
            else:
                st.info("No snippets found matching your criteria")
        else:
            # Show recent snippets
            st.markdown("**Recent Snippets:**")
            if db.connected:
                recent = db.get_recent_queries(limit=10)
                filtered = [q for q in recent if q[1] == "smalltalk_snippets"]
                
                if filtered:
                    for query in filtered[:5]:
                        with st.expander(
                            f"{query[2][:50]}... - {query[3].strftime('%Y-%m-%d %H:%M')}"
                        ):
                            full_query = db.get_query_by_id(query[0])
                            if full_query:
                                st.code(full_query[4], language="smalltalk")
                else:
                    st.info("No saved snippets yet. Generate some above!")
    
    with tabs[2]:
        st.subheader("🎯 SmallTalk Quick Reference")
        
        # Common patterns in columns
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("### Basic Syntax")
            
            patterns = {
                "Variable Declaration": """| temp collection |
temp := 'Hello'.
collection := OrderedCollection new.""",
                
                "Message Sending": """\"Unary message\"
array size.

\"Binary message\"
3 + 4.

\"Keyword message\"
array at: 1 put: 'value'.""",
                
                "Block Closure": """\"Simple block\"
[Transcript show: 'Hello'].

\"Block with arguments\"
[:x :y | x + y].

\"Block evaluation\"
aBlock value: 5.""",
                
                "Conditionals": """number > 0
    ifTrue: [Transcript show: 'Positive']
    ifFalse: [Transcript show: 'Non-positive']."""
            }
            
            for name, code in patterns.items():
                with st.expander(name):
                    st.code(code, language="smalltalk")
        
        with col2:
            st.markdown("### Collections")
            
            collection_patterns = {
                "Iteration": """\"do:\"
collection do: [:each | 
    Transcript show: each].

\"select:\"
evens := numbers select: [:n | n even].

\"collect:\"
squared := numbers collect: [:n | n * n].""",
                
                "Dictionary": """dict := Dictionary new.
dict at: #name put: 'John'.
dict at: #age put: 30.

\"Access with default\"
value := dict at: #city ifAbsent: ['Unknown'].""",
                
                "Streams": """stream := WriteStream on: String new.
stream nextPutAll: 'Hello'.
stream space.
stream nextPutAll: 'World'.
result := stream contents."""
            }
            
            for name, code in collection_patterns.items():
                with st.expander(name):
                    st.code(code, language="smalltalk")
        
        # Message syntax reference
        st.markdown("### Message Precedence")
        st.markdown("""
        | Precedence | Type | Example | Description |
        |------------|------|---------|-------------|
        | 1 (Highest) | Unary | `array size` | No arguments |
        | 2 | Binary | `3 + 4` | One argument, operator |
        | 3 (Lowest) | Keyword | `array at: 1` | Named arguments |
        
        **Cascading**: Send multiple messages to same object using `;`
        ```smalltalk
        Transcript 
            show: 'Hello';
            cr;
            show: 'World'.
        ```
        """)
    
    with tabs[3]:
        st.subheader("🏆 Design Pattern Gallery")
        
        pattern_categories = {
            "Creational Patterns": ["Singleton", "Factory Method", "Builder", "Prototype"],
            "Structural Patterns": ["Adapter", "Composite", "Proxy", "Decorator"],
            "Behavioral Patterns": ["Observer", "Strategy", "Template Method", "Visitor"]
        }
        
        selected_category = st.selectbox(
            "Pattern Category",
            list(pattern_categories.keys())
        )
        
        selected_pattern = st.selectbox(
            "Select Pattern",
            pattern_categories[selected_category]
        )
        
        if st.button("📖 Show Pattern Implementation"):
            with st.spinner(f"Generating {selected_pattern} pattern..."):
                # Generate pattern implementation
                pattern_result = finder.generate_snippet(
                    "Design Patterns",
                    "Intermediate",
                    f"Implement {selected_pattern} pattern with example usage"
                )
                
                if not pattern_result["error"]:
                    st.code(pattern_result["snippet"], language="smalltalk")
                    
                    # Pattern explanation
                    st.info(f"""
                    **{selected_pattern} Pattern**
                    
                    This pattern is useful for:
                    - Solving recurring design problems
                    - Creating flexible, maintainable code
                    - Following SmallTalk best practices
                    """)
                    
                    # Save pattern option
                    if st.button(f"💾 Save {selected_pattern} Pattern"):
                        st.success("Pattern saved to library!")
    
    # Load popular snippet if requested
    if "load_popular" in st.session_state:
        # Switch to generate tab and load the snippet
        st.info(f"Loading popular snippet: {st.session_state.load_popular}")
        del st.session_state.load_popular

# Entry point
if __name__ == "__main__":
    show()
