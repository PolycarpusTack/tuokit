"""
SmallTalk Image Browser for TuoKit
Helps navigate and query the SmallTalk image environment
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Image Browser - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class SmallTalkImageBrowser(OllamaToolBase):
    """SmallTalk image navigation and query tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="image_browser",
            default_model=ModelManager.get_default_model()
        )
        
        self.common_queries = {
            "Find Implementors": "Find all classes that implement a specific method",
            "Find Senders": "Find all methods that send a specific message",
            "Class Hierarchy": "Show inheritance hierarchy for a class",
            "Protocol Methods": "List all methods in a protocol",
            "Find References": "Find all references to a class or variable",
            "Package Contents": "Show all classes in a package",
            "Method Versions": "Show version history of a method",
            "Instance Variables": "Find all uses of an instance variable",
            "System Categories": "List all system categories",
            "Recent Changes": "Show recently modified methods"
        }
    
    def query_image(self, query: str, query_type: str = "general") -> dict:
        """Query SmallTalk image for information"""
        
        context = ""
        if query_type != "general":
            context = f"\nQuery type: {query_type} - {self.common_queries.get(query_type, '')}"
        
        prompt = f"""SmallTalk image browser query: {query}{context}

Provide:
1. How to find this information in the SmallTalk image
2. Specific browser commands or menu paths
3. Workspace code to execute if applicable
4. Example results
5. Related queries that might be helpful

Focus on VisualWorks SmallTalk environment and tools."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2,
            system="You are a SmallTalk environment expert. Provide practical guidance for navigating the image."
        )
        
        return {
            "response": result["response"],
            "error": result["error"]
        }
    
    def generate_browser_script(self, task: str) -> str:
        """Generate SmallTalk code for browser automation"""
        prompt = f"""Generate SmallTalk code to accomplish this browser task: {task}

Create a script that:
1. Opens necessary browsers
2. Navigates to the right location
3. Performs the requested action
4. Returns useful results

Use VisualWorks browser APIs."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1
        )
        
        return result["response"]
    
    def explain_browser_tool(self, tool_name: str) -> str:
        """Explain a specific SmallTalk browser tool"""
        prompt = f"""Explain the SmallTalk {tool_name} tool:

Include:
1. Purpose and main uses
2. How to open it
3. Key features and shortcuts
4. Common workflows
5. Tips and tricks"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.2
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🔍 SmallTalk Image Browser")
    st.markdown("Navigate and query the SmallTalk image environment effectively")
    
    # Initialize browser
    browser = SmallTalkImageBrowser()
    db = DatabaseManager()
    
    # Sidebar with browser tools
    with st.sidebar:
        st.subheader("🛠️ Browser Tools")
        
        tool_buttons = {
            "System Browser": "Browse classes and methods",
            "Workspace": "Execute SmallTalk code",
            "Inspector": "Examine objects",
            "Debugger": "Debug running code",
            "Changes Browser": "View code changes",
            "File Browser": "Browse file system",
            "Process Monitor": "View running processes"
        }
        
        for tool, description in tool_buttons.items():
            if st.button(tool, help=description, use_container_width=True):
                st.session_state.explain_tool = tool
        
        st.divider()
        
        # Quick actions
        st.subheader("⚡ Quick Actions")
        
        quick_actions = [
            "Open System Browser",
            "Browse Object Protocol", 
            "Find Method",
            "Search Source Code",
            "Show Class Hierarchy"
        ]
        
        selected_action = st.selectbox(
            "Select action",
            [""] + quick_actions
        )
        
        if selected_action and st.button("Show How"):
            st.session_state.show_action = selected_action
        
        st.divider()
        st.caption("💡 **Tip**: Use Cmd/Ctrl+Click for quick navigation")
    
    # Main content tabs
    tabs = st.tabs(["🔍 Query Browser", "📚 Common Tasks", "🛠️ Browser Scripts", "📖 Reference"])
    
    with tabs[0]:
        st.subheader("🔍 Query the SmallTalk Image")
        
        # Query type selection
        query_type = st.selectbox(
            "Query Type",
            ["General"] + list(browser.common_queries.keys()),
            help="Select a specific query type or use General"
        )
        
        # Show description
        if query_type != "General":
            st.info(f"**{query_type}**: {browser.common_queries[query_type]}")
        
        # Query input
        if query_type == "Find Implementors":
            query = st.text_input(
                "Method selector",
                placeholder="e.g., #printOn: or #at:put:"
            )
        elif query_type == "Find Senders":
            query = st.text_input(
                "Message selector",
                placeholder="e.g., #add: or #yourself"
            )
        elif query_type == "Class Hierarchy":
            query = st.text_input(
                "Class name",
                placeholder="e.g., Collection or Morph"
            )
        elif query_type == "Package Contents":
            query = st.text_input(
                "Package name",
                placeholder="e.g., Collections-Abstract"
            )
        else:
            query = st.text_input(
                "Your query",
                placeholder="e.g., How to find all subclasses of a class?"
            )
        
        # Search button
        if st.button("🔍 Search Image", type="primary", disabled=not query.strip()):
            with st.spinner("Querying SmallTalk image..."):
                result = browser.query_image(
                    query,
                    query_type.lower().replace(" ", "_") if query_type != "General" else "general"
                )
                
                if not result["error"]:
                    st.success("✅ Query completed!")
                    
                    # Display results
                    st.markdown(result["response"])
                    
                    # Save query option
                    if st.button("💾 Save This Query"):
                        st.session_state.save_query = {
                            "query": query,
                            "query_type": query_type,
                            "response": result["response"]
                        }
                else:
                    st.error("Query failed. Please try again.")
    
    with tabs[1]:
        st.subheader("📚 Common SmallTalk Tasks")
        
        # Task categories
        task_categories = {
            "🔍 Finding Code": [
                ("Find all implementors of a method", "implementors"),
                ("Find all senders of a message", "senders"),
                ("Search for string in source code", "source_search"),
                ("Find references to a class", "references")
            ],
            "📊 Understanding Structure": [
                ("View class hierarchy", "hierarchy"),
                ("List all methods of a class", "methods"),
                ("Show instance variables usage", "variables"),
                ("Examine protocol organization", "protocols")
            ],
            "🛠️ Development Tasks": [
                ("Create a new class", "new_class"),
                ("Add method to class", "add_method"),
                ("Refactor method names", "refactor"),
                ("Browse recent changes", "changes")
            ],
            "🔧 Debugging": [
                ("Set breakpoint", "breakpoint"),
                ("Inspect object state", "inspect"),
                ("View call stack", "stack"),
                ("Profile performance", "profile")
            ]
        }
        
        for category, tasks in task_categories.items():
            st.markdown(f"### {category}")
            
            task_cols = st.columns(2)
            for i, (task_name, task_id) in enumerate(tasks):
                with task_cols[i % 2]:
                    if st.button(task_name, key=f"task_{task_id}"):
                        with st.spinner(f"Getting instructions for: {task_name}"):
                            instructions = browser.query_image(
                                f"How to {task_name} in VisualWorks SmallTalk?",
                                "task_guide"
                            )
                            
                            with st.expander(f"📋 {task_name}", expanded=True):
                                st.markdown(instructions["response"])
    
    with tabs[2]:
        st.subheader("🛠️ Browser Automation Scripts")
        
        script_task = st.text_input(
            "Describe what you want to automate",
            placeholder="e.g., Find all methods that contain 'TODO' comments"
        )
        
        # Script options
        col1, col2 = st.columns(2)
        
        with col1:
            output_format = st.selectbox(
                "Output Format",
                ["Transcript", "Collection", "File", "Inspector"]
            )
        
        with col2:
            include_ui = st.checkbox(
                "Include UI automation",
                help="Open browsers automatically"
            )
        
        if st.button("⚙️ Generate Script", type="primary", disabled=not script_task):
            enhanced_task = f"{script_task}. Output results to {output_format}."
            if include_ui:
                enhanced_task += " Include browser UI automation."
            
            with st.spinner("Generating browser script..."):
                script = browser.generate_browser_script(enhanced_task)
                
                st.success("✅ Script generated!")
                
                # Display script
                st.subheader("Generated Script")
                st.code(script, language="smalltalk")
                
                # Usage instructions
                with st.expander("📖 How to Use This Script"):
                    st.markdown("""
                    1. **Copy the script** to your clipboard
                    2. **Open a Workspace** in VisualWorks
                    3. **Paste and select** the script
                    4. **Do It** (Ctrl+D) to execute
                    5. **Check results** in chosen output format
                    
                    **Tips:**
                    - Test on small datasets first
                    - Save useful scripts as methods
                    - Add error handling for production use
                    """)
        
        # Example scripts
        st.divider()
        st.subheader("📚 Example Scripts")
        
        example_tabs = st.tabs(["Find TODO", "Unused Methods", "Large Methods", "Dependencies"])
        
        with example_tabs[0]:
            st.code("""
"Find all methods containing TODO comments"
| todos |
todos := OrderedCollection new.
Smalltalk allClassesDo: [:class |
    class methodDictionary keysAndValuesDo: [:selector :method |
        (method sourceCode includesSubString: 'TODO') ifTrue: [
            todos add: class -> selector
        ]
    ]
].
todos inspect
            """, language="smalltalk")
        
        with example_tabs[1]:
            st.code("""
"Find potentially unused methods"
| unused |
unused := OrderedCollection new.
MyPackage allClasses do: [:class |
    class selectors do: [:selector |
        (SystemNavigation default allCallsOn: selector) isEmpty ifTrue: [
            unused add: class -> selector
        ]
    ]
].
unused
            """, language="smalltalk")
        
        with example_tabs[2]:
            st.code("""
"Find methods longer than 50 lines"
| largeMethods |
largeMethods := SortedCollection sortBlock: [:a :b | a value > b value].
Smalltalk allClassesDo: [:class |
    class methodDictionary keysAndValuesDo: [:sel :method |
        | lines |
        lines := method sourceCode lineCount.
        lines > 50 ifTrue: [
            largeMethods add: (class -> sel) -> lines
        ]
    ]
].
largeMethods inspect
            """, language="smalltalk")
        
        with example_tabs[3]:
            st.code("""
"Analyze class dependencies"
| deps |
deps := Dictionary new.
MyClass withAllSubclasses do: [:class |
    | references |
    references := Set new.
    class methodDictionary do: [:method |
        method literals do: [:lit |
            (lit isKindOf: Association) ifTrue: [
                references add: lit value
            ]
        ]
    ].
    deps at: class put: references
].
deps inspect
            """, language="smalltalk")
    
    with tabs[3]:
        st.subheader("📖 SmallTalk Browser Reference")
        
        # Tool explanation (if requested)
        if "explain_tool" in st.session_state:
            tool_name = st.session_state.explain_tool
            with st.expander(f"🛠️ {tool_name} Explanation", expanded=True):
                explanation = browser.explain_browser_tool(tool_name)
                st.markdown(explanation)
            del st.session_state.explain_tool
        
        # Action guide (if requested)
        if "show_action" in st.session_state:
            action = st.session_state.show_action
            with st.expander(f"⚡ How to: {action}", expanded=True):
                guide = browser.query_image(f"How to {action} in SmallTalk?", "action_guide")
                st.markdown(guide["response"])
            del st.session_state.show_action
        
        # Browser shortcuts
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("""
            ### ⌨️ Keyboard Shortcuts
            
            **Navigation**
            - `Cmd+B` - Browse class
            - `Cmd+N` - Find class
            - `Cmd+M` - Find method
            - `Cmd+Shift+F` - Search source
            
            **Execution**
            - `Cmd+D` - Do It
            - `Cmd+P` - Print It
            - `Cmd+I` - Inspect It
            - `Cmd+S` - Accept (Save)
            
            **Browsing**
            - `Cmd+Click` - Browse implementors
            - `Alt+Click` - Browse senders
            - `Cmd+H` - Browse hierarchy
            """)
        
        with col2:
            st.markdown("""
            ### 🖱️ Mouse Actions
            
            **System Browser**
            - Right-click for context menus
            - Double-click to browse
            - Drag to move methods
            
            **Workspace**
            - Select and right-click
            - Middle-click for menu
            
            **Inspector**
            - Click to select slot
            - Right-click to inspect
            - Drag to copy reference
            
            **Debugger**
            - Click to select frame
            - Right-click for options
            """)
        
        # Navigation workflows
        st.divider()
        st.subheader("🗺️ Navigation Workflows")
        
        workflow_tabs = st.tabs(["Finding Code", "Understanding", "Refactoring", "Debugging"])
        
        with workflow_tabs[0]:
            st.markdown("""
            ### Finding Code Workflow
            
            1. **Start with what you know**
               - Class name → System Browser
               - Method name → Implementors
               - Text → Search source
            
            2. **Follow the trail**
               - Browse senders of methods
               - Check class references
               - Examine protocols
            
            3. **Use the tools**
               - Method Finder for examples
               - Hierarchy Browser for inheritance
               - Changes Browser for history
            """)
        
        with workflow_tabs[1]:
            st.markdown("""
            ### Understanding Code Workflow
            
            1. **Start at class level**
               - Read class comment
               - Check superclass
               - List protocols
            
            2. **Examine structure**
               - Instance variables
               - Class variables
               - Method categories
            
            3. **Trace execution**
               - Set breakpoints
               - Step through code
               - Inspect objects
            """)
        
        with workflow_tabs[2]:
            st.markdown("""
            ### Refactoring Workflow
            
            1. **Identify target**
               - Find all implementors
               - Check senders
               - Analyze impact
            
            2. **Make changes**
               - Use refactoring browser
               - Update systematically
               - Maintain tests
            
            3. **Verify**
               - Run tests
               - Check senders again
               - Browse changes
            """)
        
        with workflow_tabs[3]:
            st.markdown("""
            ### Debugging Workflow
            
            1. **Reproduce issue**
               - Minimal test case
               - Consistent steps
               - Note symptoms
            
            2. **Set breakpoints**
               - Method entry
               - Conditional breaks
               - Exception breaks
            
            3. **Investigate**
               - Step through execution
               - Inspect variables
               - Check call stack
            """)
    
    # Save dialog (if triggered)
    if "save_query" in st.session_state:
        with st.expander("💾 Save Query", expanded=True):
            save_data = st.session_state.save_query
            
            title = st.text_input(
                "Title",
                value=f"Image Query: {save_data['query'][:40]}..."
            )
            
            notes = st.text_area(
                "Additional Notes",
                placeholder="Add any additional context or notes..."
            )
            
            tags = st.text_input(
                "Tags",
                value=f"image-browser, {save_data['query_type'].lower().replace(' ', '-')}, smalltalk"
            )
            
            if st.button("💾 Save Query", type="primary"):
                if db.connected:
                    content = f"""## Query Type: {save_data['query_type']}
## Query: {save_data['query']}

## Response
{save_data['response']}

## Notes
{notes}"""
                    
                    query_id = browser.db.log_query(
                        tool="image_browser",
                        model=browser.default_model,
                        prompt=save_data['query'],
                        response=save_data['response'],
                        metadata={"query_type": save_data['query_type']}
                    )
                    
                    if query_id and title:
                        success = db.save_knowledge_unit(
                            query_id=query_id,
                            title=title,
                            content=content,
                            category="SmallTalk Image Browser",
                            tags=[tag.strip() for tag in tags.split(",")]
                        )
                        if success:
                            st.success("✅ Query saved to knowledge library!")
                            del st.session_state.save_query
                            st.balloons()
                else:
                    st.warning("Database not connected")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
