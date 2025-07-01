# Alternative SmallTalk & Rails Tools Implementation

This document contains an alternative, more concise implementation of the SmallTalk and Rails development tools for TuoKit. This version emphasizes simplicity and can be used as a reference for future enhancements.

## Key Differences from Current Implementation

### 1. **Simplified Architecture**
- More compact code structure (under 100 lines per tool)
- Direct function calls instead of class-based approach
- Streamlined UI with fewer options

### 2. **Enhanced Features**
- Complexity sliders for snippet generation
- Detail level selection for code explanation
- Sidebar configuration options
- More educational components

### 3. **Database Structure**
- Simpler table schema with tags array
- Direct SQL queries instead of ORM approach
- Streamlined logging mechanism

## Alternative Implementation Code

### 1. SmallTalk Code Explainer - Compact Version

```python
# pages/smalltalk_explainer.py
import streamlit as st
from utils import OllamaInterface, DatabaseManager

def explain_smalltalk(code):
    """Generate detailed explanation of SmallTalk code"""
    return OllamaInterface.generate(
        model="deepseek-r1:6.7b",
        prompt=f"Explain this VisualWorks SmallTalk code:\n```\n{code}\n```",
        system=(
            "Provide analysis with these sections:\n"
            "1. Overall Purpose: What the code accomplishes\n"
            "2. Key Concepts: SmallTalk-specific paradigms used\n"
            "3. Execution Flow: Step-by-step walkthrough\n"
            "4. Potential Improvements: Optimization suggestions\n"
            "Use bullet points and simple language."
        )
    )

def show():
    st.title("🧑‍🏫 SmallTalk Code Explainer")
    st.caption("Understand VisualWorks SmallTalk code with AI-powered explanations")
    
    # Input
    code = st.text_area("Paste SmallTalk Code", 
                       height=300, 
                       placeholder="Example:\nObject subclass: #Person\n  instanceVariableNames: 'name age'\n  classVariableNames: ''\n  poolDictionaries: ''\n  category: 'Example'")
    
    # Configuration
    with st.sidebar:
        st.subheader("Analysis Options")
        detail_level = st.select_slider("Detail Level", ["Basic", "Detailed", "Advanced"])
        st.toggle("Include Optimization Tips", True, key="include_tips")
        st.toggle("Compare to OOP Paradigms", False, key="compare_oop")
    
    # Processing
    if st.button("Analyze Code", type="primary") and code:
        with st.spinner("Deciphering SmallTalk idioms..."):
            explanation = explain_smalltalk(code)
            
            # Display results
            st.subheader("Code Explanation")
            st.markdown(explanation)
            
            # Educational component
            with st.expander("🧠 Key SmallTalk Concepts", expanded=True):
                st.markdown("""
                - **Message Passing**: Core communication mechanism between objects
                - **Blocks**: Anonymous functions with lexical scoping
                - **MVC Pattern**: Model-View-Controller architecture
                - **Image-based Environment**: Persistent runtime state
                - **Live Debugging**: On-the-fly code modification
                """)
                st.link_button("SmallTalk Documentation", "https://wiki.squeak.org/squeak/")
            
            # Save to knowledge base
            if st.button("💾 Save to Knowledge Library"):
                db = DatabaseManager()
                db.log_query(
                    tool="smalltalk_explainer",
                    prompt=code[:500],
                    response=explanation,
                    tags=["smalltalk", "education"]
                )
                st.success("Analysis saved!")

if __name__ == "__main__":
    show()
```

### 2. Rails Scaffold Generator - Compact Version

```python
# pages/rails_scaffold.py
import streamlit as st
from utils import OllamaInterface, DatabaseManager

def generate_scaffold(description):
    """Generate Rails scaffold from natural language description"""
    return OllamaInterface.generate(
        model="deepseek-coder:6.7b",
        prompt=f"Generate Rails 7 scaffold for: {description}",
        system=(
            "Output only valid Ruby code with comments. Include:\n"
            "- Model with validations\n"
            "- Database migration\n"
            "- Controller with CRUD actions\n"
            "- Basic views (ERB)\n"
            "- Routes configuration\n"
            "Use Rails best practices and modern conventions."
        )
    )

def show():
    st.title("⚡ Rails Scaffold Generator")
    st.caption("Generate complete Rails scaffolds from natural language descriptions")
    
    # Input
    description = st.text_input("Describe your resource", 
                               placeholder="e.g., Blog post with title:string, content:text, published:boolean")
    
    # Scaffold options
    with st.sidebar:
        st.subheader("Scaffold Options")
        test_framework = st.radio("Testing", ["RSpec", "Minitest", "None"])
        template_engine = st.radio("Views", ["ERB", "Haml", "Slim"])
        st.toggle("Add Authentication", False)
        st.toggle("API Mode", False)
    
    # Processing
    if st.button("Generate Scaffold", type="primary") and description:
        with st.spinner("Building your Rails structure..."):
            code = generate_scaffold(description)
            
            # Display results
            st.subheader("Generated Scaffold")
            st.code(code, language="ruby")
            
            # Explanation panel
            with st.expander("📚 Rails Scaffold Concepts"):
                st.markdown("""
                - **Convention over Configuration**: Rails automates common patterns
                - **MVC Architecture**: Separation of concerns
                - **ActiveRecord**: ORM for database interactions
                - **RESTful Routes**: Standard resource routing
                """)
                st.link_button("Rails Guides", "https://guides.rubyonrails.org/")
            
            # Download and save options
            col1, col2 = st.columns(2)
            with col1:
                st.download_button("Download Scaffold", code, "scaffold.rb")
            with col2:
                if st.button("Save to Knowledge Library"):
                    db = DatabaseManager()
                    db.log_query(
                        tool="rails_scaffold",
                        prompt=description,
                        response=code,
                        tags=["rails", "scaffold"]
                    )
                    st.success("Scaffold saved!")

if __name__ == "__main__":
    show()
```

### 3. SmallTalk ↔ Ruby Converter - Compact Version

```python
# pages/code_converter.py
import streamlit as st
from utils import OllamaInterface

def convert_code(code, direction):
    """Convert between SmallTalk and Ruby paradigms"""
    if direction == "st2rb":
        return OllamaInterface.generate(
            model="deepseek-coder:6.7b",
            prompt=f"Convert this SmallTalk to idiomatic Ruby:\n```\n{code}\n```",
            system=(
                "Maintain original functionality while adapting to Ruby conventions.\n"
                "Include comments explaining paradigm shifts:\n"
                "- Message passing → Method calls\n"
                "- Blocks → Lambdas/Procs\n"
                "- Class initialization differences\n"
                "Output only code with comments."
            )
        )
    else:
        return OllamaInterface.generate(
            model="deepseek-coder:6.7b",
            prompt=f"Convert this Ruby to idiomatic VisualWorks SmallTalk:\n```\n{code}\n```",
            system=(
                "Use proper SmallTalk idioms and patterns.\n"
                "Include comments explaining:\n"
                "- Method calls → Message passing\n"
                "- Classes → Subclassing protocol\n"
                "- Modules → Traits\n"
                "Output only code with comments."
            )
        )

def show():
    st.title("🔄 SmallTalk ↔ Ruby Converter")
    st.caption("Convert code between VisualWorks SmallTalk and Ruby paradigms")
    
    # Direction selection
    direction = st.radio("Conversion Direction", 
                        ["SmallTalk to Ruby", "Ruby to SmallTalk"],
                        horizontal=True)
    
    # Code input
    code = st.text_area("Code to Convert", height=300,
                       placeholder="Paste code here...")
    
    # Processing
    if st.button("Convert Code", type="primary") and code:
        with st.spinner("Translating paradigms..."):
            result = convert_code(code, "st2rb" if "SmallTalk" in direction else "rb2st")
            
            # Display results
            st.subheader("Converted Code")
            st.code(result, language="ruby" if "Ruby" in direction else "smalltalk")
            
            # Paradigm comparison
            with st.expander("⚡ Paradigm Differences", expanded=True):
                if "SmallTalk" in direction:
                    st.markdown("""
                    **SmallTalk → Ruby Conversions:**
                    - Message passing → Object method calls
                    - Blocks → Lambdas/Procs
                    - Class methods → Static methods
                    - Metaclasses → Singleton classes
                    """)
                else:
                    st.markdown("""
                    **Ruby → SmallTalk Conversions:**
                    - Method calls → Message passing
                    - Classes → Subclass creation protocol
                    - Modules → Traits
                    - Yield → Block evaluation
                    """)
                st.link_button("Paradigm Comparison Guide", "#")

if __name__ == "__main__":
    show()
```

### 4. Rails Debugging Assistant - Compact Version

```python
# pages/rails_debugger.py
import streamlit as st
from utils import OllamaInterface

def debug_rails(error, code_context=""):
    """Provide debugging solutions for Rails errors"""
    # Analysis
    analysis = OllamaInterface.generate(
        model="deepseek-r1:6.7b",
        prompt=f"Debug this Rails error:\n{error}\n\nCode context:\n{code_context}",
        system=(
            "Provide structured analysis:\n"
            "1. Error Cause: Technical explanation\n"
            "2. Common Solutions: List 3-5 fixes\n"
            "3. Prevention Tips: How to avoid recurrence\n"
            "4. Related Documentation: Official Rails guides\n"
            "Use bullet points and simple language."
        )
    )
    
    # Code fix
    if code_context:
        fix = OllamaInterface.generate(
            model="deepseek-coder:6.7b",
            prompt=f"Suggest code fix for:\n{error}\n\nCode:\n{code_context}",
            system="Output only corrected Ruby code with comments explaining changes"
        )
    else:
        fix = "Code context needed for specific fix"
    
    return analysis, fix

def show():
    st.title("🐞 Rails Debugging Assistant")
    st.caption("Diagnose and fix Ruby on Rails errors with AI assistance")
    
    # Inputs
    error = st.text_area("Paste Error Message", 
                        height=150,
                        placeholder="ActiveRecord::RecordNotFound in PostsController#show...")
    code_context = st.text_area("Relevant Code (Optional)", 
                               height=200,
                               placeholder="Controller/Model code related to the error")
    
    # Processing
    if st.button("Debug Error", type="primary") and error:
        with st.spinner("Diagnosing the issue..."):
            analysis, fix = debug_rails(error, code_context)
            
            # Display analysis
            st.subheader("Error Analysis")
            st.markdown(analysis)
            
            # Display code fix
            if code_context:
                st.subheader("Suggested Fix")
                st.code(fix, language="ruby")
            
            # Common errors reference
            with st.expander("📚 Common Rails Errors", expanded=True):
                st.markdown("""
                - **ActiveRecord::RecordNotFound**: Missing database record
                - **ActionController::RoutingError**: Incorrect route definition
                - **NameError**: Missing variable/method definition
                - **SyntaxError**: Ruby syntax mistakes
                - **MissingTemplate**: View file not found
                """)
                st.link_button("Rails Error Documentation", "https://guides.rubyonrails.org/debugging_rails_applications.html")

if __name__ == "__main__":
    show()
```

### 5. SmallTalk Snippet Finder - Compact Version

```python
# pages/smalltalk_snippets.py
import streamlit as st
from utils import OllamaInterface, DatabaseManager

SNIPPET_CATEGORIES = [
    "Collections", "GUI (MVC)", "File I/O", 
    "Database Access", "Unit Testing", "Metaprogramming"
]

def generate_snippet(category, complexity):
    """Generate SmallTalk code snippet for a specific category"""
    return OllamaInterface.generate(
        model="deepseek-coder:6.7b",
        prompt=f"Generate practical VisualWorks SmallTalk snippet for: {category}",
        system=(
            f"Create a {complexity}-level complexity snippet with:\n"
            "1. Working implementation\n"
            "2. Usage example\n"
            "3. Brief explanation\n"
            "4. Key methods used\n"
            "Use modern VisualWorks conventions."
        )
    )

def show():
    st.title("📚 SmallTalk Snippet Library")
    st.caption("Discover ready-to-use VisualWorks SmallTalk code examples")
    
    # Selection
    col1, col2 = st.columns(2)
    with col1:
        category = st.selectbox("Category", SNIPPET_CATEGORIES)
    with col2:
        complexity = st.select_slider("Complexity", ["Beginner", "Intermediate", "Advanced"])
    
    # Generate snippet
    if st.button("Generate Snippet", type="primary"):
        with st.spinner("Crafting SmallTalk example..."):
            snippet = generate_snippet(category, complexity)
            
            # Display results
            st.subheader(f"{category} Example ({complexity})")
            st.code(snippet, language="smalltalk")
            
            # Category explanation
            with st.expander("🧠 About This Category", expanded=True):
                if category == "Collections":
                    st.markdown("**Collections in SmallTalk:** Arrays, Dictionaries, Sets with polymorphic messages")
                elif category == "GUI (MVC)":
                    st.markdown("**MVC Pattern:** Model-View-Controller implementation using Morphic")
                elif category == "Database Access":
                    st.markdown("**Database Access:** Glorp ORM examples")
                st.link_button(f"{category} Documentation", "#")
            
            # Save options
            if st.button("💾 Save to My Snippets"):
                db = DatabaseManager()
                db.log_query(
                    tool="smalltalk_snippets",
                    prompt=f"{category} ({complexity})",
                    response=snippet,
                    tags=["smalltalk", "snippet", category.lower()]
                )
                st.success("Snippet saved to your library!")

if __name__ == "__main__":
    show()
```

## Alternative Utils Module Structure

```python
# utils.py (alternative approach)
import ollama
import psycopg2
import os

class OllamaInterface:
    @staticmethod
    def generate(model, prompt, system="", format="text", temperature=0.3):
        """Generate text using Ollama models"""
        response = ollama.generate(
            model=model,
            prompt=prompt,
            system=system,
            options={'temperature': temperature}
        )
        return response['response']

class DatabaseManager:
    def __init__(self):
        self.conn = psycopg2.connect(os.getenv("DB_URL"))
    
    def log_query(self, tool, prompt, response, tags):
        """Save query to PostgreSQL database"""
        with self.conn.cursor() as cur:
            cur.execute(
                "INSERT INTO queries (tool, prompt, response, tags) VALUES (%s, %s, %s, %s)",
                (tool, prompt[:500], response, tags)
            )
        self.conn.commit()
    
    def __del__(self):
        self.conn.close()
```

## Alternative Database Schema

```sql
CREATE TABLE queries (
    id SERIAL PRIMARY KEY,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    tool VARCHAR(50) NOT NULL,
    prompt TEXT NOT NULL,
    response TEXT NOT NULL,
    tags VARCHAR(50)[]
);
```

## Comparison with Current Implementation

### Current Implementation Advantages:
1. **More robust error handling** - Comprehensive try/catch blocks
2. **Better integration** - Uses existing TuoKit patterns
3. **Extended functionality** - More features and options
4. **Detailed documentation** - More inline comments

### Alternative Implementation Advantages:
1. **Simpler code** - Under 100 lines per tool
2. **Direct approach** - Less abstraction layers
3. **Enhanced UI features** - Complexity sliders, detail levels
4. **Cleaner structure** - More focused functions

## Migration Path

To migrate from current to alternative implementation:

1. **Gradual Migration**:
   - Test alternative implementations in parallel
   - Compare performance and user feedback
   - Migrate one tool at a time

2. **Feature Merge**:
   - Keep robust error handling from current
   - Add UI enhancements from alternative
   - Maintain knowledge capture functionality

3. **Database Considerations**:
   - Current schema is more comprehensive
   - Alternative uses simpler tags array
   - Consider hybrid approach

## Recommendations

1. **Keep Current Implementation** for production use
2. **Use Alternative** as reference for:
   - UI improvements (sliders, toggles)
   - Code simplification opportunities
   - Feature ideas (complexity levels)

3. **Future Development**:
   - Merge best features from both
   - Add complexity sliders to current tools
   - Implement sidebar configuration options
   - Enhance educational components

## Usage Comparison

### Current Implementation:
```python
# Class-based approach
explainer = SmallTalkExplainer()
result = explainer.explain_code(code)
```

### Alternative Implementation:
```python
# Function-based approach
result = explain_smalltalk(code)
```

Both approaches are valid, with the current implementation providing better extensibility and the alternative offering simplicity.

---

This document serves as a reference for future enhancements and demonstrates alternative architectural approaches for the SmallTalk and Rails development tools in TuoKit.
