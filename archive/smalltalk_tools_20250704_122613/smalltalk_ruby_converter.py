"""
SmallTalk ↔ Ruby Converter for TuoKit
Converts code between SmallTalk and Ruby with paradigm explanations
Enhanced with conversion options and pattern library
"""

import streamlit as st

# Page configuration
st.set_page_config(
    page_title="Smalltalk Ruby Converter - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager

class CodeConverter(OllamaToolBase):
    """Converts code between SmallTalk and Ruby"""
    
    def __init__(self):
        super().__init__(
            tool_name="smalltalk_ruby_converter",
            default_model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
        )
    
    def convert_code(self, code: str, direction: str, 
                    preserve_style: bool = False,
                    add_explanations: bool = True) -> dict:
        """Convert code between languages with options"""
        
        if direction == "smalltalk_to_ruby":
            style_note = "Maintain SmallTalk's message-passing style where appropriate" if preserve_style else "Use idiomatic Ruby conventions"
            
            prompt = f"""Convert this SmallTalk code to Ruby:

```smalltalk
{code}
```

Requirements:
1. Maintain exact functionality
2. {style_note}
3. {"Add comments explaining key differences" if add_explanations else "Minimal comments"}
4. Handle SmallTalk-specific patterns appropriately"""
            
            system = """You are an expert in both SmallTalk and Ruby. Focus on accurate translations.
Key conversions to handle:
- Message passing → Method calls
- Blocks → Lambdas/Procs/Blocks
- Collections → Ruby enumerables
- Class definitions → Ruby classes
- Metaclass → Singleton class"""

        else:  # ruby_to_smalltalk
            prompt = f"""Convert this Ruby code to SmallTalk:

```ruby
{code}
```

Requirements:
1. Use proper VisualWorks SmallTalk syntax
2. Follow SmallTalk naming conventions
3. {"Add comments explaining paradigm shifts" if add_explanations else "Minimal comments"}
4. Handle Ruby-specific features appropriately"""
            
            system = """You are an expert in both Ruby and SmallTalk. Focus on proper SmallTalk patterns.
Key conversions to handle:
- Method calls → Message passing
- Classes → Subclass creation protocol
- Modules → Traits or method categories
- Yield → Block evaluation
- Instance variables → SmallTalk instance variables"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system=system
        )
        
        return {
            "converted": result["response"],
            "error": result["error"]
        }
    
    def explain_differences(self, original: str, converted: str, direction: str) -> str:
        """Explain key differences between the two versions"""
        lang1, lang2 = ("SmallTalk", "Ruby") if "to_ruby" in direction else ("Ruby", "SmallTalk")
        
        prompt = f"""Compare these code snippets and explain the key paradigm differences:

{lang1} Original:
```
{original[:500]}...
```

{lang2} Converted:
```
{converted[:500]}...
```

Focus on:
1. Syntax differences
2. Object model differences
3. Message passing vs method calls
4. Block/closure handling
5. Class definition approaches"""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.3,
            system="Provide clear, educational explanations suitable for developers learning both languages."
        )
        
        return result["response"]

def show():
    """Main page display function"""
    st.title("🔄 SmallTalk ↔ Ruby Converter")
    st.markdown("Convert code between SmallTalk and Ruby with detailed paradigm explanations")
    
    # Initialize converter
    converter = CodeConverter()
    db = DatabaseManager()
    
    # Sidebar options
    with st.sidebar:
        st.subheader("⚙️ Conversion Options")
        
        preserve_style = st.toggle(
            "Preserve Original Style",
            value=False,
            help="Keep source language idioms where possible"
        )
        
        add_explanations = st.toggle(
            "Add Explanatory Comments",
            value=True,
            help="Include comments explaining conversions"
        )
        
        st.divider()
        
        st.subheader("📚 Quick Patterns")
        
        pattern_type = st.selectbox(
            "Load Example Pattern",
            ["", "Collection Operations", "Class Definition", "Block/Closure", 
             "Exception Handling", "File I/O", "Method Definition"]
        )
        
        if pattern_type:
            if st.button(f"Load {pattern_type}"):
                st.session_state.load_pattern = pattern_type
    
    # Direction selector with visual indication
    col1, col2, col3 = st.columns([2, 1, 2])
    
    with col1:
        st.markdown("### 🟦 SmallTalk")
    with col2:
        direction = st.radio(
            "",
            ["→", "←"],
            horizontal=True,
            label_visibility="collapsed"
        )
    with col3:
        st.markdown("### 💎 Ruby")
    
    # Determine conversion direction
    conv_direction = "smalltalk_to_ruby" if direction == "→" else "ruby_to_smalltalk"
    source_lang = "SmallTalk" if direction == "→" else "Ruby"
    target_lang = "Ruby" if direction == "→" else "SmallTalk"
    
    # Load pattern if requested
    if "load_pattern" in st.session_state:
        pattern_examples = {
            "Collection Operations": {
                "SmallTalk": """numbers := #(1 2 3 4 5).
squared := numbers collect: [:n | n * n].
sum := numbers inject: 0 into: [:total :n | total + n].
evens := numbers select: [:n | n even].""",
                "Ruby": """numbers = [1, 2, 3, 4, 5]
squared = numbers.map { |n| n * n }
sum = numbers.reduce(0) { |total, n| total + n }
evens = numbers.select { |n| n.even? }"""
            },
            "Class Definition": {
                "SmallTalk": """Object subclass: #Person
    instanceVariableNames: 'name age'
    classVariableNames: 'Population'
    poolDictionaries: ''
    category: 'MyApp-Models'""",
                "Ruby": """class Person
  attr_accessor :name, :age
  @@population = 0
  
  def initialize(name, age)
    @name = name
    @age = age
    @@population += 1
  end
end"""
            },
            "Block/Closure": {
                "SmallTalk": """multiplier := [:x | [:y | x * y]].
timesFive := multiplier value: 5.
result := timesFive value: 3.""",
                "Ruby": """multiplier = ->(x) { ->(y) { x * y } }
times_five = multiplier.call(5)
result = times_five.call(3)"""
            }
        }
        
        pattern = st.session_state.load_pattern
        if pattern in pattern_examples:
            code = pattern_examples[pattern][source_lang]
        del st.session_state.load_pattern
    else:
        code = ""
    
    # Code input
    code = st.text_area(
        f"Enter {source_lang} Code",
        value=code,
        height=300,
        placeholder=f"Paste your {source_lang} code here..."
    )
    
    # Convert button
    if st.button(f"🔄 Convert to {target_lang}", type="primary", disabled=not code.strip()):
        with st.spinner(f"Converting to {target_lang}..."):
            result = converter.convert_code(
                code, 
                conv_direction,
                preserve_style=preserve_style,
                add_explanations=add_explanations
            )
            
            if not result["error"]:
                # Display in tabs
                tab1, tab2, tab3, tab4 = st.tabs([
                    f"📝 {target_lang} Code",
                    "🎓 Paradigm Analysis",
                    "📖 Language Guide",
                    "💾 Save"
                ])
                
                with tab1:
                    st.code(
                        result["converted"],
                        language="ruby" if target_lang == "Ruby" else "smalltalk"
                    )
                    
                    # Download button
                    file_ext = "rb" if target_lang == "Ruby" else "st"
                    st.download_button(
                        f"📥 Download {target_lang} Code",
                        data=result["converted"],
                        file_name=f"converted.{file_ext}",
                        mime="text/plain"
                    )
                
                with tab2:
                    with st.spinner("Analyzing paradigm differences..."):
                        explanation = converter.explain_differences(
                            code, result["converted"], conv_direction
                        )
                        st.markdown(explanation)
                    
                    # Key differences summary
                    st.subheader("🔑 Key Conversion Points")
                    
                    if conv_direction == "smalltalk_to_ruby":
                        st.markdown("""
                        | SmallTalk | Ruby | Notes |
                        |-----------|------|-------|
                        | `object message` | `object.message` | Method syntax |
                        | `object message: arg` | `object.message(arg)` | Parameter passing |
                        | `[:x | x * 2]` | `{ |x| x * 2 }` or `lambda` | Blocks |
                        | `^ result` | `return result` | Explicit return |
                        | `ifTrue: [] ifFalse: []` | `if..else..end` | Conditionals |
                        | `#symbol` | `:symbol` | Symbol syntax |
                        """)
                    else:
                        st.markdown("""
                        | Ruby | SmallTalk | Notes |
                        |------|-----------|-------|
                        | `class Person` | `Object subclass: #Person` | Class definition |
                        | `def method` | Method in browser | Method definition |
                        | `@instance_var` | Instance variable | Variable syntax |
                        | `@@class_var` | Class variable | Shared state |
                        | `module Mixin` | Trait or category | Code organization |
                        | `yield` | Block evaluation | Control flow |
                        """)
                
                with tab3:
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.subheader(f"📘 {source_lang} Concepts")
                        if source_lang == "SmallTalk":
                            st.markdown("""
                            **Everything is an Object**
                            - Even classes are objects
                            - Numbers, blocks, nil - all objects
                            - Uniform message passing
                            
                            **Live Environment**
                            - Image-based development
                            - Runtime modification
                            - Inspector and debugger
                            
                            **Pure OOP**
                            - No primitives
                            - Everything via messages
                            - Metaclass hierarchy
                            """)
                        else:
                            st.markdown("""
                            **Multi-paradigm**
                            - OOP with functional features
                            - Modules for mixins
                            - Blocks and lambdas
                            
                            **Duck Typing**
                            - Dynamic typing
                            - Respond to messages
                            - Open classes
                            
                            **Metaprogramming**
                            - define_method
                            - method_missing
                            - Class macros
                            """)
                    
                    with col2:
                        st.subheader(f"📙 {target_lang} Concepts")
                        if target_lang == "Ruby":
                            st.markdown("""
                            **Flexible Syntax**
                            - Optional parentheses
                            - Multiple block styles
                            - Operator overloading
                            
                            **Standard Library**
                            - Rich built-ins
                            - Enumerable module
                            - File and network I/O
                            
                            **Rails Ecosystem**
                            - Web framework
                            - ActiveRecord ORM
                            - Convention over configuration
                            """)
                        else:
                            st.markdown("""
                            **Message Passing**
                            - Unary: `object message`
                            - Binary: `3 + 4`
                            - Keyword: `object at: 1 put: 'x'`
                            
                            **Development Tools**
                            - System Browser
                            - Workspace
                            - Transcript
                            
                            **Collections**
                            - Array, OrderedCollection
                            - Set, Dictionary
                            - Streams
                            """)
                    
                    # Resources
                    st.divider()
                    st.subheader("🔗 Learning Resources")
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.link_button(
                            "SmallTalk-80 Blue Book",
                            "http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf",
                            use_container_width=True
                        )
                    with col2:
                        st.link_button(
                            "Ruby Programming Guide",
                            "https://www.ruby-lang.org/en/documentation/",
                            use_container_width=True
                        )
                
                with tab4:
                    st.subheader("💾 Save Conversion")
                    
                    title = st.text_input(
                        "Title",
                        value=f"{source_lang} to {target_lang}: {code[:30]}..."
                    )
                    
                    notes = st.text_area(
                        "Conversion Notes",
                        placeholder="Add any notes about this conversion..."
                    )
                    
                    tags = st.text_input(
                        "Tags",
                        value=f"conversion, {source_lang.lower()}, {target_lang.lower()}"
                    )
                    
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        save_original = st.checkbox("Save original code", value=True)
                    with col2:
                        save_analysis = st.checkbox("Save paradigm analysis", value=True)
                    
                    if st.button("💾 Save to Library", type="primary"):
                        if db.connected:
                            # Prepare content
                            content = f"## Converted {target_lang} Code\n\n```{target_lang.lower()}\n{result['converted']}\n```"
                            
                            if save_original:
                                content = f"## Original {source_lang} Code\n\n```{source_lang.lower()}\n{code}\n```\n\n" + content
                            
                            if save_analysis and 'explanation' in locals():
                                content += f"\n\n## Paradigm Analysis\n\n{explanation}"
                            
                            # Save with metadata
                            metadata = {
                                "direction": conv_direction,
                                "preserve_style": preserve_style,
                                "add_explanations": add_explanations,
                                "notes": notes
                            }
                            
                            query_id = converter.db.log_query(
                                tool="smalltalk_ruby_converter",
                                model=converter.default_model,
                                prompt=f"{conv_direction}: {code}",
                                response=result["converted"],
                                metadata=metadata
                            )
                            
                            if query_id and title:
                                success = db.save_knowledge_unit(
                                    query_id=query_id,
                                    title=title,
                                    content=content,
                                    category="Code Conversions",
                                    tags=[tag.strip() for tag in tags.split(",")]
                                )
                                if success:
                                    st.success("✅ Conversion saved to library!")
                                    st.balloons()
                        else:
                            st.warning("Database not connected")
            else:
                st.error("Conversion failed. Please check your code and try again.")

# Entry point
if __name__ == "__main__":
    show()
