# toolkits/smalltalk/ui_components.py
"""UI components for SmallTalk toolkit interface"""

import streamlit as st
from .config import (
    TOOL_CATEGORIES, SNIPPET_CATEGORIES, REFACTORING_TECHNIQUES,
    DETAIL_LEVELS, CONVERSION_DIRECTIONS, SEASIDE_TYPES, COMPLEXITY_LEVELS
)
from .generators import (
    generate_class, generate_snippet, generate_seaside_component,
    generate_metaprogramming
)
from .processors import explain_code, refactor_code, convert_code, extract_class_info

def show_class_generator(toolkit, db):
    """Show class generator interface"""
    st.subheader("🏗️ SmallTalk Class Generator")
    
    col1, col2 = st.columns([2, 1])
    
    with col1:
        description = st.text_area(
            "Class Description",
            placeholder="e.g., A shopping cart that manages items, calculates totals, and handles discounts",
            height=100
        )
    
    with col2:
        include_tests = st.checkbox("Include SUnit Tests", value=True)
        include_examples = st.checkbox("Include Usage Examples", value=True)
    
    # Advanced options
    with st.expander("Advanced Options"):
        st.write("**Design Patterns**")
        patterns = st.multiselect(
            "Apply patterns",
            ["Singleton", "Observer", "Factory", "Composite", "Strategy"]
        )
        
        st.write("**Framework Integration**")
        frameworks = st.multiselect(
            "Integrate with",
            ["Seaside", "Glorp ORM", "Morphic UI", "Zinc HTTP"]
        )
    
    if st.button("Generate Class", type="primary", use_container_width=True):
        if description:
            with st.spinner("Generating SmallTalk class..."):
                # Add patterns and frameworks to description
                full_desc = description
                if patterns:
                    full_desc += f"\nApply patterns: {', '.join(patterns)}"
                if frameworks:
                    full_desc += f"\nIntegrate with: {', '.join(frameworks)}"
                
                result = generate_class(
                    description=full_desc,
                    include_tests=include_tests,
                    include_examples=include_examples,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    st.code(result['response'], language="smalltalk")
                    
                    # Extract class info
                    class_info = extract_class_info(result['response'])
                    
                    # Show class structure
                    if class_info['classes']:
                        st.subheader("📊 Generated Structure")
                        for cls in class_info['classes']:
                            st.write(f"**{cls['name']}** < {cls['superclass']}")
                        
                        if class_info['methods']:
                            st.write("**Methods:**")
                            for method in class_info['methods']:
                                st.write(f"- {method['class']} >> {method['selector']}")
                    
                    # Save to knowledge base
                    if db and st.button("💾 Save to Knowledge Base"):
                        toolkit.save_to_knowledge_base(
                            db, description, result['response'], 
                            {"include_tests": include_tests, "patterns": patterns}
                        )
                        st.success("Saved to knowledge base!")
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning("Please provide a class description")

def show_code_explainer(toolkit, db):
    """Show code explainer interface"""
    st.subheader("📖 SmallTalk Code Explainer")
    
    code = st.text_area(
        "SmallTalk Code to Explain",
        height=300,
        placeholder="""Object subclass: #Point
    instanceVariableNames: 'x y'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Kernel-BasicObjects'

Point >> + aPoint
    "Answer a new Point that is the sum of the receiver and aPoint."
    ^(x + aPoint x) @ (y + aPoint y)"""
    )
    
    col1, col2 = st.columns(2)
    with col1:
        detail_level = st.select_slider(
            "Detail Level",
            options=DETAIL_LEVELS,
            value="Detailed"
        )
    
    with col2:
        focus_areas = st.multiselect(
            "Focus Areas",
            ["Class Structure", "Methods", "Design Patterns", "Performance", "Best Practices"]
        )
    
    if st.button("Explain Code", type="primary", use_container_width=True):
        if code:
            with st.spinner("Analyzing SmallTalk code..."):
                result = explain_code(
                    code=code,
                    detail_level=detail_level,
                    focus_areas=focus_areas,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    st.markdown(result['response'])
                    
                    # Log to database
                    if db:
                        toolkit.log_to_database(db, code, result['response'])
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning("Please provide code to explain")

def show_snippet_generator(toolkit, db):
    """Show snippet generator interface"""
    st.subheader("✨ SmallTalk Snippet Generator")
    
    col1, col2 = st.columns(2)
    
    with col1:
        category = st.selectbox(
            "Category",
            options=list(SNIPPET_CATEGORIES.keys())
        )
        
        if category in SNIPPET_CATEGORIES:
            st.info(f"{SNIPPET_CATEGORIES[category]['icon']} {SNIPPET_CATEGORIES[category]['description']}")
    
    with col2:
        complexity = st.select_slider(
            "Complexity",
            options=COMPLEXITY_LEVELS,
            value="Intermediate"
        )
    
    specific_task = st.text_input(
        "Specific Task (optional)",
        placeholder="e.g., Sort a collection by multiple criteria"
    )
    
    # Show subcategories if available
    if category in SNIPPET_CATEGORIES and 'subcategories' in SNIPPET_CATEGORIES[category]:
        subcategory = st.radio(
            "Subcategory",
            SNIPPET_CATEGORIES[category]['subcategories'],
            horizontal=True
        )
    
    if st.button("Generate Snippet", type="primary", use_container_width=True):
        with st.spinner("Generating SmallTalk snippet..."):
            task = specific_task if specific_task else f"{category} example"
            
            result = generate_snippet(
                category=category,
                complexity=complexity,
                specific_task=task,
                model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
            )
            
            if 'error' not in result:
                st.code(result['response'], language="smalltalk")
                
                # Copy to clipboard button
                st.download_button(
                    "📋 Download Snippet",
                    data=result['response'],
                    file_name=f"smalltalk_{category.lower().replace(' ', '_')}_snippet.st",
                    mime="text/plain"
                )
                
                # Save to knowledge base
                if db:
                    toolkit.save_to_knowledge_base(
                        db, f"{category}: {task}", result['response'],
                        {"complexity": complexity}
                    )
            else:
                st.error(f"Error: {result['response']}")

def show_refactorer(toolkit, db):
    """Show code refactorer interface"""
    st.subheader("🔧 SmallTalk Code Refactorer")
    
    code = st.text_area(
        "Code to Refactor",
        height=200,
        placeholder="Paste your SmallTalk code here..."
    )
    
    technique = st.selectbox(
        "Refactoring Technique",
        options=list(REFACTORING_TECHNIQUES.keys())
    )
    
    if technique in REFACTORING_TECHNIQUES:
        st.info(f"**{technique}**: {REFACTORING_TECHNIQUES[technique]['description']}")
        st.caption(f"Example: {REFACTORING_TECHNIQUES[technique]['example']}")
    
    preserve_behavior = st.checkbox("Preserve exact behavior", value=True)
    
    if st.button("Refactor Code", type="primary", use_container_width=True):
        if code:
            with st.spinner(f"Applying {technique}..."):
                result = refactor_code(
                    code=code,
                    technique=technique,
                    preserve_behavior=preserve_behavior,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    st.markdown("### Refactored Code")
                    st.code(result['response'], language="smalltalk")
                    
                    # Log to database
                    if db:
                        toolkit.log_to_database(db, code, result['response'])
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning("Please provide code to refactor")

def show_ruby_converter(toolkit, db):
    """Show Ruby-SmallTalk converter interface"""
    st.subheader("💎 Ruby ↔️ SmallTalk Converter")
    
    direction = st.radio(
        "Conversion Direction",
        CONVERSION_DIRECTIONS,
        horizontal=True
    )
    
    source_lang = direction.split(" to ")[0]
    
    code = st.text_area(
        f"{source_lang} Code",
        height=250,
        placeholder=f"Paste your {source_lang} code here..."
    )
    
    col1, col2 = st.columns(2)
    with col1:
        preserve_style = st.checkbox("Preserve coding style", value=True)
    with col2:
        add_comments = st.checkbox("Add conversion comments", value=True)
    
    if st.button("Convert Code", type="primary", use_container_width=True):
        if code:
            with st.spinner(f"Converting {direction}..."):
                result = convert_code(
                    code=code,
                    direction=direction,
                    preserve_style=preserve_style,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    target_lang = direction.split(" to ")[1]
                    st.markdown(f"### {target_lang} Code")
                    st.code(result['response'], language=target_lang.lower())
                    
                    # Download button
                    st.download_button(
                        f"📥 Download {target_lang} Code",
                        data=result['response'],
                        file_name=f"converted.{target_lang.lower()[:2]}",
                        mime="text/plain"
                    )
                    
                    # Log to database
                    if db:
                        toolkit.log_to_database(db, code, result['response'])
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning(f"Please provide {source_lang} code to convert")

def show_seaside_generator(toolkit, db):
    """Show Seaside component generator interface"""
    st.subheader("🌊 Seaside Component Generator")
    
    description = st.text_area(
        "Component Description",
        placeholder="e.g., A user registration form with validation and email confirmation",
        height=100
    )
    
    col1, col2 = st.columns(2)
    
    with col1:
        component_type = st.selectbox(
            "Component Type",
            SEASIDE_TYPES
        )
        include_css = st.checkbox("Include CSS styling", value=True)
    
    with col2:
        ajax_enabled = st.checkbox("AJAX-enabled", value=False)
        include_tests = st.checkbox("Include tests", value=True)
    
    # Advanced Seaside options
    with st.expander("Advanced Options"):
        decorations = st.multiselect(
            "Decorations",
            ["Authentication", "Caching", "Error Handling", "Logging"]
        )
        
        features = st.multiselect(
            "Features",
            ["Form Validation", "File Upload", "Progress Bar", "Modal Dialogs"]
        )
    
    if st.button("Generate Component", type="primary", use_container_width=True):
        if description:
            with st.spinner("Generating Seaside component..."):
                # Enhance description with options
                full_desc = description
                if ajax_enabled:
                    full_desc += "\nInclude AJAX functionality"
                if decorations:
                    full_desc += f"\nAdd decorations: {', '.join(decorations)}"
                if features:
                    full_desc += f"\nInclude features: {', '.join(features)}"
                
                result = generate_seaside_component(
                    description=full_desc,
                    component_type=component_type,
                    include_css=include_css,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    st.code(result['response'], language="smalltalk")
                    
                    # Save to knowledge base
                    if db and st.button("💾 Save Component"):
                        toolkit.save_to_knowledge_base(
                            db, description, result['response'],
                            {"type": component_type, "ajax": ajax_enabled}
                        )
                        st.success("Component saved!")
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning("Please provide a component description")

def show_metaprogramming(toolkit, db):
    """Show metaprogramming interface"""
    st.subheader("🧙 SmallTalk Metaprogramming")
    
    task = st.text_area(
        "Metaprogramming Task",
        placeholder="e.g., Create a trait that adds logging to all methods of a class",
        height=100
    )
    
    col1, col2 = st.columns(2)
    
    with col1:
        input_type = st.radio(
            "Input Type",
            ["No code needed", "Existing code", "Target class name"]
        )
    
    with col2:
        if input_type == "Target class name":
            target_class = st.text_input("Target Class", placeholder="e.g., MyDomainObject")
        else:
            target_class = ""
    
    if input_type == "Existing code":
        code = st.text_area(
            "Existing Code",
            height=200,
            placeholder="Paste the code to enhance with metaprogramming..."
        )
    else:
        code = ""
    
    # Metaprogramming techniques
    with st.expander("Common Techniques"):
        st.markdown("""
        - **Method Wrappers**: Intercept method calls
        - **Dynamic Methods**: Generate methods at runtime
        - **Class Building**: Create classes programmatically
        - **Reflection**: Inspect and modify objects
        - **Proxies**: Transparent object wrappers
        """)
    
    if st.button("Generate Solution", type="primary", use_container_width=True):
        if task:
            with st.spinner("Crafting metaprogramming solution..."):
                result = generate_metaprogramming(
                    code=code,
                    task=task,
                    target_class=target_class,
                    model=st.session_state.get("selected_model", "deepseek-coder:6.7b")
                )
                
                if 'error' not in result:
                    st.code(result['response'], language="smalltalk")
                    
                    # Warning about metaprogramming
                    st.warning("⚠️ Metaprogramming is powerful but can make code harder to understand. Use judiciously!")
                    
                    # Log to database
                    if db:
                        toolkit.log_to_database(db, task, result['response'])
                else:
                    st.error(f"Error: {result['response']}")
        else:
            st.warning("Please describe the metaprogramming task")