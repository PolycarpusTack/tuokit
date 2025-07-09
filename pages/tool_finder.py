"""
🔍 TuoKit Tool Finder - Discover the right tool for your task
"""

import streamlit as st
from utils.navigation import NAVIGATION_CATEGORIES, search_tools, get_all_tools
import random

st.set_page_config(
    page_title="Tool Finder",
    page_icon="🔍",
    layout="wide"
)

def main():
    st.title("🔍 Tool Finder")
    st.markdown("**Find the perfect tool for your task**")
    
    # Tool finder methods
    tab1, tab2, tab3, tab4 = st.tabs(["🎯 By Task", "📚 By Category", "🔤 Alphabetical", "🎲 Discover"])
    
    with tab1:
        st.subheader("What do you want to do?")
        
        # Common tasks
        tasks = {
            "Write code": ["code_tools", "agent_hub"],
            "Fix errors": ["error_tool", "exception_advisor", "crash_analyzer"],
            "Work with SQL": ["sql_toolkit", "sql_academy"],
            "Build Rails app": ["rails_toolkit", "rails_scaffold", "rails_model_gen"],
            "Learn programming": ["edu_mind", "study_guide_generator", "ruby_katas"],
            "Generate tests": ["rspec_generator", "rails_system_tests"],
            "Optimize performance": ["ruby_profiler", "ruby_memory_optimizer"],
            "Work with documentation": ["doc_tools", "knowledge_lib"],
            "Convert code": ["smalltalk_ruby_converter"],
            "Build UI": ["morphic_builder", "view_components"]
        }
        
        selected_task = st.selectbox("Select a task:", list(tasks.keys()))
        
        if selected_task:
            st.write(f"**Recommended tools for: {selected_task}**")
            
            tool_ids = tasks[selected_task]
            
            # Find and display the tools
            all_tools = get_all_tools()
            for tool in all_tools:
                if tool['id'] in tool_ids:
                    col1, col2 = st.columns([3, 1])
                    
                    with col1:
                        st.write(f"**{tool['icon']} {tool['name']}**")
                        st.caption(tool['description'])
                    
                    with col2:
                        if st.button(f"Open", key=f"task_{tool['id']}"):
                            st.switch_page(f"pages/{tool['file']}")
    
    with tab2:
        st.subheader("Browse by Category")
        
        # Category selector
        category_names = list(NAVIGATION_CATEGORIES.keys())
        selected_category = st.selectbox("Select a category:", category_names)
        
        if selected_category:
            category_data = NAVIGATION_CATEGORIES[selected_category]
            st.write(f"**{selected_category}**")
            st.caption(category_data["description"])
            
            st.markdown("---")
            
            # Display tools in grid
            tools = category_data["tools"]
            
            for i in range(0, len(tools), 2):
                cols = st.columns(2)
                
                for j, (tool_id, tool) in enumerate(list(tools.items())[i:i+2]):
                    if j < 2:
                        with cols[j]:
                            with st.container():
                                st.write(f"**{tool['icon']} {tool['name']}**")
                                st.caption(tool['description'])
                                
                                if st.button(f"Open", key=f"cat_{tool_id}"):
                                    st.switch_page(f"pages/{tool['file']}")
                                
                                st.markdown("---")
    
    with tab3:
        st.subheader("All Tools A-Z")
        
        # Get all tools and sort alphabetically
        all_tools = get_all_tools()
        all_tools.sort(key=lambda x: x['name'])
        
        # Filter by first letter
        letters = sorted(set(tool['name'][0].upper() for tool in all_tools))
        selected_letter = st.select_slider("Jump to letter:", letters)
        
        # Display tools
        for tool in all_tools:
            if tool['name'][0].upper() == selected_letter:
                col1, col2, col3 = st.columns([2, 2, 1])
                
                with col1:
                    st.write(f"**{tool['icon']} {tool['name']}**")
                
                with col2:
                    st.caption(f"{tool['description']} ({tool['category']})")
                
                with col3:
                    if st.button("Open", key=f"alpha_{tool['id']}"):
                        st.switch_page(f"pages/{tool['file']}")
    
    with tab4:
        st.subheader("🎲 Discover Random Tools")
        st.write("Explore tools you might not have tried yet!")
        
        # Get 5 random tools
        all_tools = get_all_tools()
        random_tools = random.sample(all_tools, min(5, len(all_tools)))
        
        for tool in random_tools:
            with st.expander(f"{tool['icon']} {tool['name']}", expanded=True):
                st.write(f"**Category**: {tool['category']}")
                st.write(f"**Description**: {tool['description']}")
                
                col1, col2 = st.columns([1, 1])
                
                with col1:
                    if st.button(f"Try it!", key=f"random_{tool['id']}"):
                        st.switch_page(f"pages/{tool['file']}")
                
                with col2:
                    if st.button(f"🎲 Another", key=f"refresh_{tool['id']}"):
                        st.rerun()
        
        if st.button("🎲 Refresh All", type="primary"):
            st.rerun()

if __name__ == "__main__":
    main()
