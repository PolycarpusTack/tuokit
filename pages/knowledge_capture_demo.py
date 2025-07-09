"""
Knowledge Capture System Demo
Shows the unified knowledge capture system in action
"""

import streamlit as st
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.tool_base import TuoKitToolBase

class KnowledgeCaptureDemo(TuoKitToolBase):
    """Demo tool showcasing the Knowledge Capture System"""
    
    def __init__(self):
        super().__init__(
            tool_name="Knowledge Capture Demo",
            tool_description="Demonstrates automatic knowledge capture and search"
        )

def main():
    st.set_page_config(
        page_title="Knowledge Capture Demo",
        page_icon="🧠",
        layout="wide"
    )
    
    # Initialize the demo tool
    demo = KnowledgeCaptureDemo()
    
    st.title("🧠 Knowledge Capture System Demo")
    st.markdown("Experience TuoKit's automatic knowledge capture in real-time!")
    
    # Show system status
    col1, col2, col3 = st.columns(3)
    
    with col1:
        if demo.knowledge_capture_enabled:
            st.success("✅ Knowledge Capture: Enabled")
        else:
            st.error("❌ Knowledge Capture: Disabled")
    
    with col2:
        if st.session_state.get('db'):
            st.success("✅ Database: Connected")
        else:
            st.error("❌ Database: Not Connected")
    
    with col3:
        if st.session_state.get('capture_manager'):
            st.success("✅ Capture Manager: Ready")
        else:
            st.error("❌ Capture Manager: Not Available")
    
    st.divider()
    
    # Knowledge Search Widget
    demo.show_knowledge_search_widget()
    
    # Demo Scenarios
    st.subheader("🎯 Try These Demo Scenarios")
    
    scenario_col1, scenario_col2 = st.columns(2)
    
    with scenario_col1:
        st.markdown("**📝 High-Quality Content (Will be Captured)**")
        demo_scenarios = [
            "Explain the difference between lists and tuples in Python with examples",
            "Write a SQL query to find the top 5 customers by order count",
            "How do I fix 'ImportError: No module named pandas' in Python?",
            "Implement a binary search algorithm in Python with comments",
            "Explain Rails migrations and provide a practical example"
        ]
        
        selected_scenario = st.selectbox(
            "Choose a demo prompt:",
            demo_scenarios,
            key="high_quality"
        )
        
        if st.button("🚀 Generate Response", key="generate_high"):
            with st.spinner("Generating response and capturing knowledge..."):
                result = demo.generate_with_capture(
                    prompt=selected_scenario,
                    options={"temperature": 0.3}
                )
                
                if not result['error']:
                    st.success("Response Generated!")
                    
                    # Show the response
                    with st.expander("📄 AI Response", expanded=True):
                        st.write(result['response'])
                    
                    # Show capture status
                    if result.get('knowledge_id'):
                        st.success(f"💡 Knowledge captured! ID: {result['knowledge_id']}")
                        st.balloons()
                        
                        # Show related knowledge widget
                        demo.show_related_knowledge_widget(result['knowledge_id'])
                    else:
                        st.info("💭 Content quality didn't meet capture threshold")
                        
                else:
                    st.error(f"Error: {result['response']}")
    
    with scenario_col2:
        st.markdown("**🚫 Low-Quality Content (Will be Rejected)**")
        
        low_quality_scenarios = [
            "What is Python?",  # Too short
            "Yes",  # Definitely too short
            "Use sorted()",  # Not helpful enough
            "Good question",  # No value
            "I don't know"  # Unhelpful
        ]
        
        selected_low = st.selectbox(
            "Choose a low-quality prompt:",
            low_quality_scenarios,
            key="low_quality"
        )
        
        if st.button("🔄 Test Quality Filter", key="generate_low"):
            with st.spinner("Testing quality filters..."):
                # Simulate a short, low-quality response
                short_response = "Python is a programming language."
                
                # Test with the quality gate directly
                from utils.knowledge_capture import QualityGate
                gate = QualityGate()
                is_valid, reason, metrics = gate.validate(short_response, {"tool": "demo"})
                
                st.info("Quality Check Results:")
                st.write(f"**Content**: {short_response}")
                st.write(f"**Valid**: {is_valid}")
                st.write(f"**Reason**: {reason}")
                
                if metrics:
                    st.write(f"**Quality Score**: {metrics.overall_score}/100")
                
                if not is_valid:
                    st.success("✅ Quality gate working! Low-quality content rejected.")
                else:
                    st.warning("⚠️ Content passed quality check")
    
    st.divider()
    
    # Show Enhanced Metrics
    st.subheader("📊 System Metrics")
    demo.show_enhanced_metrics()
    
    # Knowledge Status
    st.subheader("💡 Capture Status")
    demo.show_knowledge_status_indicator()
    
    # Real-time Statistics
    if st.session_state.get('capture_manager'):
        with st.expander("📈 Real-time Knowledge Statistics", expanded=False):
            metrics = st.session_state.capture_manager.get_metrics()
            
            if metrics:
                col1, col2, col3 = st.columns(3)
                
                with col1:
                    st.metric("Total Knowledge Units", metrics.get('total_units', 0))
                    
                with col2:
                    categories = metrics.get('by_category', {})
                    st.metric("Categories", len(categories))
                    
                with col3:
                    st.metric("24h Captures", metrics.get('captures_24h', 0))
                
                # Category breakdown
                if categories:
                    st.subheader("📚 Knowledge by Category")
                    for category, count in categories.items():
                        st.progress(count / max(categories.values()), text=f"{category}: {count}")
            else:
                st.info("No knowledge captured yet. Try the demo scenarios above!")
    
    # Instructions
    with st.expander("ℹ️ How It Works", expanded=False):
        st.markdown("""
        ### 🔄 Automatic Knowledge Capture Process
        
        1. **AI Generation**: When you request AI assistance, the response is generated
        2. **Quality Check**: Content is evaluated for length, complexity, and value
        3. **Categorization**: High-quality content is automatically categorized
        4. **Tag Extraction**: Relevant tags are extracted (languages, frameworks, etc.)
        5. **Storage**: Knowledge is stored with metadata for easy retrieval
        6. **Search**: You can search all captured knowledge across tools
        
        ### 🎯 Quality Standards
        
        **Content is captured if it:**
        - Is at least 50 characters long
        - Contains explanations or examples
        - Has specific technical content
        - Scores 30+ on quality metrics
        - Isn't a duplicate of existing knowledge
        
        **Content is rejected if it:**
        - Too short or generic
        - Low information density
        - Duplicate of existing knowledge
        - Poor quality score
        
        ### 🔍 Search Features
        
        - Search by keywords, categories, or tags
        - Quality-filtered results (30+ score)
        - Usage tracking (popular knowledge surfaces first)
        - Cross-tool knowledge discovery
        """)
    
    # Footer
    st.divider()
    st.caption("🧠 Knowledge Capture System - Building institutional memory automatically")

if __name__ == "__main__":
    main()