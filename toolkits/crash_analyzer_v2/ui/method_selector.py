"""
Method Selection UI for Crash Analyzer V2
Provides intelligent recommendations based on file characteristics
"""
import streamlit as st
from typing import Dict, Any, Optional

from ..config import ANALYSIS_METHODS

def show_method_selector(file_size: int, has_model: bool, file_metadata: Dict[str, Any]) -> Optional[str]:
    """
    Display method selection with recommendations
    
    Args:
        file_size: Size of the file in bytes
        has_model: Whether an AI model is available
        file_metadata: Additional file characteristics
        
    Returns:
        Selected analysis method key or None
    """
    st.markdown("### 🎯 Select Analysis Method")
    
    # Calculate recommendations
    recommendations = _get_recommendations(file_size, has_model, file_metadata)
    
    # Show recommendation banner
    if recommendations["primary"]:
        primary = ANALYSIS_METHODS[recommendations["primary"]]
        st.info(f"💡 **Recommended**: {primary['name']} - {recommendations['reason']}")
    
    # Method cards
    selected_method = None
    cols = st.columns(2)
    
    for i, (method_key, method_config) in enumerate(ANALYSIS_METHODS.items()):
        col = cols[i % 2]
        
        with col:
            # Check if method is available
            is_available = True
            availability_reason = ""
            
            if method_key != "quick_triage" and not has_model:
                is_available = False
                availability_reason = "Requires AI model"
            
            # All methods now implemented!
            
            # Create method card
            with st.container():
                # Card styling based on recommendation
                if method_key == recommendations["primary"]:
                    card_style = "border: 2px solid #4CAF50; background-color: #f1f8f4;"
                elif not is_available:
                    card_style = "border: 1px solid #ccc; background-color: #f5f5f5; opacity: 0.6;"
                else:
                    card_style = "border: 1px solid #ddd; background-color: #fff;"
                
                # Method card content
                st.markdown(f"""
                <div style="padding: 15px; border-radius: 8px; {card_style}">
                    <h4>{method_config['name']}</h4>
                    <p style="color: #666; font-size: 14px;">{method_config['description']}</p>
                </div>
                """, unsafe_allow_html=True)
                
                # Features
                with st.expander("Features", expanded=False):
                    for feature in method_config['features']:
                        st.write(f"• {feature}")
                
                # Processing time estimate
                time_estimate = _estimate_processing_time(method_key, file_size)
                st.caption(f"⏱️ Estimated time: {time_estimate}")
                
                # Recommendation badge
                if method_key == recommendations["primary"]:
                    st.success("✨ Recommended for your file")
                elif method_key in recommendations["alternatives"]:
                    st.info("✓ Good alternative")
                
                # Select button
                if is_available:
                    if st.button(
                        f"Select {method_config['name']}", 
                        key=f"select_{method_key}",
                        use_container_width=True,
                        type="primary" if method_key == recommendations["primary"] else "secondary"
                    ):
                        selected_method = method_key
                else:
                    st.button(
                        availability_reason,
                        key=f"select_{method_key}",
                        use_container_width=True,
                        disabled=True
                    )
    
    # Show comparison table
    with st.expander("📊 Method Comparison", expanded=False):
        _show_comparison_table()
    
    return selected_method

def _get_recommendations(file_size: int, has_model: bool, metadata: Dict[str, Any]) -> Dict[str, Any]:
    """
    Get method recommendations based on file characteristics
    
    Returns:
        Dictionary with primary recommendation, alternatives, and reasons
    """
    size_mb = file_size / (1024 * 1024)
    is_production = metadata.get("is_production", False)
    has_stack_trace = metadata.get("has_stack_trace", False)
    error_density = metadata.get("error_density", 0)
    
    # Emergency scenarios
    if is_production and error_density > 10 and size_mb < 0.1:
        return {
            "primary": "quick_triage",
            "alternatives": ["root_cause"],
            "reason": "Production emergency - need immediate assessment"
        }
    
    # Small files with AI available
    if size_mb < 0.5 and has_model and has_stack_trace:
        return {
            "primary": "root_cause",
            "alternatives": ["quick_triage"],
            "reason": "Small file with stack traces - perfect for AI analysis"
        }
    
    # Small files without AI
    if size_mb < 0.5 and not has_model:
        return {
            "primary": "quick_triage",
            "alternatives": [],
            "reason": "AI not available - algorithmic analysis recommended"
        }
    
    # Medium files
    if 0.5 <= size_mb < 2 and has_model:
        return {
            "primary": "root_cause",
            "alternatives": ["strategic_sampling"],
            "reason": "Medium-sized file - comprehensive analysis feasible"
        }
    
    # Large files
    if size_mb >= 2:
        return {
            "primary": "strategic_sampling",
            "alternatives": ["deep_forensic"],
            "reason": "Large file - smart sampling recommended for speed"
        }
    
    # Default
    return {
        "primary": "quick_triage" if not has_model else "root_cause",
        "alternatives": [],
        "reason": "Default recommendation based on available resources"
    }

def _estimate_processing_time(method: str, file_size: int) -> str:
    """Estimate processing time for a method based on file size"""
    size_mb = file_size / (1024 * 1024)
    
    estimates = {
        "quick_triage": "5-10 seconds",
        "root_cause": f"{int(30 + size_mb * 10)}-{int(60 + size_mb * 20)} seconds",
        "strategic_sampling": f"{int(60 + size_mb * 5)}-{int(120 + size_mb * 10)} seconds",
        "deep_forensic": f"{int(5 + size_mb * 2)}-{int(10 + size_mb * 5)} minutes"
    }
    
    return estimates.get(method, "Unknown")

def _show_comparison_table():
    """Show detailed comparison table of methods"""
    import pandas as pd
    
    comparison_data = {
        "Method": ["Quick Triage", "Root Cause", "Strategic Sampling", "Deep Forensic"],
        "Speed": ["5/5", "3/5", "2/5", "1/5"],
        "Depth": ["1/5", "3/5", "2/5", "5/5"],
        "AI Required": ["No", "Yes", "Yes", "Yes"],
        "Best For": [
            "Emergencies",
            "Standard cases", 
            "Large files",
            "Complete analysis"
        ],
        "Output": [
            "Basic triage",
            "Full report",
            "Key insights", 
            "Everything"
        ]
    }
    
    df = pd.DataFrame(comparison_data)
    st.dataframe(df, use_container_width=True, hide_index=True)