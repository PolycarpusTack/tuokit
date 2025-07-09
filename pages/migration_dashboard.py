"""
TuoKit Migration Dashboard
Monitor the transition to unified tools
"""
import streamlit as st

# Initialize session state
import json
from pathlib import Path
from datetime import datetime
import pandas as pd

def load_feature_analysis():
    """Load the latest feature analysis"""
    analysis_files = list(Path('.').glob('feature_analysis_*.json'))
    if not analysis_files:
        return None
    
    latest = max(analysis_files, key=lambda p: p.stat().st_mtime)
    with open(latest, 'r') as f:
        return json.load(f)

def main():
    st.set_page_config(page_title="Migration Dashboard", page_icon="📊", layout="wide")
    
    st.title("📊 TuoKit Migration Dashboard")
    st.caption("Monitor the transition to unified tools")
    
    # Load analysis
    analysis = load_feature_analysis()
    if not analysis:
        st.error("No feature analysis found. Run extract_unique_features.py first!")
        return
    
    # Metrics row
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        # Count duplicate functions
        sql_duplicates = len(analysis.get('sql', {}).get('duplicates', {}))
        st.metric("SQL Duplicates", sql_duplicates, delta=f"-{sql_duplicates} potential")
    
    with col2:
        # Count unique features to preserve
        unique_count = 0
        for file_features in analysis.get('sql', {}).get('unique', {}).values():
            unique_count += len(file_features)
        st.metric("Unique Features", unique_count, help="Must be preserved")
    
    with col3:
        # Feature toggle status
        using_next_gen = st.session_state.get('use_next_gen_tools', False)
        st.metric("Next-Gen Tools", "Active" if using_next_gen else "Inactive",
                 delta="Testing" if using_next_gen else None)
    
    with col4:
        # Days since migration started
        migration_start = datetime(2024, 1, 15)  # Set your start date
        days_elapsed = (datetime.now() - migration_start).days
        st.metric("Migration Days", days_elapsed, help="Target: 14 days testing")
    
    # Detailed analysis
    st.subheader("🔍 Detailed Analysis")
    
    tab1, tab2, tab3 = st.tabs(["SQL Tools", "Agent Systems", "Migration Plan"])
    
    with tab1:
        st.write("### SQL Tool Consolidation")
        
        # Show duplicate functions
        duplicates = analysis.get('sql', {}).get('duplicates', {})
        if duplicates:
            st.warning(f"Found {len(duplicates)} duplicate functions across SQL tools:")
            
            dup_data = []
            for func_name, implementations in duplicates.items():
                files = [impl['file'] for impl in implementations]
                dup_data.append({
                    'Function': func_name,
                    'Files': ', '.join(files),
                    'Count': len(files)
                })
            
            df = pd.DataFrame(dup_data)
            st.dataframe(df, use_container_width=True)
        
        # Show unique features
        st.write("### Unique Features to Preserve")
        unique_features = analysis.get('sql', {}).get('unique', {})
        
        for file, features in unique_features.items():
            if features:
                with st.expander(f"{file} ({len(features)} unique features)"):
                    for feat in features:
                        st.code(f"{feat['name']}({', '.join(feat.get('args', [])[1:])})")
                        if feat.get('docstring'):
                            st.caption(feat['docstring'].split('\\n')[0])
    
    with tab2:
        st.write("### Agent System Analysis")
        # Similar analysis for agents
        st.info("Agent analysis implementation pending...")
    
    with tab3:
        st.write("### Migration Checklist")
        
        tasks = [
            {"task": "Run feature extraction", "status": "done" if analysis else "pending"},
            {"task": "Create unified SQL toolkit", "status": "done" if Path("pages/sql_toolkit_next.py").exists() else "pending"},
            {"task": "Add feature toggle", "status": "check"},
            {"task": "Test for 1 week", "status": "in_progress"},
            {"task": "Gather user feedback", "status": "pending"},
            {"task": "Fix reported issues", "status": "pending"},
            {"task": "Archive old files", "status": "pending"},
        ]
        
        for item in tasks:
            if item['status'] == 'done':
                st.checkbox(item['task'], value=True, disabled=True)
            elif item['status'] == 'in_progress':
                st.checkbox(item['task'], value=False, help="Currently in progress")
            else:
                st.checkbox(item['task'], value=False)
    
    # User feedback section
    st.divider()
    st.subheader("📝 User Feedback")
    
    feedback = st.text_area("Report issues or feedback with next-gen tools:")
    if st.button("Submit Feedback"):
        # Save feedback
        feedback_file = f"feedback_{datetime.now().strftime('%Y%m%d_%H%M%S')}.txt"
        with open(feedback_file, 'w') as f:
            f.write(feedback)
        st.success(f"Feedback saved to {feedback_file}")
    
    # Rollback section
    with st.sidebar:
        st.subheader("🚨 Emergency Controls")
        
        if st.button("📋 View Rollback Plan", use_container_width=True):
            rollback_files = list(Path('.').glob('ROLLBACK_PLAN_*.md'))
            if rollback_files:
                latest_rollback = max(rollback_files, key=lambda p: p.stat().st_mtime)
                with open(latest_rollback, 'r') as f:
                    st.code(f.read(), language='markdown')
        
        if st.button("⚠️ Disable Next-Gen Tools", type="secondary", use_container_width=True):
            st.session_state.use_next_gen_tools = False
            st.rerun()

if __name__ == "__main__":
    main()
