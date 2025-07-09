"""
Demo: Enhanced Data Analysis with PostgreSQL Knowledge Storage
Shows how DuckDB analytics results are saved to PostgreSQL
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import streamlit as st

# Use the modular agent hub toolkit
from toolkits.agent_hub import AnalysisAgent
from data_analysis_enhanced import DataAnalysisAgentEnhanced, query_analytics_history
from utils import DatabaseManager

def demo_data_analysis_with_storage():
    """Demonstrate data analysis with PostgreSQL storage"""
    
    # Create sample dataset
    np.random.seed(42)
    dates = pd.date_range('2024-01-01', periods=1000, freq='D')
    
    df = pd.DataFrame({
        'date': dates,
        'product': np.random.choice(['A', 'B', 'C', 'D'], 1000),
        'region': np.random.choice(['North', 'South', 'East', 'West'], 1000),
        'sales': np.random.normal(1000, 300, 1000).clip(0),
        'quantity': np.random.poisson(50, 1000),
        'customer_satisfaction': np.random.uniform(1, 5, 1000),
        'returned': np.random.choice([True, False], 1000, p=[0.1, 0.9])
    })
    
    # Add some missing values
    df.loc[np.random.choice(df.index, 50), 'customer_satisfaction'] = np.nan
    
    print("=" * 60)
    print("Enhanced Data Analysis Demo")
    print("=" * 60)
    
    # Initialize agent
    agent = DataAnalysisAgentEnhanced()
    
    # 1. Comprehensive Analysis
    print("\n1. Running comprehensive analysis...")
    results = agent.analyze_dataframe_enhanced(df, "sales_demo_2024")
    
    print(f"✅ Analysis complete!")
    print(f"   - Data Quality Score: {results['analyses']['profile']['quality_score']:.1f}%")
    print(f"   - Issues Found: {results['analyses']['quality']['total_issues']}")
    print(f"   - Patterns Discovered: {len(results['analyses']['patterns']['correlations'])}")
    print(f"   - Insights Generated: {len(results['analyses']['insights'])}")
    
    # 2. Natural Language Queries
    print("\n2. Testing natural language queries...")
    
    queries = [
        "What is the total sales by region?",
        "Show me average sales by product for each month",
        "Which products have the highest return rate?",
        "Find the correlation between sales and customer satisfaction"
    ]
    
    for query in queries:
        print(f"\n   Query: '{query}'")
        result = agent.natural_language_query_enhanced(
            query, 
            dict(df.dtypes),
            "sales_demo_2024"
        )
        
        if result['success']:
            print(f"   ✅ Success! Returned {result['row_count']} rows")
            print(f"   SQL: {result['query']}")
        else:
            print(f"   ❌ Failed: {result['error']}")
    
    # 3. Query saved analytics
    print("\n3. Querying saved analytics from PostgreSQL...")
    
    db = DatabaseManager()
    if db and db.connected:
        # Get recent profiles
        profiles = query_analytics_history(db, "profiles")
        print(f"\n   Data Profiles Saved: {len(profiles)}")
        if len(profiles) > 0:
            print(f"   Latest: {profiles.iloc[0]['profile_name']} "
                  f"(Quality: {profiles.iloc[0]['quality_score']:.1f}%)")
        
        # Get recent queries
        queries = query_analytics_history(db, "queries")
        successful = queries[queries['success'] == True] if 'success' in queries else queries
        print(f"\n   Analytics Queries Saved: {len(queries)}")
        print(f"   Success Rate: {len(successful)/len(queries)*100:.1f}%" if len(queries) > 0 else "N/A")
        
        # Get insights
        insights = query_analytics_history(db, "insights")
        print(f"\n   Insights Discovered: {len(insights)}")
        if len(insights) > 0:
            print("   Top Insights by Confidence:")
            for _, insight in insights.head(3).iterrows():
                print(f"   - {insight['insight_description'][:60]}... "
                      f"(Confidence: {insight['confidence_score']:.2f})")
        
        # Get patterns
        patterns = query_analytics_history(db, "patterns")
        print(f"\n   Patterns Found: {len(patterns)}")
        
        db.close()
    
    print("\n" + "=" * 60)
    print("Summary: All analytics are automatically saved to PostgreSQL!")
    print("This builds a searchable knowledge base of data insights.")
    print("=" * 60)

def show_analytics_dashboard():
    """Streamlit dashboard for viewing saved analytics"""
    st.title("📊 Analytics Knowledge Base")
    st.caption("View all saved analytics from PostgreSQL")
    
    db = DatabaseManager()
    if not db or not db.connected:
        st.error("Database not connected")
        return
    
    tab1, tab2, tab3, tab4 = st.tabs(["📋 Profiles", "🔍 Queries", "💡 Insights", "🔄 Patterns"])
    
    with tab1:
        st.subheader("Data Profiles")
        profiles = query_analytics_history(db, "profiles")
        if not profiles.empty:
            st.dataframe(profiles)
            
            # Quality score distribution
            fig = px.histogram(profiles, x='quality_score', 
                             title='Data Quality Score Distribution')
            st.plotly_chart(fig)
        else:
            st.info("No profiles saved yet")
    
    with tab2:
        st.subheader("Natural Language Queries")
        queries = query_analytics_history(db, "queries")
        if not queries.empty:
            # Success rate metric
            success_rate = (queries['success'].sum() / len(queries) * 100) if 'success' in queries else 0
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Total Queries", len(queries))
            with col2:
                st.metric("Success Rate", f"{success_rate:.1f}%")
            with col3:
                avg_time = queries['execution_time_ms'].mean() if 'execution_time_ms' in queries else 0
                st.metric("Avg Execution Time", f"{avg_time:.0f}ms")
            
            # Recent queries
            st.dataframe(queries[['natural_language_query', 'row_count', 'success', 'created_at']])
        else:
            st.info("No queries saved yet")
    
    with tab3:
        st.subheader("Data Insights")
        insights = query_analytics_history(db, "insights")
        if not insights.empty:
            # Group by type
            insight_types = insights['insight_type'].value_counts()
            
            col1, col2 = st.columns(2)
            with col1:
                st.metric("Total Insights", len(insights))
                
                # Insight types
                fig = px.pie(values=insight_types.values, 
                           names=insight_types.index,
                           title='Insights by Type')
                st.plotly_chart(fig)
            
            with col2:
                # High confidence insights
                high_conf = insights[insights['confidence_score'] >= 0.8]
                st.metric("High Confidence Insights", len(high_conf))
                
                # Recent insights
                st.markdown("#### Recent High-Value Insights")
                for _, insight in insights.nlargest(5, 'confidence_score').iterrows():
                    st.write(f"**{insight['dataset_name']}** - {insight['insight_description']}")
                    st.caption(f"Confidence: {insight['confidence_score']:.2f}")
        else:
            st.info("No insights saved yet")
    
    with tab4:
        st.subheader("Discovered Patterns")
        patterns = query_analytics_history(db, "patterns")
        if not patterns.empty:
            # Pattern types
            pattern_types = patterns['pattern_type'].value_counts()
            
            st.metric("Total Patterns", len(patterns))
            
            # Pattern distribution
            fig = px.bar(x=pattern_types.index, y=pattern_types.values,
                        title='Patterns by Type')
            st.plotly_chart(fig)
            
            # Recent patterns
            st.markdown("#### Recent Patterns")
            st.dataframe(patterns[['dataset_name', 'pattern_type', 'pattern_description', 'created_at']])
        else:
            st.info("No patterns saved yet")
    
    db.close()

if __name__ == "__main__":
    # Run demo
    demo_data_analysis_with_storage()
    
    # To see the dashboard, run:
    # streamlit run demo_data_analysis_storage.py
    # And uncomment the line below:
    # show_analytics_dashboard()
