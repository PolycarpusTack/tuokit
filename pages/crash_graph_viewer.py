"""
🕸️ Crash Pattern Network - Visualize crash relationships and trends
See how crash patterns relate to each other and evolve over time
"""

import streamlit as st
import networkx as nx
import plotly.graph_objects as go
import pandas as pd
from datetime import datetime, timedelta
import json

# Add parent directory to path
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.database import DatabaseManager

# Page config
st.set_page_config(
    page_title="Crash Pattern Network",
    page_icon="🕸️",
    layout="wide"
)

# Initialize database
db = DatabaseManager()

def load_crash_graph_data():
    """Load crash patterns and relationships"""
    patterns = []
    relationships = []
    
    try:
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                # Load crash patterns as nodes
                cur.execute("""
                    SELECT 
                        id, pattern_name, error_type, cause_of_dump,
                        occurrence_count, confidence_score,
                        first_seen, last_seen,
                        verified_solutions, prevention_strategies
                    FROM crash_patterns
                    ORDER BY occurrence_count DESC
                    LIMIT 200
                """)
                
                for row in cur.fetchall():
                    patterns.append({
                        'id': row[0],
                        'name': row[1],
                        'error_type': row[2],
                        'cause': row[3],
                        'count': row[4],
                        'confidence': row[5],
                        'first_seen': row[6],
                        'last_seen': row[7],
                        'solutions': len(row[8]) if row[8] else 0,
                        'preventions': len(row[9]) if row[9] else 0
                    })
                
                # Load relationships
                cur.execute("""
                    SELECT 
                        source_pattern_id, target_pattern_id, 
                        relationship_type, confidence
                    FROM crash_relationships
                    ORDER BY confidence DESC
                """)
                
                for row in cur.fetchall():
                    relationships.append({
                        'source': row[0],
                        'target': row[1],
                        'type': row[2],
                        'confidence': row[3]
                    })
                
        return patterns, relationships
        
    except Exception as e:
        st.error(f"Error loading crash graph data: {e}")
        return [], []

def calculate_pattern_severity(pattern):
    """Calculate visual severity score for a pattern"""
    # Higher score = more severe
    base_score = pattern['count'] * 0.3  # Frequency
    base_score += pattern['confidence'] * 20  # Confidence in pattern
    
    # Adjust by error type keywords
    if 'critical' in pattern.get('cause', '').lower():
        base_score *= 1.5
    elif 'space' in pattern.get('cause', '').lower():
        base_score *= 1.3
    
    return min(base_score, 100)

def create_crash_network_graph(patterns, relationships):
    """Create interactive Plotly network graph for crash patterns"""
    
    # Create NetworkX graph
    G = nx.DiGraph()
    
    # Add nodes
    for pattern in patterns:
        G.add_node(pattern['id'], **pattern)
    
    # Add edges
    for rel in relationships:
        if rel['source'] in G.nodes and rel['target'] in G.nodes:
            G.add_edge(rel['source'], rel['target'], 
                      type=rel['type'], confidence=rel['confidence'])
    
    # Generate layout
    if len(patterns) < 30:
        pos = nx.spring_layout(G, k=3, iterations=50, seed=42)
    else:
        pos = nx.kamada_kawai_layout(G)
    
    # Create edge traces by type
    edge_colors = {
        'causes': '#ff6b6b',      # Red - causal relationships
        'related_to': '#4dabf7',  # Blue - related patterns
        'evolves_from': '#51cf66', # Green - evolution
        'similar_fix': '#ffd43b'   # Yellow - similar solutions
    }
    
    edge_traces = []
    for edge in G.edges(data=True):
        x0, y0 = pos[edge[0]]
        x1, y1 = pos[edge[1]]
        edge_type = edge[2].get('type', 'related_to')
        confidence = edge[2].get('confidence', 0.5)
        
        edge_trace = go.Scatter(
            x=[x0, x1, None],
            y=[y0, y1, None],
            mode='lines',
            line=dict(
                width=confidence * 4,
                color=edge_colors.get(edge_type, '#999')
            ),
            hoverinfo='text',
            text=f"{edge_type} (confidence: {confidence:.1%})",
            showlegend=False
        )
        edge_traces.append(edge_trace)
    
    # Create node trace
    node_x = []
    node_y = []
    node_text = []
    node_color = []
    node_size = []
    
    for node_id in G.nodes():
        x, y = pos[node_id]
        node_x.append(x)
        node_y.append(y)
        
        node_data = G.nodes[node_id]
        severity = calculate_pattern_severity(node_data)
        
        # Create hover text
        hover_text = f"<b>{node_data['name']}</b><br>"
        hover_text += f"Type: {node_data['error_type']}<br>"
        hover_text += f"Occurrences: {node_data['count']}<br>"
        hover_text += f"Confidence: {node_data['confidence']:.1%}<br>"
        hover_text += f"Solutions: {node_data['solutions']}<br>"
        hover_text += f"First seen: {node_data['first_seen'].strftime('%Y-%m-%d')}<br>"
        hover_text += f"Last seen: {node_data['last_seen'].strftime('%Y-%m-%d')}"
        
        node_text.append(hover_text)
        
        # Color by severity
        if severity > 70:
            color = '#ff6b6b'  # Red - critical
        elif severity > 40:
            color = '#ffd43b'  # Yellow - warning
        else:
            color = '#51cf66'  # Green - low
        node_color.append(color)
        
        # Size by occurrence count
        size = 15 + min(node_data['count'] * 2, 50)
        node_size.append(size)
    
    node_trace = go.Scatter(
        x=node_x,
        y=node_y,
        mode='markers+text',
        text=[G.nodes[node_id]['name'][:20] + '...' if len(G.nodes[node_id]['name']) > 20 
              else G.nodes[node_id]['name'] for node_id in G.nodes()],
        textposition="top center",
        hovertext=node_text,
        hoverinfo='text',
        marker=dict(
            size=node_size,
            color=node_color,
            line=dict(width=2, color='white')
        )
    )
    
    # Create figure
    fig = go.Figure()
    
    # Add edges
    for trace in edge_traces:
        fig.add_trace(trace)
    
    # Add nodes
    fig.add_trace(node_trace)
    
    # Update layout
    fig.update_layout(
        title={
            'text': f"Crash Pattern Network ({len(patterns)} patterns, {len(relationships)} relationships)",
            'x': 0.5,
            'xanchor': 'center'
        },
        showlegend=False,
        hovermode='closest',
        margin=dict(b=0, l=0, r=0, t=40),
        xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
        yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
        height=700,
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
    )
    
    return fig

def show_pattern_evolution(db):
    """Show how patterns have evolved over time"""
    try:
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT 
                        pe.created_at,
                        pe.change_type,
                        pe.change_description,
                        cp.pattern_name,
                        pe.created_by
                    FROM pattern_evolution pe
                    JOIN crash_patterns cp ON pe.pattern_id = cp.id
                    ORDER BY pe.created_at DESC
                    LIMIT 20
                """)
                
                evolutions = cur.fetchall()
                
                if evolutions:
                    st.subheader("📈 Pattern Evolution Timeline")
                    
                    for evolution in evolutions:
                        col1, col2 = st.columns([1, 3])
                        with col1:
                            st.caption(evolution[0].strftime('%Y-%m-%d %H:%M'))
                        with col2:
                            change_emoji = {
                                'new_solution': '💡',
                                'pattern_update': '🔄',
                                'merge': '🔀'
                            }.get(evolution[1], '📝')
                            
                            st.write(f"{change_emoji} **{evolution[3]}** - {evolution[2] or evolution[1]}")
                            st.caption(f"By: {evolution[4]}")
                else:
                    st.info("No pattern evolution recorded yet")
                    
    except Exception as e:
        st.error(f"Error loading evolution data: {e}")

def show_crash_trends(db):
    """Show crash pattern trends over time"""
    try:
        with db.get_connection() as conn:
            # Get crash counts by day
            df = pd.read_sql_query("""
                SELECT 
                    DATE(crash_timestamp) as date,
                    COUNT(*) as crash_count,
                    COUNT(DISTINCT pattern_id) as unique_patterns,
                    AVG(downtime_minutes) as avg_downtime
                FROM crash_instances
                WHERE crash_timestamp > NOW() - INTERVAL '30 days'
                GROUP BY DATE(crash_timestamp)
                ORDER BY date
            """, conn)
            
            if not df.empty:
                st.subheader("📊 30-Day Crash Trends")
                
                # Create tabs for different views
                tab1, tab2, tab3 = st.tabs(["Volume", "Patterns", "Impact"])
                
                with tab1:
                    st.line_chart(df.set_index('date')['crash_count'])
                    st.caption("Daily crash volume")
                
                with tab2:
                    st.line_chart(df.set_index('date')['unique_patterns'])
                    st.caption("Unique patterns per day")
                
                with tab3:
                    st.bar_chart(df.set_index('date')['avg_downtime'])
                    st.caption("Average downtime (minutes)")
            else:
                st.info("No crash data available for trends")
                
    except Exception as e:
        st.warning(f"Could not load trend data: {e}")

def main():
    st.title("🕸️ Crash Pattern Network")
    st.markdown("Visualize how crash patterns relate to each other and evolve over time")
    
    # Load data
    patterns, relationships = load_crash_graph_data()
    
    if not patterns:
        st.warning("No crash patterns found. Start analyzing crashes to build the pattern network!")
        
        # Show instructions
        st.info("""
        ### 🚀 Getting Started
        1. Go to the **Crash Analyzer** page
        2. Upload and analyze crash dumps
        3. Validate and save the analyses
        4. The system will learn patterns automatically
        5. Come back here to see the pattern network!
        """)
        return
    
    # Display options
    col1, col2, col3 = st.columns(3)
    
    with col1:
        min_occurrences = st.slider(
            "Min occurrences",
            1, 50, 5,
            help="Show patterns with at least this many occurrences"
        )
    
    with col2:
        min_confidence = st.slider(
            "Min confidence",
            0.0, 1.0, 0.5,
            help="Show patterns with at least this confidence score"
        )
    
    with col3:
        time_filter = st.selectbox(
            "Time range",
            ["All time", "Last 30 days", "Last 7 days", "Last 24 hours"]
        )
    
    # Filter patterns
    filtered_patterns = []
    for pattern in patterns:
        if pattern['count'] >= min_occurrences and pattern['confidence'] >= min_confidence:
            # Apply time filter
            if time_filter != "All time":
                days = {"Last 30 days": 30, "Last 7 days": 7, "Last 24 hours": 1}[time_filter]
                if pattern['last_seen'] < datetime.now() - timedelta(days=days):
                    continue
            filtered_patterns.append(pattern)
    
    # Show stats
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Total Patterns", len(patterns))
    with col2:
        st.metric("Filtered Patterns", len(filtered_patterns))
    with col3:
        st.metric("Relationships", len(relationships))
    with col4:
        high_conf = sum(1 for p in patterns if p['confidence'] > 0.8)
        st.metric("High Confidence", high_conf)
    
    # Create and display graph
    if filtered_patterns:
        fig = create_crash_network_graph(filtered_patterns, relationships)
        st.plotly_chart(fig, use_container_width=True)
        
        # Additional analytics
        st.divider()
        
        col1, col2 = st.columns(2)
        
        with col1:
            # Most connected patterns
            st.markdown("### 🔗 Most Connected Patterns")
            
            # Count connections
            connection_counts = {}
            for rel in relationships:
                connection_counts[rel['source']] = connection_counts.get(rel['source'], 0) + 1
                connection_counts[rel['target']] = connection_counts.get(rel['target'], 0) + 1
            
            # Get pattern names
            pattern_dict = {p['id']: p for p in patterns}
            
            # Sort by connections
            top_connected = sorted(connection_counts.items(), key=lambda x: x[1], reverse=True)[:5]
            
            for pattern_id, count in top_connected:
                if pattern_id in pattern_dict:
                    pattern = pattern_dict[pattern_id]
                    st.write(f"• **{pattern['name']}** ({count} connections)")
                    st.caption(f"   Occurrences: {pattern['count']}, Confidence: {pattern['confidence']:.1%}")
        
        with col2:
            # Pattern categories
            st.markdown("### 📊 Error Type Distribution")
            
            error_types = {}
            for pattern in filtered_patterns:
                error_type = pattern['error_type'] or 'Unknown'
                error_types[error_type] = error_types.get(error_type, 0) + 1
            
            for error_type, count in sorted(error_types.items(), key=lambda x: x[1], reverse=True):
                percentage = (count / len(filtered_patterns)) * 100
                st.write(f"• **{error_type}**: {count} patterns ({percentage:.1f}%)")
        
        # Evolution timeline
        st.divider()
        show_pattern_evolution(db)
        
        # Trends
        st.divider()
        show_crash_trends(db)
        
        # Export options
        st.divider()
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("📊 Export Pattern Data"):
                export_data = {
                    'patterns': filtered_patterns,
                    'relationships': relationships,
                    'metadata': {
                        'exported_at': datetime.now().isoformat(),
                        'total_patterns': len(filtered_patterns),
                        'total_relationships': len(relationships)
                    }
                }
                st.download_button(
                    "Download JSON",
                    data=json.dumps(export_data, default=str, indent=2),
                    file_name=f"crash_patterns_{datetime.now().strftime('%Y%m%d')}.json",
                    mime="application/json"
                )
        
        with col2:
            if st.button("🔄 Refresh Data"):
                st.rerun()
        
        with col3:
            if st.button("🚨 View Crash Analyzer"):
                st.switch_page("pages/crash_analyzer.py")
    
    else:
        st.info("No patterns match your filter criteria. Try adjusting the filters.")

if __name__ == "__main__":
    main()