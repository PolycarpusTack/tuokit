"""
🌐 Visual Knowledge Graph - See Your Knowledge Network
Interactive visualization of knowledge relationships
"""

import streamlit as st
import networkx as nx
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import pandas as pd
from datetime import datetime

# Add parent directory to path
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from utils.database import DatabaseManager
from utils.knowledge_relationships import RelationshipManager, RELATIONSHIP_TYPES

# Page config
st.set_page_config(
    page_title="Knowledge Graph Viewer",
    page_icon="🌐",
    layout="wide"
)

# Initialize database
db = DatabaseManager()

def load_knowledge_graph_data():
    """Load knowledge units and relationships from database"""
    nodes = []
    edges = []
    
    try:
        with db.get_connection() as conn:
            with conn.cursor() as cur:
                # Load knowledge units as nodes
                cur.execute("""
                    SELECT id, title, category, quality_score, usage_count, 
                           tool_specific_data, created_at, tags
                    FROM knowledge_units
                    WHERE quality_score >= 30
                    ORDER BY quality_score DESC
                    LIMIT 200
                """)
                
                for row in cur.fetchall():
                    # Extract tool name from metadata if available
                    tool_name = "Unknown"
                    if row[5] and isinstance(row[5], dict):
                        # tool_specific_data might contain the tool info
                        tool_name = row[5].get('tool', row[5].get('tool_name', 'Unknown'))
                    
                    nodes.append({
                        'id': row[0],
                        'title': row[1],
                        'category': row[2],
                        'quality_score': row[3],
                        'usage_count': row[4] or 0,
                        'tool': tool_name,
                        'created_at': row[6],
                        'tags': row[7] or []
                    })
                
                # Load relationships as edges
                cur.execute("""
                    SELECT source_id, target_id, relationship_type, strength
                    FROM knowledge_links
                    ORDER BY strength DESC
                """)
                
                for row in cur.fetchall():
                    edges.append({
                        'source': row[0],
                        'target': row[1],
                        'type': row[2],
                        'strength': row[3]
                    })
                
        return nodes, edges
        
    except Exception as e:
        st.error(f"Error loading graph data: {e}")
        return [], []

def create_interactive_graph(nodes, edges, highlight_node=None):
    """Create interactive Plotly network graph"""
    
    # Create NetworkX graph
    G = nx.Graph()
    
    # Add nodes
    for node in nodes:
        G.add_node(node['id'], **node)
    
    # Add edges
    edge_traces = {rel_type: [] for rel_type in RELATIONSHIP_TYPES.keys()}
    
    for edge in edges:
        G.add_edge(edge['source'], edge['target'], 
                  type=edge['type'], strength=edge['strength'])
    
    # Generate layout
    if len(nodes) < 50:
        pos = nx.spring_layout(G, k=2, iterations=50, seed=42)
    else:
        pos = nx.kamada_kawai_layout(G)
    
    # Create edge traces by type
    edge_colors = {
        'related': '#74c0fc',      # Blue
        'prerequisite': '#ffd43b',  # Yellow
        'solution': '#8ce99a'       # Green
    }
    
    for edge in G.edges(data=True):
        x0, y0 = pos[edge[0]]
        x1, y1 = pos[edge[1]]
        edge_type = edge[2].get('type', 'related')
        strength = edge[2].get('strength', 0.5)
        
        edge_trace = go.Scatter(
            x=[x0, x1, None],
            y=[y0, y1, None],
            mode='lines',
            line=dict(
                width=strength * 3,  # Width based on strength
                color=edge_colors.get(edge_type, '#999')
            ),
            hoverinfo='text',
            text=f"{RELATIONSHIP_TYPES[edge_type]['name']} ({strength:.0%})",
            showlegend=False
        )
        edge_traces[edge_type].append(edge_trace)
    
    # Create node trace
    node_x = []
    node_y = []
    node_text = []
    node_color = []
    node_size = []
    
    category_colors = {
        'code_snippet': '#4dabf7',     # Blue
        'algorithm': '#845ef7',        # Purple
        'error_solution': '#ff6b6b',   # Red
        'tutorial': '#51cf66',         # Green
        'explanation': '#ffd43b',      # Yellow
        'best_practice': '#ff922b'     # Orange
    }
    
    for node_id in G.nodes():
        x, y = pos[node_id]
        node_x.append(x)
        node_y.append(y)
        
        node_data = G.nodes[node_id]
        
        # Create hover text
        hover_text = f"<b>{node_data['title']}</b><br>"
        hover_text += f"Category: {node_data['category']}<br>"
        hover_text += f"Quality: {node_data['quality_score']}/100<br>"
        hover_text += f"Used: {node_data['usage_count']} times<br>"
        hover_text += f"Tool: {node_data['tool']}<br>"
        if node_data['tags']:
            hover_text += f"Tags: {', '.join(node_data['tags'][:5])}"
        
        node_text.append(hover_text)
        
        # Node color by category
        color = category_colors.get(node_data['category'], '#868e96')
        if highlight_node and node_id == highlight_node:
            color = '#e03131'  # Highlight color
        node_color.append(color)
        
        # Node size by quality + usage
        size = 10 + (node_data['quality_score'] / 5) + (node_data['usage_count'] * 2)
        node_size.append(min(size, 50))  # Cap at 50
    
    node_trace = go.Scatter(
        x=node_x,
        y=node_y,
        mode='markers+text',
        text=[G.nodes[node_id]['title'][:20] + '...' if len(G.nodes[node_id]['title']) > 20 
              else G.nodes[node_id]['title'] for node_id in G.nodes()],
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
    for edge_type, traces in edge_traces.items():
        for trace in traces:
            fig.add_trace(trace)
    
    # Add nodes
    fig.add_trace(node_trace)
    
    # Update layout
    fig.update_layout(
        title={
            'text': f"Knowledge Graph ({len(nodes)} nodes, {len(edges)} relationships)",
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

def show_graph_stats(nodes, edges):
    """Display graph statistics"""
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric("Knowledge Units", len(nodes))
    
    with col2:
        st.metric("Relationships", len(edges))
    
    with col3:
        # Calculate average connections
        if nodes:
            connections = {}
            for edge in edges:
                connections[edge['source']] = connections.get(edge['source'], 0) + 1
                connections[edge['target']] = connections.get(edge['target'], 0) + 1
            avg_connections = sum(connections.values()) / len(nodes) if nodes else 0
            st.metric("Avg Connections", f"{avg_connections:.1f}")
    
    with col4:
        # Relationship breakdown
        rel_types = {}
        for edge in edges:
            rel_types[edge['type']] = rel_types.get(edge['type'], 0) + 1
        
        breakdown = ", ".join([f"{RELATIONSHIP_TYPES[k]['name']}: {v}" 
                             for k, v in rel_types.items()])
        st.metric("Types", breakdown if breakdown else "None")

def main():
    st.title("🌐 Visual Knowledge Graph")
    st.markdown("Explore your interconnected knowledge network")
    
    # Load data
    nodes, edges = load_knowledge_graph_data()
    
    if not nodes:
        st.warning("No knowledge units found. Start capturing knowledge to build your graph!")
        return
    
    # Show stats
    show_graph_stats(nodes, edges)
    
    # Display options
    col1, col2, col3 = st.columns([2, 2, 1])
    
    with col1:
        # Filter by category
        categories = list(set(node['category'] for node in nodes))
        selected_categories = st.multiselect(
            "Filter by category",
            categories,
            default=categories[:3] if len(categories) > 3 else categories
        )
    
    with col2:
        # Filter by relationship type
        rel_types = list(set(edge['type'] for edge in edges))
        selected_rel_types = st.multiselect(
            "Show relationships",
            rel_types,
            default=rel_types,
            format_func=lambda x: RELATIONSHIP_TYPES[x]['name']
        )
    
    with col3:
        # Highlight specific node
        node_titles = [f"{node['id']}: {node['title'][:30]}..." for node in nodes[:20]]
        if st.checkbox("Highlight node"):
            selected_node = st.selectbox("Select node", node_titles)
            highlight_id = int(selected_node.split(":")[0])
        else:
            highlight_id = None
    
    # Filter data
    filtered_nodes = [n for n in nodes if n['category'] in selected_categories]
    filtered_edges = [e for e in edges if e['type'] in selected_rel_types and
                     e['source'] in [n['id'] for n in filtered_nodes] and
                     e['target'] in [n['id'] for n in filtered_nodes]]
    
    # Create and display graph
    if filtered_nodes:
        fig = create_interactive_graph(filtered_nodes, filtered_edges, highlight_id)
        st.plotly_chart(fig, use_container_width=True)
        
        # Analysis section
        st.divider()
        st.subheader("📊 Graph Analysis")
        
        col1, col2 = st.columns(2)
        
        with col1:
            # Most connected nodes
            st.markdown("**🔗 Most Connected Knowledge**")
            connections = {}
            for edge in filtered_edges:
                connections[edge['source']] = connections.get(edge['source'], 0) + 1
                connections[edge['target']] = connections.get(edge['target'], 0) + 1
            
            top_connected = sorted(connections.items(), key=lambda x: x[1], reverse=True)[:5]
            for node_id, count in top_connected:
                node = next((n for n in nodes if n['id'] == node_id), None)
                if node:
                    st.write(f"• **{node['title']}** ({count} connections)")
        
        with col2:
            # Knowledge clusters
            st.markdown("**🎯 Knowledge Clusters**")
            category_counts = {}
            for node in filtered_nodes:
                category_counts[node['category']] = category_counts.get(node['category'], 0) + 1
            
            for category, count in sorted(category_counts.items(), key=lambda x: x[1], reverse=True):
                st.write(f"• **{category}**: {count} units")
        
        # Export options
        st.divider()
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("📊 Export Graph Data"):
                # Create export data
                export_data = {
                    'nodes': filtered_nodes,
                    'edges': filtered_edges,
                    'metadata': {
                        'exported_at': datetime.now().isoformat(),
                        'total_nodes': len(filtered_nodes),
                        'total_edges': len(filtered_edges)
                    }
                }
                st.download_button(
                    "Download JSON",
                    data=pd.DataFrame(export_data).to_json(),
                    file_name=f"knowledge_graph_{datetime.now().strftime('%Y%m%d')}.json",
                    mime="application/json"
                )
        
        with col2:
            if st.button("🔄 Refresh Data"):
                st.rerun()
        
        with col3:
            if st.button("🎯 Find Learning Paths"):
                st.info("Learning path finder coming soon!")
    
    else:
        st.info("No data matches your filters. Try adjusting the category or relationship filters.")

if __name__ == "__main__":
    main()