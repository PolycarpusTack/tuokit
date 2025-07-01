# utils/knowledge_graph.py
import streamlit as st
import networkx as nx
import matplotlib.pyplot as plt
from io import BytesIO
import json
from utils.database import DatabaseManager

# Initialize database
db = DatabaseManager()

class KnowledgeGraph:
    def __init__(self):
        self.graph = nx.DiGraph()
        self.load_concepts()
    
    def load_concepts(self):
        """Load concepts from database or default set"""
        concepts = db.get_collection("knowledge_graph")
        if not concepts:
            concepts = self.default_concepts()
        self.build_graph(concepts)
    
    def default_concepts(self):
        """Core concepts for TuoKit"""
        return [
            {
                "id": "sql",
                "name": "SQL",
                "type": "concept",
                "description": "Structured Query Language for databases",
                "resources": [
                    {"title": "SQL Tutorial", "url": "https://www.w3schools.com/sql/"},
                    {"title": "SQL Style Guide", "url": "https://www.sqlstyle.guide/"}
                ],
                "prerequisites": [],
                "related": ["database", "query-optimization"]
            },
            {
                "id": "query-optimization",
                "name": "Query Optimization",
                "type": "concept",
                "description": "Techniques to improve database query performance",
                "resources": [
                    {"title": "Optimization Strategies", "url": "https://use-the-index-luke.com/"},
                    {"title": "EXPLAIN Plans", "url": "https://www.postgresqltutorial.com/postgresql-explain/"}
                ],
                "prerequisites": ["sql"],
                "related": ["indexing", "query-execution"]
            },
            {
                "id": "database",
                "name": "Database Systems",
                "type": "concept",
                "description": "Fundamentals of database management systems",
                "resources": [
                    {"title": "Database Design", "url": "https://www.guru99.com/database-design.html"},
                    {"title": "ACID Properties", "url": "https://www.ibm.com/topics/acid-database"}
                ],
                "prerequisites": [],
                "related": ["sql", "indexing"]
            },
            {
                "id": "indexing",
                "name": "Database Indexing",
                "type": "concept",
                "description": "Data structures to improve data retrieval speed",
                "resources": [
                    {"title": "Indexing Explained", "url": "https://www.geeksforgeeks.org/indexing-in-databases/"},
                    {"title": "Index Types", "url": "https://www.postgresql.org/docs/current/indexes-types.html"}
                ],
                "prerequisites": ["database"],
                "related": ["query-optimization"]
            },
            {
                "id": "joins",
                "name": "SQL Joins",
                "type": "concept",
                "description": "Combining rows from multiple tables based on related columns",
                "resources": [
                    {"title": "Visual JOIN Guide", "url": "https://www.sql-join.com/"},
                    {"title": "JOIN Types Explained", "url": "https://www.w3schools.com/sql/sql_join.asp"}
                ],
                "prerequisites": ["sql"],
                "related": ["query-optimization", "database"]
            },
            {
                "id": "aggregation",
                "name": "Aggregation Functions",
                "type": "concept",
                "description": "Functions that perform calculations on sets of values",
                "resources": [
                    {"title": "Aggregate Functions", "url": "https://www.w3schools.com/sql/sql_aggregate_functions.asp"},
                    {"title": "GROUP BY Guide", "url": "https://www.postgresqltutorial.com/postgresql-group-by/"}
                ],
                "prerequisites": ["sql"],
                "related": ["group-by", "having"]
            },
            {
                "id": "filtering",
                "name": "Data Filtering",
                "type": "concept",
                "description": "Using WHERE clauses to filter query results",
                "resources": [
                    {"title": "WHERE Clause Tutorial", "url": "https://www.w3schools.com/sql/sql_where.asp"},
                    {"title": "Advanced Filtering", "url": "https://www.postgresqltutorial.com/postgresql-where/"}
                ],
                "prerequisites": ["sql"],
                "related": ["query-optimization"]
            },
            {
                "id": "group-by",
                "name": "GROUP BY Clause",
                "type": "concept",
                "description": "Grouping rows that have the same values in specified columns",
                "resources": [
                    {"title": "GROUP BY Explained", "url": "https://www.w3schools.com/sql/sql_groupby.asp"},
                    {"title": "Grouping Sets", "url": "https://www.postgresqltutorial.com/postgresql-grouping-sets/"}
                ],
                "prerequisites": ["sql", "aggregation"],
                "related": ["having", "aggregation"]
            },
            {
                "id": "having",
                "name": "HAVING Clause",
                "type": "concept",
                "description": "Filtering groups based on aggregate conditions",
                "resources": [
                    {"title": "HAVING vs WHERE", "url": "https://www.w3schools.com/sql/sql_having.asp"},
                    {"title": "HAVING Examples", "url": "https://www.postgresqltutorial.com/postgresql-having/"}
                ],
                "prerequisites": ["group-by", "aggregation"],
                "related": ["group-by", "filtering"]
            },
            {
                "id": "subqueries",
                "name": "Subqueries",
                "type": "concept",
                "description": "Queries nested inside other queries",
                "resources": [
                    {"title": "Subquery Basics", "url": "https://www.w3schools.com/sql/sql_subqueries.asp"},
                    {"title": "Correlated Subqueries", "url": "https://www.postgresqltutorial.com/postgresql-subquery/"}
                ],
                "prerequisites": ["sql", "filtering"],
                "related": ["cte", "query-optimization"]
            },
            {
                "id": "cte",
                "name": "Common Table Expressions",
                "type": "concept",
                "description": "Named temporary result sets within a query",
                "resources": [
                    {"title": "CTE Tutorial", "url": "https://www.postgresqltutorial.com/postgresql-cte/"},
                    {"title": "Recursive CTEs", "url": "https://www.postgresqltutorial.com/postgresql-recursive-cte/"}
                ],
                "prerequisites": ["sql", "subqueries"],
                "related": ["subqueries", "window-functions"]
            },
            {
                "id": "window-functions",
                "name": "Window Functions",
                "type": "concept",
                "description": "Functions that perform calculations across a set of rows",
                "resources": [
                    {"title": "Window Functions Guide", "url": "https://www.postgresqltutorial.com/postgresql-window-function/"},
                    {"title": "Advanced Window Functions", "url": "https://use-the-index-luke.com/sql/window-functions"}
                ],
                "prerequisites": ["sql", "aggregation"],
                "related": ["cte", "ranking"]
            },
            {
                "id": "ranking",
                "name": "Ranking Functions",
                "type": "concept",
                "description": "ROW_NUMBER, RANK, DENSE_RANK for ordering results",
                "resources": [
                    {"title": "Ranking Functions", "url": "https://www.postgresqltutorial.com/postgresql-window-function/postgresql-rank/"},
                    {"title": "ROW_NUMBER vs RANK", "url": "https://www.sqltutorial.org/sql-window-functions/sql-row_number/"}
                ],
                "prerequisites": ["window-functions"],
                "related": ["window-functions", "ordering"]
            },
            {
                "id": "ordering",
                "name": "ORDER BY Clause",
                "type": "concept",
                "description": "Sorting query results in ascending or descending order",
                "resources": [
                    {"title": "ORDER BY Tutorial", "url": "https://www.w3schools.com/sql/sql_orderby.asp"},
                    {"title": "Multi-column Sorting", "url": "https://www.postgresqltutorial.com/postgresql-order-by/"}
                ],
                "prerequisites": ["sql"],
                "related": ["ranking", "filtering"]
            },
            {
                "id": "query-execution",
                "name": "Query Execution Plans",
                "type": "concept",
                "description": "Understanding how databases execute queries",
                "resources": [
                    {"title": "EXPLAIN ANALYZE", "url": "https://www.postgresqltutorial.com/postgresql-explain/"},
                    {"title": "Query Plan Reading", "url": "https://use-the-index-luke.com/sql/explain-plan"}
                ],
                "prerequisites": ["query-optimization"],
                "related": ["query-optimization", "indexing"]
            }
        ]
    
    def build_graph(self, concepts):
        """Build network graph from concepts"""
        for concept in concepts:
            self.graph.add_node(concept["id"], **concept)
            for related in concept.get("related", []):
                self.graph.add_edge(concept["id"], related)
            for prereq in concept.get("prerequisites", []):
                self.graph.add_edge(prereq, concept["id"])
    
    def get_concept(self, concept_id):
        """Get concept details by ID"""
        return self.graph.nodes.get(concept_id, {})
    
    def get_related_concepts(self, concept_id, relationship="related"):
        """Get related concepts with optional filter"""
        neighbors = []
        for neighbor in self.graph.neighbors(concept_id):
            node = self.graph.nodes.get(neighbor, {})
            if relationship == "all":
                neighbors.append(node)
            elif relationship == "prerequisite":
                if concept_id in node.get("prerequisites", []):
                    neighbors.append(node)
            else:  # related
                if node.get("id") in self.graph.nodes[concept_id].get("related", []):
                    neighbors.append(node)
        return neighbors
    
    def visualize_graph(self, highlight_concepts=None):
        """Create visual representation of knowledge graph"""
        plt.figure(figsize=(12, 8))
        pos = nx.spring_layout(self.graph, seed=42, k=2, iterations=50)
        
        # Draw nodes with different colors
        node_colors = []
        for node in self.graph.nodes:
            if highlight_concepts and node in highlight_concepts:
                node_colors.append("#FF6B6B")  # Highlighted nodes
            elif self.graph.nodes[node].get("type") == "tool":
                node_colors.append("#4DABF7")  # Tool nodes
            else:
                node_colors.append("#51CF66")  # Concept nodes
        
        # Draw nodes
        nx.draw_networkx_nodes(
            self.graph, pos, 
            node_size=2000, 
            node_color=node_colors,
            alpha=0.8
        )
        
        # Draw edges with different styles
        prerequisite_edges = []
        related_edges = []
        
        for source, target in self.graph.edges():
            if target in self.graph.nodes[source].get("prerequisites", []):
                prerequisite_edges.append((source, target))
            else:
                related_edges.append((source, target))
        
        # Draw prerequisite edges (solid)
        nx.draw_networkx_edges(
            self.graph, pos, 
            edgelist=prerequisite_edges,
            width=2.0, 
            arrowstyle='->', 
            arrowsize=20,
            edge_color='#495057'
        )
        
        # Draw related edges (dashed)
        nx.draw_networkx_edges(
            self.graph, pos, 
            edgelist=related_edges,
            width=1.5, 
            arrowstyle='->', 
            arrowsize=15,
            style='dashed',
            edge_color='#ADB5BD'
        )
        
        # Draw labels
        labels = {node: self.graph.nodes[node].get("name", node) for node in self.graph.nodes}
        nx.draw_networkx_labels(
            self.graph, pos, labels, 
            font_size=9,
            font_weight='bold'
        )
        
        plt.title("SQL Knowledge Graph", fontsize=16, fontweight='bold')
        plt.axis('off')
        plt.tight_layout()
        
        # Convert to PNG for Streamlit
        buf = BytesIO()
        plt.savefig(buf, format="png", dpi=150, bbox_inches='tight')
        plt.close()
        buf.seek(0)
        return buf
    
    def recommend_learning_path(self, start_concept, target_concept):
        """Generate learning path between concepts"""
        try:
            path = nx.shortest_path(self.graph, start_concept, target_concept)
            return [self.graph.nodes[node] for node in path]
        except nx.NetworkXNoPath:
            return []
    
    def get_concepts_by_difficulty(self, difficulty):
        """Get all concepts of a specific difficulty level"""
        concepts = []
        for node_id, node_data in self.graph.nodes.items():
            if node_data.get("difficulty", "").lower() == difficulty.lower():
                concepts.append(node_data)
        return concepts
    
    def get_prerequisite_tree(self, concept_id):
        """Get all prerequisites for a concept recursively"""
        prerequisites = []
        
        def get_prereqs_recursive(node_id, visited=None):
            if visited is None:
                visited = set()
            
            if node_id in visited:
                return
            
            visited.add(node_id)
            node = self.graph.nodes.get(node_id, {})
            
            for prereq in node.get("prerequisites", []):
                if prereq not in visited:
                    prereq_node = self.graph.nodes.get(prereq, {})
                    if prereq_node:
                        prerequisites.append(prereq_node)
                        get_prereqs_recursive(prereq, visited)
        
        get_prereqs_recursive(concept_id)
        return prerequisites
    
    def save_concept(self, concept):
        """Save concept to knowledge base"""
        db.save_to_collection("knowledge_graph", concept)
        self.build_graph([concept] + list(self.graph.nodes.values()))
    
    def detect_concepts_in_query(self, sql_query):
        """Detect which concepts are present in a SQL query"""
        detected = []
        
        # Define patterns for concept detection
        patterns = {
            "sql": r'\bSELECT\b',
            "joins": r'\b(INNER\s+)?JOIN\b|\bLEFT\s+JOIN\b|\bRIGHT\s+JOIN\b|\bFULL\s+JOIN\b',
            "filtering": r'\bWHERE\b',
            "group-by": r'\bGROUP\s+BY\b',
            "ordering": r'\bORDER\s+BY\b',
            "having": r'\bHAVING\b',
            "aggregation": r'\b(COUNT|SUM|AVG|MAX|MIN)\s*\(',
            "subqueries": r'\(\s*SELECT\b',
            "window-functions": r'\bOVER\s*\(',
            "cte": r'\bWITH\b.*\bAS\s*\(',
            "ranking": r'\b(ROW_NUMBER|RANK|DENSE_RANK)\s*\(',
        }
        
        import re
        for concept_id, pattern in patterns.items():
            if re.search(pattern, sql_query, re.IGNORECASE):
                if concept_id in self.graph.nodes:
                    detected.append(concept_id)
        
        return detected

# Initialize singleton instance
knowledge_graph = KnowledgeGraph()
