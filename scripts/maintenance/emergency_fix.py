"""
Emergency fix for TuoKit startup issues
Temporarily disables problematic imports
"""

import os
import shutil
from pathlib import Path

def create_minimal_knowledge_graph():
    """Create a minimal knowledge_graph.py without matplotlib"""
    content = '''# Minimal knowledge graph implementation
class KnowledgeGraph:
    """Placeholder implementation without matplotlib dependency"""
    def __init__(self):
        self.graph = {}
        
    def visualize_graph(self, highlight_concepts=None):
        """Return None instead of matplotlib figure"""
        return None
        
    def get_concepts(self):
        """Return empty list"""
        return []
        
    def add_concept(self, concept):
        """Placeholder"""
        pass

# Create instance
knowledge_graph = KnowledgeGraph()
'''
    
    kg_path = Path("C:/Projects/Tuokit/utils/knowledge_graph_minimal.py")
    kg_path.write_text(content, encoding='utf-8')
    
    # Backup original
    orig_path = Path("C:/Projects/Tuokit/utils/knowledge_graph.py")
    backup_path = Path("C:/Projects/Tuokit/utils/knowledge_graph_original.py")
    
    if orig_path.exists() and not backup_path.exists():
        shutil.copy(str(orig_path), str(backup_path))
        print("Backed up original knowledge_graph.py")
    
    # Replace with minimal version
    shutil.copy(str(kg_path), str(orig_path))
    print("Replaced with minimal knowledge_graph.py (no matplotlib)")
    
    return True

def restore_original():
    """Restore original knowledge_graph.py"""
    backup_path = Path("C:/Projects/Tuokit/utils/knowledge_graph_original.py")
    orig_path = Path("C:/Projects/Tuokit/utils/knowledge_graph.py")
    
    if backup_path.exists():
        shutil.copy(str(backup_path), str(orig_path))
        print("Restored original knowledge_graph.py")
        return True
    return False

if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1 and sys.argv[1] == "restore":
        restore_original()
    else:
        create_minimal_knowledge_graph()
        print("\nNow try running: streamlit run app.py")
