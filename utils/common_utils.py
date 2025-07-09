"""
Common utility functions for TuoKit
"""

import os
import mimetypes
from pathlib import Path
from typing import Optional, List, Dict, Any
import streamlit as st

# Optional imports for file handling
try:
    import PyPDF2
    HAS_PDF = True
except ImportError:
    HAS_PDF = False

try:
    import docx
    HAS_DOCX = True
except ImportError:
    HAS_DOCX = False


def extract_text_from_file(file_path: str) -> Optional[str]:
    """
    Extract text content from various file types
    
    Args:
        file_path: Path to the file
        
    Returns:
        Extracted text content or None if extraction fails
    """
    if not os.path.exists(file_path):
        return None
    
    file_ext = Path(file_path).suffix.lower()
    
    try:
        if file_ext == '.txt':
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
                
        elif file_ext == '.pdf' and HAS_PDF:
            with open(file_path, 'rb') as f:
                pdf_reader = PyPDF2.PdfReader(f)
                text = ""
                for page in pdf_reader.pages:
                    text += page.extract_text()
                return text
                
        elif file_ext in ['.docx', '.doc'] and HAS_DOCX:
            doc = docx.Document(file_path)
            return '\n'.join([paragraph.text for paragraph in doc.paragraphs])
            
        elif file_ext in ['.py', '.js', '.java', '.cpp', '.c', '.cs', '.rb', '.go', '.rs', '.php']:
            # Code files
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
                
        else:
            # Try to read as text
            with open(file_path, 'r', encoding='utf-8') as f:
                return f.read()
                
    except Exception as e:
        print(f"Error extracting text from {file_path}: {e}")
        return None


def get_recent_items(item_type: str = "queries", limit: int = 10) -> List[Dict[str, Any]]:
    """
    Get recent items from the database
    
    Args:
        item_type: Type of items to retrieve ("queries" or "knowledge")
        limit: Maximum number of items to return
        
    Returns:
        List of recent items as dictionaries
    """
    try:
        from . import DatabaseManager
        db = DatabaseManager()
        
        if item_type == "queries":
            results = db.execute_query("""
                SELECT id, tool, model, user_prompt, created_at
                FROM queries
                ORDER BY created_at DESC
                LIMIT %s
            """, (limit,))
            
        elif item_type == "knowledge":
            results = db.execute_query("""
                SELECT k.id, k.title, k.category, k.content, k.created_at,
                       q.tool, q.model
                FROM knowledge_units k
                LEFT JOIN queries q ON k.query_id = q.id
                ORDER BY k.created_at DESC
                LIMIT %s
            """, (limit,))
        else:
            results = []
            
        return results if results else []
        
    except Exception as e:
        print(f"Error getting recent {item_type}: {e}")
        return []


def save_query(tool: str, prompt: str, response: str, model: str = None) -> Optional[int]:
    """
    Save a query to the database
    
    Args:
        tool: Name of the tool
        prompt: User prompt
        response: AI response
        model: Model used (optional, will use session state if not provided)
        
    Returns:
        Query ID if successful, None otherwise
    """
    try:
        from . import DatabaseManager
        db = DatabaseManager()
        
        if model is None and hasattr(st.session_state, 'selected_model'):
            model = st.session_state.selected_model
        elif model is None:
            model = "unknown"
            
        return db.log_query(tool, model, prompt, response)
        
    except Exception as e:
        print(f"Error saving query: {e}")
        return None


def save_knowledge(query_id: int, title: str, content: str, category: str) -> bool:
    """
    Save knowledge to the database
    
    Args:
        query_id: Related query ID
        title: Knowledge title
        content: Knowledge content
        category: Knowledge category
        
    Returns:
        True if successful, False otherwise
    """
    try:
        from . import DatabaseManager
        db = DatabaseManager()
        
        return db.save_knowledge_unit(query_id, title, content, category)
        
    except Exception as e:
        print(f"Error saving knowledge: {e}")
        return False


def search_knowledge(query: str, category: Optional[str] = None) -> List[Dict[str, Any]]:
    """
    Search knowledge base
    
    Args:
        query: Search query
        category: Optional category filter
        
    Returns:
        List of matching knowledge items
    """
    try:
        from . import DatabaseManager
        db = DatabaseManager()
        
        if category:
            results = db.execute_query("""
                SELECT id, title, content, category, created_at
                FROM knowledge_units
                WHERE (title ILIKE %s OR content ILIKE %s) AND category = %s
                ORDER BY created_at DESC
                LIMIT 20
            """, (f'%{query}%', f'%{query}%', category))
        else:
            results = db.execute_query("""
                SELECT id, title, content, category, created_at
                FROM knowledge_units
                WHERE title ILIKE %s OR content ILIKE %s
                ORDER BY created_at DESC
                LIMIT 20
            """, (f'%{query}%', f'%{query}%'))
            
        return results if results else []
        
    except Exception as e:
        print(f"Error searching knowledge: {e}")
        return []