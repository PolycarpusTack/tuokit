"""
Knowledge capture utilities for TuoKit
Standardized patterns for capturing and organizing AI-generated insights
"""

from typing import Dict, List, Optional, Any
from datetime import datetime
import json
import re
from .database import DatabaseManager

class KnowledgePattern:
    """Represents a reusable pattern extracted from AI interactions"""
    
    def __init__(self, title: str, content: str, category: str):
        self.title = title
        self.content = content  
        self.category = category
        self.tags: List[str] = []
        self.metadata: Dict[str, Any] = {}
        self.created_at = datetime.now()
        
    def add_tags(self, tags: List[str]) -> 'KnowledgePattern':
        """Add tags for better searchability"""
        self.tags.extend(tags)
        return self
        
    def add_metadata(self, key: str, value: Any) -> 'KnowledgePattern':
        """Add metadata like performance metrics, model used, etc."""
        self.metadata[key] = value
        return self
        
    def to_dict(self) -> Dict:
        """Convert to dictionary for storage"""
        return {
            "title": self.title,
            "content": self.content,
            "category": self.category,
            "tags": self.tags,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat()
        }

class KnowledgeExtractor:
    """Extracts reusable patterns from AI responses"""
    
    # Standard categories for knowledge
    CATEGORIES = {
        "code": "Code Patterns",
        "sql": "SQL Patterns", 
        "error": "Error Solutions",
        "architecture": "Architecture Patterns",
        "optimization": "Optimization Techniques",
        "security": "Security Practices",
        "documentation": "Documentation Templates"
    }
    
    def __init__(self):
        self.db = DatabaseManager()
        
    def extract_patterns(self, tool: str, prompt: str, response: str) -> List[KnowledgePattern]:
        """Extract reusable patterns from an AI interaction"""
        patterns = []
        
        # Tool-specific extraction
        if tool == "code_explainer":
            patterns.extend(self._extract_code_patterns(prompt, response))
        elif tool == "sql_generator":
            patterns.extend(self._extract_sql_patterns(prompt, response))
        elif tool == "error_decoder":
            patterns.extend(self._extract_error_patterns(prompt, response))
        elif tool == "regex_generator":
            patterns.extend(self._extract_regex_patterns(prompt, response))
            
        return patterns    
    def _extract_code_patterns(self, prompt: str, response: str) -> List[KnowledgePattern]:
        """Extract patterns from code explanations"""
        patterns = []
        
        # Look for design patterns mentioned
        design_patterns = re.findall(r'(singleton|factory|observer|strategy|decorator)\s+pattern', 
                                    response.lower())
        for pattern_name in set(design_patterns):
            pattern = KnowledgePattern(
                title=f"{pattern_name.title()} Pattern Usage",
                content=response,
                category="architecture"
            ).add_tags([pattern_name, "design_pattern", "code"])
            patterns.append(pattern)
            
        # Extract security warnings
        if any(word in response.lower() for word in ["vulnerability", "injection", "unsafe"]):
            pattern = KnowledgePattern(
                title="Security Issue Identified",
                content=response,
                category="security"
            ).add_tags(["security", "vulnerability", "code_review"])
            patterns.append(pattern)
            
        return patterns
    
    def _extract_sql_patterns(self, prompt: str, response: str) -> List[KnowledgePattern]:
        """Extract patterns from SQL generation"""
        patterns = []
        
        # Extract JOIN patterns
        if "JOIN" in response.upper():
            join_types = re.findall(r'(INNER|LEFT|RIGHT|FULL|CROSS)\s+JOIN', response.upper())
            if join_types:
                pattern = KnowledgePattern(
                    title=f"SQL Join Pattern: {', '.join(set(join_types))}",
                    content=response,
                    category="sql"
                ).add_tags(["sql", "join", "query_pattern"])
                patterns.append(pattern)
                
        # Window functions
        if any(func in response.upper() for func in ["ROW_NUMBER", "RANK", "LAG", "LEAD"]):
            pattern = KnowledgePattern(
                title="SQL Window Function Usage",
                content=response,
                category="sql"
            ).add_tags(["sql", "window_function", "analytics"])
            patterns.append(pattern)
            
        return patterns
    
    def _extract_error_patterns(self, prompt: str, response: str) -> List[KnowledgePattern]:
        """Extract patterns from error solutions"""
        patterns = []
        
        # Common error types
        error_types = {
            "TypeError": ["type_error", "python", "debugging"],
            "NullPointerException": ["null_pointer", "java", "debugging"],
            "undefined": ["undefined_error", "javascript", "debugging"],
            "SQL syntax": ["sql_error", "syntax", "database"]
        }
        
        for error_type, tags in error_types.items():
            if error_type in prompt or error_type in response:
                pattern = KnowledgePattern(
                    title=f"{error_type} Solution",
                    content=response,
                    category="error"
                ).add_tags(tags)
                patterns.append(pattern)
                break
                
        return patterns
    
    def _extract_regex_patterns(self, prompt: str, response: str) -> List[KnowledgePattern]:
        """Extract patterns from regex generation"""
        patterns = []
        
        # Extract the actual regex pattern
        regex_patterns = re.findall(r'(?:^|[^\\])/(.+?)(?:[^\\])/[gimsu]*', response)
        if not regex_patterns:
            # Try other common formats
            regex_patterns = re.findall(r'(?:regex:|pattern:)\s*(.+?)(?:\n|$)', response, re.IGNORECASE)
            
        if regex_patterns:
            pattern = KnowledgePattern(
                title=f"Regex for: {prompt[:50]}...",
                content=response,
                category="code"
            ).add_tags(["regex", "pattern", "validation"])
            patterns.append(pattern)
            
        return patterns    
    def save_patterns(self, patterns: List[KnowledgePattern], query_id: int) -> int:
        """Save extracted patterns to database"""
        saved_count = 0
        
        for pattern in patterns:
            # Create searchable content with tags
            searchable_content = f"{pattern.content}\n\nTags: {', '.join(pattern.tags)}"
            
            if self.db.save_knowledge_unit(
                query_id=query_id,
                title=pattern.title,
                content=searchable_content,
                category=pattern.category
            ):
                saved_count += 1
                
        return saved_count
    
    def auto_extract_and_save(self, tool: str, model: str, prompt: str, 
                             response: str) -> Dict[str, Any]:
        """Complete workflow: log query, extract patterns, save knowledge"""
        # Log the query
        query_id = self.db.log_query(tool, model, prompt, response)
        
        if not query_id:
            return {"success": False, "error": "Failed to log query"}
            
        # Extract patterns
        patterns = self.extract_patterns(tool, prompt, response)
        
        # Save patterns
        saved = self.save_patterns(patterns, query_id)
        
        return {
            "success": True,
            "query_id": query_id,
            "patterns_found": len(patterns),
            "patterns_saved": saved,
            "categories": list(set(p.category for p in patterns))
        }

def standardize_knowledge_entry(tool: str, category: str, pattern: str, 
                               context: Dict, performance: Dict) -> Dict:
    """
    Create standardized knowledge entry format
    
    Args:
        tool: Tool that generated the knowledge
        category: Category (code, sql, doc, error, etc.)
        pattern: The reusable pattern/insight
        context: Relevant metadata (model, prompt excerpt, etc.)
        performance: Timing, retries, success metrics
        
    Returns:
        Standardized dictionary for consistent storage
    """
    return {
        "tool": tool,
        "category": category,
        "pattern": pattern,
        "context": context,
        "performance": performance,
        "timestamp": datetime.now().isoformat(),
        "version": "1.0"  # Schema version for future migrations
    }

# Convenience function for backward compatibility
def capture_knowledge(tool: str, model: str, prompt: str, response: str) -> Optional[int]:
    """Simple knowledge capture for existing tools"""
    extractor = KnowledgeExtractor()
    result = extractor.auto_extract_and_save(tool, model, prompt, response)
    return result.get("query_id") if result["success"] else None
