"""
Unified Knowledge Capture System for TuoKit
Automatically captures, categorizes, and connects knowledge from AI interactions
"""

import json
import re
import hashlib
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
from difflib import SequenceMatcher
import logging

# Set up logging
logger = logging.getLogger(__name__)

# Knowledge categories with subcategories
KNOWLEDGE_CATEGORIES = {
    "code_pattern": {
        "name": "Code Patterns",
        "subcategories": ["function", "class", "algorithm", "snippet", "refactoring"],
        "keywords": ["def", "class", "function", "method", "algorithm", "implement"]
    },
    "error_solution": {
        "name": "Error Solutions", 
        "subcategories": ["syntax", "runtime", "logic", "configuration", "dependency"],
        "keywords": ["error", "fix", "solve", "debug", "exception", "traceback"]
    },
    "sql_query": {
        "name": "SQL Patterns",
        "subcategories": ["select", "join", "optimization", "schema", "migration"],
        "keywords": ["SELECT", "INSERT", "UPDATE", "DELETE", "CREATE", "ALTER", "JOIN"]
    },
    "explanation": {
        "name": "Explanations",
        "subcategories": ["concept", "tutorial", "best_practice", "comparison", "overview"],
        "keywords": ["explain", "what is", "how to", "tutorial", "guide", "understand"]
    },
    "configuration": {
        "name": "Configuration",
        "subcategories": ["setup", "deployment", "environment", "integration", "settings"],
        "keywords": ["config", "setup", "install", "deploy", "environment", "settings"]
    }
}

@dataclass
class QualityMetrics:
    """Metrics for evaluating knowledge quality"""
    content_length: int
    code_block_count: int
    has_explanation: bool
    has_example: bool
    specificity_score: float
    completeness_score: float
    
    @property
    def overall_score(self) -> int:
        """Calculate overall quality score (0-100)"""
        score = 0
        
        # Length contribution (max 20 points)
        if self.content_length > 500:
            score += 20
        elif self.content_length > 200:
            score += 15
        elif self.content_length > 100:
            score += 10
        elif self.content_length > 50:
            score += 5
            
        # Code blocks (max 20 points)
        score += min(self.code_block_count * 10, 20)
        
        # Explanation (15 points)
        if self.has_explanation:
            score += 15
            
        # Example (15 points)
        if self.has_example:
            score += 15
            
        # Specificity (15 points)
        score += int(self.specificity_score * 15)
        
        # Completeness (15 points)
        score += int(self.completeness_score * 15)
        
        return min(score, 100)

class QualityGate:
    """Validates knowledge quality before capture"""
    
    MIN_CONTENT_LENGTH = 50
    MAX_CONTENT_LENGTH = 10000
    MIN_QUALITY_SCORE = 30
    DUPLICATE_THRESHOLD = 0.85  # Similarity threshold
    
    def __init__(self, db_manager=None):
        self.db = db_manager
        
    def validate(self, content: str, metadata: Dict) -> Tuple[bool, str, QualityMetrics]:
        """
        Validate content quality
        Returns: (is_valid, reason, metrics)
        """
        # Length check
        if len(content) < self.MIN_CONTENT_LENGTH:
            return False, "Content too short to be valuable", None
            
        if len(content) > self.MAX_CONTENT_LENGTH:
            return False, "Content too long, should be split into smaller units", None
            
        # Calculate quality metrics
        metrics = self._calculate_metrics(content, metadata)
        
        # Quality score check
        if metrics.overall_score < self.MIN_QUALITY_SCORE:
            return False, f"Quality score {metrics.overall_score} below threshold", metrics
            
        # Duplicate check (only if database available)
        if self.db and self._is_duplicate(content):
            return False, "Similar knowledge already exists", metrics
            
        return True, "Passed all quality gates", metrics
    
    def _calculate_metrics(self, content: str, metadata: Dict) -> QualityMetrics:
        """Calculate quality metrics for content"""
        # Count code blocks
        code_blocks = len(re.findall(r'```[\s\S]*?```', content))
        
        # Check for explanations (common explanation patterns)
        explanation_patterns = [
            r'this\s+(is|does|means|works)',
            r'explanation:',
            r'because',
            r'the\s+reason',
            r'in\s+other\s+words'
        ]
        has_explanation = any(re.search(pattern, content, re.I) for pattern in explanation_patterns)
        
        # Check for examples
        example_patterns = [
            r'example:',
            r'for\s+example',
            r'e\.g\.',
            r'such\s+as',
            r'```'  # Code blocks often contain examples
        ]
        has_example = any(re.search(pattern, content, re.I) for pattern in example_patterns)
        
        # Calculate specificity (presence of specific terms vs generic)
        specific_terms = len(re.findall(r'\b[A-Z][a-zA-Z]{2,}\b|\b\w+\(\)|\b\w+\.\w+', content))
        total_words = len(content.split())
        specificity_score = min(specific_terms / max(total_words * 0.1, 1), 1.0)
        
        # Calculate completeness (based on structure)
        completeness_indicators = [
            bool(re.search(r'^\s*#|^\s*def|^\s*class', content, re.M)),  # Code structure
            bool(re.search(r'\n\s*\n', content)),  # Paragraphs
            bool(re.search(r'[.!?]\s+[A-Z]', content)),  # Multiple sentences
            code_blocks > 0,  # Has code
            has_explanation  # Has explanation
        ]
        completeness_score = sum(completeness_indicators) / len(completeness_indicators)
        
        return QualityMetrics(
            content_length=len(content),
            code_block_count=code_blocks,
            has_explanation=has_explanation,
            has_example=has_example,
            specificity_score=specificity_score,
            completeness_score=completeness_score
        )
    
    def _is_duplicate(self, content: str) -> bool:
        """Check if similar content already exists"""
        if not self.db:
            return False
            
        try:
            # Get recent knowledge units for comparison
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT content FROM knowledge_units
                        WHERE created_at > %s
                        ORDER BY created_at DESC
                        LIMIT 100
                    """, (datetime.now() - timedelta(days=7),))
                    
                    recent_content = cur.fetchall()
                    
                    # Check similarity with recent content
                    for (existing_content,) in recent_content:
                        similarity = SequenceMatcher(None, content, existing_content).ratio()
                        if similarity > self.DUPLICATE_THRESHOLD:
                            return True
                            
        except Exception as e:
            logger.error(f"Error checking duplicates: {e}")
            
        return False

class KnowledgeCategorizer:
    """Automatically categorizes knowledge based on content"""
    
    def categorize(self, content: str, metadata: Dict) -> Tuple[str, List[str]]:
        """
        Categorize content and extract tags
        Returns: (category, tags)
        """
        content_lower = content.lower()
        scores = {}
        
        # Score each category based on keyword presence
        for category, info in KNOWLEDGE_CATEGORIES.items():
            score = 0
            for keyword in info["keywords"]:
                score += content_lower.count(keyword.lower())
            scores[category] = score
        
        # Check metadata for tool-specific hints
        tool_name = metadata.get("tool", "").lower()
        if "sql" in tool_name:
            scores["sql_query"] += 10
        elif "error" in tool_name or "exception" in tool_name:
            scores["error_solution"] += 10
        elif "code" in tool_name:
            scores["code_pattern"] += 10
            
        # Select category with highest score
        category = max(scores, key=scores.get) if max(scores.values()) > 0 else "explanation"
        
        # Extract tags
        tags = self._extract_tags(content, category)
        
        return category, tags
    
    def _extract_tags(self, content: str, category: str) -> List[str]:
        """Extract relevant tags from content"""
        tags = []
        
        # Language detection for code
        if category == "code_pattern":
            if "```python" in content or "def " in content:
                tags.append("python")
            if "```ruby" in content or "def self." in content:
                tags.append("ruby")
            if "```sql" in content:
                tags.append("sql")
            if "```javascript" in content or "function(" in content:
                tags.append("javascript")
                
        # Framework detection
        frameworks = {
            "rails": ["Rails", "ActiveRecord", "ActionController"],
            "django": ["Django", "models.Model", "views.py"],
            "streamlit": ["streamlit", "st.", "st.write"],
            "react": ["React", "useState", "useEffect", "jsx"],
        }
        
        for framework, markers in frameworks.items():
            if any(marker in content for marker in markers):
                tags.append(framework)
                
        # Error type detection
        if category == "error_solution":
            error_types = {
                "syntax": ["SyntaxError", "IndentationError", "unexpected"],
                "runtime": ["RuntimeError", "TypeError", "ValueError"],
                "import": ["ImportError", "ModuleNotFoundError", "No module named"],
                "connection": ["ConnectionError", "timeout", "refused"],
            }
            
            for error_type, markers in error_types.items():
                if any(marker in content for marker in markers):
                    tags.append(error_type)
                    
        # SQL operation detection  
        if category == "sql_query":
            sql_ops = ["SELECT", "INSERT", "UPDATE", "DELETE", "JOIN", "CREATE", "ALTER"]
            for op in sql_ops:
                if op in content.upper():
                    tags.append(op.lower())
                    
        return list(set(tags))  # Remove duplicates

class UnifiedKnowledgeCapture:
    """Central manager for all knowledge capture operations"""
    
    def __init__(self, db_manager=None):
        self.db = db_manager
        self.quality_gate = QualityGate(db_manager)
        self.categorizer = KnowledgeCategorizer()
        self.enabled = True  # Can be disabled via settings
        
    def capture(self, 
                tool_name: str,
                model: str,
                prompt: str,
                response: str,
                metadata: Optional[Dict] = None) -> Optional[int]:
        """
        Main capture method - processes and stores knowledge
        Returns: knowledge_unit_id if captured, None otherwise
        """
        if not self.enabled or not self.db:
            return None
            
        try:
            # Prepare metadata
            metadata = metadata or {}
            metadata["tool"] = tool_name
            metadata["model"] = model
            
            # Extract title from prompt (first line or first 100 chars)
            title = prompt.split('\n')[0][:100] if prompt else "Untitled"
            
            # Validate quality
            is_valid, reason, metrics = self.quality_gate.validate(response, metadata)
            if not is_valid:
                logger.info(f"Knowledge rejected: {reason}")
                return None
                
            # Categorize
            category, tags = self.categorizer.categorize(response, metadata)
            
            # Store in database
            knowledge_id = self._store_knowledge(
                title=title,
                content=response,
                category=category,
                tags=tags,
                quality_score=metrics.overall_score,
                metadata=metadata,
                query_id=metadata.get("query_id")
            )
            
            logger.info(f"Knowledge captured: {knowledge_id} (category: {category}, score: {metrics.overall_score})")
            return knowledge_id
            
        except Exception as e:
            logger.error(f"Knowledge capture failed: {e}")
            return None
    
    def _store_knowledge(self, **kwargs) -> Optional[int]:
        """Store knowledge unit in database"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO knowledge_units 
                        (title, content, category, tags, quality_score, tool_specific_data, query_id)
                        VALUES (%s, %s, %s, %s, %s, %s, %s)
                        RETURNING id
                    """, (
                        kwargs["title"],
                        kwargs["content"],
                        kwargs["category"],
                        kwargs["tags"],
                        kwargs["quality_score"],
                        json.dumps(kwargs["metadata"]),
                        kwargs.get("query_id")
                    ))
                    
                    knowledge_id = cur.fetchone()[0]
                    
                    # Log to maintenance table
                    cur.execute("""
                        INSERT INTO knowledge_maintenance_log (action, details, units_affected)
                        VALUES ('capture', %s, 1)
                    """, (json.dumps({
                        "knowledge_id": knowledge_id,
                        "category": kwargs["category"],
                        "quality_score": kwargs["quality_score"],
                        "tags": kwargs["tags"]
                    }),))
                    
                    return knowledge_id
                    
        except Exception as e:
            logger.error(f"Failed to store knowledge: {e}")
            return None
    
    def search(self, 
               query: str,
               category: Optional[str] = None,
               min_quality: int = 0,
               limit: int = 50) -> List[Dict]:
        """Search knowledge base"""
        if not self.db:
            return []
            
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Build query
                    sql = """
                        SELECT id, title, content, category, tags, quality_score, 
                               usage_count, created_at
                        FROM knowledge_units
                        WHERE quality_score >= %s
                    """
                    params = [min_quality]
                    
                    if category:
                        sql += " AND category = %s"
                        params.append(category)
                        
                    if query:
                        sql += """ AND (
                            title ILIKE %s OR 
                            content ILIKE %s OR
                            %s = ANY(tags)
                        )"""
                        params.extend([f"%{query}%", f"%{query}%", query])
                        
                    sql += " ORDER BY quality_score DESC, usage_count DESC LIMIT %s"
                    params.append(limit)
                    
                    cur.execute(sql, params)
                    
                    results = []
                    for row in cur.fetchall():
                        results.append({
                            "id": row[0],
                            "title": row[1],
                            "content": row[2],
                            "category": row[3],
                            "tags": row[4],
                            "quality_score": row[5],
                            "usage_count": row[6],
                            "created_at": row[7]
                        })
                        
                    # Update usage count for accessed items
                    if results:
                        ids = [r["id"] for r in results]
                        cur.execute("""
                            UPDATE knowledge_units 
                            SET usage_count = usage_count + 1,
                                last_accessed = CURRENT_TIMESTAMP
                            WHERE id = ANY(%s)
                        """, (ids,))
                        
                    return results
                    
        except Exception as e:
            logger.error(f"Knowledge search failed: {e}")
            return []
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get knowledge system metrics"""
        if not self.db:
            return {}
            
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    metrics = {}
                    
                    # Total knowledge units
                    cur.execute("SELECT COUNT(*) FROM knowledge_units")
                    metrics["total_units"] = cur.fetchone()[0]
                    
                    # By category
                    cur.execute("""
                        SELECT category, COUNT(*) 
                        FROM knowledge_units 
                        GROUP BY category
                    """)
                    metrics["by_category"] = dict(cur.fetchall())
                    
                    # Average quality
                    cur.execute("SELECT AVG(quality_score) FROM knowledge_units")
                    metrics["avg_quality"] = float(cur.fetchone()[0] or 0)
                    
                    # Recent captures
                    cur.execute("""
                        SELECT COUNT(*) FROM knowledge_units
                        WHERE created_at > CURRENT_TIMESTAMP - INTERVAL '24 hours'
                    """)
                    metrics["captures_24h"] = cur.fetchone()[0]
                    
                    # Most used
                    cur.execute("""
                        SELECT title, usage_count 
                        FROM knowledge_units
                        ORDER BY usage_count DESC
                        LIMIT 5
                    """)
                    metrics["most_used"] = [
                        {"title": row[0], "usage_count": row[1]}
                        for row in cur.fetchall()
                    ]
                    
                    return metrics
                    
        except Exception as e:
            logger.error(f"Failed to get metrics: {e}")
            return {}

# Singleton instance
_capture_manager = None

def get_capture_manager(db_manager=None) -> UnifiedKnowledgeCapture:
    """Get or create the capture manager instance"""
    global _capture_manager
    if _capture_manager is None:
        _capture_manager = UnifiedKnowledgeCapture(db_manager)
    return _capture_manager