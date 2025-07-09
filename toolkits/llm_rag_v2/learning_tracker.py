"""
Simple learning tracker for RAG system
Demonstrates how the system could learn from usage
"""

import json
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional
from collections import defaultdict, Counter

logger = logging.getLogger(__name__)

class LearningTracker:
    """
    Tracks user interactions and learns patterns
    This is a simple file-based implementation for demonstration
    """
    
    def __init__(self, data_dir: Optional[Path] = None):
        """Initialize learning tracker"""
        self.data_dir = data_dir or (Path.home() / ".tuokit" / "rag_learning")
        self.data_dir.mkdir(parents=True, exist_ok=True)
        
        # File paths
        self.queries_file = self.data_dir / "queries.jsonl"
        self.feedback_file = self.data_dir / "feedback.jsonl"
        self.interactions_file = self.data_dir / "interactions.jsonl"
        
        # In-memory cache
        self.query_count = Counter()
        self.helpful_results = defaultdict(set)
        self._load_data()
    
    def log_query(self, query: str, session_id: Optional[str] = None):
        """Log a search query"""
        entry = {
            "timestamp": datetime.now().isoformat(),
            "query": query,
            "session_id": session_id or "anonymous"
        }
        
        # Append to file
        with open(self.queries_file, "a") as f:
            f.write(json.dumps(entry) + "\n")
        
        # Update cache
        self.query_count[query] += 1
        
        logger.info(f"Logged query: {query}")
    
    def log_feedback(self, query: str, result_id: str, helpful: bool):
        """Log feedback on a result"""
        entry = {
            "timestamp": datetime.now().isoformat(),
            "query": query,
            "result_id": result_id,
            "helpful": helpful
        }
        
        # Append to file
        with open(self.feedback_file, "a") as f:
            f.write(json.dumps(entry) + "\n")
        
        # Update cache
        if helpful:
            self.helpful_results[query].add(result_id)
        
        logger.info(f"Logged feedback: {query} -> {result_id} = {helpful}")
    
    def log_interaction(self, query: str, result_id: str, action: str):
        """Log user interaction with a result"""
        entry = {
            "timestamp": datetime.now().isoformat(),
            "query": query,
            "result_id": result_id,
            "action": action  # view_source, copy_code, expand
        }
        
        # Append to file
        with open(self.interactions_file, "a") as f:
            f.write(json.dumps(entry) + "\n")
    
    def get_popular_queries(self, limit: int = 10) -> List[str]:
        """Get most popular queries"""
        return [query for query, _ in self.query_count.most_common(limit)]
    
    def get_query_suggestions(self, partial: str, limit: int = 5) -> List[str]:
        """Get query suggestions based on history"""
        partial_lower = partial.lower()
        suggestions = []
        
        for query, count in self.query_count.most_common():
            if partial_lower in query.lower() and query.lower() != partial_lower:
                suggestions.append(query)
                if len(suggestions) >= limit:
                    break
        
        return suggestions
    
    def get_learning_boost(self, query: str, result_id: str) -> float:
        """
        Calculate boost factor based on learning
        Returns 0.0 to 1.0 boost to apply to result score
        """
        # Simple heuristic: boost if similar queries found this helpful
        boost = 0.0
        
        # Check if this exact result was helpful for this query
        if result_id in self.helpful_results[query]:
            boost += 0.3
        
        # Check if result was helpful for similar queries
        query_words = set(query.lower().split())
        for other_query, helpful_set in self.helpful_results.items():
            if result_id in helpful_set:
                other_words = set(other_query.lower().split())
                similarity = len(query_words & other_words) / max(len(query_words), 1)
                if similarity > 0.5:
                    boost += 0.1 * similarity
        
        return min(boost, 1.0)  # Cap at 1.0
    
    def get_stats(self) -> Dict:
        """Get learning statistics"""
        total_queries = sum(self.query_count.values())
        unique_queries = len(self.query_count)
        
        # Count feedback
        helpful_count = sum(len(results) for results in self.helpful_results.values())
        
        return {
            "total_queries": total_queries,
            "unique_queries": unique_queries,
            "helpful_feedback": helpful_count,
            "top_queries": self.get_popular_queries(5),
            "learning_active": True
        }
    
    def _load_data(self):
        """Load existing data into memory"""
        # Load queries
        if self.queries_file.exists():
            with open(self.queries_file) as f:
                for line in f:
                    try:
                        entry = json.loads(line)
                        self.query_count[entry["query"]] += 1
                    except:
                        pass
        
        # Load feedback
        if self.feedback_file.exists():
            with open(self.feedback_file) as f:
                for line in f:
                    try:
                        entry = json.loads(line)
                        if entry["helpful"]:
                            self.helpful_results[entry["query"]].add(entry["result_id"])
                    except:
                        pass
    
    def analyze_patterns(self) -> Dict:
        """Analyze usage patterns for insights"""
        patterns = {
            "common_topics": self._extract_topics(),
            "query_sequences": self._find_sequences(),
            "peak_hours": self._analyze_time_patterns(),
            "improvement_areas": self._find_failed_queries()
        }
        return patterns
    
    def _extract_topics(self) -> List[str]:
        """Extract common topics from queries"""
        # Simple word frequency analysis
        word_count = Counter()
        stop_words = {"the", "a", "an", "is", "how", "what", "where", "when", "why", "does", "do", "in", "to"}
        
        for query in self.query_count:
            words = query.lower().split()
            for word in words:
                if word not in stop_words and len(word) > 2:
                    word_count[word] += self.query_count[query]
        
        return [word for word, _ in word_count.most_common(10)]
    
    def _find_sequences(self) -> List[List[str]]:
        """Find common query sequences (placeholder)"""
        # This would analyze session data to find patterns
        return []
    
    def _analyze_time_patterns(self) -> Dict:
        """Analyze when users search most (placeholder)"""
        return {"peak_hour": "14:00", "peak_day": "Tuesday"}
    
    def _find_failed_queries(self) -> List[str]:
        """Find queries that often don't get helpful results"""
        failed = []
        for query in self.query_count:
            if query not in self.helpful_results:
                failed.append(query)
        return failed[:5]  # Top 5


# Singleton instance
_learning_tracker = None

def get_learning_tracker() -> LearningTracker:
    """Get or create the learning tracker instance"""
    global _learning_tracker
    if _learning_tracker is None:
        _learning_tracker = LearningTracker()
    return _learning_tracker