"""
TuoKit Learning Strategy Module
Practical spaced repetition and progress tracking
Following TuoKit Architect: Start simple, add value immediately
"""

import json
import datetime
from typing import Dict, List, Optional, Tuple
from utils import DatabaseManager

class SimpleLearningStrategy:
    """
    Simplified spaced repetition implementation
    No complex algorithms initially - just practical intervals
    """
    
    # Simple interval progression (days)
    DEFAULT_INTERVALS = [1, 3, 7, 14, 30, 60]
    
    def __init__(self):
        self.db = DatabaseManager()
    
    def generate_review_schedule(self, concepts: List[str], 
                               difficulty: str = "Intermediate") -> Dict[str, List[datetime.date]]:
        """
        Generate simple review schedule for concepts
        Adjust intervals based on difficulty
        """
        today = datetime.date.today()
        
        # Adjust intervals based on difficulty
        if difficulty == "Beginner":
            intervals = [int(i * 0.7) for i in self.DEFAULT_INTERVALS]  # More frequent
        elif difficulty == "Advanced":
            intervals = [int(i * 1.3) for i in self.DEFAULT_INTERVALS]  # Less frequent
        else:
            intervals = self.DEFAULT_INTERVALS
        
        schedule = {}
        for concept in concepts[:10]:  # Limit to top 10 concepts
            schedule[concept] = [
                today + datetime.timedelta(days=interval)
                for interval in intervals
            ]
        
        return schedule
    
    def track_study_session(self, content_hash: str, quiz_score: float, 
                          concepts: List[str], difficulty: str):
        """
        Simple session tracking - store in queries metadata
        No new tables needed initially
        """
        metadata = {
            "type": "study_session",
            "content_hash": content_hash,
            "quiz_score": quiz_score,
            "concepts": concepts[:10],
            "difficulty": difficulty,
            "timestamp": datetime.datetime.now().isoformat()
        }
        
        # Use existing log_query with metadata
        self.db.log_query(
            tool="study_guide_generator",
            model="learning_tracker",
            prompt=f"Study session for {content_hash}",
            response=json.dumps(metadata),
            metadata=metadata
        )
    
    def get_retention_estimate(self, content_hash: str) -> Optional[float]:
        """
        Simple retention estimate based on past reviews
        Returns percentage estimate of current retention
        """
        try:
            # Query past study sessions for this content
            result = self.db.conn.cursor()
            result.execute("""
                SELECT metadata->>'quiz_score' as score, 
                       metadata->>'timestamp' as ts
                FROM queries 
                WHERE tool = 'study_guide_generator' 
                  AND metadata->>'content_hash' = %s
                  AND metadata->>'type' = 'study_session'
                ORDER BY created_at DESC
                LIMIT 5
            """, (content_hash,))
            
            sessions = result.fetchall()
            if not sessions:
                return None
            
            # Simple retention calculation
            # Most recent score weighted more heavily
            weights = [0.5, 0.25, 0.15, 0.07, 0.03]
            weighted_sum = 0
            weight_total = 0
            
            for i, (score, ts) in enumerate(sessions):
                if i < len(weights) and score:
                    weighted_sum += float(score) * weights[i]
                    weight_total += weights[i]
            
            return (weighted_sum / weight_total) if weight_total > 0 else None
            
        except Exception as e:
            print(f"Error calculating retention: {e}")
            return None
