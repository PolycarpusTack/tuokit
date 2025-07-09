"""
Crash Knowledge System - Self-learning crash pattern engine
Learns from every validated crash analysis to improve future predictions
"""

import hashlib
import json
import re
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass
import numpy as np
from collections import defaultdict

# Try to import sentence transformers for embeddings
try:
    from sentence_transformers import SentenceTransformer
    EMBEDDINGS_AVAILABLE = True
except ImportError:
    EMBEDDINGS_AVAILABLE = False
    print("Note: sentence-transformers not available. Using basic similarity matching.")

@dataclass
class CrashPattern:
    """Represents a learned crash pattern"""
    id: Optional[int] = None
    fingerprint_exact: str = ""
    fingerprint_structural: str = ""
    pattern_name: str = ""
    error_type: str = ""
    cause_of_dump: str = ""
    stack_signature: str = ""
    occurrence_count: int = 1
    confidence_score: float = 0.5
    verified_solutions: List[Dict] = None
    prevention_strategies: List[str] = None
    metadata: Dict = None
    
    def __post_init__(self):
        if self.verified_solutions is None:
            self.verified_solutions = []
        if self.prevention_strategies is None:
            self.prevention_strategies = []
        if self.metadata is None:
            self.metadata = {}

@dataclass 
class CrashMatch:
    """Result of pattern matching"""
    is_new: bool = True
    pattern_id: Optional[int] = None
    pattern_name: str = ""
    confidence: float = 0.0
    match_type: str = ""  # 'exact', 'structural', 'semantic', 'fuzzy'
    verified_solutions: List[Dict] = None

class CrashFingerprintGenerator:
    """Generates multi-level fingerprints for crash identification"""
    
    @staticmethod
    def extract_method_name(frame: str) -> str:
        """Extract method name from stack frame"""
        # Pattern: ClassName>>methodName or ClassName class>>methodName
        match = re.search(r'>>(\w+(?::)?)', frame)
        return match.group(1) if match else ""
    
    @staticmethod
    def extract_class_name(frame: str) -> str:
        """Extract class name from stack frame"""
        match = re.search(r'([\w.]+)(?:\s+class)?(?:\([\w.]+\))?>>>', frame)
        return match.group(1) if match else ""
    
    def generate(self, crash_content: str, stack_frames: List[str] = None) -> Dict[str, Any]:
        """Generate multi-level fingerprint"""
        from utils.wcr_patterns import extract_wcr_cause, extract_wcr_stack_frames
        
        # Extract key components
        cause = extract_wcr_cause(crash_content) or "Unknown"
        if not stack_frames:
            stack_frames = extract_wcr_stack_frames(crash_content, limit=5)
        
        # Level 1: Exact fingerprint (cause + first 3 frames)
        exact_content = f"{cause}{''.join(stack_frames[:3])}"
        exact_fingerprint = hashlib.sha256(exact_content.encode()).hexdigest()
        
        # Level 2: Structural fingerprint (method signatures only)
        methods = [self.extract_method_name(frame) for frame in stack_frames[:5]]
        methods = [m for m in methods if m]  # Remove empty
        structural_content = f"{cause.split(':')[0]}{''.join(methods)}"
        structural_fingerprint = hashlib.sha256(structural_content.encode()).hexdigest()
        
        # Level 3: Stack signature (for display)
        stack_signature = " -> ".join([
            f"{self.extract_class_name(frame)}.{self.extract_method_name(frame)}"
            for frame in stack_frames[:3]
        ])
        
        return {
            'exact': exact_fingerprint,
            'structural': structural_fingerprint,
            'stack_signature': stack_signature,
            'cause': cause,
            'top_methods': methods[:3]
        }

class CrashSimilarityEngine:
    """Calculates similarity between crashes using multiple strategies"""
    
    def __init__(self):
        self.fingerprint_gen = CrashFingerprintGenerator()
        if EMBEDDINGS_AVAILABLE:
            # Use a small, fast model
            self.encoder = SentenceTransformer('all-MiniLM-L6-v2')
        else:
            self.encoder = None
    
    def calculate_structural_similarity(self, crash1_fp: Dict, crash2_fp: Dict) -> float:
        """Calculate similarity based on method signatures"""
        methods1 = set(crash1_fp.get('top_methods', []))
        methods2 = set(crash2_fp.get('top_methods', []))
        
        if not methods1 or not methods2:
            return 0.0
            
        intersection = len(methods1 & methods2)
        union = len(methods1 | methods2)
        
        return intersection / union if union > 0 else 0.0
    
    def calculate_cause_similarity(self, cause1: str, cause2: str) -> float:
        """Calculate similarity between error causes"""
        # Normalize causes
        cause1_normalized = re.sub(r'[#:\s]+', ' ', cause1.lower()).strip()
        cause2_normalized = re.sub(r'[#:\s]+', ' ', cause2.lower()).strip()
        
        # Exact match
        if cause1_normalized == cause2_normalized:
            return 1.0
        
        # Partial match (e.g., "message not understood" in both)
        words1 = set(cause1_normalized.split())
        words2 = set(cause2_normalized.split())
        
        if words1 and words2:
            intersection = len(words1 & words2)
            union = len(words1 | words2)
            return intersection / union
        
        return 0.0
    
    def calculate_semantic_similarity(self, text1: str, text2: str) -> float:
        """Calculate semantic similarity using embeddings"""
        if not self.encoder:
            # Fallback to simple word overlap
            return self.calculate_cause_similarity(text1, text2)
        
        try:
            # Generate embeddings
            embeddings = self.encoder.encode([text1, text2])
            
            # Cosine similarity
            similarity = np.dot(embeddings[0], embeddings[1]) / (
                np.linalg.norm(embeddings[0]) * np.linalg.norm(embeddings[1])
            )
            
            return float(similarity)
        except:
            return 0.0
    
    def calculate_overall_similarity(self, crash1_content: str, crash2_content: str) -> Tuple[float, str]:
        """Calculate overall similarity between two crashes"""
        # Generate fingerprints
        fp1 = self.fingerprint_gen.generate(crash1_content)
        fp2 = self.fingerprint_gen.generate(crash2_content)
        
        # Exact match
        if fp1['exact'] == fp2['exact']:
            return 1.0, 'exact'
        
        # Structural match
        if fp1['structural'] == fp2['structural']:
            return 0.95, 'structural'
        
        # Calculate component similarities
        cause_sim = self.calculate_cause_similarity(fp1['cause'], fp2['cause'])
        struct_sim = self.calculate_structural_similarity(fp1, fp2)
        
        # Weight the similarities
        weighted_sim = (cause_sim * 0.6) + (struct_sim * 0.4)
        
        if weighted_sim > 0.7:
            return weighted_sim, 'fuzzy'
        
        # Try semantic similarity as last resort
        if self.encoder:
            semantic_sim = self.calculate_semantic_similarity(
                fp1['stack_signature'], 
                fp2['stack_signature']
            )
            if semantic_sim > 0.8:
                return semantic_sim, 'semantic'
        
        return weighted_sim, 'none'

class CrashPatternLearner:
    """Learns new patterns from validated crash analyses"""
    
    def __init__(self, db_manager):
        self.db = db_manager
        self.fingerprint_gen = CrashFingerprintGenerator()
        self.similarity_engine = CrashSimilarityEngine()
    
    def learn_from_crash(self, crash_content: str, analysis: Dict, validation: Dict) -> Dict[str, Any]:
        """Learn from a validated crash analysis"""
        # Generate fingerprint
        fingerprint = self.fingerprint_gen.generate(crash_content)
        
        # Check if pattern exists
        existing_pattern = self.find_existing_pattern(fingerprint)
        
        if existing_pattern:
            # Update existing pattern
            result = self.update_pattern(existing_pattern, analysis, validation)
            return {
                'action': 'updated',
                'pattern_id': existing_pattern['id'],
                'pattern_name': existing_pattern['pattern_name'],
                'confidence_before': existing_pattern['confidence_score'],
                'confidence_after': result['confidence_score']
            }
        else:
            # Create new pattern
            new_pattern = self.create_pattern(fingerprint, analysis, validation)
            return {
                'action': 'created',
                'pattern_id': new_pattern['id'],
                'pattern_name': new_pattern['pattern_name'],
                'confidence': new_pattern['confidence_score']
            }
    
    def find_existing_pattern(self, fingerprint: Dict) -> Optional[Dict]:
        """Find existing pattern by fingerprint"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Try exact match first
                    cur.execute("""
                        SELECT * FROM crash_patterns 
                        WHERE fingerprint_exact = %s
                    """, (fingerprint['exact'],))
                    
                    result = cur.fetchone()
                    if result:
                        return self._row_to_pattern(result)
                    
                    # Try structural match
                    cur.execute("""
                        SELECT * FROM crash_patterns 
                        WHERE fingerprint_structural = %s
                    """, (fingerprint['structural'],))
                    
                    result = cur.fetchone()
                    if result:
                        return self._row_to_pattern(result)
                    
                    return None
        except:
            return None
    
    def create_pattern(self, fingerprint: Dict, analysis: Dict, validation: Dict) -> Dict:
        """Create a new crash pattern"""
        pattern_name = self.generate_pattern_name(fingerprint, analysis)
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO crash_patterns (
                            fingerprint_exact, fingerprint_structural, pattern_name,
                            error_type, cause_of_dump, stack_signature,
                            occurrence_count, confidence_score,
                            verified_solutions, prevention_strategies,
                            first_seen, last_seen
                        ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                        RETURNING id
                    """, (
                        fingerprint['exact'],
                        fingerprint['structural'],
                        pattern_name,
                        analysis.get('error_type', 'Unknown'),
                        fingerprint['cause'],
                        fingerprint['stack_signature'],
                        1,
                        0.5,  # Initial confidence
                        json.dumps([{
                            'description': analysis.get('quick_fix', ''),
                            'success_count': 1,
                            'added_by': validation.get('validator_name', 'Unknown')
                        }]),
                        json.dumps([analysis.get('prevention', '')]),
                        datetime.now(),
                        datetime.now()
                    ))
                    
                    pattern_id = cur.fetchone()[0]
                    conn.commit()
                    
                    return {
                        'id': pattern_id,
                        'pattern_name': pattern_name,
                        'confidence_score': 0.5
                    }
        except Exception as e:
            print(f"Error creating pattern: {e}")
            return None
    
    def update_pattern(self, pattern: Dict, analysis: Dict, validation: Dict) -> Dict:
        """Update existing pattern with new knowledge"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Update occurrence count and confidence
                    new_confidence = min(pattern['confidence_score'] + 0.1, 1.0)
                    
                    # Add new solution if different
                    solutions = pattern.get('verified_solutions', [])
                    new_solution = {
                        'description': analysis.get('quick_fix', ''),
                        'success_count': 1,
                        'added_by': validation.get('validator_name', 'Unknown')
                    }
                    
                    # Check if solution already exists
                    solution_exists = False
                    for sol in solutions:
                        if sol['description'] == new_solution['description']:
                            sol['success_count'] += 1
                            solution_exists = True
                            break
                    
                    if not solution_exists and new_solution['description']:
                        solutions.append(new_solution)
                    
                    cur.execute("""
                        UPDATE crash_patterns 
                        SET occurrence_count = occurrence_count + 1,
                            confidence_score = %s,
                            verified_solutions = %s,
                            last_seen = %s,
                            updated_at = %s
                        WHERE id = %s
                    """, (
                        new_confidence,
                        json.dumps(solutions),
                        datetime.now(),
                        datetime.now(),
                        pattern['id']
                    ))
                    
                    conn.commit()
                    
                    return {
                        'id': pattern['id'],
                        'confidence_score': new_confidence
                    }
        except Exception as e:
            print(f"Error updating pattern: {e}")
            return pattern
    
    def generate_pattern_name(self, fingerprint: Dict, analysis: Dict) -> str:
        """Generate a descriptive pattern name"""
        cause = fingerprint['cause']
        error_type = analysis.get('error_type', '')
        
        # Clean up cause
        cause_clean = re.sub(r'[#:]', '', cause).strip()
        
        # Generate name
        if 'not understood' in cause.lower():
            return f"MethodNotFound_{cause_clean[:30]}"
        elif 'space' in cause.lower():
            return f"LowSpace_{datetime.now().strftime('%Y%m')}"
        elif error_type:
            return f"{error_type}_{cause_clean[:20]}"
        else:
            return f"Pattern_{fingerprint['structural'][:8]}"
    
    def _row_to_pattern(self, row) -> Dict:
        """Convert database row to pattern dict"""
        return {
            'id': row[0],
            'fingerprint_exact': row[1],
            'fingerprint_structural': row[2],
            'pattern_name': row[3],
            'error_type': row[4],
            'cause_of_dump': row[5],
            'stack_signature': row[6],
            'occurrence_count': row[7],
            'confidence_score': row[8],
            'verified_solutions': row[9] if isinstance(row[9], list) else json.loads(row[9] or '[]'),
            'prevention_strategies': row[10] if isinstance(row[10], list) else json.loads(row[10] or '[]')
        }

class SmartCrashMatcher:
    """Intelligent crash pattern matching"""
    
    def __init__(self, db_manager):
        self.db = db_manager
        self.fingerprint_gen = CrashFingerprintGenerator()
        self.similarity_engine = CrashSimilarityEngine()
    
    def match_crash(self, crash_content: str, threshold: float = 0.7) -> CrashMatch:
        """Match crash against known patterns"""
        # Generate fingerprint
        fingerprint = self.fingerprint_gen.generate(crash_content)
        
        # Try exact match first
        exact_match = self.find_exact_match(fingerprint['exact'])
        if exact_match:
            return CrashMatch(
                is_new=False,
                pattern_id=exact_match['id'],
                pattern_name=exact_match['pattern_name'],
                confidence=exact_match['confidence_score'],
                match_type='exact',
                verified_solutions=exact_match.get('verified_solutions', [])
            )
        
        # Try structural match
        structural_match = self.find_structural_match(fingerprint['structural'])
        if structural_match:
            return CrashMatch(
                is_new=False,
                pattern_id=structural_match['id'],
                pattern_name=structural_match['pattern_name'],
                confidence=structural_match['confidence_score'] * 0.9,
                match_type='structural',
                verified_solutions=structural_match.get('verified_solutions', [])
            )
        
        # Try similarity matching
        similar_matches = self.find_similar_patterns(crash_content, limit=5)
        if similar_matches:
            best_match = similar_matches[0]
            if best_match['similarity'] >= threshold:
                return CrashMatch(
                    is_new=False,
                    pattern_id=best_match['pattern']['id'],
                    pattern_name=best_match['pattern']['pattern_name'],
                    confidence=best_match['similarity'],
                    match_type=best_match['match_type'],
                    verified_solutions=best_match['pattern'].get('verified_solutions', [])
                )
        
        # No match - new pattern
        return CrashMatch(is_new=True, confidence=0.0)
    
    def find_exact_match(self, exact_fingerprint: str) -> Optional[Dict]:
        """Find pattern by exact fingerprint"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT * FROM crash_patterns 
                        WHERE fingerprint_exact = %s
                    """, (exact_fingerprint,))
                    
                    result = cur.fetchone()
                    if result:
                        return self._row_to_pattern(result)
                    return None
        except:
            return None
    
    def find_structural_match(self, structural_fingerprint: str) -> Optional[Dict]:
        """Find pattern by structural fingerprint"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT * FROM crash_patterns 
                        WHERE fingerprint_structural = %s
                        ORDER BY confidence_score DESC
                        LIMIT 1
                    """, (structural_fingerprint,))
                    
                    result = cur.fetchone()
                    if result:
                        return self._row_to_pattern(result)
                    return None
        except:
            return None
    
    def find_similar_patterns(self, crash_content: str, limit: int = 5) -> List[Dict]:
        """Find similar patterns using multiple strategies"""
        similar_patterns = []
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Get all patterns for comparison
                    cur.execute("""
                        SELECT * FROM crash_patterns 
                        ORDER BY occurrence_count DESC 
                        LIMIT 100
                    """)
                    
                    patterns = [self._row_to_pattern(row) for row in cur.fetchall()]
                    
                    # Calculate similarity for each pattern
                    for pattern in patterns:
                        # Reconstruct a minimal crash representation
                        pattern_content = f"Cause of Dump: {pattern['cause_of_dump']}\n{pattern['stack_signature']}"
                        
                        similarity, match_type = self.similarity_engine.calculate_overall_similarity(
                            crash_content, 
                            pattern_content
                        )
                        
                        if similarity > 0.5:  # Minimum threshold
                            similar_patterns.append({
                                'pattern': pattern,
                                'similarity': similarity,
                                'match_type': match_type
                            })
                    
                    # Sort by similarity
                    similar_patterns.sort(key=lambda x: x['similarity'], reverse=True)
                    
                    return similar_patterns[:limit]
        except Exception as e:
            print(f"Error finding similar patterns: {e}")
            return []
    
    def _row_to_pattern(self, row) -> Dict:
        """Convert database row to pattern dict"""
        return {
            'id': row[0],
            'fingerprint_exact': row[1],
            'fingerprint_structural': row[2],
            'pattern_name': row[3],
            'error_type': row[4],
            'cause_of_dump': row[5],
            'stack_signature': row[6],
            'occurrence_count': row[7],
            'confidence_score': row[8],
            'verified_solutions': row[9] if isinstance(row[9], list) else json.loads(row[9] or '[]'),
            'prevention_strategies': row[10] if isinstance(row[10], list) else json.loads(row[10] or '[]')
        }

# Analytics functions
def get_crash_pattern_stats(db_manager) -> Dict[str, Any]:
    """Get statistics about crash patterns"""
    try:
        with db_manager.get_connection() as conn:
            with conn.cursor() as cur:
                stats = {}
                
                # Total patterns
                cur.execute("SELECT COUNT(*) FROM crash_patterns")
                stats['total_patterns'] = cur.fetchone()[0]
                
                # Patterns by confidence
                cur.execute("""
                    SELECT 
                        CASE 
                            WHEN confidence_score >= 0.8 THEN 'High'
                            WHEN confidence_score >= 0.6 THEN 'Medium'
                            ELSE 'Low'
                        END as confidence_level,
                        COUNT(*)
                    FROM crash_patterns
                    GROUP BY confidence_level
                """)
                stats['confidence_distribution'] = dict(cur.fetchall())
                
                # Most common patterns
                cur.execute("""
                    SELECT pattern_name, occurrence_count, confidence_score
                    FROM crash_patterns
                    ORDER BY occurrence_count DESC
                    LIMIT 5
                """)
                stats['top_patterns'] = [
                    {'name': row[0], 'count': row[1], 'confidence': row[2]}
                    for row in cur.fetchall()
                ]
                
                # Recent patterns
                cur.execute("""
                    SELECT pattern_name, last_seen
                    FROM crash_patterns
                    WHERE last_seen > NOW() - INTERVAL '7 days'
                    ORDER BY last_seen DESC
                    LIMIT 5
                """)
                stats['recent_patterns'] = [
                    {'name': row[0], 'last_seen': row[1]}
                    for row in cur.fetchall()
                ]
                
                return stats
    except Exception as e:
        print(f"Error getting pattern stats: {e}")
        return {}