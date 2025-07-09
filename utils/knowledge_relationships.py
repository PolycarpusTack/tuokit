"""
Knowledge Relationships Engine for TuoKit
Discovers and manages relationships between knowledge units using simple, effective algorithms
"""

import re
import json
from typing import Dict, List, Tuple, Optional, Set
from collections import Counter
from datetime import datetime
import logging

logger = logging.getLogger(__name__)

# Relationship types following TuoKit's simple approach
RELATIONSHIP_TYPES = {
    "related": {
        "name": "Related Topic",
        "description": "Similar or complementary knowledge",
        "strength_threshold": 0.3,
        "max_connections": 5
    },
    "prerequisite": {
        "name": "Prerequisite",
        "description": "Knowledge needed before this",
        "strength_threshold": 0.4,
        "max_connections": 3
    },
    "solution": {
        "name": "Solves Problem", 
        "description": "Solution to an error or problem",
        "strength_threshold": 0.5,
        "max_connections": 3
    }
}

# Keywords that indicate complexity levels
COMPLEXITY_INDICATORS = {
    "basic": ["basic", "simple", "introduction", "getting started", "beginner", "first", "hello world"],
    "intermediate": ["intermediate", "after", "building on", "next step", "advanced beginner"],
    "advanced": ["advanced", "complex", "expert", "optimization", "performance", "deep dive", "master"]
}

# Problem-solution patterns
PROBLEM_PATTERNS = {
    "error_terms": ["error", "exception", "fail", "issue", "problem", "bug", "broken"],
    "solution_terms": ["fix", "solve", "solution", "resolve", "repair", "correct", "debug"]
}

class RelationshipScorer:
    """Scores relationships between knowledge units using practical algorithms"""
    
    def __init__(self):
        self.stop_words = {
            'a', 'an', 'and', 'are', 'as', 'at', 'be', 'by', 'for', 'from',
            'has', 'he', 'in', 'is', 'it', 'its', 'of', 'on', 'that', 'the',
            'to', 'was', 'will', 'with', 'you', 'your', 'this', 'how', 'what',
            'when', 'where', 'why', 'can', 'could', 'would', 'should'
        }
        
    def score_related_similarity(self, unit_a: Dict, unit_b: Dict) -> float:
        """Score how related two knowledge units are (0.0 to 1.0)"""
        if unit_a['id'] == unit_b['id']:
            return 0.0
            
        score = 0.0
        
        # 1. Category match (strong signal)
        if unit_a['category'] == unit_b['category']:
            score += 0.3
            
        # 2. Tag overlap (very strong signal)
        tags_a = set(unit_a.get('tags', []))
        tags_b = set(unit_b.get('tags', []))
        if tags_a and tags_b:
            tag_overlap = len(tags_a & tags_b) / len(tags_a | tags_b)
            score += tag_overlap * 0.4
            
        # 3. Title keyword similarity
        title_sim = self._keyword_similarity(unit_a['title'], unit_b['title'])
        score += title_sim * 0.2
        
        # 4. Content keyword similarity (sample first 500 chars for performance)
        content_a = unit_a['content'][:500]
        content_b = unit_b['content'][:500] 
        content_sim = self._keyword_similarity(content_a, content_b)
        score += content_sim * 0.1
        
        return min(score, 1.0)
    
    def score_prerequisite_relationship(self, basic_unit: Dict, advanced_unit: Dict) -> float:
        """Score if basic_unit is a prerequisite for advanced_unit"""
        score = 0.0
        
        # 1. Complexity level analysis
        basic_complexity = self._analyze_complexity(basic_unit)
        advanced_complexity = self._analyze_complexity(advanced_unit)
        
        if basic_complexity < advanced_complexity:
            score += 0.4
            
        # 2. Content references (advanced unit mentions basic concepts)
        if self._mentions_concepts(advanced_unit, basic_unit):
            score += 0.3
            
        # 3. Tag hierarchy (python → python-advanced, sql → sql-joins)
        if self._has_tag_hierarchy(basic_unit, advanced_unit):
            score += 0.3
            
        return min(score, 1.0)
    
    def score_solution_relationship(self, problem_unit: Dict, solution_unit: Dict) -> float:
        """Score if solution_unit solves problems in problem_unit"""
        if problem_unit['category'] != 'error_solution':
            return 0.0
            
        score = 0.0
        
        # 1. Problem-solution language patterns
        if self._has_problem_language(problem_unit) and self._has_solution_language(solution_unit):
            score += 0.4
            
        # 2. Shared technology context
        if self._shared_technology_context(problem_unit, solution_unit):
            score += 0.3
            
        # 3. Error type matches solution domain
        if self._error_solution_match(problem_unit, solution_unit):
            score += 0.3
            
        return min(score, 1.0)
    
    def _keyword_similarity(self, text_a: str, text_b: str) -> float:
        """Calculate keyword similarity between two texts"""
        # Extract keywords (remove stop words, normalize)
        keywords_a = self._extract_keywords(text_a.lower())
        keywords_b = self._extract_keywords(text_b.lower())
        
        if not keywords_a or not keywords_b:
            return 0.0
            
        # Calculate Jaccard similarity
        intersection = len(keywords_a & keywords_b)
        union = len(keywords_a | keywords_b)
        
        return intersection / union if union > 0 else 0.0
    
    def _extract_keywords(self, text: str) -> Set[str]:
        """Extract meaningful keywords from text"""
        # Simple tokenization and filtering
        words = re.findall(r'\b[a-zA-Z_][a-zA-Z0-9_]*\b', text)
        keywords = {
            word for word in words 
            if len(word) > 2 and word not in self.stop_words
        }
        return keywords
    
    def _analyze_complexity(self, unit: Dict) -> int:
        """Analyze complexity level (0=basic, 1=intermediate, 2=advanced)"""
        content = (unit['title'] + ' ' + unit['content'][:500]).lower()
        
        # Count complexity indicators
        basic_count = sum(1 for term in COMPLEXITY_INDICATORS['basic'] if term in content)
        intermediate_count = sum(1 for term in COMPLEXITY_INDICATORS['intermediate'] if term in content)
        advanced_count = sum(1 for term in COMPLEXITY_INDICATORS['advanced'] if term in content)
        
        # Simple scoring
        if advanced_count > basic_count:
            return 2
        elif intermediate_count > basic_count:
            return 1
        else:
            return 0
    
    def _mentions_concepts(self, advanced_unit: Dict, basic_unit: Dict) -> bool:
        """Check if advanced unit mentions concepts from basic unit"""
        basic_keywords = self._extract_keywords(basic_unit['title'].lower())
        advanced_content = advanced_unit['content'].lower()
        
        # Check if basic concepts are mentioned in advanced content
        mentions = sum(1 for keyword in basic_keywords if keyword in advanced_content)
        return mentions >= 2  # Threshold for meaningful mention
    
    def _has_tag_hierarchy(self, basic_unit: Dict, advanced_unit: Dict) -> bool:
        """Check for tag hierarchy patterns"""
        basic_tags = set(basic_unit.get('tags', []))
        advanced_tags = set(advanced_unit.get('tags', []))
        
        # Look for patterns like: python → python-advanced, sql → sql-joins
        for basic_tag in basic_tags:
            for advanced_tag in advanced_tags:
                if basic_tag in advanced_tag and basic_tag != advanced_tag:
                    return True
        return False
    
    def _has_problem_language(self, unit: Dict) -> bool:
        """Check if unit describes a problem/error"""
        content = (unit['title'] + ' ' + unit['content'][:300]).lower()
        problem_words = sum(1 for term in PROBLEM_PATTERNS['error_terms'] if term in content)
        return problem_words >= 1
    
    def _has_solution_language(self, unit: Dict) -> bool:
        """Check if unit provides solutions"""
        content = (unit['title'] + ' ' + unit['content'][:300]).lower()
        solution_words = sum(1 for term in PROBLEM_PATTERNS['solution_terms'] if term in content)
        return solution_words >= 1
    
    def _shared_technology_context(self, unit_a: Dict, unit_b: Dict) -> bool:
        """Check if units share technology context"""
        tags_a = set(unit_a.get('tags', []))
        tags_b = set(unit_b.get('tags', []))
        
        # Technology tags that matter for problem-solution relationships
        tech_tags = {'python', 'javascript', 'sql', 'rails', 'react', 'django', 'postgres'}
        
        tech_overlap = (tags_a & tech_tags) & (tags_b & tech_tags)
        return len(tech_overlap) > 0
    
    def _error_solution_match(self, problem_unit: Dict, solution_unit: Dict) -> bool:
        """Check if error type matches solution domain"""
        # Simple heuristic: if error mentions technology, solution should too
        problem_content = problem_unit['content'].lower()
        solution_content = solution_unit['content'].lower()
        
        # Common error-solution patterns
        patterns = {
            'import': ['install', 'pip', 'requirements'],
            'syntax': ['correct', 'fix', 'proper'],
            'connection': ['configure', 'setup', 'credentials'],
            'permission': ['chmod', 'access', 'rights']
        }
        
        for error_type, solution_keywords in patterns.items():
            if error_type in problem_content:
                if any(keyword in solution_content for keyword in solution_keywords):
                    return True
        
        return False

class RelationshipManager:
    """Manages relationship discovery and storage"""
    
    def __init__(self, db_manager=None):
        self.db = db_manager
        self.scorer = RelationshipScorer()
        
    def discover_relationships(self, knowledge_units: List[Dict]) -> List[Dict]:
        """Discover relationships between knowledge units"""
        relationships = []
        
        for i, unit_a in enumerate(knowledge_units):
            for j, unit_b in enumerate(knowledge_units[i+1:], i+1):
                # Find related relationships
                related_score = self.scorer.score_related_similarity(unit_a, unit_b)
                if related_score >= RELATIONSHIP_TYPES['related']['strength_threshold']:
                    relationships.append({
                        'source_id': unit_a['id'],
                        'target_id': unit_b['id'],
                        'relationship_type': 'related',
                        'strength': related_score,
                        'created_at': datetime.now()
                    })
                
                # Find prerequisite relationships (both directions)
                prereq_score = self.scorer.score_prerequisite_relationship(unit_a, unit_b)
                if prereq_score >= RELATIONSHIP_TYPES['prerequisite']['strength_threshold']:
                    relationships.append({
                        'source_id': unit_a['id'],
                        'target_id': unit_b['id'],
                        'relationship_type': 'prerequisite',
                        'strength': prereq_score,
                        'created_at': datetime.now()
                    })
                
                # Check reverse prerequisite
                reverse_prereq_score = self.scorer.score_prerequisite_relationship(unit_b, unit_a)
                if reverse_prereq_score >= RELATIONSHIP_TYPES['prerequisite']['strength_threshold']:
                    relationships.append({
                        'source_id': unit_b['id'],
                        'target_id': unit_a['id'],
                        'relationship_type': 'prerequisite',
                        'strength': reverse_prereq_score,
                        'created_at': datetime.now()
                    })
                
                # Find solution relationships
                solution_score = self.scorer.score_solution_relationship(unit_a, unit_b)
                if solution_score >= RELATIONSHIP_TYPES['solution']['strength_threshold']:
                    relationships.append({
                        'source_id': unit_a['id'],
                        'target_id': unit_b['id'],
                        'relationship_type': 'solution',
                        'strength': solution_score,
                        'created_at': datetime.now()
                    })
        
        # Sort by strength and limit per type
        return self._filter_best_relationships(relationships)
    
    def discover_incremental_relationships(self, new_unit: Dict, existing_units: List[Dict]) -> List[Dict]:
        """OPTIMIZED: Discover relationships for one new unit against existing units (O(n) instead of O(n²))"""
        relationships = []
        
        for existing_unit in existing_units:
            # Find related relationships (bidirectional)
            related_score = self.scorer.score_related_similarity(new_unit, existing_unit)
            if related_score >= RELATIONSHIP_TYPES['related']['strength_threshold']:
                relationships.append({
                    'source_id': new_unit['id'],
                    'target_id': existing_unit['id'],
                    'relationship_type': 'related',
                    'strength': related_score,
                    'created_at': datetime.now()
                })
                # Also add reverse relationship for related (symmetric)
                relationships.append({
                    'source_id': existing_unit['id'],
                    'target_id': new_unit['id'],
                    'relationship_type': 'related',
                    'strength': related_score,
                    'created_at': datetime.now()
                })
            
            # Find prerequisite relationships (both directions)
            prereq_score_new_to_existing = self.scorer.score_prerequisite_relationship(new_unit, existing_unit)
            if prereq_score_new_to_existing >= RELATIONSHIP_TYPES['prerequisite']['strength_threshold']:
                relationships.append({
                    'source_id': new_unit['id'],
                    'target_id': existing_unit['id'],
                    'relationship_type': 'prerequisite',
                    'strength': prereq_score_new_to_existing,
                    'created_at': datetime.now()
                })
            
            prereq_score_existing_to_new = self.scorer.score_prerequisite_relationship(existing_unit, new_unit)
            if prereq_score_existing_to_new >= RELATIONSHIP_TYPES['prerequisite']['strength_threshold']:
                relationships.append({
                    'source_id': existing_unit['id'],
                    'target_id': new_unit['id'],
                    'relationship_type': 'prerequisite',
                    'strength': prereq_score_existing_to_new,
                    'created_at': datetime.now()
                })
            
            # Find solution relationships (both directions)
            solution_score_new_to_existing = self.scorer.score_solution_relationship(new_unit, existing_unit)
            if solution_score_new_to_existing >= RELATIONSHIP_TYPES['solution']['strength_threshold']:
                relationships.append({
                    'source_id': new_unit['id'],
                    'target_id': existing_unit['id'],
                    'relationship_type': 'solution',
                    'strength': solution_score_new_to_existing,
                    'created_at': datetime.now()
                })
                
            solution_score_existing_to_new = self.scorer.score_solution_relationship(existing_unit, new_unit)
            if solution_score_existing_to_new >= RELATIONSHIP_TYPES['solution']['strength_threshold']:
                relationships.append({
                    'source_id': existing_unit['id'],
                    'target_id': new_unit['id'],
                    'relationship_type': 'solution',
                    'strength': solution_score_existing_to_new,
                    'created_at': datetime.now()
                })
        
        # Filter and return best relationships
        return self._filter_best_relationships(relationships)
    
    def _filter_best_relationships(self, relationships: List[Dict]) -> List[Dict]:
        """FIXED: Improved filtering with proper deduplication and quality ranking"""
        if not relationships:
            return []
        
        # Step 1: Remove exact duplicates and invalid relationships
        seen_relationships = set()
        deduplicated = []
        
        for rel in relationships:
            # Create unique key for relationship (normalize direction for symmetric relationships)
            if rel['relationship_type'] == 'related':
                # For symmetric relationships, normalize the direction
                source_id = min(rel['source_id'], rel['target_id'])
                target_id = max(rel['source_id'], rel['target_id'])
                rel_key = (source_id, target_id, rel['relationship_type'])
            else:
                # For asymmetric relationships, keep original direction
                rel_key = (rel['source_id'], rel['target_id'], rel['relationship_type'])
            
            if rel_key not in seen_relationships:
                seen_relationships.add(rel_key)
                
                # Normalize the relationship for symmetric types
                if rel['relationship_type'] == 'related' and rel['source_id'] != source_id:
                    rel['source_id'], rel['target_id'] = target_id, source_id
                
                deduplicated.append(rel)
        
        logger.info(f"Deduplication: {len(relationships)} → {len(deduplicated)} relationships")
        
        # Step 2: Group by source and relationship type for filtering
        grouped = {}
        for rel in deduplicated:
            key = (rel['source_id'], rel['relationship_type'])
            if key not in grouped:
                grouped[key] = []
            grouped[key].append(rel)
        
        # Step 3: Keep top N relationships per type, sorted by quality
        filtered = []
        for (source_id, rel_type), rels in grouped.items():
            max_connections = RELATIONSHIP_TYPES[rel_type]['max_connections']
            
            # Sort by strength (primary) and then by target quality (secondary)
            top_rels = sorted(
                rels, 
                key=lambda r: (r['strength'], r.get('target_quality_score', 0)), 
                reverse=True
            )[:max_connections]
            
            filtered.extend(top_rels)
        
        logger.info(f"Filtering: kept {len(filtered)} best relationships from {len(deduplicated)}")
        return filtered
    
    def store_relationships(self, relationships: List[Dict]) -> int:
        """Store relationships in database with transaction safety"""
        if not self.db:
            logger.warning("No database connection available for storing relationships")
            return 0
        
        if not relationships:
            logger.info("No relationships to store")
            return 0
        
        stored_count = 0
        try:
            with self.db.get_connection() as conn:
                # Start transaction
                conn.autocommit = False
                
                try:
                    with conn.cursor() as cur:
                        # Validate relationships before storing
                        validated_relationships = self._validate_relationships(relationships)
                        
                        if not validated_relationships:
                            logger.warning("No valid relationships after validation")
                            return 0
                        
                        # Batch insert for better performance
                        for batch in self._batch_relationships(validated_relationships, batch_size=50):
                            # Use ON CONFLICT to handle duplicates gracefully
                            insert_values = []
                            for rel in batch:
                                insert_values.append((
                                    rel['source_id'],
                                    rel['target_id'],
                                    rel['relationship_type'],
                                    rel['strength'],
                                    rel['created_at']
                                ))
                            
                            cur.executemany("""
                                INSERT INTO knowledge_links 
                                (source_id, target_id, relationship_type, strength, created_at)
                                VALUES (%s, %s, %s, %s, %s)
                                ON CONFLICT (source_id, target_id, relationship_type) 
                                DO UPDATE SET 
                                    strength = GREATEST(knowledge_links.strength, EXCLUDED.strength),
                                    updated_at = CURRENT_TIMESTAMP
                            """, insert_values)
                            
                            stored_count += cur.rowcount
                    
                    # Commit transaction
                    conn.commit()
                    logger.info(f"Successfully stored {stored_count} relationships")
                    
                    # Log the relationship discovery
                    if stored_count > 0:
                        self._log_relationship_discovery(stored_count, len(relationships))
                    
                except Exception as e:
                    # Rollback on error
                    conn.rollback()
                    logger.error(f"Transaction failed, rolled back: {e}")
                    raise
                    
                finally:
                    conn.autocommit = True
                
        except Exception as e:
            logger.error(f"Failed to store relationships: {e}", exc_info=True)
            
        return stored_count
    
    def _validate_relationships(self, relationships: List[Dict]) -> List[Dict]:
        """Validate relationships before storing"""
        validated = []
        
        for rel in relationships:
            # Check required fields
            if not all(key in rel for key in ['source_id', 'target_id', 'relationship_type', 'strength']):
                logger.warning(f"Relationship missing required fields: {rel}")
                continue
            
            # Validate strength range
            if not (0.0 <= rel['strength'] <= 1.0):
                logger.warning(f"Invalid strength value {rel['strength']}, skipping relationship")
                continue
            
            # Validate relationship type
            if rel['relationship_type'] not in RELATIONSHIP_TYPES:
                logger.warning(f"Invalid relationship type {rel['relationship_type']}, skipping")
                continue
            
            # Validate no self-reference
            if rel['source_id'] == rel['target_id']:
                logger.warning(f"Self-referential relationship detected, skipping")
                continue
            
            # Add created_at if missing
            if 'created_at' not in rel:
                rel['created_at'] = datetime.now()
            
            validated.append(rel)
        
        logger.info(f"Validated {len(validated)}/{len(relationships)} relationships")
        return validated
    
    def _batch_relationships(self, relationships: List[Dict], batch_size: int = 50):
        """Split relationships into batches for processing"""
        for i in range(0, len(relationships), batch_size):
            yield relationships[i:i + batch_size]
    
    def get_related_knowledge(self, knowledge_id: int, relationship_types: List[str] = None) -> List[Dict]:
        """Get knowledge units related to the given unit (bidirectional search)"""
        if not self.db:
            return []
        
        relationship_types = relationship_types or list(RELATIONSHIP_TYPES.keys())
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # FIXED: Get relationships in BOTH directions
                    cur.execute("""
                        -- Forward relationships (this unit is source)
                        SELECT 
                            kl.relationship_type,
                            kl.strength,
                            ku.id,
                            ku.title,
                            ku.category,
                            ku.quality_score,
                            LEFT(ku.content, 200) as content_preview,
                            'outgoing' as direction
                        FROM knowledge_links kl
                        JOIN knowledge_units ku ON kl.target_id = ku.id
                        WHERE kl.source_id = %s 
                        AND kl.relationship_type = ANY(%s)
                        
                        UNION ALL
                        
                        -- Reverse relationships (this unit is target)
                        SELECT 
                            CASE 
                                WHEN kl.relationship_type = 'prerequisite' THEN 'builds_on'
                                WHEN kl.relationship_type = 'solution' THEN 'has_problem'
                                ELSE kl.relationship_type
                            END as relationship_type,
                            kl.strength,
                            ku.id,
                            ku.title,
                            ku.category,
                            ku.quality_score,
                            LEFT(ku.content, 200) as content_preview,
                            'incoming' as direction
                        FROM knowledge_links kl
                        JOIN knowledge_units ku ON kl.source_id = ku.id
                        WHERE kl.target_id = %s 
                        AND kl.relationship_type = ANY(%s)
                        
                        ORDER BY strength DESC, quality_score DESC
                        LIMIT 15
                    """, (knowledge_id, relationship_types, knowledge_id, relationship_types))
                    
                    related = []
                    seen_ids = set()  # Prevent duplicates
                    
                    for row in cur.fetchall():
                        knowledge_unit_id = row[2]
                        if knowledge_unit_id not in seen_ids:
                            seen_ids.add(knowledge_unit_id)
                            related.append({
                                'relationship_type': row[0],
                                'strength': float(row[1]),
                                'direction': row[7],
                                'knowledge': {
                                    'id': row[2],
                                    'title': row[3],
                                    'category': row[4],
                                    'quality_score': row[5],
                                    'content_preview': row[6]
                                }
                            })
                    
                    return related[:10]  # Top 10 after deduplication
                    
        except Exception as e:
            logger.error(f"Failed to get related knowledge: {e}")
            return []
    
    def _log_relationship_discovery(self, stored_count: int, total_discovered: int):
        """Log relationship discovery to maintenance log"""
        if not self.db:
            return
            
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO knowledge_maintenance_log 
                        (action, details, units_affected)
                        VALUES ('relationship_discovery', %s, %s)
                    """, (
                        json.dumps({
                            'relationships_discovered': total_discovered,
                            'relationships_stored': stored_count,
                            'discovery_timestamp': datetime.now().isoformat()
                        }),
                        stored_count
                    ))
        except Exception as e:
            logger.error(f"Failed to log relationship discovery: {e}")

# Convenience functions
def discover_and_store_relationships(db_manager) -> int:
    """Discover relationships for all knowledge units and store them (full scan)"""
    if not db_manager:
        return 0
    
    try:
        # Get all knowledge units
        with db_manager.get_connection() as conn:
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT id, title, content, category, tags, quality_score
                    FROM knowledge_units
                    WHERE quality_score >= 30
                    ORDER BY quality_score DESC
                    LIMIT 100
                """)
                
                knowledge_units = []
                for row in cur.fetchall():
                    knowledge_units.append({
                        'id': row[0],
                        'title': row[1],
                        'content': row[2],
                        'category': row[3],
                        'tags': row[4] or [],
                        'quality_score': row[5]
                    })
        
        if len(knowledge_units) < 2:
            return 0
        
        # Discover and store relationships
        manager = RelationshipManager(db_manager)
        relationships = manager.discover_relationships(knowledge_units)
        stored_count = manager.store_relationships(relationships)
        
        logger.info(f"Full relationship discovery complete: {stored_count} relationships stored")
        return stored_count
        
    except Exception as e:
        logger.error(f"Relationship discovery failed: {e}")
        return 0

def discover_relationships_for_new_knowledge(db_manager, knowledge_id: int) -> int:
    """FIXED: Incremental discovery - only compare new knowledge unit with existing ones"""
    if not db_manager or not knowledge_id:
        return 0
    
    try:
        with db_manager.get_connection() as conn:
            with conn.cursor() as cur:
                # Get the new knowledge unit
                cur.execute("""
                    SELECT id, title, content, category, tags, quality_score
                    FROM knowledge_units
                    WHERE id = %s AND quality_score >= 30
                """, (knowledge_id,))
                
                new_unit_row = cur.fetchone()
                if not new_unit_row:
                    logger.info(f"Knowledge unit {knowledge_id} not found or quality too low")
                    return 0
                
                new_unit = {
                    'id': new_unit_row[0],
                    'title': new_unit_row[1],
                    'content': new_unit_row[2],
                    'category': new_unit_row[3],
                    'tags': new_unit_row[4] or [],
                    'quality_score': new_unit_row[5]
                }
                
                # Get existing knowledge units that don't already have relationships with this unit
                cur.execute("""
                    SELECT DISTINCT ku.id, ku.title, ku.content, ku.category, ku.tags, ku.quality_score
                    FROM knowledge_units ku
                    WHERE ku.id != %s 
                    AND ku.quality_score >= 30
                    AND ku.id NOT IN (
                        -- Exclude units that already have relationships with new unit
                        SELECT DISTINCT source_id FROM knowledge_links WHERE target_id = %s
                        UNION
                        SELECT DISTINCT target_id FROM knowledge_links WHERE source_id = %s
                    )
                    ORDER BY ku.quality_score DESC
                    LIMIT 50
                """, (knowledge_id, knowledge_id, knowledge_id))
                
                existing_units = []
                for row in cur.fetchall():
                    existing_units.append({
                        'id': row[0],
                        'title': row[1],
                        'content': row[2],
                        'category': row[3],
                        'tags': row[4] or [],
                        'quality_score': row[5]
                    })
                
                if not existing_units:
                    logger.info(f"No existing units to compare with knowledge {knowledge_id}")
                    return 0
                
                # Incremental discovery: compare new unit with existing units only
                manager = RelationshipManager(db_manager)
                relationships = manager.discover_incremental_relationships(new_unit, existing_units)
                stored_count = manager.store_relationships(relationships)
                
                logger.info(f"Incremental discovery for unit {knowledge_id}: {stored_count} new relationships")
                return stored_count
                
    except Exception as e:
        logger.error(f"Incremental relationship discovery failed: {e}")
        return 0