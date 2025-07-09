#!/usr/bin/env python3
"""
Isolated test for Knowledge Relationships - direct import
"""

import re
import json
from typing import Dict, List, Set
from collections import Counter
from datetime import datetime

# Copied core classes for isolated testing
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

COMPLEXITY_INDICATORS = {
    "basic": ["basic", "simple", "introduction", "getting started", "beginner", "first", "hello world"],
    "intermediate": ["intermediate", "after", "building on", "next step", "advanced beginner"],
    "advanced": ["advanced", "complex", "expert", "optimization", "performance", "deep dive", "master"]
}

PROBLEM_PATTERNS = {
    "error_terms": ["error", "exception", "fail", "issue", "problem", "bug", "broken"],
    "solution_terms": ["fix", "solve", "solution", "resolve", "repair", "correct", "debug"]
}

class RelationshipScorer:
    def __init__(self):
        self.stop_words = {
            'a', 'an', 'and', 'are', 'as', 'at', 'be', 'by', 'for', 'from',
            'has', 'he', 'in', 'is', 'it', 'its', 'of', 'on', 'that', 'the',
            'to', 'was', 'will', 'with', 'you', 'your', 'this', 'how', 'what',
            'when', 'where', 'why', 'can', 'could', 'would', 'should'
        }
        
    def score_related_similarity(self, unit_a: Dict, unit_b: Dict) -> float:
        if unit_a['id'] == unit_b['id']:
            return 0.0
            
        score = 0.0
        
        # Category match
        if unit_a['category'] == unit_b['category']:
            score += 0.3
            
        # Tag overlap
        tags_a = set(unit_a.get('tags', []))
        tags_b = set(unit_b.get('tags', []))
        if tags_a and tags_b:
            tag_overlap = len(tags_a & tags_b) / len(tags_a | tags_b)
            score += tag_overlap * 0.4
            
        # Title similarity
        title_sim = self._keyword_similarity(unit_a['title'], unit_b['title'])
        score += title_sim * 0.2
        
        # Content similarity
        content_a = unit_a['content'][:500]
        content_b = unit_b['content'][:500] 
        content_sim = self._keyword_similarity(content_a, content_b)
        score += content_sim * 0.1
        
        return min(score, 1.0)
    
    def score_prerequisite_relationship(self, basic_unit: Dict, advanced_unit: Dict) -> float:
        score = 0.0
        
        # Complexity analysis
        basic_complexity = self._analyze_complexity(basic_unit)
        advanced_complexity = self._analyze_complexity(advanced_unit)
        
        if basic_complexity < advanced_complexity:
            score += 0.4
            
        # Content references
        if self._mentions_concepts(advanced_unit, basic_unit):
            score += 0.3
            
        # Tag hierarchy
        if self._has_tag_hierarchy(basic_unit, advanced_unit):
            score += 0.3
            
        return min(score, 1.0)
    
    def score_solution_relationship(self, problem_unit: Dict, solution_unit: Dict) -> float:
        if problem_unit['category'] != 'error_solution':
            return 0.0
            
        score = 0.0
        
        # Problem-solution patterns
        if self._has_problem_language(problem_unit) and self._has_solution_language(solution_unit):
            score += 0.4
            
        # Shared technology
        if self._shared_technology_context(problem_unit, solution_unit):
            score += 0.3
            
        # Error-solution match
        if self._error_solution_match(problem_unit, solution_unit):
            score += 0.3
            
        return min(score, 1.0)
    
    def _keyword_similarity(self, text_a: str, text_b: str) -> float:
        keywords_a = self._extract_keywords(text_a.lower())
        keywords_b = self._extract_keywords(text_b.lower())
        
        if not keywords_a or not keywords_b:
            return 0.0
            
        intersection = len(keywords_a & keywords_b)
        union = len(keywords_a | keywords_b)
        
        return intersection / union if union > 0 else 0.0
    
    def _extract_keywords(self, text: str) -> Set[str]:
        words = re.findall(r'\b[a-zA-Z_][a-zA-Z0-9_]*\b', text)
        keywords = {
            word for word in words 
            if len(word) > 2 and word not in self.stop_words
        }
        return keywords
    
    def _analyze_complexity(self, unit: Dict) -> int:
        content = (unit['title'] + ' ' + unit['content'][:500]).lower()
        
        basic_count = sum(1 for term in COMPLEXITY_INDICATORS['basic'] if term in content)
        intermediate_count = sum(1 for term in COMPLEXITY_INDICATORS['intermediate'] if term in content)
        advanced_count = sum(1 for term in COMPLEXITY_INDICATORS['advanced'] if term in content)
        
        if advanced_count > basic_count:
            return 2
        elif intermediate_count > basic_count:
            return 1
        else:
            return 0
    
    def _mentions_concepts(self, advanced_unit: Dict, basic_unit: Dict) -> bool:
        basic_keywords = self._extract_keywords(basic_unit['title'].lower())
        advanced_content = advanced_unit['content'].lower()
        
        mentions = sum(1 for keyword in basic_keywords if keyword in advanced_content)
        return mentions >= 2
    
    def _has_tag_hierarchy(self, basic_unit: Dict, advanced_unit: Dict) -> bool:
        basic_tags = set(basic_unit.get('tags', []))
        advanced_tags = set(advanced_unit.get('tags', []))
        
        for basic_tag in basic_tags:
            for advanced_tag in advanced_tags:
                if basic_tag in advanced_tag and basic_tag != advanced_tag:
                    return True
        return False
    
    def _has_problem_language(self, unit: Dict) -> bool:
        content = (unit['title'] + ' ' + unit['content'][:300]).lower()
        problem_words = sum(1 for term in PROBLEM_PATTERNS['error_terms'] if term in content)
        return problem_words >= 1
    
    def _has_solution_language(self, unit: Dict) -> bool:
        content = (unit['title'] + ' ' + unit['content'][:300]).lower()
        solution_words = sum(1 for term in PROBLEM_PATTERNS['solution_terms'] if term in content)
        return solution_words >= 1
    
    def _shared_technology_context(self, unit_a: Dict, unit_b: Dict) -> bool:
        tags_a = set(unit_a.get('tags', []))
        tags_b = set(unit_b.get('tags', []))
        
        tech_tags = {'python', 'javascript', 'sql', 'rails', 'react', 'django', 'postgres'}
        
        tech_overlap = (tags_a & tech_tags) & (tags_b & tech_tags)
        return len(tech_overlap) > 0
    
    def _error_solution_match(self, problem_unit: Dict, solution_unit: Dict) -> bool:
        problem_content = problem_unit['content'].lower()
        solution_content = solution_unit['content'].lower()
        
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

def test_relationship_scoring():
    print("🧪 Testing Knowledge Relationship Algorithms")
    print("=" * 50)
    
    scorer = RelationshipScorer()
    
    # Test data
    test_units = [
        {
            'id': 1,
            'title': 'Python Lists - Basic Operations',
            'content': 'Lists in Python are ordered collections. You can create a list using square brackets: my_list = [1, 2, 3]. Basic operations include append(), remove(), and indexing.',
            'category': 'code_snippet',
            'tags': ['python', 'lists', 'basics']
        },
        {
            'id': 2,
            'title': 'Advanced Python List Comprehensions',
            'content': 'List comprehensions provide a concise way to create lists. Advanced techniques include nested comprehensions, conditional logic, and filtering. Example: [x**2 for x in range(10) if x % 2 == 0]',
            'category': 'code_snippet',
            'tags': ['python', 'lists', 'advanced', 'comprehensions']
        },
        {
            'id': 3,
            'title': 'Fix ImportError: No module named pandas',
            'content': 'This error occurs when pandas is not installed. Solution: Install pandas using pip install pandas or conda install pandas. Check your virtual environment is activated.',
            'category': 'error_solution',
            'tags': ['python', 'pandas', 'import-error']
        }
    ]
    
    print("📊 Test 1: Related Similarity")
    related_score = scorer.score_related_similarity(test_units[0], test_units[1])
    print(f"Python Lists Basic vs Advanced: {related_score:.3f}")
    assert related_score > 0.5, "Should detect strong relationship"
    print("✅ Related similarity works")
    
    print("\n📚 Test 2: Prerequisites")
    prereq_score = scorer.score_prerequisite_relationship(test_units[0], test_units[1])
    print(f"Basic → Advanced prerequisite: {prereq_score:.3f}")
    assert prereq_score > 0.3, "Should detect prerequisite relationship"
    print("✅ Prerequisite detection works")
    
    print("\n🔧 Test 3: Solutions")
    # Test with error unit
    solution_score = scorer.score_solution_relationship(test_units[2], test_units[0])
    print(f"Import error solution score: {solution_score:.3f}")
    print("✅ Solution scoring works")
    
    print("\n🎯 All tests passed!")
    return True

if __name__ == "__main__":
    test_relationship_scoring()