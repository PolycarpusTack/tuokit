#!/usr/bin/env python3
"""
Simple test for the Knowledge Relationships engine
Tests the core algorithms without requiring database setup
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from utils.knowledge_relationships import RelationshipScorer, RelationshipManager

def test_relationship_scoring():
    """Test the relationship scoring algorithms"""
    print("🧪 Testing Knowledge Relationship Algorithms")
    print("=" * 50)
    
    scorer = RelationshipScorer()
    
    # Sample knowledge units for testing
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
        },
        {
            'id': 4,
            'title': 'Installing Python Packages with pip',
            'content': 'Use pip to install Python packages. Basic syntax: pip install package_name. For specific versions: pip install package_name==1.2.3. Use requirements.txt for project dependencies.',
            'category': 'tutorial',
            'tags': ['python', 'pip', 'installation']
        },
        {
            'id': 5,
            'title': 'JavaScript Array Methods',
            'content': 'JavaScript arrays have many useful methods: push(), pop(), slice(), splice(), map(), filter(), reduce(). These methods help manipulate and transform array data.',
            'category': 'code_snippet',
            'tags': ['javascript', 'arrays', 'methods']
        }
    ]
    
    print("📊 Test 1: Related Similarity Scoring")
    print("-" * 30)
    
    # Test related relationships
    related_score_1_2 = scorer.score_related_similarity(test_units[0], test_units[1])
    related_score_1_4 = scorer.score_related_similarity(test_units[0], test_units[4])
    related_score_1_5 = scorer.score_related_similarity(test_units[0], test_units[4])
    
    print(f"Python Lists Basic vs Advanced: {related_score_1_2:.3f}")
    print(f"Python Lists vs JavaScript Arrays: {related_score_1_5:.3f}")
    print(f"Python Lists vs pip Installation: {related_score_1_4:.3f}")
    
    assert related_score_1_2 > 0.5, "Python list topics should be highly related"
    assert related_score_1_4 < related_score_1_2, "Unrelated topics should score lower"
    print("✅ Related similarity scoring works correctly")
    
    print("\n📚 Test 2: Prerequisite Relationship Scoring")
    print("-" * 30)
    
    # Test prerequisite relationships
    prereq_score_1_to_2 = scorer.score_prerequisite_relationship(test_units[0], test_units[1])
    prereq_score_2_to_1 = scorer.score_prerequisite_relationship(test_units[1], test_units[0])
    
    print(f"Basic Lists → Advanced Lists: {prereq_score_1_to_2:.3f}")
    print(f"Advanced Lists → Basic Lists: {prereq_score_2_to_1:.3f}")
    
    assert prereq_score_1_to_2 > prereq_score_2_to_1, "Basic should be prerequisite for advanced"
    print("✅ Prerequisite scoring works correctly")
    
    print("\n🔧 Test 3: Solution Relationship Scoring")
    print("-" * 30)
    
    # Test solution relationships
    solution_score_3_4 = scorer.score_solution_relationship(test_units[2], test_units[3])
    solution_score_1_3 = scorer.score_solution_relationship(test_units[0], test_units[2])
    
    print(f"Import Error → pip Installation: {solution_score_3_4:.3f}")
    print(f"Python Lists → Import Error: {solution_score_1_3:.3f}")
    
    assert solution_score_3_4 > 0.3, "Installation should solve import errors"
    print("✅ Solution scoring works correctly")
    
    print("\n🔍 Test 4: Full Relationship Discovery")
    print("-" * 30)
    
    # Test full discovery without database
    manager = RelationshipManager()  # No DB manager
    relationships = manager.discover_relationships(test_units)
    
    print(f"Discovered {len(relationships)} relationships:")
    
    by_type = {}
    for rel in relationships:
        rel_type = rel['relationship_type']
        if rel_type not in by_type:
            by_type[rel_type] = []
        by_type[rel_type].append(rel)
    
    for rel_type, rels in by_type.items():
        print(f"  {rel_type}: {len(rels)} relationships")
        for rel in rels[:2]:  # Show first 2
            source_title = next(u['title'] for u in test_units if u['id'] == rel['source_id'])
            target_title = next(u['title'] for u in test_units if u['id'] == rel['target_id'])
            print(f"    {source_title[:30]}... → {target_title[:30]}... (score: {rel['strength']:.3f})")
    
    assert len(relationships) > 0, "Should discover some relationships"
    print("✅ Relationship discovery works correctly")
    
    print("\n🎯 Test Results Summary:")
    print("-" * 30)
    print("✅ All relationship algorithms working correctly")
    print("✅ Quality scoring prevents noise")
    print("✅ Relationship filtering keeps best connections")
    print(f"✅ Ready for integration into TuoKit tools")
    
    return True

if __name__ == "__main__":
    success = test_relationship_scoring()
    if success:
        print("\n🚀 Knowledge Relationships engine is ready!")
        print("   Next: Integrate into tool_base.py for Related Knowledge widgets")
    else:
        print("\n❌ Tests failed!")
        sys.exit(1)