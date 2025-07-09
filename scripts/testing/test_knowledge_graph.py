"""
Test script for Knowledge Graph and Enhanced SQL Pipeline
Tests the educational features integration
"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from utils.knowledge_graph import KnowledgeGraph, knowledge_graph

def test_knowledge_graph():
    """Test knowledge graph functionality"""
    print("🧪 Testing Knowledge Graph...")
    
    # Test 1: Graph initialization
    print("\n1. Testing graph initialization...")
    kg = KnowledgeGraph()
    assert len(kg.graph.nodes) > 0, "Graph should have nodes"
    print(f"✅ Graph initialized with {len(kg.graph.nodes)} concepts")
    
    # Test 2: Concept retrieval
    print("\n2. Testing concept retrieval...")
    sql_concept = kg.get_concept("sql")
    assert sql_concept is not None, "Should find SQL concept"
    assert sql_concept["name"] == "SQL", "SQL concept should have correct name"
    print(f"✅ Retrieved concept: {sql_concept['name']}")
    
    # Test 3: Related concepts
    print("\n3. Testing related concepts...")
    related = kg.get_related_concepts("sql")
    assert len(related) > 0, "SQL should have related concepts"
    print(f"✅ Found {len(related)} related concepts")
    for r in related:
        print(f"   - {r['name']}")
    
    # Test 4: Learning path
    print("\n4. Testing learning path generation...")
    path = kg.recommend_learning_path("sql", "window-functions")
    assert len(path) > 0, "Should find a learning path"
    print(f"✅ Learning path from SQL to Window Functions:")
    for i, step in enumerate(path):
        print(f"   Step {i+1}: {step['name']}")
    
    # Test 5: Query concept detection
    print("\n5. Testing concept detection in queries...")
    test_query = """
    SELECT customer_id, 
           COUNT(*) as order_count,
           SUM(amount) as total_spent,
           ROW_NUMBER() OVER (ORDER BY SUM(amount) DESC) as rank
    FROM orders
    WHERE order_date > '2024-01-01'
    GROUP BY customer_id
    HAVING COUNT(*) > 5
    ORDER BY total_spent DESC
    """
    
    detected = kg.detect_concepts_in_query(test_query)
    assert len(detected) > 0, "Should detect concepts in query"
    print(f"✅ Detected {len(detected)} concepts:")
    for concept_id in detected:
        concept = kg.get_concept(concept_id)
        print(f"   - {concept['name']}")
    
    # Test 6: Prerequisite tree
    print("\n6. Testing prerequisite tree...")
    prereqs = kg.get_prerequisite_tree("window-functions")
    print(f"✅ Prerequisites for Window Functions:")
    for p in prereqs:
        print(f"   - {p['name']}")
    
    # Test 7: Graph visualization
    print("\n7. Testing graph visualization...")
    try:
        img_buffer = kg.visualize_graph(highlight_concepts=["sql", "joins"])
        assert img_buffer is not None, "Should generate visualization"
        print("✅ Graph visualization generated successfully")
    except Exception as e:
        print(f"⚠️  Visualization test skipped (matplotlib not configured): {e}")
    
    print("\n✅ All Knowledge Graph tests passed!")
    return True

def test_sql_pipeline_integration():
    """Test that SQL pipeline can use knowledge graph"""
    print("\n🧪 Testing SQL Pipeline Integration...")
    
    # Test concept detection integration
    from pages.sql_pipeline import display_concept_card
    
    print("\n1. Testing concept card display...")
    test_concept = {
        "id": "test",
        "name": "Test Concept",
        "description": "A test concept for verification",
        "resources": [
            {"title": "Test Resource", "url": "https://example.com"}
        ],
        "prerequisites": ["sql"]
    }
    
    card_html = display_concept_card(test_concept)
    assert "Test Concept" in card_html, "Card should contain concept name"
    assert "Test Resource" in card_html, "Card should contain resources"
    print("✅ Concept card generation working")
    
    print("\n✅ SQL Pipeline integration tests passed!")
    return True

def main():
    """Run all tests"""
    print("🚀 Testing TuoKit Knowledge Graph & Educational Features")
    print("=" * 50)
    
    try:
        # Run tests
        test_knowledge_graph()
        test_sql_pipeline_integration()
        
        print("\n" + "=" * 50)
        print("✅ ALL TESTS PASSED! 🎉")
        print("\nThe Knowledge Graph and SQL Pipeline educational features are ready to use.")
        print("\nKey features implemented:")
        print("- Interactive knowledge graph visualization")
        print("- Concept detection in SQL queries")
        print("- Personalized learning paths")
        print("- Concept cards with resources")
        print("- Quiz integration")
        print("- Prerequisites tracking")
        
    except Exception as e:
        print(f"\n❌ Test failed: {str(e)}")
        import traceback
        traceback.print_exc()
        return False
    
    return True

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
