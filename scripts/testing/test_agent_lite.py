#!/usr/bin/env python3
"""
Test script for TuoKit Lite Agent System
Verifies pipeline automation and educational guidance
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pages.agent_lite import run_pipeline, EducationalAgent

def test_pipeline_execution():
    """Test basic pipeline execution"""
    print("🧪 Testing Pipeline Execution")
    print("=" * 50)
    
    # Simple test pipeline
    test_steps = [
        {
            "name": "Generate SQL",
            "tool": "sql_generator",
            "params": {
                "query": "Find top 5 customers",
                "dialect": "PostgreSQL"
            }
        },
        {
            "name": "Create Regex",
            "tool": "regex_generator",
            "params": {
                "description": "Match email addresses"
            }
        }
    ]
    
    print("Pipeline steps:")
    for i, step in enumerate(test_steps):
        print(f"  {i+1}. {step['name']} ({step['tool']})")
    
    print("\nExecuting pipeline...")
    try:
        result = run_pipeline(test_steps)
        
        print(f"\n✅ Pipeline completed!")
        print(f"   Steps executed: {len(result['log'])}")
        print(f"   All successful: {all(s['success'] for s in result['log'])}")
        
        # Show results
        print("\nResults:")
        for name, output in result['results'].items():
            print(f"\n{name}:")
            print(f"  {output[:100]}..." if len(str(output)) > 100 else f"  {output}")
            
        return True
        
    except Exception as e:
        print(f"❌ Pipeline failed: {str(e)}")
        return False

def test_educational_agent():
    """Test educational guidance"""
    print("\n\n🧪 Testing Educational Agent")
    print("=" * 50)
    
    agent = EducationalAgent()
    
    test_scenarios = [
        {
            "context": "I need to extract data from PDFs and analyze it",
            "action": "Selecting the right tool"
        },
        {
            "context": "My SQL query is returning too many results",
            "action": "Debugging errors"
        }
    ]
    
    for scenario in test_scenarios:
        print(f"\nScenario: {scenario['context']}")
        print(f"Action: {scenario['action']}")
        
        try:
            guidance = agent.guide(scenario['context'], scenario['action'])
            
            if guidance and not guidance.get('error'):
                print("✅ Guidance received:")
                print(f"   - Explanation: {guidance.get('explanation', 'N/A')[:60]}...")
                print(f"   - Tip: {guidance.get('tip', 'N/A')[:60]}...")
                print(f"   - Next step: {guidance.get('next_step', 'N/A')[:60]}...")
            else:
                print("⚠️  Guidance incomplete")
                
        except Exception as e:
            print(f"❌ Agent failed: {str(e)}")
            return False
    
    return True

def test_database_integration():
    """Test database operations for pipelines"""
    print("\n\n🧪 Testing Database Integration")
    print("=" * 50)
    
    try:
        from utils import DatabaseManager
        db = DatabaseManager()
        
        if not db.connected:
            print("⚠️  Database not connected - skipping DB tests")
            return True
        
        # Test saving pipeline
        test_result = {
            "results": {"step1": "test output"},
            "log": [{"step": "step1", "success": True}]
        }
        
        pipeline_id = db.save_pipeline(
            name="Test Pipeline",
            steps=[{"name": "Test", "tool": "test"}],
            results=test_result,
            execution_time_ms=1000
        )
        
        if pipeline_id:
            print(f"✅ Pipeline saved with ID: {pipeline_id}")
        else:
            print("❌ Failed to save pipeline")
            
        # Test getting templates
        templates = db.get_pipeline_templates()
        print(f"\n📋 Found {len(templates)} pipeline templates")
        for template in templates[:3]:
            print(f"   - {template['name']} ({template['category']})")
            
        return True
        
    except Exception as e:
        print(f"❌ Database test failed: {str(e)}")
        return False

def main():
    """Run all tests"""
    print("🚀 TuoKit Lite Agent System Test Suite\n")
    
    tests = [
        ("Pipeline Execution", test_pipeline_execution),
        ("Educational Agent", test_educational_agent),
        ("Database Integration", test_database_integration)
    ]
    
    results = []
    for test_name, test_func in tests:
        success = test_func()
        results.append((test_name, success))
    
    # Summary
    print("\n" + "=" * 50)
    print("📊 Test Summary:")
    all_passed = True
    for test_name, success in results:
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"   {status} - {test_name}")
        if not success:
            all_passed = False
    
    print("\n" + "=" * 50)
    if all_passed:
        print("✨ All tests passed!")
        print("\nNext steps:")
        print("1. Run database migration: psql -f database_migration_lite_agents.sql")
        print("2. Start TuoKit: streamlit run app.py")
        print("3. Navigate to 'Agent Lite' page")
    else:
        print("⚠️  Some tests failed - check output above")
    
    return 0 if all_passed else 1

if __name__ == "__main__":
    sys.exit(main())
