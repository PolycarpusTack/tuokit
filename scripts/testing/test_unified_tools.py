"""
Quick test script for unified tools
Run this to verify the new sql_suite and agent_hub work correctly
"""

import sys
sys.path.append('.')  # Ensure we can import from current directory

def test_sql_tools():
    """Test the SQL tools backend"""
    print("Testing SQL Tools...")
    try:
        # # # from utils.sql_tools import  # Module archived  # Module archived  # Module archived SQLTools
        
        # Test generation
        sql = SQLTools.generate("show all customers", "postgresql")
        print(f"✓ Generate: {sql[:50]}...")
        
        # Test validation
        is_valid, msg = SQLTools.validate(sql)
        print(f"✓ Validate: {is_valid} - {msg}")
        
        # Test explain
        explanation = SQLTools.explain("SELECT * FROM users WHERE age > 18")
        print(f"✓ Explain: {explanation[:50]}...")
        
        print("✅ SQL Tools working!\n")
        return True
    except Exception as e:
        print(f"❌ SQL Tools error: {e}\n")
        return False

def test_agent_pipeline():
    """Test the agent pipeline execution"""
    print("Testing Agent Pipeline...")
    try:
        from pages.agent_hub import run_simple_pipeline
        
        # Test simple pipeline
        steps = [{
            "name": "Test Step",
            "tool": "sql_generator",
            "params": {"query": "count all orders", "dialect": "postgresql"}
        }]
        
        results = run_simple_pipeline(steps)
        print(f"✓ Pipeline executed: {len(results['log'])} steps")
        print(f"✓ Results: {list(results['results'].keys())}")
        
        print("✅ Agent Pipeline working!\n")
        return True
    except Exception as e:
        print(f"❌ Agent Pipeline error: {e}\n")
        return False

def test_imports():
    """Test that new pages can be imported"""
    print("Testing Page Imports...")
    try:
        import pages.sql_suite
        print("✓ sql_suite.py imports successfully")
        
        import pages.agent_hub
        print("✓ agent_hub.py imports successfully")
        
        print("✅ All imports working!\n")
        return True
    except Exception as e:
        print(f"❌ Import error: {e}\n")
        return False

def main():
    print("🧪 TuoKit Unified Tools Test")
    print("=" * 40)
    
    all_passed = True
    
    # Run tests
    all_passed &= test_sql_tools()
    all_passed &= test_agent_pipeline()
    all_passed &= test_imports()
    
    if all_passed:
        print("✅ All tests passed! Safe to run consolidation.")
    else:
        print("❌ Some tests failed. Fix issues before consolidating.")
    
    return all_passed

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)