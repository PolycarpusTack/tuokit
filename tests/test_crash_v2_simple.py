"""Simple test for crash analyzer v2"""
import os
os.environ['STREAMLIT_SERVER_HEADLESS'] = 'true'

try:
    from toolkits.crash_analyzer_v2.analyzers.quick_triage import QuickTriageAnalyzer
    print("Quick Triage import: OK")
    
    # Test with simple content
    analyzer = QuickTriageAnalyzer()
    test_content = """
    Error: NullPointerException
    at com.example.MyClass.method(MyClass.java:42)
    at com.example.Main.main(Main.java:10)
    """
    
    results = analyzer.analyze(test_content, "test.log")
    print(f"Analysis severity: {results.get('severity', 'Unknown')}")
    print(f"Primary error: {results.get('primary_error', {}).get('type', 'Unknown')}")
    print("Quick Triage test: PASS")
    
except Exception as e:
    print(f"Error: {e}")
    import traceback
    traceback.print_exc()