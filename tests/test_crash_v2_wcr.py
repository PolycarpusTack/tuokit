"""Test crash analyzer v2 with real WCR file"""
import os
import sys
from pathlib import Path

os.environ['STREAMLIT_SERVER_HEADLESS'] = 'true'

# Add UTF-8 support for Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

try:
    from toolkits.crash_analyzer_v2.analyzers.quick_triage import QuickTriageAnalyzer
    from toolkits.crash_analyzer_v2.analyzers.root_cause import RootCauseAnalyzer
    
    # Find a small WCR file
    wcr_files = list(Path("crash_examples").glob("WCR*.txt"))
    if not wcr_files:
        print("No WCR files found!")
        exit(1)
    
    # Pick the smallest file
    test_file = min(wcr_files, key=lambda f: f.stat().st_size)
    print(f"Testing with: {test_file.name} ({test_file.stat().st_size:,} bytes)")
    
    # Read content
    with open(test_file, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read()
    
    print("\n=== Quick Triage Analysis ===")
    analyzer = QuickTriageAnalyzer()
    results = analyzer.analyze(content, test_file.name)
    
    print(f"Severity: {results.get('severity', 'Unknown')}")
    print(f"Primary Error: {results.get('primary_error', {}).get('type', 'Unknown')}")
    print(f"Total Errors: {results.get('error_summary', {}).get('total_errors', 0)}")
    print(f"Immediate Action: {results.get('immediate_action', 'Unknown')}")
    
    # Show error timeline
    if results.get('error_timeline'):
        print("\nError Timeline (first 5):")
        for error in results['error_timeline'][:5]:
            print(f"  - {error}")
    
    # Try root cause if we have Ollama
    try:
        from utils.ollama import get_available_models
        models = get_available_models()
        if models:
            print("\n=== Root Cause Analysis (with AI) ===")
            root_analyzer = RootCauseAnalyzer()
            root_results = root_analyzer.analyze(content, test_file.name, selected_model=models[0])
            
            print(f"Root Causes Found: {len(root_results.get('root_causes', []))}")
            for i, cause in enumerate(root_results.get('root_causes', [])[:3], 1):
                print(f"  {i}. {cause.get('cause', 'Unknown')} (confidence: {cause.get('confidence', 0):.1%})")
    except:
        print("\nSkipping AI analysis (Ollama not available)")
    
    print("\n✅ Test completed successfully!")
    
except Exception as e:
    print(f"❌ Error: {e}")
    import traceback
    traceback.print_exc()