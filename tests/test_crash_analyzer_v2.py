#!/usr/bin/env python
"""
Test script for Crash Analyzer V2
Tests all analysis methods with sample files
"""
import os
import sys
import time
from pathlib import Path

# Set UTF-8 encoding for Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

# Suppress Streamlit warnings for tests
os.environ['STREAMLIT_SERVER_HEADLESS'] = 'true'

from toolkits.crash_analyzer_v2 import CrashAnalyzerV2
from toolkits.crash_analyzer_v2.analyzers import (
    QuickTriageAnalyzer,
    RootCauseAnalyzer,
    StrategicSamplingAnalyzer,
    DeepForensicAnalyzer
)

def test_analyzer_imports():
    """Test that all analyzers can be imported"""
    print("Testing analyzer imports...")
    try:
        analyzer = CrashAnalyzerV2()
        print("✓ CrashAnalyzerV2 imported successfully")
        
        # Test individual analyzers
        quick = QuickTriageAnalyzer()
        print("✓ QuickTriageAnalyzer imported successfully")
        
        root = RootCauseAnalyzer()
        print("✓ RootCauseAnalyzer imported successfully")
        
        sampling = StrategicSamplingAnalyzer()
        print("✓ StrategicSamplingAnalyzer imported successfully")
        
        forensic = DeepForensicAnalyzer()
        print("✓ DeepForensicAnalyzer imported successfully")
        
        return True
    except Exception as e:
        print(f"✗ Import error: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_quick_triage(content, filename):
    """Test Quick Triage analysis"""
    print("\n--- Testing Quick Triage Analysis ---")
    try:
        analyzer = QuickTriageAnalyzer()
        start_time = time.time()
        results = analyzer.analyze(content, filename)
        elapsed = time.time() - start_time
        
        print(f"✓ Analysis completed in {elapsed:.2f}s")
        print(f"  - Severity: {results.get('severity', 'Unknown')}")
        print(f"  - Primary Error: {results.get('primary_error', {}).get('type', 'Unknown')}")
        print(f"  - Error Count: {results.get('error_summary', {}).get('total_errors', 0)}")
        print(f"  - Has Stack Trace: {results.get('stack_trace_summary', {}).get('has_stack_trace', False)}")
        
        return True
    except Exception as e:
        print(f"✗ Quick Triage failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_root_cause(content, filename, model=None):
    """Test Root Cause analysis"""
    print("\n--- Testing Root Cause Analysis ---")
    if not model:
        print("⚠ No model specified, using algorithmic fallback")
    
    try:
        analyzer = RootCauseAnalyzer()
        start_time = time.time()
        results = analyzer.analyze(content, filename, selected_model=model)
        elapsed = time.time() - start_time
        
        print(f"✓ Analysis completed in {elapsed:.2f}s")
        print(f"  - Root Causes: {len(results.get('root_causes', []))}")
        print(f"  - Contributing Factors: {len(results.get('contributing_factors', []))}")
        print(f"  - Business Impact: {results.get('business_impact', {}).get('severity', 'Unknown')}")
        
        return True
    except Exception as e:
        print(f"✗ Root Cause analysis failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_strategic_sampling(content, filename, model=None):
    """Test Strategic Sampling analysis"""
    print("\n--- Testing Strategic Sampling Analysis ---")
    
    try:
        analyzer = StrategicSamplingAnalyzer()
        start_time = time.time()
        results = analyzer.analyze(content, filename, selected_model=model)
        elapsed = time.time() - start_time
        
        print(f"✓ Analysis completed in {elapsed:.2f}s")
        print(f"  - Coverage: {results.get('sampling_coverage', {}).get('coverage_percent', 0):.1f}%")
        print(f"  - Sections Analyzed: {results.get('sampling_coverage', {}).get('sections_analyzed', 0)}")
        print(f"  - Confidence: {results.get('confidence_metrics', {}).get('overall_confidence', 0):.1%}")
        
        return True
    except Exception as e:
        print(f"✗ Strategic Sampling failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_deep_forensic(content, filename, model=None):
    """Test Deep Forensic analysis"""
    print("\n--- Testing Deep Forensic Analysis ---")
    print("⚠ This may take several minutes for large files...")
    
    try:
        analyzer = DeepForensicAnalyzer()
        start_time = time.time()
        results = analyzer.analyze(content, filename, selected_model=model)
        elapsed = time.time() - start_time
        
        print(f"✓ Analysis completed in {elapsed:.2f}s")
        print(f"  - Chunks Analyzed: {results.get('overview', {}).get('total_chunks', 0)}")
        print(f"  - Security Score: {results.get('security_analysis', {}).get('security_score', 0)}/100")
        print(f"  - Confidence: {results.get('confidence', 0):.1%}")
        
        return True
    except Exception as e:
        print(f"✗ Deep Forensic analysis failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    """Main test function"""
    print("=== Crash Analyzer V2 Test Suite ===")
    
    # Test imports first
    if not test_analyzer_imports():
        print("\n✗ Import tests failed. Exiting.")
        return
    
    # Find a test file
    test_file = None
    crash_dir = Path("crash_examples")
    if crash_dir.exists():
        wcr_files = list(crash_dir.glob("WCR*.txt"))
        if wcr_files:
            # Pick a smaller file for testing
            test_file = min(wcr_files, key=lambda f: f.stat().st_size)
    
    if not test_file:
        print("\n✗ No test files found in crash_examples/")
        return
    
    print(f"\nUsing test file: {test_file}")
    print(f"File size: {test_file.stat().st_size:,} bytes")
    
    # Read test content
    try:
        with open(test_file, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        print(f"\n✗ Failed to read test file: {e}")
        return
    
    # Check for Ollama availability
    model = None
    try:
        from utils.ollama import get_available_models
        models = get_available_models()
        if models:
            model = models[0]
            print(f"\nUsing AI model: {model}")
        else:
            print("\n⚠ No AI models available, using algorithmic fallbacks")
    except:
        print("\n⚠ Ollama not available, using algorithmic fallbacks")
    
    # Run tests
    test_results = []
    
    # Test Quick Triage (no model needed)
    test_results.append(("Quick Triage", test_quick_triage(content, test_file.name)))
    
    # Test Root Cause
    test_results.append(("Root Cause", test_root_cause(content, test_file.name, model)))
    
    # Test Strategic Sampling (only for larger files)
    if len(content) > 100000:  # 100KB
        test_results.append(("Strategic Sampling", test_strategic_sampling(content, test_file.name, model)))
    else:
        print("\n⚠ Skipping Strategic Sampling (file too small)")
    
    # Test Deep Forensic (only for small files in test mode)
    if len(content) < 500000:  # 500KB
        test_results.append(("Deep Forensic", test_deep_forensic(content, test_file.name, model)))
    else:
        print("\n⚠ Skipping Deep Forensic (file too large for test)")
    
    # Summary
    print("\n=== Test Summary ===")
    passed = sum(1 for _, result in test_results if result)
    total = len(test_results)
    
    for name, result in test_results:
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"{name}: {status}")
    
    print(f"\nTotal: {passed}/{total} tests passed")
    
    if passed == total:
        print("\n✓ All tests passed!")
    else:
        print("\n✗ Some tests failed. Check the output above for details.")

if __name__ == "__main__":
    main()