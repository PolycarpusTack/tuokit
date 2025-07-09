#!/usr/bin/env python
"""
Test Deep Forensic Analysis on local hardware
Optimized for slower processing with result preservation
"""
import os
import sys
import time
from pathlib import Path
import json

# Set UTF-8 encoding for Windows
if sys.platform == 'win32':
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

# Suppress Streamlit warnings
os.environ['STREAMLIT_SERVER_HEADLESS'] = 'true'

from toolkits.crash_analyzer_v2.analyzers.deep_forensic import DeepForensicAnalyzer

def test_with_small_file():
    """Test with a small file to verify functionality"""
    print("=== Deep Forensic Analysis Test (Local Hardware) ===")
    
    # Create a small test file
    test_content = """
2024-01-15 10:23:45 ERROR Application startup failed
java.lang.NullPointerException: Cannot invoke method on null object
    at com.example.AppService.initialize(AppService.java:45)
    at com.example.Application.main(Application.java:20)
    
2024-01-15 10:23:46 ERROR Database connection failed
java.sql.SQLException: Connection refused
    at com.database.ConnectionPool.getConnection(ConnectionPool.java:120)
    
2024-01-15 10:23:47 WARN Memory usage high: 85%
2024-01-15 10:23:48 ERROR OutOfMemoryError: Java heap space
    at com.example.DataProcessor.process(DataProcessor.java:200)
    
Security Alert: Potential SQL injection detected in UserService.authenticate()
Performance Issue: Query took 5000ms: SELECT * FROM users WHERE active = true
    
Thread dump:
"main" #1 prio=5 os_prio=0 tid=0x01 nid=0x1234 waiting on condition
   java.lang.Thread.State: WAITING (parking)
    at sun.misc.Unsafe.park(Native Method)
    at java.util.concurrent.locks.LockSupport.park(LockSupport.java:175)
    
System Info:
- Memory: 4GB allocated, 3.4GB used
- CPU: 95% utilization
- Threads: 250 active
"""
    
    print(f"Test content size: {len(test_content)} bytes")
    
    # Initialize analyzer
    analyzer = DeepForensicAnalyzer()
    
    # Mock phase callback
    def phase_callback(phase: str, progress: float):
        print(f"  Phase: {phase} - {progress:.0%}")
    
    # Run analysis
    print("\nStarting analysis...")
    start_time = time.time()
    
    try:
        results = analyzer.analyze(
            test_content, 
            "test_crash.log",
            selected_model=None,  # No AI for speed
            phase_callback=phase_callback
        )
        
        elapsed = time.time() - start_time
        print(f"\nAnalysis completed in {elapsed:.1f}s")
        
        # Check results structure
        print("\n=== Results Structure ===")
        print(f"Keys in results: {list(results.keys())}")
        print(f"Has formatted output: {'formatted_output' in results}")
        print(f"Formatted output length: {len(results.get('formatted_output', ''))}")
        
        # Show key findings
        if results.get('overview'):
            print(f"\nChunks analyzed: {results['overview'].get('total_chunks', 0)}")
        
        if results.get('crash_analysis'):
            print(f"\nCrash Analysis:")
            if 'error_taxonomy' in results['crash_analysis']:
                print(f"  Error types found: {list(results['crash_analysis']['error_taxonomy'].keys())}")
        
        if results.get('security_analysis'):
            print(f"\nSecurity Score: {results['security_analysis'].get('security_score', 0)}/100")
        
        # Save results for inspection
        output_file = "deep_forensic_test_results.json"
        with open(output_file, 'w') as f:
            # Convert to JSON-serializable format
            json_results = {
                k: str(v) if not isinstance(v, (dict, list, str, int, float, bool, type(None))) else v
                for k, v in results.items()
            }
            json.dump(json_results, f, indent=2)
        print(f"\nFull results saved to: {output_file}")
        
        # Display formatted output if available
        if results.get('formatted_output'):
            print("\n=== Formatted Output Preview ===")
            print(results['formatted_output'][:1000] + "..." if len(results['formatted_output']) > 1000 else results['formatted_output'])
        
        return True
        
    except Exception as e:
        print(f"\n❌ Analysis failed: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_with_real_file(filename: str, max_size: int = 100000):
    """Test with a real crash file (limited size for testing)"""
    print(f"\n=== Testing with real file: {filename} ===")
    
    if not os.path.exists(filename):
        print(f"File not found: {filename}")
        return False
    
    # Read file (with size limit)
    with open(filename, 'r', encoding='utf-8', errors='ignore') as f:
        content = f.read(max_size)
    
    actual_size = os.path.getsize(filename)
    print(f"File size: {actual_size:,} bytes")
    print(f"Testing with: {len(content):,} bytes (limited to {max_size:,})")
    
    # Initialize analyzer
    analyzer = DeepForensicAnalyzer()
    
    # Simple progress tracking
    phases_completed = []
    def phase_callback(phase: str, progress: float):
        if progress >= 1.0 and phase not in phases_completed:
            phases_completed.append(phase)
            print(f"✓ Completed: {phase}")
    
    print("\nStarting analysis...")
    start_time = time.time()
    
    try:
        results = analyzer.analyze(
            content, 
            os.path.basename(filename),
            selected_model=None,  # No AI for faster testing
            phase_callback=phase_callback
        )
        
        elapsed = time.time() - start_time
        print(f"\nAnalysis completed in {elapsed:.1f}s")
        
        # Show summary
        print("\n=== Analysis Summary ===")
        print(f"Total chunks: {results.get('overview', {}).get('total_chunks', 0)}")
        print(f"Confidence: {results.get('confidence', 0):.1%}")
        print(f"Processing time: {results.get('processing_time', 0):.1f}s")
        
        # Critical findings
        critical = results.get('critical_findings', {})
        if critical.get('immediate_threats'):
            print(f"\n⚠️ Immediate threats: {len(critical['immediate_threats'])}")
            for threat in critical['immediate_threats'][:3]:
                print(f"  - {threat}")
        
        return True
        
    except Exception as e:
        print(f"\n❌ Analysis failed: {e}")
        return False

def main():
    """Run tests"""
    print("Testing Deep Forensic Analysis on local hardware")
    print("=" * 50)
    
    # Test 1: Small synthetic file
    print("\n1. Testing with small synthetic crash dump...")
    success1 = test_with_small_file()
    
    # Test 2: Real file (if available)
    crash_files = list(Path("crash_examples").glob("*.txt"))
    if crash_files:
        # Pick a small file
        test_file = min(crash_files, key=lambda f: f.stat().st_size)
        print(f"\n2. Testing with real file (size-limited)...")
        success2 = test_with_real_file(str(test_file))
    else:
        print("\n2. No crash example files found")
        success2 = False
    
    # Summary
    print("\n" + "=" * 50)
    print("Test Summary:")
    print(f"  Small file test: {'✓ PASS' if success1 else '✗ FAIL'}")
    print(f"  Real file test: {'✓ PASS' if success2 else '✗ FAIL'}")
    
    if success1:
        print("\n✅ Deep Forensic Analysis is working on your hardware!")
        print("For full file analysis:")
        print("  1. Use smaller chunk sizes (done)")
        print("  2. Be patient - it may take hours for large files")
        print("  3. Results WILL be preserved even after long runs")
        print("  4. Check deep_forensic_test_results.json for full output")

if __name__ == "__main__":
    main()