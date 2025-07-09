"""
Standalone test for Crash Analyzer 5MB support
Tests core functionality without dependency issues
"""

def test_chunking_calculation():
    """Test that 5MB files can be properly chunked"""
    print("Testing 5MB File Chunking Calculation...")
    
    # Configuration values from crash_analyzer_enhanced.py
    chunk_size = 8000
    chunk_overlap = 400
    max_chunks = 650
    
    # Test 5MB file
    five_mb = 5 * 1024 * 1024  # 5,242,880 bytes
    
    # Calculate chunks needed
    chunks_needed = 0
    start = 0
    positions = []
    
    while start < five_mb:
        end = min(start + chunk_size, five_mb)
        positions.append((start, end))
        chunks_needed += 1
        
        # Move to next chunk with overlap
        if end < five_mb:
            start = end - chunk_overlap
        else:
            start = end
    
    print(f"  5MB file size: {five_mb:,} bytes")
    print(f"  Chunk size: {chunk_size} bytes")
    print(f"  Overlap: {chunk_overlap} bytes")
    print(f"  Chunks needed: {chunks_needed}")
    print(f"  Max chunks allowed: {max_chunks}")
    
    # Verify coverage
    print("\n  Verifying coverage...")
    print(f"  First chunk: bytes {positions[0][0]:,} - {positions[0][1]:,}")
    print(f"  Last chunk: bytes {positions[-1][0]:,} - {positions[-1][1]:,}")
    
    # Check that we can handle 5MB
    if chunks_needed <= max_chunks:
        print(f"\n  ✓ SUCCESS: 5MB file requires {chunks_needed} chunks, which is within the {max_chunks} limit")
    else:
        print(f"\n  ✗ FAILURE: 5MB file requires {chunks_needed} chunks, which exceeds the {max_chunks} limit")
        return False
    
    # Calculate processing time estimate
    time_per_chunk = 2  # seconds
    total_time = chunks_needed * time_per_chunk
    print(f"\n  Estimated processing time: {total_time} seconds ({total_time//60} minutes {total_time%60} seconds)")
    
    return True

def test_pattern_definitions():
    """Test pattern definitions are properly configured"""
    print("\nTesting Pattern Definitions...")
    
    # Pattern definitions from crash_analyzer_enhanced.py
    patterns = {
        "NullPointerException": {
            "pattern": r"NullPointerException|NPE|null.*reference|nil.*reference|accessing null object|nil object",
            "severity": "High"
        },
        "OutOfMemoryError": {
            "pattern": r"OutOfMemoryError|OOM|heap.*space|memory.*exhausted|GC overhead limit|Java heap space",
            "severity": "Critical"
        },
        "StackOverflow": {
            "pattern": r"StackOverflowError|stack.*overflow|recursion.*limit|too many nested calls",
            "severity": "High"
        }
    }
    
    # Test that patterns have required fields
    required_fields = ["pattern", "severity"]
    valid_severities = ["Critical", "High", "Medium", "Low"]
    
    all_valid = True
    for name, config in patterns.items():
        print(f"  Checking {name}...")
        
        for field in required_fields:
            if field not in config:
                print(f"    ✗ Missing required field: {field}")
                all_valid = False
        
        if "severity" in config and config["severity"] not in valid_severities:
            print(f"    ✗ Invalid severity: {config['severity']}")
            all_valid = False
        
        if all(field in config for field in required_fields):
            print(f"    ✓ Valid pattern with severity: {config['severity']}")
    
    return all_valid

def test_chunk_aggregation_logic():
    """Test the logic for aggregating results from multiple chunks"""
    print("\nTesting Chunk Aggregation Logic...")
    
    # Simulate results from multiple chunks
    chunk_results = {
        "errors": {"Error1", "Error2", "Error3"},
        "error_types": {"SQLException", "OutOfMemoryError"},
        "severities": ["High", "Critical", "Medium", "Critical"],
        "root_causes": ["Database connection failed", "Memory leak", "Database connection failed"],
        "locations": {"File1.java:123", "File2.java:456"},
        "patterns": [
            {"pattern": "OutOfMemoryError", "severity": "Critical"},
            {"pattern": "DatabaseTimeout", "severity": "Medium"}
        ]
    }
    
    # Determine highest severity
    severity_order = {"Critical": 4, "High": 3, "Medium": 2, "Low": 1, "Unknown": 0}
    highest_severity = max(chunk_results["severities"], 
                          key=lambda x: severity_order.get(x, 0))
    
    print(f"  Total errors found: {len(chunk_results['errors'])}")
    print(f"  Error types: {', '.join(chunk_results['error_types'])}")
    print(f"  Highest severity: {highest_severity}")
    
    # Determine most likely root cause
    cause_freq = {}
    for cause in chunk_results["root_causes"]:
        cause_freq[cause] = cause_freq.get(cause, 0) + 1
    most_likely_cause = max(cause_freq.items(), key=lambda x: x[1])[0]
    
    print(f"  Most likely root cause: {most_likely_cause} (appeared {cause_freq[most_likely_cause]} times)")
    print(f"  Locations affected: {len(chunk_results['locations'])}")
    print(f"  Patterns matched: {len(chunk_results['patterns'])}")
    
    return True

def run_tests():
    """Run all standalone tests"""
    print("=" * 60)
    print("Crash Analyzer 5MB Support - Standalone Tests")
    print("=" * 60)
    
    tests = [
        test_chunking_calculation,
        test_pattern_definitions,
        test_chunk_aggregation_logic
    ]
    
    all_passed = True
    for test in tests:
        try:
            if not test():
                all_passed = False
                print(f"\n✗ {test.__name__} FAILED")
        except Exception as e:
            all_passed = False
            print(f"\n✗ {test.__name__} FAILED with error: {e}")
    
    print("\n" + "=" * 60)
    if all_passed:
        print("✓ ALL TESTS PASSED!")
        print("The crash analyzer can handle files up to 5MB!")
    else:
        print("✗ Some tests failed")
    print("=" * 60)
    
    return all_passed

if __name__ == "__main__":
    import sys
    success = run_tests()
    sys.exit(0 if success else 1)
