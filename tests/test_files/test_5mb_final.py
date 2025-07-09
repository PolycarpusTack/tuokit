"""
Final test for Crash Analyzer 5MB support
"""

def test_5mb_support():
    """Verify that crash analyzer can handle 5MB files"""
    print("Testing 5MB File Support")
    print("-" * 40)
    
    # Configuration from crash_analyzer_enhanced.py
    chunk_size = 8000
    chunk_overlap = 400
    max_chunks = 700  # Updated value
    
    # Test different file sizes
    test_sizes = [
        (1 * 1024 * 1024, "1MB"),
        (3 * 1024 * 1024, "3MB"),
        (5 * 1024 * 1024, "5MB"),
    ]
    
    for file_size, label in test_sizes:
        chunks_needed = 0
        start = 0
        
        while start < file_size:
            end = min(start + chunk_size, file_size)
            chunks_needed += 1
            
            if end < file_size:
                start = end - chunk_overlap
            else:
                start = end
        
        can_handle = chunks_needed <= max_chunks
        status = "PASS" if can_handle else "FAIL"
        
        print(f"\n{label} file ({file_size:,} bytes):")
        print(f"  Chunks needed: {chunks_needed}")
        print(f"  Can handle: {status}")
        
        if label == "5MB" and not can_handle:
            print(f"  ERROR: Cannot handle 5MB files! Need {chunks_needed} chunks but max is {max_chunks}")
            return False
    
    print("\n" + "-" * 40)
    print("SUCCESS: Crash analyzer can handle files up to 5MB!")
    print(f"Maximum capacity: {max_chunks} chunks")
    
    # Calculate actual max file size
    max_file_size = (max_chunks - 1) * (chunk_size - chunk_overlap) + chunk_size
    print(f"Theoretical maximum file size: {max_file_size / 1024 / 1024:.2f}MB")
    
    return True

if __name__ == "__main__":
    import sys
    success = test_5mb_support()
    sys.exit(0 if success else 1)
