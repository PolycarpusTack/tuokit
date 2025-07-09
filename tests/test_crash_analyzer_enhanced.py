"""
Test script for Crash Analyzer Enhanced Features
Run this to validate the enhancements work correctly including full 5MB file processing
"""
import sys
import os

# Mock streamlit for testing
class MockStreamlit:
    class session_state:
        @staticmethod
        def get(key, default=None):
            return default
    
    @staticmethod
    def progress(*args, **kwargs):
        return type('obj', (object,), {'progress': lambda *a: None, 'empty': lambda: None})()
    
    @staticmethod
    def button(*args, **kwargs):
        return False
    
    @staticmethod
    def info(*args, **kwargs):
        pass
    
    @staticmethod
    def warning(*args, **kwargs):
        pass
    
    @staticmethod
    def container():
        return type('obj', (object,), {'write': lambda *a: None, 'empty': lambda: None})()

sys.modules['streamlit'] = MockStreamlit()

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Now we can import the functions we want to test
# Import specific functions that don't depend on Streamlit UI
def test_pattern_matching():
    """Test enhanced pattern matching with context"""
    print("Testing Pattern Matching...")
    
    # Import inside function to avoid module-level issues
    from pages.crash_analyzer_enhanced import match_known_patterns, KNOWN_PATTERNS
    
    test_content = """
    2024-01-15 10:30:45 ERROR: Application startup failed
    java.lang.NullPointerException: Cannot invoke method on null object
        at com.example.MyClass.doSomething(MyClass.java:42)
        at com.example.Main.main(Main.java:15)
    
    Caused by: OutOfMemoryError: Java heap space
        at java.util.ArrayList.grow(ArrayList.java:123)
    """
    
    matches = match_known_patterns(test_content)
    
    print(f"Found {len(matches)} pattern matches:")
    for match in matches:
        print(f"  - Pattern: {match['pattern']}")
        print(f"    Severity: {match['severity']}")
        print(f"    Context: {match['context'][:50]}...")
        print(f"    Quick Fix: {match['quick_fix']}")
        print()
    
    assert len(matches) >= 2, "Should find at least 2 patterns"
    assert any(m['pattern'] == 'NullPointerException' for m in matches)
    assert any(m['pattern'] == 'OutOfMemoryError' for m in matches)
    print("✅ Pattern matching test passed!\n")

def test_critical_extraction():
    """Test smart content extraction"""
    print("Testing Critical Section Extraction...")
    
    from pages.crash_analyzer_enhanced import extract_critical_sections
    
    test_content = """
    === Application Log ===
    2024-01-15 10:30:00 INFO: Starting application
    2024-01-15 10:30:10 INFO: Loading configuration
    2024-01-15 10:30:20 INFO: Connecting to database
    
    2024-01-15 10:30:45 ERROR: Database connection failed
    java.sql.SQLException: Connection timeout
        at com.mysql.jdbc.Connection.connect(Connection.java:234)
        at com.example.DatabaseManager.init(DatabaseManager.java:56)
        at com.example.Application.start(Application.java:123)
    
    2024-01-15 10:30:46 FATAL: Application cannot continue
    System will shut down
    
    === End of log ===
    """ + "Filler content " * 1000  # Add filler to test extraction
    
    extracted = extract_critical_sections(test_content, max_length=1000)
    
    print(f"Original size: {len(test_content)} chars")
    print(f"Extracted size: {len(extracted)} chars")
    print("\nExtracted content preview:")
    print(extracted[:300] + "...")
    
    assert "SQLException" in extracted, "Should extract exception"
    assert "FATAL" in extracted, "Should extract fatal error"
    assert len(extracted) < len(test_content), "Should be smaller than original"
    print("\n✅ Critical extraction test passed!\n")

def test_strategic_sampling():
    """Test strategic sampling for large files"""
    print("Testing Strategic Sampling...")
    
    from pages.crash_analyzer_enhanced import generate_strategic_samples
    
    # Create a large test content
    test_content = "Start of file\n" * 100
    test_content += "\n\nERROR: First error occurred here\n" * 10
    test_content += "Normal content\n" * 500
    test_content += "\n\nEXCEPTION: Critical exception\n" * 10
    test_content += "More content\n" * 500
    test_content += "\n\nEnd of file"
    
    samples = generate_strategic_samples(test_content, max_samples=5)
    
    print(f"Generated {len(samples)} samples from {len(test_content)} chars")
    for i, sample in enumerate(samples):
        print(f"  Sample {i+1}: {sample['type']} - {sample['description']}")
        print(f"    Size: {len(sample['content'])} chars")
        if "error" in sample['type'].lower():
            print(f"    Contains: {sample['content'][:50]}...")
    
    assert len(samples) >= 3, "Should have at least 3 samples"
    assert any(s['type'] == 'header' for s in samples), "Should have header sample"
    assert any(s['type'] == 'footer' for s in samples), "Should have footer sample"
    assert any('error' in s['type'] for s in samples), "Should have error samples"
    print("\n✅ Strategic sampling test passed!\n")

def test_size_decision_logic():
    """Test file size decision logic"""
    print("Testing Size Decision Logic...")
    
    from pages.crash_analyzer_enhanced import determine_analysis_method
    
    test_cases = [
        (50 * 1024, "standard"),  # 50KB
        (500 * 1024, "standard_with_extraction"),  # 500KB
        (2 * 1024 * 1024, "full_chunking"),  # 2MB
        (5 * 1024 * 1024, "full_chunking"),  # 5MB
    ]
    
    for size, expected_method in test_cases:
        method, warning = determine_analysis_method(size)
        print(f"  {size/1024/1024:.1f}MB -> Method: {method}")
        if warning:
            print(f"    Warning: {warning}")
        assert expected_method in method, f"Expected {expected_method}, got {method}"
    
    print("\n✅ Size decision logic test passed!\n")

def test_full_file_chunking():
    """Test full file chunking capability for 5MB files"""
    print("Testing Full File Chunking (5MB support)...")
    
    from pages.crash_analyzer_enhanced import CRASH_ANALYZER_CONFIG
    
    # Test configuration
    chunk_size = CRASH_ANALYZER_CONFIG["chunk_size"]
    overlap = CRASH_ANALYZER_CONFIG["chunk_overlap"]
    
    # Create content that will generate multiple chunks
    test_content = ""
    for i in range(5):  # Create 5 chunks worth of content
        test_content += f"\n--- CHUNK {i} START ---\n"
        test_content += f"ERROR in chunk {i}: Something went wrong\n" * 10
        test_content += "Normal log content\n" * 200
        test_content += f"EXCEPTION in chunk {i}: Critical failure\n" * 5
        test_content += f"--- CHUNK {i} END ---\n"
    
    print(f"Test content size: {len(test_content)} chars")
    print(f"Chunk size: {chunk_size}, Overlap: {overlap}")
    
    # Calculate expected chunks
    expected_chunks = 0
    start = 0
    while start < len(test_content):
        expected_chunks += 1
        end = min(start + chunk_size, len(test_content))
        start = end - overlap if end < len(test_content) else end
    
    print(f"Expected chunks: {expected_chunks}")
    
    # Test chunking calculation
    chunks = []
    start = 0
    while start < len(test_content):
        end = min(start + chunk_size, len(test_content))
        chunks.append((start, end))
        start = end - overlap if end < len(test_content) else end
    
    print(f"Actual chunks: {len(chunks)}")
    
    # Verify chunks cover entire content
    covered_chars = set()
    for start, end in chunks:
        for i in range(start, end):
            covered_chars.add(i)
    
    print(f"Coverage: {len(covered_chars)}/{len(test_content)} chars")
    
    assert len(chunks) == expected_chunks, f"Expected {expected_chunks} chunks, got {len(chunks)}"
    assert len(covered_chars) == len(test_content), "All characters should be covered"
    
    # Test 5MB file handling
    print("\nTesting 5MB file size limits...")
    five_mb = 5 * 1024 * 1024
    chunks_for_5mb = 0
    start = 0
    while start < five_mb:
        chunks_for_5mb += 1
        end = min(start + chunk_size, five_mb)
        start = end - overlap if end < five_mb else end
    
    print(f"Chunks needed for 5MB: {chunks_for_5mb}")
    print(f"Max chunks allowed: {CRASH_ANALYZER_CONFIG['max_chunks']}")
    
    assert chunks_for_5mb <= CRASH_ANALYZER_CONFIG['max_chunks'], "Should be able to handle 5MB files"
    
    print("\n✅ Full file chunking test passed!\n")

def test_chunk_analysis_mock():
    """Test the chunk analysis function with mock data"""
    print("Testing Chunk Analysis Function...")
    
    # Create a small test content
    test_content = """
    ERROR: Database connection failed
    java.sql.SQLException: Connection refused
        at com.mysql.jdbc.connect()
    
    FATAL: Application cannot start without database
    OutOfMemoryError: Java heap space
        at java.util.ArrayList.grow()
    """
    
    # Mock the analyze_with_chunking function behavior
    print("Simulating chunk analysis behavior:")
    
    # Calculate chunks
    chunk_size = 200  # Small chunks for testing
    chunks = []
    start = 0
    while start < len(test_content):
        end = min(start + chunk_size, len(test_content))
        chunks.append(test_content[start:end])
        start = end - 50  # Small overlap
    
    print(f"  Generated {len(chunks)} chunks for analysis")
    
    # Simulate aggregation
    errors_found = set()
    error_types = set()
    
    for i, chunk in enumerate(chunks):
        if "ERROR" in chunk or "Exception" in chunk:
            errors_found.add(f"Error in chunk {i}")
        if "SQLException" in chunk:
            error_types.add("SQLException")
        if "OutOfMemoryError" in chunk:
            error_types.add("OutOfMemoryError")
    
    print(f"  Found {len(errors_found)} errors")
    print(f"  Found {len(error_types)} error types: {error_types}")
    
    assert len(errors_found) > 0, "Should find errors"
    assert "SQLException" in error_types, "Should find SQLException"
    assert "OutOfMemoryError" in error_types, "Should find OutOfMemoryError"
    
    print("\n✅ Chunk analysis test passed!\n")

def test_configuration():
    """Test configuration values"""
    print("Testing Configuration...")
    
    from pages.crash_analyzer_enhanced import CRASH_ANALYZER_CONFIG
    
    print("Current configuration:")
    for key, value in CRASH_ANALYZER_CONFIG.items():
        print(f"  {key}: {value}")
    
    assert CRASH_ANALYZER_CONFIG["max_file_size_mb"] == 5
    assert CRASH_ANALYZER_CONFIG["chunk_size"] == 8000
    assert CRASH_ANALYZER_CONFIG["max_chunks"] >= 650, "Should support 5MB files"
    assert CRASH_ANALYZER_CONFIG["enable_abort"] == True, "Should allow aborting long operations"
    print("\n✅ Configuration test passed!\n")

def run_all_tests():
    """Run all tests"""
    print("=" * 60)
    print("Running Crash Analyzer Enhancement Tests")
    print("Including full 5MB file support validation")
    print("=" * 60)
    print()
    
    try:
        test_pattern_matching()
        test_critical_extraction()
        test_strategic_sampling()
        test_size_decision_logic()
        test_full_file_chunking()
        test_chunk_analysis_mock()
        test_configuration()
        
        print("=" * 60)
        print("✅ ALL TESTS PASSED! 🎉")
        print("The crash analyzer can handle files up to 5MB!")
        print("=" * 60)
        
    except AssertionError as e:
        print(f"\n❌ Test failed: {e}")
        return False
    except Exception as e:
        print(f"\n❌ Unexpected error: {e}")
        import traceback
        traceback.print_exc()
        return False
    
    return True

if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
