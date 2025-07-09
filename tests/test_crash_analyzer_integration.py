"""
Integration test for crash analyzer with enhanced JSON handling
Simulates the actual workflow that was failing
"""
import sys
import os
import json

# Add parent directory to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def simulate_crash_analyzer_workflow():
    """Simulate the crash analyzer workflow with problem chunks"""
    print("=" * 60)
    print("Crash Analyzer Integration Test")
    print("Simulating chunks 89-102 processing")
    print("=" * 60)
    
    # Create a mock crash dump content
    crash_content = """
    2024-01-03 10:00:00 INFO: Application starting...
    2024-01-03 10:00:05 INFO: Configuration loaded
    2024-01-03 10:00:10 ERROR: Database connection failed
    java.sql.SQLException: Connection timeout
        at com.mysql.jdbc.Connection.connect(Connection.java:234)
        at com.example.DatabaseManager.init(DatabaseManager.java:56)
    """ * 100  # Repeat to create multiple chunks
    
    # Add some variety
    crash_content += """
    2024-01-03 10:00:15 FATAL: Application cannot start
    OutOfMemoryError: Java heap space
        at java.util.ArrayList.grow(ArrayList.java:123)
    """ * 50
    
    print(f"Created test content: {len(crash_content):,} characters")
    
    # Import the actual functions
    from pages.crash_analyzer import CRASH_ANALYZER_CONFIG, match_known_patterns
    from utils.json_helper_enhanced import get_chunk_analysis_safe
    
    # Calculate chunks like the real analyzer
    chunk_size = CRASH_ANALYZER_CONFIG["chunk_size"]
    overlap = CRASH_ANALYZER_CONFIG["chunk_overlap"]
    
    chunks = []
    start = 0
    while start < len(crash_content):
        end = min(start + chunk_size, len(crash_content))
        chunks.append((start, end, crash_content[start:end]))
        start = end - overlap if end < len(crash_content) else end
    
    print(f"Total chunks to process: {len(chunks)}")
    
    # Process chunks, focusing on the problematic range (89-102)
    problem_range_start = min(88, len(chunks) - 15)
    problem_range_end = min(problem_range_start + 14, len(chunks))
    
    print(f"\nProcessing chunks {problem_range_start+1} to {problem_range_end}...")
    print("-" * 40)
    
    consecutive_failures = 0
    successful_chunks = 0
    chunks_with_errors = 0
    
    # Mock the LLM response to simulate various scenarios
    def mock_safe_ollama_generate(model, prompt, temperature=0.3, format=None):
        import random
        
        # Simulate different response scenarios
        scenario = random.choice([
            "empty",
            "valid_json",
            "malformed_json",
            "text_only",
            "partial_json"
        ])
        
        if scenario == "empty":
            return {'response': ''}
        elif scenario == "valid_json":
            return {'response': json.dumps({
                "errors_found": ["SQLException", "OutOfMemoryError"],
                "error_types": ["Database", "Memory"],
                "severity": "Critical",
                "root_cause_hints": "Database timeout and memory exhaustion",
                "error_locations": ["DatabaseManager.java:56"],
                "summary": "Critical errors detected"
            })}
        elif scenario == "malformed_json":
            return {'response': '{"errors_found": ["Error", "severity": "High"'}
        elif scenario == "text_only":
            return {'response': 'The analysis shows database connection issues and memory problems.'}
        else:  # partial_json
            return {'response': 'Analysis: {"errors_found": ["PartialError"], incomplete'}
    
    # Patch the function
    import utils
    utils.safe_ollama_generate = mock_safe_ollama_generate
    
    for i in range(problem_range_start, problem_range_end):
        if i >= len(chunks):
            break
            
        start_pos, end_pos, chunk = chunks[i]
        chunk_num = i + 1
        
        print(f"\nChunk {chunk_num}:")
        
        try:
            # Use the enhanced analysis function
            chunk_data = get_chunk_analysis_safe(
                chunk=chunk,
                chunk_info=f" (position {start_pos}-{end_pos})",
                model="test-model"
            )
            
            # Verify we got valid data
            if isinstance(chunk_data, dict) and 'errors_found' in chunk_data:
                print(f"✅ Successfully processed")
                print(f"   Errors found: {len(chunk_data.get('errors_found', []))}")
                print(f"   Severity: {chunk_data.get('severity', 'Unknown')}")
                successful_chunks += 1
                consecutive_failures = 0
                
                if chunk_data.get('errors_found'):
                    chunks_with_errors += 1
            else:
                print(f"❌ Invalid response structure")
                consecutive_failures += 1
                
        except Exception as e:
            print(f"❌ Exception: {e}")
            consecutive_failures += 1
        
        # Check pattern matching as backup
        patterns = match_known_patterns(chunk[:1000])
        if patterns:
            print(f"   Pattern matches: {[p['pattern'] for p in patterns[:3]]}")
    
    print("\n" + "=" * 60)
    print("Integration Test Results:")
    print(f"✅ Successful chunks: {successful_chunks}/{problem_range_end - problem_range_start}")
    print(f"📊 Chunks with errors: {chunks_with_errors}")
    print(f"⚠️ Final consecutive failures: {consecutive_failures}")
    print(f"{'✅ PASS' if consecutive_failures < 10 else '❌ FAIL'}: Consecutive failure limit not exceeded")
    print("=" * 60)
    
    return consecutive_failures < 10

def main():
    """Run integration test"""
    try:
        success = simulate_crash_analyzer_workflow()
        
        if success:
            print("\n✅ Integration test passed!")
            print("The crash analyzer can now handle problematic chunks.")
        else:
            print("\n❌ Integration test failed!")
            print("Check the implementation.")
        
        return success
    except Exception as e:
        print(f"\n❌ Integration test error: {e}")
        import traceback
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
