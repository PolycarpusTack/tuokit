#!/usr/bin/env python3
"""
Test Enterprise Report Analyzer
"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))

from toolkits.crash_analyzer_v2.analyzers.enterprise_report import EnterpriseReportAnalyzer

def test_enterprise_report():
    """Test the Enterprise Report analyzer with WCR file"""
    print("Testing Enterprise Report Analyzer...")
    
    # Read test file
    test_file = Path("crash_examples/WCR_2-4_16-3-4.txt")
    if not test_file.exists():
        print(f"Error: Test file not found: {test_file}")
        return
    
    content = test_file.read_text(encoding='utf-8', errors='ignore')
    print(f"Loaded test file: {test_file.name} ({len(content):,} bytes)")
    
    # Create analyzer
    analyzer = EnterpriseReportAnalyzer()
    
    # Test without model (should show error)
    print("\n1. Testing without AI model...")
    results = analyzer.analyze(content, test_file.name)
    if results.get("error"):
        print(f"✓ Expected error: {results['error']}")
    
    # Test with mock model (for testing structure)
    print("\n2. Testing with mock AI responses...")
    
    # Mock the generate_with_model method for testing
    def mock_generate(prompt, model, temperature=0.3):
        # Return mock JSON responses based on prompt content
        if "executive summary" in prompt.lower():
            return '''{"crash": "Application crashed due to missing method #isRelativeToEachAiring", 
                      "root_cause": {"technical": "Method not implemented in license window class", 
                                     "data": "No data corruption detected"},
                      "actionable_items": [{"type": "method", "description": "Implement #isRelativeToEachAiring in CM2LicenseWindow", 
                                           "priority": "immediate"}]}'''
        elif "object state" in prompt.lower():
            return '''{"object_states": [{"frame": 1, "object": "CM2LicenseWindow", 
                                         "issue": "Missing method implementation", 
                                         "likely_cause": "Incomplete deployment"}],
                      "nil_checks_needed": ["airing", "license"],
                      "data_validation_needed": ["license_type"]}'''
        elif "reconstruct the user actions" in prompt.lower():
            return '''{"user_actions": [{"step": 1, "action": "User opened license window", 
                                        "ui_element": "License Manager"}],
                      "key_records": [{"type": "License", "id": "12345", "state": "being validated"}],
                      "reproduction_steps": ["Open License Manager", "Select license", "Click validate"]}'''
        elif "provide solutions" in prompt.lower():
            return '''{"root_causes": {"technical": "Missing method in deployment", 
                                      "data": "None"},
                      "code_fixes": [{"location": "CM2LicenseWindow>>isRelativeToEachAiring", 
                                     "fix": "Add method to return boolean", 
                                     "code_sample": "isRelativeToEachAiring\\n\\t^false"}],
                      "workarounds": [{"type": "immediate", 
                                      "action": "Disable license validation temporarily", 
                                      "impact": "minimal"}]}'''
        elif "sql queries" in prompt.lower():
            return '''{"index_recommendations": [{"table": "licenses", "columns": ["user_id", "status"], 
                                                 "reason": "Speed up license lookups"}],
                      "query_optimizations": ["Add LIMIT clause to queries"],
                      "performance_risks": ["Full table scan on licenses"]}'''
        elif "code efficiency" in prompt.lower():
            return '''{"anti_patterns": ["Missing error handling"],
                      "deprecated_usage": [],
                      "future_risks": ["Method missing errors will recur"],
                      "refactoring_suggestions": ["Add defensive programming checks"]}'''
        return "{}"
    
    # Patch the method
    analyzer.generate_with_model = mock_generate
    
    # Run analysis
    def progress_callback(progress, message):
        print(f"  Progress: {progress:.0%} - {message}")
    
    results = analyzer.analyze(
        content, 
        test_file.name, 
        selected_model="mock-model",
        progress_callback=progress_callback
    )
    
    # Check results
    print("\n3. Checking results structure...")
    expected_sections = [
        "executive_summary",
        "context_environment", 
        "crash_stack_analysis",
        "pre_crash_sequence",
        "solutions_workarounds",
        "suggested_improvements",
        "additional_observations",
        "formatted_output"
    ]
    
    for section in expected_sections:
        if section in results:
            print(f"✓ {section}: Found")
            if isinstance(results[section], dict):
                print(f"  Keys: {list(results[section].keys())[:3]}...")
        else:
            print(f"✗ {section}: Missing")
    
    # Check formatted output
    if results.get("formatted_output"):
        print(f"\n4. Formatted output generated ({len(results['formatted_output'])} chars)")
        print("First 500 chars:")
        print("-" * 50)
        print(results["formatted_output"][:500])
        print("-" * 50)
        
        # Save to file for review
        output_file = Path("test_enterprise_report_output.md")
        output_file.write_text(results["formatted_output"])
        print(f"\nFull report saved to: {output_file}")
    
    print("\n✅ Enterprise Report test completed!")

if __name__ == "__main__":
    test_enterprise_report()