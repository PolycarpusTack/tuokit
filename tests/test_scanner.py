"""
Quick test of the Code Health Scanner
Verifies basic functionality
"""
import sys
from pathlib import Path

# Add tools to path
sys.path.append(str(Path(__file__).parent))

from code_health_scanner import CodeHealthScanner

def test_scanner():
    """Run a quick scan test"""
    print("🏥 Testing TuoKit Code Health Scanner...\n")
    
    # Initialize scanner
    project_path = "C:/Projects/Tuokit"
    scanner = CodeHealthScanner(project_path)
    
    # Get files
    files = scanner.scan_directory()
    print(f"✅ Found {len(files)} Python files to scan")
    
    # Test on a few files
    test_files = files[:5]  # First 5 files
    
    for file_path in test_files:
        print(f"\n📄 Testing: {file_path.name}")
        
        # Check syntax
        syntax_ok, error = scanner.check_syntax(file_path)
        if syntax_ok:
            print("  ✅ Syntax OK")
            
            # Analyze structure
            scanner.analyze_code_structure(file_path)
            
            # Report findings
            if scanner.issues:
                for issue_type, issues in scanner.issues.items():
                    file_issues = [i for i in issues if i.get('file') == str(file_path)]
                    if file_issues:
                        print(f"  ⚠️  {issue_type}: {len(file_issues)} issues")
        else:
            print(f"  ❌ Syntax Error: {error}")
    
    # Summary
    print(f"\n📊 Summary:")
    print(f"  - Files scanned: {len(test_files)}")
    print(f"  - Total lines: {scanner.metrics['total_lines']}")
    print(f"  - Syntax errors: {scanner.metrics['syntax_errors']}")
    print(f"  - God objects: {scanner.metrics['god_objects']}")
    print(f"  - Long functions: {scanner.metrics['long_functions']}")
    print(f"  - Technical debt: {scanner.metrics['technical_debt']}")
    
    print("\n✅ Scanner test complete! Ready to use.")
    print("\n💡 To run full UI: streamlit run integrated_code_scanner.py")

if __name__ == "__main__":
    test_scanner()
