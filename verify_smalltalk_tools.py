#!/usr/bin/env python3
"""
TuoKit SmallTalk Tools - Complete Verification Script
Run this to ensure all SmallTalk tools are properly installed and configured
"""

import os
import sys
import subprocess
from datetime import datetime

# Add project root to path
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def print_header(text):
    """Print formatted header"""
    print("\n" + "="*60)
    print(f"  {text}")
    print("="*60 + "\n")

def check_ollama():
    """Check if Ollama is running"""
    print_header("Checking Ollama Status")
    
    try:
        result = subprocess.run(["ollama", "list"], capture_output=True, text=True)
        if result.returncode == 0:
            print("✅ Ollama is installed and running")
            
            # Check for required models
            output = result.stdout
            required_models = ["deepseek-coder:6.7b", "deepseek-r1:6.7b"]
            
            for model in required_models:
                if model in output:
                    print(f"✅ Model {model} is available")
                else:
                    print(f"❌ Model {model} is missing")
                    print(f"   Run: ollama pull {model}")
            
            return True
        else:
            print("❌ Ollama command failed")
            return False
    except FileNotFoundError:
        print("❌ Ollama not found. Please install Ollama first.")
        return False

def check_imports():
    """Check if all tools can be imported"""
    print_header("Checking Tool Imports")
    
    tools = [
        # Original SmallTalk tools
        ("SmallTalk Explainer", "smalltalk_explainer", "SmallTalkExplainer"),
        ("SmallTalk Snippets", "smalltalk_snippets", "SmallTalkSnippetFinder"),
        ("SmallTalk ↔ Ruby Converter", "smalltalk_ruby_converter", "CodeConverter"),
        # New SmallTalk tools
        ("SmallTalk Class Generator", "smalltalk_class_gen", "SmallTalkClassGenerator"),
        ("Morphic UI Builder", "morphic_builder", "MorphicUIBuilder"),
        ("Seaside Component Generator", "seaside_generator", "SeasideComponentGenerator"),
        ("SmallTalk Refactoring Assistant", "smalltalk_refactorer", "SmallTalkRefactorer"),
        ("SmallTalk Metaprogramming Helper", "smalltalk_meta", "SmallTalkMetaprogrammingHelper"),
        ("SmallTalk Image Browser", "image_browser", "SmallTalkImageBrowser"),
        # Rails tools
        ("Rails Scaffold Generator", "rails_scaffold", "RailsScaffoldGenerator"),
        ("Rails Debugger", "rails_debugger", "RailsDebugger")
    ]
    
    success_count = 0
    total_count = len(tools)
    
    for tool_name, module_name, class_name in tools:
        try:
            module = __import__(f"pages.{module_name}", fromlist=[class_name])
            tool_class = getattr(module, class_name)
            print(f"✅ {tool_name}")
            success_count += 1
        except Exception as e:
            print(f"❌ {tool_name}: {str(e)[:50]}...")
    
    print(f"\nSummary: {success_count}/{total_count} tools imported successfully")
    return success_count == total_count

def check_files():
    """Check if all required files exist"""
    print_header("Checking File Structure")
    
    required_files = [
        # Tool files
        "pages/smalltalk_explainer.py",
        "pages/smalltalk_class_gen.py",
        "pages/morphic_builder.py",
        "pages/seaside_generator.py",
        "pages/smalltalk_refactorer.py",
        "pages/smalltalk_meta.py",
        "pages/image_browser.py",
        "pages/smalltalk_snippets.py",
        "pages/smalltalk_ruby_converter.py",
        "pages/rails_scaffold.py",
        "pages/rails_debugger.py",
        # Documentation
        "docs/SMALLTALK_TOOLS_COMPLETE.md",
        "docs/SMALLTALK_QUICK_START.md",
        "docs/SMALLTALK_INTEGRATION_SUMMARY.md",
        # Tests
        "tests/test_new_smalltalk_tools.py"
    ]
    
    missing_files = []
    for file_path in required_files:
        full_path = os.path.join(os.path.dirname(os.path.dirname(__file__)), file_path)
        if os.path.exists(full_path):
            print(f"✅ {file_path}")
        else:
            print(f"❌ {file_path} - MISSING")
            missing_files.append(file_path)
    
    if missing_files:
        print(f"\n⚠️  {len(missing_files)} files are missing")
        return False
    else:
        print("\n✅ All required files present")
        return True

def check_database():
    """Check database connection"""
    print_header("Checking Database Connection")
    
    try:
        from utils.database import DatabaseManager
        db = DatabaseManager()
        
        if db.connected:
            print("✅ Database connected successfully")
            
            # Check knowledge count
            count = db.get_knowledge_count()
            print(f"   Knowledge units: {count}")
            
            # Check recent queries
            recent = db.get_recent_queries(limit=1)
            if recent:
                print(f"   Most recent query: {recent[0][3].strftime('%Y-%m-%d %H:%M')}")
            
            return True
        else:
            print("❌ Database connection failed")
            print("   Check your .env configuration")
            return False
    except Exception as e:
        print(f"❌ Database check failed: {e}")
        return False

def generate_report():
    """Generate verification report"""
    print_header("SmallTalk Tools Verification Report")
    
    print(f"Report generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"TuoKit location: {os.path.dirname(os.path.dirname(__file__))}")
    
    # Run all checks
    checks = {
        "Ollama Status": check_ollama(),
        "Tool Imports": check_imports(),
        "File Structure": check_files(),
        "Database Connection": check_database()
    }
    
    # Summary
    print_header("Verification Summary")
    
    all_passed = all(checks.values())
    
    for check_name, passed in checks.items():
        status = "✅ PASS" if passed else "❌ FAIL"
        print(f"{status} - {check_name}")
    
    if all_passed:
        print("\n🎉 All checks passed! Your SmallTalk tools are ready to use.")
        print("\nNext steps:")
        print("1. Start TuoKit: streamlit run app.py")
        print("2. Navigate to SmallTalk tools in the sidebar")
        print("3. Try the Quick Start examples")
    else:
        print("\n⚠️  Some checks failed. Please address the issues above.")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is running: ollama serve")
        print("2. Pull required models: ollama pull deepseek-coder:6.7b")
        print("3. Check database configuration in .env")
        print("4. Verify all files were created properly")

def main():
    """Main verification process"""
    print("""
╔══════════════════════════════════════════════════════════╗
║          TuoKit SmallTalk Tools Verification             ║
║                                                          ║
║  This script verifies that all SmallTalk development     ║
║  tools are properly installed and configured.            ║
╚══════════════════════════════════════════════════════════╝
    """)
    
    generate_report()
    
    print("\n" + "="*60)
    print("Verification complete!")

if __name__ == "__main__":
    main()
