"""
TuoKit RAG Quick Setup Script
Run this to quickly set up and test the RAG system
"""
import subprocess
import sys
import os
from pathlib import Path

def run_command(cmd, description):
    """Run a command and show progress"""
    print(f"\n🔧 {description}...")
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
        if result.returncode == 0:
            print(f"✅ {description} - Success!")
            if result.stdout:
                print(result.stdout)
        else:
            print(f"❌ {description} - Failed!")
            print(result.stderr)
            return False
    except Exception as e:
        print(f"❌ Error: {e}")
        return False
    return True

def main():
    print("🚀 TuoKit RAG Quick Setup")
    print("=" * 50)
    
    # Check Python version
    if sys.version_info < (3, 8):
        print("❌ Python 3.8+ required")
        sys.exit(1)
    
    # Set up paths
    project_root = Path(__file__).parent.parent.parent
    os.chdir(project_root)
    
    # Step 1: Install dependencies
    if input("\n1. Install dependencies? (y/n): ").lower() == 'y':
        run_command(
            "pip install sentence-transformers pgvector pyyaml beautifulsoup4 lxml pypdf python-docx",
            "Installing core RAG dependencies"
        )
    
    # Step 2: Initialize the system
    if input("\n2. Initialize RAG system? (y/n): ").lower() == 'y':
        run_command(
            "python -m toolkits.llm_rag.cli init",
            "Initializing RAG system"
        )
    
    # Step 3: Quick test - index README
    if input("\n3. Test by indexing README.md? (y/n): ").lower() == 'y':
        run_command(
            'python -m toolkits.llm_rag.cli index-dir . --pattern "README.md"',
            "Indexing README.md as test"
        )
    
    # Step 4: Test search
    if input("\n4. Test search functionality? (y/n): ").lower() == 'y':
        run_command(
            'python -m toolkits.llm_rag.cli search "TuoKit"',
            "Testing search"
        )
    
    # Step 5: Launch UI
    if input("\n5. Launch Streamlit UI? (y/n): ").lower() == 'y':
        print("\n🌐 Launching web interface...")
        print("Press Ctrl+C to stop the server")
        subprocess.run("streamlit run toolkits/llm_rag/app.py", shell=True)
    
    print("\n✨ Setup complete! Check TASKLIST.md for next steps.")

if __name__ == "__main__":
    main()
