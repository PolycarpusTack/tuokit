#!/usr/bin/env python
"""
Quick start script for TuoKit Enhanced Scanner v2.0
Run this to automatically set up and start the scanner
"""

import subprocess
import sys
import os
from pathlib import Path

def check_python_version():
    """Ensure Python 3.7+ is being used"""
    if sys.version_info < (3, 7):
        print("❌ Error: Python 3.7 or higher is required")
        print(f"Current version: {sys.version}")
        sys.exit(1)
    print(f"✅ Python {sys.version.split()[0]} detected")

def install_dependencies():
    """Install required packages"""
    print("\n📦 Installing dependencies...")
    
    requirements_file = Path(__file__).parent / "requirements_scanner_v2.txt"
    
    if not requirements_file.exists():
        print("❌ Error: requirements_scanner_v2.txt not found")
        print("Please ensure all scanner files are in the same directory")
        sys.exit(1)
    
    try:
        subprocess.check_call([
            sys.executable, "-m", "pip", "install", "-r", str(requirements_file)
        ])
        print("✅ Dependencies installed successfully")
    except subprocess.CalledProcessError:
        print("❌ Error installing dependencies")
        print("Try running: pip install -r requirements_scanner_v2.txt")
        sys.exit(1)

def check_optional_tools():
    """Check for optional external tools"""
    print("\n🛠️  Checking optional tools...")
    
    tools = {
        "flake8": "pip install flake8",
        "pylint": "pip install pylint",
        "bandit": "pip install bandit",
        "eslint": "npm install -g eslint"
    }
    
    for tool, install_cmd in tools.items():
        try:
            subprocess.run([tool, "--version"], 
                         capture_output=True, 
                         check=True)
            print(f"  ✅ {tool} is installed")
        except (subprocess.CalledProcessError, FileNotFoundError):
            print(f"  ⚠️  {tool} not found - install with: {install_cmd}")

def start_scanner():
    """Start the Streamlit application"""
    print("\n🚀 Starting TuoKit Enhanced Scanner v2.0...")
    
    scanner_file = Path(__file__).parent / "enhanced_scanner_v2.py"
    
    if not scanner_file.exists():
        print("❌ Error: enhanced_scanner_v2.py not found")
        sys.exit(1)
    
    try:
        # Start Streamlit
        subprocess.run([
            sys.executable, "-m", "streamlit", "run", 
            str(scanner_file),
            "--server.port", "8501",
            "--server.address", "localhost"
        ])
    except KeyboardInterrupt:
        print("\n\n👋 Scanner stopped by user")
    except Exception as e:
        print(f"❌ Error starting scanner: {e}")
        sys.exit(1)

def main():
    """Main entry point"""
    print("=" * 50)
    print("🔍 TuoKit Enhanced Scanner v2.0 - Quick Start")
    print("=" * 50)
    
    # Check Python version
    check_python_version()
    
    # Install dependencies
    install_dependencies()
    
    # Check optional tools
    check_optional_tools()
    
    # Start the scanner
    print("\n" + "=" * 50)
    print("Opening scanner in your default browser...")
    print("URL: http://localhost:8501")
    print("Press Ctrl+C to stop the scanner")
    print("=" * 50 + "\n")
    
    start_scanner()

if __name__ == "__main__":
    main()
