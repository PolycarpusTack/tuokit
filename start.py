#!/usr/bin/env python3
"""TuoKit Simple Launcher"""
import subprocess
import sys
import webbrowser
import time

def main():
    print("🚀 Starting TuoKit...")
    print("=" * 50)
    
    # Start streamlit in a non-blocking way
    process = subprocess.Popen([sys.executable, "-m", "streamlit", "run", "app.py"])
    
    # Give it a moment to start
    time.sleep(2)
    
    # Open browser
    print("📱 Opening browser at http://localhost:8501")
    webbrowser.open("http://localhost:8501")
    
    print("\n✅ TuoKit is running!")
    print("To stop: Press Ctrl+C or close this window")
    print("=" * 50)
    
    try:
        # Wait for the process
        process.wait()
    except KeyboardInterrupt:
        print("\n👋 Shutting down TuoKit...")
        process.terminate()

if __name__ == "__main__":
    main()
