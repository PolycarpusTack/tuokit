"""
Quick fix for numpy/matplotlib import issues
"""
import subprocess
import sys
import os

def fix_numpy():
    """Fix numpy installation issues"""
    print("Fixing numpy/matplotlib issues...")
    
    # Clear pip cache
    print("Clearing pip cache...")
    subprocess.run([sys.executable, "-m", "pip", "cache", "purge"])
    
    # Install numpy with specific version that works with Python 3.12
    print("Installing compatible numpy version...")
    subprocess.run([sys.executable, "-m", "pip", "install", "--force-reinstall", "numpy==1.26.0"])
    
    # Install matplotlib
    print("Installing matplotlib...")
    subprocess.run([sys.executable, "-m", "pip", "install", "--force-reinstall", "matplotlib==3.8.0"])
    
    print("Done! Try running the app again.")

if __name__ == "__main__":
    fix_numpy()
