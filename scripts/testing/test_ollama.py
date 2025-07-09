#!/usr/bin/env python3
"""
TuoKit Ollama Test
Single unified test for Ollama connectivity
"""

import sys
import os

# Add project root to path
project_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, project_root)

from utils.ollama import test_ollama_setup

if __name__ == "__main__":
    print("\n[TEST] TuoKit Ollama Test\n")
    
    success, message = test_ollama_setup()
    
    if success:
        print("\n[OK] Ollama is working correctly!")
        print("\nYou can now use TuoKit with Ollama models.")
    else:
        print(f"\n[ERROR] Ollama test failed: {message}")
        print("\nTroubleshooting:")
        print("1. Ensure Ollama is installed: https://ollama.ai")
        print("2. Start Ollama service: ollama serve")
        print("3. Pull required models: ollama pull deepseek-r1:1.5b")
        
    sys.exit(0 if success else 1)
