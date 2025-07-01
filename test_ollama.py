#!/usr/bin/env python3
"""
TuoKit Ollama Connection Test
Tests if Ollama is properly installed and accessible
"""

import sys

def test_ollama():
    """Test Ollama connection and list available models"""
    print("🔍 Testing Ollama connection...")
    print("-" * 50)
    
    try:
        import ollama
        print("✅ Ollama library imported successfully")
    except ImportError:
        print("❌ Ollama library not found. Please run: pip install ollama")
        return False
    
    try:
        # List available models
        models = ollama.list()
        print(f"\n✅ Connected to Ollama! Found {len(models['models'])} models:")
        print("-" * 50)
        
        for model in models['models']:
            name = model['name']
            size = model.get('size', 0) / (1024**3)  # Convert to GB
            print(f"  • {name:<30} ({size:.1f} GB)")
        
        print("-" * 50)
        
        # Test generation with a simple prompt
        print("\n🧪 Testing generation with first available model...")
        if models['models']:
            test_model = models['models'][0]['name']
            print(f"Using model: {test_model}")
            
            response = ollama.generate(
                model=test_model,
                prompt="Say 'Hello from TuoKit!' in exactly 5 words."
            )
            
            print(f"Response: {response['response'].strip()}")
            print("\n✅ Ollama is working correctly!")
            return True
        else:
            print("⚠️  No models found. Please install a model:")
            print("   ollama pull deepseek-coder:6.7b")
            return False
            
    except Exception as e:
        print(f"\n❌ Error connecting to Ollama: {e}")
        print("\nTroubleshooting steps:")
        print("1. Make sure Ollama is installed: https://ollama.ai")
        print("2. Start Ollama service: ollama serve")
        print("3. Pull a model: ollama pull deepseek-coder:6.7b")
        return False

if __name__ == "__main__":
    print("🧠 TuoKit - Ollama Connection Test")
    print("=" * 50)
    
    success = test_ollama()
    sys.exit(0 if success else 1)