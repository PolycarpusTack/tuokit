"""
Quick test to verify Enhanced Scanner components work
"""
import sys
from pathlib import Path

# Add tools to path
sys.path.append(str(Path(__file__).parent))

def test_components():
    print("Testing Enhanced Scanner Components...\n")
    
    # Test 1: Import main components
    try:
        from enhanced_scanner_full import EnhancedCodeScanner, EnhancedFixEngine
        print("[OK] Core components import successfully")
    except Exception as e:
        print(f"❌ Import error: {e}")
        return False
    
    # Test 2: Create scanner instance
    try:
        scanner = EnhancedCodeScanner("C:/Projects/Tuokit")
        print("✅ Scanner instance created")
    except Exception as e:
        print(f"❌ Scanner creation error: {e}")
        return False
    
    # Test 3: Create fixer instance
    try:
        fixer = EnhancedFixEngine()
        print("✅ Fix engine instance created")
    except Exception as e:
        print(f"❌ Fix engine error: {e}")
        return False
    
    # Test 4: Test TODO prompt generation
    try:
        test_todo = {
            'file': 'test.py',
            'line': 10,
            'type': 'todo',
            'message': 'Add caching for performance'
        }
        prompt = fixer.generate_todo_prompt(test_todo)
        print("✅ TODO prompt generation works")
        print(f"   Generated {len(prompt)} character prompt")
    except Exception as e:
        print(f"❌ Prompt generation error: {e}")
        return False
    
    # Test 5: Test backup directory
    try:
        backup_dir = Path("C:/Projects/Tuokit/backups/enhanced_scanner")
        backup_dir.mkdir(parents=True, exist_ok=True)
        print("✅ Backup directory accessible")
    except Exception as e:
        print(f"❌ Backup directory error: {e}")
        return False
    
    print("\n✅ All components working correctly!")
    print("\nYou can now run: python launch_enhanced_scanner.py")
    return True

if __name__ == "__main__":
    success = test_components()
    exit(0 if success else 1)
