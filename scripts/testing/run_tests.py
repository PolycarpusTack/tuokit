#!/usr/bin/env python3
"""
Quick test runner script for TuoKit
Simple wrapper around test_runner.py with common use cases
"""

import sys
import os
import subprocess

# Add current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

def run_command(cmd):
    """Run a command and print output"""
    print(f"\n🚀 Running: {' '.join(cmd)}")
    print("-" * 60)
    result = subprocess.run(cmd, text=True)
    return result.returncode

def main():
    """Main test runner entry point"""
    print("🧪 TuoKit Test Runner")
    print("=" * 60)
    
    if len(sys.argv) > 1:
        # Pass arguments to test runner
        cmd = [sys.executable, 'test_runner.py'] + sys.argv[1:]
        return run_command(cmd)
    
    # Interactive mode
    print("\nSelect test option:")
    print("1. Run all tests")
    print("2. Run SQL tests")
    print("3. Run Agent tests")
    print("4. Run Ollama tests")
    print("5. Run Tool tests")
    print("6. Run UI tests")
    print("7. Run Knowledge tests")
    print("8. List all tests")
    print("9. Run with custom options")
    print("0. Exit")
    
    choice = input("\nEnter choice (0-9): ").strip()
    
    if choice == '0':
        return 0
    elif choice == '1':
        return run_command([sys.executable, 'test_runner.py'])
    elif choice == '2':
        return run_command([sys.executable, 'test_runner.py', '--category', 'sql'])
    elif choice == '3':
        return run_command([sys.executable, 'test_runner.py', '--category', 'agent'])
    elif choice == '4':
        return run_command([sys.executable, 'test_runner.py', '--category', 'ollama'])
    elif choice == '5':
        return run_command([sys.executable, 'test_runner.py', '--category', 'tools'])
    elif choice == '6':
        return run_command([sys.executable, 'test_runner.py', '--category', 'ui'])
    elif choice == '7':
        return run_command([sys.executable, 'test_runner.py', '--category', 'knowledge'])
    elif choice == '8':
        return run_command([sys.executable, 'test_runner.py', '--list'])
    elif choice == '9':
        print("\nCustom options:")
        print("  --parallel      Run tests in parallel")
        print("  --sequential    Run tests sequentially")
        print("  --verbose       Increase verbosity")
        print("  --no-html       Skip HTML report")
        print("  --timeout N     Set timeout in seconds")
        print("  --file FILE     Run specific test file")
        
        custom_args = input("\nEnter options: ").strip().split()
        return run_command([sys.executable, 'test_runner.py'] + custom_args)
    else:
        print("Invalid choice!")
        return 1

if __name__ == '__main__':
    sys.exit(main())