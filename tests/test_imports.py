"""
Simple test to check imports
"""

print("Starting import test...")

try:
    print("1. Importing sys...")
    import sys
    print("   ✅ sys imported")
    
    print("2. Adding path...")
    sys.path.insert(0, 'C:/Projects/Tuokit')
    print("   ✅ Path added")
    
    print("3. Importing DatabaseManager...")
    from utils import DatabaseManager
    print("   ✅ DatabaseManager imported")
    
    print("4. Creating DatabaseManager instance...")
    db = DatabaseManager()
    print(f"   ✅ DatabaseManager created, connected: {db.connected}")
    
except Exception as e:
    print(f"   ❌ Error: {e}")
    import traceback
    traceback.print_exc()

print("\nTest complete!")
