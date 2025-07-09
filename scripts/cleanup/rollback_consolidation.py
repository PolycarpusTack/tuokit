"""
Quick rollback script if consolidation causes issues
"""
import shutil
from pathlib import Path
import sys

def rollback_consolidation():
    """Restore from backup"""
    print("TuoKit Consolidation Rollback")
    print("=" * 40)
    
    # Find backup directory
    backup_dirs = list(Path(".").glob("backup_before_consolidation_*"))
    
    if not backup_dirs:
        print("❌ No backup found! Cannot rollback.")
        return False
    
    backup_dir = backup_dirs[0]  # Use most recent
    print(f"Found backup: {backup_dir}")
    
    response = input("\nRestore from this backup? (y/N): ")
    if response.lower() != 'y':
        print("Rollback cancelled.")
        return False
    
    print("\nRestoring files...")
    
    # Restore each file
    for backup_file in backup_dir.rglob("*"):
        if backup_file.is_file():
            # Calculate relative path
            rel_path = backup_file.relative_to(backup_dir)
            dest_path = Path(".") / rel_path
            
            # Create directories if needed
            dest_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Copy file
            shutil.copy2(backup_file, dest_path)
            print(f"  ✓ Restored: {rel_path}")
    
    print("\n✅ Rollback complete!")
    print("\nNext steps:")
    print("1. Remove any newly created consolidated files")
    print("2. Restart your application")
    print("3. Test that everything works as before")
    
    return True

if __name__ == "__main__":
    success = rollback_consolidation()
    sys.exit(0 if success else 1)
