#!/usr/bin/env python3
"""
Pre-migration checker - Ensures everything is ready
Run this BEFORE starting the migration
"""
import os
import sys
from pathlib import Path
import subprocess

def check_environment():
    """Check Python environment and dependencies"""
    print("🔍 Checking Environment...")
    
    # Python version
    py_version = sys.version_info
    print(f"  Python version: {py_version.major}.{py_version.minor}.{py_version.micro}", end=" ")
    if py_version >= (3, 7):
        print("✅")
    else:
        print("❌ (Need 3.7+)")
        return False
    
    # Required packages
    required = ['streamlit', 'pandas', 'requests', 'ollama']
    missing = []
    
    for package in required:
        try:
            __import__(package)
            print(f"  {package}: ✅")
        except ImportError:
            print(f"  {package}: ❌")
            missing.append(package)
    
    if missing:
        print(f"\n⚠️  Missing packages: {', '.join(missing)}")
        print("Install with: pip install " + " ".join(missing))
        return False
    
    return True

def check_ollama():
    """Check Ollama connection"""
    print("\n🔍 Checking Ollama...")
    
    try:
        # Try running test script
        result = subprocess.run(
            [sys.executable, "test_ollama_simple.py"],
            capture_output=True,
            text=True,
            timeout=10
        )
        
        if "SUCCESS" in result.stdout:
            print("  Ollama connection: ✅")
            
            # Extract model count
            import re
            models = re.search(r'Found (\d+) models', result.stdout)
            if models:
                print(f"  Available models: {models.group(1)}")
            
            return True
        else:
            print("  Ollama connection: ❌")
            print("  Run 'ollama serve' and try again")
            return False
            
    except FileNotFoundError:
        print("  test_ollama_simple.py not found")
        return False
    except Exception as e:
        print(f"  Error checking Ollama: {e}")
        return False

def check_migration_scripts():
    """Check all migration scripts are present"""
    print("\n🔍 Checking Migration Scripts...")
    
    scripts = [
        ("migrate_tuokit.py", "Master migration script"),
        ("extract_unique_features.py", "Feature analysis"),
        ("smart_cleanup.py", "Create unified tools"),
        ("add_feature_toggle.py", "Add UI toggle"),
        ("complete_unified_tools.py", "Complete implementations"),
        ("test_unified_tools.py", "Test suite"),
        ("archive_old_tools.py", "Final archival")
    ]
    
    all_present = True
    for script, description in scripts:
        if Path(script).exists():
            print(f"  {script}: ✅ ({description})")
        else:
            print(f"  {script}: ❌ ({description})")
            all_present = False
    
    return all_present

def check_target_files():
    """Check files that will be migrated"""
    print("\n🔍 Checking Target Files...")
    
    # SQL tools
    sql_tools = [
        "pages/sql_generator.py",
        "pages/sql_optimizer.py",
        "pages/sql_pipeline.py"
    ]
    
    print("  SQL Tools:")
    sql_count = 0
    for tool in sql_tools:
        if Path(tool).exists():
            print(f"    {tool}: ✅")
            sql_count += 1
        else:
            print(f"    {tool}: ❓ (not found)")
    
    # Agent systems
    agent_files = [
        "pages/agent_lite.py",
        "pages/agent_portal.py",
        "pages/agent_hub.py"
    ]
    
    print("\n  Agent Systems:")
    agent_count = 0
    for agent in agent_files:
        if Path(agent).exists():
            print(f"    {agent}: ✅")
            agent_count += 1
        else:
            print(f"    {agent}: ❓ (not found)")
    
    print(f"\n  Found {sql_count} SQL tools and {agent_count} agent systems to migrate")
    return sql_count > 0 or agent_count > 0

def check_backup_space():
    """Check available disk space for backups"""
    print("\n🔍 Checking Disk Space...")
    
    try:
        import shutil
        stats = shutil.disk_usage(".")
        free_gb = stats.free / (1024**3)
        
        print(f"  Free space: {free_gb:.1f} GB", end=" ")
        if free_gb > 1:
            print("✅")
            return True
        else:
            print("⚠️  (Low space)")
            return True  # Warning only
    except:
        print("  Could not check disk space")
        return True  # Don't block on this

def main():
    """Run all pre-migration checks"""
    print("🚀 TuoKit Pre-Migration Checker")
    print("=" * 60)
    
    checks = [
        ("Environment", check_environment),
        ("Ollama", check_ollama),
        ("Migration Scripts", check_migration_scripts),
        ("Target Files", check_target_files),
        ("Disk Space", check_backup_space)
    ]
    
    results = {}
    for name, check_func in checks:
        results[name] = check_func()
    
    # Summary
    print("\n" + "=" * 60)
    print("📊 Pre-Migration Summary:")
    print("=" * 60)
    
    all_good = True
    for name, passed in results.items():
        status = "✅ READY" if passed else "❌ NEEDS ATTENTION"
        print(f"  {name}: {status}")
        if not passed:
            all_good = False
    
    print("\n" + "=" * 60)
    
    if all_good:
        print("✅ All checks passed! Ready to migrate.")
        print("\n🎯 Next steps:")
        print("  1. Review MIGRATION_QUICK_START.md")
        print("  2. Run: python migrate_tuokit.py --mode=analysis")
        print("  3. Follow the prompts")
    else:
        print("❌ Some checks failed. Please fix issues above.")
        print("\n💡 Tips:")
        print("  - Install missing packages with pip")
        print("  - Make sure Ollama is running")
        print("  - Check that all migration scripts are present")
    
    return all_good

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
