#!/usr/bin/env python3
"""
TuoKit Smart Migration - Master Execution Script
Run this to execute the complete migration process
"""
import subprocess
import sys
import os
from pathlib import Path
from datetime import datetime

class MigrationRunner:
    def __init__(self):
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.log_file = f"migration_log_{self.timestamp}.txt"
        self.steps_completed = []
        
    def log(self, message):
        """Log to console and file"""
        print(message)
        with open(self.log_file, 'a') as f:
            f.write(f"{datetime.now().strftime('%H:%M:%S')} - {message}\n")
    
    def run_step(self, step_name, script_name, description):
        """Run a migration step"""
        self.log(f"\n{'='*60}")
        self.log(f"🚀 {step_name}: {description}")
        self.log(f"{'='*60}")
        
        if not Path(script_name).exists():
            self.log(f"❌ Error: {script_name} not found!")
            return False
        
        try:
            result = subprocess.run(
                [sys.executable, script_name],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                self.log(f"✅ {step_name} completed successfully")
                self.steps_completed.append(step_name)
                
                # Show key output
                if result.stdout:
                    important_lines = [
                        line for line in result.stdout.split('\n')
                        if any(marker in line for marker in ['✓', '✅', 'Created', 'Found'])
                    ]
                    for line in important_lines[:5]:  # Show first 5 important lines
                        self.log(f"  {line.strip()}")
                
                return True
            else:
                self.log(f"❌ {step_name} failed!")
                if result.stderr:
                    self.log(f"Error: {result.stderr}")
                return False
                
        except Exception as e:
            self.log(f"❌ Exception running {step_name}: {str(e)}")
            return False
    
    def run_migration(self, mode='analysis'):
        """Run the migration process"""
        self.log("🚀 TuoKit Smart Migration Process")
        self.log(f"Mode: {mode}")
        self.log(f"Log file: {self.log_file}")
        
        if mode == 'analysis':
            # Step 1: Feature Analysis
            if not self.run_step(
                "Step 1",
                "extract_unique_features.py",
                "Analyze codebase for unique features"
            ):
                return False
            
            self.log("\n📊 Analysis complete!")
            self.log("Review these files:")
            self.log("  - consolidation_plan_*.md (human-readable report)")
            self.log("  - feature_analysis_*.json (detailed data)")
            self.log("\nNext: Run 'python migrate_tuokit.py --mode=implement'")
            
        elif mode == 'implement':
            # Step 2: Create unified tools
            if not self.run_step(
                "Step 2",
                "smart_cleanup.py",
                "Create unified tool templates"
            ):
                return False
            
            # Step 3: Add feature toggle
            if not self.run_step(
                "Step 3", 
                "add_feature_toggle.py",
                "Add experimental feature toggle to app.py"
            ):
                return False
            
            # Step 4: Complete implementations
            if not self.run_step(
                "Step 4",
                "complete_unified_tools.py",
                "Port implementations to unified tools"
            ):
                return False
            
            # Step 5: Run tests
            if not self.run_step(
                "Step 5",
                "test_unified_tools.py",
                "Test unified tools"
            ):
                self.log("⚠️  Tests failed but continuing...")
            
            self.log("\n✅ Implementation complete!")
            self.log("\n📋 Next steps:")
            self.log("  1. Start TuoKit: streamlit run app.py")
            self.log("  2. Toggle is OFF by default (safe!)")
            self.log("  3. Test with 'Use Next-Gen Tools' toggle")
            self.log("  4. Monitor for 14 days")
            self.log("  5. Run 'python migrate_tuokit.py --mode=finalize'")
            
        elif mode == 'finalize':
            # Final step: Archive old tools
            self.log("📦 Preparing to archive old tools...")
            self.log("This will:")
            self.log("  - Check migration metrics")
            self.log("  - Archive old tool files")
            self.log("  - Update navigation")
            
            response = input("\nRun in dry-run mode first? (y/n): ")
            
            if response.lower() == 'y':
                subprocess.run([sys.executable, "archive_old_tools.py", "--dry-run"])
                
                self.log("\n👆 Review the dry-run output above")
                final = input("Proceed with actual archival? (yes/no): ")
                
                if final.lower() == 'yes':
                    self.run_step(
                        "Step 6",
                        "archive_old_tools.py",
                        "Archive old tool files"
                    )
            else:
                self.run_step(
                    "Step 6",
                    "archive_old_tools.py",
                    "Archive old tool files"
                )
        
        else:
            self.log(f"❌ Unknown mode: {mode}")
            self.log("Valid modes: analysis, implement, finalize")
            return False
        
        # Summary
        self.log(f"\n{'='*60}")
        self.log(f"📊 Migration Summary")
        self.log(f"{'='*60}")
        self.log(f"Steps completed: {len(self.steps_completed)}")
        for step in self.steps_completed:
            self.log(f"  ✅ {step}")
        self.log(f"\nLog saved to: {self.log_file}")
        
        return True


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(
        description="TuoKit Smart Migration Process",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python migrate_tuokit.py --mode=analysis    # Step 1: Analyze codebase
  python migrate_tuokit.py --mode=implement   # Steps 2-5: Create unified tools
  python migrate_tuokit.py --mode=finalize    # Step 6: Archive old tools

Migration phases:
  1. Analysis   - Understand what needs to be preserved
  2. Implement  - Create unified tools with feature toggle
  3. Monitor    - 14 days of parallel testing
  4. Finalize   - Archive old tools after success
        """
    )
    
    parser.add_argument(
        '--mode',
        choices=['analysis', 'implement', 'finalize'],
        default='analysis',
        help='Migration phase to execute'
    )
    
    args = parser.parse_args()
    
    # Check Python version
    if sys.version_info < (3, 7):
        print("❌ Python 3.7+ required")
        sys.exit(1)
    
    # Run migration
    runner = MigrationRunner()
    success = runner.run_migration(args.mode)
    
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
