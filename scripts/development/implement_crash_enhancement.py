#!/usr/bin/env python3
"""
Crash Analyzer V2 Enhancement Implementation Helper
Provides the exact prompt for the selected enhancement
"""

import sys
from pathlib import Path

# Enhancement prompts (shortened for display, full prompts in guide)
ENHANCEMENTS = {
    "1": {
        "name": "Knowledge Base Search Enhancement",
        "brief": "Add export, bulk ops, advanced search, and tagging to Knowledge Base",
        "prompt_file": "kb_search_prompt.txt"
    },
    "2": {
        "name": "Crash Pattern Recognition", 
        "brief": "Detect recurring patterns, sequences, and cascading failures",
        "prompt_file": "pattern_recognition_prompt.txt"
    },
    "3": {
        "name": "Comparative Analysis",
        "brief": "Add period comparison with side-by-side metrics",
        "prompt_file": "comparative_analysis_prompt.txt"
    },
    "4": {
        "name": "Smart Alerts (Phase 1.5)",
        "brief": "In-app alerts without email integration",
        "prompt_file": "smart_alerts_prompt.txt"
    },
    "5": {
        "name": "Predictive Analytics",
        "brief": "Basic statistical predictions using moving averages",
        "prompt_file": "predictive_analytics_prompt.txt"
    }
}

def show_menu():
    """Display enhancement selection menu"""
    print("\n" + "=" * 60)
    print("🚀 Crash Analyzer V2 Enhancement Implementation")
    print("=" * 60)
    print("\nSelect an enhancement to implement:\n")
    
    for key, enhancement in ENHANCEMENTS.items():
        print(f"{key}. {enhancement['name']}")
        print(f"   → {enhancement['brief']}\n")
    
    print("0. Exit")
    print("=" * 60)

def get_full_prompt(enhancement_num):
    """Extract the full prompt from the enhancement guide"""
    guide_path = Path(__file__).parent / "docs" / "CRASH_ANALYZER_V2_ENHANCEMENT_GUIDE.md"
    
    # Map enhancement number to task name in guide
    task_names = {
        "1": "Task 1: Knowledge Base Search Enhancement",
        "2": "Task 2: Crash Pattern Recognition",
        "3": "Task 3: Comparative Analysis",
        "4": "Task 4: Smart Alerts (Phase 1.5)",
        "5": "Task 5: Predictive Analytics (Basic)"
    }
    
    task_name = task_names.get(enhancement_num)
    if not task_name:
        return None
    
    # Read the guide
    with open(guide_path, 'r') as f:
        content = f.read()
    
    # Extract the task section
    start_marker = f"## 📚 {task_name}" if "Task 1" in task_name else f"## 🔍 {task_name}" if "Task 2" in task_name else f"## 📊 {task_name}" if "Task 3" in task_name else f"## 🚨 {task_name}" if "Task 4" in task_name else f"## 📈 {task_name}"
    
    # Find the start
    start_idx = content.find(start_marker)
    if start_idx == -1:
        return None
    
    # Find the next section (marked by ##)
    next_section_idx = content.find("\n## ", start_idx + 1)
    if next_section_idx == -1:
        next_section_idx = content.find("\n---", start_idx + 1)
    
    # Extract the section
    task_section = content[start_idx:next_section_idx] if next_section_idx != -1 else content[start_idx:]
    
    return task_section

def main():
    """Main execution"""
    while True:
        show_menu()
        choice = input("\nEnter your choice (0-5): ").strip()
        
        if choice == "0":
            print("\n👋 Exiting enhancement helper")
            break
        
        if choice in ENHANCEMENTS:
            enhancement = ENHANCEMENTS[choice]
            print(f"\n✅ Selected: {enhancement['name']}")
            print("\n" + "=" * 60)
            print("📋 IMPLEMENTATION GUIDE")
            print("=" * 60)
            
            # Get the full task section
            task_content = get_full_prompt(choice)
            if task_content:
                print(task_content)
            else:
                print("Error: Could not find task details in guide")
            
            print("\n" + "=" * 60)
            print("💡 QUICK COMMANDS")
            print("=" * 60)
            print("\n# After implementation, run quality gate tests:")
            
            test_commands = {
                "1": "python3 -m pytest tests/test_knowledge_base_enhancements.py -v",
                "2": "python3 -m pytest tests/test_pattern_detection.py -v",
                "3": "python3 -m pytest tests/test_comparative_analysis.py -v",
                "4": "python3 -m pytest tests/test_smart_alerts.py -v",
                "5": "python3 -m pytest tests/test_predictive_analytics.py -v"
            }
            
            print(f"{test_commands[choice]}")
            print("\n# Check implementation status:")
            print("python3 crash_analyzer_v2_enhancement_tracker.py")
            print("\n# Commit your changes:")
            print(f'git add -A && git commit -m "feat(crash-analyzer): implement {enhancement["name"].lower()}"')
            print("=" * 60)
            
            input("\nPress Enter to return to menu...")
        else:
            print("\n❌ Invalid choice. Please try again.")

if __name__ == "__main__":
    main()