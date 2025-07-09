#!/usr/bin/env python3
"""
Update TuoKit navigation after cleanup
Removes references to archived pages
"""

import re
import os
from datetime import datetime

def update_navigation():
    """Update app.py navigation to reflect cleaned structure"""
    
    # Pages to remove from navigation
    removed_pages = [
        "pages/agent_hub.py",
        "pages/agent_portal.py", 
        "pages/agent_unified.py",
        "pages/sql_generator.py",
        "pages/sql_optimizer.py",
        "pages/sql_pipeline.py",
        "pages/sql_suite.py"
    ]
    
    # New navigation entries
    new_entries = {
        "sql_toolkit": '    st.page_link("pages/sql_toolkit.py", label="🛢️ SQL Toolkit", icon="🛢️")',
        "agent": '    st.page_link("pages/agent_lite.py", label="🤖 AI Agent", icon="🤖")'
    }
    
    # Read current app.py
    app_path = "app.py"
    if not os.path.exists(app_path):
        print("Error: app.py not found")
        return
    
    with open(app_path, 'r') as f:
        content = f.read()
    
    # Backup original
    backup_path = f"app_backup_{datetime.now().strftime('%Y%m%d_%H%M%S')}.py"
    with open(backup_path, 'w') as f:
        f.write(content)
    print(f"Created backup: {backup_path}")
    
    # Remove old page links
    for page in removed_pages:
        page_name = os.path.basename(page)
        # Match various st.page_link patterns
        patterns = [
            rf'st\.page_link\(["\'].*{page_name}["\'].*?\)',
            rf'st\.page_link\(["\'].*{page_name}["\'].*?\n.*?\)',  # Multi-line
        ]
        
        for pattern in patterns:
            content = re.sub(pattern, '', content, flags=re.DOTALL | re.MULTILINE)
    
    # Clean up multiple empty lines
    content = re.sub(r'\n\s*\n\s*\n', '\n\n', content)
    
    # Add new unified tools section if not exists
    if "SQL Toolkit" not in content:
        # Find a good place to insert (after other tool links)
        insert_point = content.find("st.page_link(\"pages/code_tools.py\"")
        if insert_point > 0:
            # Find the end of this section
            section_end = content.find("\n\n", insert_point)
            if section_end > 0:
                new_section = f"""
    
    # Unified Tools
    st.divider()
    st.subheader("🎯 Unified Tools")
    {new_entries['sql_toolkit']}
    {new_entries['agent']}
"""
                content = content[:section_end] + new_section + content[section_end:]
    
    # Write updated content
    with open(app_path, 'w') as f:
        f.write(content)
    
    print("✅ Updated app.py navigation")
    print("\nRemoved references to:")
    for page in removed_pages:
        print(f"  - {page}")
    print("\nAdded unified tools section")
    print("\nPlease review app.py to ensure navigation looks correct")

def verify_pages_exist():
    """Verify that new pages exist before updating navigation"""
    required_pages = [
        "pages/sql_toolkit.py",
        "pages/agent_lite.py"
    ]
    
    missing = []
    for page in required_pages:
        if not os.path.exists(page):
            missing.append(page)
    
    if missing:
        print("⚠️  Warning: The following pages don't exist yet:")
        for page in missing:
            print(f"  - {page}")
        print("\nCreate these pages before updating navigation")
        return False
    
    return True

if __name__ == "__main__":
    print("🗺️  TuoKit Navigation Updater")
    print("=" * 40)
    
    if verify_pages_exist():
        response = input("Update app.py navigation? (y/n): ")
        if response.lower() == 'y':
            update_navigation()
        else:
            print("Navigation update cancelled")
    else:
        print("\nPlease create the unified tool pages first")
