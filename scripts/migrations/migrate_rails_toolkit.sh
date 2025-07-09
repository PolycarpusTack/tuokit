#!/bin/bash
# Rails Toolkit Migration Script for TuoKit
# Migrates from individual Rails tools to unified toolkit

echo "🛤️ Rails Toolkit Migration Script"
echo "================================="

# Check if we're in TuoKit directory
if [ ! -f "app.py" ]; then
    echo "❌ Error: Not in TuoKit root directory"
    echo "Please run from C:/Projects/Tuokit/"
    exit 1
fi

# Create backup directory
echo "📦 Creating backup directory..."
mkdir -p backups/rails_tools_backup

# Backup old Rails tools
echo "💾 Backing up old Rails tools..."
for tool in rails_controller_gen rails_debugger rails_graphql rails_model_gen rails_scaffold rails_system_tests rails_toolkit rails_upgrader; do
    if [ -f "${tool}.py" ]; then
        cp "${tool}.py" "backups/rails_tools_backup/"
        echo "  ✓ Backed up ${tool}.py"
    fi
done

# Move old tools to archive
echo "📁 Archiving old tools..."
mkdir -p archived/old_rails_tools
for tool in rails_controller_gen rails_debugger rails_graphql rails_model_gen rails_scaffold rails_system_tests rails_toolkit rails_upgrader; do
    if [ -f "${tool}.py" ]; then
        mv "${tool}.py" "archived/old_rails_tools/"
        echo "  ✓ Archived ${tool}.py"
    fi
done

# Update pages/__init__.py if it exists
if [ -f "pages/__init__.py" ]; then
    echo "📝 Updating pages/__init__.py..."
    # Remove old imports and add new one
    sed -i '/rails_controller_gen/d' pages/__init__.py
    sed -i '/rails_debugger/d' pages/__init__.py
    sed -i '/rails_graphql/d' pages/__init__.py
    sed -i '/rails_model_gen/d' pages/__init__.py
    sed -i '/rails_scaffold/d' pages/__init__.py
    sed -i '/rails_system_tests/d' pages/__init__.py
    sed -i '/rails_toolkit/d' pages/__init__.py
    sed -i '/rails_upgrader/d' pages/__init__.py
    
    # Add new import if not exists
    if ! grep -q "rails_ultimate_toolkit" pages/__init__.py; then
        echo "from . import rails_ultimate_toolkit" >> pages/__init__.py
    fi
fi

# Create quick access script
echo "🔧 Creating quick access script..."
cat > rails_tools.py << 'EOF'
"""
Quick access to Rails Ultimate Toolkit
Run: python rails_tools.py
"""

import streamlit as st
from rails_ultimate_toolkit import show

if __name__ == "__main__":
    show()
EOF

echo "✅ Migration complete!"
echo ""
echo "📋 Summary:"
echo "  - Old tools backed up to: backups/rails_tools_backup/"
echo "  - Old tools archived to: archived/old_rails_tools/"
echo "  - New toolkit: rails_ultimate_toolkit.py"
echo "  - Quick access: python rails_tools.py"
echo ""
echo "🚀 Next steps:"
echo "  1. Update your navigation menu to use 'rails_ultimate_toolkit'"
echo "  2. Test the new unified toolkit"
echo "  3. Remove backups once confirmed working"
echo ""
echo "Happy Rails development! 🛤️"
