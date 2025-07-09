# TuoKit Unified Entry Point

## Overview

The TuoKit Unified entry point (`tuokit_unified.py`) consolidates all TuoKit applications, tools, and interfaces into a single, cohesive application. This replaces the need for multiple entry points (`app.py`, `app_modern.py`, `main.py`, etc.) with one unified interface.

## Architecture

### Tool Registry System

The unified app uses a hierarchical tool registry that organizes all tools into categories:

```python
TOOL_REGISTRY = {
    "category_id": {
        "name": "Category Name",
        "description": "Category description",
        "tools": {
            "tool_id": {
                "name": "Tool Name",
                "icon": "🔧",
                "description": "Tool description",
                "type": "page|toolkit|internal",
                "page": "path/to/page.py",      # for page type
                "function": "function_name"      # for toolkit/internal type
            }
        }
    }
}
```

### Tool Types

1. **Internal Tools**: Built directly into the unified app
   - Home Dashboard
   - Analytics
   - Activity Feed
   - Settings

2. **Page Tools**: Individual Streamlit pages in the `pages/` directory
   - Code tools (explainer, scanner, debugger, etc.)
   - SQL tools
   - Ruby/Rails tools
   - Agent systems

3. **Toolkit Tools**: Consolidated toolkits that were created during consolidation
   - Learning Toolkit (EduMind, Study Guide Generator, SQL Academy)
   - Documentation Toolkit (Doc Tools, Knowledge Library, Help)
   - SmallTalk Toolkit (all SmallTalk tools)
   - Error Analysis Toolkit (Error Decoder, Exception Advisor, Crash Analyzer)
   - Diagnostic Toolkit (System diagnostics and fixes)

## Features

### 1. Unified Navigation
- **Category-based sidebar**: Tools organized by purpose
- **Two-level navigation**: Category → Tool selection
- **Visual indicators**: Active category and tool highlighted
- **Tool descriptions**: Hover help for each tool

### 2. Built-in Dashboards

#### Home Dashboard
- Overview metrics (queries, tools, knowledge items)
- Tool category browser with quick launch
- Recent activity summary
- Quick action buttons

#### Analytics Dashboard
- Usage trends over time
- Tool popularity charts
- Model usage distribution
- Knowledge base growth tracking
- Configurable date ranges

#### Activity Feed
- Chronological activity log
- Query previews
- Knowledge save tracking
- Grouped by date

### 3. Centralized Settings
- Model configuration
- Feature toggles
- Database statistics
- Export/Import options
- About information

### 4. Smart Tool Loading
The unified app intelligently loads tools based on their type:
- Internal functions are called directly
- Page files are dynamically imported
- Toolkit main functions are invoked
- Error handling for missing tools

## Benefits

### 1. Single Entry Point
- No confusion about which app to run
- One command: `streamlit run tuokit_unified.py`
- All tools accessible from one interface

### 2. Consistent Experience
- Unified navigation across all tools
- Consistent styling and theme
- Shared model selection
- Integrated analytics

### 3. Better Organization
- Clear category structure
- Easy tool discovery
- Logical grouping of related tools
- Scalable architecture

### 4. Enhanced Features
- Cross-tool analytics
- Unified activity tracking
- Global settings management
- Integrated help system

## Migration Guide

### From Multiple Entry Points

1. **Instead of `app.py`**:
   ```bash
   # Old
   streamlit run app.py
   
   # New
   streamlit run tuokit_unified.py
   ```

2. **Instead of `app_modern.py`**:
   - All modern UI features integrated
   - Analytics dashboard included
   - Activity feed available

3. **Instead of individual toolkits**:
   - All toolkits accessible from categories
   - No need to run separate toolkit files
   - Integrated navigation

### Adding New Tools

To add a new tool to the unified app:

1. **For a page tool**:
   ```python
   "new_tool": {
       "name": "New Tool",
       "icon": "🆕",
       "description": "Description of the tool",
       "type": "page",
       "page": "pages/new_tool.py"
   }
   ```

2. **For an internal tool**:
   ```python
   "new_feature": {
       "name": "New Feature",
       "icon": "✨",
       "description": "Built-in feature",
       "type": "internal",
       "function": "show_new_feature"
   }
   ```

3. **For a toolkit**:
   ```python
   "new_toolkit": {
       "name": "New Toolkit",
       "icon": "🧰",
       "description": "Collection of tools",
       "type": "toolkit",
       "function": "new_toolkit_main"
   }
   ```

## Customization

### Theme
The unified app includes a modern dark theme with:
- Dark background (#0e1117)
- Card-style components
- Hover effects
- Smooth transitions
- Consistent color scheme

### Configuration
Modify `UNIFIED_CONFIG` to change:
- App name and version
- Default model
- Feature toggles
- Analytics settings

## Usage

### Basic Usage
```bash
# Start the unified app
streamlit run tuokit_unified.py

# With specific port
streamlit run tuokit_unified.py --server.port 8502
```

### Navigation Flow
1. Launch the app
2. Select a category from the sidebar
3. Choose a specific tool within the category
4. Tool loads in the main area
5. Use global model selector for AI features

### Best Practices
1. **Use categories**: Group related tools together
2. **Consistent naming**: Follow naming conventions
3. **Tool descriptions**: Provide helpful descriptions
4. **Error handling**: Handle missing dependencies gracefully
5. **Performance**: Lazy load tools only when selected

## Future Enhancements

1. **Plugin System**: Dynamic tool loading from plugins
2. **User Profiles**: Personalized tool recommendations
3. **Keyboard Shortcuts**: Quick tool switching
4. **Search**: Global search across all tools
5. **Favorites**: Pin frequently used tools
6. **Themes**: Multiple theme options
7. **Export/Import**: Full configuration management

The TuoKit Unified entry point represents the evolution of TuoKit from a collection of tools to a integrated development environment for AI-assisted programming.