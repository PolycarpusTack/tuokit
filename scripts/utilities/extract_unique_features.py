#!/usr/bin/env python3
"""
TuoKit Feature Extraction & Analysis Tool
Step 1 of the smart migration process
"""
import ast
import os
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Set, Tuple
import hashlib

class FeatureExtractor:
    """Extract and analyze features from TuoKit codebase"""
    
    def __init__(self, project_root: str = "C:/Projects/Tuokit"):
        self.project_root = Path(project_root)
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.results = {
            'timestamp': self.timestamp,
            'summary': {},
            'sql': {},
            'agents': {},
            'ruby_rails': {},
            'smalltalk': {}
        }
    
    def run_complete_analysis(self):
        """Run analysis on all tool groups"""
        print("🔍 TuoKit Feature Extraction Starting...")
        print("=" * 60)
        
        # Analyze each tool group
        self.analyze_sql_tools()
        self.analyze_agent_systems()
        self.analyze_ruby_rails_tools()
        self.analyze_smalltalk_tools()
        
        # Generate reports
        self.save_analysis()
        self.generate_report()
        
        print("\n✅ Analysis Complete!")
        print(f"📊 Raw data: feature_analysis_{self.timestamp}.json")
        print(f"📄 Report: consolidation_plan_{self.timestamp}.md")
    
    def analyze_sql_tools(self):
        """Analyze SQL tool implementations"""
        print("\n📊 Analyzing SQL Tools...")
        
        sql_files = [
            "pages/sql_generator.py",
            "pages/sql_optimizer.py",
            "pages/sql_pipeline.py",
            "pages/sql_suite.py"
        ]
        
        features_by_file = {}
        all_functions = {}
        
        for sql_file in sql_files:
            file_path = self.project_root / sql_file
            if file_path.exists():
                features = self.extract_features(file_path)
                features_by_file[sql_file] = features
                
                # Track function occurrences
                for func in features['functions']:
                    name = func['name']
                    if name not in all_functions:
                        all_functions[name] = []
                    all_functions[name].append({
                        'file': sql_file,
                        'details': func
                    })
                
                print(f"  ✓ {sql_file}: {len(features['functions'])} functions")
        
        # Categorize functions
        unique_functions = {}
        duplicate_functions = {}
        
        for func_name, occurrences in all_functions.items():
            if len(occurrences) == 1:
                file = occurrences[0]['file']
                if file not in unique_functions:
                    unique_functions[file] = []
                unique_functions[file].append(occurrences[0]['details'])
            else:
                duplicate_functions[func_name] = occurrences
        
        # Calculate metrics
        total_functions = sum(len(f['functions']) for f in features_by_file.values())
        duplicate_count = len(duplicate_functions)
        
        self.results['sql'] = {
            'files': list(features_by_file.keys()),
            'features_by_file': features_by_file,
            'unique_functions': unique_functions,
            'duplicate_functions': duplicate_functions,
            'metrics': {
                'total_functions': total_functions,
                'unique_count': len(unique_functions),
                'duplicate_count': duplicate_count,
                'duplication_rate': f"{(duplicate_count / total_functions * 100):.1f}%" if total_functions > 0 else "0%"
            }
        }
        
        print(f"  📈 Found {duplicate_count} duplicate functions ({self.results['sql']['metrics']['duplication_rate']} duplication)")
    
    def analyze_agent_systems(self):
        """Analyze agent implementations"""
        print("\n🤖 Analyzing Agent Systems...")
        
        agent_files = [
            "pages/agent_lite.py",
            "pages/agent_portal.py",
            "pages/agent_unified.py",
            "pages/agent_hub.py",
            "agent_system.py",
            "team_agent.py"
        ]
        
        features_by_file = {}
        
        for agent_file in agent_files:
            file_path = self.project_root / agent_file
            if file_path.exists():
                features = self.extract_features(file_path)
                features_by_file[agent_file] = features
                print(f"  ✓ {agent_file}: {len(features['functions'])} functions, {len(features['classes'])} classes")
        
        self.results['agents'] = {
            'files': list(features_by_file.keys()),
            'features_by_file': features_by_file,
            'recommendation': 'Keep agent_lite.py - most aligned with TuoKit principles'
        }
    
    def analyze_ruby_rails_tools(self):
        """Analyze Ruby/Rails tools"""
        print("\n💎 Analyzing Ruby/Rails Tools...")
        
        pages_dir = self.project_root / "pages"
        rails_tools = []
        ruby_tools = []
        
        if pages_dir.exists():
            for file in pages_dir.iterdir():
                if file.suffix == '.py':
                    if file.name.startswith('rails_'):
                        rails_tools.append(file.name)
                    elif file.name.startswith('ruby_'):
                        ruby_tools.append(file.name)
                    elif file.name == 'rspec_generator.py':
                        rails_tools.append(file.name)
        
        self.results['ruby_rails'] = {
            'rails_tools': rails_tools,
            'ruby_tools': ruby_tools,
            'total': len(rails_tools) + len(ruby_tools),
            'recommendation': f'Consolidate {len(rails_tools)} Rails tools → rails_toolkit.py, {len(ruby_tools)} Ruby tools → ruby_toolkit.py'
        }
        
        print(f"  ✓ Found {len(rails_tools)} Rails tools")
        print(f"  ✓ Found {len(ruby_tools)} Ruby tools")
    
    def analyze_smalltalk_tools(self):
        """Analyze SmallTalk tools"""
        print("\n🗣️ Analyzing SmallTalk Tools...")
        
        pages_dir = self.project_root / "pages"
        smalltalk_tools = []
        
        if pages_dir.exists():
            for file in pages_dir.iterdir():
                if file.suffix == '.py' and file.name.startswith('smalltalk_'):
                    smalltalk_tools.append(file.name)
        
        self.results['smalltalk'] = {
            'tools': smalltalk_tools,
            'total': len(smalltalk_tools),
            'recommendation': f'Consolidate {len(smalltalk_tools)} tools → smalltalk_toolkit.py'
        }
        
        print(f"  ✓ Found {len(smalltalk_tools)} SmallTalk tools")
    
    def extract_features(self, file_path: Path) -> Dict:
        """Extract features from a Python file"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            tree = ast.parse(content)
            
            functions = []
            classes = []
            imports = []
            
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    functions.append({
                        'name': node.name,
                        'args': [arg.arg for arg in node.args.args],
                        'docstring': ast.get_docstring(node) or "",
                        'decorators': [d.id if hasattr(d, 'id') else str(d) for d in node.decorator_list],
                        'line': node.lineno
                    })
                elif isinstance(node, ast.ClassDef):
                    methods = []
                    for item in node.body:
                        if isinstance(item, ast.FunctionDef):
                            methods.append(item.name)
                    classes.append({
                        'name': node.name,
                        'methods': methods,
                        'docstring': ast.get_docstring(node) or "",
                        'line': node.lineno
                    })
                elif isinstance(node, ast.Import):
                    for alias in node.names:
                        imports.append(alias.name)
                elif isinstance(node, ast.ImportFrom) and node.module:
                    imports.append(node.module)
            
            # Calculate file hash for change detection
            file_hash = hashlib.md5(content.encode()).hexdigest()[:8]
            
            return {
                'functions': functions,
                'classes': classes,
                'imports': list(set(imports)),
                'line_count': len(content.split('\n')),
                'file_hash': file_hash
            }
            
        except Exception as e:
            print(f"  ⚠️  Error parsing {file_path}: {e}")
            return {'functions': [], 'classes': [], 'imports': [], 'error': str(e)}
    
    def save_analysis(self):
        """Save analysis results to JSON"""
        output_file = f"feature_analysis_{self.timestamp}.json"
        with open(output_file, 'w') as f:
            json.dump(self.results, f, indent=2)
    
    def generate_report(self):
        """Generate human-readable markdown report"""
        report = f"""# TuoKit Consolidation Plan
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Executive Summary

- **SQL Tools**: {self.results['sql']['metrics']['duplication_rate']} duplication across {len(self.results['sql']['files'])} files
- **Agent Systems**: {len(self.results['agents']['files'])} competing implementations
- **Ruby/Rails**: {self.results['ruby_rails']['total']} tools can be consolidated to 2
- **SmallTalk**: {self.results['smalltalk']['total']} tools can be unified

## Detailed Analysis

### 🛢️ SQL Tools Consolidation

**Files analyzed:**
"""
        
        for file in self.results['sql']['files']:
            report += f"- {file}\n"
        
        report += f"\n**Duplication found:**\n"
        for func_name, occurrences in self.results['sql']['duplicate_functions'].items():
            files = [occ['file'] for occ in occurrences]
            report += f"- `{func_name}()` appears in: {', '.join(files)}\n"
        
        report += f"\n**Unique features to preserve:**\n"
        for file, functions in self.results['sql']['unique_functions'].items():
            if functions:
                report += f"\n*{file}:*\n"
                for func in functions[:3]:  # Show first 3
                    report += f"- `{func['name']}()` - {func['docstring'].split(chr(10))[0] if func['docstring'] else 'No description'}\n"
                if len(functions) > 3:
                    report += f"- ... and {len(functions) - 3} more unique functions\n"
        
        report += f"""
### 🤖 Agent Systems

**Recommendation**: {self.results['agents']['recommendation']}

**Files found:**
"""
        for file in self.results['agents']['files']:
            report += f"- {file}\n"
        
        report += f"""
### 💎 Ruby/Rails Tools

{self.results['ruby_rails']['recommendation']}

**Rails tools ({len(self.results['ruby_rails']['rails_tools'])}):**
"""
        for tool in self.results['ruby_rails']['rails_tools'][:5]:
            report += f"- {tool}\n"
        if len(self.results['ruby_rails']['rails_tools']) > 5:
            report += f"- ... and {len(self.results['ruby_rails']['rails_tools']) - 5} more\n"
        
        report += f"""
### 🗣️ SmallTalk Tools

{self.results['smalltalk']['recommendation']}

## Implementation Strategy

### Phase 1: Create Unified Versions (Day 1-2)
1. Run `smart_cleanup.py` to create unified tool templates
2. Port unique features identified above
3. Add compatibility wrappers for old function names

### Phase 2: Parallel Testing (Day 3-14)
1. Deploy with feature toggle (default OFF)
2. Monitor usage via migration dashboard
3. Fix issues as they arise

### Phase 3: Switch & Archive (Day 15-30)
1. Enable new tools by default (Day 15)
2. Monitor for 2 weeks
3. Archive old files (Day 30)

## Expected Outcomes

- **File reduction**: ~{len(self.results['sql']['files']) + len(self.results['agents']['files']) + self.results['ruby_rails']['total'] + self.results['smalltalk']['total']} → ~10 files
- **Code duplication**: Eliminated
- **Maintenance**: Significantly easier
- **User experience**: Unchanged (with option for improvements)
"""
        
        output_file = f"consolidation_plan_{self.timestamp}.md"
        with open(output_file, 'w') as f:
            f.write(report)


def main():
    """Run feature extraction and analysis"""
    extractor = FeatureExtractor()
    extractor.run_complete_analysis()
    
    print("\n🎯 Next step: Run smart_cleanup.py to create unified tools")


if __name__ == "__main__":
    main()
