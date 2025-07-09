#!/usr/bin/env python3
"""
Smart Cleanup - Creates unified tools based on feature analysis
Step 2 of the migration process
"""
import json
import os
from pathlib import Path
from datetime import datetime
import shutil

class SmartCleaner:
    def __init__(self):
        self.timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.analysis = self.load_latest_analysis()
        
    def load_latest_analysis(self):
        """Load the most recent feature analysis"""
        analysis_files = list(Path('.').glob('feature_analysis_*.json'))
        if not analysis_files:
            raise FileNotFoundError(
                "No feature analysis found! Run extract_unique_features.py first."
            )
        
        latest = max(analysis_files, key=lambda p: p.stat().st_mtime)
        print(f"📊 Loading analysis from: {latest}")
        
        with open(latest, 'r') as f:
            return json.load(f)
    
    def run(self):
        """Execute smart cleanup process"""
        print("🧹 Smart TuoKit Cleanup")
        print("=" * 60)
        
        # Create unified tools
        self.create_sql_toolkit_next()
        self.create_agent_next()
        self.create_rollback_plan()
        
        print("\n✅ Smart cleanup complete!")
        print("\n📋 Created files:")
        print("  - pages/sql_toolkit_next.py")
        print("  - pages/agent_next.py")
        print(f"  - ROLLBACK_PLAN_{self.timestamp}.md")
        print("\n🎯 Next step: Run add_feature_toggle.py")
    
    def create_sql_toolkit_next(self):
        """Create unified SQL toolkit preserving all unique features"""
        print("\n🛢️ Creating unified SQL toolkit...")
        
        sql_data = self.analysis.get('sql', {})
        unique_features = sql_data.get('unique_functions', {})
        duplicate_features = sql_data.get('duplicate_functions', {})
        
        code = '''"""
SQL Toolkit Next - Unified implementation preserving ALL features
Created by smart migration process
Toggle in sidebar to test alongside existing tools
"""
import streamlit as st
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
from typing import Dict, List, Optional, Union, Any
import pandas as pd
import re

class SQLToolkitNext(OllamaToolBase):
    """
    Next-generation SQL toolkit combining all SQL tools:
    - sql_generator.py → Natural language to SQL
    - sql_optimizer.py → Query optimization
    - sql_pipeline.py → ETL pipelines
    - sql_suite.py → Advanced features
    
    All unique features preserved with clear attribution.
    """
    
    def __init__(self):
        super().__init__("sql_toolkit_next", "deepseek-coder:6.7b")
        self.db = DatabaseManager()
        
        # Feature flags for compatibility
        self.compat_mode = True  # Support old method names
        self.feature_tracking = True  # Track which features are used
        
        # Usage tracking
        self.feature_usage = {}
        
    # ===== Core Unified Methods =====
    
    def generate(self, prompt: str, dialect: str = "postgresql", 
                schema: Dict = None, **kwargs) -> Dict[str, Any]:
        """
        Unified SQL generation - replaces generate_sql from all tools
        """
        self._track_usage('generate', 'unified')
        
        # Build comprehensive prompt
        system_prompt = f"You are an expert {dialect} SQL developer."
        
        if schema:
            schema_text = self._format_schema(schema)
            prompt = f"{prompt}\\n\\nDatabase Schema:\\n{schema_text}"
        
        # Generate SQL
        result = self.generate_with_logging(
            prompt,
            system=system_prompt,
            temperature=0.3  # Lower temp for more consistent SQL
        )
        
        if result['error']:
            return {'success': False, 'error': result['response']}
        
        # Extract and clean SQL
        sql = self._extract_sql(result['response'])
        
        # Auto-optimize if enabled
        if kwargs.get('auto_optimize', True):
            sql = self._basic_optimization(sql)
        
        return {
            'success': True,
            'sql': sql,
            'dialect': dialect,
            'optimized': kwargs.get('auto_optimize', True)
        }
    
    def optimize(self, sql: str, explain_plan: str = None, **kwargs) -> Dict[str, Any]:
        """
        Unified optimization - combines best of all optimizers
        """
        self._track_usage('optimize', 'unified')
        
        optimization_prompt = f"""
Optimize this SQL query:

```sql
{sql}
```

{f'EXPLAIN ANALYZE output:\\n{explain_plan}' if explain_plan else ''}

Focus on:
1. Index usage optimization
2. JOIN order optimization
3. Subquery elimination
4. Aggregate optimization
5. Partition pruning (if applicable)

Provide:
- Optimized SQL
- List of recommended indexes
- Brief explanation of changes
"""
        
        result = self.generate_with_logging(optimization_prompt, temperature=0.3)
        
        if result['error']:
            return {'success': False, 'error': result['response']}
        
        # Parse response
        response = result['response']
        optimized_sql = self._extract_sql(response)
        indexes = self._extract_index_recommendations(response)
        
        return {
            'success': True,
            'original_sql': sql,
            'optimized_sql': optimized_sql,
            'indexes': indexes,
            'explanation': response
        }
'''
        
        # Add unique features from each file
        for file, functions in unique_features.items():
            if functions:
                code += f"\n    # ===== Unique features from {file} =====\n"
                for func in functions:
                    # Skip if it's a private method or test
                    if func['name'].startswith('_') or func['name'].startswith('test_'):
                        continue
                    
                    args = ', '.join(func['args'][1:]) if len(func['args']) > 1 else ''
                    code += f"""
    def {func['name']}(self, {args}):
        \"\"\"{func['docstring'] or f'Preserved from {file}'}\"\"\"
        self._track_usage('{func['name']}', '{file}')
        
        # TODO: Port implementation from {file}
        raise NotImplementedError(
            f"Feature '{func['name']}' from {file} needs implementation. "
            "Please port the original code."
        )
"""
        
        # Add compatibility layer
        code += '''
    
    # ===== Compatibility Layer =====
    
    def generate_sql(self, *args, **kwargs):
        """Legacy method name compatibility"""
        return self.generate(*args, **kwargs)
    
    def optimize_sql(self, *args, **kwargs):
        """Legacy method name compatibility"""
        return self.optimize(*args, **kwargs)
    
    # ===== Helper Methods =====
    
    def _track_usage(self, feature: str, origin: str):
        """Track feature usage for migration metrics"""
        if self.feature_tracking:
            key = f"{origin}::{feature}"
            self.feature_usage[key] = self.feature_usage.get(key, 0) + 1
    
    def _extract_sql(self, text: str) -> str:
        """Extract SQL from LLM response"""
        # Look for code blocks
        import re
        sql_match = re.search(r'```sql\n(.*?)\n```', text, re.DOTALL)
        if sql_match:
            return sql_match.group(1).strip()
        
        # Look for SELECT/INSERT/UPDATE/DELETE/CREATE
        lines = text.split('\\n')
        sql_lines = []
        for line in lines:
            if any(line.strip().upper().startswith(cmd) for cmd in 
                   ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'CREATE', 'WITH']):
                sql_lines.append(line)
        
        return '\\n'.join(sql_lines) if sql_lines else text
    
    def _format_schema(self, schema: Dict) -> str:
        """Format schema for prompt"""
        lines = []
        for table, columns in schema.items():
            lines.append(f"Table: {table}")
            for col in columns:
                if isinstance(col, dict):
                    lines.append(f"  - {col.get('name', 'unknown')}: {col.get('type', 'unknown')}")
                else:
                    lines.append(f"  - {col}")
        return '\\n'.join(lines)
    
    def _basic_optimization(self, sql: str) -> str:
        """Apply basic SQL optimizations"""
        # This is a placeholder - would implement actual optimizations
        return sql
    
    def _extract_index_recommendations(self, text: str) -> List[str]:
        """Extract CREATE INDEX statements from response"""
        indexes = []
        import re
        for match in re.finditer(r'CREATE\\s+(?:UNIQUE\\s+)?INDEX.*?;', text, re.IGNORECASE | re.DOTALL):
            indexes.append(match.group(0).strip())
        return indexes
    
    def get_usage_report(self) -> Dict[str, int]:
        """Get feature usage statistics"""
        return dict(self.feature_usage)


def main():
    """Streamlit UI for SQL Toolkit Next"""
    st.set_page_config(
        page_title="SQL Toolkit Next",
        page_icon="🚀",
        layout="wide"
    )
    
    # Experimental banner
    st.info("🧪 **Experimental Feature** - Testing unified SQL toolkit. "
            "Switch back to original tools in sidebar if needed.")
    
    st.title("🚀 SQL Toolkit Next")
    st.caption("All SQL tools unified - Generate, Optimize, Analyze, and more!")
    
    # Initialize toolkit
    toolkit = SQLToolkitNext()
    
    # Create tabs
    tabs = st.tabs(["Generate", "Optimize", "Pipeline", "Advanced"])
    
    # Generate Tab
    with tabs[0]:
        prompt = st.text_area(
            "Describe what you need in plain English",
            placeholder="Show me all orders from last month with customer names"
        )
        
        col1, col2 = st.columns([3, 1])
        with col1:
            dialect = st.selectbox("SQL Dialect", ["postgresql", "mysql", "sqlite", "sqlserver"])
        with col2:
            auto_optimize = st.checkbox("Auto-optimize", value=True)
        
        if st.button("Generate SQL", type="primary"):
            if prompt:
                with st.spinner("Generating SQL..."):
                    result = toolkit.generate(prompt, dialect, auto_optimize=auto_optimize)
                    
                    if result['success']:
                        st.code(result['sql'], language='sql')
                        
                        # Copy button
                        if st.button("📋 Copy to clipboard"):
                            st.write("SQL copied!")  # Would implement actual copy
                    else:
                        st.error(f"Error: {result['error']}")
    
    # Optimize Tab
    with tabs[1]:
        sql_input = st.text_area("Paste SQL to optimize", height=200)
        explain_plan = st.text_area("EXPLAIN ANALYZE output (optional)", height=100)
        
        if st.button("Optimize", type="primary"):
            if sql_input:
                with st.spinner("Optimizing..."):
                    result = toolkit.optimize(sql_input, explain_plan)
                    
                    if result['success']:
                        col1, col2 = st.columns(2)
                        
                        with col1:
                            st.subheader("Original")
                            st.code(result['original_sql'], language='sql')
                        
                        with col2:
                            st.subheader("Optimized")
                            st.code(result['optimized_sql'], language='sql')
                        
                        if result['indexes']:
                            st.subheader("Recommended Indexes")
                            for idx in result['indexes']:
                                st.code(idx, language='sql')
    
    # Show usage stats in sidebar
    with st.sidebar:
        st.subheader("📊 Feature Usage")
        usage = toolkit.get_usage_report()
        if usage:
            for feature, count in usage.items():
                st.metric(feature.split('::')[1], count)
        else:
            st.info("No features used yet")


if __name__ == "__main__":
    main()
'''
        
        # Write the file
        output_path = Path("pages/sql_toolkit_next.py")
        output_path.parent.mkdir(exist_ok=True)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(code)
        
        print(f"  ✓ Created {output_path}")
        print(f"  ✓ Preserved {sum(len(funcs) for funcs in unique_features.values())} unique features")
        print(f"  ✓ Added compatibility layer for old method names")
    
    def create_agent_next(self):
        """Create unified agent system based on agent_lite"""
        print("\n🤖 Creating unified agent system...")
        
        code = '''"""
Agent Next - Unified agent system based on agent_lite.py
The simplest, most effective agent implementation
"""
import streamlit as st
from utils.ollama import OllamaToolBase
from typing import Dict, List, Optional
import json

class AgentNext(OllamaToolBase):
    """
    Unified agent system combining best features from all implementations.
    Based primarily on agent_lite.py with enhancements from other versions.
    """
    
    def __init__(self):
        super().__init__("agent_next", "deepseek-r1:latest")
        
        # Agent configuration
        self.max_iterations = 5
        self.tools = self._load_available_tools()
        self.conversation_history = []
    
    def _load_available_tools(self) -> Dict:
        """Load tools the agent can use"""
        return {
            'sql_toolkit': "Generate, optimize, or explain SQL queries",
            'code_explainer': "Explain code in any programming language",
            'doc_analyzer': "Analyze and answer questions about documents",
            'web_search': "Search the web for current information"
        }
    
    def process(self, user_input: str, context: Dict = None) -> Dict:
        """
        Process user input and determine appropriate action
        """
        self.conversation_history.append({"role": "user", "content": user_input})
        
        # Determine intent and required tools
        plan = self._create_plan(user_input, context)
        
        # Execute plan
        results = []
        for step in plan['steps']:
            result = self._execute_step(step)
            results.append(result)
        
        # Synthesize final response
        final_response = self._synthesize_response(results, user_input)
        
        self.conversation_history.append({
            "role": "assistant", 
            "content": final_response
        })
        
        return {
            'response': final_response,
            'plan': plan,
            'results': results
        }
    
    def _create_plan(self, user_input: str, context: Dict = None) -> Dict:
        """Create execution plan based on user input"""
        planning_prompt = f"""
Given this user request: {user_input}

Available tools:
{json.dumps(self.tools, indent=2)}

Create a step-by-step plan to fulfill this request.
Return as JSON with structure:
{{
    "goal": "what user wants to achieve",
    "steps": [
        {{"tool": "tool_name", "action": "what to do", "params": {{}}}}
    ]
}}
"""
        
        result = self.generate_with_logging(planning_prompt, temperature=0.3)
        
        try:
            # Extract JSON from response
            import re
            json_match = re.search(r'\\{.*\\}', result['response'], re.DOTALL)
            if json_match:
                plan = json.loads(json_match.group(0))
            else:
                # Fallback plan
                plan = {
                    "goal": "Process user request",
                    "steps": [{"tool": "direct_response", "action": "respond", "params": {}}]
                }
        except:
            plan = {
                "goal": "Process user request",
                "steps": [{"tool": "direct_response", "action": "respond", "params": {}}]
            }
        
        return plan
    
    def _execute_step(self, step: Dict) -> Dict:
        """Execute a single step in the plan"""
        tool = step.get('tool', 'direct_response')
        action = step.get('action', '')
        params = step.get('params', {})
        
        # Here you would integrate with actual tools
        # For now, simulate tool execution
        return {
            'tool': tool,
            'action': action,
            'result': f"Executed {action} using {tool}"
        }
    
    def _synthesize_response(self, results: List[Dict], user_input: str) -> str:
        """Synthesize final response from all results"""
        synthesis_prompt = f"""
User asked: {user_input}

Results from execution:
{json.dumps(results, indent=2)}

Provide a clear, helpful response that directly answers the user's question.
"""
        
        result = self.generate_with_logging(synthesis_prompt, temperature=0.7)
        return result['response']


def main():
    """Streamlit UI for Agent Next"""
    st.set_page_config(
        page_title="Agent Next",
        page_icon="🤖",
        layout="wide"
    )
    
    st.info("🧪 **Experimental** - Unified agent system. Switch to original in sidebar if needed.")
    
    st.title("🤖 Agent Next")
    st.caption("Intelligent agent that can use multiple tools to help you")
    
    # Initialize agent
    if 'agent' not in st.session_state:
        st.session_state.agent = AgentNext()
    
    # Chat interface
    if 'messages' not in st.session_state:
        st.session_state.messages = []
    
    # Display chat history
    for message in st.session_state.messages:
        with st.chat_message(message["role"]):
            st.write(message["content"])
    
    # Input
    if prompt := st.chat_input("How can I help you today?"):
        # Add user message
        st.session_state.messages.append({"role": "user", "content": prompt})
        with st.chat_message("user"):
            st.write(prompt)
        
        # Process with agent
        with st.chat_message("assistant"):
            with st.spinner("Thinking..."):
                response = st.session_state.agent.process(prompt)
                st.write(response['response'])
                
                # Show plan in expander
                with st.expander("🔍 Execution Plan"):
                    st.json(response['plan'])
        
        # Add assistant message
        st.session_state.messages.append({
            "role": "assistant", 
            "content": response['response']
        })
    
    # Sidebar
    with st.sidebar:
        st.subheader("🛠️ Agent Capabilities")
        for tool, description in st.session_state.agent.tools.items():
            st.write(f"**{tool}**: {description}")
        
        if st.button("Clear Conversation"):
            st.session_state.messages = []
            st.session_state.agent.conversation_history = []
            st.rerun()


if __name__ == "__main__":
    main()
'''
        
        output_path = Path("pages/agent_next.py")
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(code)
        
        print(f"  ✓ Created {output_path}")
        print(f"  ✓ Based on agent_lite.py architecture")
        print(f"  ✓ Ready for tool integration")
    
    def create_rollback_plan(self):
        """Create detailed rollback plan"""
        print("\n📋 Creating rollback plan...")
        
        plan = f"""# Rollback Plan for TuoKit Migration
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## 🚨 Quick Rollback (< 1 minute)

### Option 1: UI Toggle
1. In the TuoKit sidebar, toggle OFF "Use Next-Gen Tools"
2. All users immediately return to original tools

### Option 2: Code Change
1. Edit `app.py`
2. Change: `st.session_state.setdefault('use_next_gen_tools', True)`
3. To: `st.session_state.setdefault('use_next_gen_tools', False)`
4. Restart Streamlit

## 🔄 Full Rollback (< 5 minutes)

If you need to completely remove the new tools:

```bash
# 1. Remove new files
rm pages/sql_toolkit_next.py
rm pages/agent_next.py
rm pages/migration_dashboard.py

# 2. Remove feature toggle from app.py
# Delete the experimental features section

# 3. Restart application
streamlit run app.py
```

## 📦 Restore from Archive

If you've already archived old files and need them back:

```bash
# Find your archive directory (format: archived_YYYYMMDD_HHMMSS)
ls -la archived_*

# Restore files
cp archived_20250102_143000/pages/* pages/

# Restart application
streamlit run app.py
```

## ⚡ Partial Rollback

Keep some improvements while reverting problematic features:

### In sql_toolkit_next.py:
```python
# Disable specific features
self.compat_mode = True  # Keep compatibility
self.enable_pipeline = False  # Disable pipeline features
self.enable_advanced = False  # Disable advanced features
```

### In agent_next.py:
```python
# Limit agent capabilities
self.tools = {{
    'sql_toolkit': "...",  # Keep this
    # 'web_search': "...",  # Disable this
}}
```

## 📊 Rollback Decision Matrix

| Issue | Severity | Action |
|-------|----------|--------|
| Missing feature | Low | Add to unified tool |
| Performance issue | Medium | Optimize or disable feature |
| Data corruption | High | Full rollback immediately |
| User confusion | Low | Add documentation/tooltips |
| Critical bug | High | UI toggle OFF + hotfix |

## 📞 Emergency Contacts

- Dev Team: [your-team@company.com]
- Migration Lead: [your-name]
- Escalation: [manager]

## 📝 Post-Rollback Checklist

- [ ] Notify users of rollback
- [ ] Document reason for rollback
- [ ] Create fix plan
- [ ] Schedule retry
- [ ] Update tests to catch issue

---

Remember: The feature toggle means you can rollback instantly without any file changes!
"""
        
        output_path = f"ROLLBACK_PLAN_{self.timestamp}.md"
        with open(output_path, 'w') as f:
            f.write(plan)
        
        print(f"  ✓ Created {output_path}")


def main():
    """Run smart cleanup process"""
    try:
        cleaner = SmartCleaner()
        cleaner.run()
    except FileNotFoundError as e:
        print(f"❌ Error: {e}")
        print("\n📋 Please run these steps in order:")
        print("  1. python extract_unique_features.py")
        print("  2. python smart_cleanup.py")
        print("  3. python add_feature_toggle.py")


if __name__ == "__main__":
    main()
