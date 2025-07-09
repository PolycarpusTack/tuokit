"""
Fix Database Operations Script
Updates all database operations to use the proper connection pattern
"""

import re

def fix_database_operations(file_path):
    """Fix all database operations in a file"""
    
    print(f"Fixing database operations in {file_path}...")
    
    # Read the file
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Pattern replacements
    replacements = [
        # Fix execute patterns
        (r'self\.db\.connection\.execute\(', 'cur.execute('),
        (r'result = self\.db\.connection\.execute\(', 'cur.execute('),
        (r'self\.db\.connection\.commit\(\)', '# Commit handled by context manager'),
        
        # Fix the create tables method
        (r'def _create_analytics_tables\(self\):\s*"""Create tables for storing analytics results"""\s*try:',
         '''def _create_analytics_tables(self):
        """Create tables for storing analytics results"""
        if not self.db or not self.db.connected:
            return
            
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:'''),
    ]
    
    # Apply replacements
    for pattern, replacement in replacements:
        content = re.sub(pattern, replacement, content)
    
    # Find all execute blocks and wrap them properly
    # This is a simplified approach - for production, use proper AST parsing
    
    # Replace individual execute statements with proper context
    lines = content.split('\n')
    new_lines = []
    in_method = False
    method_indent = 0
    
    for i, line in enumerate(lines):
        # Detect method definitions
        if re.match(r'^\s*def\s+\w+\(self.*\):', line):
            in_method = True
            method_indent = len(line) - len(line.lstrip())
        
        # Check if we have a database operation that needs wrapping
        if 'cur.execute(' in line and in_method:
            # Check if we're already in a cursor context
            if i > 0 and 'with conn.cursor() as cur:' not in lines[i-1]:
                # Need to wrap this in context managers
                indent = len(line) - len(line.lstrip())
                base_indent = ' ' * (indent - 4)
                
                # Look back to see if we need to add the connection context too
                need_conn_context = True
                for j in range(max(0, i-10), i):
                    if 'with self.db.get_connection() as conn:' in lines[j]:
                        need_conn_context = False
                        break
                
                if need_conn_context:
                    new_lines.append(base_indent + 'with self.db.get_connection() as conn:')
                    new_lines.append(base_indent + '    with conn.cursor() as cur:')
                    new_lines.append(' ' * 8 + line.lstrip())
                else:
                    new_lines.append(base_indent + 'with conn.cursor() as cur:')
                    new_lines.append(' ' * 4 + line.lstrip())
            else:
                new_lines.append(line)
        else:
            new_lines.append(line)
    
    # Join lines back
    content = '\n'.join(new_lines)
    
    # Write back
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"✅ Fixed database operations in {file_path}")

# Apply fixes
if __name__ == "__main__":
    # Fix data_analysis_enhanced.py
    try:
        fix_database_operations('C:/Projects/Tuokit/data_analysis_enhanced.py')
    except Exception as e:
        print(f"Error: {e}")
    
    print("\nManual fix still needed for data_analysis_enhanced.py")
    print("The file has complex database operations that need careful updating.")
    print("For now, you can use the original DataAnalysisAgent from agent_hub_enhancements.py")
