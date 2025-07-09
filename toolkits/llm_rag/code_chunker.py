"""
Code-aware chunking for better RAG performance
Preserves code structure and context
"""
import ast
from typing import List, Dict, Optional, Tuple
import re
from pathlib import Path
import textwrap

class CodeAwareChunker:
    """Smart code chunking that preserves semantic boundaries"""
    
    def __init__(self, max_chunk_size: int = 1500, chunk_overlap: int = 200):
        self.max_chunk_size = max_chunk_size
        self.chunk_overlap = chunk_overlap
        
    def chunk_file(self, file_path: str) -> List[Dict]:
        """Process a file with appropriate chunking strategy"""
        path = Path(file_path)
        ext = path.suffix.lower()
        
        # Read file content
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
        except:
            return []
        
        # Choose chunking strategy based on file type
        if ext == '.py':
            return self.chunk_python_code(content, file_path)
        elif ext in ['.js', '.ts', '.jsx', '.tsx']:
            return self.chunk_javascript_code(content, file_path)
        elif ext == '.md':
            return self.chunk_markdown(content, file_path)
        elif ext in ['.yaml', '.yml']:
            return self.chunk_yaml(content, file_path)
        else:
            return self.chunk_generic_text(content, file_path)
    
    def chunk_python_code(self, code: str, file_path: str) -> List[Dict]:
        """Extract semantic chunks from Python code"""
        chunks = []
        
        try:
            tree = ast.parse(code)
            
            # First, extract module-level docstring and imports
            module_header = self._extract_python_header(code, tree)
            if module_header:
                chunks.append({
                    'content': module_header,
                    'type': 'module_header',
                    'name': 'imports_and_constants',
                    'file_path': file_path,
                    'language': 'python',
                    'line_start': 1
                })
            
            # Extract classes with their methods
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    class_chunk = self._extract_class(code, node)
                    if class_chunk:
                        chunks.append({
                            'content': class_chunk,
                            'type': 'class',
                            'name': node.name,
                            'file_path': file_path,
                            'language': 'python',
                            'line_start': node.lineno
                        })
                
                # Extract standalone functions
                elif isinstance(node, ast.FunctionDef) and self._is_top_level_function(node, tree):
                    func_chunk = self._extract_function(code, node)
                    if func_chunk:
                        chunks.append({
                            'content': func_chunk,
                            'type': 'function',
                            'name': node.name,
                            'file_path': file_path,
                            'language': 'python',
                            'line_start': node.lineno
                        })
                        
        except SyntaxError:
            # Fallback for files with syntax errors
            return self._fallback_chunk(code, file_path, 'python')
        
        return chunks
    
    def _extract_python_header(self, code: str, tree: ast.AST) -> Optional[str]:
        """Extract imports, constants, and module docstring"""
        lines = code.split('\n')
        header_lines = []
        
        # Find the last import or top-level assignment
        last_header_line = 0
        for node in ast.walk(tree):
            if isinstance(node, (ast.Import, ast.ImportFrom)):
                last_header_line = max(last_header_line, node.end_lineno or node.lineno)
            elif isinstance(node, ast.Assign) and hasattr(node, 'lineno'):
                # Check if it's a top-level constant
                if node.col_offset == 0:
                    last_header_line = max(last_header_line, node.end_lineno or node.lineno)
        
        if last_header_line > 0:
            header = '\n'.join(lines[:last_header_line])
            if len(header.strip()) > 50:  # Only include if substantial
                return header
        
        return None
    
    def _extract_class(self, code: str, node: ast.ClassDef) -> Optional[str]:
        """Extract a complete class definition"""
        try:
            class_source = ast.get_source_segment(code, node)
            if not class_source:
                return None
                
            # If class is too large, extract it in parts
            if len(class_source) > self.max_chunk_size:
                # Extract class header and docstring
                lines = class_source.split('\n')
                class_header = []
                
                # Find where methods start
                method_start = len(lines)
                for i, line in enumerate(lines):
                    if line.strip().startswith('def ') and i > 0:
                        method_start = i
                        break
                
                # Return class definition with docstring
                header_content = '\n'.join(lines[:method_start])
                if len(header_content.strip()) > 50:
                    return header_content + '\n    # ... methods follow ...'
            
            return class_source
            
        except:
            return None
    
    def _extract_function(self, code: str, node: ast.FunctionDef) -> Optional[str]:
        """Extract a complete function definition"""
        try:
            func_source = ast.get_source_segment(code, node)
            return func_source
        except:
            return None
    
    def _is_top_level_function(self, node: ast.FunctionDef, tree: ast.AST) -> bool:
        """Check if function is at module level (not inside a class)"""
        for parent in ast.walk(tree):
            if isinstance(parent, ast.ClassDef):
                for child in ast.walk(parent):
                    if child is node:
                        return False
        return True
    
    def chunk_javascript_code(self, code: str, file_path: str) -> List[Dict]:
        """Chunk JavaScript/TypeScript code"""
        chunks = []
        
        # Extract imports and constants
        import_pattern = r'^(import\s+.*?;|const\s+\w+\s*=.*?;|export\s+.*?;)$'
        imports = []
        lines = code.split('\n')
        
        for i, line in enumerate(lines):
            if re.match(import_pattern, line.strip()):
                imports.append(line)
            elif line.strip() and not line.strip().startswith('//'):
                # Stop at first non-import line
                break
        
        if imports:
            chunks.append({
                'content': '\n'.join(imports),
                'type': 'imports',
                'name': 'module_imports',
                'file_path': file_path,
                'language': 'javascript',
                'line_start': 1
            })
        
        # Extract functions and classes
        function_pattern = r'(async\s+)?function\s+(\w+)\s*\([^)]*\)\s*\{|(\w+)\s*[:=]\s*(async\s*)?\([^)]*\)\s*=>\s*\{|class\s+(\w+)'
        
        current_chunk = []
        current_name = None
        brace_count = 0
        in_function = False
        
        for i, line in enumerate(lines[len(imports):], start=len(imports)):
            match = re.search(function_pattern, line)
            if match and brace_count == 0:
                # Save previous chunk if exists
                if current_chunk and current_name:
                    chunks.append({
                        'content': '\n'.join(current_chunk),
                        'type': 'function',
                        'name': current_name,
                        'file_path': file_path,
                        'language': 'javascript',
                        'line_start': i - len(current_chunk) + 1
                    })
                
                # Start new chunk
                current_chunk = [line]
                current_name = match.group(2) or match.group(3) or match.group(5)
                in_function = True
                brace_count = line.count('{') - line.count('}')
            elif in_function:
                current_chunk.append(line)
                brace_count += line.count('{') - line.count('}')
                
                if brace_count == 0:
                    # Function complete
                    chunks.append({
                        'content': '\n'.join(current_chunk),
                        'type': 'function',
                        'name': current_name,
                        'file_path': file_path,
                        'language': 'javascript',
                        'line_start': i - len(current_chunk) + 1
                    })
                    current_chunk = []
                    current_name = None
                    in_function = False
        
        return chunks if chunks else self._fallback_chunk(code, file_path, 'javascript')
    
    def chunk_markdown(self, content: str, file_path: str) -> List[Dict]:
        """Chunk Markdown by headers"""
        chunks = []
        lines = content.split('\n')
        
        current_section = []
        current_header = None
        current_level = 0
        
        for i, line in enumerate(lines):
            # Check if line is a header
            header_match = re.match(r'^(#{1,6})\s+(.+)$', line)
            
            if header_match:
                # Save previous section if exists
                if current_section:
                    chunks.append({
                        'content': '\n'.join(current_section),
                        'type': 'section',
                        'name': current_header or 'Introduction',
                        'file_path': file_path,
                        'language': 'markdown',
                        'level': current_level,
                        'line_start': i - len(current_section) + 1
                    })
                
                # Start new section
                current_level = len(header_match.group(1))
                current_header = header_match.group(2)
                current_section = [line]
            else:
                current_section.append(line)
        
        # Don't forget the last section
        if current_section:
            chunks.append({
                'content': '\n'.join(current_section),
                'type': 'section',
                'name': current_header or 'Content',
                'file_path': file_path,
                'language': 'markdown',
                'level': current_level,
                'line_start': len(lines) - len(current_section) + 1
            })
        
        return chunks
    
    def chunk_yaml(self, content: str, file_path: str) -> List[Dict]:
        """Chunk YAML by top-level keys"""
        chunks = []
        lines = content.split('\n')
        
        current_section = []
        current_key = None
        
        for i, line in enumerate(lines):
            # Check if line is a top-level key
            if line and not line.startswith(' ') and not line.startswith('\t') and ':' in line:
                # Save previous section
                if current_section and current_key:
                    chunks.append({
                        'content': '\n'.join(current_section),
                        'type': 'config_section',
                        'name': current_key,
                        'file_path': file_path,
                        'language': 'yaml',
                        'line_start': i - len(current_section) + 1
                    })
                
                # Start new section
                current_key = line.split(':')[0].strip()
                current_section = [line]
            else:
                current_section.append(line)
        
        # Last section
        if current_section and current_key:
            chunks.append({
                'content': '\n'.join(current_section),
                'type': 'config_section', 
                'name': current_key,
                'file_path': file_path,
                'language': 'yaml',
                'line_start': len(lines) - len(current_section) + 1
            })
        
        return chunks
    
    def chunk_generic_text(self, content: str, file_path: str) -> List[Dict]:
        """Generic text chunking with overlap"""
        return self._fallback_chunk(content, file_path, 'text')
    
    def _fallback_chunk(self, content: str, file_path: str, language: str) -> List[Dict]:
        """Fallback chunking for when semantic chunking fails"""
        chunks = []
        lines = content.split('\n')
        
        current_chunk = []
        current_size = 0
        
        for i, line in enumerate(lines):
            line_size = len(line) + 1  # +1 for newline
            
            if current_size + line_size > self.max_chunk_size and current_chunk:
                # Save current chunk
                chunks.append({
                    'content': '\n'.join(current_chunk),
                    'type': 'text_chunk',
                    'name': f'chunk_{len(chunks) + 1}',
                    'file_path': file_path,
                    'language': language,
                    'line_start': i - len(current_chunk) + 1
                })
                
                # Start new chunk with overlap
                overlap_lines = []
                overlap_size = 0
                for j in range(len(current_chunk) - 1, -1, -1):
                    if overlap_size + len(current_chunk[j]) < self.chunk_overlap:
                        overlap_lines.insert(0, current_chunk[j])
                        overlap_size += len(current_chunk[j]) + 1
                    else:
                        break
                
                current_chunk = overlap_lines + [line]
                current_size = overlap_size + line_size
            else:
                current_chunk.append(line)
                current_size += line_size
        
        # Last chunk
        if current_chunk:
            chunks.append({
                'content': '\n'.join(current_chunk),
                'type': 'text_chunk',
                'name': f'chunk_{len(chunks) + 1}',
                'file_path': file_path,
                'language': language,
                'line_start': len(lines) - len(current_chunk) + 1
            })
        
        return chunks


# Example usage
if __name__ == "__main__":
    chunker = CodeAwareChunker()
    
    # Test with a Python file
    chunks = chunker.chunk_file("app.py")
    for chunk in chunks:
        print(f"\n--- {chunk['type']}: {chunk['name']} ---")
        print(f"Lines {chunk['line_start']} - ...")
        print(chunk['content'][:200] + "..." if len(chunk['content']) > 200 else chunk['content'])