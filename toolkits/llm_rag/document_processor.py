"""
Document Processor - Handles multiple document types
Integrates with llm-search parsers but stores in Postgres
"""
import os
from pathlib import Path
from typing import List, Dict, Generator
import hashlib
from datetime import datetime
import logging

# Document parsing libraries
# from llmsearch.parsers import MarkdownParser, PDFParser  # Not needed - using custom parsers
from sentence_transformers import SentenceTransformer
import pypdf
from docx import Document
import json

logger = logging.getLogger(__name__)

class DocumentProcessor:
    """Process various document types into chunks with embeddings"""
    
    def __init__(self, embedding_model: str = "all-MiniLM-L6-v2"):
        self.embedder = SentenceTransformer(embedding_model)
        self.chunk_size = 1000
        self.chunk_overlap = 200
        
        # Initialize parsers
        self.parsers = {
            '.md': self._parse_markdown,
            '.py': self._parse_code,
            '.js': self._parse_code,
            '.pdf': self._parse_pdf,
            '.docx': self._parse_docx,
            '.txt': self._parse_text,
            '.json': self._parse_json,
            '.yaml': self._parse_yaml,
            '.yml': self._parse_yaml
        }
    
    def process_file(self, file_path: str) -> List[Dict]:
        """Process a single file into chunks with embeddings"""
        path = Path(file_path)
        
        if not path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
        
        ext = path.suffix.lower()
        if ext not in self.parsers:
            logger.warning(f"Unsupported file type: {ext}")
            return []
        
        # Get file hash for deduplication
        file_hash = self._get_file_hash(file_path)
        
        # Parse file into chunks
        chunks = list(self.parsers[ext](file_path))
        
        # Add embeddings and metadata
        processed_chunks = []
        for i, chunk_text in enumerate(chunks):
            embedding = self.embedder.encode(chunk_text)
            
            processed_chunks.append({
                'content': chunk_text,
                'embedding': embedding,
                'metadata': {
                    'file_hash': file_hash,
                    'file_name': path.name,
                    'file_type': ext,
                    'chunk_size': len(chunk_text),
                    'processed_at': datetime.now().isoformat()
                },
                'source_type': self._get_source_type(ext),
                'source_path': str(path),
                'chunk_index': i
            })
        
        logger.info(f"Processed {len(processed_chunks)} chunks from {file_path}")
        return processed_chunks
    
    def _get_file_hash(self, file_path: str) -> str:
        """Get file hash for deduplication"""
        with open(file_path, 'rb') as f:
            return hashlib.sha256(f.read()).hexdigest()
    
    def _get_source_type(self, ext: str) -> str:
        """Map extension to source type"""
        mapping = {
            '.py': 'code',
            '.js': 'code',
            '.md': 'documentation',
            '.pdf': 'release_notes',
            '.docx': 'documentation',
            '.txt': 'documentation',
            '.json': 'config',
            '.yaml': 'config',
            '.yml': 'config'
        }
        return mapping.get(ext, 'unknown')
    
    def _chunk_text(self, text: str) -> List[str]:
        """Simple text chunking with overlap"""
        chunks = []
        start = 0
        
        while start < len(text):
            end = start + self.chunk_size
            chunk = text[start:end]
            
            # Try to break at sentence boundary
            if end < len(text):
                last_period = chunk.rfind('.')
                if last_period > self.chunk_size * 0.8:
                    end = start + last_period + 1
                    chunk = text[start:end]
            
            chunks.append(chunk)
            start = end - self.chunk_overlap
        
        return chunks
    
    def _parse_markdown(self, file_path: str) -> Generator[str, None, None]:
        """Parse markdown with heading awareness"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Split by headers
        import re
        sections = re.split(r'\n#{1,6}\s+', content)
        
        for section in sections:
            if section.strip():
                yield section.strip()
    
    def _parse_code(self, file_path: str) -> Generator[str, None, None]:
        """Parse code files preserving structure"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Split by functions/classes
        import ast
        try:
            tree = ast.parse(content)
            for node in ast.walk(tree):
                if isinstance(node, (ast.FunctionDef, ast.ClassDef)):
                    start_line = node.lineno - 1
                    end_line = node.end_lineno
                    chunk = '\n'.join(content.split('\n')[start_line:end_line])
                    yield chunk
        except:
            # Fallback to simple chunking
            yield from self._chunk_text(content)
    
    def _parse_pdf(self, file_path: str) -> Generator[str, None, None]:
        """Parse PDF documents"""
        try:
            with open(file_path, 'rb') as f:
                pdf_reader = pypdf.PdfReader(f)
                for page_num, page in enumerate(pdf_reader.pages):
                    text = page.extract_text()
                    if text.strip():
                        # Add page number to help with citations
                        yield f"[Page {page_num + 1}]\n{text}"
        except Exception as e:
            logger.error(f"Error parsing PDF {file_path}: {e}")
    
    def _parse_docx(self, file_path: str) -> Generator[str, None, None]:
        """Parse DOCX with table support"""
        doc = Document(file_path)
        
        current_chunk = []
        current_size = 0
        
        for element in doc.element.body:
            if element.tag.endswith('p'):
                # Paragraph
                para = element.text
                if para.strip():
                    current_chunk.append(para)
                    current_size += len(para)
            
            elif element.tag.endswith('tbl'):
                # Table - convert to markdown format
                table_text = self._extract_table_text(element)
                current_chunk.append(table_text)
                current_size += len(table_text)
            
            if current_size > self.chunk_size:
                yield '\n\n'.join(current_chunk)
                current_chunk = []
                current_size = 0
        
        if current_chunk:
            yield '\n\n'.join(current_chunk)
    
    def _extract_table_text(self, table_element) -> str:
        """Extract table as markdown"""
        # Simple table extraction
        rows = []
        for row in table_element.xpath('.//w:tr', namespaces={'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}):
            cells = []
            for cell in row.xpath('.//w:tc', namespaces={'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}):
                cell_text = ''.join(cell.itertext()).strip()
                cells.append(cell_text)
            rows.append(' | '.join(cells))
        
        return '\n'.join(rows)
    
    def _parse_text(self, file_path: str) -> Generator[str, None, None]:
        """Parse plain text files"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        yield from self._chunk_text(content)
    
    def _parse_json(self, file_path: str) -> Generator[str, None, None]:
        """Parse JSON files"""
        with open(file_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        # Convert to readable format
        formatted = json.dumps(data, indent=2)
        yield from self._chunk_text(formatted)
    
    def _parse_yaml(self, file_path: str) -> Generator[str, None, None]:
        """Parse YAML files"""
        import yaml
        with open(file_path, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f)
        
        # Convert to readable format
        formatted = yaml.dump(data, default_flow_style=False)
        yield from self._chunk_text(formatted)
