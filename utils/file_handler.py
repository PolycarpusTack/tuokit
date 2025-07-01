"""
TuoKit File Handler Utility
Practical file extraction with comprehensive error handling
Following TuoKit Architect principles: minimal, practical, safe
"""

import streamlit as st
from pathlib import Path
import tempfile
import re
from typing import Optional, Union

def extract_text(uploaded_file) -> Optional[str]:
    """
    Extract text from uploaded files with robust error handling
    Supports: TXT, PDF, DOCX formats
    """
    if not uploaded_file:
        return None
        
    filename = uploaded_file.name.lower()
    
    try:
        # Text file processing
        if filename.endswith('.txt'):
            return uploaded_file.read().decode('utf-8', errors='ignore')
        
        # PDF processing
        elif filename.endswith('.pdf'):
            return extract_pdf(uploaded_file)
            
        # DOCX processing  
        elif filename.endswith('.docx'):
            return extract_docx(uploaded_file)
            
        else:
            st.error(f"Unsupported file format: {Path(filename).suffix}")
            return None
            
    except Exception as e:
        st.error(f"Error extracting text from {filename}: {str(e)}")
        return None

def extract_pdf(uploaded_file) -> Optional[str]:
    """Extract text from PDF using available libraries"""
    try:
        # Try PyMuPDF first (faster and more accurate)
        import fitz
        doc = fitz.open(stream=uploaded_file.read(), filetype="pdf")
        text = ""
        for page_num, page in enumerate(doc):
            text += f"\n--- Page {page_num + 1} ---\n"
            text += page.get_text()
        doc.close()
        return text.strip()
        
    except ImportError:
        # Fallback to PyPDF2
        try:
            from PyPDF2 import PdfReader
            uploaded_file.seek(0)  # Reset file pointer
            reader = PdfReader(uploaded_file)
            text = ""
            for page_num, page in enumerate(reader.pages):
                text += f"\n--- Page {page_num + 1} ---\n"
                text += page.extract_text()
            return text.strip()
            
        except Exception as e:
            st.error(f"PDF extraction failed: {str(e)}")
            return None

def extract_docx(uploaded_file) -> Optional[str]:
    """Extract text from DOCX files"""
    try:
        import zipfile
        import xml.etree.ElementTree as ET
        
        # Save uploaded file to temp location
        with tempfile.NamedTemporaryFile(delete=False, suffix='.docx') as tmp:
            tmp.write(uploaded_file.read())
            tmp_path = tmp.name
            
        text = ""
        with zipfile.ZipFile(tmp_path, 'r') as docx:
            # Extract main document content
            xml_content = docx.read('word/document.xml')
            tree = ET.fromstring(xml_content)
            
            # Extract all text elements
            for elem in tree.iter():
                if elem.text:
                    text += elem.text + " "
                    
        # Cleanup
        Path(tmp_path).unlink()
        return text.strip()
        
    except Exception as e:
        st.error(f"DOCX extraction failed: {str(e)}")
        st.info("Consider installing python-docx for better DOCX support")
        return None

def extract_text_from_url(url: str) -> Optional[str]:
    """
    Extract clean text from URL with basic web scraping
    Minimal implementation - no external dependencies beyond requests
    """
    try:
        import requests
        from html.parser import HTMLParser
        
        # Basic HTML text extractor
        class TextExtractor(HTMLParser):
            def __init__(self):
                super().__init__()
                self.text_parts = []
                self.skip_tags = {'script', 'style', 'meta', 'link'}
                self.current_tag = None
                
            def handle_starttag(self, tag, attrs):
                self.current_tag = tag.lower()
                
            def handle_data(self, data):
                if self.current_tag not in self.skip_tags:
                    text = data.strip()
                    if text:
                        self.text_parts.append(text)
                        
            def get_text(self):
                return ' '.join(self.text_parts)
        
        # Fetch content
        response = requests.get(url, timeout=10, headers={
            'User-Agent': 'Mozilla/5.0 (TuoKit Educational Tool)'
        })
        response.raise_for_status()
        
        # Extract text
        extractor = TextExtractor()
        extractor.feed(response.text)
        text = extractor.get_text()
        
        # Basic cleanup
        text = re.sub(r'\s+', ' ', text)  # Normalize whitespace
        text = text[:10000]  # Limit to first 10k chars for performance
        
        return text
        
    except ImportError:
        st.error("Please install requests: pip install requests")
        return None
    except Exception as e:
        st.error(f"URL extraction failed: {str(e)}")
        return None


def validate_file_size(uploaded_file, max_size_mb: int = 10) -> bool:
    """Validate uploaded file size"""
    if uploaded_file.size > max_size_mb * 1024 * 1024:
        st.error(f"File too large. Maximum size: {max_size_mb}MB")
        return False
    return True


# TODO: Add support for more formats (RTF, ODT, etc.)
# TODO: Implement OCR for scanned PDFs using pytesseract
# TODO: Add language detection for better text processing
