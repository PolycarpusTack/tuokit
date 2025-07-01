#!/usr/bin/env python3
"""
TuoKit PDF Processing Test
Tests if PDF libraries are properly installed
"""

import sys

def test_pdf_libraries():
    """Test PDF processing libraries"""
    print("🔍 Testing PDF processing libraries...")
    print("-" * 50)
    
    # Test PyPDF2
    try:
        import PyPDF2
        print("✅ PyPDF2 imported successfully")
        print(f"   Version: {PyPDF2.__version__ if hasattr(PyPDF2, '__version__') else 'Unknown'}")
    except ImportError as e:
        print("❌ PyPDF2 not found. Please run: pip install pypdf2")
        print(f"   Error: {e}")
    
    print()
    
    # Test PyMuPDF (fitz)
    try:
        import fitz
        print("✅ PyMuPDF (fitz) imported successfully")
        print(f"   Version: {fitz.version}")
    except ImportError as e:
        print("❌ PyMuPDF not found. Please run: pip install pymupdf")
        print(f"   Error: {e}")
    
    print("-" * 50)
    
    # Test with a simple PDF if both libraries are available
    try:
        import PyPDF2
        import fitz
        print("\n✅ Both PDF libraries are available!")
        print("   Document Tools should work correctly.")
        return True
    except ImportError:
        print("\n⚠️  At least one PDF library is missing.")
        print("   Document Tools may have limited functionality.")
        return False

if __name__ == "__main__":
    print("📄 TuoKit - PDF Library Test")
    print("=" * 50)
    
    success = test_pdf_libraries()
    
    print("\nRecommended installation:")
    print("pip install pypdf2 pymupdf")
    
    sys.exit(0 if success else 1)