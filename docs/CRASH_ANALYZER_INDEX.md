# 🚨 Crash Analyzer Documentation Index

## Overview
The TuoKit Crash Analyzer Pro is an AI-powered tool for analyzing crash dumps, error logs, and diagnostic files up to 5MB in size.

## 📚 Documentation Files

### Quick Start
- **[CRASH_ANALYZER_READY.md](CRASH_ANALYZER_READY.md)** - Start here! Current status and quick start guide
- **[crash_analyzer_quick_reference.md](crash_analyzer_quick_reference.md)** - Quick reference card for all features

### Implementation Details
- **[CRASH_ANALYZER_ALL_IMPROVEMENTS.md](CRASH_ANALYZER_ALL_IMPROVEMENTS.md)** - Complete list of all enhancements
- **[CRASH_ANALYZER_5MB_COMPLETE.md](CRASH_ANALYZER_5MB_COMPLETE.md)** - Details on 5MB file support
- **[crash_analyzer_enhancement_plan.md](crash_analyzer_enhancement_plan.md)** - Original implementation plan

### Bug Fixes
- **[crash_analyzer_expander_fix.md](crash_analyzer_expander_fix.md)** - Fix for nested expander error
- **[crash_analyzer_format_fix.md](crash_analyzer_format_fix.md)** - Fix for format parameter error
- **[CRASH_ANALYZER_FIXES_COMPLETE.md](CRASH_ANALYZER_FIXES_COMPLETE.md)** - Summary of all fixes

### Technical Documentation
- **[crash_analyzer_enhancement_review.md](../artifacts/crash-analyzer-review)** - Detailed technical review
- **[crash_analyzer_comparison.py](crash_analyzer_comparison.py)** - Feature comparison tool

## 🧪 Test Files
Located in `/test_files/`:
- `test_crash.log` - Sample crash dump for testing
- `test_format_fix.py` - Test script for JSON format
- `test_5mb_final.py` - Test script for 5MB support

## 🚀 Features

### Core Capabilities
- **File Support**: Up to 5MB files
- **Formats**: .txt, .log, .dmp, .wcr, .crash, .error
- **Pattern Detection**: 8 common error patterns
- **Analysis Methods**: 4 different approaches

### Analysis Methods
1. **⚡ Basic Analysis** - Fast JSON-based (5-15s)
2. **🕵️ Expert Diagnostics** - Comprehensive report (30-60s)
3. **📊 Smart Sampling** - Strategic extraction (10-20s)
4. **🧩 Full File Analysis** - Complete processing (varies by size)

### Key Features
- Real-time progress tracking
- Abort capability for long operations
- Pattern matching with severity levels
- Similar crash detection
- Knowledge base integration
- Export to markdown

## 💡 Best Practices

1. **Start with Basic Analysis** for quick diagnosis
2. **Use Pattern Detection** for immediate insights
3. **Try Smart Sampling** before full analysis on large files
4. **Save Validated Results** to build knowledge base
5. **Export Reports** for documentation

## 🔧 Configuration

Edit `CRASH_ANALYZER_CONFIG` in `pages/crash_analyzer.py`:
```python
{
    "max_file_size_mb": 5,
    "chunk_size": 8000,
    "chunk_overlap": 400,
    "max_chunks": 700,
    "pattern_match_context_chars": 100
}
```

---
*TuoKit Crash Analyzer - Building fast, building smart, building exactly what's needed*
