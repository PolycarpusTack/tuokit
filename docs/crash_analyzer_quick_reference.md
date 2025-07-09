# 🚨 Crash Analyzer Pro - Quick Reference

## 📁 File Upload
- **Max Size**: 5MB
- **Formats**: .txt, .log, .dmp, .wcr, .crash, .error

## 🔍 Analysis Methods

### ⚡ Basic Analysis (5-15 seconds)
- Uses smart extraction
- Returns JSON with root cause, severity, fixes
- Best for: Quick diagnosis

### 🕵️ Expert Diagnostics (30-60 seconds)
- Comprehensive markdown report
- Includes ELI5 explanation
- Best for: Detailed investigation

### 📊 Smart Sampling (10-20 seconds)
- Analyzes strategic sections
- Samples around errors and stack traces
- Best for: Large files when speed matters

### 🧩 Full File Analysis (varies)
- Processes entire file in chunks
- Shows progress with abort option
- Best for: Complete analysis of large files
- Time estimate:
  - 1MB: ~5 minutes
  - 3MB: ~15 minutes  
  - 5MB: ~25 minutes

## 🎯 Pattern Detection
Automatically detects 8 common patterns:
- 🔴 **Critical**: OutOfMemoryError
- 🟠 **High**: NullPointerException, StackOverflow, DeadlockDetected
- 🟡 **Medium**: DatabaseTimeout, PermissionDenied, NetworkError
- 🟢 **Low**: FileNotFound

## 💾 Features
- **Save Analysis**: Store in knowledge base with validator name
- **Export Report**: Download as markdown file
- **Similar Crashes**: Find related previous analyses
- **Statistics Dashboard**: View crash trends and metrics

## 🔧 Tips
1. Start with Basic Analysis for quick results
2. Use Expert Diagnostics for actionable insights
3. Try Smart Sampling before Full Analysis on large files
4. Check detected patterns for immediate fixes
5. Save validated analyses to build knowledge base

## 📊 Performance
- Pattern detection runs automatically
- Progress bars show chunk processing
- Abort button available for long operations
- Time estimates shown before processing

---
*TuoKit Crash Analyzer - Building fast, building smart, building exactly what's needed*
