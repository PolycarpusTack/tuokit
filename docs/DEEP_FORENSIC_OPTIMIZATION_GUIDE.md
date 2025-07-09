# Deep Forensic Analysis - Optimization Guide

## Issue: Long Analysis Times and Minimal Results

You experienced a Deep Forensic analysis that took 9483.66 seconds (2.6 hours) but returned minimal results. This guide explains the issue and provides solutions.

## Why This Happens

1. **Timeout Issues**: Individual chunks may timeout during analysis
2. **Memory Pressure**: Large files can cause memory issues on local hardware
3. **Ollama Performance**: Local AI models can be slow on consumer hardware
4. **Result Truncation**: Very long analyses may lose intermediate results

## Immediate Solutions

### 1. Use Strategic Sampling Instead
For large files, Strategic Sampling is often more appropriate:
- Analyzes key sections instead of entire file
- Takes 1-2 minutes instead of hours
- Provides statistical confidence metrics
- Still identifies critical issues

### 2. Optimize Chunk Settings
Updated configuration (already applied):
```python
# Reduced from 300s to 60s per chunk
"chunk_timeout": 60,  

# Maximum 1 hour for entire analysis
"max_analysis_time": 3600,

# Save partial results
"save_partial_results": True
```

### 3. Pre-filter Your Files
Before Deep Forensic analysis:
- Use Quick Triage first (< 10 seconds)
- Extract only the relevant time period
- Remove repetitive log entries
- Focus on error-dense sections

## Recommended Workflow

1. **Quick Triage** → Get immediate insights
2. **Root Cause Analysis** → Understand the primary issue
3. **Strategic Sampling** → For files > 1MB
4. **Deep Forensic** → Only for critical investigations on smaller files

## Performance Tips

### For Large Files (> 5MB):
```bash
# Extract relevant section first
grep -A 50 -B 50 "ERROR" large_file.log > error_section.log

# Then analyze the extracted section
```

### For Faster Analysis:
1. Use a faster Ollama model:
   ```bash
   ollama pull deepseek-r1:1.5b  # Smaller, faster model
   ```

2. Increase Ollama's memory:
   ```bash
   # Windows
   set OLLAMA_MAX_LOADED_MODELS=1
   set OLLAMA_NUM_PARALLEL=1
   
   # Linux/Mac
   export OLLAMA_MAX_LOADED_MODELS=1
   export OLLAMA_NUM_PARALLEL=1
   ```

3. Run analysis on specific time ranges

## Understanding Results

When you see minimal results like:
```
- severity
- confidence  
- processing_time
- formatted_output
```

This indicates the analysis completed but detailed results weren't preserved. The `formatted_output` field should still contain the analysis summary.

## Future Improvements

We're working on:
1. Incremental result saving
2. Resume capability for interrupted analyses
3. Cloud-based analysis options
4. Better progress estimation

## Quick Reference

| File Size | Recommended Method | Expected Time |
|-----------|-------------------|---------------|
| < 100KB   | Quick Triage      | < 10 seconds  |
| 100KB-1MB | Root Cause        | 30-60 seconds |
| 1MB-5MB   | Strategic Sampling| 1-2 minutes   |
| > 5MB     | Extract + Analyze | Varies        |

## Debug Mode

To see what data was returned:
1. Look for "Debug: Raw Results" expander
2. Check if `formatted_output` exists
3. Review the processing time

## Contact

If you continue experiencing issues:
1. Check Ollama is running: `ollama list`
2. Monitor system resources during analysis
3. Consider using cloud-based analysis tools for very large files