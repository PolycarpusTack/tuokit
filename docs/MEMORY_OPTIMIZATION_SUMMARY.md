# Memory & Performance Optimization Summary

## Quick Reference: Optimizations by Impact

### 🔴 **CRITICAL** (Implement First)
These changes provide the most immediate relief for low-memory systems:

1. **Reduce Ollama Context Window**
   ```bash
   export OLLAMA_NUM_CTX=2048  # Default is 4096
   export OLLAMA_MAX_LOADED_MODELS=1
   ```
   - **Impact**: Saves 50% model memory
   - **Effort**: 1 minute

2. **Use Smaller Models**
   - Instead of `deepseek-r1:latest` (8GB)
   - Use `deepseek-r1:1.5b` (2GB) or `tinyllama:latest` (1GB)
   - **Impact**: 75-87% memory reduction
   - **Effort**: Change model name in UI

3. **Reduce Chunk Size**
   - Change from 16KB to 4KB chunks
   - **Impact**: 75% less memory per chunk
   - **Location**: `settings.py` -> `chunk_size: 4096`

### 🟡 **HIGH IMPACT** (Next Priority)

4. **Disable Parallel Processing**
   ```python
   PERFORMANCE_CONFIG = {
       "parallel_chunks": 1,  # Was 4
   }
   ```
   - **Impact**: 75% memory reduction during processing
   - **Location**: `settings.py`

5. **Enable Streaming Mode**
   - Process files in chunks instead of loading entirely
   - **Impact**: Can handle files 10x larger
   - **Effort**: Moderate code change

6. **Add Memory Monitoring**
   - Auto-trigger garbage collection
   - Warn before out-of-memory
   - **Impact**: Prevents crashes

### 🟢 **NICE TO HAVE** (Long Term)

7. **Lazy UI Loading**
   - Load only visible components
   - **Impact**: 30-50% UI memory reduction

8. **Result Pagination**
   - Show 20 results at a time
   - **Impact**: Better for large result sets

## Memory Usage by Component

| Component | Current | Optimized | Savings |
|-----------|---------|-----------|---------|
| Ollama Model | 8GB | 1-2GB | 75-87% |
| File in Memory | 100% | Streaming | 90%+ |
| Chunk Processing | 64KB | 16KB | 75% |
| Parallel Workers | 4x memory | 1x memory | 75% |
| UI Components | All loaded | Lazy loaded | 50% |

## Quick Setup for Low Memory (< 4GB RAM)

### 1. Environment Setup
```bash
# .env file
OLLAMA_NUM_CTX=2048
OLLAMA_MAX_LOADED_MODELS=1
OLLAMA_NUM_PARALLEL=1
TUOKIT_LOW_MEMORY_MODE=true
```

### 2. Model Configuration
```python
# Use in your code
LOW_MEMORY_MODELS = {
    "primary": "tinyllama:latest",
    "fallback": "no_ai_analysis"
}
```

### 3. Performance Settings
```python
LOW_MEMORY_SETTINGS = {
    "chunk_size": 4096,
    "parallel_chunks": 1,
    "use_streaming": True,
    "cache_results": False
}
```

## Expected Results

### Before Optimization
- **Max file size**: 10MB
- **Memory usage**: 8-12GB
- **Processing time**: Variable
- **Crash risk**: High on <8GB systems

### After Optimization
- **Max file size**: 100MB+ (with streaming)
- **Memory usage**: 2-4GB
- **Processing time**: 20-30% slower
- **Crash risk**: Low

## Monitoring Commands

```bash
# Watch memory usage
watch -n 1 free -h

# Monitor Ollama
ps aux | grep ollama

# Python memory profiling
python -m memory_profiler app.py
```

## Trade-offs

### What You Gain
- ✅ Runs on 4GB RAM systems
- ✅ Handles larger files
- ✅ More stable operation
- ✅ Predictable memory usage

### What You Lose
- ❌ 20-30% slower processing
- ❌ Less accurate AI analysis (smaller models)
- ❌ No parallel processing
- ❌ Limited caching

## Emergency Mode

If still running out of memory:

1. **Disable AI completely**
   ```python
   USE_AI = False  # Falls back to pattern matching
   ```

2. **Reduce chunk size further**
   ```python
   EMERGENCY_CHUNK_SIZE = 1024  # 1KB chunks
   ```

3. **Process only critical sections**
   ```python
   SAMPLE_MODE = True  # Analyze only 10% of file
   ```

---
*Remember: It's better to process slowly than to crash!*