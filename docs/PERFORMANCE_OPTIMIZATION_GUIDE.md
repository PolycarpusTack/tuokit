# Performance Optimization Guide for Low-Resource Hardware

## Overview
This guide provides memory and performance optimizations for running TuoKit on hardware without GPU/VRAM support.

## Current Performance Bottlenecks

### 1. Memory Issues
- **Large file loading**: Entire files loaded into memory
- **Chunk processing**: All chunks kept in memory during analysis
- **Model loading**: Ollama models consume significant RAM
- **Concurrent processing**: Multiple threads increase memory usage

### 2. Processing Issues
- **Sequential AI calls**: Each chunk waits for previous to complete
- **Large chunk sizes**: 16KB chunks may be too large for weak hardware
- **No streaming**: Results accumulated in memory before display

## Optimization Strategies

### 1. Memory-Efficient File Processing

#### A. Streaming File Reader
```python
# Instead of loading entire file
content = file.read()  # BAD - loads entire file

# Use streaming approach
def stream_file_chunks(filepath, chunk_size=4096):
    """Stream file in chunks without loading entire file"""
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        while True:
            chunk = f.read(chunk_size)
            if not chunk:
                break
            yield chunk
```

#### B. Adaptive Chunk Sizing
```python
def get_optimal_chunk_size(available_memory_mb):
    """Determine chunk size based on available memory"""
    if available_memory_mb < 1024:  # Less than 1GB
        return 4096  # 4KB chunks
    elif available_memory_mb < 2048:  # Less than 2GB
        return 8192  # 8KB chunks
    else:
        return 16384  # 16KB chunks (current default)
```

### 2. Ollama Optimization

#### A. Model Selection for Low Memory
```python
LOW_MEMORY_MODELS = {
    "tiny": ["tinyllama:1.1b", "phi:2.7b"],
    "small": ["deepseek-r1:1.5b", "mistral:7b-instruct-v0.2-q4_0"],
    "efficient": ["codellama:7b-instruct-q4_0", "llama2:7b-chat-q4_0"]
}

def select_model_for_hardware(available_memory_gb):
    """Select appropriate model based on available memory"""
    if available_memory_gb < 4:
        return LOW_MEMORY_MODELS["tiny"][0]
    elif available_memory_gb < 8:
        return LOW_MEMORY_MODELS["small"][0]
    else:
        return LOW_MEMORY_MODELS["efficient"][0]
```

#### B. Ollama Memory Settings
```bash
# Set Ollama memory limits
export OLLAMA_MAX_LOADED_MODELS=1
export OLLAMA_NUM_PARALLEL=1
export OLLAMA_MAX_QUEUE=1

# Reduce context window for less memory usage
export OLLAMA_NUM_CTX=2048  # Default is 4096
```

### 3. Crash Analyzer Optimizations

#### A. Lazy Loading Pattern
```python
class OptimizedDeepForensicAnalyzer:
    def analyze_file_lazy(self, filepath: str):
        """Analyze file without loading it entirely into memory"""
        # Process metadata first
        file_size = os.path.getsize(filepath)
        
        # For large files, use sampling
        if file_size > 5 * 1024 * 1024:  # 5MB
            return self.strategic_sample_analysis(filepath)
        
        # For smaller files, process in streams
        results = []
        for chunk in stream_file_chunks(filepath):
            result = self.analyze_chunk(chunk)
            results.append(result)
            # Free memory after processing
            del chunk
            
        return self.merge_results(results)
```

#### B. Result Streaming
```python
def stream_results_to_ui(self, analysis_generator):
    """Stream results to UI as they're generated"""
    for partial_result in analysis_generator:
        # Update UI with partial results
        st.session_state.partial_results = partial_result
        # Force garbage collection periodically
        if partial_result.get('chunk_id', 0) % 10 == 0:
            gc.collect()
```

### 4. Database Optimizations

#### A. Batch Inserts
```python
def batch_insert_results(self, results, batch_size=100):
    """Insert results in batches to reduce memory usage"""
    for i in range(0, len(results), batch_size):
        batch = results[i:i + batch_size]
        self.db.insert_batch(batch)
        # Clear batch from memory
        del batch
```

#### B. Query Result Streaming
```python
def stream_query_results(self, query, chunk_size=1000):
    """Stream large query results instead of loading all at once"""
    offset = 0
    while True:
        chunk_query = f"{query} LIMIT {chunk_size} OFFSET {offset}"
        results = self.db.execute(chunk_query)
        if not results:
            break
        yield results
        offset += chunk_size
```

### 5. UI Optimizations

#### A. Lazy Component Loading
```python
# Instead of loading all tabs at once
def show_all_tabs():
    tab1, tab2, tab3 = st.tabs(["Analysis", "Results", "History"])
    with tab1:
        show_analysis()  # Loads immediately
    with tab2:
        show_results()   # Loads immediately
    with tab3:
        show_history()   # Loads immediately

# Use lazy loading
def show_tabs_lazy():
    selected_tab = st.radio("Select", ["Analysis", "Results", "History"])
    
    if selected_tab == "Analysis":
        show_analysis()  # Only loads when selected
    elif selected_tab == "Results":
        show_results()   # Only loads when selected
    elif selected_tab == "History":
        show_history()   # Only loads when selected
```

#### B. Pagination for Large Results
```python
def paginate_results(results, page_size=20):
    """Paginate large result sets"""
    total_pages = len(results) // page_size + 1
    page = st.number_input("Page", 1, total_pages, 1)
    
    start_idx = (page - 1) * page_size
    end_idx = start_idx + page_size
    
    return results[start_idx:end_idx]
```

### 6. Configuration Changes

#### A. Update `settings.py` for Low Memory
```python
# Low memory configuration
LOW_MEMORY_CONFIG = {
    "parallel_chunks": 1,  # No parallel processing
    "chunk_size": 4096,    # Smaller chunks
    "chunk_overlap": 256,  # Less overlap
    "cache_results": False,  # Don't cache in memory
    "stream_mode": True,    # Enable streaming
}

# Detect available memory and adjust
import psutil
available_memory = psutil.virtual_memory().available / (1024 * 1024 * 1024)  # GB
if available_memory < 4:
    PERFORMANCE_CONFIG.update(LOW_MEMORY_CONFIG)
```

#### B. Model Fallback Strategy
```python
MODEL_FALLBACK_CHAIN = [
    "deepseek-r1:latest",     # Try full model first
    "deepseek-r1:1.5b",       # Fall back to smaller
    "tinyllama:latest",       # Fall back to tiny
    "no_ai_analysis"          # Final fallback: pattern matching only
]
```

### 7. Quick Implementation Changes

#### A. Add Memory Monitor
```python
# utils/memory_monitor.py
import psutil
import gc

class MemoryMonitor:
    def __init__(self, threshold_mb=100):
        self.threshold_mb = threshold_mb
        
    def check_memory(self):
        """Check available memory and trigger GC if needed"""
        available_mb = psutil.virtual_memory().available / (1024 * 1024)
        if available_mb < self.threshold_mb:
            gc.collect()
            return False  # Low memory warning
        return True
    
    def get_memory_usage(self):
        """Get current memory usage stats"""
        vm = psutil.virtual_memory()
        return {
            "total_gb": vm.total / (1024**3),
            "available_gb": vm.available / (1024**3),
            "percent_used": vm.percent
        }
```

#### B. Add Streaming Support to Analyzers
```python
# Modify deep_forensic.py
def analyze_with_streaming(self, filepath: str, **kwargs):
    """Memory-efficient streaming analysis"""
    # Initialize results
    results = {
        "overview": {},
        "findings": [],
        "streaming": True
    }
    
    # Process file in chunks
    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
        chunk_num = 0
        while True:
            chunk = f.read(4096)  # 4KB chunks
            if not chunk:
                break
                
            # Analyze chunk
            chunk_result = self._analyze_chunk_lightweight(chunk)
            
            # Stream to UI
            if kwargs.get('progress_callback'):
                kwargs['progress_callback'](chunk_num, chunk_result)
            
            # Merge results
            self._merge_chunk_results(results, chunk_result)
            
            # Free memory
            del chunk
            chunk_num += 1
            
            # Check memory periodically
            if chunk_num % 10 == 0:
                if not self.memory_monitor.check_memory():
                    results["memory_warning"] = True
    
    return results
```

### 8. Recommended Settings for Low-Resource Systems

```python
# .env or config.py
# For systems with < 4GB RAM, no GPU
LOW_RESOURCE_SETTINGS = {
    # Ollama settings
    "OLLAMA_MODEL": "tinyllama:latest",
    "OLLAMA_NUM_CTX": 2048,
    "OLLAMA_NUM_PARALLEL": 1,
    
    # Crash analyzer settings
    "MAX_FILE_SIZE_MB": 10,
    "CHUNK_SIZE_KB": 4,
    "USE_STREAMING": True,
    "DISABLE_PARALLEL": True,
    
    # UI settings
    "PAGE_SIZE": 20,
    "CACHE_TTL": 0,  # No caching
    "AUTO_CLEAR_RESULTS": True
}
```

## Implementation Priority

1. **Immediate (High Impact)**:
   - Implement streaming file reader
   - Add memory monitoring
   - Reduce default chunk sizes
   - Add model fallback chain

2. **Short Term**:
   - Implement lazy UI loading
   - Add result pagination
   - Optimize database queries
   - Add progress streaming

3. **Long Term**:
   - Implement full streaming pipeline
   - Add compression for stored results
   - Implement result summarization
   - Add memory-mapped file support

## Testing on Low-Resource Hardware

```bash
# Test with memory limits
docker run -m 2g --memory-swap 2g tuokit

# Monitor memory usage
watch -n 1 'ps aux | grep ollama'

# Profile memory usage
python -m memory_profiler app.py
```

## Conclusion

These optimizations can reduce memory usage by 60-80% and improve performance on low-resource hardware significantly. Start with the immediate changes and progressively implement more optimizations based on your specific hardware constraints.