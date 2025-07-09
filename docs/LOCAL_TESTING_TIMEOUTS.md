# Local Testing Timeout Configuration

## Overview
Timeouts have been extended to 2 hours for local testing with Ollama to accommodate slower local hardware and model loading times.

## Changed Configurations

### 1. AI Utilities (`toolkits/crash_analyzer_v2/utils/ai_utils.py`)
- **with_timeout decorator**: 7200 seconds (2 hours)
- **safe_ai_generate timeout**: 7200 seconds (2 hours)

### 2. Performance Config (`toolkits/crash_analyzer_v2/config/settings.py`)
- **chunk_timeout**: 300 seconds (5 minutes per chunk)
- **max_analysis_time**: 7200 seconds (2 hours total)
- **deep_forensic.max_processing_time**: 2 hours

## Why These Changes?

Local Ollama instances often experience:
- Slower model loading times (especially for large models)
- Variable response times based on hardware
- Memory constraints that may slow processing
- No GPU acceleration on some systems

## Recommendations for Local Testing

1. **Start with smaller files** (< 1MB) to test functionality
2. **Use faster models** for initial testing:
   - `deepseek-r1:1.5b` instead of `deepseek-r1:latest`
   - `mistral:7b` instead of larger variants
3. **Monitor resource usage** - Ollama can be memory intensive
4. **Consider using Strategic Sampling** for large files instead of Deep Forensic

## Reverting for Production

For production deployments, consider reverting to shorter timeouts:
```python
# Production values
with_timeout(timeout_seconds=600.0)  # 10 minutes
chunk_timeout: 60  # 1 minute per chunk
max_analysis_time: 1800  # 30 minutes
```

## Performance Tips

1. **Pre-load models**: Run `ollama pull model_name` before testing
2. **Increase Ollama memory**: Set `OLLAMA_MAX_MEMORY` environment variable
3. **Use SSD storage**: Ollama models load faster from SSD
4. **Close other applications**: Free up RAM for Ollama

---
*Note: These extended timeouts are for local development only. Production systems should use appropriate timeouts based on SLA requirements.*