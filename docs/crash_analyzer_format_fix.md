# Crash Analyzer - Format Parameter Fix

## 🐛 Issue Fixed

**Error**: `safe_ollama_generate() got an unexpected keyword argument 'format'`

**Cause**: The crash analyzer was trying to use `format="json"` but the `safe_ollama_generate` function didn't support this parameter.

## ✅ Solution Applied

Updated the `safe_ollama_generate` function in `utils/ollama.py` to accept an optional `format` parameter:

**Before**:
```python
def safe_ollama_generate(model: str, prompt: str, 
                        temperature: float = 0.7,
                        max_tokens: Optional[int] = None,
                        system: Optional[str] = None) -> Dict:
```

**After**:
```python
def safe_ollama_generate(model: str, prompt: str, 
                        temperature: float = 0.7,
                        max_tokens: Optional[int] = None,
                        system: Optional[str] = None,
                        format: Optional[str] = None) -> Dict:
```

The function now properly passes the format parameter to `ollama.generate()` when specified.

## 🚀 Benefits

1. **JSON Responses**: The crash analyzer can now request structured JSON responses from the LLM
2. **Better Parsing**: JSON format ensures consistent, parseable responses
3. **Error Prevention**: Reduces parsing errors from unstructured text

## 📝 Usage

The crash analyzer uses `format="json"` in two key places:
1. When analyzing individual chunks in `analyze_with_chunking()`
2. When performing basic crash analysis in `analyze_crash_dump()`

This ensures the LLM returns properly structured JSON that can be parsed reliably.

## 🧪 Testing

Run the test script to verify the fix:
```bash
python test_format_fix.py
```

The crash analyzer should now work without the format parameter error!
