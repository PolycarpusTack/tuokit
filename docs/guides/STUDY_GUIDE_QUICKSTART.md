# Quick Start Guide: Study Guide Generator

## Installation Steps

1. **Install Required Dependencies**
   ```bash
   pip install requests
   ```

2. **Verify Ollama Models**
   Ensure you have at least one of these models installed:
   - `deepseek-r1:1.5b` (recommended for speed)
   - `deepseek-r1:6.7b` (for better quality)
   - `llama3.2` (alternative)

3. **Start TuoKit**
   ```bash
   # On Windows
   start_tuokit.bat
   
   # On Linux/Mac
   ./start_tuokit.sh
   ```

4. **Access Study Guide Generator**
   - Click "📚 Study Guide" in the sidebar
   - Or click the "📚 Study Guide" button on the dashboard

## First Run Checklist

- [ ] Ollama is running (`ollama serve`)
- [ ] PostgreSQL database is accessible
- [ ] At least one AI model is loaded
- [ ] requests library is installed

## Quick Test

1. Navigate to Study Guide Generator
2. Select "Text" input method
3. Paste this sample text:
   ```
   Python functions are reusable blocks of code that perform specific tasks. 
   They help organize code and avoid repetition. Functions are defined using 
   the 'def' keyword, can accept parameters, and may return values.
   ```
4. Click "Generate Study Guide"
5. Review the generated materials in each tab

## Troubleshooting

If you encounter issues:
1. Check Ollama status on the dashboard
2. Verify database connection in the sidebar
3. Try a smaller AI model (1.5b)
4. Check browser console for errors

## Next Steps

- Try uploading a PDF document
- Test URL extraction with an article
- Save materials to Knowledge Library
- Export study guide as text file
