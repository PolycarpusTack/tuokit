@echo off
echo ========================================
echo TuoKit Mockup Launcher
echo ========================================
echo.

REM Check if Ollama is running
ollama list >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Ollama is not running!
    echo Please start Ollama first with: ollama serve
    echo.
    pause
    exit /b 1
)

echo ✓ Ollama detected
echo.

REM Check if Streamlit is installed
python -c "import streamlit" >nul 2>&1
if %errorlevel% neq 0 (
    echo Installing required packages...
    pip install -r requirements_mockup.txt
)

echo Starting TuoKit Mockup...
echo.
streamlit run tuokit_mockup.py

pause