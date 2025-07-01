@echo off
echo === TuoKit Quick Start ===
echo.

REM Check if virtual environment exists
if not exist "tuokit-env" (
    echo Creating virtual environment...
    python -m venv tuokit-env
    echo Virtual environment created!
    echo.
)

echo Activating virtual environment...
call tuokit-env\Scripts\activate.bat

echo Installing dependencies...
pip install -r requirements.txt
echo.

echo Verifying installations...
python test_ollama.py
python test_pdf.py
echo.

echo Starting TuoKit Dashboard...
echo.
echo TuoKit will open in your browser at http://localhost:8501
echo Press Ctrl+C to stop the server
echo.

streamlit run app.py