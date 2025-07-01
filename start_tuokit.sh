#!/bin/bash

echo "=== TuoKit Quick Start ==="
echo

# Check if virtual environment exists
if [ ! -d "tuokit-env" ]; then
    echo "Creating virtual environment..."
    python3 -m venv tuokit-env
    echo "Virtual environment created!"
    echo
fi

echo "Activating virtual environment..."
source tuokit-env/bin/activate

echo "Installing dependencies..."
pip install -r requirements.txt
echo

echo "Verifying installations..."
python test_ollama.py
python test_pdf.py
echo

echo "Starting TuoKit Dashboard..."
echo
echo "TuoKit will open in your browser at http://localhost:8501"
echo "Press Ctrl+C to stop the server"
echo

streamlit run app.py