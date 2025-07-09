#!/bin/bash

echo "Starting TuoKit Modern UI..."
echo

# Check if Python is installed
if ! command -v python3 &> /dev/null; then
    echo "Error: Python 3 is not installed"
    echo "Please install Python 3.8+ from https://www.python.org/downloads/"
    exit 1
fi

# Check if virtual environment exists
if [ ! -d "tuokit-env" ]; then
    echo "Creating virtual environment..."
    python3 -m venv tuokit-env
fi

# Activate virtual environment
echo "Activating virtual environment..."
source tuokit-env/bin/activate

# Install/upgrade requirements
echo "Installing requirements..."
pip install -r requirements.txt > /dev/null 2>&1

# Install additional requirements for modern UI
echo "Installing modern UI dependencies..."
pip install plotly==5.20.0 streamlit-extras==0.4.2 pyperclip==1.8.2 > /dev/null 2>&1

# Check if Ollama is running
echo
echo "Checking Ollama status..."
if ! ollama list > /dev/null 2>&1; then
    echo "Warning: Ollama is not running. Some features may not work."
    echo "To start Ollama, run: ollama serve"
    echo
fi

# Start the modern UI
echo
echo "Starting TuoKit Modern UI on http://localhost:8501"
echo "Press Ctrl+C to stop the server"
echo
streamlit run app_modern.py
