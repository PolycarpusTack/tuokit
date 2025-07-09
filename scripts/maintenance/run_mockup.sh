#!/bin/bash

echo "========================================"
echo "TuoKit Mockup Launcher"
echo "========================================"
echo

# Check if Ollama is running
if ! command -v ollama &> /dev/null; then
    echo "ERROR: Ollama is not installed!"
    echo "Please install Ollama from: https://ollama.ai"
    exit 1
fi

if ! ollama list &> /dev/null; then
    echo "ERROR: Ollama is not running!"
    echo "Please start Ollama first with: ollama serve"
    exit 1
fi

echo "✓ Ollama detected"
echo

# Check if Streamlit is installed
if ! python -c "import streamlit" &> /dev/null; then
    echo "Installing required packages..."
    pip install -r requirements_mockup.txt
fi

echo "Starting TuoKit Mockup..."
echo
streamlit run tuokit_mockup.py