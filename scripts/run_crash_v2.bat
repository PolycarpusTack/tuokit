@echo off
echo Starting Crash Analyzer V2...
call tuokit-env\Scripts\activate
streamlit run pages/crash_analyzer_v2.py --server.port 8502
pause