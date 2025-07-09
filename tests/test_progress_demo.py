"""
Demo script to showcase the progress tracking features
"""
import streamlit as st
import time
import random
import sys
from pathlib import Path

# Add parent directory to path if needed
sys.path.insert(0, str(Path(__file__).parent))

from toolkits.crash_analyzer_v2.utils.progress_tracker import (
    ProgressTracker, 
    ChunkingProgressTracker, 
    AnalysisProgressTracker
)

st.set_page_config(page_title="Progress Tracker Demo", layout="wide")
st.title("🎯 Crash Analyzer V2 - Progress Tracking Demo")

tab1, tab2, tab3 = st.tabs(["Simple Progress", "Chunking Progress", "Multi-Phase Analysis"])

with tab1:
    st.header("Simple Progress Tracker")
    if st.button("Start Simple Analysis", key="simple"):
        tracker = ProgressTracker()
        tracker.start("🔍 Analyzing Crash Dump", total_steps=10)
        
        steps = [
            (2, "Loading file", "File loaded successfully"),
            (4, "Parsing errors", "Found 42 error patterns"),
            (6, "Extracting stack traces", "Extracted 3 stack traces"),
            (8, "Identifying root cause", "Root cause identified: NullPointerException"),
            (10, "Generating report", "Report generation complete")
        ]
        
        for step, phase, detail in steps:
            time.sleep(0.5)
            tracker.update(step, phase, detail)
            tracker.add_detail(detail, "success" if step == 10 else "processing")
        
        tracker.complete("Analysis finished!")

with tab2:
    st.header("Chunking Progress Tracker")
    if st.button("Start Chunking Demo", key="chunking"):
        chunk_tracker = ChunkingProgressTracker()
        
        # Simulate a 5MB file with 64KB chunks
        file_size = 5 * 1024 * 1024
        chunk_size = 64 * 1024
        total_chunks = (file_size + chunk_size - 1) // chunk_size
        
        chunk_tracker.start_chunking(file_size, chunk_size)
        
        for i in range(total_chunks):
            time.sleep(0.1)  # Simulate processing time
            
            # Randomly simulate different outcomes
            rand = random.random()
            if rand < 0.02:  # 2% error rate
                chunk_tracker.update_chunk(i, "error")
            elif rand < 0.05:  # 3% skip rate
                chunk_tracker.update_chunk(i, "skipped")
            else:
                chunk_tracker.update_chunk(i, "completed")
        
        chunk_tracker.complete(f"Processed {total_chunks} chunks successfully!")

with tab3:
    st.header("Multi-Phase Analysis Progress")
    if st.button("Start Multi-Phase Analysis", key="multiphase"):
        phase_tracker = AnalysisProgressTracker()
        
        # Define analysis phases
        phase_tracker.add_phase("init", "Initialization", 0.5)
        phase_tracker.add_phase("chunking", "Creating chunks", 1.0)
        phase_tracker.add_phase("parallel", "Parallel analysis", 3.0)
        phase_tracker.add_phase("crash", "Crash analysis", 1.5)
        phase_tracker.add_phase("security", "Security scan", 1.0)
        phase_tracker.add_phase("report", "Report generation", 0.5)
        
        phase_tracker.start()
        
        # Simulate phases
        phases = [
            ("init", 10, 0.05),
            ("chunking", 20, 0.03),
            ("parallel", 50, 0.02),
            ("crash", 30, 0.03),
            ("security", 20, 0.04),
            ("report", 10, 0.05)
        ]
        
        for phase_id, steps, delay in phases:
            phase_tracker.start_phase(phase_id)
            
            for i in range(steps):
                time.sleep(delay)
                progress = (i + 1) / steps
                phase_tracker.update_phase(phase_id, progress)
            
            phase_tracker.complete_phase(phase_id)

st.divider()
st.caption("This demo shows the different progress tracking options available in Crash Analyzer V2")