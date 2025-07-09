"""
Progress tracking utilities for visual feedback during analysis
"""
import streamlit as st
import time
from typing import Optional, Dict, Any, Callable
from dataclasses import dataclass
from datetime import datetime
import threading

@dataclass
class ProgressState:
    """Tracks progress state for an analysis operation"""
    total_steps: int
    current_step: int
    current_phase: str
    start_time: float
    messages: list
    is_complete: bool = False
    has_error: bool = False
    error_message: str = ""

class ProgressTracker:
    """Enhanced progress tracking with visual feedback"""
    
    def __init__(self, container=None):
        """Initialize progress tracker with optional container"""
        self.container = container or st.container()
        self.progress_bar = None
        self.status_text = None
        self.phase_text = None
        self.metrics_cols = None
        self.details_expander = None
        self.state = None
        
    def start(self, title: str, total_steps: int = 100, show_metrics: bool = True):
        """Start progress tracking with visual elements"""
        with self.container:
            st.markdown(f"### {title}")
            
            # Create layout
            col1, col2 = st.columns([3, 1])
            
            with col1:
                self.progress_bar = st.progress(0)
                self.status_text = st.empty()
                self.phase_text = st.empty()
            
            with col2:
                if show_metrics:
                    self.elapsed_metric = st.empty()
                    self.speed_metric = st.empty()
            
            # Details expander for live updates
            self.details_expander = st.expander("📊 Analysis Details", expanded=True)
            with self.details_expander:
                self.details_container = st.container()
                
        self.state = ProgressState(
            total_steps=total_steps,
            current_step=0,
            current_phase="Initializing",
            start_time=time.time(),
            messages=[]
        )
        
        self._update_display()
    
    def update(self, step: int, phase: str, message: Optional[str] = None):
        """Update progress with new step and phase"""
        if not self.state:
            return
            
        self.state.current_step = min(step, self.state.total_steps)
        self.state.current_phase = phase
        
        if message:
            self.state.messages.append({
                "time": datetime.now().strftime("%H:%M:%S"),
                "phase": phase,
                "message": message
            })
        
        self._update_display()
    
    def add_detail(self, detail: str, detail_type: str = "info"):
        """Add a detail message to the expander"""
        if not self.state:
            return
            
        icon = {
            "info": "ℹ️",
            "success": "✅",
            "warning": "⚠️",
            "error": "❌",
            "processing": "⚙️"
        }.get(detail_type, "•")
        
        self.state.messages.append({
            "time": datetime.now().strftime("%H:%M:%S"),
            "icon": icon,
            "message": detail
        })
        
        self._update_details()
    
    def complete(self, message: str = "Analysis complete!"):
        """Mark progress as complete"""
        if not self.state:
            return
            
        self.state.is_complete = True
        self.state.current_step = self.state.total_steps
        self.update(self.state.total_steps, "Complete", message)
        
        # Collapse details on completion
        if self.details_expander:
            self.details_expander.expanded = False
    
    def error(self, error_message: str):
        """Mark progress as errored"""
        if not self.state:
            return
            
        self.state.has_error = True
        self.state.error_message = error_message
        self.add_detail(error_message, "error")
        self._update_display()
    
    def _update_display(self):
        """Update all visual elements"""
        if not self.state:
            return
            
        # Calculate progress
        progress = self.state.current_step / self.state.total_steps
        elapsed = time.time() - self.state.start_time
        
        # Update progress bar
        if self.progress_bar:
            self.progress_bar.progress(progress)
        
        # Update status text
        if self.status_text:
            if self.state.has_error:
                self.status_text.error(f"❌ Error: {self.state.error_message}")
            elif self.state.is_complete:
                self.status_text.success(f"✅ {self.state.current_phase}")
            else:
                self.status_text.info(f"🔄 {self.state.current_phase}...")
        
        # Update phase text with animated indicator
        if self.phase_text and not self.state.is_complete:
            dots = "." * (int(elapsed * 2) % 4)
            self.phase_text.caption(f"Step {self.state.current_step}/{self.state.total_steps} {dots}")
        
        # Update metrics
        if hasattr(self, 'elapsed_metric'):
            self.elapsed_metric.metric("⏱️ Elapsed", f"{elapsed:.1f}s")
            
        if hasattr(self, 'speed_metric') and self.state.current_step > 0:
            speed = self.state.current_step / elapsed
            self.speed_metric.metric("⚡ Speed", f"{speed:.1f} steps/s")
        
        # Update details
        self._update_details()
    
    def _update_details(self):
        """Update the details expander with messages"""
        if not self.details_container or not self.state:
            return
            
        # Show last 10 messages
        with self.details_container:
            # Clear previous content
            self.details_container.empty()
            
            # Show recent messages
            for msg in self.state.messages[-10:]:
                if "icon" in msg:
                    st.caption(f"{msg['time']} {msg['icon']} {msg['message']}")
                else:
                    st.caption(f"{msg['time']} [{msg['phase']}] {msg['message']}")

class ChunkingProgressTracker(ProgressTracker):
    """Specialized tracker for chunk processing"""
    
    def start_chunking(self, total_size: int, chunk_size: int):
        """Start chunking progress"""
        total_chunks = (total_size + chunk_size - 1) // chunk_size
        self.start(f"🔪 Processing {total_chunks} chunks", total_chunks)
        self.chunk_size = chunk_size
        self.total_size = total_size
        
    def update_chunk(self, chunk_id: int, chunk_status: str = "processing"):
        """Update progress for a specific chunk"""
        chunk_start = chunk_id * self.chunk_size
        chunk_end = min(chunk_start + self.chunk_size, self.total_size)
        
        icons = {
            "processing": "⚙️",
            "completed": "✅",
            "error": "❌",
            "skipped": "⏭️"
        }
        
        icon = icons.get(chunk_status, "•")
        message = f"{icon} Chunk {chunk_id + 1}: bytes {chunk_start:,} - {chunk_end:,}"
        
        self.update(
            chunk_id + 1,
            f"Processing chunk {chunk_id + 1}",
            message
        )
        
        # Add chunk-specific details
        if chunk_status == "completed":
            self.add_detail(f"Chunk {chunk_id + 1} analyzed successfully", "success")
        elif chunk_status == "error":
            self.add_detail(f"Chunk {chunk_id + 1} failed", "error")

class AnalysisProgressTracker:
    """Multi-phase analysis progress tracker"""
    
    def __init__(self, container=None):
        self.container = container or st.container()
        self.phases = {}
        self.current_phase = None
        
    def add_phase(self, phase_id: str, phase_name: str, weight: float = 1.0):
        """Add a phase to track"""
        self.phases[phase_id] = {
            "name": phase_name,
            "weight": weight,
            "progress": 0,
            "status": "pending"
        }
    
    def start(self):
        """Start multi-phase tracking"""
        with self.container:
            st.markdown("### 📊 Analysis Progress")
            
            # Overall progress
            self.overall_progress = st.progress(0)
            self.overall_status = st.empty()
            
            # Phase breakdown
            self.phase_container = st.container()
            
            # Time estimate
            self.time_container = st.container()
            
        self.start_time = time.time()
        self._update_display()
    
    def start_phase(self, phase_id: str):
        """Start a specific phase"""
        if phase_id in self.phases:
            self.current_phase = phase_id
            self.phases[phase_id]["status"] = "running"
            self.phases[phase_id]["start_time"] = time.time()
            self._update_display()
    
    def update_phase(self, phase_id: str, progress: float):
        """Update progress for a phase"""
        if phase_id in self.phases:
            self.phases[phase_id]["progress"] = progress
            self._update_display()
    
    def complete_phase(self, phase_id: str):
        """Mark a phase as complete"""
        if phase_id in self.phases:
            self.phases[phase_id]["status"] = "complete"
            self.phases[phase_id]["progress"] = 1.0
            if "start_time" in self.phases[phase_id]:
                self.phases[phase_id]["duration"] = time.time() - self.phases[phase_id]["start_time"]
            self._update_display()
    
    def _update_display(self):
        """Update the multi-phase display"""
        # Calculate overall progress
        total_weight = sum(p["weight"] for p in self.phases.values())
        weighted_progress = sum(
            p["weight"] * p["progress"] for p in self.phases.values()
        ) / total_weight if total_weight > 0 else 0
        
        # Update overall progress
        self.overall_progress.progress(weighted_progress)
        
        # Update status
        elapsed = time.time() - self.start_time
        if weighted_progress < 1.0:
            remaining = (elapsed / weighted_progress * (1 - weighted_progress)) if weighted_progress > 0 else 0
            self.overall_status.info(
                f"⏳ Progress: {weighted_progress:.0%} | "
                f"Elapsed: {elapsed:.1f}s | "
                f"Remaining: ~{remaining:.0f}s"
            )
        else:
            self.overall_status.success(f"✅ Complete in {elapsed:.1f}s")
        
        # Update phase breakdown
        with self.phase_container:
            for phase_id, phase_data in self.phases.items():
                col1, col2, col3 = st.columns([3, 1, 1])
                
                with col1:
                    # Phase name with status icon
                    status_icons = {
                        "pending": "⏸️",
                        "running": "▶️",
                        "complete": "✅"
                    }
                    icon = status_icons.get(phase_data["status"], "•")
                    st.caption(f"{icon} {phase_data['name']}")
                    
                    # Phase progress bar
                    st.progress(phase_data["progress"])
                
                with col2:
                    st.caption(f"{phase_data['progress']:.0%}")
                
                with col3:
                    if phase_data["status"] == "complete" and "duration" in phase_data:
                        st.caption(f"{phase_data['duration']:.1f}s")
                    elif phase_data["status"] == "running":
                        st.caption("🔄")

def create_progress_callback(tracker: ProgressTracker, phase: str) -> Callable:
    """Create a callback function for progress updates"""
    def callback(progress: float, message: Optional[str] = None):
        step = int(progress * tracker.state.total_steps)
        tracker.update(step, phase, message)
    return callback