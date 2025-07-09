"""
Data models for Crash Analyzer V2
Provides consistent result structures across all analyzers
"""
from dataclasses import dataclass, field
from typing import Dict, Any, Optional, List
from datetime import datetime, timezone

@dataclass
class AnalysisResult:
    """
    Unified result structure for all analysis methods
    
    Attributes:
        text: Human-readable analysis summary
        confidence: Confidence score (0.0 to 1.0)
        metadata: Additional structured data
        method: Analysis method used
        processing_time: Time taken in seconds
        timestamp: When analysis was performed
    """
    text: str
    confidence: float = 0.5
    metadata: Dict[str, Any] = field(default_factory=dict)
    method: str = "unknown"
    processing_time: float = 0.0
    timestamp: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat())
    
    def __post_init__(self):
        """Validate and normalize values"""
        # Ensure confidence is in valid range
        self.confidence = max(0.0, min(1.0, float(self.confidence)))
        
    @property
    def confidence_label(self) -> str:
        """Get human-readable confidence label"""
        if self.confidence >= 0.9:
            return "Very High"
        elif self.confidence >= 0.75:
            return "High"
        elif self.confidence >= 0.5:
            return "Medium"
        elif self.confidence >= 0.25:
            return "Low"
        else:
            return "Very Low"
    
    @property
    def is_high_confidence(self) -> bool:
        """Check if this is a high-confidence result"""
        return self.confidence >= 0.75
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization"""
        return {
            "text": self.text,
            "confidence": self.confidence,
            "confidence_label": self.confidence_label,
            "metadata": self.metadata,
            "method": self.method,
            "processing_time": self.processing_time,
            "timestamp": self.timestamp
        }

@dataclass
class ErrorInfo:
    """Information about a detected error"""
    type: str
    message: str
    location: str = "Unknown"
    severity: str = "MEDIUM"
    position: int = -1
    
    def __str__(self) -> str:
        return f"{self.type}: {self.message} at {self.location}"

@dataclass
class ChunkResult:
    """Result from analyzing a single chunk"""
    chunk_id: int
    patterns_found: List[Dict[str, Any]] = field(default_factory=list)
    issues_found: List[str] = field(default_factory=list)
    metrics: Dict[str, Any] = field(default_factory=dict)
    error: Optional[str] = None
    
    @property
    def has_issues(self) -> bool:
        return bool(self.patterns_found or self.issues_found)

# Progress callback type for UI integration
from typing import Protocol

class ProgressCallback(Protocol):
    """Protocol for progress callbacks"""
    def __call__(self, current: int, total: int, message: str = "") -> None:
        """Update progress"""
        ...

# Version info
__version__ = "2.0.0"