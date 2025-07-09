"""
Base Analyzer Class for Crash Analyzer V2
Provides common functionality for all analysis methods
"""
import time
import logging
import re
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List
from datetime import datetime, timezone

from utils import DatabaseManager, capture_knowledge
from utils.tool_base import TuoKitToolBase
from ..utils.patterns import normalize_confidence
from ..utils.ai_utils import safe_ai_generate
from ..utils.logger import get_logger, log_analysis_start, log_analysis_complete
from ..models import AnalysisResult

# Module logger
logger = get_logger(__name__)

class BaseAnalyzer(ABC):
    """Abstract base class for all crash analysis methods"""
    
    def __init__(self, method_config: Dict[str, Any]):
        """
        Initialize base analyzer
        
        Args:
            method_config: Configuration for this analysis method
        """
        self.config = method_config
        self.name = method_config["name"]
        self.description = method_config["description"]
        self.start_time = None
        # Try to get database, but don't fail analyzer init
        try:
            self.db = self._get_database()
        except RuntimeError as e:
            logger.warning(f"Database unavailable: {e}")
            self.db = None
        
    def _get_database(self) -> Optional[DatabaseManager]:
        """Get database instance with proper error handling"""
        try:
            return DatabaseManager()
        except Exception as e:
            logger.error(f"Failed to initialize database: {e}")
            # Re-raise with more context
            raise RuntimeError(f"Database initialization failed: {e}") from e
    
    @abstractmethod
    def analyze(self, content: str, filename: str, **kwargs) -> Dict[str, Any]:
        """
        Perform analysis on crash dump content
        
        Args:
            content: Crash dump content
            filename: Name of the file being analyzed
            **kwargs: Additional analysis parameters
            
        Returns:
            Analysis results dictionary
        """
        raise NotImplementedError("Subclasses must implement analyze()")
    
    @abstractmethod
    def format_output(self, results: Dict[str, Any]) -> str:
        """
        Format analysis results for display
        
        Args:
            results: Raw analysis results
            
        Returns:
            Formatted output string
        """
        raise NotImplementedError("Subclasses must implement format_output()")
    
    def pre_analysis(self, content: str) -> Dict[str, Any]:
        """Common pre-analysis steps"""
        self.start_time = time.time()
        
        # Basic metrics with UTC timestamp
        metrics = {
            "file_size": len(content),
            "line_count": content.count('\n'),
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
        
        # Quick content classification
        metrics["has_stack_trace"] = self._has_stack_trace(content)
        metrics["error_density"] = self._calculate_error_density(content)
        metrics["is_production"] = self._is_production_crash(content)
        
        return metrics
    
    def post_analysis(self, results: Dict[str, Any], content: str, filename: str) -> Dict[str, Any]:
        """Common post-analysis steps"""
        # Add timing
        processing_time = time.time() - self.start_time
        results["processing_time"] = processing_time
        results["analysis_method"] = self.name
        results["filename"] = filename
        
        # Calculate confidence if not present
        if "confidence" not in results:
            results["confidence"] = self._calculate_confidence(results)
        
        # Save to knowledge base if configured
        confidence = normalize_confidence(results.get("confidence", 0))
        results["confidence"] = confidence  # Ensure it's always a float
        
        # Log completion
        log_analysis_complete(self.name, processing_time, confidence)
        
        if self.db and confidence > 0.7:
            try:
                self._save_to_knowledge(results, content, filename)
            except Exception as e:
                logger.error(f"Failed to save to knowledge base: {e}")
        
        return results
    
    def create_analysis_result(self, results: Dict[str, Any]) -> AnalysisResult:
        """
        Create unified AnalysisResult from analyzer output
        
        Args:
            results: Raw analyzer results
            
        Returns:
            AnalysisResult object
        """
        # Get formatted text if available
        text = results.get("formatted_output", "")
        if not text and "format_output" in dir(self):
            text = self.format_output(results)
        
        return AnalysisResult(
            text=text,
            confidence=results.get("confidence", 0.5),
            metadata=results,
            method=self.name,
            processing_time=results.get("processing_time", 0.0)
        )
    
    def _has_stack_trace(self, content: str) -> bool:
        """Check if content contains stack trace patterns"""
        from ..utils.patterns import STACK_TRACE_PATTERNS
        
        # Check first 50KB for performance
        sample = content[:51200] if len(content) > 51200 else content
        
        for lang, pattern in STACK_TRACE_PATTERNS.items():
            if pattern.search(sample):
                return True
        return False
    
    def _calculate_error_density(self, content: str) -> float:
        """Calculate error keyword density per KB"""
        from ..utils.patterns import LOG_LEVEL_PATTERNS
        
        # Sample first 100KB for performance
        sample = content[:102400] if len(content) > 102400 else content
        
        error_count = 0
        if 'error' in LOG_LEVEL_PATTERNS:
            error_count = len(LOG_LEVEL_PATTERNS['error'].findall(sample))
        
        size_kb = len(sample) / 1024
        return error_count / max(size_kb, 1)
    
    def _is_production_crash(self, content: str) -> bool:
        """Detect if crash is from production environment"""
        prod_indicators = [
            r'(?i)\bprod(?:uction)?\b',
            r'(?i)\brelease\b',
            r'(?i)\blive\b',
            r'(?i)www\.',
            r'(?i)api\.',
            r'(?i)\.com\b',
            r'(?i)\.org\b',
        ]
        
        # Check first 5KB
        sample = content[:5120] if len(content) > 5120 else content
        
        for pattern in prod_indicators:
            if re.search(pattern, sample):
                return True
        return False
    
    def _calculate_confidence(self, results: Dict[str, Any]) -> float:
        """Calculate confidence score based on results"""
        confidence = 0.5  # Base confidence
        
        # Factors that increase confidence
        if results.get("root_cause"):
            confidence += 0.2
        if results.get("stack_trace_found"):
            confidence += 0.1
        if results.get("patterns_matched", 0) > 0:
            confidence += min(0.2, results["patterns_matched"] * 0.05)
        if results.get("error_chain"):
            confidence += 0.1
            
        # Factors that decrease confidence
        if results.get("parse_errors", 0) > 0:
            confidence -= 0.1
        if results.get("incomplete_data"):
            confidence -= 0.2
            
        return max(0.0, min(1.0, confidence))
    
    def _save_to_knowledge(self, results: Dict[str, Any], content: str, filename: str):
        """Save analysis results to knowledge base"""
        try:
            # Extract key information
            knowledge_entry = {
                "tool_name": "crash_analyzer_v2",
                "prompt": f"Analyze crash in {filename}",
                "response": self.format_output(results),
                "metadata": {
                    "filename": filename,
                    "analysis_method": self.name,
                    "severity": results.get("severity", "Unknown"),
                    "error_type": results.get("error_type", "Unknown"),
                    "confidence": results.get("confidence", 0),
                    "processing_time": results.get("processing_time", 0)
                }
            }
            
            capture_knowledge(**knowledge_entry)
            
        except Exception as e:
            # Don't fail analysis if knowledge save fails
            pass
    
    def generate_with_model(self, prompt: str, model: str = None, temperature: float = 0.3) -> Optional[str]:
        """
        Generate response using specified model with fallback
        
        Args:
            prompt: The prompt to send to the model
            model: Model name or None for default
            temperature: Generation temperature
            
        Returns:
            Generated response text or None
        """
        if not model:
            logger.warning("No model specified for AI generation")
            return None
            
        try:
            response = safe_ai_generate(
                prompt=prompt,
                model=model,
                temperature=temperature
            )
            
            if response and not response.get("error"):
                return response.get("response", "")
            else:
                logger.warning(f"AI generation returned no response or error: {response}")
                
        except Exception as e:
            logger.error(f"AI generation failed: {e}")
            
        return None