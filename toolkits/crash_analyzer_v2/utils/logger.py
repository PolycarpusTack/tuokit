"""
Central logging configuration for Crash Analyzer V2
Provides consistent logging across all modules
"""
import logging
import sys
from typing import Optional

# Module logger
_logger: Optional[logging.Logger] = None

def get_logger(name: str = "crash_analyzer_v2") -> logging.Logger:
    """
    Get or create a logger with consistent configuration
    
    Args:
        name: Logger name (defaults to crash_analyzer_v2)
        
    Returns:
        Configured logger instance
    """
    global _logger
    
    if _logger is None:
        _logger = logging.getLogger(name)
        
        # Only configure if no handlers exist
        if not _logger.handlers:
            # Create console handler
            handler = logging.StreamHandler(sys.stdout)
            
            # Create formatter
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                datefmt='%Y-%m-%d %H:%M:%S'
            )
            handler.setFormatter(formatter)
            
            # Add handler to logger
            _logger.addHandler(handler)
            
            # Set default level (can be overridden)
            _logger.setLevel(logging.INFO)
    
    return _logger

def set_log_level(level: str):
    """
    Set logging level
    
    Args:
        level: One of DEBUG, INFO, WARNING, ERROR, CRITICAL
    """
    logger = get_logger()
    numeric_level = getattr(logging, level.upper(), None)
    if numeric_level is not None:
        logger.setLevel(numeric_level)
        logger.info(f"Log level set to {level.upper()}")
    else:
        logger.warning(f"Invalid log level: {level}")

def log_progress(message: str, current: Optional[int] = None, total: Optional[int] = None):
    """
    Log progress with optional counter
    
    Args:
        message: Progress message
        current: Current item number
        total: Total items
    """
    logger = get_logger()
    if current is not None and total is not None:
        logger.info(f"[{current}/{total}] {message}")
    else:
        logger.info(message)

def log_analysis_start(method: str, filename: str, model: Optional[str] = None):
    """Log the start of an analysis"""
    logger = get_logger()
    if model:
        logger.info(f"Starting {method} analysis on '{filename}' using model '{model}'")
    else:
        logger.info(f"Starting {method} analysis on '{filename}'")

def log_analysis_complete(method: str, elapsed_time: float, confidence: float):
    """Log analysis completion"""
    logger = get_logger()
    logger.info(
        f"Completed {method} analysis in {elapsed_time:.2f}s "
        f"(confidence: {confidence:.1%})"
    )

def log_error(message: str, exception: Optional[Exception] = None):
    """
    Log error with user-friendly message
    
    Args:
        message: User-friendly error message
        exception: Optional exception for debug details
    """
    logger = get_logger()
    logger.error(message)
    if exception:
        logger.debug(f"Exception details: {type(exception).__name__}: {str(exception)}")

# Convenience exports
__all__ = [
    'get_logger',
    'set_log_level', 
    'log_progress',
    'log_analysis_start',
    'log_analysis_complete',
    'log_error'
]