"""
Crash Analyzer V2 - Redesigned crash analysis system
Fast, modular crash analysis with tiered methods
"""
from .models import __version__

# Lazy loading - only import main class
_analyzer = None

def get_analyzer():
    """Lazy load the main analyzer to speed up imports"""
    global _analyzer
    if _analyzer is None:
        from .analyzer import CrashAnalyzerV2
        _analyzer = CrashAnalyzerV2
    return _analyzer

# For backwards compatibility
def __getattr__(name):
    if name == "CrashAnalyzerV2":
        return get_analyzer()
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

__all__ = ['get_analyzer', '__version__']
__version__ = __version__  # Re-export from models