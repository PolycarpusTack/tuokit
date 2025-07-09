# toolkits/crash_analyzer_v2/analytics/__init__.py
"""
Crash Analytics Module - Business Intelligence for Crash Analysis
"""

from .statistics import CrashStatistics
from .dashboard import CrashAnalyticsDashboard

__all__ = ['CrashStatistics', 'CrashAnalyticsDashboard']