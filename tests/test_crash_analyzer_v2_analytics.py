# tests/test_crash_analyzer_v2_analytics.py
"""Test analytics functionality for Crash Analyzer V2"""

import pytest
from datetime import datetime, timedelta
from toolkits.crash_analyzer_v2.analytics import CrashStatistics

class TestCrashAnalyzerAnalytics:
    """Test the analytics functionality"""
    
    def test_statistics_init(self):
        """Test that statistics engine initializes"""
        stats = CrashStatistics()
        assert hasattr(stats, 'db')
        assert hasattr(stats, 'get_crash_distribution')
        assert hasattr(stats, 'get_severity_metrics')
        assert hasattr(stats, 'get_time_series_data')
    
    def test_extract_error_type(self):
        """Test error type extraction from responses"""
        stats = CrashStatistics()
        
        # Test JSON response
        json_response = '{"error_type": "KERNEL_PANIC", "severity": "CRITICAL"}'
        assert stats._extract_error_type(json_response) == "KERNEL_PANIC"
        
        # Test text response
        text_response = "Error Type: ACCESS_VIOLATION\nSeverity: HIGH"
        assert stats._extract_error_type(text_response) == "ACCESS_VIOLATION"
        
        # Test unknown format
        unknown_response = "Some random text"
        assert stats._extract_error_type(unknown_response) == "Unknown"
    
    def test_extract_severity(self):
        """Test severity extraction from responses"""
        stats = CrashStatistics()
        
        # Test JSON response
        json_response = '{"error_type": "KERNEL_PANIC", "severity": "CRITICAL"}'
        assert stats._extract_severity(json_response) == "CRITICAL"
        
        # Test text response
        text_response = "Error Type: ACCESS_VIOLATION\nSeverity: HIGH"
        assert stats._extract_severity(text_response) == "HIGH"
        
        # Test unknown format (should default to MEDIUM)
        unknown_response = "Some random text"
        assert stats._extract_severity(unknown_response) == "MEDIUM"
    
    def test_calculate_metrics_summary(self):
        """Test metrics summary calculation"""
        stats = CrashStatistics()
        
        # With no database, should return empty/zero metrics
        metrics = stats.calculate_metrics_summary("30d")
        
        assert "total_crashes" in metrics
        assert "trend_percentage" in metrics
        assert "critical_issues" in metrics
        assert "risk_score" in metrics
        assert "unique_error_types" in metrics
        assert "most_common_error" in metrics
        
        # Values should be 0 or appropriate defaults without data
        assert metrics["total_crashes"] == 0
        assert metrics["risk_score"] == 0
    
    def test_time_series_structure(self):
        """Test time series data structure"""
        stats = CrashStatistics()
        
        # Without database, should return empty structure
        time_data = stats.get_time_series_data("30d")
        
        assert "dates" in time_data
        assert "counts" in time_data
        assert isinstance(time_data["dates"], list)
        assert isinstance(time_data["counts"], list)
        assert len(time_data["dates"]) == len(time_data["counts"])
    
    def test_dashboard_imports(self):
        """Test that dashboard can be imported"""
        from toolkits.crash_analyzer_v2.analytics.dashboard import CrashAnalyticsDashboard
        
        dashboard = CrashAnalyticsDashboard()
        assert hasattr(dashboard, 'render')
        assert hasattr(dashboard, 'stats')
    
    def test_analytics_module_exports(self):
        """Test that analytics module exports correctly"""
        from toolkits.crash_analyzer_v2.analytics import CrashStatistics, CrashAnalyticsDashboard
        
        assert CrashStatistics is not None
        assert CrashAnalyticsDashboard is not None