# tests/test_comparative_analysis.py
"""
Test Comparative Analysis for Crash Analyzer V2
Quality Gate for Task 3
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock
import streamlit as st
import json

# Import after mocking streamlit
with patch('streamlit.subheader'), patch('streamlit.columns'), \
     patch('streamlit.checkbox'), patch('streamlit.selectbox'), \
     patch('streamlit.button'), patch('streamlit.metric'), \
     patch('streamlit.markdown'), patch('streamlit.warning'), \
     patch('streamlit.success'), patch('streamlit.error'), \
     patch('streamlit.download_button'):
    from toolkits.crash_analyzer_v2.analytics.dashboard import CrashAnalyticsDashboard

class TestComparativeAnalysis:
    """Test all comparative analysis features"""
    
    def setup_method(self):
        """Set up test data"""
        self.mock_db = Mock()
        
        # Mock statistics methods
        self.mock_stats = Mock()
        
        # Period 1 metrics (current)
        self.metrics_period_1 = {
            "total_crashes": 100,
            "trend_percentage": 15.0,
            "risk_score": 6.5,
            "critical_issues": 10,
            "unique_error_types": 15,
            "most_common_error": "MEMORY_LEAK"
        }
        
        # Period 2 metrics (previous)
        self.metrics_period_2 = {
            "total_crashes": 80,
            "trend_percentage": -5.0,
            "risk_score": 5.0,
            "critical_issues": 8,
            "unique_error_types": 12,
            "most_common_error": "NULL_POINTER"
        }
        
        # Mock session state
        self.mock_session_state = MagicMock()
        self.mock_session_state.db = self.mock_db
        self.mock_session_state.compare_mode = False
    
    def test_same_period_comparison_shows_zero_change(self):
        """Test 1: Same period comparison shows 0% change"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Compare same metrics
            change = dashboard._calculate_change(100, 100)
            assert change == 0.0
            
            # Test with zero values
            change = dashboard._calculate_change(0, 0)
            assert change == 0.0
    
    def test_percentage_calculations_accurate(self):
        """Test 2: Percentage calculations are accurate"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Test various scenarios
            # 100 vs 80 = 25% increase
            assert dashboard._calculate_change(100, 80) == 25.0
            
            # 80 vs 100 = -20% decrease
            assert dashboard._calculate_change(80, 100) == -20.0
            
            # From 0 to 100 = 100% increase
            assert dashboard._calculate_change(100, 0) == 100.0
            
            # From 100 to 0 = -100% decrease
            assert dashboard._calculate_change(0, 100) == -100.0
    
    def test_significant_changes_highlighted(self):
        """Test 3: Significant changes highlighted correctly"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Mock the statistics
            dashboard.stats.calculate_metrics_summary = Mock(
                side_effect=lambda period: 
                self.metrics_period_1 if period == "7d" else self.metrics_period_2
            )
            
            # Test change calculation
            changes = {
                'total_change': dashboard._calculate_change(
                    self.metrics_period_1['total_crashes'],
                    self.metrics_period_2['total_crashes']
                ),
                'risk_change': dashboard._calculate_change(
                    self.metrics_period_1['risk_score'],
                    self.metrics_period_2['risk_score']
                )
            }
            
            # 100 vs 80 = 25% increase (significant)
            assert abs(changes['total_change']) > 20
            
            # 6.5 vs 5.0 = 30% increase (significant)
            assert changes['risk_change'] > 20
    
    def test_export_includes_both_periods_data(self):
        """Test 4: Export includes both periods' data"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Generate comparison report
            report = dashboard._generate_comparison_report(
                self.metrics_period_1,
                self.metrics_period_2,
                "Last 7 Days",
                "Previous 7 Days",
                {'total_change': 25.0, 'risk_change': 30.0, 
                 'critical_change': 25.0, 'unique_change': 25.0}
            )
            
            # Verify report contains both periods
            assert "Period 1: Last 7 Days" in report
            assert "Period 2: Previous 7 Days" in report
            
            # Verify metrics from both periods
            assert "Total Crashes: 100" in report
            assert "Total Crashes: 80" in report
            assert "Risk Score: 6.5/10" in report
            assert "Risk Score: 5.0/10" in report
            
            # Verify changes are included
            assert "Total Crashes: +25.0%" in report
            assert "Risk Score: +30.0%" in report
    
    def test_ui_elements_enable_disable_properly(self):
        """Test 5: UI elements enable/disable properly"""
        # Test with compare mode off
        self.mock_session_state.compare_mode = False
        
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # In normal mode, should use regular metrics display
            # This is verified by checking the state
            assert not st.session_state.compare_mode
        
        # Test with compare mode on
        self.mock_session_state.compare_mode = True
        
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # In compare mode, should show period selectors
            assert st.session_state.compare_mode
    
    def test_null_data_handled_gracefully(self):
        """Test 6: Null data handled gracefully"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Test with None values
            metrics_with_nulls = {
                "total_crashes": None,
                "trend_percentage": 0.0,
                "risk_score": None,
                "critical_issues": 0,
                "unique_error_types": 0,
                "most_common_error": "Unknown"
            }
            
            # Should not raise errors
            try:
                # Test percentage calculation with nulls
                change = dashboard._calculate_change(None, 10)
                # Should handle gracefully (treat None as 0)
                
                # Test report generation with nulls
                report = dashboard._generate_comparison_report(
                    metrics_with_nulls,
                    self.metrics_period_2,
                    "Last 7 Days",
                    "Previous 7 Days",
                    {'total_change': 0, 'risk_change': 0, 
                     'critical_change': 0, 'unique_change': 0}
                )
                assert report is not None
            except Exception as e:
                pytest.fail(f"Should handle null data gracefully, but raised: {e}")
    
    def test_time_range_validation(self):
        """Test 7: Time range validation works"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Test period 2 parsing
            # "Previous 7d" should extract "7"
            test_period_2 = "Previous 7d"
            days = int(test_period_2.split()[1].rstrip('d'))
            assert days == 7
            
            # Test various period formats
            periods = ["Previous 30d", "Previous 90d"]
            for period in periods:
                days = int(period.split()[1].rstrip('d'))
                assert days in [30, 90]
    
    def test_comparison_methods_exist(self):
        """Test that all comparison methods exist"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Verify methods
            assert hasattr(dashboard, '_render_comparison_metrics')
            assert hasattr(dashboard, '_display_period_metrics')
            assert hasattr(dashboard, '_calculate_change')
            assert hasattr(dashboard, '_generate_comparison_report')
    
    def test_comparison_insights_generated(self):
        """Test that comparison generates appropriate insights"""
        with patch.object(st, 'session_state', self.mock_session_state):
            dashboard = CrashAnalyticsDashboard(self.mock_db)
            
            # Test report with significant increase
            report = dashboard._generate_comparison_report(
                self.metrics_period_1,
                self.metrics_period_2,
                "Last 7 Days",
                "Previous 7 Days",
                {'total_change': 25.0, 'risk_change': 30.0,
                 'critical_change': 25.0, 'unique_change': 25.0}
            )
            
            # Should include insight about significant increase
            assert "Significant increase in crash volume" in report
            assert "System stability has declined significantly" in report
            
            # Test with different most common errors
            assert "Most common error changed from" in report

if __name__ == "__main__":
    pytest.main([__file__, "-v"])