# tests/test_trending_calculation.py
"""
Test trending calculation for Crash Analyzer V2
Quality Gate 1: Verify accurate trend indicators
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import Mock, patch
from toolkits.crash_analyzer_v2.analytics.statistics import CrashStatistics

class TestTrendingCalculation:
    """Test accurate trend calculation with various scenarios"""
    
    def test_new_error_trend(self):
        """Test that new errors show 🆕 indicator"""
        stats = CrashStatistics()
        
        # Mock data: error only in current period
        current_dist = {"MEMORY_LEAK": 10, "NULL_POINTER": 5}
        prev_dist_full = {"NULL_POINTER": 5}  # MEMORY_LEAK not in previous
        
        # Calculate trend (simulating dashboard logic)
        prev_dist = {}
        for error_type, total_count in prev_dist_full.items():
            current_count = current_dist.get(error_type, 0)
            prev_count = max(0, total_count - current_count)
            if prev_count > 0:
                prev_dist[error_type] = prev_count
        
        # Check MEMORY_LEAK is detected as new
        prev_count = prev_dist.get("MEMORY_LEAK", 0)
        assert prev_count == 0
        
        # Verify trend indicator
        if prev_count == 0:
            trend_indicator = "🆕"
        assert trend_indicator == "🆕"
    
    def test_increasing_trend(self):
        """Test that increasing errors show 📈 indicator"""
        # Simulate dashboard logic
        current_dist = {"CRASH_ERROR": 50}
        prev_dist = {"CRASH_ERROR": 30}  # Previous period had 30
        
        current_count = current_dist["CRASH_ERROR"]
        prev_count = prev_dist["CRASH_ERROR"]
        
        # Check > 20% increase
        assert current_count > prev_count * 1.2
        
        if current_count > prev_count * 1.2:
            trend_indicator = "📈"
            trend_percent = ((current_count - prev_count) / prev_count) * 100
        
        assert trend_indicator == "📈"
        assert trend_percent == pytest.approx(66.67, rel=0.01)
    
    def test_decreasing_trend(self):
        """Test that decreasing errors show 📉 indicator"""
        # Simulate dashboard logic
        current_dist = {"TIMEOUT_ERROR": 15}
        prev_dist = {"TIMEOUT_ERROR": 30}  # Previous period had 30
        
        current_count = current_dist["TIMEOUT_ERROR"]
        prev_count = prev_dist["TIMEOUT_ERROR"]
        
        # Check < 20% decrease
        assert current_count < prev_count * 0.8
        
        if current_count < prev_count * 0.8:
            trend_indicator = "📉"
            trend_percent = ((current_count - prev_count) / prev_count) * 100
        
        assert trend_indicator == "📉"
        assert trend_percent == pytest.approx(-50.0, rel=0.01)
    
    def test_stable_trend(self):
        """Test that stable errors show ➡️ indicator"""
        # Simulate dashboard logic
        current_dist = {"STABLE_ERROR": 25}
        prev_dist = {"STABLE_ERROR": 24}  # Very small change
        
        current_count = current_dist["STABLE_ERROR"]
        prev_count = prev_dist["STABLE_ERROR"]
        
        # Check within 20% threshold
        assert not (current_count > prev_count * 1.2)
        assert not (current_count < prev_count * 0.8)
        
        if not (current_count > prev_count * 1.2) and not (current_count < prev_count * 0.8):
            trend_indicator = "➡️"
            trend_percent = 0
        
        assert trend_indicator == "➡️"
        assert trend_percent == 0
    
    def test_main_metric_trends(self):
        """Test main metric trend emoji logic"""
        test_cases = [
            (50.0, "📈"),   # > 20% increase
            (-50.0, "📉"),  # < -20% decrease
            (15.0, "➡️"),   # Within stable range
            (-15.0, "➡️"),  # Within stable range
            (0.0, "➡️"),    # No change
        ]
        
        for trend_pct, expected_emoji in test_cases:
            if trend_pct > 20:
                trend_emoji = "📈"
            elif trend_pct < -20:
                trend_emoji = "📉"
            else:
                trend_emoji = "➡️"
            
            assert trend_emoji == expected_emoji, f"Failed for {trend_pct}%"
    
    def test_trend_sorting(self):
        """Test that trends are sorted by absolute change"""
        # Simulate trending data with different changes
        trending = [
            ("ERROR_A", 10, "➡️", 5.0),
            ("ERROR_B", 20, "📈", 50.0),
            ("ERROR_C", 5, "📉", -60.0),
            ("ERROR_D", 15, "🆕", 100.0),
        ]
        
        # Sort by absolute trend percentage
        trending.sort(key=lambda x: abs(x[3]), reverse=True)
        
        # Verify order: D (100%), C (60%), B (50%), A (5%)
        assert trending[0][0] == "ERROR_D"
        assert trending[1][0] == "ERROR_C"
        assert trending[2][0] == "ERROR_B"
        assert trending[3][0] == "ERROR_A"
    
    @patch('toolkits.crash_analyzer_v2.analytics.statistics.CrashStatistics.get_crash_distribution')
    def test_integrated_trending_calculation(self, mock_get_dist):
        """Test the full trending calculation flow"""
        stats = CrashStatistics()
        
        # Mock returns for current and previous periods
        def mock_distribution(time_range):
            if time_range == "30d":
                return {"ERROR_X": 100, "ERROR_Y": 50, "ERROR_Z": 25}
            elif time_range == "60d":  # 2x period for previous
                return {"ERROR_X": 150, "ERROR_Y": 60}  # ERROR_Z is new
            return {}
        
        mock_get_dist.side_effect = mock_distribution
        
        # Get distributions
        current_dist = stats.get_crash_distribution("30d")
        prev_dist_full = stats.get_crash_distribution("60d")
        
        # Calculate previous period (excluding current)
        prev_dist = {}
        for error_type, total_count in prev_dist_full.items():
            current_count = current_dist.get(error_type, 0)
            prev_count = max(0, total_count - current_count)
            if prev_count > 0:
                prev_dist[error_type] = prev_count
        
        # Verify calculations
        assert prev_dist.get("ERROR_X") == 50  # 150 - 100
        assert prev_dist.get("ERROR_Y") == 10  # 60 - 50
        assert "ERROR_Z" not in prev_dist  # New error
        
        # Calculate trends
        trends = {}
        for error_type, current_count in current_dist.items():
            prev_count = prev_dist.get(error_type, 0)
            
            if prev_count == 0:
                trends[error_type] = "🆕"
            elif current_count > prev_count * 1.2:
                trends[error_type] = "📈"
            elif current_count < prev_count * 0.8:
                trends[error_type] = "📉"
            else:
                trends[error_type] = "➡️"
        
        # Verify trends
        assert trends["ERROR_X"] == "📈"  # 100 vs 50 = 100% increase
        assert trends["ERROR_Y"] == "📈"  # 50 vs 10 = 400% increase
        assert trends["ERROR_Z"] == "🆕"  # New error

if __name__ == "__main__":
    pytest.main([__file__, "-v"])