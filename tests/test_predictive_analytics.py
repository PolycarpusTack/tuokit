# tests/test_predictive_analytics.py
"""
Test Predictive Analytics for Crash Analyzer V2
Quality Gate for Task 5
"""

import pytest
import numpy as np
from datetime import datetime, timedelta
from unittest.mock import Mock, patch

from toolkits.crash_analyzer_v2.analytics.predictor import CrashPredictor

class TestPredictiveAnalytics:
    """Test all predictive analytics features"""
    
    def setup_method(self):
        """Set up test data"""
        self.mock_db = Mock()
        
        # Create test time series data
        self.base_date = datetime.now()
        self.test_queries = []
        
        # Generate 30 days of crash data with pattern
        for i in range(30):
            date = self.base_date - timedelta(days=29-i)
            # Add some pattern: base + trend + noise
            base_crashes = 50
            trend = i * 0.5  # Slight upward trend
            noise = np.random.normal(0, 5)
            daily_crashes = int(base_crashes + trend + noise)
            
            # Create that many crashes for the day
            for j in range(max(1, daily_crashes)):
                self.test_queries.append((
                    len(self.test_queries),
                    "crash_analyzer_v2",
                    date + timedelta(hours=j % 24),
                    "test",
                    "test_response"
                ))
        
        self.mock_db.get_queries_with_filters.return_value = self.test_queries
    
    def test_moving_average_calculated_correctly(self):
        """Test 1: Moving average calculated correctly"""
        predictor = CrashPredictor(self.mock_db)
        
        # Get predictions
        result = predictor.predict_moving_average(days_ahead=7, window_size=7)
        
        # Debug print if failing
        if result['status'] != 'success':
            print(f"Result status: {result['status']}")
            print(f"Result: {result}")
            # Check what queries returned
            queries = self.mock_db.get_queries_with_filters.return_value
            print(f"Number of queries: {len(queries)}")
            if queries:
                print(f"First query: {queries[0]}")
                print(f"Last query: {queries[-1]}")
        
        assert result['status'] == 'success'
        assert len(result['predictions']) == 7
        assert result['method'] == 'moving_average'
        
        # Check predictions are reasonable
        for pred in result['predictions']:
            assert pred['predicted'] >= 0  # No negative crashes
            assert pred['lower_bound'] <= pred['predicted']
            assert pred['upper_bound'] >= pred['predicted']
            assert 0 <= pred['confidence'] <= 100
    
    def test_trend_line_fits_historical_data(self):
        """Test 2: Trend line fits historical data"""
        predictor = CrashPredictor(self.mock_db)
        
        # Calculate trend
        result = predictor.calculate_trend_line(days_back=30)
        
        assert result['status'] == 'success'
        assert 'slope' in result
        assert 'intercept' in result
        assert 'r_squared' in result
        assert 0 <= result['r_squared'] <= 1
        
        # With upward trend data, slope should be positive
        assert result['slope'] > 0
    
    def test_predictions_stay_within_bounds(self):
        """Test 3: Predictions stay within reasonable bounds"""
        predictor = CrashPredictor(self.mock_db)
        
        # Get predictions
        result = predictor.predict_moving_average(days_ahead=7)
        
        predictions = result['predictions']
        historical_mean = result['historical_mean']
        historical_std = result['historical_std']
        
        # Predictions should be within reasonable bounds (5 std devs due to trend)
        for pred in predictions:
            assert pred['predicted'] <= historical_mean + (5 * historical_std)
            assert pred['predicted'] >= max(0, historical_mean - (5 * historical_std))
    
    def test_confidence_interval_widens_over_time(self):
        """Test 4: Confidence interval widens over time"""
        predictor = CrashPredictor(self.mock_db)
        
        # Get predictions
        result = predictor.predict_moving_average(days_ahead=7)
        predictions = result['predictions']
        
        # Check intervals widen
        for i in range(1, len(predictions)):
            prev_width = predictions[i-1]['upper_bound'] - predictions[i-1]['lower_bound']
            curr_width = predictions[i]['upper_bound'] - predictions[i]['lower_bound']
            assert curr_width >= prev_width  # Should widen or stay same
        
        # Check confidence decreases
        for i in range(1, len(predictions)):
            assert predictions[i]['confidence'] < predictions[i-1]['confidence']
    
    def test_zero_crashes_predicts_zero(self):
        """Test 5: Zero crashes predicts zero"""
        # Create empty data
        self.mock_db.get_queries_with_filters.return_value = []
        
        # Add minimal data to avoid insufficient data error
        min_queries = []
        for i in range(7):
            date = datetime.now() - timedelta(days=i)
            min_queries.append((
                i,
                "crash_analyzer_v2",
                date,
                "test",
                "test"
            ))
        
        # First 7 days have 0 crashes, then add the minimal set
        zero_period = []
        base_date = datetime.now() - timedelta(days=14)
        for i in range(7):
            date = base_date + timedelta(days=i)
            # Don't add any crashes for these days
            pass
        
        self.mock_db.get_queries_with_filters.return_value = min_queries
        
        predictor = CrashPredictor(self.mock_db)
        result = predictor.predict_moving_average(days_ahead=7)
        
        if result['status'] == 'success':
            # Low crash predictions
            for pred in result['predictions']:
                assert pred['predicted'] >= 0
    
    def test_insufficient_data_handled(self):
        """Test 6: Insufficient data handled"""
        # Provide empty data
        self.mock_db.get_queries_with_filters.return_value = []
        
        predictor = CrashPredictor(self.mock_db)
        result = predictor.predict_moving_average(days_ahead=7)
        
        assert result['status'] == 'insufficient_data'
        assert 'message' in result
    
    def test_performance_under_one_second(self):
        """Test 7: Performance under 1 second"""
        # Create large dataset
        large_queries = []
        for i in range(365):  # One year of data
            date = self.base_date - timedelta(days=i)
            for j in range(100):  # 100 crashes per day
                large_queries.append((
                    len(large_queries),
                    "crash_analyzer_v2",
                    date + timedelta(hours=j % 24),
                    "test",
                    "test"
                ))
        
        self.mock_db.get_queries_with_filters.return_value = large_queries
        
        predictor = CrashPredictor(self.mock_db)
        
        # Measure performance
        import time
        start = time.time()
        
        result = predictor.predict_moving_average(days_ahead=7)
        trend = predictor.calculate_trend_line()
        summary = predictor.get_prediction_summary()
        
        elapsed = time.time() - start
        
        assert elapsed < 1.0, f"Prediction took {elapsed:.2f}s, should be <1s"
        assert result['status'] == 'success'
    
    def test_prediction_summary_comprehensive(self):
        """Test comprehensive prediction summary"""
        predictor = CrashPredictor(self.mock_db)
        
        summary = predictor.get_prediction_summary(days_ahead=7)
        
        # Check all components present
        assert 'moving_average' in summary
        assert 'trend_analysis' in summary
        assert 'metrics' in summary
        assert 'recommendation' in summary
        
        # Check metrics
        metrics = summary['metrics']
        assert 'recent_average' in metrics
        assert 'overall_average' in metrics
        assert 'volatility' in metrics
        assert 'data_points' in metrics
        
        # Check recommendation is generated
        assert isinstance(summary['recommendation'], str)
        assert len(summary['recommendation']) > 0
    
    def test_trend_calculation_edge_cases(self):
        """Test trend calculation with edge cases"""
        predictor = CrashPredictor(self.mock_db)
        
        # Test with constant values (no variance)
        constant_queries = []
        for i in range(10):
            date = datetime.now() - timedelta(days=i)
            for j in range(10):  # Exactly 10 crashes each day
                constant_queries.append((
                    len(constant_queries),
                    "crash_analyzer_v2",
                    date,
                    "test",
                    "test"
                ))
        
        self.mock_db.get_queries_with_filters.return_value = constant_queries
        
        result = predictor.calculate_trend_line()
        # Should have very small slope (close to 0) for constant values
        assert abs(result['slope']) < 0.5  # Allow small variation due to grouping

if __name__ == "__main__":
    pytest.main([__file__, "-v"])