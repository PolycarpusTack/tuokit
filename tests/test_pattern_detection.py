# tests/test_pattern_detection.py
"""
Test Pattern Detection for Crash Analyzer V2
Quality Gate for Task 2
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import Mock, patch
import pandas as pd
import json

from toolkits.crash_analyzer_v2.analytics.patterns import CrashPatternDetector

class TestPatternDetection:
    """Test all pattern detection features"""
    
    def setup_method(self):
        """Set up test data"""
        self.mock_db = Mock()
        
        # Create test crash data with patterns
        now = datetime.now()
        self.test_queries = []
        
        # Create hourly pattern - ERROR_A happens mostly at 14:00
        for day in range(7):
            base_time = now - timedelta(days=day)
            # Peak hour crashes
            for i in range(5):
                timestamp = base_time.replace(hour=14, minute=i*10)
                self.test_queries.append((
                    len(self.test_queries),
                    "crash_analyzer_v2",
                    timestamp,
                    "Method: test, File: test.log",
                    '{"error_type": "ERROR_A", "severity": "HIGH"}'
                ))
            # Few crashes at other hours
            for hour in [9, 16, 20]:
                timestamp = base_time.replace(hour=hour, minute=0)
                self.test_queries.append((
                    len(self.test_queries),
                    "crash_analyzer_v2",
                    timestamp,
                    "Method: test, File: test.log",
                    '{"error_type": "ERROR_A", "severity": "HIGH"}'
                ))
        
        # Create weekly pattern - ERROR_B happens on Mondays
        for week in range(4):
            # Find next Monday
            monday = now - timedelta(days=now.weekday()) - timedelta(weeks=week)
            for i in range(10):
                timestamp = monday.replace(hour=10+i, minute=0)
                self.test_queries.append((
                    len(self.test_queries),
                    "crash_analyzer_v2",
                    timestamp,
                    "Method: test, File: test.log",
                    '{"error_type": "ERROR_B", "severity": "MEDIUM"}'
                ))
        
        # Create error sequences - ERROR_C followed by ERROR_D
        for i in range(5):
            base_time = now - timedelta(hours=i*24)
            # First error
            self.test_queries.append((
                len(self.test_queries),
                "crash_analyzer_v2",
                base_time,
                "Method: test, File: test.log",
                '{"error_type": "ERROR_C", "severity": "HIGH"}'
            ))
            # Following error within 5 minutes
            self.test_queries.append((
                len(self.test_queries),
                "crash_analyzer_v2",
                base_time + timedelta(minutes=3),
                "Method: test, File: test.log",
                '{"error_type": "ERROR_D", "severity": "CRITICAL"}'
            ))
        
        self.mock_db.get_queries_with_filters.return_value = self.test_queries
    
    def test_time_patterns_detected_hourly(self):
        """Test 1: Time patterns detected for hourly recurring crashes"""
        detector = CrashPatternDetector(self.mock_db)
        patterns = detector.detect_time_patterns("30d")
        
        # Check hourly patterns detected
        assert "hourly_patterns" in patterns
        assert "ERROR_A" in patterns["hourly_patterns"]
        
        # Verify peak hour detection
        error_a_pattern = patterns["hourly_patterns"]["ERROR_A"]
        assert error_a_pattern["peak_hour"] == 14
        assert error_a_pattern["concentration"] > 30  # Should be concentrated
    
    def test_weekly_patterns_identified(self):
        """Test 2: Weekly patterns identified correctly"""
        detector = CrashPatternDetector(self.mock_db)
        patterns = detector.detect_time_patterns("30d")
        
        # Check daily patterns detected
        assert "daily_patterns" in patterns
        assert "ERROR_B" in patterns["daily_patterns"]
        
        # Verify Monday detection
        error_b_pattern = patterns["daily_patterns"]["ERROR_B"]
        assert error_b_pattern["peak_day"] == "Monday"
        assert error_b_pattern["concentration"] > 25
    
    def test_error_sequences_found(self):
        """Test 3: Error sequences found within time windows"""
        detector = CrashPatternDetector(self.mock_db)
        sequences = detector.find_error_sequences(window_minutes=5)
        
        # Should find ERROR_C -> ERROR_D sequence
        assert len(sequences) > 0
        
        # Check sequence content
        found_sequence = False
        for seq in sequences:
            if seq['sequence'] == ['ERROR_C', 'ERROR_D']:
                found_sequence = True
                assert seq['count'] >= 5
                assert seq['window_minutes'] == 5
                break
        
        assert found_sequence, "ERROR_C -> ERROR_D sequence not found"
    
    def test_cascading_failures_detected(self):
        """Test 4: Cascading failures show cause->effect"""
        detector = CrashPatternDetector(self.mock_db)
        cascades = detector.detect_cascading_failures()
        
        # Should detect ERROR_C causes ERROR_D
        assert "ERROR_C" in cascades
        
        effects = cascades["ERROR_C"]
        assert len(effects) > 0
        
        # Check first effect
        assert effects[0]['effect'] == "ERROR_D"
        assert effects[0]['occurrences'] >= 5
    
    def test_performance_with_large_dataset(self):
        """Test 5: Performance: 1000 crashes analyzed in <2s"""
        # Create large dataset
        large_queries = []
        now = datetime.now()
        
        for i in range(1000):
            timestamp = now - timedelta(minutes=i)
            error_type = f"ERROR_{i % 10}"  # 10 different error types
            large_queries.append((
                i,
                "crash_analyzer_v2",
                timestamp,
                "Method: test, File: test.log",
                json.dumps({"error_type": error_type, "severity": "HIGH"})
            ))
        
        self.mock_db.get_queries_with_filters.return_value = large_queries
        
        detector = CrashPatternDetector(self.mock_db)
        
        # Measure performance
        import time
        start = time.time()
        patterns = detector.detect_time_patterns("30d")
        sequences = detector.find_error_sequences()
        cascades = detector.detect_cascading_failures()
        elapsed = time.time() - start
        
        # Should complete within 2 seconds
        assert elapsed < 2.0, f"Analysis took {elapsed:.2f}s, should be <2s"
        
        # Should still return results
        assert patterns is not None
        assert isinstance(sequences, list)
        assert isinstance(cascades, dict)
    
    def test_empty_data_returns_empty_patterns(self):
        """Test 6: Empty data returns empty patterns"""
        self.mock_db.get_queries_with_filters.return_value = []
        
        detector = CrashPatternDetector(self.mock_db)
        
        # Test all methods with empty data
        patterns = detector.detect_time_patterns()
        assert patterns["hourly_patterns"] == {}
        assert patterns["daily_patterns"] == {}
        assert patterns["pattern_strength"] == 0.0
        
        sequences = detector.find_error_sequences()
        assert sequences == []
        
        cascades = detector.detect_cascading_failures()
        assert cascades == {}
    
    def test_single_crash_returns_no_patterns(self):
        """Test 7: Single crash returns no patterns"""
        self.mock_db.get_queries_with_filters.return_value = [
            (1, "crash_analyzer_v2", datetime.now(), "Method: test", '{"error_type": "SINGLE"}')
        ]
        
        detector = CrashPatternDetector(self.mock_db)
        
        patterns = detector.detect_time_patterns()
        assert len(patterns["hourly_patterns"]) == 0
        
        sequences = detector.find_error_sequences()
        assert len(sequences) == 0
        
        cascades = detector.detect_cascading_failures()
        assert len(cascades) == 0
    
    def test_analyze_patterns_comprehensive(self):
        """Test comprehensive analysis method"""
        detector = CrashPatternDetector(self.mock_db)
        analysis = detector.analyze_patterns("30d")
        
        # Check all components present
        assert "time_patterns" in analysis
        assert "sequences" in analysis
        assert "cascades" in analysis
        assert "insights" in analysis
        assert "summary" in analysis
        
        # Check insights generated
        assert len(analysis["insights"]) > 0
        
        # Check summary metrics
        summary = analysis["summary"]
        assert "patterns_found" in summary
        assert "pattern_strength" in summary
        assert "has_cascading_failures" in summary
        assert "has_time_patterns" in summary
        
        # Verify insights have required fields
        for insight in analysis["insights"]:
            assert "type" in insight
            assert "severity" in insight
            assert "message" in insight
    
    def test_pattern_detector_inherits_correctly(self):
        """Test that pattern detector is properly structured"""
        detector = CrashPatternDetector(self.mock_db)
        
        # Check required methods exist
        assert hasattr(detector, 'detect_time_patterns')
        assert hasattr(detector, 'find_error_sequences')
        assert hasattr(detector, 'detect_cascading_failures')
        assert hasattr(detector, 'analyze_patterns')
        
        # Check it stays within line limit
        import inspect
        source = inspect.getsource(CrashPatternDetector)
        lines = len(source.split('\n'))
        assert lines <= 500, f"Pattern detector has {lines} lines, should be <= 500"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])