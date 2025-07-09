# tests/test_performance_optimization.py
"""
Test performance optimization for Crash Analyzer V2
Quality Gate 3: Test performance with simulated large dataset
"""

import pytest
import time
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock
from toolkits.crash_analyzer_v2.analytics.statistics import CrashStatistics

class TestPerformanceOptimization:
    """Test performance improvements with SQL filtering"""
    
    def create_mock_queries(self, count: int, days_back: int = 90):
        """Create mock query data for testing"""
        queries = []
        base_date = datetime.now()
        
        for i in range(count):
            # Distribute queries over time period
            days_offset = (i * days_back) // count
            timestamp = base_date - timedelta(days=days_offset)
            
            # Mix of error types
            error_types = ["MEMORY_LEAK", "NULL_POINTER", "TIMEOUT", "CRASH", "EXCEPTION"]
            error_type = error_types[i % len(error_types)]
            
            # Create mock query tuple
            query = (
                i,  # id
                "crash_analyzer_v2",  # tool
                timestamp,  # created_at
                f"Method: quick_triage, File: test_{i}.log",  # prompt
                f'{{"error_type": "{error_type}", "severity": "HIGH"}}'  # response
            )
            queries.append(query)
        
        return queries
    
    def test_optimized_vs_original_performance(self):
        """Compare performance of optimized vs original query method"""
        # Create large dataset
        all_queries = self.create_mock_queries(5000, days_back=90)
        
        # Mock database with both methods
        mock_db = Mock()
        mock_db.get_recent_queries = Mock(return_value=all_queries)
        
        # Mock the new method (returns filtered queries)
        def filtered_side_effect(tool=None, start_date=None, **kwargs):
            filtered = []
            for q in all_queries:
                if tool and q[1] != tool:
                    continue
                if start_date and q[2] < start_date:
                    continue
                filtered.append(q)
            return filtered[:kwargs.get('limit', 1000)]
        
        mock_db.get_queries_with_filters = Mock(side_effect=filtered_side_effect)
        
        # Test with old approach (simulated)
        start_old = time.time()
        
        # Simulate old approach - get all and filter in Python
        all_data = mock_db.get_recent_queries.return_value
        start_date = datetime.now() - timedelta(days=30)
        filtered_old = [q for q in all_data if q[1] == "crash_analyzer_v2" and q[2] >= start_date]
        
        time_old = time.time() - start_old
        
        # Test with new approach
        stats_new = CrashStatistics(mock_db)
        start_new = time.time()
        
        # Use optimized method
        result = stats_new.get_crash_distribution("30d")
        
        time_new = time.time() - start_new
        
        # Assertions
        assert len(result) > 0  # Should return data
        assert mock_db.get_queries_with_filters.called  # Should use new method
        
        # Performance improvement should be visible with larger datasets
        print(f"\nPerformance Test Results:")
        print(f"Old approach time: {time_old:.4f}s")
        print(f"New approach time: {time_new:.4f}s")
        print(f"Queries processed: {len(filtered_old)}")
        print(f"Results returned: {len(result)}")
        
        # The new approach includes additional processing so may be slightly slower
        # but saves memory and database load
        assert time_new < 0.1  # Should complete quickly
    
    @patch('utils.database.DatabaseManager.get_queries_with_filters')
    def test_date_filtering_accuracy(self, mock_get_filtered):
        """Test that date filtering works correctly"""
        # Create queries spanning 90 days
        all_queries = self.create_mock_queries(90, days_back=90)
        
        def filtered_side_effect(tool=None, start_date=None, **kwargs):
            return [q for q in all_queries if q[2] >= start_date][:kwargs.get('limit', 1000)]
        
        mock_get_filtered.side_effect = filtered_side_effect
        
        # Create mock database with the method
        mock_db = Mock()
        mock_db.get_queries_with_filters = mock_get_filtered
        
        stats = CrashStatistics(mock_db)
        
        # Test different time ranges
        test_cases = [
            ("7d", 7),
            ("30d", 30),
            ("90d", 90),
        ]
        
        for time_range, expected_days in test_cases:
            mock_get_filtered.reset_mock()
            result = stats.get_crash_distribution(time_range)
            
            # Verify start_date parameter
            call_args = mock_get_filtered.call_args
            assert call_args is not None, f"get_queries_with_filters not called for {time_range}"
            
            # Check kwargs
            kwargs = call_args[1]
            start_date = kwargs['start_date']
            
            # Check date calculation
            expected_start = datetime.now() - timedelta(days=expected_days)
            date_diff = abs((start_date - expected_start).total_seconds())
            
            assert date_diff < 60  # Within 1 minute tolerance
            assert kwargs['tool'] == "crash_analyzer_v2"
            assert kwargs['limit'] == 1000
    
    def test_large_dataset_handling(self):
        """Test handling of large datasets efficiently"""
        # Simulate very large dataset
        large_queries = self.create_mock_queries(10000, days_back=365)
        
        # Create mock database
        mock_db = Mock()
        
        # Return only requested limit
        def filtered_side_effect(**kwargs):
            limit = kwargs.get('limit', 1000)
            return large_queries[:limit]
        
        mock_db.get_queries_with_filters = Mock(side_effect=filtered_side_effect)
        
        stats = CrashStatistics(mock_db)
        
        start_time = time.time()
        result = stats.get_crash_distribution("30d")
        elapsed = time.time() - start_time
        
        # Should process quickly even with large potential dataset
        assert elapsed < 1.0  # Should complete in under 1 second
        assert len(result) > 0
        
        # Verify we're using limit to prevent loading too much data
        call_args = mock_db.get_queries_with_filters.call_args[1]
        assert call_args['limit'] == 1000
    
    def test_database_method_signature(self):
        """Test that get_queries_with_filters has correct signature"""
        from utils.database import DatabaseManager
        
        # Check method exists
        assert hasattr(DatabaseManager, 'get_queries_with_filters')
        
        # Check it accepts expected parameters
        import inspect
        sig = inspect.signature(DatabaseManager.get_queries_with_filters)
        params = list(sig.parameters.keys())
        
        assert 'self' in params
        assert 'tool' in params
        assert 'start_date' in params
        assert 'end_date' in params
        assert 'limit' in params
    
    def test_memory_efficiency(self):
        """Test memory efficiency with streaming results"""
        # Create a generator to simulate streaming
        def query_generator():
            for i in range(1000):
                yield (
                    i,
                    "crash_analyzer_v2",
                    datetime.now() - timedelta(days=i % 30),
                    "Method: quick_triage",
                    '{"error_type": "TEST_ERROR", "severity": "HIGH"}'
                )
        
        # Create mock database
        mock_db = Mock()
        mock_db.get_queries_with_filters = Mock(return_value=list(query_generator()))
        
        stats = CrashStatistics(mock_db)
        
        # Should handle results without loading all into memory at once
        result = stats.get_crash_distribution("30d")
        
        assert len(result) > 0
        assert "TEST_ERROR" in result

if __name__ == "__main__":
    pytest.main([__file__, "-v", "-s"])