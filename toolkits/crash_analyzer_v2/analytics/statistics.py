# toolkits/crash_analyzer_v2/analytics/statistics.py
"""
Statistical analysis engine for crash data
"""

import streamlit as st
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import json
from collections import Counter, defaultdict

from utils.tool_base import TuoKitToolBase
from utils.database import DatabaseManager

class CrashStatistics(TuoKitToolBase):
    """
    Statistical analysis engine for crash patterns
    Following TuoKit principles: simple, effective, no over-engineering
    """
    
    def __init__(self, db: Optional[DatabaseManager] = None):
        super().__init__(
            tool_name="Crash Statistics",
            tool_description="Statistical analysis of crash patterns"
        )
        self.db = db or st.session_state.get('db')
    
    def get_crash_distribution(self, time_range: str = "30d") -> Dict[str, int]:
        """
        Get crash frequency by error type
        
        Args:
            time_range: Time range string (e.g., "7d", "30d", "90d")
            
        Returns:
            Dict mapping error types to counts
        """
        if not self.db:
            return {}
        
        # Parse time range
        days = int(time_range.rstrip('d'))
        start_date = datetime.now() - timedelta(days=days)
        
        try:
            # Use optimized query with date filtering
            queries = self.db.get_queries_with_filters(
                tool="crash_analyzer_v2",
                start_date=start_date,
                limit=1000
            )
            
            # Count by error type
            error_counts = Counter()
            
            for query in queries:
                # Extract error type from response (column 4)
                response = query[4]
                error_type = self._extract_error_type(response)
                if error_type:
                    error_counts[error_type] += 1
            
            return dict(error_counts)
            
        except Exception as e:
            st.error(f"Error getting crash distribution: {e}")
            return {}
    
    def get_severity_metrics(self, time_range: str = "30d") -> Dict[str, int]:
        """
        Get severity distribution of crashes
        
        Returns:
            Dict mapping severity levels to counts
        """
        if not self.db:
            return {}
        
        days = int(time_range.rstrip('d'))
        start_date = datetime.now() - timedelta(days=days)
        
        try:
            # Use optimized query with date filtering
            queries = self.db.get_queries_with_filters(
                tool="crash_analyzer_v2",
                start_date=start_date,
                limit=1000
            )
            
            severity_counts = Counter()
            
            for query in queries:
                severity = self._extract_severity(query[4])
                if severity:
                    severity_counts[severity] += 1
            
            return dict(severity_counts)
            
        except Exception as e:
            st.error(f"Error getting severity metrics: {e}")
            return {}
    
    def get_time_series_data(self, time_range: str = "30d", 
                           group_by: str = "day") -> Dict[str, List[int]]:
        """
        Get crash counts over time
        
        Args:
            time_range: Time range string
            group_by: Grouping period ("day", "week")
            
        Returns:
            Dict with dates and counts arrays
        """
        if not self.db:
            return {"dates": [], "counts": []}
        
        days = int(time_range.rstrip('d'))
        start_date = datetime.now() - timedelta(days=days)
        
        try:
            # Use optimized query with date filtering
            queries = self.db.get_queries_with_filters(
                tool="crash_analyzer_v2",
                start_date=start_date,
                limit=1000
            )
            
            # Group by date
            daily_counts = defaultdict(int)
            
            for query in queries:
                timestamp = query[2]  # created_at is already a datetime
                
                if group_by == "day":
                    date_key = timestamp.date()
                else:  # week
                    date_key = timestamp.date() - timedelta(days=timestamp.weekday())
                
                daily_counts[date_key] += 1
            
            # Sort by date and fill gaps
            if daily_counts:
                min_date = min(daily_counts.keys())
                max_date = max(daily_counts.keys())
                
                dates = []
                counts = []
                
                current_date = min_date
                while current_date <= max_date:
                    dates.append(current_date.strftime("%Y-%m-%d"))
                    counts.append(daily_counts.get(current_date, 0))
                    
                    if group_by == "day":
                        current_date += timedelta(days=1)
                    else:
                        current_date += timedelta(weeks=1)
                
                return {"dates": dates, "counts": counts}
            
            return {"dates": [], "counts": []}
            
        except Exception as e:
            st.error(f"Error getting time series data: {e}")
            return {"dates": [], "counts": []}
    
    def get_analysis_method_stats(self, time_range: str = "30d") -> Dict[str, int]:
        """
        Get usage statistics by analysis method
        """
        if not self.db:
            return {}
        
        days = int(time_range.rstrip('d'))
        start_date = datetime.now() - timedelta(days=days)
        
        try:
            # Use optimized query with date filtering
            queries = self.db.get_queries_with_filters(
                tool="crash_analyzer_v2",
                start_date=start_date,
                limit=1000
            )
            
            method_counts = Counter()
            
            for query in queries:
                # Extract method from prompt (column 3)
                prompt = query[3]
                if "Method:" in prompt:
                    method = prompt.split("Method:")[1].split(",")[0].strip()
                    method_counts[method] += 1
            
            return dict(method_counts)
            
        except Exception as e:
            st.error(f"Error getting method stats: {e}")
            return {}
    
    def get_top_issues(self, limit: int = 5, time_range: str = "30d") -> List[Tuple[str, int]]:
        """
        Get top crash issues by frequency
        
        Returns:
            List of (error_type, count) tuples
        """
        distribution = self.get_crash_distribution(time_range)
        return sorted(distribution.items(), key=lambda x: x[1], reverse=True)[:limit]
    
    def calculate_metrics_summary(self, time_range: str = "30d") -> Dict[str, any]:
        """
        Calculate summary metrics for dashboard
        
        Returns:
            Dict with key metrics
        """
        # Get current period data
        current_distribution = self.get_crash_distribution(time_range)
        current_total = sum(current_distribution.values())
        
        # Get previous period for comparison
        days = int(time_range.rstrip('d'))
        prev_time_range = f"{days * 2}d"
        prev_distribution = self.get_crash_distribution(prev_time_range)
        
        # Calculate previous period counts (excluding current period)
        prev_counts = {}
        for error_type, total_count in prev_distribution.items():
            current_count = current_distribution.get(error_type, 0)
            prev_counts[error_type] = max(0, total_count - current_count)
        
        prev_total = sum(prev_counts.values())
        
        # Calculate trend
        if prev_total > 0:
            trend = ((current_total - prev_total) / prev_total) * 100
        else:
            trend = 100 if current_total > 0 else 0
        
        # Get severity breakdown
        severity_metrics = self.get_severity_metrics(time_range)
        critical_count = severity_metrics.get("CRITICAL", 0) + severity_metrics.get("HIGH", 0)
        
        # Calculate risk score using weighted factors
        # Factors: severity distribution, trend, volume, error diversity
        risk_score = 0.0
        
        if current_total > 0:
            # Factor 1: Critical/High severity percentage (40% weight)
            critical_percentage = (critical_count / current_total) * 100
            severity_score = min(10, critical_percentage / 10)  # 100% critical = 10
            risk_score += severity_score * 0.4
            
            # Factor 2: Trend impact (30% weight)
            if trend > 50:  # Rapid increase
                trend_score = min(10, trend / 20)  # 200% increase = 10
            elif trend < -30:  # Significant decrease
                trend_score = 0  # Decreasing is good
            else:
                trend_score = 3 + (trend / 50) * 2  # Stable = 3-5
            risk_score += trend_score * 0.3
            
            # Factor 3: Volume impact (20% weight)
            # More crashes = higher risk, normalized by time period
            daily_average = current_total / days
            if daily_average > 100:
                volume_score = 10
            elif daily_average > 50:
                volume_score = 8
            elif daily_average > 20:
                volume_score = 6
            elif daily_average > 10:
                volume_score = 4
            else:
                volume_score = 2
            risk_score += volume_score * 0.2
            
            # Factor 4: Error diversity (10% weight)
            # Many different errors = system instability
            unique_percentage = (len(current_distribution) / current_total) * 100
            if unique_percentage > 50:  # Many unique errors
                diversity_score = 8
            elif unique_percentage > 20:
                diversity_score = 5
            else:
                diversity_score = 2
            risk_score += diversity_score * 0.1
            
            # Ensure score is between 0 and 10
            risk_score = max(0, min(10, risk_score))
        else:
            risk_score = 0
        
        return {
            "total_crashes": current_total,
            "trend_percentage": round(trend, 1),
            "critical_issues": critical_count,
            "risk_score": round(risk_score, 1),
            "unique_error_types": len(current_distribution),
            "most_common_error": max(current_distribution.items(), key=lambda x: x[1])[0] if current_distribution else "None"
        }
    
    def _extract_error_type(self, response: str) -> Optional[str]:
        """Extract error type from analysis response"""
        try:
            if response.startswith("{"):
                # JSON response
                data = json.loads(response)
                return data.get("error_type", "Unknown")
            else:
                # Text response - look for patterns
                if "Error Type:" in response:
                    return response.split("Error Type:")[1].split("\n")[0].strip()
                return "Unknown"
        except:
            return "Unknown"
    
    def _extract_severity(self, response: str) -> Optional[str]:
        """Extract severity from analysis response"""
        try:
            if response.startswith("{"):
                # JSON response
                data = json.loads(response)
                return data.get("severity", "MEDIUM").upper()
            else:
                # Text response - look for patterns
                if "Severity:" in response:
                    return response.split("Severity:")[1].split("\n")[0].strip().upper()
                return "MEDIUM"
        except:
            return "MEDIUM"