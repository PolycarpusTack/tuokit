# toolkits/crash_analyzer_v2/analytics/predictor.py
"""
Basic Predictive Analytics for Crash Analyzer V2
Simple statistical methods only - no ML libraries
"""

import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional

from utils.database import DatabaseManager

class CrashPredictor:
    """
    Basic crash prediction using simple statistical methods
    No sklearn, no complex models - just numpy and pandas
    """
    
    def __init__(self, db: DatabaseManager):
        self.db = db
        self.min_data_points = 7  # Need at least 7 days for prediction
    
    def predict_moving_average(self, days_ahead: int = 7, window_size: int = 7) -> Dict:
        """
        Use moving average to predict future crashes
        
        Args:
            days_ahead: Number of days to predict (max 7)
            window_size: Moving average window size
            
        Returns:
            Dict with predictions and confidence intervals
        """
        # Limit prediction to 7 days
        days_ahead = min(days_ahead, 7)
        
        # Get historical data (need more than window size)
        historical_days = max(30, window_size * 3)
        start_date = datetime.now() - timedelta(days=historical_days)
        
        # Get time series data
        time_series = self._get_time_series_data(start_date)
        
        # Check if we have enough data points with actual crashes
        non_zero_days = sum(1 for count in time_series['counts'] if count > 0)
        
        # Need actual crash data, not just empty days
        if non_zero_days < self.min_data_points:
            return {
                "status": "insufficient_data",
                "message": f"Need at least {self.min_data_points} days with crashes, got {non_zero_days}",
                "predictions": []
            }
        
        # Convert to pandas series
        dates = pd.to_datetime(time_series['dates'])
        values = pd.Series(time_series['counts'], index=dates)
        
        # Calculate moving average
        ma = values.rolling(window=window_size, min_periods=1).mean()
        
        # Calculate trend from recent data
        recent_days = min(14, len(values))
        recent_values = values[-recent_days:].values
        trend_slope = self._calculate_simple_slope(recent_values)
        
        # Generate predictions
        predictions = []
        last_ma = ma.iloc[-1]
        last_date = dates[-1]
        
        # Calculate standard deviation for confidence intervals
        std_dev = values[-window_size:].std() if len(values) >= window_size else values.std()
        
        for i in range(1, days_ahead + 1):
            pred_date = last_date + timedelta(days=i)
            
            # Predict using MA + trend
            pred_value = last_ma + (trend_slope * i)
            pred_value = max(0, pred_value)  # Can't have negative crashes
            
            # Confidence interval widens with time
            confidence_multiplier = 1 + (i * 0.1)  # 10% wider per day
            lower_bound = max(0, pred_value - (std_dev * confidence_multiplier))
            upper_bound = pred_value + (std_dev * confidence_multiplier)
            
            predictions.append({
                "date": pred_date.strftime("%Y-%m-%d"),
                "predicted": round(pred_value, 1),
                "lower_bound": round(lower_bound, 1),
                "upper_bound": round(upper_bound, 1),
                "confidence": round(100 - (i * 5), 0)  # Confidence decreases 5% per day
            })
        
        return {
            "status": "success",
            "method": "moving_average",
            "window_size": window_size,
            "predictions": predictions,
            "historical_mean": round(values.mean(), 1),
            "historical_std": round(std_dev, 1),
            "trend": "increasing" if trend_slope > 0 else "decreasing"
        }
    
    def calculate_trend_line(self, days_back: int = 30) -> Dict:
        """
        Calculate simple linear regression trend line
        
        Returns:
            Dict with slope, intercept, and R-squared
        """
        start_date = datetime.now() - timedelta(days=days_back)
        time_series = self._get_time_series_data(start_date)
        
        if len(time_series) < 2:
            return {
                "status": "insufficient_data",
                "slope": 0,
                "intercept": 0,
                "r_squared": 0
            }
        
        # Prepare data
        x = np.arange(len(time_series['counts']))
        y = np.array(time_series['counts'])
        
        # Calculate linear regression manually
        n = len(x)
        x_mean = x.mean()
        y_mean = y.mean()
        
        # Calculate slope and intercept
        numerator = np.sum((x - x_mean) * (y - y_mean))
        denominator = np.sum((x - x_mean) ** 2)
        
        if denominator == 0 or np.abs(numerator) < 1e-10:
            return {
                "status": "no_variance",
                "slope": 0.0,
                "intercept": y_mean,
                "r_squared": 0.0
            }
        
        slope = numerator / denominator
        intercept = y_mean - slope * x_mean
        
        # Calculate R-squared
        y_pred = slope * x + intercept
        ss_res = np.sum((y - y_pred) ** 2)
        ss_tot = np.sum((y - y_mean) ** 2)
        r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
        
        # If all values are identical, force slope to 0
        if np.all(y == y[0]):
            slope = 0.0
            r_squared = 1.0  # Perfect fit for constant line
        
        return {
            "status": "success",
            "slope": round(slope, 3),
            "intercept": round(intercept, 1),
            "r_squared": round(r_squared, 3),
            "trend_per_day": round(slope, 1),
            "equation": f"y = {slope:.2f}x + {intercept:.1f}"
        }
    
    def get_confidence_interval(self, predictions: List[float], 
                              historical_std: float) -> List[Tuple[float, float]]:
        """
        Calculate confidence intervals for predictions
        
        Returns:
            List of (lower_bound, upper_bound) tuples
        """
        intervals = []
        
        for i, pred in enumerate(predictions):
            # Confidence interval widens with prediction distance
            multiplier = 1.96 * (1 + i * 0.1)  # 95% CI, widening 10% per step
            margin = historical_std * multiplier
            
            lower = max(0, pred - margin)
            upper = pred + margin
            intervals.append((round(lower, 1), round(upper, 1)))
        
        return intervals
    
    def get_prediction_summary(self, days_ahead: int = 7) -> Dict:
        """
        Get comprehensive prediction summary
        
        Returns:
            Dict with all prediction methods and visualizations
        """
        # Moving average prediction
        ma_prediction = self.predict_moving_average(days_ahead)
        
        # Trend line analysis
        trend_analysis = self.calculate_trend_line()
        
        # Get historical data for context
        historical = self._get_time_series_data(
            datetime.now() - timedelta(days=30)
        )
        
        # Calculate additional metrics
        if historical['counts']:
            recent_avg = np.mean(historical['counts'][-7:])
            overall_avg = np.mean(historical['counts'])
            volatility = np.std(historical['counts']) / overall_avg if overall_avg > 0 else 0
        else:
            recent_avg = 0
            overall_avg = 0
            volatility = 0
        
        return {
            "moving_average": ma_prediction,
            "trend_analysis": trend_analysis,
            "metrics": {
                "recent_average": round(recent_avg, 1),
                "overall_average": round(overall_avg, 1),
                "volatility": round(volatility * 100, 1),  # As percentage
                "data_points": len(historical['counts'])
            },
            "recommendation": self._generate_recommendation(
                ma_prediction, trend_analysis, volatility
            )
        }
    
    def _get_time_series_data(self, start_date: datetime) -> Dict:
        """Get crash counts grouped by day"""
        queries = self.db.get_queries_with_filters(
            tool="crash_analyzer_v2",
            start_date=start_date
        )
        
        # Always return a valid structure
        if queries is None:
            queries = []
        
        # Group by date
        daily_counts = {}
        for query in queries:
            # Handle different timestamp formats
            timestamp = query[2]
            if isinstance(timestamp, str):
                try:
                    timestamp = datetime.strptime(timestamp, "%Y-%m-%d %H:%M:%S")
                except:
                    continue
            date = timestamp.date()
            daily_counts[date] = daily_counts.get(date, 0) + 1
        
        # Fill missing dates with 0
        all_dates = []
        all_counts = []
        current_date = start_date.date()
        end_date = datetime.now().date()
        
        while current_date <= end_date:
            all_dates.append(current_date)
            all_counts.append(daily_counts.get(current_date, 0))
            current_date += timedelta(days=1)
        
        return {
            "dates": all_dates,
            "counts": all_counts
        }
    
    def _calculate_simple_slope(self, values: np.ndarray) -> float:
        """Calculate simple slope for trend"""
        if len(values) < 2:
            return 0.0
        
        x = np.arange(len(values))
        # Simple slope calculation
        slope = (values[-1] - values[0]) / (len(values) - 1)
        return slope
    
    def _generate_recommendation(self, ma_pred: Dict, trend: Dict, 
                               volatility: float) -> str:
        """Generate actionable recommendation based on predictions"""
        recommendations = []
        
        # Check trend direction
        if trend.get('slope', 0) > 0.5:
            recommendations.append("🔴 Increasing crash trend detected - investigate root causes")
        elif trend.get('slope', 0) < -0.5:
            recommendations.append("🟢 Decreasing crash trend - current measures appear effective")
        
        # Check volatility
        if volatility > 0.5:  # 50% volatility
            recommendations.append("⚠️ High volatility in crash patterns - system may be unstable")
        
        # Check predictions
        if ma_pred.get('predictions'):
            next_week_avg = np.mean([p['predicted'] for p in ma_pred['predictions']])
            historical_mean = ma_pred.get('historical_mean', 0)
            
            if next_week_avg > historical_mean * 1.2:
                recommendations.append("📈 Predictions show potential spike - prepare response team")
            
        if not recommendations:
            recommendations.append("✅ Crash patterns appear stable - maintain current monitoring")
        
        return " | ".join(recommendations)