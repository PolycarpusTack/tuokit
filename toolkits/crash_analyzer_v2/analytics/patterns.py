# toolkits/crash_analyzer_v2/analytics/patterns.py
"""
Crash Pattern Detection for Analytics
Identifies recurring patterns, sequences, and cascading failures
"""

import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
from collections import defaultdict, Counter
import json

from utils.database import DatabaseManager

class CrashPatternDetector:
    """
    Detects patterns in crash data using statistical analysis
    No ML libraries - just pandas and standard Python
    """
    
    def __init__(self, db: DatabaseManager):
        self.db = db
    
    def detect_time_patterns(self, time_range: str = "30d") -> Dict:
        """
        Detect time-based patterns (hourly, daily, weekly)
        
        Returns:
            Dict with pattern types and their strength scores
        """
        # Get crash data
        days = int(time_range.rstrip('d'))
        start_date = datetime.now() - timedelta(days=days)
        
        queries = self.db.get_queries_with_filters(
            tool="crash_analyzer_v2",
            start_date=start_date,
            limit=1000
        )
        
        if not queries:
            return {
                "hourly_patterns": {},
                "daily_patterns": {},
                "weekly_patterns": {},
                "pattern_strength": 0.0
            }
        
        # Extract timestamps and error types
        crash_data = []
        for query in queries:
            timestamp = query[2]  # Already datetime
            response = query[4]
            error_type = self._extract_error_type(response)
            crash_data.append((timestamp, error_type))
        
        # Create DataFrame for analysis
        df = pd.DataFrame(crash_data, columns=['timestamp', 'error_type'])
        df['hour'] = df['timestamp'].dt.hour
        df['weekday'] = df['timestamp'].dt.dayofweek
        df['day_name'] = df['timestamp'].dt.day_name()
        
        # Hourly patterns
        hourly_counts = df.groupby(['hour', 'error_type']).size().reset_index(name='count')
        hourly_patterns = {}
        
        for error_type in df['error_type'].unique():
            error_hourly = hourly_counts[hourly_counts['error_type'] == error_type]
            if len(error_hourly) > 0:
                # Find peak hours
                peak_hour = error_hourly.loc[error_hourly['count'].idxmax()]
                total_for_error = error_hourly['count'].sum()
                concentration = (peak_hour['count'] / total_for_error) * 100 if total_for_error > 0 else 0
                
                # Need multiple occurrences to be a pattern
                if concentration > 20 and total_for_error > 2:  # Significant if >20% occur in one hour AND more than 2 total
                    hourly_patterns[error_type] = {
                        'peak_hour': int(peak_hour['hour']),
                        'concentration': round(concentration, 1),
                        'count': int(peak_hour['count'])
                    }
        
        # Daily patterns (by day of week)
        daily_counts = df.groupby(['weekday', 'day_name', 'error_type']).size().reset_index(name='count')
        daily_patterns = {}
        
        for error_type in df['error_type'].unique():
            error_daily = daily_counts[daily_counts['error_type'] == error_type]
            if len(error_daily) > 0:
                peak_day = error_daily.loc[error_daily['count'].idxmax()]
                total_for_error = error_daily['count'].sum()
                concentration = (peak_day['count'] / total_for_error) * 100 if total_for_error > 0 else 0
                
                # Need multiple occurrences to be a pattern
                if concentration > 25 and total_for_error > 2:  # Significant if >25% occur on one day AND more than 2 total
                    daily_patterns[error_type] = {
                        'peak_day': peak_day['day_name'],
                        'concentration': round(concentration, 1),
                        'count': int(peak_day['count'])
                    }
        
        # Weekly patterns (detect if crashes increase over weekends, etc.)
        weekend_mask = df['weekday'].isin([5, 6])  # Saturday, Sunday
        weekend_ratio = len(df[weekend_mask]) / len(df) if len(df) > 0 else 0
        
        weekly_patterns = {
            'weekend_concentration': round(weekend_ratio * 100, 1),
            'is_weekend_heavy': weekend_ratio > 0.4  # >40% on weekends is significant
        }
        
        # Calculate overall pattern strength
        pattern_strength = 0.0
        if hourly_patterns:
            pattern_strength += 0.4
        if daily_patterns:
            pattern_strength += 0.3
        if weekly_patterns['is_weekend_heavy']:
            pattern_strength += 0.3
        
        return {
            "hourly_patterns": hourly_patterns,
            "daily_patterns": daily_patterns,
            "weekly_patterns": weekly_patterns,
            "pattern_strength": round(pattern_strength, 2)
        }
    
    def find_error_sequences(self, window_minutes: int = 5) -> List[Dict]:
        """
        Find errors that frequently occur together within time windows
        
        Args:
            window_minutes: Time window to consider errors as sequential
            
        Returns:
            List of sequence patterns with confidence scores
        """
        # Get recent crash data
        queries = self.db.get_queries_with_filters(
            tool="crash_analyzer_v2",
            start_date=datetime.now() - timedelta(days=30),
            limit=1000
        )
        
        if len(queries) < 2:
            return []
        
        # Sort by timestamp
        sorted_crashes = sorted(queries, key=lambda x: x[2])
        
        # Find sequences
        sequences = []
        window_delta = timedelta(minutes=window_minutes)
        
        for i in range(len(sorted_crashes) - 1):
            current = sorted_crashes[i]
            current_time = current[2]
            current_error = self._extract_error_type(current[4])
            
            # Look for following errors within window
            sequence = [current_error]
            j = i + 1
            
            while j < len(sorted_crashes):
                next_crash = sorted_crashes[j]
                next_time = next_crash[2]
                
                if next_time - current_time <= window_delta:
                    next_error = self._extract_error_type(next_crash[4])
                    if next_error != current_error:  # Different error type
                        sequence.append(next_error)
                    current_time = next_time
                else:
                    break
                j += 1
            
            if len(sequence) > 1:
                sequences.append(tuple(sequence))
        
        # Count sequence frequencies
        sequence_counts = Counter(sequences)
        
        # Calculate confidence scores
        results = []
        total_crashes = len(sorted_crashes)
        
        for sequence, count in sequence_counts.most_common(10):
            if count >= 2:  # At least 2 occurrences
                confidence = (count / total_crashes) * 100
                results.append({
                    'sequence': list(sequence),
                    'count': count,
                    'confidence': round(confidence, 1),
                    'window_minutes': window_minutes
                })
        
        return results
    
    def detect_cascading_failures(self) -> Dict[str, List[Dict]]:
        """
        Analyze error chains to find root cause -> effect relationships
        
        Returns:
            Dict mapping root causes to their cascading effects
        """
        sequences = self.find_error_sequences(window_minutes=10)
        
        if not sequences:
            return {}
        
        # Build cause-effect relationships
        cascades = defaultdict(list)
        
        for seq_data in sequences:
            sequence = seq_data['sequence']
            if len(sequence) >= 2:
                # First error is potential root cause
                root = sequence[0]
                
                # Track what follows
                for i in range(1, len(sequence)):
                    effect = sequence[i]
                    
                    # Check if this cascade already exists
                    existing = next(
                        (item for item in cascades[root] if item['effect'] == effect),
                        None
                    )
                    
                    if existing:
                        existing['occurrences'] += seq_data['count']
                    else:
                        cascades[root].append({
                            'effect': effect,
                            'occurrences': seq_data['count'],
                            'typical_delay': f"{seq_data['window_minutes']} min",
                            'confidence': seq_data['confidence']
                        })
        
        # Sort effects by occurrence
        for root in cascades:
            cascades[root].sort(key=lambda x: x['occurrences'], reverse=True)
        
        return dict(cascades)
    
    def analyze_patterns(self, time_range: str = "30d") -> Dict:
        """
        Comprehensive pattern analysis combining all detection methods
        
        Returns:
            Complete pattern analysis report
        """
        # Run all analyses
        time_patterns = self.detect_time_patterns(time_range)
        sequences = self.find_error_sequences()
        cascades = self.detect_cascading_failures()
        
        # Calculate insights
        insights = []
        
        # Time pattern insights
        if time_patterns['hourly_patterns']:
            for error, pattern in time_patterns['hourly_patterns'].items():
                insights.append({
                    'type': 'time_pattern',
                    'severity': 'medium',
                    'message': f"{error} peaks at {pattern['peak_hour']}:00 ({pattern['concentration']}% of occurrences)"
                })
        
        if time_patterns.get('weekly_patterns', {}).get('is_weekend_heavy', False):
            insights.append({
                'type': 'time_pattern',
                'severity': 'high',
                'message': f"Weekend crash rate is {time_patterns['weekly_patterns']['weekend_concentration']}% - investigate weekend-specific issues"
            })
        
        # Sequence insights
        if sequences:
            top_sequence = sequences[0]
            insights.append({
                'type': 'sequence',
                'severity': 'high',
                'message': f"Frequent error sequence: {' → '.join(top_sequence['sequence'])} (occurs {top_sequence['count']} times)"
            })
        
        # Cascade insights
        if cascades:
            for root, effects in list(cascades.items())[:3]:  # Top 3 root causes
                if effects:
                    top_effect = effects[0]
                    insights.append({
                        'type': 'cascade',
                        'severity': 'critical',
                        'message': f"{root} causes {top_effect['effect']} in {top_effect['occurrences']} cases"
                    })
        
        return {
            'time_patterns': time_patterns,
            'sequences': sequences,
            'cascades': cascades,
            'insights': insights,
            'summary': {
                'patterns_found': len(insights),
                'pattern_strength': time_patterns['pattern_strength'],
                'has_cascading_failures': len(cascades) > 0,
                'has_time_patterns': time_patterns['pattern_strength'] > 0
            }
        }
    
    def _extract_error_type(self, response: str) -> str:
        """Extract error type from analysis response"""
        try:
            if response.startswith("{"):
                data = json.loads(response)
                return data.get("error_type", "Unknown")
            else:
                if "Error Type:" in response:
                    return response.split("Error Type:")[1].split("\\n")[0].strip()
                return "Unknown"
        except:
            return "Unknown"