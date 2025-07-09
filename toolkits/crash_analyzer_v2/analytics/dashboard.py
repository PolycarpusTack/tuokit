# toolkits/crash_analyzer_v2/analytics/dashboard.py
"""
Analytics Dashboard for Crash Analyzer V2
Streamlit-native visualization following TuoKit principles
"""

import streamlit as st
from datetime import datetime, timedelta
import pandas as pd
from typing import Optional

from utils.database import DatabaseManager
from .statistics import CrashStatistics
from .alerts import SmartAlerts
from .predictor import CrashPredictor

class CrashAnalyticsDashboard:
    """
    Analytics dashboard using Streamlit native components
    No complex JS frameworks - just what's needed!
    """
    
    def __init__(self, db: Optional[DatabaseManager] = None):
        self.db = db or st.session_state.get('db')
        self.stats = CrashStatistics(self.db)
        self.alerts = SmartAlerts()
        self.predictor = CrashPredictor(self.db)
    
    def render(self):
        """Main render method for analytics dashboard"""
        if not self.db:
            st.warning("⚠️ Analytics requires database connection")
            return
        
        # Sidebar for alerts
        with st.sidebar:
            st.markdown("## 🚨 Alerts")
            # Get current metrics for alert checking
            # Use default time range for sidebar alerts
            sidebar_metrics = self.stats.calculate_metrics_summary("30d")
            triggered_alerts = self.alerts.check_alerts(sidebar_metrics)
            self.alerts.render_active_alerts(triggered_alerts)
        
        # Header and controls
        st.subheader("📊 Crash Analytics Dashboard")
        
        # Comparison mode toggle
        compare_mode = st.checkbox("📊 Compare Periods", key="compare_mode")
        
        if compare_mode:
            # Comparison mode controls
            col1, col2, col3 = st.columns([2, 2, 1])
            
            with col1:
                period_1 = st.selectbox(
                    "📅 Period 1",
                    ["7d", "30d", "90d"],
                    index=0,
                    format_func=lambda x: {
                        "7d": "Last 7 Days",
                        "30d": "Last 30 Days", 
                        "90d": "Last 90 Days"
                    }[x],
                    key="period_1"
                )
            
            with col2:
                period_2 = st.selectbox(
                    "📅 Period 2 (Compare to)",
                    ["Previous 7d", "Previous 30d", "Previous 90d"],
                    index=0,
                    format_func=lambda x: {
                        "Previous 7d": "Previous 7 Days",
                        "Previous 30d": "Previous 30 Days", 
                        "Previous 90d": "Previous 90 Days"
                    }[x],
                    key="period_2"
                )
            
            with col3:
                if st.button("🔄 Refresh", use_container_width=True):
                    st.rerun()
            
            time_range = period_1  # For compatibility
        else:
            # Normal mode
            col1, col2, col3 = st.columns([2, 2, 1])
            
            with col1:
                time_range = st.selectbox(
                    "📅 Time Range",
                    ["7d", "30d", "90d"],
                    index=1,
                    format_func=lambda x: {
                        "7d": "Last 7 Days",
                        "30d": "Last 30 Days", 
                        "90d": "Last 90 Days"
                    }[x]
                )
            
            with col3:
                if st.button("🔄 Refresh", use_container_width=True):
                    st.rerun()
        
        # Get metrics summary
        if compare_mode:
            # Get metrics for both periods
            metrics_1 = self.stats.calculate_metrics_summary(period_1)
            
            # Calculate period 2 time range
            days = int(period_2.split()[1].rstrip('d'))
            period_2_range = f"{days}d"
            metrics_2 = self.stats.calculate_metrics_summary(period_2_range)
            
            # Show comparison view
            self._render_comparison_metrics(metrics_1, metrics_2, period_1, period_2)
            
            # Use period 1 metrics for compatibility
            metrics = metrics_1
        else:
            metrics = self.stats.calculate_metrics_summary(time_range)
            
            # KPI Metrics Row
            st.markdown("### 📈 Key Metrics")
            col1, col2, col3, col4 = st.columns(4)
            
            with col1:
                st.metric(
                    "Total Crashes",
                    metrics["total_crashes"],
                    f"{metrics['trend_percentage']:+.1f}%",
                    delta_color="inverse"  # Red is bad for crashes
                )
            
            with col2:
                trend_pct = metrics['trend_percentage']
                if trend_pct > 20:
                    trend_emoji = "📈"
                elif trend_pct < -20:
                    trend_emoji = "📉"
                else:
                    trend_emoji = "➡️"
                st.metric(
                    "Trend",
                    f"{trend_emoji} {abs(trend_pct):.1f}%",
                    "vs previous period"
                )
            
            with col3:
                risk_color = "🟢" if metrics['risk_score'] < 3 else "🟡" if metrics['risk_score'] < 7 else "🔴"
                st.metric(
                    "Risk Score",
                    f"{risk_color} {metrics['risk_score']}/10",
                    f"{metrics['critical_issues']} critical"
                )
            
            with col4:
                st.metric(
                    "Unique Errors",
                    metrics['unique_error_types'],
                    metrics['most_common_error'][:15] + "..."
                )
        
        # Main content area with tabs
        tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs(["📊 Overview", "📈 Trends", "🎯 Top Issues", "🔍 Patterns", "📋 Reports", "🚨 Alerts"])
        
        with tab1:
            self._render_overview_tab(time_range)
        
        with tab2:
            self._render_trends_tab(time_range)
        
        with tab3:
            self._render_top_issues_tab(time_range)
        
        with tab4:
            self._render_patterns_tab(time_range)
        
        with tab5:
            self._render_reports_tab(time_range, metrics)
        
        with tab6:
            self._render_alerts_tab(metrics)
    
    def _render_overview_tab(self, time_range: str):
        """Render overview tab with distributions"""
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("#### 🎨 Crash Distribution by Type")
            distribution = self.stats.get_crash_distribution(time_range)
            
            if distribution:
                # Create simple bar chart data
                df = pd.DataFrame(
                    list(distribution.items()),
                    columns=["Error Type", "Count"]
                ).sort_values("Count", ascending=True)
                
                # Use Streamlit native bar chart
                st.bar_chart(df.set_index("Error Type"))
            else:
                st.info("No crash data available for selected period")
        
        with col2:
            st.markdown("#### 🚨 Severity Distribution")
            severity_data = self.stats.get_severity_metrics(time_range)
            
            if severity_data:
                # Create pie chart data using st.pyplot (minimal dependency)
                severity_df = pd.DataFrame(
                    list(severity_data.items()),
                    columns=["Severity", "Count"]
                )
                
                # Show as metrics for simplicity
                for severity, count in severity_data.items():
                    color = {
                        "CRITICAL": "🔴",
                        "HIGH": "🟠",
                        "MEDIUM": "🟡",
                        "LOW": "🟢"
                    }.get(severity, "⚪")
                    st.write(f"{color} **{severity}**: {count} crashes")
            else:
                st.info("No severity data available")
        
        # Analysis method usage
        st.markdown("#### 🔧 Analysis Method Usage")
        method_stats = self.stats.get_analysis_method_stats(time_range)
        
        if method_stats:
            method_df = pd.DataFrame(
                list(method_stats.items()),
                columns=["Method", "Usage Count"]
            ).sort_values("Usage Count", ascending=False)
            
            st.dataframe(method_df, use_container_width=True, hide_index=True)
        else:
            st.info("No method usage data available")
    
    def _render_trends_tab(self, time_range: str):
        """Render trends visualization tab"""
        st.markdown("#### 📈 Crash Trends Over Time")
        
        # Time series chart
        time_data = self.stats.get_time_series_data(time_range, group_by="day")
        
        if time_data["dates"]:
            # Create DataFrame for line chart
            df = pd.DataFrame({
                "Date": pd.to_datetime(time_data["dates"]),
                "Crashes": time_data["counts"]
            })
            
            # Streamlit native line chart
            st.line_chart(df.set_index("Date"))
            
            # Show statistics
            col1, col2, col3 = st.columns(3)
            
            with col1:
                avg_daily = sum(time_data["counts"]) / len(time_data["counts"])
                st.metric("Daily Average", f"{avg_daily:.1f}")
            
            with col2:
                max_day = max(time_data["counts"])
                st.metric("Peak Day", max_day)
            
            with col3:
                # Calculate trend direction
                if len(time_data["counts"]) > 7:
                    recent_avg = sum(time_data["counts"][-7:]) / 7
                    older_avg = sum(time_data["counts"][:-7]) / (len(time_data["counts"]) - 7)
                    trend = "📈 Increasing" if recent_avg > older_avg else "📉 Decreasing"
                else:
                    trend = "➡️ Stable"
                st.metric("Trend", trend)
        else:
            st.info("No trend data available for selected period")
        
        # Crash pattern analysis
        st.markdown("#### 🔍 Pattern Analysis")
        
        # Simple pattern detection
        if time_data["counts"]:
            # Day of week analysis (if we have enough data)
            if len(time_data["dates"]) >= 7:
                st.write("**Crash Patterns by Day of Week:**")
                
                # Group by day of week
                dates = pd.to_datetime(time_data["dates"])
                day_counts = {}
                for date, count in zip(dates, time_data["counts"]):
                    day_name = date.strftime("%A")
                    if day_name not in day_counts:
                        day_counts[day_name] = []
                    day_counts[day_name].append(count)
                
                # Calculate averages
                day_avgs = {day: sum(counts)/len(counts) for day, counts in day_counts.items()}
                
                # Sort by day of week
                days_order = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
                sorted_days = sorted(day_avgs.items(), key=lambda x: days_order.index(x[0]) if x[0] in days_order else 7)
                
                # Display
                day_df = pd.DataFrame(sorted_days, columns=["Day", "Avg Crashes"])
                st.bar_chart(day_df.set_index("Day"))
    
    def _render_top_issues_tab(self, time_range: str):
        """Render top issues analysis"""
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("#### 🎯 Top Crash Issues")
            top_issues = self.stats.get_top_issues(limit=10, time_range=time_range)
            
            if top_issues:
                for i, (error_type, count) in enumerate(top_issues, 1):
                    # Calculate percentage
                    total = sum(count for _, count in top_issues)
                    percentage = (count / total) * 100 if total > 0 else 0
                    
                    # Progress bar visualization
                    st.write(f"**{i}. {error_type}**")
                    st.progress(percentage / 100, text=f"{count} crashes ({percentage:.1f}%)")
            else:
                st.info("No crash data available")
        
        with col2:
            st.markdown("#### 🔥 Trending Issues")
            
            # Compare current vs previous period
            current_dist = self.stats.get_crash_distribution(time_range)
            
            # Get previous period data for comparison
            days = int(time_range.rstrip('d'))
            prev_time_range = f"{days * 2}d"
            prev_dist_full = self.stats.get_crash_distribution(prev_time_range)
            
            # Calculate previous period counts (excluding current period)
            prev_dist = {}
            for error_type, total_count in prev_dist_full.items():
                current_count = current_dist.get(error_type, 0)
                prev_count = max(0, total_count - current_count)
                if prev_count > 0:
                    prev_dist[error_type] = prev_count
            
            # Calculate trends
            trending = []
            for error_type, current_count in current_dist.items():
                prev_count = prev_dist.get(error_type, 0)
                
                # Calculate trend
                if prev_count == 0:
                    # New error type
                    trend_indicator = "🆕"
                    trend_percent = 100
                elif current_count > prev_count * 1.2:  # 20% increase threshold
                    trend_indicator = "📈"
                    trend_percent = ((current_count - prev_count) / prev_count) * 100
                elif current_count < prev_count * 0.8:  # 20% decrease threshold
                    trend_indicator = "📉"
                    trend_percent = ((current_count - prev_count) / prev_count) * 100
                else:
                    trend_indicator = "➡️"
                    trend_percent = 0
                
                trending.append((error_type, current_count, trend_indicator, trend_percent))
            
            # Sort by absolute trend percentage (most changing first)
            trending.sort(key=lambda x: abs(x[3]), reverse=True)
            
            if trending:
                st.write("**Trending Issues:**")
                for error_type, count, indicator, trend_percent in trending[:5]:
                    if trend_percent != 0:
                        st.write(f"{indicator} **{error_type}**: {count} occurrences ({trend_percent:+.0f}%)")
                    else:
                        st.write(f"{indicator} **{error_type}**: {count} occurrences (stable)")
            else:
                st.info("No trending data available")
            
            # Actionable insights
            st.markdown("#### 💡 Insights")
            if top_issues:
                top_error = top_issues[0][0]
                st.info(f"**Focus Area**: {top_error} accounts for the most crashes. Consider prioritizing fixes for this error type.")
                
                if metrics := self.stats.calculate_metrics_summary(time_range):
                    if metrics['risk_score'] > 7:
                        st.warning("**High Risk**: Risk score indicates critical issues need immediate attention.")
                    elif metrics['trend_percentage'] > 20:
                        st.warning("**Rising Trend**: Crash rate increasing significantly. Investigation recommended.")
    
    def _render_patterns_tab(self, time_range: str):
        """Render pattern analysis tab"""
        from .patterns import CrashPatternDetector
        
        st.markdown("### 🔍 Pattern Analysis")
        
        # Initialize pattern detector
        detector = CrashPatternDetector(self.db)
        
        # Run analysis with progress
        with st.spinner("Analyzing crash patterns..."):
            analysis = detector.analyze_patterns(time_range)
        
        # Summary metrics
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.metric(
                "Patterns Found",
                analysis['summary']['patterns_found'],
                help="Total number of significant patterns detected"
            )
        
        with col2:
            pattern_score = analysis['summary']['pattern_strength']
            st.metric(
                "Pattern Strength",
                f"{pattern_score:.0%}",
                help="How strong the time-based patterns are"
            )
        
        with col3:
            has_cascades = "✅ Yes" if analysis['summary']['has_cascading_failures'] else "❌ No"
            st.metric(
                "Cascading Failures",
                has_cascades,
                help="Whether one error leads to others"
            )
        
        with col4:
            sequence_count = len(analysis['sequences'])
            st.metric(
                "Error Sequences",
                sequence_count,
                help="Errors that occur together"
            )
        
        # Pattern details
        if analysis['insights']:
            st.markdown("#### 💡 Key Insights")
            
            # Group insights by severity
            critical = [i for i in analysis['insights'] if i['severity'] == 'critical']
            high = [i for i in analysis['insights'] if i['severity'] == 'high']
            medium = [i for i in analysis['insights'] if i['severity'] == 'medium']
            
            # Display critical insights first
            for insight in critical:
                st.error(f"🔴 **Critical**: {insight['message']}")
            
            for insight in high:
                st.warning(f"🟠 **High**: {insight['message']}")
            
            for insight in medium:
                st.info(f"🟡 **Medium**: {insight['message']}")
        
        # Detailed pattern sections
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown("#### ⏰ Time Patterns")
            time_patterns = analysis['time_patterns']
            
            if time_patterns['hourly_patterns']:
                st.write("**Peak Hours by Error Type:**")
                for error, pattern in time_patterns['hourly_patterns'].items():
                    st.write(f"• **{error}**: {pattern['peak_hour']}:00 ({pattern['concentration']}% concentration)")
            
            if time_patterns['daily_patterns']:
                st.write("\n**Peak Days by Error Type:**")
                for error, pattern in time_patterns['daily_patterns'].items():
                    st.write(f"• **{error}**: {pattern['peak_day']} ({pattern['concentration']}% concentration)")
            
            if time_patterns.get('weekly_patterns', {}).get('is_weekend_heavy', False):
                st.write(f"\n⚠️ **Weekend Alert**: {time_patterns['weekly_patterns']['weekend_concentration']}% of crashes occur on weekends")
        
        with col2:
            st.markdown("#### 🔗 Error Sequences")
            
            if analysis['sequences']:
                st.write("**Common Error Sequences:**")
                for i, seq in enumerate(analysis['sequences'][:5], 1):
                    sequence_str = " → ".join(seq['sequence'])
                    st.write(f"{i}. {sequence_str}")
                    st.caption(f"   Occurs {seq['count']} times ({seq['confidence']}% of crashes)")
            else:
                st.info("No significant error sequences detected")
        
        # Cascading failures section
        if analysis['cascades']:
            st.markdown("#### 🌊 Cascading Failures")
            st.write("**Root Causes and Their Effects:**")
            
            for root, effects in list(analysis['cascades'].items())[:5]:
                with st.expander(f"🔴 {root} (Root Cause)"):
                    for effect in effects[:3]:
                        st.write(f"→ **{effect['effect']}**")
                        st.caption(f"  Occurs {effect['occurrences']} times, typically within {effect['typical_delay']}")
        
        # Recommendations based on patterns
        if analysis['insights']:
            st.markdown("#### 📌 Recommendations")
            
            recommendations = []
            
            # Time-based recommendations
            if any(i['type'] == 'time_pattern' for i in analysis['insights']):
                recommendations.append("📅 **Schedule Maintenance**: Plan system maintenance outside of peak crash hours")
            
            # Sequence-based recommendations
            if any(i['type'] == 'sequence' for i in analysis['insights']):
                recommendations.append("🔗 **Break Error Chains**: Implement circuit breakers to prevent error propagation")
            
            # Cascade-based recommendations
            if any(i['type'] == 'cascade' for i in analysis['insights']):
                recommendations.append("🛡️ **Fix Root Causes**: Prioritize fixing errors that trigger cascading failures")
            
            for rec in recommendations:
                st.write(rec)
        
        # Add predictions section
        st.markdown("---")
        st.markdown("### 📈 Predictive Analytics (EXPERIMENTAL)")
        
        with st.expander("7-Day Crash Predictions", expanded=True):
            # Get predictions
            prediction_summary = self.predictor.get_prediction_summary(days_ahead=7)
            
            if prediction_summary['moving_average']['status'] == 'success':
                # Show prediction chart
                predictions = prediction_summary['moving_average']['predictions']
                
                # Create DataFrame for visualization
                pred_dates = [p['date'] for p in predictions]
                pred_values = [p['predicted'] for p in predictions]
                lower_bounds = [p['lower_bound'] for p in predictions]
                upper_bounds = [p['upper_bound'] for p in predictions]
                
                pred_df = pd.DataFrame({
                    'Date': pd.to_datetime(pred_dates),
                    'Predicted': pred_values,
                    'Lower Bound': lower_bounds,
                    'Upper Bound': upper_bounds
                })
                
                # Metrics
                col1, col2, col3 = st.columns(3)
                
                with col1:
                    avg_predicted = sum(pred_values) / len(pred_values)
                    st.metric(
                        "Avg Predicted",
                        f"{avg_predicted:.1f}",
                        f"{prediction_summary['moving_average']['trend']}"
                    )
                
                with col2:
                    st.metric(
                        "Historical Avg",
                        prediction_summary['moving_average']['historical_mean'],
                        help="Average crashes over historical period"
                    )
                
                with col3:
                    volatility = prediction_summary['metrics']['volatility']
                    vol_emoji = "🟢" if volatility < 30 else "🟡" if volatility < 50 else "🔴"
                    st.metric(
                        "Volatility",
                        f"{vol_emoji} {volatility:.1f}%",
                        help="Standard deviation as % of mean"
                    )
                
                # Show predictions table
                st.write("**7-Day Forecast:**")
                for pred in predictions[:7]:
                    confidence_color = "🟢" if pred['confidence'] >= 80 else "🟡" if pred['confidence'] >= 60 else "🔴"
                    st.write(f"• **{pred['date']}**: {pred['predicted']:.0f} crashes "
                           f"(range: {pred['lower_bound']:.0f}-{pred['upper_bound']:.0f}) "
                           f"{confidence_color} {pred['confidence']:.0f}% confidence")
                
                # Trend analysis
                if prediction_summary['trend_analysis']['status'] == 'success':
                    st.write(f"\n**Trend**: {prediction_summary['trend_analysis']['equation']}")
                    st.write(f"R² = {prediction_summary['trend_analysis']['r_squared']:.3f}")
                
                # Recommendation
                st.info(f"💡 {prediction_summary['recommendation']}")
            else:
                st.warning("Insufficient data for predictions. Need at least 7 days of crash data.")
    
    def _render_comparison_metrics(self, metrics_1: dict, metrics_2: dict, period_1: str, period_2: str):
        """Render side-by-side comparison metrics"""
        st.markdown("### 📊 Period Comparison")
        
        # Create two columns for side-by-side display
        col1, col2 = st.columns(2)
        
        with col1:
            st.markdown(f"#### 📅 {period_1}")
            self._display_period_metrics(metrics_1, "period1")
        
        with col2:
            st.markdown(f"#### 📅 {period_2}")
            self._display_period_metrics(metrics_2, "period2")
        
        # Comparison insights
        st.markdown("### 📈 Comparison Analysis")
        
        # Calculate changes
        changes = {
            'total_change': self._calculate_change(metrics_1['total_crashes'], metrics_2['total_crashes']),
            'risk_change': self._calculate_change(metrics_1['risk_score'], metrics_2['risk_score']),
            'critical_change': self._calculate_change(metrics_1['critical_issues'], metrics_2['critical_issues']),
            'unique_change': self._calculate_change(metrics_1['unique_error_types'], metrics_2['unique_error_types'])
        }
        
        # Display significant changes
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            change = changes['total_change']
            arrow = "↑" if change > 0 else "↓" if change < 0 else "→"
            color = "inverse" if abs(change) > 20 else None
            st.metric(
                "Total Crashes Change",
                f"{arrow} {abs(change):.1f}%",
                f"{metrics_1['total_crashes']} vs {metrics_2['total_crashes']}",
                delta_color=color
            )
        
        with col2:
            change = changes['risk_change']
            arrow = "↑" if change > 0 else "↓" if change < 0 else "→"
            st.metric(
                "Risk Score Change",
                f"{arrow} {abs(change):.1f}%",
                f"{metrics_1['risk_score']} vs {metrics_2['risk_score']}"
            )
        
        with col3:
            change = changes['critical_change']
            arrow = "↑" if change > 0 else "↓" if change < 0 else "→"
            st.metric(
                "Critical Issues Change",
                f"{arrow} {abs(change):.1f}%",
                f"{metrics_1['critical_issues']} vs {metrics_2['critical_issues']}"
            )
        
        with col4:
            change = changes['unique_change']
            arrow = "↑" if change > 0 else "↓" if change < 0 else "→"
            st.metric(
                "Error Types Change",
                f"{arrow} {abs(change):.1f}%",
                f"{metrics_1['unique_error_types']} vs {metrics_2['unique_error_types']}"
            )
        
        # Highlight significant differences
        if abs(changes['total_change']) > 20:
            if changes['total_change'] > 0:
                st.warning(f"⚠️ Significant increase in crashes: {changes['total_change']:.1f}% more crashes in {period_1}")
            else:
                st.success(f"✅ Significant decrease in crashes: {abs(changes['total_change']):.1f}% fewer crashes in {period_1}")
        
        if changes['risk_change'] > 20:
            st.error(f"🔴 Risk score increased by {changes['risk_change']:.1f}% - system stability declining")
        
        # Export comparison button
        if st.button("📥 Export Comparison Report"):
            report = self._generate_comparison_report(metrics_1, metrics_2, period_1, period_2, changes)
            st.download_button(
                "Download Report",
                report,
                f"comparison_{period_1}_vs_{period_2}_{datetime.now().strftime('%Y%m%d')}.md",
                "text/markdown"
            )
    
    def _display_period_metrics(self, metrics: dict, key_prefix: str):
        """Display metrics for a single period"""
        col1, col2 = st.columns(2)
        
        with col1:
            st.metric("Total Crashes", metrics['total_crashes'])
            st.metric("Critical Issues", metrics['critical_issues'])
        
        with col2:
            risk_color = "🟢" if metrics['risk_score'] < 3 else "🟡" if metrics['risk_score'] < 7 else "🔴"
            st.metric("Risk Score", f"{risk_color} {metrics['risk_score']}/10")
            st.metric("Unique Errors", metrics['unique_error_types'])
        
        # Trend within period
        trend_pct = metrics['trend_percentage']
        trend_emoji = "📈" if trend_pct > 20 else "📉" if trend_pct < -20 else "➡️"
        st.metric("Internal Trend", f"{trend_emoji} {abs(trend_pct):.1f}%")
    
    def _calculate_change(self, value1: float, value2: float) -> float:
        """Calculate percentage change between two values"""
        # Handle None values
        if value1 is None:
            value1 = 0
        if value2 is None:
            value2 = 0
            
        if value2 == 0:
            return 100.0 if value1 > 0 else 0.0
        return ((value1 - value2) / value2) * 100
    
    def _generate_comparison_report(self, metrics_1: dict, metrics_2: dict, 
                                   period_1: str, period_2: str, changes: dict) -> str:
        """Generate markdown comparison report"""
        report = f"""# Crash Analysis Comparison Report
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Periods Compared
- **Period 1**: {period_1}
- **Period 2**: {period_2}

## Summary
Total crashes changed by {changes['total_change']:+.1f}% between periods.

## Detailed Metrics

### Period 1: {period_1}
- Total Crashes: {metrics_1['total_crashes']}
- Risk Score: {metrics_1['risk_score']}/10
- Critical Issues: {metrics_1['critical_issues']}
- Unique Error Types: {metrics_1['unique_error_types']}
- Most Common Error: {metrics_1['most_common_error']}

### Period 2: {period_2}
- Total Crashes: {metrics_2['total_crashes']}
- Risk Score: {metrics_2['risk_score']}/10
- Critical Issues: {metrics_2['critical_issues']}
- Unique Error Types: {metrics_2['unique_error_types']}
- Most Common Error: {metrics_2['most_common_error']}

## Changes
- Total Crashes: {changes['total_change']:+.1f}%
- Risk Score: {changes['risk_change']:+.1f}%
- Critical Issues: {changes['critical_change']:+.1f}%
- Unique Error Types: {changes['unique_change']:+.1f}%

## Insights
"""
        if abs(changes['total_change']) > 20:
            report += f"- Significant {'increase' if changes['total_change'] > 0 else 'decrease'} in crash volume\n"
        
        if changes['risk_change'] > 20:
            report += "- System stability has declined significantly\n"
        elif changes['risk_change'] < -20:
            report += "- System stability has improved significantly\n"
        
        if metrics_1['most_common_error'] != metrics_2['most_common_error']:
            report += f"- Most common error changed from {metrics_2['most_common_error']} to {metrics_1['most_common_error']}\n"
        
        return report
    
    def _render_reports_tab(self, time_range: str, metrics: dict):
        """Render reports and export options"""
        st.markdown("#### 📋 Executive Summary Report")
        
        # Generate report summary
        report_date = datetime.now().strftime("%Y-%m-%d")
        days = int(time_range.rstrip('d'))
        
        # Create markdown report
        report = f"""# Crash Analysis Report
Generated: {report_date}
Period: Last {days} days

## Executive Summary
- **Total Crashes**: {metrics['total_crashes']}
- **Trend**: {metrics['trend_percentage']:+.1f}% vs previous period
- **Risk Score**: {metrics['risk_score']}/10
- **Critical Issues**: {metrics['critical_issues']}
- **Most Common Error**: {metrics['most_common_error']}

## Key Findings
"""
        
        # Add top issues
        top_issues = self.stats.get_top_issues(limit=5, time_range=time_range)
        if top_issues:
            report += "\n### Top Issues by Frequency\n"
            for i, (error_type, count) in enumerate(top_issues, 1):
                report += f"{i}. **{error_type}**: {count} occurrences\n"
        
        # Add severity breakdown
        severity_data = self.stats.get_severity_metrics(time_range)
        if severity_data:
            report += "\n### Severity Distribution\n"
            for severity, count in sorted(severity_data.items()):
                report += f"- {severity}: {count} crashes\n"
        
        # Add recommendations
        report += "\n## Recommendations\n"
        if metrics['risk_score'] > 7:
            report += "1. **Immediate Action Required**: High risk score indicates system instability\n"
        if metrics['trend_percentage'] > 20:
            report += "2. **Investigate Rising Trend**: Significant increase in crash rate detected\n"
        if top_issues:
            report += f"3. **Priority Fix**: Focus on {top_issues[0][0]} errors first\n"
        
        # Display report
        st.text_area("Report Preview", report, height=400)
        
        # Export options
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.download_button(
                "📥 Download Report (Markdown)",
                data=report,
                file_name=f"crash_report_{report_date}.md",
                mime="text/markdown"
            )
        
        with col2:
            # CSV export of raw data
            if distribution := self.stats.get_crash_distribution(time_range):
                import csv
                import io
                
                output = io.StringIO()
                writer = csv.writer(output)
                writer.writerow(["Error Type", "Count"])
                for error_type, count in distribution.items():
                    writer.writerow([error_type, count])
                
                st.download_button(
                    "📊 Download Data (CSV)",
                    data=output.getvalue(),
                    file_name=f"crash_data_{report_date}.csv",
                    mime="text/csv"
                )
        
        with col3:
            if st.button("📧 Email Report (Phase 2)", use_container_width=True, disabled=True):
                st.info("📧 Email integration feature coming in Phase 2 - will allow automated report delivery to stakeholders")
        
        # Settings for automated reports
        st.markdown("#### ⚙️ Report Settings (Phase 2)")
        
        with st.expander("Automated Reports Configuration - Coming Soon", expanded=False):
            st.info("🚧 **Phase 2 Feature**: Automated report scheduling and email delivery")
            st.write("**Planned Features:**")
            st.write("- Schedule daily/weekly/monthly reports")
            st.write("- Email delivery to multiple recipients")
            st.write("- Save report preferences")
            st.write("- Automatic knowledge base archival")
            
            # Show disabled preview
            st.selectbox(
                "Report Frequency (Preview)",
                ["Disabled", "Daily", "Weekly", "Monthly"],
                disabled=True,
                help="Will be enabled in Phase 2"
            )
            
            st.text_input(
                "Email Recipients (Preview)",
                placeholder="email1@example.com, email2@example.com",
                disabled=True,
                help="Will be enabled in Phase 2"
            )
    
    def _render_alerts_tab(self, metrics: dict):
        """Render alerts configuration and history"""
        st.markdown("### 🚨 Smart Alerts (Phase 1.5)")
        st.info("Configure alerts to notify you when metrics exceed thresholds. Alerts are shown in the sidebar.")
        
        # Check alerts based on current metrics
        triggered_alerts = self.alerts.check_alerts(metrics)
        
        # Show alert settings
        self.alerts.render_alert_settings()
        
        # Display triggered alerts if any
        if triggered_alerts and not st.session_state.alerts_muted:
            st.markdown("#### 🔔 Currently Triggered Alerts")
            for alert in triggered_alerts:
                if alert.severity == "high":
                    st.error(f"🔴 {alert.message}")
                elif alert.severity == "medium":
                    st.warning(f"🟡 {alert.message}")
                else:
                    st.info(f"🟢 {alert.message}")
        
        # Alert history section
        st.markdown("#### 📜 Alert History")
        if st.session_state.alert_history:
            # Show recent alerts
            for alert in reversed(st.session_state.alert_history):
                time_str = alert.timestamp.strftime("%Y-%m-%d %H:%M:%S")
                severity_icon = {
                    "high": "🔴",
                    "medium": "🟡",
                    "low": "🟢"
                }.get(alert.severity, "⚪")
                
                with st.container():
                    col1, col2 = st.columns([3, 1])
                    with col1:
                        st.write(f"{severity_icon} **{alert.rule_name}**")
                        st.caption(f"{alert.message}")
                    with col2:
                        st.caption(time_str)
            
            # Clear history button
            if st.button("🗑️ Clear History"):
                self.alerts.clear_history()
                st.rerun()
        else:
            st.info("No alerts in history. Alerts will appear here when triggered.")