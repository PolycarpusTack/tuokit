# toolkits/crash_analyzer_v2/analytics/alerts.py
"""
Smart Alerts for Crash Analyzer V2 (Phase 1.5)
In-memory alerts without database storage or email
"""

import streamlit as st
from datetime import datetime
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, asdict
import json

@dataclass
class AlertRule:
    """Alert rule configuration"""
    name: str
    metric: str
    operator: str  # >, <, ==, !=
    value: float
    severity: str  # high, medium, low
    enabled: bool = True
    
    def check(self, current_value: Any) -> bool:
        """Check if rule condition is met"""
        if not self.enabled:
            return False
            
        try:
            # Convert to float for comparison
            if current_value is None:
                return False
                
            val = float(current_value)
            
            if self.operator == ">":
                return val > self.value
            elif self.operator == "<":
                return val < self.value
            elif self.operator == "==":
                return val == self.value
            elif self.operator == "!=":
                return val != self.value
            else:
                return False
        except (ValueError, TypeError):
            return False

@dataclass
class Alert:
    """Alert instance"""
    rule_name: str
    metric: str
    current_value: Any
    threshold_value: float
    severity: str
    timestamp: datetime
    message: str
    is_muted: bool = False

class SmartAlerts:
    """
    Smart alerts system for crash analytics (Phase 1.5)
    Uses session state only - no database storage
    """
    
    def __init__(self):
        self._init_session_state()
    
    def _init_session_state(self):
        """Initialize session state for alerts"""
        if 'alert_rules' not in st.session_state:
            # Default alert rules
            st.session_state.alert_rules = [
                AlertRule(
                    name="High Risk Score",
                    metric="risk_score",
                    operator=">",
                    value=7.0,
                    severity="high"
                ),
                AlertRule(
                    name="Crash Spike",
                    metric="total_crashes",
                    operator=">",
                    value=100,
                    severity="high"
                ),
                AlertRule(
                    name="Critical Issues Rising",
                    metric="critical_issues",
                    operator=">",
                    value=10,
                    severity="medium"
                ),
                AlertRule(
                    name="Positive Trend",
                    metric="trend_percentage",
                    operator=">",
                    value=20.0,
                    severity="medium"
                ),
                AlertRule(
                    name="Many Error Types",
                    metric="unique_error_types",
                    operator=">",
                    value=20,
                    severity="low"
                )
            ]
        
        if 'alert_history' not in st.session_state:
            st.session_state.alert_history = []
        
        if 'alerts_muted' not in st.session_state:
            st.session_state.alerts_muted = False
    
    def check_alerts(self, metrics: Dict) -> List[Alert]:
        """Check all rules against current metrics"""
        triggered_alerts = []
        
        if st.session_state.alerts_muted:
            return triggered_alerts
        
        for rule in st.session_state.alert_rules:
            if rule.metric in metrics:
                current_value = metrics[rule.metric]
                
                if rule.check(current_value):
                    # Create alert
                    alert = Alert(
                        rule_name=rule.name,
                        metric=rule.metric,
                        current_value=current_value,
                        threshold_value=rule.value,
                        severity=rule.severity,
                        timestamp=datetime.now(),
                        message=self._format_alert_message(rule, current_value)
                    )
                    triggered_alerts.append(alert)
        
        # Add to history (keep last 10)
        if triggered_alerts:
            st.session_state.alert_history.extend(triggered_alerts)
            st.session_state.alert_history = st.session_state.alert_history[-10:]
        
        return triggered_alerts
    
    def _format_alert_message(self, rule: AlertRule, current_value: Any) -> str:
        """Format alert message"""
        return f"{rule.name}: {rule.metric} is {current_value} (threshold: {rule.operator} {rule.value})"
    
    def render_alert_settings(self):
        """Render alert configuration UI"""
        st.markdown("#### ⚙️ Alert Settings")
        
        # Mute toggle
        col1, col2 = st.columns([3, 1])
        with col1:
            st.session_state.alerts_muted = st.checkbox(
                "🔇 Mute All Alerts",
                value=st.session_state.alerts_muted,
                help="Temporarily disable all alerts"
            )
        
        with col2:
            if st.button("🧪 Test Alert"):
                self._trigger_test_alert()
        
        # Rule configuration
        st.markdown("##### Alert Rules")
        
        for i, rule in enumerate(st.session_state.alert_rules):
            with st.expander(f"{rule.name} ({rule.severity})"):
                col1, col2, col3, col4 = st.columns([2, 1, 1, 1])
                
                with col1:
                    rule.enabled = st.checkbox(
                        "Enabled",
                        value=rule.enabled,
                        key=f"alert_enabled_{i}"
                    )
                
                with col2:
                    rule.operator = st.selectbox(
                        "Operator",
                        [">", "<", "==", "!="],
                        index=[">", "<", "==", "!="].index(rule.operator),
                        key=f"alert_op_{i}"
                    )
                
                with col3:
                    rule.value = st.number_input(
                        "Threshold",
                        value=rule.value,
                        key=f"alert_val_{i}"
                    )
                
                with col4:
                    rule.severity = st.selectbox(
                        "Severity",
                        ["high", "medium", "low"],
                        index=["high", "medium", "low"].index(rule.severity),
                        key=f"alert_sev_{i}"
                    )
                
                # Update rule in session state
                st.session_state.alert_rules[i] = rule
        
        # Add new rule section
        with st.expander("➕ Add New Rule", expanded=False):
            new_name = st.text_input("Rule Name", key="new_rule_name")
            
            col1, col2 = st.columns(2)
            with col1:
                new_metric = st.selectbox(
                    "Metric",
                    ["risk_score", "total_crashes", "critical_issues", 
                     "trend_percentage", "unique_error_types"],
                    key="new_rule_metric"
                )
            
            with col2:
                new_operator = st.selectbox(
                    "Operator",
                    [">", "<", "==", "!="],
                    key="new_rule_operator"
                )
            
            col3, col4 = st.columns(2)
            with col3:
                new_value = st.number_input(
                    "Threshold Value",
                    value=0.0,
                    key="new_rule_value"
                )
            
            with col4:
                new_severity = st.selectbox(
                    "Severity",
                    ["high", "medium", "low"],
                    key="new_rule_severity"
                )
            
            if st.button("Add Rule") and new_name:
                new_rule = AlertRule(
                    name=new_name,
                    metric=new_metric,
                    operator=new_operator,
                    value=new_value,
                    severity=new_severity
                )
                st.session_state.alert_rules.append(new_rule)
                st.success(f"Added rule: {new_name}")
                st.rerun()
    
    def render_active_alerts(self, alerts: List[Alert]):
        """Render active alerts in sidebar"""
        if not alerts and not st.session_state.alert_history:
            st.info("No active alerts")
            return
        
        # Current alerts
        if alerts and not st.session_state.alerts_muted:
            st.markdown("### 🚨 Active Alerts")
            for alert in alerts:
                if alert.severity == "high":
                    st.error(f"🔴 {alert.message}")
                elif alert.severity == "medium":
                    st.warning(f"🟡 {alert.message}")
                else:
                    st.info(f"🟢 {alert.message}")
        
        # Alert history
        if st.session_state.alert_history:
            with st.expander("📜 Alert History", expanded=False):
                for alert in reversed(st.session_state.alert_history[-5:]):
                    time_str = alert.timestamp.strftime("%H:%M:%S")
                    severity_icon = {
                        "high": "🔴",
                        "medium": "🟡", 
                        "low": "🟢"
                    }.get(alert.severity, "⚪")
                    
                    st.caption(f"{time_str} - {severity_icon} {alert.rule_name}")
    
    def _trigger_test_alert(self):
        """Create a test alert"""
        test_alert = Alert(
            rule_name="Test Alert",
            metric="test_metric",
            current_value=999,
            threshold_value=100,
            severity="medium",
            timestamp=datetime.now(),
            message="This is a test alert to verify the system is working"
        )
        
        st.session_state.alert_history.append(test_alert)
        st.session_state.alert_history = st.session_state.alert_history[-10:]
        st.success("Test alert triggered!")
        st.rerun()
    
    def clear_history(self):
        """Clear alert history"""
        st.session_state.alert_history = []
        st.success("Alert history cleared")