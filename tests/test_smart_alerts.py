# tests/test_smart_alerts.py
"""
Test Smart Alerts for Crash Analyzer V2
Quality Gate for Task 4 (Phase 1.5)
"""

import pytest
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock
import streamlit as st

# Import after mocking streamlit
with patch('streamlit.markdown'), patch('streamlit.checkbox'), \
     patch('streamlit.columns'), patch('streamlit.button'), \
     patch('streamlit.selectbox'), patch('streamlit.number_input'), \
     patch('streamlit.text_input'), patch('streamlit.expander'), \
     patch('streamlit.success'), patch('streamlit.error'), \
     patch('streamlit.warning'), patch('streamlit.info'), \
     patch('streamlit.caption'), patch('streamlit.container'), \
     patch('streamlit.write'), patch('streamlit.rerun'):
    from toolkits.crash_analyzer_v2.analytics.alerts import SmartAlerts, AlertRule, Alert

class TestSmartAlerts:
    """Test all smart alert features"""
    
    def setup_method(self):
        """Set up test data"""
        # Mock session state
        self.mock_session_state = MagicMock()
        self.mock_session_state.alert_rules = []
        self.mock_session_state.alert_history = []
        self.mock_session_state.alerts_muted = False
        
        # Sample metrics
        self.sample_metrics = {
            "risk_score": 8.5,  # Should trigger high risk alert
            "total_crashes": 150,  # Should trigger crash spike
            "critical_issues": 15,  # Should trigger critical issues
            "trend_percentage": 25.0,  # Should trigger positive trend
            "unique_error_types": 10  # Should not trigger
        }
        
        self.low_metrics = {
            "risk_score": 3.0,
            "total_crashes": 50,
            "critical_issues": 5,
            "trend_percentage": -5.0,
            "unique_error_types": 8
        }
    
    def test_alert_triggers_when_condition_met(self):
        """Test 1: Alert triggers when condition met"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Add test rule
            rule = AlertRule(
                name="Test High Risk",
                metric="risk_score",
                operator=">",
                value=7.0,
                severity="high"
            )
            st.session_state.alert_rules = [rule]
            
            # Check alerts
            triggered = alerts.check_alerts(self.sample_metrics)
            
            assert len(triggered) == 1
            assert triggered[0].rule_name == "Test High Risk"
            assert triggered[0].current_value == 8.5
            assert triggered[0].severity == "high"
    
    def test_multiple_alerts_can_be_active(self):
        """Test 2: Multiple alerts can be active"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Initialize default rules
            alerts._init_session_state()
            
            # Check alerts with high metrics
            triggered = alerts.check_alerts(self.sample_metrics)
            
            # Should trigger multiple alerts
            assert len(triggered) >= 3  # At least risk, crashes, and critical issues
            
            # Verify different severities
            severities = [alert.severity for alert in triggered]
            assert "high" in severities
            assert "medium" in severities
    
    def test_alert_history_maintains_ten_items_max(self):
        """Test 3: Alert history maintains 10 items max"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Add a simple rule
            rule = AlertRule(
                name="Test Rule",
                metric="risk_score",
                operator=">",
                value=2.0,
                severity="low"
            )
            st.session_state.alert_rules = [rule]
            
            # Trigger alerts 15 times
            for i in range(15):
                metrics = {"risk_score": 5.0 + i}
                alerts.check_alerts(metrics)
            
            # History should only have 10 items
            assert len(st.session_state.alert_history) == 10
            
            # Latest alerts should be preserved
            assert st.session_state.alert_history[-1].current_value == 19.0
    
    def test_muted_alerts_dont_show(self):
        """Test 4: Muted alerts don't show"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Add test rule
            rule = AlertRule(
                name="Test Rule",
                metric="risk_score",
                operator=">",
                value=7.0,
                severity="high"
            )
            st.session_state.alert_rules = [rule]
            
            # Mute alerts
            st.session_state.alerts_muted = True
            
            # Check alerts
            triggered = alerts.check_alerts(self.sample_metrics)
            
            # Should return empty list when muted
            assert len(triggered) == 0
    
    def test_test_button_creates_sample_alert(self):
        """Test 5: Test button creates sample alert"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Trigger test alert
            alerts._trigger_test_alert()
            
            # Check history
            assert len(st.session_state.alert_history) == 1
            assert st.session_state.alert_history[0].rule_name == "Test Alert"
            assert st.session_state.alert_history[0].severity == "medium"
            assert "test alert" in st.session_state.alert_history[0].message.lower()
    
    def test_invalid_rules_rejected(self):
        """Test 6: Invalid rules rejected"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Test rule with invalid operator
            rule = AlertRule(
                name="Invalid Rule",
                metric="risk_score",
                operator="invalid",
                value=5.0,
                severity="high"
            )
            
            # Check should return False for invalid operator
            assert not rule.check(8.0)
            
            # Test rule with None value
            assert not rule.check(None)
            
            # Test rule with non-numeric value
            assert not rule.check("not a number")
    
    def test_alerts_cleared_on_session_reset(self):
        """Test 7: Alerts cleared on session reset"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Add some alerts to history
            st.session_state.alert_history = [
                Alert(
                    rule_name="Test",
                    metric="risk_score",
                    current_value=8.0,
                    threshold_value=7.0,
                    severity="high",
                    timestamp=datetime.now(),
                    message="Test alert"
                )
            ]
            
            # Clear history
            alerts.clear_history()
            
            assert len(st.session_state.alert_history) == 0
    
    def test_alert_rule_operators(self):
        """Test various alert rule operators"""
        # Test greater than
        rule_gt = AlertRule("GT", "metric", ">", 5.0, "high")
        assert rule_gt.check(6.0) == True
        assert rule_gt.check(5.0) == False
        assert rule_gt.check(4.0) == False
        
        # Test less than
        rule_lt = AlertRule("LT", "metric", "<", 5.0, "high")
        assert rule_lt.check(4.0) == True
        assert rule_lt.check(5.0) == False
        assert rule_lt.check(6.0) == False
        
        # Test equals
        rule_eq = AlertRule("EQ", "metric", "==", 5.0, "high")
        assert rule_eq.check(5.0) == True
        assert rule_eq.check(4.0) == False
        assert rule_eq.check(6.0) == False
        
        # Test not equals
        rule_ne = AlertRule("NE", "metric", "!=", 5.0, "high")
        assert rule_ne.check(4.0) == True
        assert rule_ne.check(5.0) == False
        assert rule_ne.check(6.0) == True
    
    def test_disabled_rules_dont_trigger(self):
        """Test that disabled rules don't trigger alerts"""
        with patch.object(st, 'session_state', self.mock_session_state):
            alerts = SmartAlerts()
            
            # Add disabled rule
            rule = AlertRule(
                name="Disabled Rule",
                metric="risk_score",
                operator=">",
                value=5.0,
                severity="high",
                enabled=False
            )
            st.session_state.alert_rules = [rule]
            
            # Check alerts
            triggered = alerts.check_alerts(self.sample_metrics)
            
            # Should not trigger disabled rule
            assert len(triggered) == 0

if __name__ == "__main__":
    pytest.main([__file__, "-v"])