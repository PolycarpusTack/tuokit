"""
Main Health Analyzer for TuoKit
Unified health monitoring across all system components
"""

import streamlit as st
from typing import Dict, Any, Optional
from datetime import datetime

from utils.tool_base import TuoKitToolBase

class HealthAnalyzer(TuoKitToolBase):
    """Main health monitoring dashboard"""
    
    def __init__(self):
        super().__init__(
            tool_name="System Health Monitor",
            tool_description="Comprehensive health monitoring for database, Ollama, and system resources"
        )
        
        # Import health checkers
        from .database_health import DatabaseHealthChecker
        from .ollama_health import OllamaHealthChecker
        from .system_health import SystemHealthDashboard
        
        self.db_health = DatabaseHealthChecker()
        self.ollama_health = OllamaHealthChecker()
        self.system_health = SystemHealthDashboard()
    
    def run(self):
        """Main entry point for health monitoring"""
        st.title("🏥 TuoKit Health Monitor")
        st.markdown("Comprehensive system health monitoring and diagnostics")
        
        # Navigation tabs
        tab1, tab2, tab3, tab4 = st.tabs([
            "📊 Overview", 
            "🗄️ Database Health",
            "🤖 Ollama Health",
            "🧪 System Diagnostics"
        ])
        
        with tab1:
            self._show_overview()
            
        with tab2:
            self.db_health.run()
            
        with tab3:
            self.ollama_health.run()
            
        with tab4:
            self.system_health.run()
    
    def _show_overview(self):
        """Show overall system health overview"""
        st.subheader("🌐 System Health Overview")
        
        # Quick status indicators
        col1, col2, col3, col4 = st.columns(4)
        
        # Database status
        with col1:
            db_status = self.db_health.get_quick_status()
            if db_status['connected']:
                st.success("✅ Database")
                st.caption(f"{db_status['table_count']}/{db_status['required_tables']} tables")
            else:
                st.error("❌ Database")
                st.caption("Not connected")
        
        # Ollama status
        with col2:
            ollama_status = self.ollama_health.get_quick_status()
            if ollama_status['connected']:
                st.success("✅ Ollama")
                st.caption(f"{ollama_status['model_count']} models")
            else:
                st.error("❌ Ollama")
                st.caption("Not running")
        
        # System resources
        with col3:
            sys_status = self.system_health.get_quick_status()
            if sys_status['healthy']:
                st.success("✅ Resources")
                st.caption(f"CPU: {sys_status['cpu']}%")
            else:
                st.warning("⚠️ Resources")
                st.caption("High usage")
        
        # Last check time
        with col4:
            st.info("🕐 Last Check")
            st.caption(datetime.now().strftime("%H:%M:%S"))
        
        # Health summary
        st.divider()
        
        # Recent issues
        st.subheader("⚠️ Recent Issues")
        issues = self._collect_recent_issues()
        
        if issues:
            for issue in issues[:5]:  # Show last 5 issues
                with st.container():
                    col1, col2, col3 = st.columns([3, 1, 1])
                    with col1:
                        st.warning(f"**{issue['component']}**: {issue['message']}")
                    with col2:
                        st.caption(issue['severity'])
                    with col3:
                        st.caption(issue['timestamp'])
        else:
            st.success("No recent issues detected")
        
        # Quick actions
        st.subheader("⚡ Quick Actions")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            if st.button("🔄 Refresh All", use_container_width=True):
                st.rerun()
        
        with col2:
            if st.button("💾 Save Health Report", use_container_width=True):
                self._save_health_report()
        
        with col3:
            if st.button("🧪 Run Diagnostics", use_container_width=True):
                self._run_diagnostics()
    
    def _collect_recent_issues(self):
        """Collect recent issues from all components"""
        issues = []
        
        # Check database issues
        db_status = self.db_health.get_quick_status()
        if not db_status['connected']:
            issues.append({
                'component': 'Database',
                'message': 'Connection failed',
                'severity': 'High',
                'timestamp': datetime.now().strftime("%H:%M")
            })
        elif db_status['missing_tables']:
            issues.append({
                'component': 'Database',
                'message': f"Missing {len(db_status['missing_tables'])} tables",
                'severity': 'Medium',
                'timestamp': datetime.now().strftime("%H:%M")
            })
        
        # Check Ollama issues
        ollama_status = self.ollama_health.get_quick_status()
        if not ollama_status['connected']:
            issues.append({
                'component': 'Ollama',
                'message': 'Service not running',
                'severity': 'High',
                'timestamp': datetime.now().strftime("%H:%M")
            })
        
        # Check system resources
        sys_status = self.system_health.get_quick_status()
        if sys_status['cpu'] > 90:
            issues.append({
                'component': 'System',
                'message': f"High CPU usage: {sys_status['cpu']}%",
                'severity': 'Medium',
                'timestamp': datetime.now().strftime("%H:%M")
            })
        
        return issues
    
    def _save_health_report(self):
        """Save comprehensive health report"""
        with st.spinner("Generating health report..."):
            report = {
                'timestamp': datetime.now().isoformat(),
                'database': self.db_health.get_detailed_status(),
                'ollama': self.ollama_health.get_detailed_status(),
                'system': self.system_health.get_detailed_status()
            }
            
            # Save to knowledge base
            if hasattr(st.session_state, 'db') and st.session_state.db:
                try:
                    import json
                    query_id = st.session_state.db.log_query(
                        tool="health_monitor",
                        model="system",
                        prompt="health_report",
                        response=json.dumps(report),
                        metadata={"type": "comprehensive_health_report"}
                    )
                    st.success("✅ Health report saved to knowledge base")
                except Exception as e:
                    st.error(f"Failed to save report: {e}")
            else:
                st.warning("Database not connected - report not saved")
    
    def _run_diagnostics(self):
        """Run comprehensive system diagnostics"""
        with st.spinner("Running diagnostics..."):
            # Database diagnostics
            st.subheader("🗄️ Database Diagnostics")
            db_diag = self.db_health.run_diagnostics()
            for test, result in db_diag.items():
                if result['passed']:
                    st.success(f"✅ {test}: {result['message']}")
                else:
                    st.error(f"❌ {test}: {result['message']}")
            
            # Ollama diagnostics
            st.subheader("🤖 Ollama Diagnostics")
            ollama_diag = self.ollama_health.run_diagnostics()
            for test, result in ollama_diag.items():
                if result['passed']:
                    st.success(f"✅ {test}: {result['message']}")
                else:
                    st.error(f"❌ {test}: {result['message']}")
            
            st.success("Diagnostics complete!")

# Create a page wrapper for Streamlit
def show():
    """Streamlit page entry point"""
    analyzer = HealthAnalyzer()
    analyzer.run()

if __name__ == "__main__":
    show()