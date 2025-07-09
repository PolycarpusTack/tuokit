"""
Rails Debugging Tools
Error analysis, performance profiling, and query optimization
"""

import streamlit as st
from typing import Dict, Any, List, Optional
import re

from utils.ollama import safe_ollama_generate
from .config import ERROR_PATTERNS, PERFORMANCE_THRESHOLDS

class ErrorAnalyzer:
    """Analyze Rails errors and provide solutions"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
        self.error_patterns = ERROR_PATTERNS
    
    def analyze_error(self, error_log: str) -> Dict[str, Any]:
        """Analyze Rails error and provide solutions"""
        # Detect error type
        error_type = self._detect_error_type(error_log)
        
        prompt = f"""Analyze this Rails error and provide solutions:

{error_log}

Provide:
1. Root cause explanation
2. Step-by-step solution
3. Code examples to fix the issue
4. Prevention strategies
5. Related documentation links

Focus on practical, actionable solutions."""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="You are a Rails debugging expert. Provide clear, practical solutions.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Analysis failed")
            }
        
        return {
            "success": True,
            "error_type": error_type,
            "analysis": response.get("response", ""),
            "quick_fix": self._extract_quick_fix(response.get("response", ""))
        }
    
    def _detect_error_type(self, error_log: str) -> Dict[str, str]:
        """Detect the type of error from the log"""
        for error_key, error_config in self.error_patterns.items():
            if re.search(error_config["pattern"], error_log, re.IGNORECASE):
                return {
                    "type": error_key,
                    "category": error_config["category"],
                    "icon": error_config["icon"]
                }
        
        return {
            "type": "unknown",
            "category": "Unknown Error",
            "icon": "❓"
        }
    
    def _extract_quick_fix(self, analysis: str) -> Optional[str]:
        """Extract quick fix from analysis"""
        # Look for code blocks or specific fix patterns
        code_match = re.search(r'```ruby\n(.*?)\n```', analysis, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Look for numbered steps
        fix_match = re.search(r'(?:Quick fix|Solution):\s*\n?\s*(.+?)(?:\n\n|$)', analysis, re.IGNORECASE)
        if fix_match:
            return fix_match.group(1)
        
        return None
    
    def show_interface(self):
        """Show error analyzer interface"""
        st.markdown("### 🐛 Rails Error Analyzer")
        
        error_input = st.text_area(
            "Paste your error log",
            height=300,
            placeholder="Paste Rails error stack trace or log here..."
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            include_stack_trace = st.checkbox("Full stack trace included", value=True)
            include_env_info = st.checkbox("Include environment info", value=False)
        
        with col2:
            rails_version = st.selectbox("Rails Version", ["7.1", "7.0", "6.1", "6.0"])
            ruby_version = st.selectbox("Ruby Version", ["3.2", "3.1", "3.0", "2.7"])
        
        if st.button("🔍 Analyze Error", type="primary", use_container_width=True):
            if error_input:
                with st.spinner("Analyzing error..."):
                    result = self.analyze_error(error_input)
                    
                    if result.get('success'):
                        # Show error type
                        error_type = result.get('error_type', {})
                        st.info(f"{error_type.get('icon', '❓')} **{error_type.get('category', 'Unknown Error')}**")
                        
                        # Quick fix if available
                        if result.get('quick_fix'):
                            st.success("**Quick Fix:**")
                            st.code(result['quick_fix'], language='ruby')
                        
                        # Full analysis
                        st.markdown("**Detailed Analysis:**")
                        st.markdown(result.get('analysis', ''))
                    else:
                        st.error(f"Analysis failed: {result.get('error')}")
            else:
                st.warning("Please paste an error log to analyze")


class PerformanceAnalyzer:
    """Analyze Rails performance issues"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
        self.thresholds = PERFORMANCE_THRESHOLDS
    
    def analyze_performance_log(self, log: str) -> Dict[str, Any]:
        """Analyze performance from Rails logs"""
        # Extract metrics
        metrics = self._extract_metrics(log)
        
        prompt = f"""Analyze this Rails performance log:

{log}

Identified metrics:
- Response time: {metrics.get('response_time', 'Unknown')}ms
- Database time: {metrics.get('db_time', 'Unknown')}ms
- View time: {metrics.get('view_time', 'Unknown')}ms

Provide:
1. Performance bottlenecks identification
2. Optimization strategies
3. Code improvements with examples
4. Caching recommendations
5. Database optimization tips"""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="You are a Rails performance expert. Provide actionable optimization advice.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Analysis failed")
            }
        
        return {
            "success": True,
            "metrics": metrics,
            "analysis": response.get("response", ""),
            "recommendations": self._extract_recommendations(response.get("response", ""))
        }
    
    def _extract_metrics(self, log: str) -> Dict[str, Any]:
        """Extract performance metrics from log"""
        metrics = {}
        
        # Response time
        time_match = re.search(r'Completed.*in\s+(\d+(?:\.\d+)?)ms', log)
        if time_match:
            metrics['response_time'] = float(time_match.group(1))
        
        # Database time
        db_match = re.search(r'ActiveRecord:\s*(\d+(?:\.\d+)?)ms', log)
        if db_match:
            metrics['db_time'] = float(db_match.group(1))
        
        # View time
        view_match = re.search(r'Views:\s*(\d+(?:\.\d+)?)ms', log)
        if view_match:
            metrics['view_time'] = float(view_match.group(1))
        
        # Allocations
        alloc_match = re.search(r'Allocations:\s*(\d+)', log)
        if alloc_match:
            metrics['allocations'] = int(alloc_match.group(1))
        
        return metrics
    
    def _extract_recommendations(self, analysis: str) -> List[str]:
        """Extract key recommendations"""
        recommendations = []
        
        # Look for numbered recommendations
        rec_pattern = r'\d+\.\s*([^\n]+)'
        matches = re.findall(rec_pattern, analysis)
        
        for match in matches[:5]:  # Top 5 recommendations
            recommendations.append(match.strip())
        
        return recommendations
    
    def show_interface(self):
        """Show performance analyzer interface"""
        st.markdown("### 📈 Rails Performance Analyzer")
        
        log_input = st.text_area(
            "Paste Rails log or performance data",
            height=200,
            placeholder="Paste Rails server log, New Relic data, or performance metrics..."
        )
        
        # Quick metrics input
        st.markdown("**Or enter metrics manually:**")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            response_time = st.number_input("Response Time (ms)", min_value=0)
        
        with col2:
            db_time = st.number_input("Database Time (ms)", min_value=0)
        
        with col3:
            view_time = st.number_input("View Time (ms)", min_value=0)
        
        analysis_type = st.radio(
            "Analysis Focus",
            ["General", "Database", "Caching", "Memory", "N+1 Queries"],
            horizontal=True
        )
        
        if st.button("🚀 Analyze Performance", type="primary", use_container_width=True):
            if log_input or (response_time or db_time or view_time):
                with st.spinner("Analyzing performance..."):
                    # Build log from manual input if needed
                    if not log_input and (response_time or db_time or view_time):
                        log_input = f"Completed in {response_time}ms (ActiveRecord: {db_time}ms | Views: {view_time}ms)"
                    
                    result = self.analyze_performance_log(log_input)
                    
                    if result.get('success'):
                        # Show metrics
                        metrics = result.get('metrics', {})
                        if metrics:
                            cols = st.columns(len(metrics))
                            for i, (key, value) in enumerate(metrics.items()):
                                with cols[i]:
                                    st.metric(key.replace('_', ' ').title(), f"{value}")
                        
                        # Performance status
                        response_time = metrics.get('response_time', 0)
                        if response_time > 1000:
                            st.error("⚠️ Slow response time detected!")
                        elif response_time > 500:
                            st.warning("⚡ Response time could be improved")
                        else:
                            st.success("✅ Good response time")
                        
                        # Recommendations
                        if result.get('recommendations'):
                            st.markdown("**Top Recommendations:**")
                            for rec in result['recommendations']:
                                st.write(f"• {rec}")
                        
                        # Full analysis
                        with st.expander("Detailed Analysis", expanded=True):
                            st.markdown(result.get('analysis', ''))
                    else:
                        st.error(f"Analysis failed: {result.get('error')}")
            else:
                st.warning("Please provide log data or metrics to analyze")


class QueryOptimizer:
    """Optimize Rails database queries"""
    
    def __init__(self):
        self.model = st.session_state.get('selected_model', 'deepseek-coder:6.7b')
    
    def optimize_query(self, query: str, schema: str = "") -> Dict[str, Any]:
        """Optimize a database query"""
        prompt = f"""Optimize this Rails/SQL query:

Query:
{query}

{"Schema:" + schema if schema else ""}

Provide:
1. Optimized query version
2. Explanation of optimizations
3. Index recommendations
4. ActiveRecord equivalent if SQL
5. Performance impact estimate"""
        
        response = safe_ollama_generate(
            model=self.model,
            prompt=prompt,
            system="You are a database optimization expert. Provide query optimizations for Rails.",
            options={"temperature": 0.1}
        )
        
        if response.get("error"):
            return {
                "success": False,
                "error": response.get("response", "Optimization failed")
            }
        
        return {
            "success": True,
            "optimization": response.get("response", "")
        }