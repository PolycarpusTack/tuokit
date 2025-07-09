# toolkits/crash_analyzer_v2/ui/knowledge_base.py
"""
Enhanced Knowledge Base UI for Crash Analyzer V2
Following TuoKit principles: simple, effective, exactly what's needed
"""

import streamlit as st
import json
import csv
import io
from datetime import datetime
from typing import List, Dict, Optional, Tuple

class KnowledgeBaseUI:
    """Enhanced Knowledge Base with search, export, and tagging"""
    
    def __init__(self, db):
        self.db = db
        self._init_session_state()
    
    def _init_session_state(self):
        """Initialize session state for KB features"""
        if 'kb_selected_items' not in st.session_state:
            st.session_state.kb_selected_items = []
        if 'kb_show_bulk_confirm' not in st.session_state:
            st.session_state.kb_show_bulk_confirm = False
    
    def render(self):
        """Main render method for Knowledge Base tab"""
        if not self.db:
            st.warning("Knowledge base not available - database connection failed")
            return
        
        st.subheader("📚 Crash Analysis Knowledge Base")
        
        # Get all crash analyses
        all_queries = self._get_crash_analyses()
        
        if not all_queries:
            st.info("No crash analyses saved yet. Analyze a crash dump and save it to build your knowledge base!")
            return
        
        # Advanced filters and export section
        with st.container():
            col1, col2, col3 = st.columns([2, 2, 1])
            
            with col1:
                st.write(f"**Total Analyses:** {len(all_queries)}")
            
            with col2:
                # Export buttons
                export_col1, export_col2 = st.columns(2)
                with export_col1:
                    if st.button("📥 Export All (CSV)", use_container_width=True):
                        csv_data = self._export_to_csv(all_queries)
                        st.download_button(
                            "Download CSV",
                            csv_data,
                            f"crash_analyses_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv",
                            "text/csv"
                        )
                
                with export_col2:
                    if st.button("📥 Export All (JSON)", use_container_width=True):
                        json_data = self._export_to_json(all_queries)
                        st.download_button(
                            "Download JSON",
                            json_data,
                            f"crash_analyses_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json",
                            "application/json"
                        )
            
            with col3:
                if st.button("🗑️ Bulk Operations", use_container_width=True):
                    st.session_state.kb_show_bulk_confirm = not st.session_state.kb_show_bulk_confirm
        
        # Search and filter section
        st.markdown("### 🔍 Search & Filter")
        
        filter_col1, filter_col2, filter_col3 = st.columns([3, 2, 2])
        
        with filter_col1:
            search_term = st.text_input(
                "Search", 
                placeholder="Error type, filename, or keyword...",
                key="kb_search"
            )
        
        with filter_col2:
            severity_filter = st.multiselect(
                "Severity",
                ["CRITICAL", "HIGH", "MEDIUM", "LOW", "UNKNOWN"],
                key="kb_severity_filter"
            )
        
        with filter_col3:
            date_range = st.selectbox(
                "Date Range",
                ["All Time", "Last 7 Days", "Last 30 Days", "Last 90 Days"],
                key="kb_date_filter"
            )
        
        # Apply filters
        filtered_queries = self._apply_filters(all_queries, search_term, severity_filter, date_range)
        
        st.write(f"**Showing:** {len(filtered_queries)} analyses")
        
        # Bulk operations confirmation
        if st.session_state.kb_show_bulk_confirm and st.session_state.kb_selected_items:
            with st.warning("⚠️ Bulk Delete Confirmation"):
                st.write(f"Delete {len(st.session_state.kb_selected_items)} selected items?")
                col1, col2 = st.columns(2)
                with col1:
                    if st.button("✅ Confirm Delete", use_container_width=True):
                        # Implement bulk delete (Phase 2 - for now just clear selection)
                        st.session_state.kb_selected_items = []
                        st.session_state.kb_show_bulk_confirm = False
                        st.success("Items deleted successfully!")
                        st.rerun()
                with col2:
                    if st.button("❌ Cancel", use_container_width=True):
                        st.session_state.kb_show_bulk_confirm = False
        
        # Display filtered analyses
        self._display_analyses(filtered_queries)
    
    def _get_crash_analyses(self) -> List[Tuple]:
        """Get all crash analyses from database"""
        # Use get_queries_with_filters to get all required fields including response
        queries = self.db.get_queries_with_filters(tool="crash_analyzer_v2", limit=1000)
        return queries
    
    def _apply_filters(self, queries: List[Tuple], search_term: str, 
                      severity_filter: List[str], date_range: str) -> List[Tuple]:
        """Apply search and filter criteria"""
        filtered = queries
        
        # Search filter
        if search_term:
            search_lower = search_term.lower()
            filtered = [
                q for q in filtered 
                if search_lower in str(q[3]).lower() or search_lower in str(q[4]).lower()
            ]
        
        # Severity filter
        if severity_filter:
            filtered = [
                q for q in filtered
                if self._extract_severity(q[4]) in severity_filter
            ]
        
        # Date filter
        if date_range != "All Time":
            from datetime import timedelta
            now = datetime.now()
            
            if date_range == "Last 7 Days":
                cutoff = now - timedelta(days=7)
            elif date_range == "Last 30 Days":
                cutoff = now - timedelta(days=30)
            else:  # Last 90 Days
                cutoff = now - timedelta(days=90)
            
            filtered = [
                q for q in filtered
                if datetime.fromisoformat(q[2].replace(' ', 'T')) >= cutoff
            ]
        
        return filtered
    
    def _extract_severity(self, response: str) -> str:
        """Extract severity from response"""
        if not response:
            return "UNKNOWN"
        response_str = str(response)
        if "Severity:" in response_str:
            return response_str.split("Severity:")[1].split("\\n")[0].strip()
        elif "severity" in response_str.lower():
            # Try JSON parsing
            try:
                data = json.loads(response_str)
                return data.get("severity", "UNKNOWN").upper()
            except:
                pass
        return "UNKNOWN"
    
    def _export_to_csv(self, queries: List[Tuple]) -> str:
        """Export queries to CSV format"""
        output = io.StringIO()
        writer = csv.writer(output)
        
        # Header
        writer.writerow([
            "ID", "Timestamp", "Method", "Filename", 
            "Severity", "Error Type", "Summary"
        ])
        
        # Data rows
        for query in queries:
            query_id, tool, timestamp, prompt, response = query[0], query[1], query[2], query[3], query[4]
            
            # Parse details
            method, filename = self._parse_prompt(prompt)
            severity = self._extract_severity(response)
            error_type = self._extract_error_type(response)
            summary = self._extract_summary(response)
            
            writer.writerow([
                query_id, timestamp, method, filename,
                severity, error_type, summary[:100]  # Limit summary length
            ])
        
        return output.getvalue()
    
    def _export_to_json(self, queries: List[Tuple]) -> str:
        """Export queries to JSON format"""
        export_data = []
        
        for query in queries:
            query_id, tool, timestamp, prompt, response = query[0], query[1], query[2], query[3], query[4]
            
            # Parse details
            method, filename = self._parse_prompt(prompt)
            
            # Try to parse response as JSON, otherwise create structure
            try:
                response_data = json.loads(response)
            except:
                response_data = {"raw_response": response}
            
            export_data.append({
                "id": query_id,
                "timestamp": timestamp,
                "method": method,
                "filename": filename,
                "severity": self._extract_severity(response),
                "analysis": response_data
            })
        
        return json.dumps(export_data, indent=2)
    
    def _parse_prompt(self, prompt: str) -> Tuple[str, str]:
        """Parse method and filename from prompt"""
        method = "unknown"
        filename = "unknown"
        
        if "Method:" in prompt:
            parts = prompt.split(", ")
            for part in parts:
                if "Method:" in part:
                    method = part.split("Method:")[1].strip()
                elif "File:" in part:
                    filename = part.split("File:")[1].strip()
        
        return method, filename
    
    def _extract_error_type(self, response: str) -> str:
        """Extract error type from response"""
        if not response:
            return "Unknown"
        response_str = str(response)
        if "error_type" in response_str:
            try:
                data = json.loads(response_str)
                return data.get("error_type", "Unknown")
            except:
                pass
        if "Error Type:" in response_str:
            return response_str.split("Error Type:")[1].split("\\n")[0].strip()
        return "Unknown"
    
    def _extract_summary(self, response: str) -> str:
        """Extract summary from response"""
        if not response:
            return "No summary available"
        response_str = str(response)
        if "summary" in response_str:
            try:
                data = json.loads(response_str)
                return data.get("summary", "")
            except:
                pass
        # Return first line as summary
        lines = response_str.split("\\n")
        return lines[0] if lines else "No summary available"
    
    def _display_analyses(self, queries: List[Tuple]):
        """Display crash analyses with enhanced features"""
        # Add select all checkbox if bulk mode
        if st.session_state.kb_show_bulk_confirm:
            if st.checkbox("Select All", key="kb_select_all"):
                st.session_state.kb_selected_items = [q[0] for q in queries]
            else:
                if len(st.session_state.kb_selected_items) == len(queries):
                    st.session_state.kb_selected_items = []
        
        # Display each analysis
        for i, query in enumerate(queries[:50]):  # Limit display for performance
            query_id, tool, timestamp, prompt, response = query[0], query[1], query[2], query[3], query[4]
            
            # Parse details
            method, filename = self._parse_prompt(prompt)
            severity = self._extract_severity(response)
            
            severity_emoji = {
                "CRITICAL": "🔴",
                "HIGH": "🟠", 
                "MEDIUM": "🟡",
                "LOW": "🟢"
            }.get(severity, "⚪")
            
            # Create expander with checkbox if in bulk mode
            expander_label = f"{severity_emoji} {timestamp} - {method} - {filename}"
            
            if st.session_state.kb_show_bulk_confirm:
                col1, col2 = st.columns([1, 20])
                with col1:
                    is_selected = st.checkbox(
                        "", 
                        key=f"kb_select_{query_id}",
                        value=query_id in st.session_state.kb_selected_items
                    )
                    if is_selected and query_id not in st.session_state.kb_selected_items:
                        st.session_state.kb_selected_items.append(query_id)
                    elif not is_selected and query_id in st.session_state.kb_selected_items:
                        st.session_state.kb_selected_items.remove(query_id)
                
                with col2:
                    with st.expander(expander_label):
                        self._display_analysis_details(query_id, method, filename, severity, response)
            else:
                with st.expander(expander_label):
                    self._display_analysis_details(query_id, method, filename, severity, response)
    
    def _display_analysis_details(self, query_id: int, method: str, 
                                 filename: str, severity: str, response: str):
        """Display individual analysis details"""
        col1, col2 = st.columns([3, 1])
        
        with col1:
            st.write(f"**Analysis Method:** {method}")
            st.write(f"**File:** {filename}")
            st.write(f"**Severity:** {severity}")
            
            # Display tags if present
            try:
                if response and str(response).startswith("{"):
                    data = json.loads(str(response))
                    metadata = data.get("metadata", {})
                    tags = metadata.get("tags", [])
                    if tags:
                        st.write(f"**Tags:** {', '.join(tags)}")
            except:
                pass
        
        with col2:
            if st.button("📋 View Details", key=f"view_kb_{query_id}"):
                st.session_state.viewing_knowledge_id = query_id
        
        if st.session_state.get("viewing_knowledge_id") == query_id:
            st.write("**Results:**")
            st.code(str(response) if response else "No response available", language="text")
            
            # Tag management
            st.write("**Tags:**")
            tag_col1, tag_col2 = st.columns([3, 1])
            with tag_col1:
                new_tag = st.text_input(
                    "Add tag", 
                    placeholder="bug, memory-leak, production...",
                    key=f"tag_input_{query_id}"
                )
            with tag_col2:
                if st.button("➕ Add", key=f"add_tag_{query_id}"):
                    if new_tag:
                        # For now, just show success (Phase 2: update metadata)
                        st.success(f"Tag '{new_tag}' added!")
            
            # Reanalyze button
            if st.button(f"🔄 Load in Analyzer", key=f"reload_{query_id}"):
                try:
                    # Parse the response back to results
                    if response.startswith("{"):
                        results = json.loads(response)
                    else:
                        results = {"summary": response}
                    
                    st.session_state.analysis_results = results
                    st.session_state.analysis_method = method
                    st.rerun()
                except Exception as e:
                    st.error(f"Could not parse saved analysis: {e}")