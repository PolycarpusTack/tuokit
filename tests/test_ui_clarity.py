# tests/test_ui_clarity.py
"""
Test UI clarity for Crash Analyzer V2
Quality Gate 2: Verify no misleading UI elements remain
"""

import pytest
import re
from pathlib import Path

class TestUIClarityQualityGate:
    """Test that UI elements are clear and not misleading"""
    
    def test_phase_2_features_clearly_marked(self):
        """Test that Phase 2 features are clearly labeled"""
        dashboard_path = Path("/mnt/c/Projects/Tuokit/toolkits/crash_analyzer_v2/analytics/dashboard.py")
        
        with open(dashboard_path, 'r') as f:
            content = f.read()
        
        # Check email button is marked as Phase 2
        assert "Email Report (Phase 2)" in content
        assert 'disabled=True' in content.split("Email Report (Phase 2)")[1].split('\n')[0]
        
        # Check report settings marked as Phase 2
        assert "Report Settings (Phase 2)" in content
        
        # Check automated reports section
        assert "Automated Reports Configuration - Coming Soon" in content
        assert "Phase 2 Feature" in content
        
        # Verify preview fields are disabled
        assert 'Report Frequency (Preview)' in content
        assert 'Email Recipients (Preview)' in content
        
        # Count disabled=True occurrences in Phase 2 sections
        phase2_sections = re.findall(r'Phase 2.*?(?=\n\s*(?:def|class|with|$))', content, re.DOTALL)
        for section in phase2_sections:
            if 'st.button' in section or 'st.selectbox' in section or 'st.text_input' in section:
                # Should have disabled=True if it's an interactive element
                if not ('disabled=True' in section or 'st.info' in section or 'st.write' in section):
                    pytest.fail(f"Found Phase 2 UI element without disabled=True")
    
    def test_no_coming_soon_without_context(self):
        """Test that 'coming soon' always has Phase 2 context"""
        analytics_path = Path("/mnt/c/Projects/Tuokit/toolkits/crash_analyzer_v2/analytics")
        
        for py_file in analytics_path.rglob("*.py"):
            with open(py_file, 'r') as f:
                content = f.read()
            
            # Find all "coming soon" instances
            coming_soon_matches = re.finditer(r'(?i)coming\s+(soon|in\s+phase)', content)
            
            for match in coming_soon_matches:
                # Get context around match (100 chars before and after)
                start = max(0, match.start() - 100)
                end = min(len(content), match.end() + 100)
                context = content[start:end]
                
                # Should have Phase 2 or clear feature description nearby
                if not re.search(r'(?i)(phase\s*2|planned\s+features?|future\s+enhancement)', context):
                    pytest.fail(f"'Coming soon' without clear Phase 2 context in {py_file.name}")
    
    def test_no_stub_implementations(self):
        """Test that there are no TODO or stub implementations in UI"""
        dashboard_path = Path("/mnt/c/Projects/Tuokit/toolkits/crash_analyzer_v2/analytics/dashboard.py")
        
        with open(dashboard_path, 'r') as f:
            lines = f.readlines()
        
        for i, line in enumerate(lines):
            # Check for common stub patterns
            if any(pattern in line.lower() for pattern in ['todo:', 'fixme:', 'hack:', 'xxx:']):
                # Exceptions for documented Phase 2 features
                if 'phase 2' not in lines[max(0, i-2):min(len(lines), i+3)]:
                    pytest.fail(f"Found unmarked TODO/stub on line {i+1}: {line.strip()}")
    
    def test_trend_indicators_complete(self):
        """Test that trend indicators are properly implemented"""
        dashboard_path = Path("/mnt/c/Projects/Tuokit/toolkits/crash_analyzer_v2/analytics/dashboard.py")
        
        with open(dashboard_path, 'r') as f:
            content = f.read()
        
        # Check all trend emoji types are used
        required_emojis = ["📈", "📉", "➡️", "🆕"]
        for emoji in required_emojis:
            assert emoji in content, f"Missing trend emoji: {emoji}"
        
        # Verify no hardcoded trend indicators
        assert 'trend_indicator = "📈"  # Simplified for now' not in content
        
        # Check trend calculation logic exists
        assert "if prev_count == 0:" in content
        assert "elif current_count > prev_count * 1.2:" in content
        assert "elif current_count < prev_count * 0.8:" in content
        assert "else:" in content  # For stable trend
    
    def test_informative_messages(self):
        """Test that all info/warning messages are informative"""
        dashboard_path = Path("/mnt/c/Projects/Tuokit/toolkits/crash_analyzer_v2/analytics/dashboard.py")
        
        with open(dashboard_path, 'r') as f:
            content = f.read()
        
        # Find all st.info, st.warning, st.error calls
        message_patterns = [
            r'st\.info\("([^"]+)"\)',
            r'st\.warning\("([^"]+)"\)',
            r'st\.error\("([^"]+)"\)'
        ]
        
        for pattern in message_patterns:
            matches = re.findall(pattern, content)
            for message in matches:
                # Check message is informative (not just "Coming soon")
                if len(message) < 10:
                    pytest.fail(f"Message too short to be informative: '{message}'")
                
                # If it mentions Phase 2, should explain what it is
                if 'phase 2' in message.lower() and 'feature' not in message.lower():
                    pytest.fail(f"Phase 2 mentioned without context: '{message}'")

if __name__ == "__main__":
    pytest.main([__file__, "-v"])