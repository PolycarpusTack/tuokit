"""
Test the clipboard functionality
Run with: streamlit run test_clipboard.py
"""
import streamlit as st
import sys
from pathlib import Path

# Add tools directory to path
sys.path.append(str(Path(__file__).parent))
from clipboard_utils import create_copy_button, display_prompt_with_copy

st.title("📋 Clipboard Functionality Test")

st.markdown("### Test 1: Simple Copy Button")
test_text = """This is a test prompt.
It has multiple lines.
Special characters: "quotes" and 'apostrophes'
Even some code: if x > 5: print("Hello")"""

st.text_area("Test content:", test_text, height=100)
create_copy_button(test_text, button_id="test1")

st.divider()

st.markdown("### Test 2: Full Prompt Display")
ai_prompt = """Please implement the following TODO:

📍 Location: test.py line 42
📝 Task: Add error handling

Requirements:
1. Catch specific exceptions
2. Log errors appropriately
3. Return meaningful error messages

Example:
```python
try:
    result = risky_operation()
except ValueError as e:
    logger.error(f"Invalid value: {e}")
    return None
```"""

display_prompt_with_copy(ai_prompt, title="Test AI Prompt", key_prefix="test2")

st.divider()
st.info("💡 Click the copy buttons above to test clipboard functionality!")
