# pages/smalltalk_toolkit.py
"""
Thin wrapper for SmallTalk Development Toolkit
Migrated to modular structure in toolkits/smalltalk/
"""

from toolkits.smalltalk import SmallTalkToolkit

# Create and run the SmallTalk toolkit
if __name__ == "__main__":
    toolkit = SmallTalkToolkit()
    toolkit.run()