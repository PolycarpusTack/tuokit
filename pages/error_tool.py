# pages/error_tool.py
"""
Thin wrapper for Error Decoder toolkit
Migrated to modular structure in toolkits/error_decoder/
"""

from toolkits.error_decoder import ErrorDecoder

# Create and run the error decoder
if __name__ == "__main__":
    decoder = ErrorDecoder()
    decoder.run()