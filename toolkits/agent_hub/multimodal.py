"""
Multi-Modal Support - Screenshot and Document Analysis
Practical implementation for broadcast software teams
"""

from typing import Dict, Any, Optional, List
import base64
import io
from pathlib import Path
import streamlit as st

# Optional imports - work without them
try:
    from PIL import Image
    PIL_AVAILABLE = True
except ImportError:
    PIL_AVAILABLE = False

try:
    import pytesseract
    TESSERACT_AVAILABLE = True
except ImportError:
    TESSERACT_AVAILABLE = False

try:
    import PyPDF2
    PYPDF2_AVAILABLE = True
except ImportError:
    PYPDF2_AVAILABLE = False

from .core import BaseAgent, AgentType
from utils import safe_ollama_generate, capture_knowledge


class ScreenshotAnalyzer:
    """Analyzes screenshots of error messages and UIs"""
    
    @staticmethod
    def extract_text_from_image(image_path: str) -> str:
        """Extract text from screenshot using OCR"""
        if not TESSERACT_AVAILABLE or not PIL_AVAILABLE:
            return "[OCR not available - install pillow and pytesseract]"
        
        try:
            image = Image.open(image_path)
            text = pytesseract.image_to_string(image)
            return text.strip()
        except Exception as e:
            return f"Error extracting text: {str(e)}"
    
    @staticmethod
    def analyze_screenshot(image_path: str, context: str = "") -> Dict[str, Any]:
        """Analyze screenshot for errors and UI issues"""
        
        # Extract text first
        extracted_text = ScreenshotAnalyzer.extract_text_from_image(image_path)
        
        # Build analysis prompt
        prompt = f"""
Analyze this screenshot from a broadcast software interface:

Extracted Text:
{extracted_text}

Context: {context}

Please identify:
1. Any error messages or warnings
2. UI component that seems problematic  
3. Potential causes based on the interface
4. Suggested troubleshooting steps

Focus on broadcast-specific issues like:
- Streaming errors (RTMP, HLS, WebRTC)
- Timeline/scheduling issues
- Signal quality warnings
- Configuration problems
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        return {
            'extracted_text': extracted_text,
            'analysis': response['response'],
            'screenshot_path': image_path,
            'context': context
        }
    
    @staticmethod
    def analyze_whats_on_interface(image_path: str, client: str = "") -> Dict[str, Any]:
        """Specific analysis for WHAT'S ON interface screenshots"""
        
        extracted_text = ScreenshotAnalyzer.extract_text_from_image(image_path)
        
        prompt = f"""
Analyze this WHAT'S ON broadcast interface screenshot:

Extracted Text:
{extracted_text}

Client: {client}

Check for:
1. Schedule conflicts or gaps
2. Missing or incorrect program information
3. Time zone issues
4. Signal status indicators
5. Any error messages or warnings

Provide broadcast-specific recommendations.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        return {
            'interface': 'WHATS_ON',
            'client': client,
            'extracted_text': extracted_text,
            'analysis': response['response']
        }


class DocumentAnalyzer:
    """Analyzes PDFs and documents for legal and compliance"""
    
    @staticmethod
    def extract_text_from_pdf(pdf_path: str) -> str:
        """Extract text from PDF document"""
        if not PYPDF2_AVAILABLE:
            return "[PDF extraction not available - install PyPDF2]"
        
        try:
            text = ""
            with open(pdf_path, 'rb') as file:
                pdf_reader = PyPDF2.PdfReader(file)
                for page_num in range(len(pdf_reader.pages)):
                    page = pdf_reader.pages[page_num]
                    text += page.extract_text() + "\n"
            return text.strip()
        except Exception as e:
            return f"Error extracting PDF text: {str(e)}"
    
    @staticmethod
    def analyze_legal_document(pdf_path: str, doc_type: str = "contract") -> Dict[str, Any]:
        """Analyze legal documents for compliance and key terms"""
        
        extracted_text = DocumentAnalyzer.extract_text_from_pdf(pdf_path)
        
        prompt = f"""
Analyze this {doc_type} for a broadcast software company:

Document excerpt (first 2000 chars):
{extracted_text[:2000]}

Focus on:
1. Licensing terms and restrictions
2. Liability and indemnification clauses
3. Data privacy and security requirements
4. Broadcast-specific regulations (FCC, etc.)
5. Payment terms and penalties
6. Termination conditions

Highlight any concerns for broadcast software operations.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        return {
            'document_type': doc_type,
            'text_length': len(extracted_text),
            'analysis': response['response'],
            'extracted_text': extracted_text[:500] + "..." if len(extracted_text) > 500 else extracted_text
        }
    
    @staticmethod
    def analyze_compliance_document(pdf_path: str, regulations: List[str] = None) -> Dict[str, Any]:
        """Check document against specific regulations"""
        
        if regulations is None:
            regulations = ["FCC", "GDPR", "CCPA", "COPPA"]
        
        extracted_text = DocumentAnalyzer.extract_text_from_pdf(pdf_path)
        
        prompt = f"""
Review this document for compliance with broadcast regulations:

Regulations to check: {', '.join(regulations)}

Document excerpt:
{extracted_text[:3000]}

For each regulation, identify:
1. Relevant clauses or sections
2. Compliance status (Compliant/Non-compliant/Needs review)
3. Specific concerns or gaps
4. Recommended actions

Focus on broadcast industry requirements.
"""
        
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        
        return {
            'regulations_checked': regulations,
            'analysis': response['response'],
            'document_length': len(extracted_text)
        }


class MultiModalAgent(BaseAgent):
    """Agent that handles screenshots and documents"""
    
    def __init__(self):
        super().__init__(
            name="MultiModal Analyst",
            description="Analyzes screenshots and documents for broadcast teams",
            tools=["analyze_screenshot", "analyze_document", "analyze_whats_on", "check_compliance"],
            agent_type=AgentType.SPECIALIST
        )
        self.screenshot_analyzer = ScreenshotAnalyzer()
        self.document_analyzer = DocumentAnalyzer()
    
    def _initialize_tools(self) -> Dict[str, Any]:
        return {
            "analyze_screenshot": self._analyze_screenshot,
            "analyze_document": self._analyze_document,
            "analyze_whats_on": self._analyze_whats_on,
            "check_compliance": self._check_compliance
        }
    
    def _analyze_screenshot(self, params: Dict) -> Dict[str, Any]:
        """Analyze error screenshot"""
        image_path = params.get('image_path', '')
        context = params.get('context', '')
        client = params.get('client', '')
        
        result = self.screenshot_analyzer.analyze_screenshot(image_path, context)
        
        # Store in memory for future reference
        if self.db:
            from .memory import AgentMemory, BroadcastMemoryPatterns
            memory = AgentMemory()
            
            # Extract key error info
            if 'error' in result['extracted_text'].lower():
                BroadcastMemoryPatterns.remember_customer_issue(
                    memory,
                    client or "Unknown",
                    f"Screenshot: {result['extracted_text'][:100]}",
                    result['analysis'][:200]
                )
        
        return result
    
    def _analyze_document(self, params: Dict) -> Dict[str, Any]:
        """Analyze legal document"""
        pdf_path = params.get('pdf_path', '')
        doc_type = params.get('doc_type', 'contract')
        
        return self.document_analyzer.analyze_legal_document(pdf_path, doc_type)
    
    def _analyze_whats_on(self, params: Dict) -> Dict[str, Any]:
        """Analyze WHAT'S ON interface screenshot"""
        image_path = params.get('image_path', '')
        client = params.get('client', '')
        
        return self.screenshot_analyzer.analyze_whats_on_interface(image_path, client)
    
    def _check_compliance(self, params: Dict) -> Dict[str, Any]:
        """Check document compliance"""
        pdf_path = params.get('pdf_path', '')
        regulations = params.get('regulations', ["FCC", "GDPR"])
        
        result = self.document_analyzer.analyze_compliance_document(pdf_path, regulations)
        
        # Store compliance check in memory
        if self.db:
            from .memory import AgentMemory, BroadcastMemoryPatterns
            memory = AgentMemory()
            
            BroadcastMemoryPatterns.remember_compliance_check(
                memory,
                pdf_path.split('/')[-1],  # Just filename
                ', '.join(regulations),
                "Analyzed",
                result['analysis'][:200]
            )
        
        return result


# Streamlit UI components for multimodal
def render_screenshot_analyzer(st):
    """Streamlit UI for screenshot analysis"""
    st.subheader("📸 Screenshot Analyzer")
    
    # Client selection
    client = st.selectbox(
        "Client (optional)",
        ["", "ESPN", "NBC", "CBS", "ABC", "Fox", "HBO", "Discovery", "Other"]
    )
    
    # File uploader
    uploaded_file = st.file_uploader(
        "Upload screenshot",
        type=['png', 'jpg', 'jpeg', 'gif', 'bmp'],
        help="Upload error messages or WHAT'S ON interface screenshots"
    )
    
    # Context
    context = st.text_area(
        "Additional context",
        placeholder="What was happening when this error occurred?",
        height=100
    )
    
    if uploaded_file is not None:
        # Display image
        st.image(uploaded_file, caption="Uploaded Screenshot", use_column_width=True)
        
        # Analyze button
        if st.button("🔍 Analyze Screenshot", type="primary"):
            with st.spinner("Analyzing screenshot..."):
                # Save temporarily
                temp_path = f"temp_{uploaded_file.name}"
                with open(temp_path, "wb") as f:
                    f.write(uploaded_file.getbuffer())
                
                # Create agent and analyze
                agent = MultiModalAgent()
                
                # Detect if it's WHAT'S ON interface
                if "whats" in uploaded_file.name.lower() or "schedule" in context.lower():
                    result = agent.execute_tool("analyze_whats_on", {
                        'image_path': temp_path,
                        'client': client
                    })
                else:
                    result = agent.execute_tool("analyze_screenshot", {
                        'image_path': temp_path,
                        'context': context,
                        'client': client
                    })
                
                # Display results
                st.success("Analysis complete!")
                
                with st.expander("📝 Extracted Text", expanded=True):
                    st.text(result.get('extracted_text', 'No text extracted'))
                
                st.subheader("🎯 Analysis")
                st.write(result.get('analysis', 'No analysis available'))
                
                # Clean up
                import os
                os.remove(temp_path)


def render_document_analyzer(st):
    """Streamlit UI for document analysis"""
    st.subheader("📄 Document Analyzer")
    
    # Document type
    doc_type = st.selectbox(
        "Document Type",
        ["contract", "license", "compliance_report", "technical_spec", "legal_notice"]
    )
    
    # File uploader
    uploaded_file = st.file_uploader(
        "Upload PDF document",
        type=['pdf'],
        help="Upload contracts, licenses, or compliance documents"
    )
    
    # Compliance check option
    check_compliance = st.checkbox("Check regulatory compliance")
    
    if check_compliance:
        regulations = st.multiselect(
            "Regulations to check",
            ["FCC", "GDPR", "CCPA", "COPPA", "HIPAA", "SOC2"],
            default=["FCC", "GDPR"]
        )
    
    if uploaded_file is not None:
        st.info(f"📎 Uploaded: {uploaded_file.name}")
        
        # Analyze button
        if st.button("📑 Analyze Document", type="primary"):
            with st.spinner("Analyzing document..."):
                # Save temporarily
                temp_path = f"temp_{uploaded_file.name}"
                with open(temp_path, "wb") as f:
                    f.write(uploaded_file.getbuffer())
                
                # Create agent and analyze
                agent = MultiModalAgent()
                
                if check_compliance:
                    result = agent.execute_tool("check_compliance", {
                        'pdf_path': temp_path,
                        'regulations': regulations
                    })
                else:
                    result = agent.execute_tool("analyze_document", {
                        'pdf_path': temp_path,
                        'doc_type': doc_type
                    })
                
                # Display results
                st.success("Analysis complete!")
                
                st.subheader("📊 Analysis Results")
                st.write(result.get('analysis', 'No analysis available'))
                
                if 'regulations_checked' in result:
                    st.subheader("✅ Compliance Status")
                    for reg in result['regulations_checked']:
                        st.write(f"**{reg}**: Check analysis above for details")
                
                # Clean up
                import os
                os.remove(temp_path)


# Client-specific patterns
class ClientPatterns:
    """Patterns for specific broadcast clients"""
    
    # Common client configurations
    CLIENTS = {
        "ESPN": {
            "protocols": ["RTMP", "HLS"],
            "common_issues": ["stream drops", "audio sync", "scoreboard overlay"],
            "ui_elements": ["live ticker", "score bug", "stats panel"]
        },
        "NBC": {
            "protocols": ["HLS", "DASH"],
            "common_issues": ["peacock integration", "local affiliate feeds", "time zone sync"],
            "ui_elements": ["program guide", "local news insert", "weather graphics"]
        },
        "CBS": {
            "protocols": ["RTMP", "SRT"],
            "common_issues": ["paramount+ sync", "NFL graphics", "commercial insertion"],
            "ui_elements": ["sports ticker", "breaking news banner", "station ID"]
        },
        "Discovery": {
            "protocols": ["HLS", "WebRTC"],
            "common_issues": ["multi-language audio", "subtitle sync", "4K streaming"],
            "ui_elements": ["show info overlay", "next episode prompt", "channel bug"]
        }
    }
    
    @staticmethod
    def get_client_context(client: str) -> Dict[str, Any]:
        """Get client-specific context for analysis"""
        return ClientPatterns.CLIENTS.get(client, {
            "protocols": ["RTMP", "HLS"],
            "common_issues": ["general streaming", "signal quality"],
            "ui_elements": ["standard interface"]
        })
    
    @staticmethod
    def analyze_with_client_context(screenshot_text: str, client: str) -> str:
        """Add client-specific insights to analysis"""
        context = ClientPatterns.get_client_context(client)
        
        insights = []
        
        # Check for protocol-specific issues
        for protocol in context["protocols"]:
            if protocol.lower() in screenshot_text.lower():
                insights.append(f"- {client} primarily uses {protocol}, check configuration")
        
        # Check for known issues
        for issue in context["common_issues"]:
            if any(word in screenshot_text.lower() for word in issue.split()):
                insights.append(f"- Common {client} issue: {issue}")
        
        return "\n".join(insights) if insights else "No client-specific patterns detected"
