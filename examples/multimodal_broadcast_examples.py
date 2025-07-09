"""
MultiModal Examples for Broadcast Teams
Practical screenshot and document analysis
"""

from toolkits.agent_hub import (
    MultiModalAgent, ScreenshotAnalyzer, DocumentAnalyzer,
    AgentMemory, BroadcastMemoryPatterns, ClientPatterns
)


def example_screenshot_debugging():
    """Example: Support analyzing ESPN error screenshot"""
    
    print("=== Screenshot Debugging Example ===\n")
    
    # Create multimodal agent
    agent = MultiModalAgent()
    memory = AgentMemory()
    
    # Simulate screenshot analysis
    # In real use, this would be an actual image file
    mock_screenshot_text = """
    ESPN Broadcast Control
    ERROR: RTMP Connection Lost
    Stream ID: ESPN-MNF-2024
    Timestamp: 22:45:31
    Bitrate: 0 kbps
    Buffer: Empty
    Retry attempts: 5/5 failed
    """
    
    # Get client-specific insights
    client_insights = ClientPatterns.analyze_with_client_context(
        mock_screenshot_text, 
        "ESPN"
    )
    
    print(f"Client-specific insights:\n{client_insights}\n")
    
    # Analyze the screenshot
    result = {
        'extracted_text': mock_screenshot_text,
        'analysis': """
        Critical streaming failure detected:
        1. RTMP connection lost after 5 retry attempts
        2. Stream ID indicates Monday Night Football broadcast
        3. Complete buffer drain suggests network or server issue
        
        Recommended actions:
        - Check ESPN edge server status
        - Verify firewall rules for idle connections
        - Restart RTMP encoder with increased buffer size
        - Contact ESPN NOC if issue persists
        """,
        'client': 'ESPN'
    }
    
    # Remember the issue and solution
    BroadcastMemoryPatterns.remember_customer_issue(
        memory,
        "ESPN",
        "RTMP Connection Lost during MNF",
        "Restart edge server, increase buffer to 10s"
    )
    
    print(f"Analysis complete!")
    print(f"Extracted: {result['extracted_text'][:50]}...")
    print(f"\nAnalysis: {result['analysis']}")
    
    # Check if we've seen similar before
    similar = BroadcastMemoryPatterns.search_similar_issues(
        memory, 
        "RTMP connection"
    )
    
    if similar:
        print(f"\nFound {len(similar)} similar past issues")


def example_document_compliance():
    """Example: Legal team checking contract compliance"""
    
    print("\n=== Document Compliance Example ===\n")
    
    # Create agent
    agent = MultiModalAgent()
    memory = AgentMemory()
    
    # Simulate PDF analysis
    # In real use, this would be an actual PDF file
    mock_contract_text = """
    BROADCAST SOFTWARE LICENSE AGREEMENT
    
    Section 5.2 - Data Protection
    Provider shall comply with all applicable data protection laws
    including GDPR and CCPA. Customer data shall be encrypted at
    rest and in transit using AES-256 encryption.
    
    Section 7.1 - Liability Limitation  
    Provider's total liability shall not exceed the fees paid in
    the preceding 12 months. Provider is not liable for indirect
    or consequential damages.
    
    Section 9.3 - Broadcast Compliance
    Software must comply with FCC regulations for closed captioning
    and emergency alert systems (EAS).
    """
    
    # Check compliance
    compliance_result = {
        'regulations_checked': ['FCC', 'GDPR', 'CCPA'],
        'analysis': """
        Compliance Status:
        
        ✅ GDPR - COMPLIANT
        - Section 5.2 explicitly mentions GDPR compliance
        - AES-256 encryption meets GDPR standards
        - Data protection measures specified
        
        ✅ CCPA - COMPLIANT  
        - Section 5.2 includes CCPA compliance
        - Data encryption requirements satisfied
        
        ⚠️ FCC - NEEDS REVIEW
        - Section 9.3 mentions FCC compliance
        - Closed captioning compliance stated
        - EAS compliance mentioned
        - Missing: Specific technical standards (CEA-608/708)
        
        Recommendations:
        1. Add specific FCC technical standards to Section 9.3
        2. Include audit rights for compliance verification
        3. Add breach notification timeline requirements
        """
    }
    
    print(f"Compliance check complete!")
    print(f"Regulations checked: {', '.join(compliance_result['regulations_checked'])}")
    print(f"\n{compliance_result['analysis']}")
    
    # Remember compliance check
    BroadcastMemoryPatterns.remember_compliance_check(
        memory,
        "ESPN_License_2024",
        "FCC",
        "Needs Review",
        "Missing CEA-608/708 technical standards"
    )


def example_whats_on_interface():
    """Example: Analyzing WHAT'S ON interface issues"""
    
    print("\n=== WHAT'S ON Interface Analysis ===\n")
    
    analyzer = ScreenshotAnalyzer()
    
    # Simulate WHAT'S ON screenshot
    mock_whats_on_text = """
    WHAT'S ON - NBC Schedule
    
    18:00 Local News [LIVE]
    18:30 <<<GAP ERROR>>>
    19:00 NBC Nightly News
    20:00 The Voice [SCHEDULED]
    21:00 [MISSING PROGRAM]
    22:00 Chicago Fire
    
    Signal: HD 1080i
    Affiliate: WNBC New York
    Status: WARNING - Schedule Gap Detected
    """
    
    # Analyze for NBC
    analysis = """
    WHAT'S ON Interface Issues Detected:
    
    1. SCHEDULE GAP at 18:30
       - 30-minute gap between Local News and NBC Nightly News
       - Likely missing syndicated content or local program
    
    2. MISSING PROGRAM at 21:00  
       - Prime time slot empty
       - Critical for NBC Monday night lineup
    
    3. Signal status shows WARNING
       - Schedule gaps triggering alert system
    
    NBC-Specific Considerations:
    - Local affiliates handle 18:30 slot differently
    - Check if WNBC has local programming scheduled
    - 21:00 is typically 'Ordinary Joe' or similar drama
    
    Recommended Actions:
    1. Contact NBC Network Operations
    2. Verify affiliate schedule sync
    3. Check for last-minute program changes
    4. Update schedule before 18:30 air time
    """
    
    print(f"WHAT'S ON Analysis:")
    print(f"Client: NBC")
    print(f"\nIssues found:")
    print(analysis)
    
    # Get NBC-specific patterns
    nbc_context = ClientPatterns.get_client_context("NBC")
    print(f"\nNBC common issues: {', '.join(nbc_context['common_issues'])}")


def example_quick_multimodal_pipeline():
    """Example: Combined screenshot and document analysis"""
    
    print("\n=== Quick MultiModal Pipeline ===\n")
    
    from toolkits.agent_hub import quick_pipeline
    
    # Create a pipeline for incident investigation
    incident_pipeline = quick_pipeline([
        {"do": "analyze error screenshot from CBS", "with": "analyze_screenshot"},
        {"do": "check our CBS contract for SLA terms", "with": "analyze_document"},
        {"do": "generate incident report", "with": "doc_generator"},
        {"do": "create customer communication", "with": "doc_generator"}
    ], name="CBS Incident Response")
    
    print(f"Created pipeline: {incident_pipeline['name']}")
    print(f"Steps: {len(incident_pipeline['pipeline'])}")
    
    for i, step in enumerate(incident_pipeline['pipeline'], 1):
        print(f"  {i}. {step['description']}")


if __name__ == "__main__":
    # Run all examples
    example_screenshot_debugging()
    example_document_compliance()
    example_whats_on_interface()
    example_quick_multimodal_pipeline()
    
    print("\n=== Examples Complete ===")
    print("\nKey Takeaways:")
    print("1. Screenshot analysis extracts text and provides context-aware insights")
    print("2. Document analysis checks compliance and extracts key terms")
    print("3. Client-specific patterns improve accuracy")
    print("4. Memory system learns from each analysis")
    print("5. Quick pipelines combine multiple analysis types")
