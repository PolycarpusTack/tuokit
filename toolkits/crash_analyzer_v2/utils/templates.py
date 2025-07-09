"""
Prompt templates for Crash Analyzer V2
Centralized, reusable templates for AI interactions
"""

# Template for business impact analysis
BUSINESS_IMPACT_TEMPLATE = """Analyze the business impact of this crash. Focus on user impact and revenue risk.

Error Type: {error_type}
Error Message: {error_message}
Environment: {environment}
Affected Components: {components}

Key sections from crash dump:
{context}

Provide analysis in this exact JSON format:
{{
    "business_impact": "Brief description of business impact",
    "affected_users": "Estimated number or percentage of affected users",
    "revenue_risk": "Estimated revenue impact (per hour if applicable)",
    "urgency": "IMMEDIATE/HIGH/MEDIUM/LOW",
    "key_stakeholders": ["List of teams/roles to notify"]
}}

Be specific and quantitative where possible."""

# Template for technical root cause analysis
TECHNICAL_ANALYSIS_TEMPLATE = """Perform deep technical analysis of this crash. Think step by step.

Primary Error: {error_type}: {error_message}
Error Chain: {error_chain}
Environment: {environment}

Crash dump sections:
{context}

Provide technical analysis in this exact JSON format:
{{
    "root_cause": "The actual root cause (not just the symptom)",
    "error_chain": ["Step 1 what happened", "Step 2 what happened", "Step 3 final error"],
    "contributing_factors": ["Factor 1", "Factor 2"],
    "technical_details": "Detailed technical explanation",
    "confidence_level": 0.0 to 1.0
}}

Focus on identifying the true root cause, not just the final error."""

# Template for recommendations
RECOMMENDATIONS_TEMPLATE = """Based on this analysis, provide actionable recommendations.

Root Cause: {root_cause}
Business Impact: {business_impact}
Urgency: {urgency}

Provide recommendations in this exact JSON format:
{{
    "immediate_actions": ["Action 1 to do right now", "Action 2"],
    "short_term_fixes": ["Fix to implement within 48h", "Another fix"],
    "long_term_improvements": ["Architectural change", "Process improvement"],
    "monitoring_additions": ["What to monitor", "Alerts to add"]
}}

Make recommendations specific and actionable."""

# Template for section analysis (used in sampling)
SECTION_ANALYSIS_TEMPLATE = """Analyze this section from a crash dump (section: {section_name}).
Focus on identifying errors, patterns, and potential issues.

Section content:
{section_content}

Provide analysis in JSON format:
{{
    "primary_issues": ["issue 1", "issue 2"],
    "error_types": ["type 1", "type 2"],
    "severity": "CRITICAL/HIGH/MEDIUM/LOW",
    "patterns": ["pattern 1", "pattern 2"],
    "recommendations": ["recommendation 1"]
}}"""

# Template for chunk insights (used in deep forensic)
CHUNK_INSIGHTS_TEMPLATE = """Analyze this code/log chunk for issues beyond obvious errors.
Look for:
1. Anti-patterns and code smells
2. Performance bottlenecks
3. Security vulnerabilities
4. Architectural issues

Chunk:
{chunk_content}

Provide insights in JSON format:
{{
    "anti_patterns": ["pattern1", "pattern2"],
    "performance_risks": ["risk1"],
    "security_concerns": ["concern1"],
    "improvement_suggestions": ["suggestion1"]
}}"""

def format_template(template: str, **kwargs) -> str:
    """
    Format a template with provided values
    
    Args:
        template: Template string
        **kwargs: Values to substitute
        
    Returns:
        Formatted template
    """
    # Safely format, replacing missing keys with "Unknown"
    for key in kwargs:
        if kwargs[key] is None:
            kwargs[key] = "Unknown"
        elif isinstance(kwargs[key], list):
            kwargs[key] = ", ".join(str(item) for item in kwargs[key][:5])  # Limit lists
    
    try:
        return template.format(**kwargs)
    except KeyError as e:
        # Log missing key but continue
        from .logger import get_logger
        logger = get_logger()
        logger.warning(f"Template missing key: {e}")
        # Return template with available substitutions
        return template.format_map({k: v for k, v in kwargs.items() if k in template})

# Quick access functions
def get_business_impact_prompt(**kwargs) -> str:
    """Get formatted business impact analysis prompt"""
    return format_template(BUSINESS_IMPACT_TEMPLATE, **kwargs)

def get_technical_analysis_prompt(**kwargs) -> str:
    """Get formatted technical analysis prompt"""
    return format_template(TECHNICAL_ANALYSIS_TEMPLATE, **kwargs)

def get_recommendations_prompt(**kwargs) -> str:
    """Get formatted recommendations prompt"""
    return format_template(RECOMMENDATIONS_TEMPLATE, **kwargs)

def get_section_analysis_prompt(**kwargs) -> str:
    """Get formatted section analysis prompt"""
    return format_template(SECTION_ANALYSIS_TEMPLATE, **kwargs)

def get_chunk_insights_prompt(**kwargs) -> str:
    """Get formatted chunk insights prompt"""
    return format_template(CHUNK_INSIGHTS_TEMPLATE, **kwargs)