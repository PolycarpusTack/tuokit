"""
Agent Templates for Broadcast Software Teams
Practical, ready-to-use agent configurations
"""

from typing import Dict, List, Any
from ..orchestrator import AgentOrchestrator
from ..pipelines import PipelineExecutor


class AgentTemplates:
    """Pre-configured agent workflows for common tasks"""
    
    @staticmethod
    def broadcast_api_debugger() -> Dict[str, Any]:
        """Debug broadcast streaming APIs and protocols"""
        return {
            "name": "Broadcast API Debugger",
            "description": "Analyzes streaming protocols, codecs, and API issues",
            "pipeline": [
                {
                    "agent": "analysis",
                    "tool": "error_decoder",
                    "description": "Decode streaming errors (RTMP, HLS, WebRTC)"
                },
                {
                    "agent": "code", 
                    "tool": "code_reviewer",
                    "description": "Review codec implementations"
                },
                {
                    "agent": "docs",
                    "tool": "doc_generator",
                    "description": "Generate fix documentation"
                }
            ],
            "context": {
                "protocols": ["RTMP", "HLS", "DASH", "WebRTC"],
                "focus": "broadcast streaming"
            }
        }
    
    @staticmethod
    def compliance_checker() -> Dict[str, Any]:
        """For legal team - check broadcast compliance"""
        return {
            "name": "Broadcast Compliance Checker",
            "description": "Analyzes code and docs for FCC/broadcast compliance",
            "pipeline": [
                {
                    "agent": "code",
                    "tool": "code_reviewer",
                    "description": "Scan for compliance violations"
                },
                {
                    "agent": "docs",
                    "tool": "doc_qa",
                    "description": "Check against compliance docs"
                },
                {
                    "agent": "analysis",
                    "tool": "security_scanner",
                    "description": "Verify data handling compliance"
                }
            ],
            "context": {
                "regulations": ["FCC", "GDPR", "CCPA"],
                "industry": "broadcast software"
            }
        }
    
    @staticmethod
    def support_ticket_resolver() -> Dict[str, Any]:
        """For support team - analyze and resolve tickets"""
        return {
            "name": "Support Ticket Resolver",
            "description": "Analyzes tickets and suggests solutions from knowledge base",
            "pipeline": [
                {
                    "agent": "analysis",
                    "tool": "error_decoder",
                    "description": "Understand the reported issue"
                },
                {
                    "agent": "docs",
                    "tool": "doc_qa",
                    "description": "Search knowledge base for solutions"
                },
                {
                    "agent": "docs",
                    "tool": "doc_generator",
                    "description": "Create customer-friendly response"
                }
            ],
            "context": {
                "tone": "professional and helpful",
                "audience": "broadcast engineers"
            }
        }
    
    @staticmethod
    def sales_demo_builder() -> Dict[str, Any]:
        """For sales team - create technical demos"""
        return {
            "name": "Sales Demo Builder",
            "description": "Generates demo scripts and technical talking points",
            "pipeline": [
                {
                    "agent": "code",
                    "tool": "code_generator",
                    "description": "Create demo code snippets"
                },
                {
                    "agent": "docs",
                    "tool": "doc_generator", 
                    "description": "Generate talking points"
                },
                {
                    "agent": "sql",
                    "tool": "sql_generator",
                    "description": "Create sample queries for analytics demos"
                }
            ],
            "context": {
                "audience": "broadcast industry buyers",
                "focus": "ROI and efficiency"
            }
        }
    
    @staticmethod
    def broadcast_analytics_pipeline() -> Dict[str, Any]:
        """For analysts - analyze broadcast metrics"""
        return {
            "name": "Broadcast Analytics Pipeline",
            "description": "Analyzes streaming quality, viewer metrics, and performance",
            "pipeline": [
                {
                    "agent": "sql",
                    "tool": "sql_generator",
                    "description": "Query streaming metrics database"
                },
                {
                    "agent": "analysis",
                    "tool": "performance_analyzer",
                    "description": "Analyze streaming performance"
                },
                {
                    "agent": "docs",
                    "tool": "doc_generator",
                    "description": "Generate analytics report"
                }
            ],
            "context": {
                "metrics": ["bitrate", "latency", "buffer_ratio", "viewer_retention"],
                "output": "executive summary with charts"
            }
        }
    
    @staticmethod
    def license_analyzer() -> Dict[str, Any]:
        """For legal team - analyze software licenses"""
        return {
            "name": "License Analyzer",
            "description": "Reviews code dependencies for license compliance",
            "pipeline": [
                {
                    "agent": "code",
                    "tool": "code_reviewer",
                    "description": "Scan for third-party dependencies"
                },
                {
                    "agent": "docs",
                    "tool": "doc_qa",
                    "description": "Check license compatibility"
                },
                {
                    "agent": "docs",
                    "tool": "doc_generator",
                    "description": "Generate compliance report"
                }
            ],
            "context": {
                "acceptable_licenses": ["MIT", "Apache 2.0", "BSD"],
                "warn_on": ["GPL", "AGPL"],
                "industry": "commercial broadcast software"
            }
        }
    
    @staticmethod
    def list_templates() -> List[str]:
        """List all available templates"""
        return [
            "broadcast_api_debugger",
            "compliance_checker",
            "support_ticket_resolver",
            "sales_demo_builder",
            "broadcast_analytics_pipeline",
            "license_analyzer"
        ]
    
    @staticmethod
    def get_template(name: str) -> Dict[str, Any]:
        """Get a template by name"""
        templates = {
            "broadcast_api_debugger": AgentTemplates.broadcast_api_debugger,
            "compliance_checker": AgentTemplates.compliance_checker,
            "support_ticket_resolver": AgentTemplates.support_ticket_resolver,
            "sales_demo_builder": AgentTemplates.sales_demo_builder,
            "broadcast_analytics_pipeline": AgentTemplates.broadcast_analytics_pipeline,
            "license_analyzer": AgentTemplates.license_analyzer
        }
        
        if name in templates:
            return templates[name]()
        else:
            raise ValueError(f"Template '{name}' not found. Available: {list(templates.keys())}")
