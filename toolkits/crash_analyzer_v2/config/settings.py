"""
Crash Analyzer V2 Configuration
Enhanced settings for the redesigned crash analysis system
"""
from typing import Dict, Any, List
from datetime import timedelta

# Analysis Method Configurations
ANALYSIS_METHODS = {
    "quick_triage": {
        "name": "Quick Triage",
        "description": "Immediate crash assessment in under 10 seconds",
        "max_processing_time": timedelta(seconds=10),
        "max_content_size": 5 * 1024,  # 5KB
        "features": [
            "Instant severity assessment",
            "Pattern matching",
            "No AI processing",
            "Immediate actionable insights"
        ],
        "recommended_for": "Files < 100KB, Emergency response"
    },
    "root_cause": {
        "name": "Root Cause Analysis", 
        "description": "Comprehensive single-pass analysis with AI insights",
        "max_processing_time": timedelta(minutes=1),
        "max_content_size": 100 * 1024,  # 100KB smart extraction
        "features": [
            "Multi-model AI analysis",
            "Causal chain reconstruction",
            "Business impact assessment",
            "Visual diagrams"
        ],
        "recommended_for": "Files < 1MB, Standard investigation"
    },
    "strategic_sampling": {
        "name": "Strategic Sampling",
        "description": "Intelligent analysis of large files without full processing",
        "max_processing_time": timedelta(minutes=2),
        "sampling_ratio": 0.1,  # 10% of file
        "features": [
            "Adaptive sampling",
            "Error neighborhood analysis",
            "Performance detection",
            "Statistical confidence scoring"
        ],
        "recommended_for": "Files 1-5MB, Quick large file insights"
    },
    "deep_forensic": {
        "name": "Deep Forensic Analysis",
        "description": "Comprehensive analysis including non-crash improvements",
        "max_processing_time": timedelta(hours=2),  # 2 hours for local testing
        "chunk_size": 16384,  # 16KB chunks - larger chunks = fewer API calls
        "features": [
            "100% file coverage",
            "Multi-pass analysis",
            "Performance profiling",
            "Security scanning",
            "Predictive insights"
        ],
        "recommended_for": "Critical investigations, smaller files preferred",
        "skip_ai_on_timeout": True,  # Fallback to algorithmic analysis
        "save_intermediate": True  # Save results as we go
    },
    "enterprise_report": {
        "name": "Enterprise Report",
        "description": "Structured enterprise-grade analysis with 7 comprehensive sections",
        "max_processing_time": timedelta(minutes=5),
        "features": [
            "Executive summary with actionable items",
            "Context & environment analysis",
            "Stack trace deep dive",
            "Pre-crash sequence reconstruction",
            "Solutions & workarounds",
            "Proactive improvements",
            "Memory & performance analysis"
        ],
        "recommended_for": "Enterprise support teams, incident reports",
        "requires_ai": True
    }
}

# Severity Scoring Configuration
SEVERITY_SCORING = {
    "keywords": {
        "FATAL": 10,
        "CRITICAL": 9,
        "SEVERE": 8,
        "ERROR": 7,
        "EXCEPTION": 7,
        "FAILURE": 6,
        "WARNING": 4,
        "WARN": 4,
        "INFO": 2,
        "DEBUG": 1
    },
    "patterns": {
        "NullPointerException": 8,
        "OutOfMemoryError": 9,
        "StackOverflowError": 9,
        "SecurityException": 9,
        "DatabaseException": 7,
        "NetworkException": 6,
        "TimeoutException": 6,
        "ValidationException": 4
    },
    "context_multipliers": {
        "production": 1.5,
        "authentication": 1.4,
        "payment": 1.6,
        "data_loss": 2.0,
        "security": 1.8
    }
}

# Sampling Strategy Configuration
SAMPLING_STRATEGY = {
    "priority_sections": [
        {
            "name": "file_header",
            "description": "First 10KB for metadata",
            "size": 10 * 1024,
            "position": "start"
        },
        {
            "name": "error_neighborhoods", 
            "description": "±5KB around error keywords",
            "size": 5 * 1024,
            "position": "around_errors"
        },
        {
            "name": "stack_traces",
            "description": "Complete stack trace extraction",
            "size": "dynamic",
            "position": "pattern_based"
        },
        {
            "name": "timestamps",
            "description": "Temporal correlation points",
            "size": 2 * 1024,
            "position": "timestamp_clusters"
        },
        {
            "name": "final_state",
            "description": "Last 10KB for final state",
            "size": 10 * 1024,
            "position": "end"
        }
    ],
    "anomaly_detection": {
        "memory_spike_threshold": 1.5,  # 150% of average
        "time_gap_threshold": 60,  # seconds
        "error_cluster_threshold": 5  # errors within 1KB
    }
}

# Deep Analysis Dimensions
DEEP_ANALYSIS_DIMENSIONS = {
    "crash_analysis": {
        "enabled": True,
        "passes": ["error_detection", "severity_scoring", "pattern_matching"],
        "output_sections": ["critical_errors", "warnings", "correlations"]
    },
    "performance_analysis": {
        "enabled": True,
        "thresholds": {
            "slow_operation": 1000,  # ms
            "memory_allocation": 100 * 1024 * 1024,  # 100MB
            "thread_count": 100,
            "gc_pause": 500  # ms
        },
        "output_sections": ["bottlenecks", "resource_usage", "optimization_opportunities"]
    },
    "security_analysis": {
        "enabled": True,
        "patterns": [
            r"password\s*=\s*['\"].*['\"]",
            r"api[_-]?key\s*=\s*['\"].*['\"]",
            r"token\s*=\s*['\"].*['\"]",
            r"secret\s*=\s*['\"].*['\"]"
        ],
        "output_sections": ["vulnerabilities", "exposed_secrets", "unsafe_operations"]
    },
    "code_quality": {
        "enabled": True,
        "checks": ["deprecated_apis", "anti_patterns", "error_handling"],
        "output_sections": ["quality_issues", "best_practice_violations", "improvement_suggestions"]
    },
    "predictive_analysis": {
        "enabled": True,
        "models": ["trend_analysis", "capacity_planning", "failure_prediction"],
        "output_sections": ["future_risks", "trending_issues", "capacity_warnings"]
    }
}

# Output Format Templates
OUTPUT_TEMPLATES = {
    "quick_triage": {
        "format": "yaml",
        "sections": ["severity", "error_type", "immediate_cause", "quick_fix", "confidence"]
    },
    "root_cause": {
        "format": "markdown",
        "sections": ["executive_summary", "technical_analysis", "recommendations", "prevention"]
    },
    "strategic_sampling": {
        "format": "structured",
        "sections": ["coverage", "findings", "patterns", "confidence", "next_steps"]
    },
    "deep_forensic": {
        "format": "comprehensive_markdown",
        "sections": ["overview", "critical_findings", "all_dimensions", "knowledge_integration"]
    }
}

# Visual Elements Configuration
VISUAL_ELEMENTS = {
    "severity_indicators": {
        "CRITICAL": {"emoji": "🔴", "color": "#FF0000", "priority": 1},
        "HIGH": {"emoji": "🟠", "color": "#FF8C00", "priority": 2},
        "MEDIUM": {"emoji": "🟡", "color": "#FFD700", "priority": 3},
        "LOW": {"emoji": "🟢", "color": "#32CD32", "priority": 4},
        "INFO": {"emoji": "🔵", "color": "#4169E1", "priority": 5}
    },
    "progress_indicators": {
        "quick_triage": "spinner",
        "root_cause": "progress_bar",
        "strategic_sampling": "sampling_map",
        "deep_forensic": "multi_progress"
    }
}

# Model Selection Preferences
MODEL_PREFERENCES = {
    "quick_triage": None,  # No AI model needed
    "root_cause": {
        "primary": ["deepseek-r1:latest", "mistral:latest"],
        "fallback": ["deepseek-r1:1.5b", "codellama:7b-instruct"],
        "temperature": 0.3
    },
    "strategic_sampling": {
        "primary": ["deepseek-r1:1.5b"],  # Faster model for sampling
        "temperature": 0.2
    },
    "deep_forensic": {
        "primary": ["deepseek-r1:latest", "deepseek-r1:8b"],
        "validation": ["mistral:latest"],  # For cross-validation
        "temperature": 0.4
    }
}

# Performance Configuration
PERFORMANCE_CONFIG = {
    "parallel_chunks": 1,  # Reduced from 4 to avoid overwhelming Ollama
    "chunk_overlap": 512,  # Bytes to overlap between chunks
    "chunk_timeout": 300,  # 5 minutes per chunk for local testing
    "max_analysis_time": 7200,  # 2 hours max for deep analysis (local testing)
    "progress_update_interval": 1,  # seconds
    "partial_results_interval": 10,  # seconds
    "memory_limit_mb": 512,
    "abort_on_timeout": False,  # Don't abort on timeout for deep analysis
    "save_partial_results": True  # Save results even if analysis is incomplete
}

# Knowledge Integration
KNOWLEDGE_CONFIG = {
    "auto_save_patterns": True,
    "min_confidence_to_save": 0.8,
    "pattern_learning": True,
    "collaborative_filtering": True
}