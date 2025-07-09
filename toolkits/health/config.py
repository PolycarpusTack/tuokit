"""
Configuration for Health Toolkit
"""

# Database configuration
REQUIRED_TABLES = [
    'queries',
    'knowledge_units', 
    'knowledge_links',
    'knowledge_maintenance_log',
    'pipeline_templates',
    'pipelines',
    'code_snippets'
]

EXPECTED_COLUMNS = {
    'queries': ['id', 'tool', 'model', 'user_prompt', 'ai_response', 'metadata', 'created_at'],
    'knowledge_units': ['id', 'query_id', 'title', 'content', 'category', 'tags', 'quality_score', 'usage_count', 'created_at'],
    'knowledge_links': ['id', 'source_id', 'target_id', 'relationship_type', 'strength', 'created_at'],
    'knowledge_maintenance_log': ['id', 'action', 'details', 'units_affected', 'execution_time_ms', 'created_at']
}

# Ollama configuration
OLLAMA_DEFAULT_HOST = "http://localhost:11434"
REFRESH_INTERVAL = 5  # seconds
CONNECTION_TIMEOUT = 3  # seconds
BENCHMARK_CACHE_HOURS = 24  # How long to cache benchmark results

# System resource thresholds
RESOURCE_THRESHOLDS = {
    'cpu_warning': 80,  # percent
    'cpu_critical': 90,
    'memory_warning': 80,
    'memory_critical': 90,
    'disk_warning': 85,
    'disk_critical': 95
}