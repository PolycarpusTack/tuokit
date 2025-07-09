-- TuoKit Lite Agent System Database Migration
-- Adds pipeline storage for the simplified agent system

-- Pipeline storage table
CREATE TABLE IF NOT EXISTS pipelines (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    steps JSONB NOT NULL,
    results JSONB NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    created_by VARCHAR(100),
    execution_time_ms INTEGER,
    success BOOLEAN DEFAULT TRUE
);

-- Index for faster lookups
CREATE INDEX idx_pipelines_created_at ON pipelines(created_at DESC);
CREATE INDEX idx_pipelines_name ON pipelines(name);

-- Educational guidance history (optional - for future enhancement)
CREATE TABLE IF NOT EXISTS educational_guidance (
    id SERIAL PRIMARY KEY,
    context TEXT NOT NULL,
    user_action VARCHAR(100) NOT NULL,
    guidance JSONB NOT NULL,
    helpful BOOLEAN,  -- User feedback on guidance quality
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Pipeline templates for common workflows
CREATE TABLE IF NOT EXISTS pipeline_templates (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL UNIQUE,
    description TEXT,
    category VARCHAR(50),
    steps JSONB NOT NULL,
    usage_count INTEGER DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Insert some starter templates
INSERT INTO pipeline_templates (name, description, category, steps) VALUES
(
    'Data Analysis Workflow',
    'Extract data from database, clean it, and generate summary',
    'analytics',
    '[
        {
            "name": "Extract Customer Data",
            "tool": "sql_generator",
            "params": {
                "query": "Get all customer orders from the last 30 days with product details",
                "dialect": "PostgreSQL"
            }
        },
        {
            "name": "Clean Email Addresses",
            "tool": "regex_generator",
            "params": {
                "description": "Extract and validate email addresses from customer data"
            }
        },
        {
            "name": "Generate Report",
            "tool": "doc_summarizer",
            "params": {
                "text": "{{previous_results}}",
                "length": 200
            }
        }
    ]'::jsonb
),
(
    'Code Migration Helper',
    'Analyze legacy code and generate modern equivalent',
    'development',
    '[
        {
            "name": "Analyze Legacy Code",
            "tool": "code_explainer",
            "params": {
                "code": "# Paste your legacy code here"
            }
        },
        {
            "name": "Generate Modern Version",
            "tool": "code_generator",
            "params": {
                "task": "Convert the analyzed code to use modern Python patterns and type hints"
            }
        }
    ]'::jsonb
),
(
    'Error Investigation Pipeline',
    'Decode error, analyze code context, and suggest fixes',
    'debugging',
    '[
        {
            "name": "Decode Error Message",
            "tool": "error_decoder",
            "params": {
                "error": "# Paste error message here"
            }
        },
        {
            "name": "Generate Fix",
            "tool": "code_generator",
            "params": {
                "task": "Generate corrected code based on the error analysis"
            }
        }
    ]'::jsonb
);

-- Views for analytics
CREATE OR REPLACE VIEW pipeline_analytics AS
SELECT 
    DATE_TRUNC('day', created_at) as day,
    COUNT(*) as pipeline_runs,
    AVG(execution_time_ms) as avg_execution_time_ms,
    SUM(CASE WHEN success THEN 1 ELSE 0 END) as successful_runs,
    ROUND(AVG(CASE WHEN success THEN 1 ELSE 0 END) * 100, 2) as success_rate
FROM pipelines
GROUP BY DATE_TRUNC('day', created_at)
ORDER BY day DESC;

CREATE OR REPLACE VIEW popular_tools AS
SELECT 
    tool,
    COUNT(*) as usage_count,
    ROUND(AVG(CASE WHEN success THEN 1 ELSE 0 END) * 100, 2) as success_rate
FROM (
    SELECT 
        jsonb_array_elements(steps)->>'tool' as tool,
        success
    FROM pipelines
) tool_usage
WHERE tool IS NOT NULL
GROUP BY tool
ORDER BY usage_count DESC;

-- Grant permissions
GRANT ALL ON pipelines TO ollama_user;
GRANT ALL ON educational_guidance TO ollama_user;
GRANT ALL ON pipeline_templates TO ollama_user;
GRANT SELECT ON pipeline_analytics TO ollama_user;
GRANT SELECT ON popular_tools TO ollama_user;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO ollama_user;
