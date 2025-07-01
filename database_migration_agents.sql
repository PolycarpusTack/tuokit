-- TuoKit Agent System Database Migration
-- Adds agent-specific tracking tables

-- Agent execution history
CREATE TABLE IF NOT EXISTS agent_executions (
    id BIGSERIAL PRIMARY KEY,
    goal TEXT NOT NULL,
    agent_name VARCHAR(64) NOT NULL,
    agent_type VARCHAR(32) NOT NULL, -- specialist, team, meta
    state_json JSONB NOT NULL, -- Complete AgentState serialized
    start_time TIMESTAMPTZ DEFAULT NOW(),
    end_time TIMESTAMPTZ,
    phase VARCHAR(32) NOT NULL, -- planning, execution, validation
    success BOOLEAN DEFAULT FALSE,
    error_message TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Agent performance metrics
CREATE TABLE IF NOT EXISTS agent_metrics (
    id BIGSERIAL PRIMARY KEY,
    agent_name VARCHAR(64) NOT NULL,
    execution_id BIGINT REFERENCES agent_executions(id),
    metric_name VARCHAR(64) NOT NULL,
    metric_value DECIMAL,
    measured_at TIMESTAMPTZ DEFAULT NOW()
);

-- Agent collaboration records
CREATE TABLE IF NOT EXISTS agent_collaborations (
    id BIGSERIAL PRIMARY KEY,
    team_name VARCHAR(64) NOT NULL,
    execution_id BIGINT REFERENCES agent_executions(id),
    member_agent VARCHAR(64) NOT NULL,
    subtask TEXT NOT NULL,
    dependency_agents TEXT[], -- Array of agent names this task depends on
    result_summary TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
-- Indexes for performance
CREATE INDEX idx_agent_executions_agent_name ON agent_executions(agent_name);
CREATE INDEX idx_agent_executions_created_at ON agent_executions(created_at DESC);
CREATE INDEX idx_agent_metrics_agent_name ON agent_metrics(agent_name);
CREATE INDEX idx_agent_collaborations_team ON agent_collaborations(team_name);

-- Useful views
CREATE OR REPLACE VIEW agent_success_rates AS
SELECT 
    agent_name,
    agent_type,
    COUNT(*) as total_executions,
    SUM(CASE WHEN success THEN 1 ELSE 0 END) as successful_executions,
    ROUND(AVG(CASE WHEN success THEN 1 ELSE 0 END) * 100, 2) as success_rate,
    AVG(EXTRACT(EPOCH FROM (end_time - start_time))) as avg_duration_seconds
FROM agent_executions
WHERE end_time IS NOT NULL
GROUP BY agent_name, agent_type;

CREATE OR REPLACE VIEW recent_agent_activity AS
SELECT 
    ae.id,
    ae.goal,
    ae.agent_name,
    ae.phase,
    ae.success,
    ae.created_at,
    COUNT(ac.id) as collaboration_count
FROM agent_executions ae
LEFT JOIN agent_collaborations ac ON ae.id = ac.execution_id
GROUP BY ae.id, ae.goal, ae.agent_name, ae.phase, ae.success, ae.created_at
ORDER BY ae.created_at DESC
LIMIT 20;

-- Grant permissions
GRANT ALL ON agent_executions TO ollama_user;
GRANT ALL ON agent_metrics TO ollama_user;
GRANT ALL ON agent_collaborations TO ollama_user;
GRANT SELECT ON agent_success_rates TO ollama_user;
GRANT SELECT ON recent_agent_activity TO ollama_user;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO ollama_user;