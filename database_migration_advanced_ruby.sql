-- Database migration for advanced Ruby tools (Pattern Matching, Ractors, GraphQL)
-- Run this after the existing TuoKit schema and ruby_tools migration

-- GraphQL specific table
CREATE TABLE IF NOT EXISTS graphql_apis (
    id SERIAL PRIMARY KEY,
    resource VARCHAR(100) NOT NULL,
    operations VARCHAR(100)[],
    types VARCHAR(50)[],
    authentication_method VARCHAR(50),
    pagination_type VARCHAR(50),
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Concurrency patterns table
CREATE TABLE IF NOT EXISTS concurrency_patterns (
    id SERIAL PRIMARY KEY,
    pattern VARCHAR(50) NOT NULL,
    use_case TEXT,
    example TEXT,
    performance_characteristics JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Pattern matching examples table
CREATE TABLE IF NOT EXISTS pattern_matching_examples (
    id SERIAL PRIMARY KEY,
    description TEXT NOT NULL,
    pattern_type VARCHAR(50),
    complexity VARCHAR(20),
    code TEXT,
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Ractor implementations table
CREATE TABLE IF NOT EXISTS ractor_implementations (
    id SERIAL PRIMARY KEY,
    task_description TEXT,
    worker_count INTEGER,
    communication_model VARCHAR(50),
    estimated_speedup FLOAT,
    code TEXT,
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Insert default concurrency patterns
INSERT INTO concurrency_patterns (pattern, use_case, performance_characteristics) VALUES
('Ractor', 'CPU-bound parallel processing without shared state', '{"gvl_free": true, "memory": "isolated", "overhead": "high"}'::jsonb),
('Thread Pool', 'I/O-bound concurrent operations', '{"gvl_free": false, "memory": "shared", "overhead": "medium"}'::jsonb),
('Fiber', 'Lightweight cooperative multitasking', '{"gvl_free": false, "memory": "shared", "overhead": "low"}'::jsonb),
('Async', 'Non-blocking I/O operations', '{"gvl_free": false, "memory": "shared", "overhead": "low"}'::jsonb),
('Process', 'Complete isolation with high overhead', '{"gvl_free": true, "memory": "isolated", "overhead": "highest"}'::jsonb)
ON CONFLICT (pattern) DO NOTHING;

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_graphql_apis_resource ON graphql_apis(resource);
CREATE INDEX IF NOT EXISTS idx_pattern_matching_complexity ON pattern_matching_examples(complexity);
CREATE INDEX IF NOT EXISTS idx_pattern_matching_type ON pattern_matching_examples(pattern_type);
CREATE INDEX IF NOT EXISTS idx_ractor_worker_count ON ractor_implementations(worker_count);
CREATE INDEX IF NOT EXISTS idx_concurrency_pattern ON concurrency_patterns(pattern);

-- Comments for documentation
COMMENT ON TABLE graphql_apis IS 'Generated GraphQL API schemas and configurations';
COMMENT ON TABLE concurrency_patterns IS 'Ruby concurrency model reference and best practices';
COMMENT ON TABLE pattern_matching_examples IS 'Ruby 3+ pattern matching examples and use cases';
COMMENT ON TABLE ractor_implementations IS 'Generated Ractor-based parallel processing solutions';

COMMENT ON COLUMN graphql_apis.operations IS 'Array of supported operations: Query, Mutation, Subscription';
COMMENT ON COLUMN ractor_implementations.estimated_speedup IS 'Estimated performance improvement vs sequential execution';
COMMENT ON COLUMN pattern_matching_examples.pattern_type IS 'Type: array, hash, guard, alternative, etc.';
