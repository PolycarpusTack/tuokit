-- Database migration for Ruby Performance and Testing tools
-- Run this after the existing TuoKit schema

-- Add columns to existing queries table
ALTER TABLE queries ADD COLUMN IF NOT EXISTS complexity VARCHAR(50);
ALTER TABLE queries ADD COLUMN IF NOT EXISTS category VARCHAR(50);

-- Performance specific table
CREATE TABLE IF NOT EXISTS performance_findings (
    id SERIAL PRIMARY KEY,
    query_id INTEGER REFERENCES queries(id) ON DELETE CASCADE,
    issue_type VARCHAR(50) NOT NULL,
    severity SMALLINT CHECK (severity IN (1, 2, 3)), -- 1=low, 2=medium, 3=high
    solution TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Testing table
CREATE TABLE IF NOT EXISTS test_cases (
    id SERIAL PRIMARY KEY,
    feature TEXT NOT NULL,
    framework VARCHAR(20) NOT NULL,
    coverage FLOAT CHECK (coverage >= 0 AND coverage <= 100),
    test_code TEXT,
    generated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_performance_findings_query_id ON performance_findings(query_id);
CREATE INDEX IF NOT EXISTS idx_performance_findings_issue_type ON performance_findings(issue_type);
CREATE INDEX IF NOT EXISTS idx_test_cases_framework ON test_cases(framework);
CREATE INDEX IF NOT EXISTS idx_queries_category ON queries(category);
CREATE INDEX IF NOT EXISTS idx_queries_complexity ON queries(complexity);

-- Comments for documentation
COMMENT ON TABLE performance_findings IS 'Ruby performance issues detected during code analysis';
COMMENT ON TABLE test_cases IS 'Generated Rails system tests with metadata';
COMMENT ON COLUMN performance_findings.severity IS '1=low, 2=medium, 3=high severity';
COMMENT ON COLUMN test_cases.coverage IS 'Estimated test coverage percentage';
