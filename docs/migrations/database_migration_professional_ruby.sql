-- Database migration for professional Ruby tools
-- Run this after all previous TuoKit migrations

-- Memory optimization patterns table
CREATE TABLE IF NOT EXISTS memory_patterns (
    pattern VARCHAR(50) PRIMARY KEY,
    solution TEXT NOT NULL,
    severity SMALLINT CHECK (severity BETWEEN 1 AND 5),
    example TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Insert default memory patterns
INSERT INTO memory_patterns (pattern, solution, severity, example) VALUES
('String Duplication', 'Use << instead of += for string concatenation', 3, 'str += ''text'' → str << ''text'''),
('Unbounded Growth', 'Implement pagination or lazy loading', 4, 'Limit array size or use lazy enumerators'),
('N+1 Caching', 'Cache entire collections instead of individual elements', 2, 'Cache the full result set'),
('Leaky Constants', 'Use class methods instead of top-level constants', 3, 'CACHE = [] → def self.cache; @cache ||= []; end'),
('Frozen String Missing', 'Add # frozen_string_literal: true', 1, 'Reduces string allocation overhead')
ON CONFLICT (pattern) DO NOTHING;

-- Katas library table
CREATE TABLE IF NOT EXISTS katas (
    id SERIAL PRIMARY KEY,
    level VARCHAR(20) NOT NULL CHECK (level IN ('Beginner', 'Intermediate', 'Advanced')),
    topic VARCHAR(50) NOT NULL,
    focus_area VARCHAR(50),
    problem TEXT NOT NULL,
    solution TEXT,
    hints TEXT[],
    estimated_minutes INTEGER,
    difficulty_score SMALLINT CHECK (difficulty_score BETWEEN 1 AND 10),
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- View components library
CREATE TABLE IF NOT EXISTS view_components (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    template_language VARCHAR(20) CHECK (template_language IN ('ERB', 'HAML', 'SLIM')),
    javascript_framework VARCHAR(50),
    features VARCHAR(50)[],
    component_code TEXT,
    tests TEXT,
    stimulus_controller TEXT,
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Rails upgrade plans
CREATE TABLE IF NOT EXISTS rails_upgrades (
    id SERIAL PRIMARY KEY,
    from_version VARCHAR(10) NOT NULL,
    to_version VARCHAR(10) NOT NULL,
    project_size VARCHAR(50),
    upgrade_plan TEXT,
    estimated_days_min INTEGER,
    estimated_days_max INTEGER,
    risk_level VARCHAR(20) CHECK (risk_level IN ('Low', 'Medium', 'High')),
    critical_gems TEXT[],
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- C extensions registry
CREATE TABLE IF NOT EXISTS c_extensions (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    memory_model VARCHAR(50),
    thread_safe BOOLEAN DEFAULT FALSE,
    source_code TEXT,
    extconf_rb TEXT,
    benchmarks TEXT,
    query_id INTEGER REFERENCES queries(id) ON DELETE SET NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Kata completion tracking
CREATE TABLE IF NOT EXISTS kata_completions (
    id SERIAL PRIMARY KEY,
    kata_id INTEGER REFERENCES katas(id) ON DELETE CASCADE,
    user_solution TEXT,
    completion_time_seconds INTEGER,
    passed BOOLEAN DEFAULT FALSE,
    score INTEGER CHECK (score BETWEEN 0 AND 100),
    completed_at TIMESTAMPTZ DEFAULT NOW()
);

-- Component usage analytics
CREATE TABLE IF NOT EXISTS component_usage (
    id SERIAL PRIMARY KEY,
    component_id INTEGER REFERENCES view_components(id) ON DELETE CASCADE,
    usage_count INTEGER DEFAULT 1,
    last_used TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_katas_level_topic ON katas(level, topic);
CREATE INDEX IF NOT EXISTS idx_katas_focus_area ON katas(focus_area);
CREATE INDEX IF NOT EXISTS idx_view_components_name ON view_components(name);
CREATE INDEX IF NOT EXISTS idx_view_components_template ON view_components(template_language);
CREATE INDEX IF NOT EXISTS idx_rails_upgrades_versions ON rails_upgrades(from_version, to_version);
CREATE INDEX IF NOT EXISTS idx_c_extensions_name ON c_extensions(name);
CREATE INDEX IF NOT EXISTS idx_kata_completions_kata_id ON kata_completions(kata_id);
CREATE INDEX IF NOT EXISTS idx_memory_patterns_severity ON memory_patterns(severity);

-- Comments for documentation
COMMENT ON TABLE memory_patterns IS 'Ruby memory optimization patterns and antipatterns';
COMMENT ON TABLE katas IS 'Ruby programming challenges for skill development';
COMMENT ON TABLE view_components IS 'Rails ViewComponent library with tests and examples';
COMMENT ON TABLE rails_upgrades IS 'Rails version upgrade plans and documentation';
COMMENT ON TABLE c_extensions IS 'Ruby C extension implementations for performance';
COMMENT ON TABLE kata_completions IS 'Track user progress on kata challenges';
COMMENT ON TABLE component_usage IS 'Analytics for ViewComponent reuse';

COMMENT ON COLUMN katas.difficulty_score IS 'Score from 1-10 indicating challenge difficulty';
COMMENT ON COLUMN rails_upgrades.risk_level IS 'Risk assessment for the upgrade path';
COMMENT ON COLUMN c_extensions.thread_safe IS 'Whether the extension is thread-safe';
COMMENT ON COLUMN view_components.features IS 'Array of features like Slots, Variants, I18n';
