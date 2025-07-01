-- TuoKit Database Setup Script
-- Run this in PostgreSQL to create the required database structure

-- Create database
CREATE DATABASE ollama_knowledge;

-- Connect to the database
\c ollama_knowledge;

-- Create user (adjust password as needed)
CREATE USER ollama_user WITH PASSWORD 'your_secure_password';

-- Grant privileges
GRANT ALL PRIVILEGES ON DATABASE ollama_knowledge TO ollama_user;

-- Create queries table
CREATE TABLE queries (
    id SERIAL PRIMARY KEY,
    tool VARCHAR(100) NOT NULL,
    model VARCHAR(100) NOT NULL,
    user_prompt TEXT NOT NULL,
    ai_response TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create knowledge units table
CREATE TABLE knowledge_units (
    id SERIAL PRIMARY KEY,
    query_id INTEGER REFERENCES queries(id) ON DELETE CASCADE,
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    category VARCHAR(100) NOT NULL,
    verified BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for better performance
CREATE INDEX idx_queries_tool ON queries(tool);
CREATE INDEX idx_queries_created_at ON queries(created_at);
CREATE INDEX idx_knowledge_category ON knowledge_units(category);
CREATE INDEX idx_knowledge_title ON knowledge_units(title);

-- Grant table permissions to user
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO ollama_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO ollama_user;

-- Create a view for recent activity
CREATE VIEW recent_activity AS
SELECT 
    q.id,
    q.tool,
    q.model,
    q.user_prompt,
    q.created_at,
    COUNT(k.id) as knowledge_count
FROM queries q
LEFT JOIN knowledge_units k ON q.id = k.query_id
GROUP BY q.id, q.tool, q.model, q.user_prompt, q.created_at
ORDER BY q.created_at DESC
LIMIT 50;

-- Grant view permission
GRANT SELECT ON recent_activity TO ollama_user;

-- Success message
SELECT 'Database setup complete!' as message;