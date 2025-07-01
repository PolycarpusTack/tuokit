-- Database migration for Knowledge Graph feature
-- This adds support for storing knowledge graph concepts

-- Create knowledge_graph collection if it doesn't exist
INSERT INTO knowledge_collections (name, description, created_at)
SELECT 
    'knowledge_graph',
    'SQL concepts and their relationships for educational features',
    CURRENT_TIMESTAMP
WHERE NOT EXISTS (
    SELECT 1 FROM knowledge_collections WHERE name = 'knowledge_graph'
);

-- Sample entry structure for knowledge_graph collection:
-- {
--   "id": "concept_id",
--   "name": "Concept Name",
--   "type": "concept",
--   "description": "Description of the concept",
--   "difficulty": "Beginner|Intermediate|Advanced",
--   "resources": [
--     {"title": "Resource Title", "url": "https://..."}
--   ],
--   "prerequisites": ["prereq_id1", "prereq_id2"],
--   "related": ["related_id1", "related_id2"]
-- }
