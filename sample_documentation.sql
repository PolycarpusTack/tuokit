-- Sample Documentation for Knowledge Base
-- Run this after database setup to populate help documentation

-- Insert documentation entries
INSERT INTO queries (tool, model, user_prompt, ai_response) VALUES
('help_system', 'system', 'Documentation: Code Explainer', 
'The Code Explainer analyzes your code and provides insights about functionality, algorithms, and potential issues.'),
('help_system', 'system', 'Documentation: Document Tools', 
'Document Tools help you extract insights from PDFs and text files through summarization and Q&A.'),
('help_system', 'system', 'Documentation: Knowledge Library', 
'The Knowledge Library stores and organizes all your AI-generated insights for easy retrieval and reuse.');

-- Insert corresponding knowledge units
INSERT INTO knowledge_units (query_id, title, content, category) 
SELECT 
    q.id,
    q.user_prompt,
    CASE 
        WHEN q.user_prompt LIKE '%Code Explainer%' THEN 
            E'### Code Explainer Guide\n\n**Purpose**: Understand code functionality and structure\n\n**How to use**:\n1. Paste your code\n2. Add comments for focus areas (e.g., # Focus on security)\n3. Click "Analyze Code"\n\n**Best practices**:\n- Include imports for context\n- Add error messages if debugging\n- Specify language if not Python\n\n**Pro tips**:\n- Use # TODO: for specific questions\n- Add # PERFORMANCE: for optimization focus\n- Include # SECURITY: for vulnerability checks'
        WHEN q.user_prompt LIKE '%Document Tools%' THEN 
            E'### Document Tools Guide\n\n**Capabilities**:\n- Smart summarization\n- Context-aware Q&A\n- Data extraction\n\n**Supported formats**:\n- PDF (text-based)\n- TXT files\n- More coming soon\n\n**Workflow**:\n1. Upload document\n2. Choose action (Summarize/Q&A/Extract)\n3. Review results\n4. Save to knowledge base'
        WHEN q.user_prompt LIKE '%Knowledge Library%' THEN 
            E'### Knowledge Library Guide\n\n**Features**:\n- Full-text search\n- Category filtering\n- In-place editing\n- Bulk export\n\n**Organization tips**:\n- Use descriptive titles\n- Choose specific categories\n- Verify important entries\n- Regular cleanup\n\n**Search operators**:\n- category:"Code Snippet"\n- verified:true\n- tool:code_explainer'
    END,
    'Documentation'
FROM queries q
WHERE q.tool = 'help_system'
ORDER BY q.id DESC
LIMIT 3;

-- Success message
SELECT 'Documentation loaded successfully!' as status,
       COUNT(*) as doc_count
FROM knowledge_units
WHERE category = 'Documentation';