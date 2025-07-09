-- Sample Knowledge Data for TuoKit Demo
-- Run this to populate knowledge base with demo content

-- First, insert some sample queries
INSERT INTO queries (tool, model, user_prompt, ai_response, created_at) VALUES
('code_explainer', 'deepseek-coder:6.7b', 'def fibonacci(n): return n if n <= 1 else fibonacci(n-1) + fibonacci(n-2)', 
'This is a recursive implementation of the Fibonacci sequence. Key points: 1) Base case handles n <= 1, 2) Recursive calls calculate F(n) = F(n-1) + F(n-2), 3) Time complexity is O(2^n) due to repeated calculations.', 
NOW() - INTERVAL '7 days'),

('code_debugger', 'deepseek-coder:6.7b', 'TypeError: unsupported operand type(s) for /: ''str'' and ''int''', 
'The error occurs when trying to divide a string by an integer. Solution: Convert the string to a number first using int() or float(). Example fix: result = int(user_input) / 10', 
NOW() - INTERVAL '5 days'),

('code_generator', 'deepseek-coder:6.7b', 'Create a Python function to validate email addresses', 
'import re\n\ndef validate_email(email: str) -> bool:\n    """Validate email address format."""\n    pattern = r''^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$''\n    return bool(re.match(pattern, email))', 
NOW() - INTERVAL '3 days'),

('doc_summary', 'deepseek-r1:6.7b', 'Technical specification document', 
'Summary: The document outlines API endpoints for user management including authentication, profile updates, and role assignments. Key requirements include OAuth2 authentication and rate limiting.', 
NOW() - INTERVAL '2 days'),

('doc_qa', 'deepseek-r1:6.7b', 'What methodology was used in the research?', 
'Based on the document, the research employed a mixed-methods approach combining quantitative analysis of system logs with qualitative user interviews. Data was collected over a 3-month period from 500 participants.', 
NOW() - INTERVAL '1 day');

-- Insert corresponding knowledge units with varied categories
INSERT INTO knowledge_units (query_id, title, content, category, created_at)
SELECT 
    q.id,
    CASE row_number() OVER (ORDER BY q.created_at DESC)
        WHEN 1 THEN 'Fibonacci Recursive Implementation'
        WHEN 2 THEN 'Fix String Division Error'
        WHEN 3 THEN 'Email Validation Function'
        WHEN 4 THEN 'API Design Best Practices'
        WHEN 5 THEN 'Research Methodology Summary'
    END as title,
    CASE row_number() OVER (ORDER BY q.created_at DESC)
        WHEN 1 THEN 'def fibonacci(n):\n    """Calculate fibonacci number recursively."""\n    if n <= 1:\n        return n\n    return fibonacci(n-1) + fibonacci(n-2)\n\n# Note: For better performance, use memoization or iterative approach'
        WHEN 2 THEN '# Fix for TypeError: string division\ntry:\n    result = int(user_input) / 10\nexcept ValueError:\n    print("Please enter a valid number")\nexcept ZeroDivisionError:\n    print("Cannot divide by zero")'
        WHEN 3 THEN 'import re\n\ndef validate_email(email: str) -> bool:\n    """Validate email address format.\n    \n    Args:\n        email: Email address to validate\n        \n    Returns:\n        bool: True if valid, False otherwise\n    """\n    pattern = r''^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$''\n    return bool(re.match(pattern, email))\n\n# Usage example:\n# is_valid = validate_email("user@example.com")'
        WHEN 4 THEN '## API Design Principles\n\n1. **RESTful conventions**: Use proper HTTP methods\n2. **Authentication**: Implement OAuth2 with JWT tokens\n3. **Rate limiting**: 100 requests/minute per user\n4. **Versioning**: Use URL path versioning (v1, v2)\n5. **Error handling**: Consistent error response format\n\n### Example Error Response:\n```json\n{\n  "error": {\n    "code": "RATE_LIMIT_EXCEEDED",\n    "message": "Too many requests",\n    "details": {\n      "retry_after": 60\n    }\n  }\n}\n```'
        WHEN 5 THEN '# Research Methodology\n\n## Approach: Mixed Methods\n- **Quantitative**: System log analysis (n=500)\n- **Qualitative**: User interviews (n=50)\n\n## Timeline\n- Data collection: 3 months\n- Analysis period: 1 month\n\n## Key Findings\n1. 87% improvement in task completion\n2. User satisfaction increased by 42%\n3. Error rates decreased by 65%'
    END as content,
    CASE row_number() OVER (ORDER BY q.created_at DESC)
        WHEN 1 THEN 'Algorithm'
        WHEN 2 THEN 'Error Solution'
        WHEN 3 THEN 'Utility Function'
        WHEN 4 THEN 'Technical Documentation'
        WHEN 5 THEN 'Research Findings'
    END as category,
    q.created_at + INTERVAL '1 hour'
FROM queries q
WHERE q.created_at > NOW() - INTERVAL '30 days'
ORDER BY q.created_at DESC
LIMIT 5;

-- Add a high-value code snippet
INSERT INTO knowledge_units (query_id, title, content, category, created_at) VALUES
(1, 'Python API Client with Retry Logic', 
'import requests
from time import sleep
from typing import Dict, Any

class APIClient:
    def __init__(self, base_url: str, api_key: str):
        self.base_url = base_url
        self.headers = {"Authorization": f"Bearer {api_key}"}
        self.session = requests.Session()
        
    def _retry_request(self, method: str, endpoint: str, **kwargs) -> Dict[str, Any]:
        """Make request with exponential backoff retry."""
        max_retries = 3
        
        for attempt in range(max_retries):
            try:
                response = self.session.request(
                    method, 
                    f"{self.base_url}/{endpoint}",
                    headers=self.headers,
                    **kwargs
                )
                response.raise_for_status()
                return response.json()
            except requests.exceptions.RequestException as e:
                if attempt == max_retries - 1:
                    raise
                sleep(2 ** attempt)  # Exponential backoff
                
    def get(self, endpoint: str) -> Dict[str, Any]:
        return self._retry_request("GET", endpoint)
        
    def post(self, endpoint: str, data: Dict[str, Any]) -> Dict[str, Any]:
        return self._retry_request("POST", endpoint, json=data)',
'Code Snippet', 
NOW() - INTERVAL '4 days');

-- Show summary
SELECT 'Sample data loaded successfully!' as status,
       COUNT(DISTINCT q.id) as total_queries,
       COUNT(DISTINCT k.id) as total_knowledge_units
FROM queries q
LEFT JOIN knowledge_units k ON q.id = k.query_id;