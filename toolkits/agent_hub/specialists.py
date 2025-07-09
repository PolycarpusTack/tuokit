"""
Specialist Agents for specific domains
Each agent has expertise in a particular area
"""

from typing import Dict, Any
import json
import re

from .core import BaseAgent, AgentType
from utils import safe_ollama_generate, capture_knowledge


class CodeAgent(BaseAgent):
    """Specialized agent for code-related tasks"""
    
    def __init__(self):
        super().__init__(
            name="Code Specialist",
            description="Expert in code generation, analysis, and optimization",
            tools=["code_explainer", "code_generator", "code_reviewer", "test_generator"],
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self) -> Dict[str, Any]:
        """Initialize code-specific tools"""
        return {
            "code_explainer": self._execute_code_explainer,
            "code_generator": self._execute_code_generator,
            "code_reviewer": self._execute_code_reviewer,
            "test_generator": self._execute_test_generator
        }
    
    def _execute_code_explainer(self, params: Dict) -> str:
        """Explain code in detail"""
        code = params.get('code', '')
        language = params.get('language', 'auto-detect')
        
        prompt = f"""
Explain this {language} code in detail:

```{language}
{code}
```

Provide:
1. Overall purpose
2. Step-by-step breakdown
3. Key concepts used
4. Potential improvements
"""
        response = safe_ollama_generate(
            params.get('model', 'deepseek-coder:6.7b'), 
            prompt
        )
        
        # Capture to knowledge base
        if self.db:
            capture_knowledge(
                tool="code_explainer",
                prompt=prompt,
                response=response['response'],
                metadata={'language': language}
            )
        
        return response['response']
    
    def _execute_code_generator(self, params: Dict) -> str:
        """Generate code based on requirements"""
        task = params.get('task', '')
        language = params.get('language', 'Python')
        style = params.get('style', 'clean and well-commented')
        
        prompt = f"""
Generate {language} code for: {task}

Requirements:
- Language: {language}
- Style: {style}
- Include proper error handling
- Add comprehensive comments
- Follow best practices

Provide complete, working code.
"""
        response = safe_ollama_generate(
            params.get('model', 'deepseek-coder:6.7b'),
            prompt
        )
        
        return response['response']
    
    def _execute_code_reviewer(self, params: Dict) -> str:
        """Review code for issues and improvements"""
        code = params.get('code', '')
        focus = params.get('focus', 'general')  # general, security, performance
        
        prompt = f"""
Review this code with focus on {focus}:

```
{code}
```

Analyze:
1. Potential bugs or issues
2. {focus.capitalize()} concerns
3. Code quality and style
4. Suggested improvements
5. Best practices compliance
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_test_generator(self, params: Dict) -> str:
        """Generate tests for code"""
        code = params.get('code', '')
        framework = params.get('framework', 'pytest')
        coverage = params.get('coverage', 'comprehensive')
        
        prompt = f"""
Generate {framework} tests for this code:

```
{code}
```

Requirements:
- Test framework: {framework}
- Coverage level: {coverage}
- Include edge cases
- Test both success and failure scenarios
- Add clear test descriptions
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']


class SQLAgent(BaseAgent):
    """Specialized agent for SQL and database tasks"""
    
    def __init__(self):
        super().__init__(
            name="SQL Specialist",
            description="Expert in SQL query generation and optimization",
            tools=["sql_generator", "sql_optimizer", "sql_explainer", "schema_designer"],
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self) -> Dict[str, Any]:
        """Initialize SQL-specific tools"""
        return {
            "sql_generator": self._execute_sql_generator,
            "sql_optimizer": self._execute_sql_optimizer,
            "sql_explainer": self._execute_sql_explainer,
            "schema_designer": self._execute_schema_designer
        }
    
    def _execute_sql_generator(self, params: Dict) -> str:
        """Generate SQL queries"""
        requirement = params.get('requirement', '')
        dialect = params.get('dialect', 'postgresql')
        schema = params.get('schema', '')
        
        prompt = f"""
Generate {dialect} SQL query for: {requirement}

{f'Schema: {schema}' if schema else ''}

Requirements:
- Use {dialect} syntax
- Optimize for performance
- Include appropriate indexes
- Add comments for clarity
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_sql_optimizer(self, params: Dict) -> str:
        """Optimize SQL queries"""
        query = params.get('query', '')
        dialect = params.get('dialect', 'postgresql')
        
        prompt = f"""
Optimize this {dialect} query:

```sql
{query}
```

Provide:
1. Optimized version
2. Explanation of changes
3. Performance improvements
4. Index recommendations
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_sql_explainer(self, params: Dict) -> str:
        """Explain SQL query execution"""
        query = params.get('query', '')
        dialect = params.get('dialect', 'postgresql')
        
        prompt = f"""
Explain this {dialect} query:

```sql
{query}
```

Provide:
1. What the query does
2. Step-by-step execution
3. Performance characteristics
4. Potential issues
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_schema_designer(self, params: Dict) -> str:
        """Design database schema"""
        requirements = params.get('requirements', '')
        
        prompt = f"""
Design a database schema for: {requirements}

Include:
1. Table definitions
2. Relationships
3. Indexes
4. Constraints
5. Sample queries
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']


class DocAgent(BaseAgent):
    """Specialized agent for documentation tasks"""
    
    def __init__(self):
        super().__init__(
            name="Documentation Specialist",
            description="Expert in documentation analysis and generation",
            tools=["doc_qa", "doc_summarizer", "doc_generator", "doc_translator"],
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self) -> Dict[str, Any]:
        """Initialize documentation tools"""
        return {
            "doc_qa": self._execute_doc_qa,
            "doc_summarizer": self._execute_doc_summarizer,
            "doc_generator": self._execute_doc_generator,
            "doc_translator": self._execute_doc_translator
        }
    
    def _execute_doc_qa(self, params: Dict) -> str:
        """Answer questions based on documentation"""
        document = params.get('document', '')
        question = params.get('question', '')
        
        prompt = f"""
Based on this document:
{document[:2000]}...

Answer: {question}

Provide a clear, accurate answer based only on the document content.
"""
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']
    
    def _execute_doc_summarizer(self, params: Dict) -> str:
        """Summarize documentation"""
        text = params.get('text', '')
        length = params.get('length', 'medium')
        style = params.get('style', 'technical')
        
        word_limits = {
            'brief': 50,
            'medium': 150,
            'detailed': 300
        }
        
        prompt = f"""
Summarize this {style} document in {word_limits.get(length, 150)} words:

{text[:3000]}...

Focus on key points and maintain technical accuracy.
"""
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']
    
    def _execute_doc_generator(self, params: Dict) -> str:
        """Generate documentation"""
        content = params.get('content', '')
        doc_type = params.get('doc_type', 'technical')
        format = params.get('format', 'markdown')
        
        prompt = f"""
Generate {doc_type} documentation in {format} format for:

{content}

Include appropriate sections, examples, and formatting.
"""
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']
    
    def _execute_doc_translator(self, params: Dict) -> str:
        """Translate technical documentation"""
        text = params.get('text', '')
        target_audience = params.get('audience', 'technical')
        
        prompt = f"""
Translate this technical content for {target_audience} audience:

{text}

Maintain accuracy while adjusting complexity for the target audience.
"""
        response = safe_ollama_generate('deepseek-r1:1.5b', prompt)
        return response['response']


class AnalysisAgent(BaseAgent):
    """Specialized agent for analysis tasks"""
    
    def __init__(self):
        super().__init__(
            name="Analysis Specialist", 
            description="Expert in error analysis and performance optimization",
            tools=["error_decoder", "performance_analyzer", "security_scanner", "complexity_analyzer"],
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self) -> Dict[str, Any]:
        """Initialize analysis tools"""
        return {
            "error_decoder": self._execute_error_decoder,
            "performance_analyzer": self._execute_performance_analyzer,
            "security_scanner": self._execute_security_scanner,
            "complexity_analyzer": self._execute_complexity_analyzer
        }
    
    def _execute_error_decoder(self, params: Dict) -> str:
        """Decode and analyze errors"""
        error = params.get('error', '')
        context = params.get('context', '')
        stack_trace = params.get('stack_trace', '')
        
        prompt = f"""
Analyze this error:

Error: {error}
Context: {context}
{f'Stack trace: {stack_trace}' if stack_trace else ''}

Provide:
1. Root cause analysis
2. Possible solutions
3. Prevention strategies
4. Related issues
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_performance_analyzer(self, params: Dict) -> str:
        """Analyze performance issues"""
        code = params.get('code', '')
        metrics = params.get('metrics', {})
        
        prompt = f"""
Analyze performance of this code:

```
{code}
```

{f'Metrics: {json.dumps(metrics, indent=2)}' if metrics else ''}

Identify:
1. Performance bottlenecks
2. Optimization opportunities  
3. Resource usage
4. Scalability concerns
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_security_scanner(self, params: Dict) -> str:
        """Scan for security vulnerabilities"""
        code = params.get('code', '')
        language = params.get('language', 'auto-detect')
        
        prompt = f"""
Security scan for {language} code:

```
{code}
```

Check for:
1. Common vulnerabilities (OWASP Top 10)
2. Input validation issues
3. Authentication/authorization flaws
4. Data exposure risks
5. Dependency vulnerabilities

Provide severity ratings and remediation steps.
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
    
    def _execute_complexity_analyzer(self, params: Dict) -> str:
        """Analyze code complexity"""
        code = params.get('code', '')
        
        prompt = f"""
Analyze complexity of this code:

```
{code}
```

Calculate and explain:
1. Cyclomatic complexity
2. Cognitive complexity
3. Maintainability index
4. Refactoring suggestions
"""
        response = safe_ollama_generate('deepseek-coder:6.7b', prompt)
        return response['response']
