"""
Ruby learning tools - Katas, Best Practices, Code Review
"""

import streamlit as st
from typing import Dict, Any, List, Optional
import re
import random
from datetime import datetime

from utils.ollama import safe_ollama_generate
from .config import KATA_CATEGORIES, DIFFICULTY_LEVELS, RUBY_IDIOMS


class KataGenerator:
    """Ruby kata generator for practice and learning"""
    
    def generate(self, difficulty: str = "Intermediate", category: str = "Algorithms",
                time_limit: str = "30 minutes", include_hints: bool = True,
                include_solution: bool = False) -> Dict[str, Any]:
        """Generate a Ruby kata"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Get kata specifications
            kata_spec = self._get_kata_spec(difficulty, category)
            
            # Generate kata content
            kata_content = self._generate_kata_content(kata_spec, model)
            
            # Generate examples
            examples = self._generate_examples(kata_content['description'], model)
            
            # Generate starter code
            starter_code = self._generate_starter_code(kata_content, model)
            
            # Generate test cases
            test_cases = self._generate_test_cases(kata_content, model)
            
            # Generate hints if requested
            hints = []
            if include_hints:
                hints = self._generate_hints(kata_content, difficulty, model)
            
            # Generate solution if requested
            solution = ""
            explanation = ""
            if include_solution:
                solution, explanation = self._generate_solution(kata_content, model)
            
            return {
                'success': True,
                'title': kata_content.get('title', f'{category} Kata'),
                'description': kata_content.get('description', ''),
                'examples': examples,
                'starter_code': starter_code,
                'test_cases': test_cases,
                'hints': hints,
                'solution': solution,
                'explanation': explanation
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def run_tests(self, code: str, test_cases: str) -> Dict[str, Any]:
        """Run tests against provided code"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Validate code syntax
            syntax_check = self._check_syntax(code, model)
            if not syntax_check['valid']:
                return {
                    'passed': False,
                    'output': f"Syntax Error: {syntax_check['error']}"
                }
            
            # Simulate test execution
            test_result = self._simulate_test_execution(code, test_cases, model)
            
            return test_result
            
        except Exception as e:
            return {
                'passed': False,
                'output': f"Error running tests: {str(e)}"
            }
    
    def _get_kata_spec(self, difficulty: str, category: str) -> Dict[str, Any]:
        """Get kata specifications based on difficulty and category"""
        diff_info = DIFFICULTY_LEVELS.get(difficulty, DIFFICULTY_LEVELS["Intermediate"])
        cat_topics = KATA_CATEGORIES.get(category, ["General programming"])
        
        return {
            'difficulty': difficulty,
            'category': category,
            'time_estimate': diff_info['time_estimate'],
            'concepts': diff_info['concepts'],
            'topics': cat_topics
        }
    
    def _generate_kata_content(self, spec: Dict[str, Any], model: str) -> Dict[str, Any]:
        """Generate kata problem content"""
        prompt = f"""Generate a Ruby kata problem:

Difficulty: {spec['difficulty']}
Category: {spec['category']}
Topics: {', '.join(spec['topics'])}
Concepts: {', '.join(spec['concepts'])}

Create:
1. A clear, engaging title
2. A detailed problem description
3. Input/output specifications
4. Constraints and edge cases
5. Real-world context if applicable

Make it challenging but solvable within {spec['time_estimate']}."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Create engaging, educational Ruby katas."
        )
        
        response = result.get('response', '')
        
        # Extract title and description
        title_match = re.search(r'(?:Title|#)\s*:?\s*(.+)', response, re.IGNORECASE)
        title = title_match.group(1).strip() if title_match else f"{spec['category']} Challenge"
        
        return {
            'title': title,
            'description': response
        }
    
    def _generate_examples(self, description: str, model: str) -> str:
        """Generate examples for the kata"""
        prompt = f"""Based on this kata description:
{description[:500]}...

Generate 2-3 clear examples showing:
1. Input format
2. Expected output
3. Edge cases

Format as Ruby comments and code."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate clear, helpful examples."
        )
        
        return result.get('response', '# Examples not available')
    
    def _generate_starter_code(self, kata_content: Dict[str, Any], model: str) -> str:
        """Generate starter code template"""
        prompt = f"""Generate starter code for this kata:
{kata_content['description'][:500]}...

Include:
1. Method signature
2. Parameter names
3. Basic structure
4. TODO comments
5. Type hints if applicable"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate clean starter code template."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Fallback template
        return """def solution(input)
  # TODO: Implement your solution here
  
  # Your code goes here
  
end"""
    
    def _generate_test_cases(self, kata_content: Dict[str, Any], model: str) -> str:
        """Generate test cases for the kata"""
        prompt = f"""Generate RSpec test cases for this kata:
{kata_content['description'][:500]}...

Include:
1. Basic functionality tests
2. Edge case tests
3. Performance tests if relevant
4. Clear test descriptions"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate comprehensive RSpec tests."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Fallback template
        return """require 'rspec'

RSpec.describe 'Solution' do
  it 'handles basic case' do
    expect(solution(input)).to eq(expected_output)
  end
  
  it 'handles edge case' do
    expect(solution(edge_input)).to eq(edge_output)
  end
end"""
    
    def _generate_hints(self, kata_content: Dict[str, Any], difficulty: str, model: str) -> List[str]:
        """Generate progressive hints"""
        num_hints = {"Beginner": 3, "Intermediate": 2, "Advanced": 1, "Expert": 0}.get(difficulty, 2)
        
        if num_hints == 0:
            return []
        
        prompt = f"""Generate {num_hints} progressive hints for solving this kata:
{kata_content['description'][:500]}...

Make hints progressively more specific without giving away the complete solution."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate helpful, progressive hints."
        )
        
        response = result.get('response', '')
        
        # Extract hints
        hints = []
        for line in response.split('\n'):
            if line.strip() and (line[0].isdigit() or line.strip().startswith(('-', '•', '*'))):
                hint_text = re.sub(r'^[\d\-•\*\.\s]+', '', line).strip()
                if hint_text:
                    hints.append(hint_text)
        
        return hints[:num_hints]
    
    def _generate_solution(self, kata_content: Dict[str, Any], model: str) -> tuple:
        """Generate solution and explanation"""
        prompt = f"""Provide a complete solution for this kata:
{kata_content['description']}

Include:
1. Optimal solution
2. Clear comments
3. Time/space complexity
4. Alternative approaches

Also explain the approach and why it works."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide optimal Ruby solutions with explanations."
        )
        
        response = result.get('response', '')
        
        # Extract code and explanation
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        solution = code_match.group(1) if code_match else "# Solution not available"
        
        # Rest is explanation
        explanation = re.sub(r'```ruby\n.*?\n```', '', response, flags=re.DOTALL).strip()
        
        return solution, explanation
    
    def _check_syntax(self, code: str, model: str) -> Dict[str, Any]:
        """Check Ruby syntax validity"""
        # Simple syntax validation
        try:
            compile(code, '<string>', 'exec')  # Basic check
            return {'valid': True}
        except SyntaxError as e:
            return {'valid': False, 'error': str(e)}
    
    def _simulate_test_execution(self, code: str, test_cases: str, model: str) -> Dict[str, Any]:
        """Simulate test execution"""
        prompt = f"""Analyze if this solution would pass these tests:

Solution:
```ruby
{code}
```

Tests:
```ruby
{test_cases}
```

Determine if tests would pass and provide output."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Analyze code correctness against tests."
        )
        
        response = result.get('response', '')
        
        # Simple pass/fail detection
        passed = any(word in response.lower() for word in ['pass', 'correct', 'success'])
        
        return {
            'passed': passed,
            'output': response
        }


class BestPracticesAnalyzer:
    """Ruby best practices analyzer and guide"""
    
    def show_interface(self):
        """Show best practices analyzer interface"""
        st.markdown("### 📚 Ruby Best Practices Analyzer")
        st.caption("Analyze code against Ruby community standards")
        
        code = st.text_area(
            "Ruby Code to Analyze",
            height=300,
            placeholder="""class User
  def initialize(fn, ln, e)
    @fn = fn
    @ln = ln
    @e = e
  end
  
  def getName
    @fn + " " + @ln
  end
  
  def check_email
    return @e.match(/@/) != nil
  end
end""",
            key="best_practices_code"
        )
        
        analysis_options = st.multiselect(
            "Analysis Categories",
            ["Naming Conventions", "Code Style", "Idioms", "Performance", 
             "Security", "Testing", "Documentation", "Design Patterns"],
            default=["Naming Conventions", "Code Style", "Idioms"]
        )
        
        if st.button("🔍 Analyze Best Practices", type="primary", use_container_width=True):
            if code:
                with st.spinner("Analyzing code against best practices..."):
                    result = self.analyze(code, analysis_options)
                    
                    if result.get('success'):
                        # Overall score
                        score = result.get('score', 0)
                        col1, col2, col3 = st.columns([1, 2, 1])
                        with col2:
                            st.metric("Best Practices Score", f"{score}/100")
                            st.progress(score / 100)
                        
                        # Issues by category
                        tabs = st.tabs(["Issues", "Suggestions", "Refactored Code", "Resources"])
                        
                        with tabs[0]:
                            st.markdown("### 🚨 Issues Found")
                            for issue in result.get('issues', []):
                                severity_icon = {"high": "🔴", "medium": "🟡", "low": "🟢"}.get(issue['severity'], "⚪")
                                st.write(f"{severity_icon} **{issue['category']}**: {issue['description']}")
                                if issue.get('line'):
                                    st.caption(f"Line {issue['line']}: `{issue['code']}`")
                        
                        with tabs[1]:
                            st.markdown("### 💡 Improvement Suggestions")
                            for suggestion in result.get('suggestions', []):
                                with st.expander(suggestion['title']):
                                    st.markdown(suggestion['description'])
                                    if suggestion.get('example'):
                                        col1, col2 = st.columns(2)
                                        with col1:
                                            st.markdown("**Bad:**")
                                            st.code(suggestion['example']['bad'], language='ruby')
                                        with col2:
                                            st.markdown("**Good:**")
                                            st.code(suggestion['example']['good'], language='ruby')
                        
                        with tabs[2]:
                            st.markdown("### ✨ Refactored Code")
                            st.code(result.get('refactored_code', ''), language='ruby')
                            
                            # Improvements made
                            if result.get('improvements_made'):
                                st.success("Improvements made:")
                                for imp in result['improvements_made']:
                                    st.write(f"✅ {imp}")
                        
                        with tabs[3]:
                            st.markdown("### 📖 Learning Resources")
                            for resource in result.get('resources', []):
                                st.markdown(f"- [{resource['title']}]({resource['url']})")
                    else:
                        st.error(f"Analysis failed: {result.get('error')}")
            else:
                st.warning("Please provide code to analyze")
    
    def analyze(self, code: str, categories: List[str]) -> Dict[str, Any]:
        """Analyze code for best practices"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Detect issues
            issues = self._detect_issues(code, categories)
            
            # Generate suggestions
            suggestions = self._generate_suggestions(code, issues, model)
            
            # Generate refactored code
            refactored_code = self._generate_refactored_code(code, issues, model)
            
            # Calculate score
            score = self._calculate_score(issues)
            
            # Identify improvements made
            improvements_made = self._identify_improvements(code, refactored_code, model)
            
            # Get learning resources
            resources = self._get_resources(categories)
            
            return {
                'success': True,
                'score': score,
                'issues': issues,
                'suggestions': suggestions,
                'refactored_code': refactored_code,
                'improvements_made': improvements_made,
                'resources': resources
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _detect_issues(self, code: str, categories: List[str]) -> List[Dict[str, Any]]:
        """Detect best practice violations"""
        issues = []
        
        # Naming conventions
        if "Naming Conventions" in categories:
            # Check for non-snake_case methods
            method_pattern = r'def\s+([a-zA-Z_]\w*)'
            for match in re.finditer(method_pattern, code):
                method_name = match.group(1)
                if not self._is_snake_case(method_name):
                    issues.append({
                        'category': 'Naming Conventions',
                        'severity': 'high',
                        'description': f"Method name '{method_name}' should use snake_case",
                        'line': code[:match.start()].count('\n') + 1,
                        'code': method_name
                    })
            
            # Check for non-descriptive variable names
            var_pattern = r'@([a-zA-Z_]\w*)'
            for match in re.finditer(var_pattern, code):
                var_name = match.group(1)
                if len(var_name) <= 2 and var_name not in ['id', 'to', 'at']:
                    issues.append({
                        'category': 'Naming Conventions',
                        'severity': 'medium',
                        'description': f"Variable name '@{var_name}' is not descriptive",
                        'line': code[:match.start()].count('\n') + 1,
                        'code': f'@{var_name}'
                    })
        
        # Code style
        if "Code Style" in categories:
            # Check for string concatenation with +
            if re.search(r'["\'].*["\']\\s*\\+\\s*["\']', code):
                issues.append({
                    'category': 'Code Style',
                    'severity': 'low',
                    'description': "Use string interpolation instead of concatenation",
                    'line': None,
                    'code': 'string + string'
                })
        
        # Idioms
        if "Idioms" in categories:
            for idiom_name, idiom_info in RUBY_IDIOMS.items():
                if idiom_info['bad'] in code:
                    issues.append({
                        'category': 'Idioms',
                        'severity': 'medium',
                        'description': f"{idiom_info['description']}: use '{idiom_info['good']}' instead of '{idiom_info['bad']}'",
                        'line': None,
                        'code': idiom_info['bad']
                    })
        
        return issues
    
    def _is_snake_case(self, name: str) -> bool:
        """Check if name follows snake_case convention"""
        return bool(re.match(r'^[a-z_][a-z0-9_]*$', name))
    
    def _generate_suggestions(self, code: str, issues: List[Dict], model: str) -> List[Dict[str, Any]]:
        """Generate improvement suggestions"""
        issue_summary = "\n".join([f"- {issue['description']}" for issue in issues[:5]])
        
        prompt = f"""Based on these Ruby best practice issues:
{issue_summary}

Generate specific improvement suggestions with examples.
Focus on practical, actionable advice."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide Ruby best practice suggestions."
        )
        
        # Parse suggestions from response
        suggestions = []
        response = result.get('response', '')
        
        # Simple parsing - in production would be more sophisticated
        sections = response.split('\n\n')
        for section in sections[:3]:  # Limit to 3 suggestions
            if section.strip():
                suggestions.append({
                    'title': section.split('\n')[0].strip(),
                    'description': '\n'.join(section.split('\n')[1:]).strip(),
                    'example': self._extract_example(section)
                })
        
        return suggestions
    
    def _extract_example(self, text: str) -> Dict[str, str]:
        """Extract bad/good examples from text"""
        bad_match = re.search(r'bad:?\s*```ruby\n(.*?)\n```', text, re.IGNORECASE | re.DOTALL)
        good_match = re.search(r'good:?\s*```ruby\n(.*?)\n```', text, re.IGNORECASE | re.DOTALL)
        
        return {
            'bad': bad_match.group(1) if bad_match else '',
            'good': good_match.group(1) if good_match else ''
        }
    
    def _generate_refactored_code(self, code: str, issues: List[Dict], model: str) -> str:
        """Generate refactored code following best practices"""
        issue_summary = "\n".join([f"- {issue['description']}" for issue in issues])
        
        prompt = f"""Refactor this Ruby code to follow best practices:

```ruby
{code}
```

Issues to fix:
{issue_summary}

Apply:
- Ruby naming conventions
- Idiomatic Ruby patterns
- Clean code principles
- Modern Ruby features

Preserve all functionality."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate clean, idiomatic Ruby code."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return code
    
    def _calculate_score(self, issues: List[Dict]) -> int:
        """Calculate best practices score"""
        if not issues:
            return 100
        
        # Deduct points based on severity
        deductions = {
            'high': 10,
            'medium': 5,
            'low': 2
        }
        
        total_deduction = sum(deductions.get(issue['severity'], 0) for issue in issues)
        score = max(0, 100 - total_deduction)
        
        return score
    
    def _identify_improvements(self, original: str, refactored: str, model: str) -> List[str]:
        """Identify specific improvements made"""
        prompt = f"""List the specific improvements made in the refactored code.
Focus on:
- Naming improvements
- Idiom usage
- Code clarity
- Performance enhancements"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Identify specific code improvements."
        )
        
        response = result.get('response', '')
        
        # Extract improvements
        improvements = []
        for line in response.split('\n'):
            if line.strip().startswith(('-', '•', '*')):
                improvements.append(line.strip()[1:].strip())
        
        return improvements[:5]  # Limit to 5
    
    def _get_resources(self, categories: List[str]) -> List[Dict[str, str]]:
        """Get learning resources for categories"""
        resources = []
        
        resource_map = {
            "Naming Conventions": {
                'title': "Ruby Style Guide - Naming",
                'url': "https://rubystyle.guide/#naming-conventions"
            },
            "Code Style": {
                'title': "RuboCop Style Guide",
                'url': "https://docs.rubocop.org/rubocop/cops_style.html"
            },
            "Idioms": {
                'title': "Ruby Idioms",
                'url': "https://www.rubyguides.com/ruby-idioms/"
            },
            "Performance": {
                'title': "Fast Ruby",
                'url': "https://github.com/JuanitoFatas/fast-ruby"
            }
        }
        
        for category in categories:
            if category in resource_map:
                resources.append(resource_map[category])
        
        return resources


class CodeReviewer:
    """Automated Ruby code review tool"""
    
    def review(self, code: str, review_type: str = "comprehensive") -> Dict[str, Any]:
        """Perform code review"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Perform different types of review
            if review_type == "comprehensive":
                return self._comprehensive_review(code, model)
            elif review_type == "security":
                return self._security_review(code, model)
            elif review_type == "performance":
                return self._performance_review(code, model)
            else:
                return {'success': False, 'error': 'Unknown review type'}
                
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _comprehensive_review(self, code: str, model: str) -> Dict[str, Any]:
        """Perform comprehensive code review"""
        prompt = f"""Perform a comprehensive code review for this Ruby code:

```ruby
{code}
```

Review:
1. Code quality and maintainability
2. Potential bugs or issues
3. Performance concerns
4. Security vulnerabilities
5. Testing recommendations
6. Documentation needs

Provide specific, actionable feedback."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are an experienced Ruby code reviewer. Provide constructive feedback."
        )
        
        return {
            'success': True,
            'review': result.get('response', 'Review not available'),
            'type': 'comprehensive'
        }
    
    def _security_review(self, code: str, model: str) -> Dict[str, Any]:
        """Perform security-focused review"""
        prompt = f"""Review this Ruby code for security vulnerabilities:

```ruby
{code}
```

Check for:
1. SQL injection risks
2. XSS vulnerabilities
3. Authentication/authorization issues
4. Input validation problems
5. Sensitive data exposure
6. Dependency vulnerabilities

Provide specific security recommendations."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a security expert. Identify vulnerabilities and provide fixes."
        )
        
        return {
            'success': True,
            'review': result.get('response', 'Security review not available'),
            'type': 'security'
        }
    
    def _performance_review(self, code: str, model: str) -> Dict[str, Any]:
        """Perform performance-focused review"""
        prompt = f"""Review this Ruby code for performance issues:

```ruby
{code}
```

Analyze:
1. Algorithm efficiency
2. Database query optimization
3. Memory usage patterns
4. Caching opportunities
5. Parallel processing potential

Provide specific optimization suggestions."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a performance expert. Identify bottlenecks and optimizations."
        )
        
        return {
            'success': True,
            'review': result.get('response', 'Performance review not available'),
            'type': 'performance'
        }