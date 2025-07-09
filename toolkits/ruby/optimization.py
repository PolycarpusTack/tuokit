"""
Ruby optimization tools - Memory, Performance, and Code optimization
"""

import streamlit as st
from typing import Dict, Any, List, Optional
import re
from datetime import datetime

from utils.ollama import safe_ollama_generate
from .config import MEMORY_ANTIPATTERNS, PERFORMANCE_PATTERNS, GC_CONFIGS


class MemoryOptimizer:
    """Ruby memory optimization and analysis tool"""
    
    def analyze(self, code: str, depth: str = "Standard", 
                gc_tuning: bool = True, benchmarks: bool = True) -> Dict[str, Any]:
        """Analyze Ruby code for memory usage patterns"""
        try:
            # Quick antipattern detection
            antipatterns = self._detect_antipatterns(code)
            
            # Get selected model
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Comprehensive memory analysis
            analysis = self._analyze_memory_usage(code, model, depth)
            
            # Generate optimized code
            optimized_code = self._optimize_code(code, model, antipatterns)
            
            # Calculate metrics and savings
            metrics = self._calculate_metrics(code, optimized_code, model)
            
            # Generate benchmark code if requested
            benchmark_code = ""
            if benchmarks:
                benchmark_code = self._generate_benchmark(code, optimized_code)
            
            # Generate GC configuration if requested
            gc_config = ""
            if gc_tuning:
                gc_config = self._generate_gc_config(code, model)
            
            return {
                'success': True,
                'antipatterns': antipatterns,
                'analysis': analysis,
                'optimized_code': optimized_code,
                'metrics': metrics,
                'savings_estimate': metrics.get('savings_estimate', 'N/A'),
                'benchmark_code': benchmark_code,
                'gc_config': gc_config
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _detect_antipatterns(self, code: str) -> List[Dict[str, str]]:
        """Detect memory antipatterns in code"""
        found = []
        for name, pattern_info in MEMORY_ANTIPATTERNS.items():
            if re.search(pattern_info['pattern'], code, re.IGNORECASE | re.MULTILINE):
                found.append({
                    'name': name,
                    'description': pattern_info['description'],
                    'fix': pattern_info['fix']
                })
        return found
    
    def _analyze_memory_usage(self, code: str, model: str, depth: str) -> str:
        """Perform comprehensive memory analysis"""
        depth_prompts = {
            "Quick": "Provide a brief memory usage overview",
            "Standard": "Analyze memory allocation patterns and potential issues",
            "Deep": "Perform deep memory analysis including object lifecycle and GC impact"
        }
        
        prompt = f"""Analyze memory usage in this Ruby code:

```ruby
{code}
```

{depth_prompts[depth]}. Focus on:
- Object allocation hotspots
- Memory retention issues
- GC pressure points
- Potential memory leaks
- String and collection usage
- Instance variable growth
"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a Ruby memory optimization expert. Provide specific, actionable insights."
        )
        
        return result.get('response', 'Analysis failed')
    
    def _optimize_code(self, code: str, model: str, antipatterns: List[Dict]) -> str:
        """Generate optimized version of the code"""
        antipattern_context = "\n".join([f"- {ap['name']}: {ap['fix']}" for ap in antipatterns])
        
        prompt = f"""Optimize this Ruby code for memory efficiency:

```ruby
{code}
```

{f"Known issues to fix:\\n{antipattern_context}" if antipatterns else ""}

Apply these optimizations:
- Use lazy loading where appropriate
- Implement object pooling for repeated allocations
- Use memory-efficient data structures
- Freeze strings that don't change
- Use symbols for hash keys
- Clear collections when done
- Avoid creating unnecessary intermediate objects

Preserve all functionality while reducing memory footprint."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a Ruby performance expert. Generate clean, optimized code."
        )
        
        # Extract code from response
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return response
    
    def _calculate_metrics(self, original: str, optimized: str, model: str) -> Dict[str, Any]:
        """Calculate memory metrics and savings estimate"""
        prompt = f"""Compare memory usage between original and optimized Ruby code:

Original:
```ruby
{original[:500]}...
```

Optimized:
```ruby
{optimized[:500]}...
```

Estimate:
1. Object allocations reduction (percentage)
2. Memory usage reduction (percentage)
3. GC pressure reduction
4. Overall savings estimate

Provide specific numbers."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide realistic memory savings estimates based on the optimizations."
        )
        
        response = result.get('response', '')
        
        # Try to extract metrics
        metrics = {
            'allocations': 'N/A',
            'memory_usage': 'N/A',
            'gc_runs': 'N/A',
            'savings_estimate': 'N/A'
        }
        
        # Look for percentages
        percentage_match = re.search(r'(\d+)\s*%', response)
        if percentage_match:
            metrics['savings_estimate'] = f"{percentage_match.group(1)}%"
        
        return metrics
    
    def _generate_benchmark(self, original: str, optimized: str) -> str:
        """Generate benchmark code to compare memory usage"""
        return f"""require 'benchmark/memory'

# Original implementation
def original_implementation
{self._indent_code(original, 2)}
end

# Optimized implementation
def optimized_implementation
{self._indent_code(optimized, 2)}
end

# Benchmark memory usage
Benchmark.memory do |x|
  x.report("Original") {{ original_implementation }}
  x.report("Optimized") {{ optimized_implementation }}
  x.compare!
end

# Profile memory allocations
require 'memory_profiler'

puts "\\n=== Original Implementation ==="
report_original = MemoryProfiler.report {{ original_implementation }}
report_original.pretty_print

puts "\\n=== Optimized Implementation ==="
report_optimized = MemoryProfiler.report {{ optimized_implementation }}
report_optimized.pretty_print
"""
    
    def _generate_gc_config(self, code: str, model: str) -> str:
        """Generate GC tuning configuration"""
        # Analyze code to determine app type
        app_type = "web_app"  # Default
        if "batch" in code.lower() or "process" in code.lower():
            app_type = "batch_processing"
        elif "memory" in code.lower() and "constraint" in code.lower():
            app_type = "low_memory"
        
        config = GC_CONFIGS.get(app_type, GC_CONFIGS["web_app"])
        
        gc_config = f"""# Ruby GC Configuration
# {config['description']}

# Add to your Ruby startup script or environment
"""
        
        for key, value in config.items():
            if key != "description":
                gc_config += f"ENV['{key}'] = '{value}'\n"
        
        gc_config += f"""
# Alternative: Set via command line
# {' '.join([f"{k}={v}" for k, v in config.items() if k != 'description'])} ruby your_app.rb

# Monitor GC stats
GC::Profiler.enable
at_exit {{ GC::Profiler.report }}
"""
        
        return gc_config
    
    def _indent_code(self, code: str, spaces: int) -> str:
        """Indent code by specified spaces"""
        indent = " " * spaces
        return "\n".join([indent + line for line in code.split("\n")])


class PerformanceProfiler:
    """Ruby performance profiling and optimization tool"""
    
    def show_interface(self):
        """Show performance profiler interface"""
        st.markdown("### ⚡ Performance Profiler")
        st.caption("Profile and optimize Ruby application performance")
        
        code = st.text_area(
            "Ruby Code to Profile",
            height=300,
            placeholder="""def calculate_primes(n)
  primes = []
  (2..n).each do |num|
    is_prime = true
    (2...num).each do |div|
      if num % div == 0
        is_prime = false
        break
      end
    end
    primes << num if is_prime
  end
  primes
end""",
            key="perf_code"
        )
        
        col1, col2 = st.columns(2)
        
        with col1:
            profile_type = st.selectbox(
                "Profile Type",
                ["CPU Time", "Memory", "Object Allocation", "Method Calls", "Full Profile"]
            )
        
        with col2:
            optimization_level = st.select_slider(
                "Optimization Level",
                ["Conservative", "Balanced", "Aggressive"],
                value="Balanced"
            )
        
        include_flame_graph = st.checkbox("Generate Flame Graph", value=True)
        include_benchmarks = st.checkbox("Include Benchmarks", value=True)
        
        if st.button("🔍 Profile Performance", type="primary", use_container_width=True):
            if code:
                with st.spinner("Profiling performance..."):
                    result = self.profile(
                        code,
                        profile_type=profile_type,
                        optimization_level=optimization_level,
                        flame_graph=include_flame_graph,
                        benchmarks=include_benchmarks
                    )
                    
                    if result.get('success'):
                        # Show results in tabs
                        tabs = st.tabs(["Profile Report", "Optimized Code", "Benchmarks", "Recommendations"])
                        
                        with tabs[0]:
                            st.markdown("**Performance Profile**")
                            st.markdown(result.get('profile_report', ''))
                            
                            # Show metrics
                            if result.get('metrics'):
                                col1, col2, col3 = st.columns(3)
                                with col1:
                                    st.metric("Execution Time", result['metrics'].get('execution_time', 'N/A'))
                                with col2:
                                    st.metric("Method Calls", result['metrics'].get('method_calls', 'N/A'))
                                with col3:
                                    st.metric("Memory Usage", result['metrics'].get('memory_usage', 'N/A'))
                        
                        with tabs[1]:
                            st.markdown("**Optimized Code**")
                            st.code(result.get('optimized_code', ''), language='ruby')
                            
                            if result.get('performance_gain'):
                                st.success(f"🚀 Expected Performance Gain: {result['performance_gain']}")
                        
                        with tabs[2]:
                            if include_benchmarks and result.get('benchmark_code'):
                                st.markdown("**Benchmark Code**")
                                st.code(result['benchmark_code'], language='ruby')
                        
                        with tabs[3]:
                            st.markdown("**Optimization Recommendations**")
                            st.markdown(result.get('recommendations', ''))
                    else:
                        st.error(f"Profiling failed: {result.get('error')}")
            else:
                st.warning("Please provide code to profile")
    
    def profile(self, code: str, profile_type: str = "Full Profile",
                optimization_level: str = "Balanced", 
                flame_graph: bool = True, benchmarks: bool = True) -> Dict[str, Any]:
        """Profile Ruby code performance"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Detect performance issues
            issues = self._detect_performance_issues(code)
            
            # Generate profile report
            profile_report = self._generate_profile_report(code, model, profile_type)
            
            # Generate optimized code
            optimized_code = self._optimize_for_performance(code, model, optimization_level, issues)
            
            # Calculate performance metrics
            metrics = self._calculate_performance_metrics(code, optimized_code, model)
            
            # Generate recommendations
            recommendations = self._generate_recommendations(code, issues, model)
            
            # Generate benchmark code if requested
            benchmark_code = ""
            if benchmarks:
                benchmark_code = self._generate_performance_benchmark(code, optimized_code)
            
            return {
                'success': True,
                'profile_report': profile_report,
                'optimized_code': optimized_code,
                'metrics': metrics,
                'performance_gain': metrics.get('performance_gain', 'N/A'),
                'recommendations': recommendations,
                'benchmark_code': benchmark_code,
                'issues': issues
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _detect_performance_issues(self, code: str) -> List[Dict[str, str]]:
        """Detect performance issues in code"""
        issues = []
        for name, pattern_info in PERFORMANCE_PATTERNS.items():
            if re.search(pattern_info['pattern'], code, re.IGNORECASE | re.MULTILINE):
                issues.append({
                    'type': name,
                    'description': pattern_info['description'],
                    'optimization': pattern_info['optimization']
                })
        return issues
    
    def _generate_profile_report(self, code: str, model: str, profile_type: str) -> str:
        """Generate performance profile report"""
        prompt = f"""Profile this Ruby code for {profile_type}:

```ruby
{code}
```

Analyze:
1. Performance bottlenecks
2. Time complexity
3. Resource usage
4. Method call frequency
5. Optimization opportunities

Provide specific measurements and insights."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a Ruby performance expert. Provide detailed profiling insights."
        )
        
        return result.get('response', 'Profile generation failed')
    
    def _optimize_for_performance(self, code: str, model: str, level: str, issues: List[Dict]) -> str:
        """Generate performance-optimized code"""
        optimization_strategies = {
            "Conservative": "Apply safe optimizations that maintain readability",
            "Balanced": "Apply standard optimizations balancing performance and maintainability",
            "Aggressive": "Apply all possible optimizations for maximum performance"
        }
        
        issues_context = "\n".join([f"- {issue['type']}: {issue['optimization']}" for issue in issues])
        
        prompt = f"""Optimize this Ruby code for performance:

```ruby
{code}
```

Optimization level: {level} - {optimization_strategies[level]}

{f"Detected issues to fix:\\n{issues_context}" if issues else ""}

Apply optimizations:
- Algorithm improvements
- Data structure optimization
- Caching and memoization
- Lazy evaluation
- Parallel processing where applicable
- Reduce object allocations
- Optimize loops and iterations"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate highly optimized Ruby code while preserving functionality."
        )
        
        # Extract code from response
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return response
    
    def _calculate_performance_metrics(self, original: str, optimized: str, model: str) -> Dict[str, Any]:
        """Calculate performance metrics"""
        prompt = f"""Compare performance between original and optimized code:

Estimate improvements in:
1. Execution time
2. CPU usage
3. Memory usage
4. Method calls
5. Overall performance gain

Provide percentage improvements."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide realistic performance estimates."
        )
        
        response = result.get('response', '')
        
        # Extract metrics
        metrics = {
            'execution_time': 'N/A',
            'method_calls': 'N/A',
            'memory_usage': 'N/A',
            'performance_gain': 'N/A'
        }
        
        # Look for percentage
        percentage_match = re.search(r'(\d+)\s*%', response)
        if percentage_match:
            metrics['performance_gain'] = f"{percentage_match.group(1)}%"
        
        return metrics
    
    def _generate_recommendations(self, code: str, issues: List[Dict], model: str) -> str:
        """Generate performance recommendations"""
        prompt = f"""Based on this Ruby code analysis, provide performance recommendations:

Code has these issues:
{chr(10).join([f"- {issue['description']}" for issue in issues]) if issues else "No specific issues detected"}

Provide:
1. Immediate optimizations
2. Architectural improvements
3. Best practices to follow
4. Tools for monitoring
5. Long-term optimization strategy"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide actionable performance recommendations."
        )
        
        return result.get('response', 'No recommendations generated')
    
    def _generate_performance_benchmark(self, original: str, optimized: str) -> str:
        """Generate performance benchmark code"""
        return f"""require 'benchmark'
require 'benchmark/ips'

# Original implementation
def original_method
{self._indent_code(original, 2)}
end

# Optimized implementation
def optimized_method
{self._indent_code(optimized, 2)}
end

# Time-based benchmark
puts "=== Time Benchmark ==="
n = 10000
Benchmark.bm(20) do |x|
  x.report("Original:") {{ n.times {{ original_method }} }}
  x.report("Optimized:") {{ n.times {{ optimized_method }} }}
end

# Iterations per second benchmark
puts "\\n=== IPS Benchmark ==="
Benchmark.ips do |x|
  x.report("Original") {{ original_method }}
  x.report("Optimized") {{ optimized_method }}
  x.compare!
end

# CPU profiling
require 'ruby-prof'

puts "\\n=== CPU Profile ==="
result = RubyProf.profile {{ 1000.times {{ optimized_method }} }}
printer = RubyProf::FlatPrinter.new(result)
printer.print(STDOUT, min_percent: 1)
"""
    
    def _indent_code(self, code: str, spaces: int) -> str:
        """Indent code by specified spaces"""
        indent = " " * spaces
        return "\n".join([indent + line for line in code.split("\n")])


class CodeOptimizer:
    """General Ruby code optimization tool"""
    
    def show_interface(self):
        """Show code optimizer interface"""
        st.markdown("### 🔧 Code Optimizer")
        st.caption("Optimize Ruby code for readability, performance, and best practices")
        
        code = st.text_area(
            "Ruby Code to Optimize",
            height=300,
            placeholder="""class UserService
  def initialize
    @users = []
  end
  
  def add_user(name, email)
    @users << {name: name, email: email}
  end
  
  def find_user(email)
    @users.each do |user|
      return user if user[:email] == email
    end
    nil
  end
end""",
            key="optimize_code"
        )
        
        # Optimization options
        st.markdown("**Optimization Goals**")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            opt_performance = st.checkbox("Performance", value=True)
            opt_memory = st.checkbox("Memory Usage", value=True)
        
        with col2:
            opt_readability = st.checkbox("Readability", value=True)
            opt_idioms = st.checkbox("Ruby Idioms", value=True)
        
        with col3:
            opt_solid = st.checkbox("SOLID Principles", value=True)
            opt_dry = st.checkbox("DRY Principle", value=True)
        
        ruby_version = st.session_state.get("ruby_version", "3.0")
        
        if st.button("🚀 Optimize Code", type="primary", use_container_width=True):
            if code:
                with st.spinner("Optimizing code..."):
                    goals = []
                    if opt_performance: goals.append("performance")
                    if opt_memory: goals.append("memory")
                    if opt_readability: goals.append("readability")
                    if opt_idioms: goals.append("idioms")
                    if opt_solid: goals.append("solid")
                    if opt_dry: goals.append("dry")
                    
                    result = self.optimize(code, goals=goals, ruby_version=ruby_version)
                    
                    if result.get('success'):
                        # Show results
                        tabs = st.tabs(["Optimized Code", "Analysis", "Refactoring Steps", "Before/After"])
                        
                        with tabs[0]:
                            st.markdown("**Optimized Code**")
                            st.code(result.get('optimized_code', ''), language='ruby')
                            
                            # Download button
                            st.download_button(
                                "Download Optimized Code",
                                result.get('optimized_code', ''),
                                "optimized_code.rb",
                                mime="text/plain"
                            )
                        
                        with tabs[1]:
                            st.markdown("**Optimization Analysis**")
                            st.markdown(result.get('analysis', ''))
                            
                            # Show improvements
                            if result.get('improvements'):
                                st.markdown("**Improvements Made:**")
                                for improvement in result['improvements']:
                                    st.write(f"✅ {improvement}")
                        
                        with tabs[2]:
                            st.markdown("**Refactoring Steps**")
                            st.markdown(result.get('refactoring_steps', ''))
                        
                        with tabs[3]:
                            col1, col2 = st.columns(2)
                            with col1:
                                st.markdown("**Before**")
                                st.code(code, language='ruby')
                            with col2:
                                st.markdown("**After**")
                                st.code(result.get('optimized_code', ''), language='ruby')
                    else:
                        st.error(f"Optimization failed: {result.get('error')}")
            else:
                st.warning("Please provide code to optimize")
    
    def optimize(self, code: str, goals: List[str] = None, 
                ruby_version: str = "3.0") -> Dict[str, Any]:
        """Optimize Ruby code based on specified goals"""
        try:
            if goals is None:
                goals = ["performance", "readability", "idioms"]
            
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Analyze current code
            analysis = self._analyze_code(code, model)
            
            # Generate optimized version
            optimized_code = self._generate_optimized_code(code, goals, ruby_version, model)
            
            # Identify improvements
            improvements = self._identify_improvements(code, optimized_code, goals, model)
            
            # Generate refactoring steps
            refactoring_steps = self._generate_refactoring_steps(code, optimized_code, model)
            
            return {
                'success': True,
                'optimized_code': optimized_code,
                'analysis': analysis,
                'improvements': improvements,
                'refactoring_steps': refactoring_steps
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _analyze_code(self, code: str, model: str) -> str:
        """Analyze current code quality"""
        prompt = f"""Analyze this Ruby code:

```ruby
{code}
```

Evaluate:
1. Code quality and structure
2. Performance characteristics
3. Adherence to Ruby conventions
4. Potential issues or code smells
5. Opportunities for improvement"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide a comprehensive code analysis."
        )
        
        return result.get('response', 'Analysis failed')
    
    def _generate_optimized_code(self, code: str, goals: List[str], 
                                ruby_version: str, model: str) -> str:
        """Generate optimized code based on goals"""
        goals_text = ", ".join(goals)
        
        prompt = f"""Optimize this Ruby code for {goals_text}:

```ruby
{code}
```

Target Ruby version: {ruby_version}

Apply optimizations for:
{chr(10).join([f"- {goal.capitalize()}" for goal in goals])}

Ensure the optimized code:
- Maintains all original functionality
- Follows Ruby best practices
- Is compatible with Ruby {ruby_version}
- Includes appropriate error handling"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate clean, optimized Ruby code."
        )
        
        # Extract code from response
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return response
    
    def _identify_improvements(self, original: str, optimized: str, 
                             goals: List[str], model: str) -> List[str]:
        """Identify specific improvements made"""
        prompt = f"""Compare the original and optimized Ruby code.

List specific improvements made for these goals: {', '.join(goals)}

Focus on:
- Performance enhancements
- Memory optimizations
- Code clarity improvements
- Ruby idiom usage
- Design pattern applications

Provide a bullet list of improvements."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="List specific, concrete improvements made."
        )
        
        response = result.get('response', '')
        
        # Extract bullet points
        improvements = []
        for line in response.split('\n'):
            if line.strip().startswith(('-', '•', '*')):
                improvements.append(line.strip()[1:].strip())
        
        return improvements if improvements else ["Code optimized according to specified goals"]
    
    def _generate_refactoring_steps(self, original: str, optimized: str, model: str) -> str:
        """Generate step-by-step refactoring guide"""
        prompt = f"""Create a step-by-step guide to refactor the original code into the optimized version.

Provide:
1. Numbered steps in order
2. Explanation for each change
3. Code snippets showing the transformation
4. Rationale for each optimization"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Create a clear, educational refactoring guide."
        )
        
        return result.get('response', 'Refactoring steps not generated')
    
    def _indent_code(self, code: str, spaces: int) -> str:
        """Indent code by specified spaces"""
        indent = " " * spaces
        return "\n".join([indent + line for line in code.split("\n")])