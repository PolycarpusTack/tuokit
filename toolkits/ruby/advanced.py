"""
Advanced Ruby features - C Extensions, Ractors, Pattern Matching
"""

import streamlit as st
from typing import Dict, Any, List, Optional
import re
from datetime import datetime

from utils.ollama import safe_ollama_generate
from .config import C_EXTENSION_TEMPLATES


class CExtensionBuilder:
    """Ruby C Extension builder and generator"""
    
    def generate(self, extension_type: str, description: str = None,
                include_tests: bool = True, include_benchmarks: bool = True,
                include_makefile: bool = True, include_docs: bool = True) -> Dict[str, Any]:
        """Generate a complete C extension"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Get extension details
            if extension_type == "Custom":
                if not description:
                    return {'success': False, 'error': 'Description required for custom extension'}
                ext_description = description
            else:
                template = C_EXTENSION_TEMPLATES.get(extension_type, {})
                ext_description = template.get('description', extension_type)
            
            # Generate C source code
            c_source = self._generate_c_source(extension_type, ext_description, model)
            
            # Generate Ruby wrapper
            ruby_wrapper = self._generate_ruby_wrapper(extension_type, ext_description, model)
            
            # Generate tests if requested
            tests = ""
            if include_tests:
                tests = self._generate_tests(extension_type, ext_description, model)
            
            # Generate Makefile if requested
            makefile = ""
            if include_makefile:
                makefile = self._generate_makefile(extension_type)
            
            # Generate usage example
            usage_example = self._generate_usage_example(extension_type, ext_description, model)
            
            return {
                'success': True,
                'c_source': c_source,
                'ruby_wrapper': ruby_wrapper,
                'tests': tests,
                'makefile': makefile,
                'usage_example': usage_example
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _generate_c_source(self, ext_type: str, description: str, model: str) -> str:
        """Generate C source code for extension"""
        template = C_EXTENSION_TEMPLATES.get(ext_type, {})
        includes = template.get('includes', ['ruby.h'])
        
        includes_text = "\n".join([f'#include <{inc}>' for inc in includes])
        
        prompt = f"""Generate a Ruby C extension for: {description}

Extension type: {ext_type}
Required includes: {', '.join(includes)}

Generate complete C code with:
1. Proper Ruby C API usage
2. Error handling
3. Memory management
4. Type checking
5. Module and method definitions
6. Documentation comments

Focus on performance and safety."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a C and Ruby expert. Generate production-ready C extension code."
        )
        
        # Extract code or use response
        response = result.get('response', '')
        code_match = re.search(r'```c\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Generate a template if AI fails
        return f"""{includes_text}

// {description}
static VALUE rb_module;

// Example method implementation
static VALUE
example_method(VALUE self, VALUE arg)
{{
    // Type checking
    Check_Type(arg, T_STRING);
    
    // Implementation here
    char *str = StringValueCStr(arg);
    
    // Return result
    return rb_str_new_cstr("Result");
}}

// Extension initialization
void
Init_extension(void)
{{
    rb_module = rb_define_module("Extension");
    rb_define_module_function(rb_module, "example_method", example_method, 1);
}}"""
    
    def _generate_ruby_wrapper(self, ext_type: str, description: str, model: str) -> str:
        """Generate Ruby wrapper for C extension"""
        prompt = f"""Generate a Ruby wrapper for a C extension: {description}

Create a clean Ruby API that:
1. Loads the compiled extension
2. Provides Ruby-friendly method names
3. Adds input validation
4. Includes documentation
5. Handles errors gracefully"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate idiomatic Ruby wrapper code."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Template fallback
        return f"""# Ruby wrapper for {ext_type} extension
require_relative 'extension'

module {ext_type.replace(' ', '')}
  class << self
    # Main method with Ruby-friendly interface
    def process(input)
      validate_input(input)
      Extension.example_method(input)
    rescue => e
      handle_error(e)
    end
    
    private
    
    def validate_input(input)
      raise ArgumentError, "Input required" if input.nil?
    end
    
    def handle_error(error)
      raise ExtensionError, "Extension error: #{{error.message}}"
    end
  end
  
  class ExtensionError < StandardError; end
end"""
    
    def _generate_tests(self, ext_type: str, description: str, model: str) -> str:
        """Generate test suite for C extension"""
        prompt = f"""Generate RSpec tests for a Ruby C extension: {description}

Include tests for:
1. Basic functionality
2. Edge cases
3. Error handling
4. Performance characteristics
5. Memory safety"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate comprehensive RSpec tests."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Template fallback
        return f"""require 'rspec'
require_relative '../lib/{ext_type.lower().replace(' ', '_')}'

RSpec.describe {ext_type.replace(' ', '')} do
  describe '.process' do
    it 'processes valid input' do
      result = described_class.process("test")
      expect(result).to be_a(String)
    end
    
    it 'handles nil input' do
      expect {{ described_class.process(nil) }}.to raise_error(ArgumentError)
    end
    
    it 'performs efficiently' do
      time = Benchmark.realtime do
        1000.times {{ described_class.process("test") }}
      end
      expect(time).to be < 0.1
    end
  end
end"""
    
    def _generate_makefile(self, ext_type: str) -> str:
        """Generate Makefile for C extension"""
        ext_name = ext_type.lower().replace(' ', '_')
        
        return f"""# Makefile for {ext_type} extension

SHELL = /bin/sh

# V=0 quiet, V=1 verbose
V = 0
Q1 = $(V:1=)
Q = $(Q1:0=@)
ECHO1 = $(V:1=@ :)
ECHO = $(ECHO1:0=@ echo)

#### Start of system configuration section. ####

srcdir = .
topdir = $(shell $(RUBY) -e 'print RbConfig::CONFIG["rubyhdrdir"]')
hdrdir = $(topdir)
arch_hdrdir = $(shell $(RUBY) -e 'print RbConfig::CONFIG["rubyarchhdrdir"]')
VPATH = $(srcdir)

CC = gcc
CXX = g++
RUBY = ruby
RM = rm -f

CFLAGS = -O3 -Wall -Wextra -Wno-unused-parameter -Wno-parentheses -Wno-long-long
CXXFLAGS = $(CFLAGS)
CPPFLAGS = -I. -I$(hdrdir) -I$(arch_hdrdir)
DLDFLAGS = -shared
LDSHARED = $(CC) -shared

TARGET = {ext_name}
DLLIB = $(TARGET).so
OBJS = {ext_name}.o

all: $(DLLIB)

clean:
\t@-$(RM) $(OBJS) $(DLLIB)

$(DLLIB): $(OBJS) Makefile
\t$(ECHO) linking shared-object $(DLLIB)
\t$(Q) $(LDSHARED) -o $@ $(OBJS) $(DLDFLAGS)

%.o: %.c
\t$(ECHO) compiling $(<)
\t$(Q) $(CC) $(CFLAGS) $(CPPFLAGS) -c $<

# Dependencies
{ext_name}.o: {ext_name}.c"""
    
    def _generate_usage_example(self, ext_type: str, description: str, model: str) -> str:
        """Generate usage example for the extension"""
        prompt = f"""Generate a usage example for a Ruby C extension: {description}

Show:
1. How to require the extension
2. Basic usage examples
3. Advanced usage patterns
4. Performance comparison with pure Ruby
5. Best practices"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate clear, practical usage examples."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        
        # Template fallback
        return f"""# Using the {ext_type} extension
require_relative 'lib/{ext_type.lower().replace(' ', '_')}'

# Basic usage
result = {ext_type.replace(' ', '')}::process("input data")
puts result

# Performance comparison
require 'benchmark'

n = 100_000
Benchmark.bm do |x|
  x.report("C Extension:") {{ n.times {{ {ext_type.replace(' ', '')}::process("test") }} }}
  x.report("Pure Ruby:") {{ n.times {{ pure_ruby_version("test") }} }}
end

# Advanced usage
data = ["item1", "item2", "item3"]
results = data.map {{ |item| {ext_type.replace(' ', '')}::process(item) }}"""


class RactorGuide:
    """Ruby Ractor concurrency guide and examples"""
    
    def show_interface(self):
        """Show Ractor guide interface"""
        st.markdown("### 🔀 Ruby Ractors Guide")
        st.caption("Learn and implement Ruby's Actor-model concurrency with Ractors")
        
        guide_type = st.selectbox(
            "Select Guide Type",
            ["Introduction to Ractors", "Basic Examples", "Advanced Patterns", 
             "Convert Code to Ractors", "Ractor Best Practices"]
        )
        
        if guide_type == "Convert Code to Ractors":
            code = st.text_area(
                "Ruby Code to Convert",
                height=200,
                placeholder="""# Sequential processing
def process_items(items)
  items.map { |item| expensive_operation(item) }
end""",
                key="ractor_code"
            )
            
            if st.button("🔄 Convert to Ractors", type="primary"):
                if code:
                    with st.spinner("Converting to Ractor-based implementation..."):
                        result = self.convert_to_ractors(code)
                        
                        if result.get('success'):
                            # Show results
                            col1, col2 = st.columns(2)
                            
                            with col1:
                                st.markdown("**Original Code**")
                                st.code(code, language='ruby')
                            
                            with col2:
                                st.markdown("**Ractor Implementation**")
                                st.code(result.get('ractor_code', ''), language='ruby')
                            
                            # Explanation
                            with st.expander("💡 Explanation", expanded=True):
                                st.markdown(result.get('explanation', ''))
                            
                            # Performance notes
                            if result.get('performance_notes'):
                                st.info(f"⚡ {result['performance_notes']}")
                        else:
                            st.error(f"Conversion failed: {result.get('error')}")
                else:
                    st.warning("Please provide code to convert")
        else:
            # Show guide content
            self._show_guide_content(guide_type)
    
    def convert_to_ractors(self, code: str) -> Dict[str, Any]:
        """Convert sequential code to use Ractors"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Generate Ractor implementation
            ractor_code = self._generate_ractor_code(code, model)
            
            # Generate explanation
            explanation = self._generate_explanation(code, ractor_code, model)
            
            # Performance analysis
            performance_notes = self._analyze_performance(code, ractor_code, model)
            
            return {
                'success': True,
                'ractor_code': ractor_code,
                'explanation': explanation,
                'performance_notes': performance_notes
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _generate_ractor_code(self, code: str, model: str) -> str:
        """Generate Ractor-based implementation"""
        prompt = f"""Convert this Ruby code to use Ractors for parallel execution:

```ruby
{code}
```

Requirements:
1. Use Ractors for parallelism
2. Handle communication between Ractors
3. Ensure thread safety
4. Maintain functionality
5. Add error handling

Generate clean, idiomatic Ractor code."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="You are a Ruby concurrency expert. Generate correct Ractor implementations."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return response
    
    def _generate_explanation(self, original: str, ractor_code: str, model: str) -> str:
        """Generate explanation of Ractor implementation"""
        prompt = f"""Explain how the Ractor implementation works and its benefits.

Cover:
1. How Ractors provide parallelism
2. Communication patterns used
3. Thread safety guarantees
4. Performance implications
5. When to use this pattern"""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide clear, educational explanations about Ractors."
        )
        
        return result.get('response', 'Explanation not available')
    
    def _analyze_performance(self, original: str, ractor_code: str, model: str) -> str:
        """Analyze performance implications"""
        prompt = "Analyze the performance benefits of using Ractors for this code. Provide specific insights."
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Provide realistic performance analysis."
        )
        
        return result.get('response', '')
    
    def _show_guide_content(self, guide_type: str):
        """Show Ractor guide content"""
        if guide_type == "Introduction to Ractors":
            st.markdown("""
            ## Introduction to Ruby Ractors
            
            Ractors (Ruby Actors) provide a parallel execution feature without thread-safety concerns.
            
            ### Key Concepts:
            
            1. **Isolation**: Each Ractor has its own object space
            2. **Communication**: Message passing between Ractors
            3. **No Shared State**: Objects can't be shared (with exceptions)
            4. **True Parallelism**: Ractors can run in parallel
            
            ### Basic Example:
            ```ruby
            # Create a Ractor
            r = Ractor.new do
              # This runs in parallel
              42
            end
            
            # Get the result
            result = r.take  # => 42
            ```
            
            ### Communication Patterns:
            ```ruby
            # Push pattern
            r = Ractor.new do
              msg = Ractor.receive
              msg * 2
            end
            
            r.send(21)
            r.take  # => 42
            
            # Pull pattern
            r1 = Ractor.new do
              'hello'
            end
            
            r2 = Ractor.new(r1) do |r1|
              msg = r1.take
              msg.upcase
            end
            
            r2.take  # => "HELLO"
            ```
            """)
            
        elif guide_type == "Basic Examples":
            st.markdown("""
            ## Basic Ractor Examples
            
            ### Parallel Map
            ```ruby
            def parallel_map(array, &block)
              ractors = array.map do |item|
                Ractor.new(item) { |i| block.call(i) }
              end
              ractors.map(&:take)
            end
            
            # Usage
            results = parallel_map([1, 2, 3, 4]) { |n| n ** 2 }
            # => [1, 4, 9, 16]
            ```
            
            ### Worker Pool
            ```ruby
            def worker_pool(num_workers, jobs)
              pipe = Ractor.new do
                loop do
                  Ractor.yield(Ractor.receive)
                end
              end
              
              workers = num_workers.times.map do
                Ractor.new(pipe) do |pipe|
                  loop do
                    job = pipe.take
                    Ractor.yield(process_job(job))
                  end
                end
              end
              
              # Send jobs
              jobs.each { |job| pipe.send(job) }
              
              # Collect results
              jobs.size.times.map do
                Ractor.select(*workers)[1]
              end
            end
            ```
            
            ### Pipeline Processing
            ```ruby
            # Stage 1: Read data
            reader = Ractor.new do
              data = File.read('input.txt')
              data.split("\\n").each { |line| Ractor.yield(line) }
            end
            
            # Stage 2: Process
            processor = Ractor.new(reader) do |r|
              loop do
                line = r.take
                processed = line.upcase.strip
                Ractor.yield(processed)
              end
            end
            
            # Stage 3: Write
            writer = Ractor.new(processor) do |r|
              File.open('output.txt', 'w') do |f|
                loop do
                  f.puts r.take
                end
              end
            end
            ```
            """)
            
        elif guide_type == "Advanced Patterns":
            st.markdown("""
            ## Advanced Ractor Patterns
            
            ### Supervisor Pattern
            ```ruby
            class RactorSupervisor
              def initialize(worker_count)
                @workers = create_workers(worker_count)
                @supervisor = create_supervisor
              end
              
              private
              
              def create_workers(count)
                count.times.map do |i|
                  Ractor.new(name: "worker-#{i}") do
                    loop do
                      task = Ractor.receive
                      result = process_task(task)
                      Ractor.yield(result)
                    rescue => e
                      Ractor.yield({error: e.message})
                    end
                  end
                end
              end
              
              def create_supervisor
                Ractor.new(@workers) do |workers|
                  loop do
                    # Monitor workers
                    ready = Ractor.select(*workers)
                    # Restart failed workers if needed
                  end
                end
              end
            end
            ```
            
            ### Shareable Objects
            ```ruby
            # Make objects shareable
            class SharedConfig
              include Ractor::Shareable
              
              def initialize(settings)
                @settings = settings.freeze
                freeze
              end
              
              attr_reader :settings
            end
            
            config = SharedConfig.new({timeout: 30})
            
            # Can be shared between Ractors
            r1 = Ractor.new(config) { |c| c.settings[:timeout] }
            r2 = Ractor.new(config) { |c| c.settings[:timeout] }
            ```
            """)
            
        elif guide_type == "Ractor Best Practices":
            st.markdown("""
            ## Ractor Best Practices
            
            ### ✅ DO:
            
            1. **Use Immutable Data**
               ```ruby
               data = [1, 2, 3].freeze
               Ractor.new(data) { |d| d.sum }
               ```
            
            2. **Handle Errors Gracefully**
               ```ruby
               Ractor.new do
                 begin
                   risky_operation
                 rescue => e
                   {error: e.message}
                 end
               end
               ```
            
            3. **Use Meaningful Names**
               ```ruby
               Ractor.new(name: "image-processor") do
                 # Processing logic
               end
               ```
            
            ### ❌ DON'T:
            
            1. **Share Mutable Objects**
               ```ruby
               # Bad
               array = [1, 2, 3]
               Ractor.new(array) { |a| a << 4 }  # Error!
               ```
            
            2. **Use Global Variables**
               ```ruby
               # Bad
               $global = "data"
               Ractor.new { puts $global }  # Error!
               ```
            
            3. **Ignore Ractor Limits**
               ```ruby
               # Consider system resources
               num_ractors = [Etc.nprocessors, max_tasks].min
               ```
            
            ### Performance Tips:
            
            1. **Batch Work**: Send larger chunks instead of individual items
            2. **Minimize Communication**: Reduce message passing overhead
            3. **Use Select Wisely**: `Ractor.select` for multiple Ractors
            4. **Profile First**: Measure before optimizing
            """)


class PatternMatcher:
    """Ruby pattern matching guide and converter"""
    
    def show_interface(self):
        """Show pattern matching interface"""
        st.markdown("### 🎯 Ruby Pattern Matching")
        st.caption("Modern pattern matching in Ruby 3.0+")
        
        tool_type = st.selectbox(
            "Select Tool",
            ["Convert to Pattern Matching", "Pattern Matching Guide", 
             "Examples Gallery", "Performance Comparison"]
        )
        
        if tool_type == "Convert to Pattern Matching":
            code = st.text_area(
                "Ruby Code to Convert",
                height=200,
                placeholder="""def process_data(data)
  if data.is_a?(Hash) && data[:type] == 'user'
    handle_user(data[:name], data[:age])
  elsif data.is_a?(Array) && data.length == 2
    handle_pair(data[0], data[1])
  else
    handle_other(data)
  end
end""",
                key="pattern_code"
            )
            
            pattern_style = st.radio(
                "Pattern Style",
                ["case/in", "Rightward assignment", "Find pattern", "Array pattern"],
                horizontal=True
            )
            
            if st.button("🔄 Convert to Pattern Matching", type="primary"):
                if code:
                    with st.spinner("Converting to pattern matching..."):
                        result = self.convert_to_patterns(code, pattern_style)
                        
                        if result.get('success'):
                            # Show results
                            col1, col2 = st.columns(2)
                            
                            with col1:
                                st.markdown("**Original Code**")
                                st.code(code, language='ruby')
                            
                            with col2:
                                st.markdown("**Pattern Matching Version**")
                                st.code(result.get('pattern_code', ''), language='ruby')
                            
                            # Benefits
                            if result.get('benefits'):
                                st.success("✨ Benefits:")
                                for benefit in result['benefits']:
                                    st.write(f"• {benefit}")
                        else:
                            st.error(f"Conversion failed: {result.get('error')}")
                else:
                    st.warning("Please provide code to convert")
        else:
            # Show other tools
            self._show_pattern_content(tool_type)
    
    def convert_to_patterns(self, code: str, style: str) -> Dict[str, Any]:
        """Convert code to use pattern matching"""
        try:
            model = st.session_state.get("selected_model", "deepseek-r1:latest")
            
            # Generate pattern matching code
            pattern_code = self._generate_pattern_code(code, style, model)
            
            # Identify benefits
            benefits = self._identify_benefits(code, pattern_code, model)
            
            return {
                'success': True,
                'pattern_code': pattern_code,
                'benefits': benefits
            }
            
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _generate_pattern_code(self, code: str, style: str, model: str) -> str:
        """Generate pattern matching version of code"""
        prompt = f"""Convert this Ruby code to use pattern matching ({style} style):

```ruby
{code}
```

Use Ruby 3.0+ pattern matching features:
- case/in expressions
- Array and hash patterns
- Variable binding
- Guard clauses
- Find patterns

Make the code more readable and maintainable."""
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Generate idiomatic Ruby pattern matching code."
        )
        
        response = result.get('response', '')
        code_match = re.search(r'```ruby\n(.*?)\n```', response, re.DOTALL)
        if code_match:
            return code_match.group(1)
        return response
    
    def _identify_benefits(self, original: str, pattern_code: str, model: str) -> List[str]:
        """Identify benefits of pattern matching version"""
        prompt = "List the specific benefits of using pattern matching for this code transformation."
        
        result = safe_ollama_generate(
            model=model,
            prompt=prompt,
            system="Identify concrete benefits of pattern matching."
        )
        
        response = result.get('response', '')
        
        # Extract benefits
        benefits = []
        for line in response.split('\n'):
            if line.strip().startswith(('-', '•', '*')):
                benefits.append(line.strip()[1:].strip())
        
        return benefits if benefits else ["More readable and maintainable code"]
    
    def _show_pattern_content(self, content_type: str):
        """Show pattern matching content"""
        if content_type == "Pattern Matching Guide":
            st.markdown("""
            ## Ruby Pattern Matching Guide
            
            ### Basic Syntax
            ```ruby
            case value
            in pattern
              # match
            end
            ```
            
            ### Pattern Types
            
            #### Value Patterns
            ```ruby
            case x
            in 0  # matches 0
            in 1..10  # matches range
            in /^Ruby/  # matches regex
            end
            ```
            
            #### Array Patterns
            ```ruby
            case array
            in []  # empty array
            in [a]  # single element
            in [a, b]  # two elements
            in [a, *rest]  # first and rest
            in [*, last]  # all but last
            end
            ```
            
            #### Hash Patterns
            ```ruby
            case hash
            in {name: "John"}  # specific value
            in {name:, age:}  # capture values
            in {name:, **rest}  # capture rest
            end
            ```
            
            #### With Guards
            ```ruby
            case user
            in {age:} if age >= 18
              "Adult"
            in {age:}
              "Minor"
            end
            ```
            """)
            
        elif content_type == "Examples Gallery":
            st.markdown("""
            ## Pattern Matching Examples
            
            ### API Response Handler
            ```ruby
            def handle_response(response)
              case response
              in {status: 200, body:}
                process_success(body)
              in {status: 404}
                handle_not_found
              in {status: 500..599, error:}
                log_server_error(error)
              else
                handle_unknown_response
              end
            end
            ```
            
            ### Data Transformer
            ```ruby
            def transform_data(data)
              case data
              in String => s
                {type: 'string', value: s}
              in Integer | Float => n
                {type: 'number', value: n}
              in [*, last] if last.is_a?(Hash)
                {type: 'array_with_options', items: data[0...-1], options: last}
              in Array => arr
                {type: 'array', items: arr}
              in Hash => h
                {type: 'object', properties: h}
              end
            end
            ```
            
            ### Command Parser
            ```ruby
            def parse_command(input)
              case input.split
              in ["help", command]
                show_help(command)
              in ["create", type, name, *options]
                create_resource(type, name, options)
              in ["delete", id] if id.match?(/^\d+$/)
                delete_by_id(id.to_i)
              in ["list", *filters]
                list_resources(filters)
              else
                show_general_help
              end
            end
            ```
            """)
            
        elif content_type == "Performance Comparison":
            st.markdown("""
            ## Pattern Matching Performance
            
            ### Benchmarks
            
            Pattern matching can be faster than traditional conditionals:
            
            ```ruby
            require 'benchmark/ips'
            
            data = {type: 'user', name: 'John', age: 30}
            
            Benchmark.ips do |x|
              x.report("if/elsif") do
                if data[:type] == 'user' && data[:name] && data[:age]
                  [data[:name], data[:age]]
                elsif data[:type] == 'admin'
                  'admin'
                end
              end
              
              x.report("pattern matching") do
                case data
                in {type: 'user', name:, age:}
                  [name, age]
                in {type: 'admin'}
                  'admin'
                end
              end
              
              x.compare!
            end
            ```
            
            ### When to Use
            
            ✅ **Use Pattern Matching When:**
            - Destructuring complex data structures
            - Multiple conditions on same value
            - Extracting nested values
            - Type checking with value extraction
            
            ❌ **Avoid When:**
            - Simple boolean conditions
            - Performance critical paths (benchmark first!)
            - Ruby < 3.0
            """)