#!/usr/bin/env python3
"""
Unified Test Runner for TuoKit
Consolidates all test functionality from 30+ test files
Supports multiple test frameworks, parallel execution, and comprehensive reporting
"""

import os
import sys
import time
import json
import unittest
import subprocess
import multiprocessing
import argparse
import traceback
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import importlib.util

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Color codes for console output
class Colors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

class TuoKitTestRunner:
    """Unified test runner for all TuoKit tests"""
    
    def __init__(self, config_path: Optional[str] = None):
        self.config = self._load_config(config_path)
        self.test_results = {
            'passed': 0,
            'failed': 0,
            'skipped': 0,
            'errors': 0,
            'duration': 0,
            'timestamp': datetime.now().isoformat(),
            'details': {}
        }
        self.test_categories = {
            'sql': {
                'description': 'SQL generation, optimization, and pipeline tests',
                'files': [
                    'test_sql_generator.py',
                    'test_sql_generator_enhanced.py', 
                    'test_sql_optimizer.py',
                    'test_sql_pipeline.py',
                    'test_sql_enterprise.py',
                    'test_sql_simple.py',
                    'tests/test_sql_suite.py'
                ]
            },
            'agent': {
                'description': 'Agent system and integration tests',
                'files': [
                    'test_agent_lite.py',
                    'test_agent_system.py'
                ]
            },
            'ollama': {
                'description': 'Ollama integration and connection tests',
                'files': [
                    'test_ollama.py',
                    'test_ollama_autodetect.py',
                    'test_ollama_connection.py',
                    'test_ollama_direct.py',
                    'test_ollama_manager.py',
                    'test_ollama_simple.py'
                ]
            },
            'tools': {
                'description': 'Individual tool tests',
                'files': [
                    'test_unified_tools.py',
                    'tests/test_enhanced_tools.py',
                    'tests/test_new_smalltalk_tools.py',
                    'tests/test_smalltalk_rails_tools.py',
                    'tools/test_scanner.py',
                    'tools/test_enhanced_scanner.py',
                    'tools/test_clipboard.py',
                    'tools/test_syntax_fix_patterns.py'
                ]
            },
            'ui': {
                'description': 'UI and app functionality tests',
                'files': [
                    'test_app.py',
                    'tests/test_crash_analyzer_enhanced.py',
                    'tests/test_crash_analyzer_integration.py',
                    'tests/test_crash_analyzer_5mb.py'
                ]
            },
            'knowledge': {
                'description': 'Knowledge graph and educational feature tests',
                'files': [
                    'test_knowledge_graph.py',
                    'tests/test_edu_mind.py',
                    'tests/test_study_guide.py',
                    'tests/test_study_guide_enhanced.py'
                ]
            },
            'utilities': {
                'description': 'Utility and helper function tests',
                'files': [
                    'test_pdf.py',
                    'tests/test_json_parsing_fix.py',
                    'tests/test_enhanced_features_simple.py',
                    'test_files/test_5mb_final.py',
                    'test_files/test_format_fix.py'
                ]
            }
        }
    
    def _load_config(self, config_path: Optional[str]) -> Dict:
        """Load test configuration from file or use defaults"""
        default_config = {
            'parallel': True,
            'max_workers': multiprocessing.cpu_count(),
            'timeout': 300,  # 5 minutes per test file
            'verbose': 1,
            'coverage': True,
            'html_report': True,
            'json_report': True,
            'skip_slow': False,
            'mock_ollama': True,
            'database': {
                'use_test_db': True,
                'test_db_path': 'test_tuokit.db'
            },
            'environment': {
                'TUOKIT_TEST_MODE': '1',
                'OLLAMA_HOST': 'http://localhost:11434'
            }
        }
        
        if config_path and os.path.exists(config_path):
            try:
                with open(config_path, 'r') as f:
                    if config_path.endswith('.json'):
                        user_config = json.load(f)
                    else:
                        # Support for Python config files
                        spec = importlib.util.spec_from_file_location("config", config_path)
                        config_module = importlib.util.module_from_spec(spec)
                        spec.loader.exec_module(config_module)
                        user_config = config_module.TEST_CONFIG
                    
                    # Merge with defaults
                    default_config.update(user_config)
            except Exception as e:
                print(f"{Colors.WARNING}Warning: Failed to load config from {config_path}: {e}{Colors.ENDC}")
        
        return default_config
    
    def setup_test_environment(self):
        """Set up test environment variables and mock services"""
        # Set environment variables
        for key, value in self.config['environment'].items():
            os.environ[key] = str(value)
        
        # Create test database if needed
        if self.config['database']['use_test_db']:
            test_db_path = self.config['database']['test_db_path']
            if os.path.exists(test_db_path):
                os.remove(test_db_path)
            print(f"{Colors.OKBLUE}Created test database: {test_db_path}{Colors.ENDC}")
        
        # Set up mock Ollama if needed
        if self.config['mock_ollama']:
            self._setup_mock_ollama()
    
    def _setup_mock_ollama(self):
        """Set up mock Ollama responses for testing"""
        sys.modules['ollama'] = type(sys)('ollama')
        sys.modules['ollama'].list = lambda: {'models': [{'name': 'test-model'}]}
        sys.modules['ollama'].generate = lambda **kwargs: {
            'response': 'Mock response for testing'
        }
        print(f"{Colors.OKBLUE}Mock Ollama service enabled{Colors.ENDC}")
    
    def discover_tests(self, category: Optional[str] = None) -> List[str]:
        """Discover all test files or tests in a specific category"""
        test_files = []
        
        if category:
            if category in self.test_categories:
                test_files = self.test_categories[category]['files']
            else:
                print(f"{Colors.WARNING}Unknown category: {category}{Colors.ENDC}")
                return []
        else:
            # Get all test files
            for cat_info in self.test_categories.values():
                test_files.extend(cat_info['files'])
        
        # Filter out non-existent files
        existing_files = []
        for test_file in test_files:
            if os.path.exists(test_file):
                existing_files.append(test_file)
            else:
                print(f"{Colors.WARNING}Test file not found: {test_file}{Colors.ENDC}")
        
        return existing_files
    
    def run_test_file(self, test_file: str) -> Dict[str, Any]:
        """Run a single test file and return results"""
        start_time = time.time()
        result = {
            'file': test_file,
            'passed': 0,
            'failed': 0,
            'errors': 0,
            'skipped': 0,
            'duration': 0,
            'output': '',
            'error': None
        }
        
        try:
            # Determine test type and run appropriately
            if test_file.endswith('.py'):
                # Check if it's a unittest file
                if self._is_unittest_file(test_file):
                    result = self._run_unittest_file(test_file)
                else:
                    # Run as a simple Python script
                    result = self._run_python_script(test_file)
            
            result['duration'] = time.time() - start_time
            
        except Exception as e:
            result['error'] = str(e)
            result['errors'] = 1
            result['output'] = traceback.format_exc()
        
        return result
    
    def _is_unittest_file(self, test_file: str) -> bool:
        """Check if a file contains unittest test cases"""
        try:
            with open(test_file, 'r') as f:
                content = f.read()
                return 'unittest' in content and 'TestCase' in content
        except:
            return False
    
    def _run_unittest_file(self, test_file: str) -> Dict[str, Any]:
        """Run unittest test file"""
        result = {
            'file': test_file,
            'passed': 0,
            'failed': 0,
            'errors': 0,
            'skipped': 0,
            'output': ''
        }
        
        try:
            # Run unittest with subprocess to capture output
            cmd = [sys.executable, '-m', 'unittest', test_file, '-v']
            process = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=self.config['timeout']
            )
            
            result['output'] = process.stdout + process.stderr
            
            # Parse unittest output
            if process.returncode == 0:
                # All tests passed
                test_count = result['output'].count('... ok')
                result['passed'] = test_count
            else:
                # Some tests failed
                result['passed'] = result['output'].count('... ok')
                result['failed'] = result['output'].count('... FAIL')
                result['errors'] = result['output'].count('... ERROR')
                result['skipped'] = result['output'].count('... skipped')
            
        except subprocess.TimeoutExpired:
            result['error'] = f"Test timed out after {self.config['timeout']} seconds"
            result['errors'] = 1
        except Exception as e:
            result['error'] = str(e)
            result['errors'] = 1
        
        return result
    
    def _run_python_script(self, test_file: str) -> Dict[str, Any]:
        """Run a simple Python test script"""
        result = {
            'file': test_file,
            'passed': 0,
            'failed': 0,
            'errors': 0,
            'skipped': 0,
            'output': ''
        }
        
        try:
            # Run script with subprocess
            cmd = [sys.executable, test_file]
            process = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=self.config['timeout'],
                cwd=os.path.dirname(os.path.abspath(test_file)) or '.'
            )
            
            result['output'] = process.stdout + process.stderr
            
            # Simple heuristic: check for success/failure indicators
            output_lower = result['output'].lower()
            
            if process.returncode == 0:
                # Count success indicators
                result['passed'] = (
                    output_lower.count('✓') + 
                    output_lower.count('✅') +
                    output_lower.count('success') +
                    output_lower.count('passed')
                )
                
                # Count failure indicators
                result['failed'] = (
                    output_lower.count('✗') +
                    output_lower.count('❌') +
                    output_lower.count('failed') +
                    output_lower.count('error')
                )
                
                # If no specific counts, assume 1 test passed
                if result['passed'] == 0 and result['failed'] == 0:
                    result['passed'] = 1
            else:
                result['errors'] = 1
                result['error'] = f"Script exited with code {process.returncode}"
        
        except subprocess.TimeoutExpired:
            result['error'] = f"Test timed out after {self.config['timeout']} seconds"
            result['errors'] = 1
        except Exception as e:
            result['error'] = str(e)
            result['errors'] = 1
        
        return result
    
    def run_tests_parallel(self, test_files: List[str]) -> List[Dict[str, Any]]:
        """Run tests in parallel"""
        results = []
        
        with ProcessPoolExecutor(max_workers=self.config['max_workers']) as executor:
            future_to_file = {
                executor.submit(self.run_test_file, test_file): test_file 
                for test_file in test_files
            }
            
            for future in future_to_file:
                try:
                    result = future.result(timeout=self.config['timeout'])
                    results.append(result)
                    self._print_test_result(result)
                except Exception as e:
                    test_file = future_to_file[future]
                    results.append({
                        'file': test_file,
                        'error': str(e),
                        'errors': 1,
                        'passed': 0,
                        'failed': 0,
                        'skipped': 0
                    })
        
        return results
    
    def run_tests_sequential(self, test_files: List[str]) -> List[Dict[str, Any]]:
        """Run tests sequentially"""
        results = []
        
        for test_file in test_files:
            print(f"\n{Colors.HEADER}Running {test_file}...{Colors.ENDC}")
            result = self.run_test_file(test_file)
            results.append(result)
            self._print_test_result(result)
        
        return results
    
    def _print_test_result(self, result: Dict[str, Any]):
        """Print test result with colors"""
        if result.get('error'):
            status = f"{Colors.FAIL}ERROR{Colors.ENDC}"
        elif result['failed'] > 0:
            status = f"{Colors.FAIL}FAILED{Colors.ENDC}"
        elif result['passed'] > 0:
            status = f"{Colors.OKGREEN}PASSED{Colors.ENDC}"
        else:
            status = f"{Colors.WARNING}UNKNOWN{Colors.ENDC}"
        
        print(f"{result['file']}: {status}")
        print(f"  Passed: {result['passed']} | Failed: {result['failed']} | " +
              f"Errors: {result['errors']} | Duration: {result.get('duration', 0):.2f}s")
        
        if self.config['verbose'] > 1 and result.get('output'):
            print(f"\n{Colors.OKCYAN}Output:{Colors.ENDC}")
            print(result['output'][:500] + '...' if len(result['output']) > 500 else result['output'])
    
    def run_all_tests(self, category: Optional[str] = None, 
                     specific_files: Optional[List[str]] = None) -> Dict[str, Any]:
        """Run all tests or specific category/files"""
        print(f"\n{Colors.BOLD}TuoKit Test Runner{Colors.ENDC}")
        print("=" * 60)
        
        # Setup environment
        self.setup_test_environment()
        
        # Determine which tests to run
        if specific_files:
            test_files = specific_files
        else:
            test_files = self.discover_tests(category)
        
        if not test_files:
            print(f"{Colors.WARNING}No test files found!{Colors.ENDC}")
            return self.test_results
        
        print(f"\nDiscovered {len(test_files)} test files")
        if category:
            print(f"Category: {category} - {self.test_categories[category]['description']}")
        
        # Run tests
        start_time = time.time()
        
        if self.config['parallel'] and len(test_files) > 1:
            print(f"\nRunning tests in parallel (max {self.config['max_workers']} workers)...")
            results = self.run_tests_parallel(test_files)
        else:
            print("\nRunning tests sequentially...")
            results = self.run_tests_sequential(test_files)
        
        # Aggregate results
        for result in results:
            self.test_results['passed'] += result['passed']
            self.test_results['failed'] += result['failed']
            self.test_results['errors'] += result['errors']
            self.test_results['skipped'] += result.get('skipped', 0)
            self.test_results['details'][result['file']] = result
        
        self.test_results['duration'] = time.time() - start_time
        
        # Print summary
        self._print_summary()
        
        # Generate reports
        if self.config['html_report']:
            self._generate_html_report()
        
        if self.config['json_report']:
            self._generate_json_report()
        
        if self.config['coverage']:
            self._generate_coverage_report()
        
        return self.test_results
    
    def _print_summary(self):
        """Print test run summary"""
        print(f"\n{Colors.BOLD}Test Summary{Colors.ENDC}")
        print("=" * 60)
        
        total_tests = (self.test_results['passed'] + self.test_results['failed'] + 
                      self.test_results['errors'] + self.test_results['skipped'])
        
        print(f"Total Tests: {total_tests}")
        print(f"{Colors.OKGREEN}Passed: {self.test_results['passed']}{Colors.ENDC}")
        print(f"{Colors.FAIL}Failed: {self.test_results['failed']}{Colors.ENDC}")
        print(f"{Colors.FAIL}Errors: {self.test_results['errors']}{Colors.ENDC}")
        print(f"{Colors.WARNING}Skipped: {self.test_results['skipped']}{Colors.ENDC}")
        print(f"\nDuration: {self.test_results['duration']:.2f} seconds")
        
        # Success rate
        if total_tests > 0:
            success_rate = (self.test_results['passed'] / total_tests) * 100
            color = Colors.OKGREEN if success_rate >= 80 else Colors.WARNING if success_rate >= 60 else Colors.FAIL
            print(f"\n{color}Success Rate: {success_rate:.1f}%{Colors.ENDC}")
    
    def _generate_html_report(self):
        """Generate HTML test report"""
        html_template = """
<!DOCTYPE html>
<html>
<head>
    <title>TuoKit Test Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }
        .header { background-color: #333; color: white; padding: 20px; border-radius: 5px; }
        .summary { background-color: white; padding: 20px; margin: 20px 0; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .test-file { background-color: white; padding: 15px; margin: 10px 0; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .passed { color: #28a745; font-weight: bold; }
        .failed { color: #dc3545; font-weight: bold; }
        .error { color: #ff6b6b; font-weight: bold; }
        .skipped { color: #ffc107; font-weight: bold; }
        .metric { display: inline-block; margin: 10px 20px; }
        .output { background-color: #f8f9fa; padding: 10px; border-radius: 3px; margin-top: 10px; font-family: monospace; font-size: 12px; max-height: 300px; overflow-y: auto; }
        .progress-bar { width: 100%; height: 20px; background-color: #e0e0e0; border-radius: 10px; overflow: hidden; }
        .progress-fill { height: 100%; background-color: #28a745; transition: width 0.3s; }
    </style>
</head>
<body>
    <div class="header">
        <h1>TuoKit Test Report</h1>
        <p>Generated: {timestamp}</p>
    </div>
    
    <div class="summary">
        <h2>Summary</h2>
        <div class="metric">Total: {total}</div>
        <div class="metric passed">Passed: {passed}</div>
        <div class="metric failed">Failed: {failed}</div>
        <div class="metric error">Errors: {errors}</div>
        <div class="metric skipped">Skipped: {skipped}</div>
        <div class="metric">Duration: {duration:.2f}s</div>
        
        <div style="margin-top: 20px;">
            <div class="progress-bar">
                <div class="progress-fill" style="width: {success_rate}%"></div>
            </div>
            <p>Success Rate: {success_rate:.1f}%</p>
        </div>
    </div>
    
    <h2>Test Details</h2>
    {test_details}
</body>
</html>
        """
        
        # Generate test details HTML
        test_details_html = ""
        for file_path, result in self.test_results['details'].items():
            status_class = 'passed' if result['failed'] == 0 and result['errors'] == 0 else 'failed'
            
            test_details_html += f"""
            <div class="test-file">
                <h3>{file_path}</h3>
                <div>
                    <span class="passed">Passed: {result['passed']}</span> | 
                    <span class="failed">Failed: {result['failed']}</span> | 
                    <span class="error">Errors: {result['errors']}</span> | 
                    Duration: {result.get('duration', 0):.2f}s
                </div>
                {f'<div class="output">{result["output"]}</div>' if result.get('output') else ''}
            </div>
            """
        
        # Calculate totals
        total_tests = (self.test_results['passed'] + self.test_results['failed'] + 
                      self.test_results['errors'] + self.test_results['skipped'])
        success_rate = (self.test_results['passed'] / total_tests * 100) if total_tests > 0 else 0
        
        # Generate final HTML
        html_content = html_template.format(
            timestamp=self.test_results['timestamp'],
            total=total_tests,
            passed=self.test_results['passed'],
            failed=self.test_results['failed'],
            errors=self.test_results['errors'],
            skipped=self.test_results['skipped'],
            duration=self.test_results['duration'],
            success_rate=success_rate,
            test_details=test_details_html
        )
        
        # Save report
        report_path = f"test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.html"
        with open(report_path, 'w') as f:
            f.write(html_content)
        
        print(f"\n{Colors.OKBLUE}HTML report saved to: {report_path}{Colors.ENDC}")
    
    def _generate_json_report(self):
        """Generate JSON test report for CI/CD"""
        report_path = f"test_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(report_path, 'w') as f:
            json.dump(self.test_results, f, indent=2)
        
        print(f"{Colors.OKBLUE}JSON report saved to: {report_path}{Colors.ENDC}")
    
    def _generate_coverage_report(self):
        """Generate test coverage report"""
        try:
            # Try to run coverage if installed
            subprocess.run(['coverage', 'report'], capture_output=True)
            subprocess.run(['coverage', 'html'])
            print(f"{Colors.OKBLUE}Coverage report generated in htmlcov/{Colors.ENDC}")
        except:
            print(f"{Colors.WARNING}Coverage not available. Install with: pip install coverage{Colors.ENDC}")
    
    def list_tests(self):
        """List all available tests by category"""
        print(f"\n{Colors.BOLD}Available Test Categories:{Colors.ENDC}")
        print("=" * 60)
        
        for category, info in self.test_categories.items():
            print(f"\n{Colors.OKGREEN}{category}{Colors.ENDC}: {info['description']}")
            for test_file in info['files']:
                exists = "✓" if os.path.exists(test_file) else "✗"
                print(f"  {exists} {test_file}")


def main():
    """Main entry point for test runner"""
    parser = argparse.ArgumentParser(description='TuoKit Unified Test Runner')
    parser.add_argument('--category', '-c', help='Run tests from specific category')
    parser.add_argument('--file', '-f', nargs='+', help='Run specific test files')
    parser.add_argument('--list', '-l', action='store_true', help='List all available tests')
    parser.add_argument('--config', help='Path to test configuration file')
    parser.add_argument('--parallel', '-p', action='store_true', help='Run tests in parallel')
    parser.add_argument('--sequential', '-s', action='store_true', help='Run tests sequentially')
    parser.add_argument('--verbose', '-v', action='count', default=1, help='Increase verbosity')
    parser.add_argument('--no-html', action='store_true', help='Skip HTML report generation')
    parser.add_argument('--no-json', action='store_true', help='Skip JSON report generation')
    parser.add_argument('--timeout', type=int, help='Test timeout in seconds')
    parser.add_argument('--workers', type=int, help='Number of parallel workers')
    
    args = parser.parse_args()
    
    # Create test runner
    runner = TuoKitTestRunner(config_path=args.config)
    
    # Override config with command line arguments
    if args.parallel:
        runner.config['parallel'] = True
    elif args.sequential:
        runner.config['parallel'] = False
    
    if args.verbose:
        runner.config['verbose'] = args.verbose
    
    if args.no_html:
        runner.config['html_report'] = False
    
    if args.no_json:
        runner.config['json_report'] = False
    
    if args.timeout:
        runner.config['timeout'] = args.timeout
    
    if args.workers:
        runner.config['max_workers'] = args.workers
    
    # Execute requested action
    if args.list:
        runner.list_tests()
    else:
        results = runner.run_all_tests(
            category=args.category,
            specific_files=args.file
        )
        
        # Exit with appropriate code
        if results['failed'] > 0 or results['errors'] > 0:
            sys.exit(1)
        else:
            sys.exit(0)


if __name__ == '__main__':
    main()