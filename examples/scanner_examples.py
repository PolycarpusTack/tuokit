#!/usr/bin/env python
"""
Example: Using TuoKit Enhanced Scanner v2.0 Programmatically
This shows how to integrate the scanner into automated workflows
"""

import sys
from pathlib import Path
import json

# Add the scanner to Python path
sys.path.append(str(Path(__file__).parent))

from enhanced_scanner_v2 import (
    EnhancedCodeScannerV2,
    TechnologyDetector,
    DependencyAnalyzer,
    ScanProfile,
    SARIFExporter
)

def automated_security_scan(project_path: str):
    """Example: Automated security-focused scan"""
    print(f"🔍 Running automated security scan on: {project_path}")
    
    # Get security profile
    profile_manager = ScanProfile()
    security_profile = profile_manager.get_profile('security')
    
    # Initialize scanner
    scanner = EnhancedCodeScannerV2(Path(project_path), security_profile)
    
    # Run technology detection
    print("📋 Detecting technologies...")
    technologies = scanner.tech_detector.detect(scanner.root_path)
    print(f"Found: {', '.join(technologies.keys())}")
    
    # Scan files
    print("📁 Scanning files...")
    files = scanner.scan_directory()
    print(f"Found {len(files)} files to analyze")
    
    # Analyze each file
    for i, file_path in enumerate(files):
        if i % 10 == 0:
            print(f"Progress: {i}/{len(files)} files...")
        scanner.analyze_file(file_path)
    
    # Calculate scores
    scanner.calculate_scores()
    
    # Generate report
    print("\n📊 Security Scan Results:")
    print(f"Security Score: {scanner.metrics['security_score']}%")
    
    # Check for critical issues
    critical_issues = [
        issue for issue in scanner.issues.get('security', [])
        if issue['severity'] == 'critical'
    ]
    
    if critical_issues:
        print(f"\n🚨 CRITICAL SECURITY ISSUES FOUND: {len(critical_issues)}")
        for issue in critical_issues[:5]:
            print(f"  - {issue['type']}: {issue['file']}:{issue['line']}")
        
        # Export SARIF for CI/CD
        sarif_path = Path(project_path) / 'security_scan.sarif'
        SARIFExporter.export(scanner, sarif_path)
        print(f"\n📄 SARIF report exported to: {sarif_path}")
        
        return False  # Fail the build
    
    print("\n✅ No critical security issues found!")
    return True

def check_dependencies(project_path: str):
    """Example: Check for vulnerable dependencies"""
    print(f"\n📦 Checking dependencies in: {project_path}")
    
    analyzer = DependencyAnalyzer()
    results = analyzer.analyze(Path(project_path))
    
    print(f"Total dependencies: {results['total_dependencies']}")
    print(f"Package managers: {len(results['package_managers'])}")
    
    if results['vulnerabilities']:
        print(f"\n⚠️  Found {len(results['vulnerabilities'])} vulnerable dependencies:")
        for vuln in results['vulnerabilities']:
            print(f"  - {vuln['package']} ({vuln['vulnerability']}): {vuln['fix']}")
        return False
    
    if results['missing_lock_files']:
        print(f"\n⚠️  Missing lock files for: {', '.join(results['missing_lock_files'])}")
        print("  This may cause reproducibility issues!")
    
    return True

def generate_quality_report(project_path: str, output_file: str = "quality_report.json"):
    """Example: Generate a quality report for tracking"""
    print(f"\n📈 Generating quality report for: {project_path}")
    
    # Use full analysis profile
    profile_manager = ScanProfile()
    full_profile = profile_manager.get_profile('full')
    
    # Quick scan for demo (normally you'd do full scan)
    full_profile['max_files'] = 100  # Limit for demo
    
    scanner = EnhancedCodeScannerV2(Path(project_path), full_profile)
    
    # Run full analysis
    files = scanner.scan_directory()
    for file_path in files:
        scanner.analyze_file(file_path)
    
    scanner.calculate_scores()
    
    # Create quality report
    report = {
        'project': str(project_path),
        'scan_date': str(Path.ctime(Path.cwd())),
        'scores': {
            'overall': scanner.metrics['health_score'],
            'security': scanner.metrics['security_score'],
            'quality': scanner.metrics['quality_score'],
            'performance': scanner.metrics['performance_score']
        },
        'metrics': {
            'total_files': scanner.metrics['total_files'],
            'total_lines': scanner.metrics['total_lines'],
            'languages': dict(scanner.metrics['languages'])
        },
        'issues': {
            issue_type: len(issues)
            for issue_type, issues in scanner.issues.items()
        },
        'technologies': scanner.metrics.get('technologies', {})
    }
    
    # Save report
    output_path = Path(project_path) / output_file
    with open(output_path, 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n📊 Quality report saved to: {output_path}")
    print(f"Overall health score: {report['scores']['overall']}%")
    
    return report

def ci_cd_integration_example():
    """Example: How to integrate with CI/CD pipelines"""
    print("\n🔧 CI/CD Integration Example")
    print("=" * 50)
    
    # Get project path from environment or argument
    project_path = sys.argv[1] if len(sys.argv) > 1 else "."
    
    # Run security scan
    security_passed = automated_security_scan(project_path)
    
    # Check dependencies
    deps_passed = check_dependencies(project_path)
    
    # Generate quality report
    quality_report = generate_quality_report(project_path)
    
    # Determine build status
    print("\n" + "=" * 50)
    print("BUILD STATUS SUMMARY")
    print("=" * 50)
    
    if not security_passed:
        print("❌ Build FAILED: Critical security issues found")
        sys.exit(1)
    
    if not deps_passed:
        print("⚠️  Build WARNING: Vulnerable dependencies detected")
        # Could fail build here if strict mode
    
    if quality_report['scores']['overall'] < 70:
        print(f"⚠️  Build WARNING: Low quality score ({quality_report['scores']['overall']}%)")
        # Could fail build here if quality gates enabled
    
    print("✅ Build PASSED: All checks completed")
    return 0

if __name__ == "__main__":
    # Run examples
    if len(sys.argv) > 1:
        # CI/CD mode - analyze provided path
        ci_cd_integration_example()
    else:
        # Demo mode - show examples
        print("TuoKit Scanner v2.0 - Automation Examples")
        print("=" * 40)
        print("\nUsage examples:")
        print("1. Security scan: automated_security_scan('/path/to/project')")
        print("2. Dependency check: check_dependencies('/path/to/project')")
        print("3. Quality report: generate_quality_report('/path/to/project')")
        print("\nFor CI/CD integration:")
        print("  python scanner_examples.py /path/to/project")
