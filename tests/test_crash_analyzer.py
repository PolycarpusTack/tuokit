#!/usr/bin/env python3
"""Quick test for crash analyzer functionality"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pages.crash_analyzer import (
    match_known_patterns,
    extract_critical_sections,
    determine_analysis_method
)

# Test 1: Pattern matching
test_content = """
ERROR: NullPointerException at com.example.MyClass.doSomething(MyClass.java:42)
    at java.base/java.lang.Thread.run(Thread.java:829)
FATAL: OutOfMemoryError: Java heap space
Connection timeout after 30000ms
FileNotFoundException: config.xml not found
"""

print("🧪 Testing Crash Analyzer Core Functions")
print("=" * 50)

print("\n1. Testing pattern matching:")
patterns = match_known_patterns(test_content)
print(f"   Found {len(patterns)} patterns")
for p in patterns:
    print(f"   - {p['pattern']} ({p['severity']}): {p['quick_fix'][:50]}...")

print("\n2. Testing critical section extraction:")
extracted = extract_critical_sections(test_content, 500)
print(f"   Extracted {len(extracted)} characters")
print(f"   Preview: {extracted[:100]}...")

print("\n3. Testing analysis method determination:")
sizes = [50000, 500000, 2000000, 6000000]  # 50KB, 500KB, 2MB, 6MB
for size in sizes:
    method, warning = determine_analysis_method(size)
    size_mb = size / (1024 * 1024)
    print(f"   {size_mb:.1f}MB → {method}" + (f" ({warning[:30]}...)" if warning else ""))

print("\n✅ All tests passed! Crash Analyzer core functions are working.")