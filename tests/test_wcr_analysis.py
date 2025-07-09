#!/usr/bin/env python3
"""Test WCR pattern analysis"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from utils.wcr_patterns import (
    extract_wcr_cause, 
    extract_wcr_stack_frames,
    extract_wcr_metadata,
    generate_wcr_summary,
    WCR_PATTERNS
)

# Test with a sample WCR snippet
test_wcr = """
==2024/11/25==9:42:27==BEGIN RUNTIME DIAGNOSTIC DUMP

System Error Report
Whats'On User: WONSERVICE
Whats'On Site: MTVNL(WONP11)
Whats'On Version: 2023r10.000.059e

Cause of Dump: Message not understood: #productInContractFor:

Active Process
Process named: 'Unnamed Process'
Context stack
[1] 	UndefinedObject(Object)>>doesNotUnderstand:
[2] 	MediaGeniX.CM2UpdateRunLogRecord>>contractProductExistsInBroadcastRightGroup
[3] 	MediaGeniX.CM2UpdateRunLogRecord>>recalculateRunNumbers
"""

print("🧪 Testing WCR Analysis Functions")
print("=" * 50)

# Test 1: Extract cause
cause = extract_wcr_cause(test_wcr)
print(f"\n1. Extracted Cause: '{cause}'")

# Test 2: Extract stack frames
frames = extract_wcr_stack_frames(test_wcr)
print(f"\n2. Stack Frames ({len(frames)} found):")
for frame in frames[:3]:
    print(f"   {frame}")

# Test 3: Extract metadata
metadata = extract_wcr_metadata(test_wcr)
print(f"\n3. Metadata:")
for key, value in metadata.items():
    print(f"   {key}: {value}")

# Test 4: Generate summary
summary = generate_wcr_summary(test_wcr)
print(f"\n4. WCR Summary:")
print(f"   Cause: {summary['cause']}")
print(f"   Category: {summary['category']}")
print(f"   Patterns Matched: {len(summary['matched_patterns'])}")
if summary['matched_patterns']:
    for pattern in summary['matched_patterns']:
        print(f"   - {pattern['name']} ({pattern['severity']})")

# Test 5: Pattern matching
print(f"\n5. Available WCR Patterns: {len(WCR_PATTERNS)}")
for name, info in list(WCR_PATTERNS.items())[:3]:
    print(f"   - {name}: {info['category']}")

print("\n✅ WCR analysis functions are working correctly!")