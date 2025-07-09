# WCR (What's On Crash Report) Analysis

## File Structure Analysis

### 1. Header Section (Lines 1-30)
```
==2024/11/25==9:42:27==BEGIN RUNTIME DIAGNOSTIC DUMP

System Error Report
Whats'On User: WONSERVICE
Whats'On Site: MTVNL(WONP11)
Whats'On Version: 2023r10.000.059e
...
Oracle Server Version: Oracle Database 19c
```

**Key Information:**
- Timestamp
- User and site information
- Software version details
- Database configuration
- System environment variables

### 2. Critical Error Information (Line ~59)
```
Cause of Dump: Message not understood: #productInContractFor:
```
or
```
Cause of Dump: Emergency: No Space Left.
```

**This is the MOST IMPORTANT line** - it clearly states the error cause.

### 3. Stack Trace Section (Lines 60+)
```
Active Process
Process named: 'Unnamed Process'
Process priority: 30
Context stack
[1] UndefinedObject(Object)>>doesNotUnderstand:
[2] MediaGeniX.CM2UpdateRunLogRecord>>contractProductExistsInBroadcastRightGroup
...
```

**Pattern:** Each stack frame shows:
- [Number] ClassName>>methodName
- The error originates at [1] and propagates up

## Common Error Patterns in WCR Files

### 1. Method Not Found Errors
```
Cause of Dump: Message not understood: #productInContractFor:
[1] UndefinedObject(Object)>>doesNotUnderstand:
```
**Explanation:** A method was called on nil or an object that doesn't implement it
**Solution:** Check for nil values, verify method exists, add defensive coding

### 2. Memory/Space Errors
```
Cause of Dump: Emergency: No Space Left.
class: LowSpaceInterrupt
```
**Explanation:** System ran out of memory or disk space
**Solution:** Increase heap size, check disk space, optimize memory usage

### 3. Database Connection Issues
```
[11] OracleConnection(ExternalDatabaseConnection)>>dbTransactionWhile:
```
**Pattern:** Look for Oracle/Database related classes in stack
**Solution:** Check connection settings, timeout values, network connectivity

## Optimal Crash Analysis Strategy

### 1. Quick Identification (First Pass)
```python
def identify_crash_type(content):
    # 1. Extract "Cause of Dump" line - this is the primary indicator
    cause_match = re.search(r'Cause of Dump:\s*(.+)', content)
    if cause_match:
        cause = cause_match.group(1)
        
    # 2. Look for exception class
    exception_match = re.search(r'class:\s*(\w+)', content)
    
    # 3. Extract first few stack frames
    stack_pattern = r'\[1\]\s+(.+)'
    first_frame = re.search(stack_pattern, content)
```

### 2. Context Extraction
- **Error Location:** Look at frames [1-5] for the immediate error context
- **Business Logic:** Scan for MediaGeniX.* classes to understand what operation failed
- **System State:** Check process names, priorities, and active operations

### 3. Pattern Recognition for WCR
Key patterns to detect:
- `doesNotUnderstand:` → Method not found
- `Emergency:` → Critical system error
- `LowSpaceInterrupt` → Memory issues
- `OracleConnection` → Database problems
- `timeout` → Performance/timeout issues

## Current Crash Analyzer Evaluation

### ✅ Strengths for WCR Analysis
1. **Pattern Matching System** - Can be extended with WCR-specific patterns
2. **Smart Content Extraction** - Already looks for stack traces
3. **Chunking for Large Files** - WCR files can be 1.6MB+

### ❌ Areas Needing Enhancement

1. **WCR-Specific Patterns**
```python
WCR_PATTERNS = {
    "MethodNotUnderstood": {
        "pattern": r"Message not understood:|doesNotUnderstand:",
        "quick_fix": "Check for nil values, verify method exists in class",
        "prevention": "Add nil checks and method existence validation"
    },
    "LowSpaceInterrupt": {
        "pattern": r"Emergency: No Space Left|LowSpaceInterrupt",
        "quick_fix": "Free disk space or increase JVM heap size",
        "prevention": "Monitor disk/memory usage, implement cleanup routines"
    }
}
```

2. **WCR Structure Parser**
```python
def parse_wcr_structure(content):
    sections = {
        'header': extract_header_info(content),
        'cause': extract_cause_of_dump(content),
        'stack_trace': extract_stack_trace(content),
        'process_info': extract_process_info(content),
        'environment': extract_environment(content)
    }
    return sections
```

3. **Smalltalk-Specific Analysis**
- Understanding Smalltalk stack traces (Class>>method format)
- MediaGeniX framework knowledge
- Common Smalltalk error patterns

## Recommended Enhancements

### 1. Add WCR-Specific Prompt Template
```python
WCR_EXPERT_PROMPT = """You are a What's On/VisualWorks Smalltalk expert.

Key areas to analyze:
1. Cause of Dump line - this is the primary error
2. First 5 stack frames - immediate error context  
3. MediaGeniX classes - business logic involved
4. Oracle/Database frames - data access issues
5. Process information - what was system doing

Smalltalk-specific insights:
- doesNotUnderstand: means method not found (usually nil receiver)
- Emergency: indicates critical system state
- Check for collection operations that might fail
"""
```

### 2. Quick WCR Summary Function
```python
def get_wcr_summary(content):
    cause = extract_line_after(content, "Cause of Dump:")
    first_frame = extract_line_after(content, "[1]")
    process_name = extract_line_after(content, "Process named:")
    
    return {
        'error': cause,
        'location': first_frame,
        'process': process_name,
        'type': categorize_wcr_error(cause)
    }
```

### 3. Pattern Library for Common WCR Issues
- Nil receiver errors
- Collection bounds errors  
- Database transaction failures
- Memory/space exhaustion
- Timeout exceptions
- User permission errors

## Conclusion

The current Crash Analyzer is **well-equipped** to handle WCR files with minor enhancements:

1. ✅ Already handles large files (up to 5MB)
2. ✅ Has pattern matching infrastructure
3. ✅ Smart content extraction works for stack traces
4. ⚠️ Needs WCR-specific patterns added
5. ⚠️ Could benefit from Smalltalk-aware parsing
6. ⚠️ Should prioritize "Cause of Dump" line extraction

The analyzer is definitely "up to par" - it just needs some WCR-specific patterns and prompts added to be optimal for these files.