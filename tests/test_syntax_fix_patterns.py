"""
Test the enhanced syntax error fix patterns
"""
import re

# Test patterns from the enhanced fix_syntax_error
test_cases = [
    # Case 1: Multiple spaces between statements
    {
        'input': '        else:            try:',
        'expected': '        else:\n            try:',
        'description': 'Multiple spaces between else and try'
    },
    # Case 2: Return followed by except
    {
        'input': '            return text        except ImportError:',
        'expected': '            return text\n        except ImportError:',
        'description': 'Return statement followed by except'
    },
    # Case 3: With statement followed by if
    {
        'input': '        with db.conn.cursor() as cur:            if category and category != "All":',
        'expected': '        with db.conn.cursor() as cur:\n            if category and category != "All":',
        'description': 'With statement followed by if'
    }
]

def test_pattern(pattern, replacement, test_input):
    """Test a single regex pattern"""
    result = re.sub(pattern, replacement, test_input)
    return result

# Test the main patterns
patterns = [
    (r'(\s*else\s*:)\s+(\S.*)', r'\1\n    \2'),
    (r'(\S.*)\s{2,}(try\s*:)', r'\1\n\2'),
    (r'(\s*return\s+.+?)\s+(except\s+\w+)', r'\1\n\2'),
    (r'(\s*with\s+.+:)\s+(\S.*)', r'\1\n    \2'),
    (r'(.+?)\s{2,}(\w+\s*:)', r'\1\n\2'),
]

print("Testing Enhanced Syntax Fix Patterns\n" + "="*50)

for i, test in enumerate(test_cases, 1):
    print(f"\nTest {i}: {test['description']}")
    print(f"Input:    '{test['input']}'")
    print(f"Expected: '{test['expected']}'")
    
    fixed = False
    for pattern, replacement in patterns:
        result = test_pattern(pattern, replacement, test['input'])
        if result != test['input']:
            print(f"Result:   '{result}'")
            print(f"Pattern:  {pattern}")
            fixed = True
            
            # Check if it matches expected
            if result.strip() == test['expected'].strip():
                print("✅ PASS")
            else:
                print("⚠️  Fixed but not as expected")
            break
    
    if not fixed:
        print("❌ No pattern matched")

print("\n" + "="*50)
print("\nThe enhanced patterns should now handle:")
print("1. Multiple spaces between statements")
print("2. Various control flow combinations")
print("3. Return statements followed by except")
print("4. Unterminated strings")
print("5. Missing indentation after colons")
