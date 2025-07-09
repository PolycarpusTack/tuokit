import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

print("Step 1: Starting test")

try:
    print("Step 2: Importing BPMNGenerator")
    from toolkits.camunda.generator import BPMNGenerator
    
    print("Step 3: Creating generator instance")
    generator = BPMNGenerator()
    
    print("Step 4: Testing simple workflow extraction")
    workflow = generator._extract_simple_workflow("do task one then task two")
    print(f"Workflow tasks: {len(workflow['tasks'])}")
    print(f"Workflow flows: {len(workflow['flows'])}")
    
    print("[SUCCESS] Basic test passed!")
    
except Exception as e:
    import traceback
    print(f"[ERROR] Test failed: {e}")
    traceback.print_exc()
