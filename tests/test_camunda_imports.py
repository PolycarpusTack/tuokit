import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

print("Testing imports...")

try:
    from toolkits.camunda import BPMNGenerator
    print("[OK] BPMNGenerator imported")
except Exception as e:
    print(f"[FAIL] BPMNGenerator import: {e}")

try:
    from toolkits.camunda.client import CamundaClient
    print("[OK] CamundaClient imported")
except Exception as e:
    print(f"[FAIL] CamundaClient import: {e}")

print("Import test complete")
