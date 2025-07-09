"""
Test Camunda Toolkit Integration
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from toolkits.camunda import CamundaToolkit, BPMNGenerator


def test_bpmn_generation():
    """Test BPMN generation without requiring Camunda connection"""
    print("Testing BPMN Generation...")
    
    generator = BPMNGenerator()
    
    # Test simple process
    simple_description = "Check inventory, then ship order, finally send confirmation email"
    bpmn = generator.from_text(simple_description)
    
    assert bpmn is not None
    assert '<process' in bpmn
    assert 'isExecutable="true"' in bpmn
    print("[OK] Simple process generation: PASSED")
    
    # Test complex process with conditions
    complex_description = """
    When order is received, validate customer information.
    If customer is premium, apply 10% discount.
    Check product availability.
    If product is available, process payment.
    If payment succeeds, ship order and send confirmation.
    Otherwise, cancel order and notify customer.
    """
    
    complex_bpmn = generator.from_text(complex_description, "Order Processing")
    
    assert complex_bpmn is not None
    assert 'Order Processing' in complex_bpmn
    assert '<exclusiveGateway' in complex_bpmn or '<gateway' in complex_bpmn.lower()
    print("[OK] Complex process generation: PASSED")
    
    # Test workflow extraction
    workflow = generator._extract_simple_workflow(simple_description)
    assert len(workflow['tasks']) > 0
    assert len(workflow['flows']) > 0
    print("[OK] Workflow extraction: PASSED")
    
    return True


def test_client_initialization():
    """Test client can be initialized (no connection required)"""
    print("\nTesting Client Initialization...")
    
    from toolkits.camunda.client import CamundaClient
    
    client = CamundaClient("http://localhost:8080/engine-rest")
    assert client.base_url == "http://localhost:8080/engine-rest"
    assert client.session is not None
    print("[OK] Client initialization: PASSED")
    
    # Test variable formatting
    variables = {
        'amount': 100,
        'approved': True,
        'rate': 3.14,
        'name': 'Test'
    }
    
    formatted = client._format_variables(variables)
    assert formatted['amount']['type'] == 'Integer'
    assert formatted['approved']['type'] == 'Boolean'
    assert formatted['rate']['type'] == 'Double'
    assert formatted['name']['type'] == 'String'
    print("[OK] Variable formatting: PASSED")
    
    return True


def test_monitor_initialization():
    """Test monitor can be initialized"""
    print("\nTesting Monitor Initialization...")
    
    from toolkits.camunda.client import CamundaClient
    from toolkits.camunda.monitor import ProcessMonitor
    
    client = CamundaClient("http://localhost:8080/engine-rest")
    monitor = ProcessMonitor(client)
    
    assert monitor.client is not None
    assert monitor.alert_thresholds['incident_count'] == 3
    print("[OK] Monitor initialization: PASSED")
    
    return True


def test_analyzer_initialization():
    """Test analyzer can be initialized"""
    print("\nTesting Analyzer Initialization...")
    
    from toolkits.camunda.client import CamundaClient
    from toolkits.camunda.analyzer import WorkflowAnalyzer
    
    client = CamundaClient("http://localhost:8080/engine-rest")
    analyzer = WorkflowAnalyzer(client)
    
    assert analyzer.client is not None
    assert isinstance(analyzer.analysis_cache, dict)
    print("[OK] Analyzer initialization: PASSED")
    
    return True


def run_all_tests():
    """Run all tests"""
    print("Running Camunda Toolkit Tests\n")
    
    tests = [
        test_bpmn_generation,
        test_client_initialization,
        test_monitor_initialization,
        test_analyzer_initialization
    ]
    
    passed = 0
    failed = 0
    
    for test in tests:
        try:
            if test():
                passed += 1
        except Exception as e:
            print(f"[FAIL] {test.__name__}: FAILED - {str(e)}")
            failed += 1
    
    print(f"\nTest Results: {passed} passed, {failed} failed")
    
    if failed == 0:
        print("[SUCCESS] All tests passed!")
        return True
    else:
        print("[ERROR] Some tests failed")
        return False


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
