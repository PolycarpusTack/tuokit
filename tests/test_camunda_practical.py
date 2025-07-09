"""
Test suite for Camunda Practical Toolkit
Tests what actually matters and can actually run
"""

import pytest
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from toolkits.camunda.manager_v2 import (
    CamundaManagerV2,
    CamundaToolError,
    CamundaConnectionError,
    CamundaValidationError,
    SimpleProcess,
    SimpleIncident,
    create_service_task,
    create_user_task,
    create_exclusive_gateway
)

class TestCamundaManagerV2:
    """Test the practical Camunda manager"""
    
    def test_initialization(self):
        """Test manager can be created"""
        manager = CamundaManagerV2()
        assert manager.base_url == "http://localhost:8080/engine-rest"
        
        manager2 = CamundaManagerV2("http://custom:8080/engine-rest")
        assert manager2.base_url == "http://custom:8080/engine-rest"
    
    def test_bpmn_validation_valid(self):
        """Test validation of valid BPMN"""
        manager = CamundaManagerV2()
        
        valid_bpmn = """<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL"
                  targetNamespace="http://bpmn.io/schema/bpmn">
  <bpmn:process id="TestProcess" name="Test" isExecutable="true">
    <bpmn:startEvent id="Start">
      <bpmn:outgoing>Flow1</bpmn:outgoing>
    </bpmn:startEvent>
    <bpmn:endEvent id="End">
      <bpmn:incoming>Flow1</bpmn:incoming>
    </bpmn:endEvent>
    <bpmn:sequenceFlow id="Flow1" sourceRef="Start" targetRef="End"/>
  </bpmn:process>
</bpmn:definitions>"""
        
        is_valid, message = manager.validate_bpmn(valid_bpmn)
        assert is_valid == True
        assert "valid" in message.lower()
    
    def test_bpmn_validation_invalid_xml(self):
        """Test validation of invalid XML"""
        manager = CamundaManagerV2()
        
        invalid_xml = "<this is not valid XML"
        is_valid, message = manager.validate_bpmn(invalid_xml)
        assert is_valid == False
        assert "parsing error" in message.lower()
    
    def test_bpmn_validation_no_process(self):
        """Test validation of BPMN without process"""
        manager = CamundaManagerV2()
        
        no_process = """<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL">
</bpmn:definitions>"""
        
        is_valid, message = manager.validate_bpmn(no_process)
        assert is_valid == False
        assert "no process" in message.lower()
    
    def test_bpmn_validation_no_start_event(self):
        """Test validation of process without start event"""
        manager = CamundaManagerV2()
        
        no_start = """<?xml version="1.0" encoding="UTF-8"?>
<bpmn:definitions xmlns:bpmn="http://www.omg.org/spec/BPMN/20100524/MODEL">
  <bpmn:process id="TestProcess" name="Test" isExecutable="true">
    <bpmn:endEvent id="End"/>
  </bpmn:process>
</bpmn:definitions>"""
        
        is_valid, message = manager.validate_bpmn(no_start)
        assert is_valid == False
        assert "no start event" in message.lower()
    
    def test_template_bpmn_generation(self):
        """Test template BPMN generation"""
        manager = CamundaManagerV2()
        
        template = manager._create_template_bpmn("Test Process")
        assert "<?xml" in template
        assert "Test_Process" in template
        assert "StartEvent_1" in template
        assert "EndEvent_1" in template
        
        # Should be valid
        is_valid, _ = manager.validate_bpmn(template)
        assert is_valid == True
    
    def test_helper_functions(self):
        """Test XML helper functions"""
        # Service task
        service = create_service_task("Task1", "My Task", "my-topic")
        assert 'id="Task1"' in service
        assert 'name="My Task"' in service
        assert 'value="my-topic"' in service
        
        # User task
        user = create_user_task("Task2", "Review", "john")
        assert 'id="Task2"' in user
        assert 'name="Review"' in user
        assert 'camunda:assignee="john"' in user
        
        # User task without assignee
        user2 = create_user_task("Task3", "Approve")
        assert 'id="Task3"' in user2
        assert 'assignee' not in user2
        
        # Gateway
        gateway = create_exclusive_gateway("Gateway1", "Decision?")
        assert 'id="Gateway1"' in gateway
        assert 'name="Decision?"' in gateway
    
    def test_error_hierarchy(self):
        """Test error classes work correctly"""
        base_error = CamundaToolError("Base error")
        assert "check your connection" in base_error.user_friendly_message().lower()
        
        conn_error = CamundaConnectionError("Connection failed")
        assert "cannot connect" in conn_error.user_friendly_message().lower()
        
        val_error = CamundaValidationError("Invalid input")
        assert "invalid input" in val_error.user_friendly_message().lower()
    
    def test_data_classes(self):
        """Test simple data classes"""
        process = SimpleProcess(
            id="process:1",
            name="My Process",
            version=1,
            active_instances=5
        )
        assert process.id == "process:1"
        assert process.active_instances == 5
        
        incident = SimpleIncident(
            id="inc-1",
            process_id="proc-1",
            activity="Task1",
            error_message="Timeout",
            timestamp="2024-01-01"
        )
        assert incident.id == "inc-1"
        assert incident.error_message == "Timeout"


if __name__ == "__main__":
    # Run tests
    pytest.main([__file__, "-v"])