"""
Camunda Toolkit - Monitor and manage Camunda workflows with AI
==============================================================

TuoKit module for intelligent Camunda BPM management.

## Core Components

1. **Monitor** (`monitor.py`) - Real-time process monitoring
2. **Analyzer** (`analyzer.py`) - AI-powered workflow analysis  
3. **Generator** (`generator.py`) - BPMN generation from NLP
4. **API Client** (`client.py`) - Camunda REST API wrapper

## Quick Start

```python
from toolkits.camunda import CamundaToolkit

# Initialize with your Camunda instance
toolkit = CamundaToolkit(
    base_url="http://your-camunda:8080/engine-rest",
    username="admin",
    password="admin"
)

# Monitor running processes
processes = toolkit.monitor.get_running_processes()

# Generate BPMN from description
bpmn = toolkit.generator.from_text(
    "When order received, check inventory. "
    "If available, process payment then ship. "
    "Otherwise, notify customer."
)
```
"""