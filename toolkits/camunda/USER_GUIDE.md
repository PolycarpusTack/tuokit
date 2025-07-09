# 🏭 Camunda Toolkit for TuoKit

## Overview

The Camunda Toolkit adds AI-powered management capabilities for external Camunda BPM instances to TuoKit. It does NOT integrate Camunda into TuoKit, but rather provides intelligent tools to make working with your existing Camunda deployments easier.

## Features

### 1. 📊 **Process Monitoring**
- Real-time dashboard with health scores
- Stuck process detection with AI recommendations
- Incident tracking and analysis
- Performance metrics visualization

### 2. 🤖 **BPMN Generation** 
- Convert natural language to BPMN XML
- AI understands complex business logic
- Direct deployment to Camunda
- Automatic workflow optimization

### 3. 🔍 **Process Analysis**
- AI-powered bottleneck detection
- Performance optimization recommendations
- Complexity scoring
- Historical trend analysis

### 4. 🚨 **Incident Management**
- Intelligent troubleshooting assistance
- Root cause analysis
- Automated resolution suggestions
- Pattern recognition across incidents

## Installation

The Camunda toolkit is included with TuoKit. No additional installation needed!

## Configuration

### 1. Access the Camunda Manager

Navigate to: **TuoKit → Tools → Camunda Manager** 

### 2. Connect to Your Camunda Instance

```
URL: http://your-camunda-server:8080/engine-rest
Username: your-username (optional)
Password: your-password (optional)
```

## Usage Examples

### Generate BPMN from Requirements

```python
from toolkits.camunda import CamundaToolkit

toolkit = CamundaToolkit("http://localhost:8080/engine-rest")

# Describe your process in plain English
description = """
When a loan application is submitted, validate customer information.
If credit score is above 700, auto-approve loans under $50,000.
Otherwise, send to manual review. 
After approval, generate loan documents and send to customer.
"""

# Generate BPMN
bpmn_xml = toolkit.generator.from_text(description, "Loan Application Process")

# Deploy directly to Camunda
result = toolkit.client.deploy_bpmn("Loan Process v1", bpmn_xml)
```

### Monitor Running Processes

```python
# Get monitoring dashboard
dashboard = toolkit.monitor.get_dashboard_data()

# Find stuck processes
stuck_processes = toolkit.monitor.find_stuck_processes()
for process in stuck_processes:
    print(f"Process {process['process']['id']} stuck at {process['stuck_activity']}")
    print(f"AI recommendation: {process['recommendation']}")
```

### Analyze Process Performance

```python
# Analyze a deployed process
analysis = toolkit.analyzer.analyze_process("loan-application")

print(f"Optimization Score: {analysis['optimization_score']}/100")
print(f"Success Rate: {analysis['performance']['success_rate']}%")

# Get AI recommendations
for recommendation in analysis['recommendations']:
    print(f"- {recommendation}")
```

## Use Cases

### 1. **Process Design Acceleration**
- Business analysts describe processes in plain English
- AI generates compliant BPMN automatically
- Reduces design time from hours to minutes

### 2. **Production Monitoring**
- DevOps teams monitor all Camunda instances from one place
- Proactive alerts for stuck processes
- AI-powered incident resolution

### 3. **Process Optimization**
- Identify bottlenecks across workflows
- Get specific optimization recommendations
- Track improvements over time

### 4. **Rapid Prototyping**
- Developers quickly test workflow ideas
- Generate and deploy processes on the fly
- Iterate based on AI feedback

## Architecture

```
Your Application
       ↓
   TuoKit UI (Streamlit)
       ↓
   Camunda Toolkit
       ↓
   REST API Calls
       ↓
Your Camunda Instance(s)
```

## Best Practices

1. **Start Simple**: Begin with basic process descriptions and refine
2. **Review Generated BPMN**: Always review AI-generated workflows before production
3. **Monitor Regularly**: Set up dashboards for critical processes
4. **Act on Recommendations**: Implement AI suggestions incrementally
5. **Capture Knowledge**: All interactions are saved to TuoKit's knowledge base

## Troubleshooting

### Connection Issues
- Verify Camunda is running and accessible
- Check firewall rules for port 8080
- Ensure REST API is enabled in Camunda

### Generation Issues
- Be specific in process descriptions
- Use clear action words (validate, approve, check, send)
- Include decision criteria explicitly

### Performance Issues
- For large deployments, limit dashboard queries
- Use process key filters when analyzing
- Cache analysis results for repeated access

## Future Enhancements

- [ ] Multi-instance Camunda support
- [ ] Process version comparison
- [ ] Automated testing generation
- [ ] SLA monitoring and alerts
- [ ] Integration with Camunda Optimize

## Support

For issues or feature requests, check:
- TuoKit documentation
- Camunda REST API docs
- Create an issue in the TuoKit repository
