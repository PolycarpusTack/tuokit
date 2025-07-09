# 📁 Agent Organization & Creation Workflow

## Directory Structure

```
C:/Projects/Tuokit/
├── toolkits/
│   └── agent_hub/
│       ├── core/              # Core system agents (don't modify)
│       │   ├── __init__.py
│       │   ├── specialists.py
│       │   └── multimodal.py
│       │
│       └── custom/            # User-created agents (organized)
│           ├── __init__.py
│           ├── by_team/       # Team-specific agents
│           │   ├── support/
│           │   │   ├── __init__.py
│           │   │   ├── ticket_resolver_v2.py
│           │   │   └── escalation_handler.py
│           │   ├── legal/
│           │   │   ├── __init__.py
│           │   │   ├── contract_reviewer.py
│           │   │   └── compliance_monitor.py
│           │   ├── dev/
│           │   │   └── stream_debugger.py
│           │   └── sales/
│           │       └── demo_builder.py
│           │
│           ├── by_client/     # Client-specific agents
│           │   ├── espn/
│           │   │   ├── __init__.py
│           │   │   ├── espn_monitor.py
│           │   │   └── mnf_specialist.py
│           │   ├── nbc/
│           │   │   └── affiliate_coordinator.py
│           │   └── cbs/
│           │       └── nfl_handler.py
│           │
│           └── by_function/   # Function-specific agents
│               ├── monitoring/
│               │   └── stream_health.py
│               ├── analysis/
│               │   └── performance_analyzer.py
│               └── automation/
│                   └── incident_responder.py
│
├── agent_store/               # Agent definitions & metadata
│   ├── definitions/           # JSON/YAML agent definitions
│   │   ├── espn_monitor.json
│   │   └── support_ticket_v2.yaml
│   │
│   ├── templates/            # Agent templates
│   │   ├── client_monitor.template.json
│   │   └── issue_resolver.template.json
│   │
│   └── exports/              # Shareable agent packages
│       ├── espn_suite_v1.2.zip
│       └── support_tools_2024Q4.zip
```

## Database Schema

```sql
-- Agent Registry Table
CREATE TABLE agent_registry (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_id TEXT UNIQUE NOT NULL,      -- e.g., "custom.espn.monitor_v2"
    name TEXT NOT NULL,                 -- Display name
    category TEXT NOT NULL,             -- team|client|function|core
    subcategory TEXT,                   -- espn|nbc|support|legal
    version TEXT DEFAULT '1.0.0',
    created_by TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    is_active BOOLEAN DEFAULT TRUE,
    metadata JSON                       -- Additional config
);

-- Agent Definitions Table
CREATE TABLE agent_definitions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_id TEXT NOT NULL,
    definition JSON NOT NULL,           -- Full agent configuration
    tools JSON,                         -- List of tools
    prompts JSON,                       -- Custom prompts
    client_config JSON,                 -- Client-specific settings
    FOREIGN KEY (agent_id) REFERENCES agent_registry(agent_id)
);

-- Agent Usage Stats
CREATE TABLE agent_usage (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_id TEXT NOT NULL,
    used_by TEXT,
    used_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    success BOOLEAN,
    execution_time REAL,
    context JSON,
    FOREIGN KEY (agent_id) REFERENCES agent_registry(agent_id)
);

-- Agent Versions (for updates)
CREATE TABLE agent_versions (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    agent_id TEXT NOT NULL,
    version TEXT NOT NULL,
    definition JSON NOT NULL,
    change_notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (agent_id) REFERENCES agent_registry(agent_id)
);
```

## Agent Creation Workflow

### 1. **Interactive Agent Builder**
```python
class AgentBuilder:
    """Guided agent creation workflow"""
    
    def create_agent_interactive(self):
        # Step 1: Basic Info
        name = input("Agent name: ")
        category = select_from(["team", "client", "function"])
        
        # Step 2: Purpose
        purpose = select_from([
            "Error Resolution",
            "Monitoring",
            "Analysis", 
            "Automation",
            "Client Support"
        ])
        
        # Step 3: Tools needed
        tools = multi_select_from(available_tools)
        
        # Step 4: Client/Team specific?
        if category == "client":
            client = select_from(["ESPN", "NBC", "CBS", "Custom"])
        elif category == "team":
            team = select_from(["support", "legal", "dev", "sales"])
        
        # Step 5: Generate agent
        agent_config = self.generate_config(...)
        
        # Step 6: Test agent
        test_result = self.test_agent(agent_config)
        
        # Step 7: Save agent
        if test_result.success:
            self.save_agent(agent_config)
```

### 2. **Template-Based Creation**
```python
# Start from template
def create_from_template(template_name: str, customizations: dict):
    template = load_template(f"agent_store/templates/{template_name}.json")
    
    # Customize
    agent_config = template.copy()
    agent_config.update(customizations)
    
    # Validate
    validate_agent_config(agent_config)
    
    # Save
    return save_agent(agent_config)
```

### 3. **Clone & Modify**
```python
# Clone existing agent
def clone_agent(source_agent_id: str, new_name: str):
    source = load_agent(source_agent_id)
    
    new_agent = source.copy()
    new_agent['id'] = generate_agent_id(new_name)
    new_agent['name'] = new_name
    new_agent['version'] = "1.0.0"
    
    return save_agent(new_agent)
```

## Storage Best Practices

### 1. **Naming Convention**
```
Format: {category}.{subcategory}.{name}_v{version}

Examples:
- core.specialists.code_v1          # System agent
- custom.team.support.ticket_v2     # Team agent
- custom.client.espn.monitor_v3     # Client agent
- custom.function.monitoring.health_v1  # Function agent
```

### 2. **Agent Configuration Format**
```json
{
  "id": "custom.client.espn.monitor_v2",
  "name": "ESPN Stream Monitor",
  "version": "2.1.0",
  "category": "client",
  "subcategory": "espn",
  "metadata": {
    "author": "john.doe",
    "created": "2024-01-15",
    "team": "support",
    "tags": ["monitoring", "streaming", "espn"]
  },
  "config": {
    "tools": ["check_stream", "analyze_quality", "alert_issues"],
    "prompts": {
      "check_stream": "Monitor ESPN {stream_type} at {url}",
      "analyze_quality": "Check {metric} for ESPN broadcast"
    },
    "client_specific": {
      "protocols": ["RTMP", "HLS"],
      "check_interval": "5min",
      "alert_threshold": "3_failures"
    }
  },
  "permissions": {
    "teams": ["support", "ops"],
    "users": ["*"]
  }
}
```

### 3. **Agent Lifecycle**
```python
class AgentLifecycle:
    """Manage agent creation, updates, deprecation"""
    
    def create(self, config: dict) -> str:
        # Validate
        self.validate_config(config)
        
        # Check naming conflicts
        if self.exists(config['id']):
            raise ValueError("Agent ID already exists")
        
        # Save to appropriate location
        location = self.determine_location(config)
        self.save_to_filesystem(location, config)
        self.register_in_database(config)
        
        return config['id']
    
    def update(self, agent_id: str, updates: dict) -> str:
        # Load current
        current = self.load_agent(agent_id)
        
        # Version bump
        new_version = self.bump_version(current['version'])
        
        # Save old version
        self.archive_version(agent_id, current)
        
        # Apply updates
        current.update(updates)
        current['version'] = new_version
        
        # Save
        self.save_agent(current)
        
        return new_version
    
    def deprecate(self, agent_id: str, reason: str):
        # Mark as inactive
        self.db.execute("""
            UPDATE agent_registry 
            SET is_active = FALSE,
                metadata = json_set(metadata, '$.deprecated_reason', ?)
            WHERE agent_id = ?
        """, (reason, agent_id))
        
        # Move file to archive
        self.move_to_archive(agent_id)
```

## Organization Strategies

### 1. **Team-Based Organization**
Best when agents are team-specific:
```
custom/by_team/
├── support/         # All support team agents
├── legal/          # Legal team agents
└── dev/            # Dev team agents
```

### 2. **Client-Based Organization**
Best when you have client-specific workflows:
```
custom/by_client/
├── espn/           # All ESPN-related agents
├── nbc/            # NBC agents
└── cbs/            # CBS agents
```

### 3. **Function-Based Organization**
Best for cross-team utilities:
```
custom/by_function/
├── monitoring/     # All monitoring agents
├── analysis/       # Analysis agents
└── automation/     # Automation agents
```

### 4. **Hybrid Approach** (Recommended)
```python
# Primary organization by team
# Secondary tags for client/function

agent_config = {
    "category": "team",
    "subcategory": "support",
    "tags": ["espn", "monitoring", "rtmp"]
}

# Can query by any dimension
find_agents(team="support")
find_agents(client="espn")
find_agents(function="monitoring")
```

## Import/Export Workflow

### 1. **Export Agent**
```python
def export_agent(agent_id: str, include_history: bool = False):
    package = {
        "agent": load_agent_definition(agent_id),
        "metadata": load_agent_metadata(agent_id),
        "version": get_agent_version(agent_id)
    }
    
    if include_history:
        package["usage_stats"] = get_usage_stats(agent_id)
        package["success_examples"] = get_successful_runs(agent_id)
    
    # Create zip
    filename = f"exports/{agent_id}_{timestamp}.zip"
    create_agent_package(package, filename)
    
    return filename
```

### 2. **Import Agent**
```python
def import_agent(package_path: str, rename: Optional[str] = None):
    # Extract and validate
    package = extract_agent_package(package_path)
    validate_agent_package(package)
    
    # Check conflicts
    agent_id = package['agent']['id']
    if rename:
        agent_id = generate_new_id(rename)
        package['agent']['id'] = agent_id
    
    # Import
    if agent_exists(agent_id):
        if confirm("Agent exists. Create new version?"):
            import_as_new_version(package)
        else:
            return None
    
    # Save
    save_imported_agent(package)
    return agent_id
```

## Best Practices

### 1. **Keep Agents Focused**
```python
# Good: Single purpose
espn_rtmp_monitor = Agent(
    name="ESPN RTMP Monitor",
    tools=["check_rtmp_status", "analyze_rtmp_errors"]
)

# Bad: Kitchen sink
super_agent = Agent(
    name="Do Everything",
    tools=["monitor", "analyze", "fix", "report", "email", ...]
)
```

### 2. **Version Meaningfully**
```
1.0.0 → 1.0.1  # Bug fix
1.0.0 → 1.1.0  # New feature
1.0.0 → 2.0.0  # Breaking change
```

### 3. **Document Changes**
```json
{
  "version": "2.1.0",
  "changes": [
    "Added NBC affiliate support",
    "Fixed RTMP timeout handling",
    "Improved error messages"
  ]
}
```

### 4. **Test Before Saving**
```python
# Always test new agents
test_results = test_agent(new_agent, test_cases=[
    {"input": "ESPN stream down", "expected": "check_rtmp"},
    {"input": "NBC schedule gap", "expected": "check_affiliates"}
])

if test_results.passed:
    save_agent(new_agent)
```

## UI Integration

### Agent Management Interface
```python
def render_agent_manager(st):
    tab1, tab2, tab3, tab4 = st.tabs([
        "Browse Agents",
        "Create New",
        "Import/Export", 
        "Usage Stats"
    ])
    
    with tab1:
        # Filter and browse
        category = st.selectbox("Category", ["All", "Team", "Client", "Function"])
        agents = list_agents(category=category)
        
        # Display grid
        for agent in agents:
            with st.expander(agent['name']):
                st.write(f"Version: {agent['version']}")
                st.write(f"Usage: {agent['usage_count']} times")
                if st.button("Edit", key=agent['id']):
                    edit_agent(agent['id'])
```

This structure ensures:
- **No clutter** - Everything has its place
- **Easy discovery** - Multiple organization methods
- **Version control** - Track changes over time
- **Team sharing** - Import/export workflows
- **Performance** - Database indexes on key fields

Want me to implement any specific part of this structure?
