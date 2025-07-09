"""
Agent Management System
Handles creation, storage, and organization of custom agents
"""

from typing import Dict, List, Optional, Any, Tuple
import json
import sqlite3
from pathlib import Path
from datetime import datetime
import shutil
import zipfile
import uuid
from dataclasses import dataclass, asdict

from .core import BaseAgent, AgentType
from .registry import register_agent
from .quick_builder import QuickAgent


@dataclass
class AgentConfig:
    """Standard agent configuration"""
    id: str
    name: str
    version: str = "1.0.0"
    category: str = "custom"  # team|client|function|core
    subcategory: Optional[str] = None
    tools: List[str] = None
    prompts: Dict[str, str] = None
    metadata: Dict[str, Any] = None
    permissions: Dict[str, List[str]] = None
    
    def __post_init__(self):
        if self.tools is None:
            self.tools = []
        if self.prompts is None:
            self.prompts = {}
        if self.metadata is None:
            self.metadata = {}
        if self.permissions is None:
            self.permissions = {"teams": ["*"], "users": ["*"]}
    
    def to_dict(self) -> Dict:
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'AgentConfig':
        return cls(**data)


class AgentManager:
    """Manages agent lifecycle and organization"""
    
    def __init__(self, db_path: str = "tuokit.db", 
                 agent_store_path: str = "agent_store"):
        self.db_path = db_path
        self.agent_store_path = Path(agent_store_path)
        self._init_database()
    
    def _init_database(self):
        """Initialize agent management tables"""
        with sqlite3.connect(self.db_path) as conn:
            # Agent registry
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_registry (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_id TEXT UNIQUE NOT NULL,
                    name TEXT NOT NULL,
                    category TEXT NOT NULL,
                    subcategory TEXT,
                    version TEXT DEFAULT '1.0.0',
                    created_by TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    is_active BOOLEAN DEFAULT TRUE,
                    metadata JSON
                )
            """)
            
            # Agent definitions
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_definitions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_id TEXT NOT NULL,
                    definition JSON NOT NULL,
                    tools JSON,
                    prompts JSON,
                    client_config JSON,
                    FOREIGN KEY (agent_id) REFERENCES agent_registry(agent_id)
                )
            """)
            
            # Usage statistics
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_usage (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_id TEXT NOT NULL,
                    used_by TEXT,
                    used_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    success BOOLEAN,
                    execution_time REAL,
                    context JSON
                )
            """)
            
            # Version history
            conn.execute("""
                CREATE TABLE IF NOT EXISTS agent_versions (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    agent_id TEXT NOT NULL,
                    version TEXT NOT NULL,
                    definition JSON NOT NULL,
                    change_notes TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Create indexes
            conn.execute("CREATE INDEX IF NOT EXISTS idx_agent_category ON agent_registry(category, subcategory)")
            conn.execute("CREATE INDEX IF NOT EXISTS idx_agent_usage ON agent_usage(agent_id, used_at)")
    
    def create_agent(self, config: AgentConfig) -> str:
        """Create a new agent with proper organization"""
        # Validate
        self._validate_config(config)
        
        # Check for conflicts
        if self._agent_exists(config.id):
            raise ValueError(f"Agent {config.id} already exists")
        
        # Determine file location
        file_path = self._get_agent_path(config)
        
        # Create agent class
        agent_code = self._generate_agent_code(config)
        
        # Save to filesystem
        file_path.parent.mkdir(parents=True, exist_ok=True)
        file_path.write_text(agent_code)
        
        # Register in database
        self._register_agent(config)
        
        # Save definition
        definition_path = self.agent_store_path / "definitions" / f"{config.id}.json"
        definition_path.parent.mkdir(parents=True, exist_ok=True)
        with open(definition_path, 'w') as f:
            json.dump(config.to_dict(), f, indent=2)
        
        return config.id
    
    def _validate_config(self, config: AgentConfig):
        """Validate agent configuration"""
        if not config.id:
            raise ValueError("Agent ID required")
        
        if not config.name:
            raise ValueError("Agent name required")
        
        if config.category not in ["team", "client", "function", "core"]:
            raise ValueError(f"Invalid category: {config.category}")
        
        # Validate ID format
        parts = config.id.split('.')
        if len(parts) < 3:
            raise ValueError("Agent ID must follow format: category.subcategory.name_version")
    
    def _agent_exists(self, agent_id: str) -> bool:
        """Check if agent already exists"""
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute(
                "SELECT 1 FROM agent_registry WHERE agent_id = ?",
                (agent_id,)
            ).fetchone()
            return result is not None
    
    def _get_agent_path(self, config: AgentConfig) -> Path:
        """Determine where to save agent file"""
        base = Path("toolkits/agent_hub/custom")
        
        if config.category == "team":
            return base / "by_team" / config.subcategory / f"{config.name.lower().replace(' ', '_')}.py"
        elif config.category == "client":
            return base / "by_client" / config.subcategory / f"{config.name.lower().replace(' ', '_')}.py"
        elif config.category == "function":
            return base / "by_function" / config.subcategory / f"{config.name.lower().replace(' ', '_')}.py"
        else:
            return base / f"{config.name.lower().replace(' ', '_')}.py"
    
    def _generate_agent_code(self, config: AgentConfig) -> str:
        """Generate Python code for agent"""
        return f'''"""
{config.name}
Auto-generated agent for {config.category}/{config.subcategory or 'general'}
Version: {config.version}
"""

from toolkits.agent_hub import BaseAgent, AgentType

class {config.name.replace(" ", "")}(BaseAgent):
    """{config.metadata.get('description', config.name)}"""
    
    def __init__(self):
        super().__init__(
            name="{config.name}",
            description="""{config.metadata.get('description', f'Custom agent: {config.name}')}""",
            tools={config.tools},
            agent_type=AgentType.SPECIALIST
        )
    
    def _initialize_tools(self):
        """Initialize tool implementations"""
        tools = {{}}
        
        # Add tool implementations
        {self._generate_tool_methods(config)}
        
        return tools

# Auto-register agent
from toolkits.agent_hub import register_agent
agent_instance = {config.name.replace(" ", "")}()
register_agent("{config.id}", agent_instance)
'''
    
    def _generate_tool_methods(self, config: AgentConfig) -> str:
        """Generate tool method implementations"""
        methods = []
        
        for tool in config.tools:
            if tool in config.prompts:
                prompt = config.prompts[tool]
                methods.append(f'''
        def _{tool}_impl(params):
            from utils import safe_ollama_generate
            prompt = f"""{prompt}"""
            for key, value in params.items():
                prompt = prompt.replace(f"{{{{{key}}}}}", str(value))
            response = safe_ollama_generate(
                params.get('model', 'deepseek-r1:1.5b'),
                prompt
            )
            return response['response']
        
        tools["{tool}"] = _{tool}_impl''')
        
        return '\n'.join(methods)
    
    def _register_agent(self, config: AgentConfig):
        """Register agent in database"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                INSERT INTO agent_registry 
                (agent_id, name, category, subcategory, version, created_by, metadata)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            """, (
                config.id,
                config.name,
                config.category,
                config.subcategory,
                config.version,
                config.metadata.get('author', 'system'),
                json.dumps(config.metadata)
            ))
            
            conn.execute("""
                INSERT INTO agent_definitions
                (agent_id, definition, tools, prompts)
                VALUES (?, ?, ?, ?)
            """, (
                config.id,
                json.dumps(config.to_dict()),
                json.dumps(config.tools),
                json.dumps(config.prompts)
            ))
    
    def list_agents(self, category: Optional[str] = None, 
                   subcategory: Optional[str] = None,
                   active_only: bool = True) -> List[Dict]:
        """List agents with optional filtering"""
        with sqlite3.connect(self.db_path) as conn:
            query = "SELECT * FROM agent_registry WHERE 1=1"
            params = []
            
            if active_only:
                query += " AND is_active = TRUE"
            
            if category:
                query += " AND category = ?"
                params.append(category)
            
            if subcategory:
                query += " AND subcategory = ?"
                params.append(subcategory)
            
            query += " ORDER BY category, subcategory, name"
            
            cursor = conn.execute(query, params)
            columns = [desc[0] for desc in cursor.description]
            
            agents = []
            for row in cursor.fetchall():
                agent_dict = dict(zip(columns, row))
                if agent_dict.get('metadata'):
                    agent_dict['metadata'] = json.loads(agent_dict['metadata'])
                agents.append(agent_dict)
            
            return agents
    
    def get_agent(self, agent_id: str) -> Optional[AgentConfig]:
        """Load agent configuration"""
        with sqlite3.connect(self.db_path) as conn:
            result = conn.execute("""
                SELECT definition FROM agent_definitions
                WHERE agent_id = ?
            """, (agent_id,)).fetchone()
            
            if result:
                return AgentConfig.from_dict(json.loads(result[0]))
            return None
    
    def update_agent(self, agent_id: str, updates: Dict) -> str:
        """Update existing agent with version bump"""
        # Load current
        current = self.get_agent(agent_id)
        if not current:
            raise ValueError(f"Agent {agent_id} not found")
        
        # Archive current version
        self._archive_version(current)
        
        # Apply updates
        for key, value in updates.items():
            if hasattr(current, key):
                setattr(current, key, value)
        
        # Bump version
        current.version = self._bump_version(current.version)
        current.metadata['updated_at'] = datetime.now().isoformat()
        
        # Regenerate code
        file_path = self._get_agent_path(current)
        agent_code = self._generate_agent_code(current)
        file_path.write_text(agent_code)
        
        # Update database
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                UPDATE agent_registry 
                SET version = ?, updated_at = CURRENT_TIMESTAMP, metadata = ?
                WHERE agent_id = ?
            """, (current.version, json.dumps(current.metadata), agent_id))
            
            conn.execute("""
                UPDATE agent_definitions
                SET definition = ?, tools = ?, prompts = ?
                WHERE agent_id = ?
            """, (
                json.dumps(current.to_dict()),
                json.dumps(current.tools),
                json.dumps(current.prompts),
                agent_id
            ))
        
        return current.version
    
    def _bump_version(self, version: str) -> str:
        """Increment version number"""
        parts = version.split('.')
        parts[-1] = str(int(parts[-1]) + 1)
        return '.'.join(parts)
    
    def _archive_version(self, config: AgentConfig):
        """Archive current version before update"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                INSERT INTO agent_versions
                (agent_id, version, definition, change_notes)
                VALUES (?, ?, ?, ?)
            """, (
                config.id,
                config.version,
                json.dumps(config.to_dict()),
                config.metadata.get('change_notes', 'Auto-archived')
            ))
    
    def clone_agent(self, source_id: str, new_name: str, 
                   modifications: Optional[Dict] = None) -> str:
        """Clone existing agent with modifications"""
        # Load source
        source = self.get_agent(source_id)
        if not source:
            raise ValueError(f"Source agent {source_id} not found")
        
        # Create new config
        new_config = AgentConfig(
            id=self._generate_agent_id(new_name, source.category, source.subcategory),
            name=new_name,
            version="1.0.0",
            category=source.category,
            subcategory=source.subcategory,
            tools=source.tools.copy(),
            prompts=source.prompts.copy(),
            metadata={
                **source.metadata,
                'cloned_from': source_id,
                'created_at': datetime.now().isoformat()
            },
            permissions=source.permissions.copy()
        )
        
        # Apply modifications
        if modifications:
            for key, value in modifications.items():
                if hasattr(new_config, key):
                    setattr(new_config, key, value)
        
        # Create new agent
        return self.create_agent(new_config)
    
    def _generate_agent_id(self, name: str, category: str, 
                          subcategory: Optional[str] = None) -> str:
        """Generate unique agent ID"""
        base_name = name.lower().replace(' ', '_')
        
        if subcategory:
            base_id = f"custom.{category}.{subcategory}.{base_name}_v1"
        else:
            base_id = f"custom.{category}.{base_name}_v1"
        
        # Ensure uniqueness
        counter = 1
        agent_id = base_id
        while self._agent_exists(agent_id):
            counter += 1
            agent_id = base_id.replace('_v1', f'_v{counter}')
        
        return agent_id
    
    def export_agent(self, agent_id: str, include_history: bool = False) -> str:
        """Export agent to shareable package"""
        agent = self.get_agent(agent_id)
        if not agent:
            raise ValueError(f"Agent {agent_id} not found")
        
        # Create export package
        package = {
            "agent": agent.to_dict(),
            "exported_at": datetime.now().isoformat(),
            "format_version": "1.0"
        }
        
        if include_history:
            # Get usage stats
            with sqlite3.connect(self.db_path) as conn:
                usage = conn.execute("""
                    SELECT COUNT(*) as total_uses,
                           AVG(execution_time) as avg_time,
                           SUM(CASE WHEN success THEN 1 ELSE 0 END) * 100.0 / COUNT(*) as success_rate
                    FROM agent_usage
                    WHERE agent_id = ?
                """, (agent_id,)).fetchone()
                
                package["usage_stats"] = {
                    "total_uses": usage[0],
                    "avg_execution_time": usage[1],
                    "success_rate": usage[2]
                }
                
                # Get sample successful runs
                successful_runs = conn.execute("""
                    SELECT context, execution_time
                    FROM agent_usage
                    WHERE agent_id = ? AND success = TRUE
                    ORDER BY used_at DESC
                    LIMIT 5
                """, (agent_id,)).fetchall()
                
                package["example_runs"] = [
                    {"context": json.loads(run[0]) if run[0] else {}, "time": run[1]}
                    for run in successful_runs
                ]
        
        # Create zip file
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        export_filename = f"{agent_id.replace('.', '_')}_{timestamp}.zip"
        export_path = self.agent_store_path / "exports" / export_filename
        export_path.parent.mkdir(parents=True, exist_ok=True)
        
        with zipfile.ZipFile(export_path, 'w') as zf:
            # Add package info
            zf.writestr("agent.json", json.dumps(package, indent=2))
            
            # Add agent code if it exists
            agent_file = self._get_agent_path(agent)
            if agent_file.exists():
                zf.write(agent_file, f"code/{agent_file.name}")
            
            # Add template if it's based on one
            if agent.metadata.get('template'):
                template_file = self.agent_store_path / "templates" / f"{agent.metadata['template']}.json"
                if template_file.exists():
                    zf.write(template_file, f"template/{template_file.name}")
        
        return str(export_path)
    
    def import_agent(self, package_path: str, rename: Optional[str] = None) -> str:
        """Import agent from package"""
        if not Path(package_path).exists():
            raise ValueError(f"Package not found: {package_path}")
        
        with zipfile.ZipFile(package_path, 'r') as zf:
            # Read package info
            package = json.loads(zf.read("agent.json"))
            
            # Extract agent config
            agent_dict = package["agent"]
            config = AgentConfig.from_dict(agent_dict)
            
            # Handle renaming
            if rename:
                config.name = rename
                config.id = self._generate_agent_id(
                    rename, config.category, config.subcategory
                )
            
            # Check for conflicts
            if self._agent_exists(config.id):
                # Create new version instead
                existing = self.get_agent(config.id)
                config.version = self._bump_version(existing.version)
                config.metadata['imported_at'] = datetime.now().isoformat()
                return self.update_agent(config.id, config.to_dict())
            
            # Create new agent
            return self.create_agent(config)
    
    def log_usage(self, agent_id: str, user: str, success: bool, 
                  execution_time: float, context: Optional[Dict] = None):
        """Log agent usage for analytics"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                INSERT INTO agent_usage
                (agent_id, used_by, success, execution_time, context)
                VALUES (?, ?, ?, ?, ?)
            """, (
                agent_id,
                user,
                success,
                execution_time,
                json.dumps(context or {})
            ))
    
    def get_usage_stats(self, agent_id: Optional[str] = None, 
                       days_back: int = 30) -> Dict[str, Any]:
        """Get usage statistics for agents"""
        with sqlite3.connect(self.db_path) as conn:
            cutoff_date = datetime.now().timestamp() - (days_back * 24 * 60 * 60)
            
            if agent_id:
                # Stats for specific agent
                stats = conn.execute("""
                    SELECT 
                        COUNT(*) as total_uses,
                        AVG(execution_time) as avg_time,
                        MIN(execution_time) as min_time,
                        MAX(execution_time) as max_time,
                        SUM(CASE WHEN success THEN 1 ELSE 0 END) * 100.0 / COUNT(*) as success_rate,
                        COUNT(DISTINCT used_by) as unique_users
                    FROM agent_usage
                    WHERE agent_id = ? 
                    AND julianday(used_at) > julianday('now') - ?
                """, (agent_id, days_back)).fetchone()
                
                return {
                    "agent_id": agent_id,
                    "total_uses": stats[0],
                    "avg_execution_time": stats[1],
                    "min_execution_time": stats[2],
                    "max_execution_time": stats[3],
                    "success_rate": stats[4],
                    "unique_users": stats[5]
                }
            else:
                # Overall stats
                popular_agents = conn.execute("""
                    SELECT agent_id, COUNT(*) as uses
                    FROM agent_usage
                    WHERE julianday(used_at) > julianday('now') - ?
                    GROUP BY agent_id
                    ORDER BY uses DESC
                    LIMIT 10
                """, (days_back,)).fetchall()
                
                return {
                    "popular_agents": [
                        {"agent_id": row[0], "uses": row[1]}
                        for row in popular_agents
                    ]
                }
    
    def deprecate_agent(self, agent_id: str, reason: str):
        """Mark agent as deprecated"""
        with sqlite3.connect(self.db_path) as conn:
            conn.execute("""
                UPDATE agent_registry
                SET is_active = FALSE,
                    metadata = json_set(
                        COALESCE(metadata, '{}'),
                        '$.deprecated_at', ?,
                        '$.deprecated_reason', ?
                    )
                WHERE agent_id = ?
            """, (datetime.now().isoformat(), reason, agent_id))
        
        # Move file to archive
        agent = self.get_agent(agent_id)
        if agent:
            source_path = self._get_agent_path(agent)
            if source_path.exists():
                archive_path = Path("archive/agents") / source_path.name
                archive_path.parent.mkdir(parents=True, exist_ok=True)
                shutil.move(str(source_path), str(archive_path))
