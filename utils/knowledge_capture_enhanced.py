"""
Enhanced Knowledge Capture for Agent Hub
Comprehensive integration with all agent operations
"""

from typing import Dict, List, Optional, Any, Union
from datetime import datetime
import json
import hashlib
from dataclasses import dataclass, field

from utils.database import DatabaseManager
from utils.knowledge import KnowledgeExtractor, KnowledgePattern, standardize_knowledge_entry

@dataclass
class AgentKnowledgeEntry:
    """Enhanced knowledge entry for agent operations"""
    agent_name: str
    tool_name: str
    operation_type: str  # plan, execute, analyze, etc.
    category: str
    subcategory: str
    title: str
    content: str
    input_data: Dict = field(default_factory=dict)
    output_data: Dict = field(default_factory=dict)
    metadata: Dict = field(default_factory=dict)
    tags: List[str] = field(default_factory=list)
    performance_metrics: Dict = field(default_factory=dict)
    success: bool = True
    error_message: Optional[str] = None
    confidence_score: float = 1.0
    reusability_score: float = 1.0
    timestamp: datetime = field(default_factory=datetime.now)

class EnhancedKnowledgeCapture:
    """
    Enhanced knowledge capture system for the Agent Hub
    Captures all agent operations and insights
    """
    
    # Knowledge categories specific to agents
    AGENT_CATEGORIES = {
        "architecture": ["system_design", "component_design", "patterns", "decisions"],
        "research": ["findings", "summaries", "sources", "insights"],
        "analytics": ["queries", "patterns", "insights", "visualizations"],
        "code": ["generation", "review", "optimization", "testing"],
        "sql": ["queries", "optimizations", "schemas", "performance"],
        "error": ["diagnosis", "solutions", "patterns", "prevention"],
        "pipeline": ["workflows", "orchestration", "dependencies", "optimization"],
        "learning": ["tutorials", "examples", "best_practices", "mistakes"]
    }
    
    def __init__(self):
        self.db = DatabaseManager()
        self.extractor = KnowledgeExtractor()
        self._init_knowledge_tables()
    
    def _init_knowledge_tables(self):
        """Initialize enhanced knowledge tables if needed"""
        if self.db and self.db.connected:
            try:
                # Enhanced knowledge entries table
                with self.db.get_connection() as conn:
                    with conn.cursor() as cur:
                        cur.execute("""
                            CREATE TABLE IF NOT EXISTS agent_knowledge_entries (
                                id SERIAL PRIMARY KEY,
                                agent_name VARCHAR(100),
                                tool_name VARCHAR(100),
                                operation_type VARCHAR(50),
                                category VARCHAR(50),
                                subcategory VARCHAR(50),
                                title TEXT,
                                content TEXT,
                                input_data JSONB,
                                output_data JSONB,
                                metadata JSONB,
                                tags TEXT[],
                                performance_metrics JSONB,
                                success BOOLEAN DEFAULT TRUE,
                                error_message TEXT,
                                confidence_score FLOAT DEFAULT 1.0,
                                reusability_score FLOAT DEFAULT 1.0,
                                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                            )
                        """)
                        
                        # Create indexes
                        cur.execute("CREATE INDEX IF NOT EXISTS idx_agent_tool ON agent_knowledge_entries(agent_name, tool_name)")
                        cur.execute("CREATE INDEX IF NOT EXISTS idx_category ON agent_knowledge_entries(category, subcategory)")
                        cur.execute("CREATE INDEX IF NOT EXISTS idx_created ON agent_knowledge_entries(created_at DESC)")
                        
                        # Knowledge relationships table
                        cur.execute("""
                            CREATE TABLE IF NOT EXISTS knowledge_relationships (
                                id SERIAL PRIMARY KEY,
                                source_id INTEGER REFERENCES agent_knowledge_entries(id),
                                target_id INTEGER REFERENCES agent_knowledge_entries(id),
                                relationship_type VARCHAR(50),
                                strength FLOAT DEFAULT 1.0,
                                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                            )
                        """)
                        
                        # Knowledge usage tracking
                        cur.execute("""
                            CREATE TABLE IF NOT EXISTS knowledge_usage (
                                id SERIAL PRIMARY KEY,
                                knowledge_id INTEGER REFERENCES agent_knowledge_entries(id),
                                used_by_agent VARCHAR(100),
                                used_in_context TEXT,
                                usefulness_rating INTEGER,
                                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                            )
                        """)
            except Exception as e:
                print(f"Note: Enhanced knowledge tables may already exist: {e}")
    
    def capture_agent_operation(self, entry: AgentKnowledgeEntry) -> Optional[int]:
        """
        Capture a complete agent operation with all context
        Returns the knowledge entry ID if successful
        """
        if not self.db or not self.db.connected:
            return None
        
        try:
            # Calculate content hash for deduplication
            content_hash = self._calculate_content_hash(entry)
            
            # Check if similar knowledge exists
            existing = self._find_similar_knowledge(content_hash)
            if existing:
                # Update usage count instead of duplicating
                self._increment_usage(existing['id'], entry.agent_name)
                return existing['id']
            
            # Insert new knowledge entry
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO agent_knowledge_entries (
                            agent_name, tool_name, operation_type, category, subcategory,
                            title, content, input_data, output_data, metadata, tags,
                            performance_metrics, success, error_message, 
                            confidence_score, reusability_score
                        ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                        RETURNING id
                    """, (
                        entry.agent_name, entry.tool_name, entry.operation_type,
                        entry.category, entry.subcategory, entry.title, entry.content,
                        json.dumps(entry.input_data), json.dumps(entry.output_data),
                        json.dumps(entry.metadata), entry.tags,
                        json.dumps(entry.performance_metrics), entry.success,
                        entry.error_message, entry.confidence_score, entry.reusability_score
                    ))
                    
                    knowledge_id = cur.fetchone()[0]
            
            # Extract and link patterns
            patterns = self.extractor.extract_patterns(
                entry.tool_name, 
                str(entry.input_data), 
                entry.content
            )
            
            for pattern in patterns:
                self._link_pattern_to_knowledge(knowledge_id, pattern)
            
            # Auto-link related knowledge
            self._auto_link_related_knowledge(knowledge_id, entry)
            
            return knowledge_id
            
        except Exception as e:
            print(f"Error capturing agent knowledge: {e}")
            # No need to rollback - connection context manager handles it
            return None
    
    def capture_agent_plan(self, agent_name: str, goal: str, steps: List[Dict]) -> Optional[int]:
        """Capture an agent's execution plan"""
        entry = AgentKnowledgeEntry(
            agent_name=agent_name,
            tool_name="planner",
            operation_type="plan",
            category="pipeline",
            subcategory="orchestration",
            title=f"Execution plan for: {goal[:100]}",
            content=json.dumps(steps, indent=2),
            input_data={"goal": goal},
            output_data={"steps": steps, "step_count": len(steps)},
            metadata={"goal_length": len(goal), "complexity": len(steps)},
            tags=["plan", "orchestration", agent_name.lower()]
        )
        return self.capture_agent_operation(entry)
    
    def capture_tool_execution(self, agent_name: str, tool: str, params: Dict, 
                             result: Dict, execution_time: float = 0) -> Optional[int]:
        """Capture a tool execution result"""
        entry = AgentKnowledgeEntry(
            agent_name=agent_name,
            tool_name=tool,
            operation_type="execute",
            category=self._determine_category(tool),
            subcategory=self._determine_subcategory(tool, params),
            title=f"{tool} execution: {self._generate_title(tool, params)}",
            content=str(result.get('result', result)),
            input_data=params,
            output_data=result,
            metadata={"execution_time_ms": execution_time * 1000},
            tags=[tool, agent_name.lower(), "execution"],
            success=result.get('success', True),
            error_message=result.get('error'),
            performance_metrics={"execution_time": execution_time}
        )
        return self.capture_agent_operation(entry)
    
    def capture_analysis_result(self, agent_name: str, analysis_type: str,
                              data_info: Dict, results: Dict) -> Optional[int]:
        """Capture data analysis results"""
        entry = AgentKnowledgeEntry(
            agent_name=agent_name,
            tool_name=f"{analysis_type}_analyzer",
            operation_type="analyze",
            category="analytics",
            subcategory=analysis_type,
            title=f"{analysis_type} analysis of {data_info.get('name', 'dataset')}",
            content=json.dumps(results, indent=2),
            input_data=data_info,
            output_data=results,
            metadata={
                "row_count": data_info.get('rows', 0),
                "column_count": data_info.get('columns', 0),
                "analysis_depth": len(results)
            },
            tags=[analysis_type, "analysis", agent_name.lower()],
            confidence_score=results.get('confidence', 0.8)
        )
        return self.capture_agent_operation(entry)
    
    def capture_learning_scenario(self, scenario_name: str, steps: List[Dict],
                                results: List[Dict], learning_outcomes: List[str]) -> Optional[int]:
        """Capture educational scenario execution"""
        entry = AgentKnowledgeEntry(
            agent_name="Educational",
            tool_name="learning_scenario",
            operation_type="teach",
            category="learning",
            subcategory="tutorials",
            title=f"Learning scenario: {scenario_name}",
            content=json.dumps({
                "steps": steps,
                "results": results,
                "outcomes": learning_outcomes
            }, indent=2),
            input_data={"scenario": scenario_name, "steps": steps},
            output_data={"results": results, "outcomes": learning_outcomes},
            metadata={"step_count": len(steps), "success_rate": self._calculate_success_rate(results)},
            tags=["education", "tutorial", scenario_name.lower()],
            reusability_score=0.9  # Learning scenarios are highly reusable
        )
        return self.capture_agent_operation(entry)
    
    def query_knowledge(self, filters: Dict[str, Any], limit: int = 10) -> List[Dict]:
        """
        Query captured knowledge with filters
        
        Filters can include:
        - agent_name: str
        - tool_name: str
        - category: str
        - subcategory: str
        - tags: List[str]
        - success: bool
        - min_confidence: float
        - date_range: Tuple[datetime, datetime]
        """
        if not self.db or not self.db.connected:
            return []
        
        # Build query dynamically
        query = "SELECT * FROM agent_knowledge_entries WHERE 1=1"
        params = []
        
        if 'agent_name' in filters:
            query += " AND agent_name = %s"
            params.append(filters['agent_name'])
        
        if 'category' in filters:
            query += " AND category = %s"
            params.append(filters['category'])
        
        if 'min_confidence' in filters:
            query += " AND confidence_score >= %s"
            params.append(filters['min_confidence'])
        
        if 'tags' in filters and filters['tags']:
            query += " AND tags && %s"  # Array overlap
            params.append(filters['tags'])
        
        query += " ORDER BY created_at DESC, confidence_score DESC LIMIT %s"
        params.append(limit)
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute(query, params)
                    columns = [desc[0] for desc in cur.description]
                    
                    entries = []
                    for row in cur.fetchall():
                        entry = dict(zip(columns, row))
                        # Parse JSON fields
                        for json_field in ['input_data', 'output_data', 'metadata', 'performance_metrics']:
                            if entry.get(json_field):
                                entry[json_field] = json.loads(entry[json_field]) if isinstance(entry[json_field], str) else entry[json_field]
                        entries.append(entry)
                    
                    return entries
            
        except Exception as e:
            print(f"Error querying knowledge: {e}")
            return []
    
    def get_related_knowledge(self, knowledge_id: int, limit: int = 5) -> List[Dict]:
        """Get knowledge entries related to a specific entry"""
        if not self.db or not self.db.connected:
            return []
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT k.*, r.relationship_type, r.strength
                        FROM agent_knowledge_entries k
                        JOIN knowledge_relationships r ON k.id = r.target_id
                        WHERE r.source_id = %s
                        ORDER BY r.strength DESC, k.confidence_score DESC
                        LIMIT %s
                    """, (knowledge_id, limit))
                    
                    # Format results
                    related = []
                    columns = [desc[0] for desc in cur.description]
                    for row in cur.fetchall():
                        related.append(dict(zip(columns, row)))
                    
                    return related
            
        except Exception as e:
            print(f"Error getting related knowledge: {e}")
            return []
    
    # Helper methods
    def _calculate_content_hash(self, entry: AgentKnowledgeEntry) -> str:
        """Calculate hash for content deduplication"""
        content_str = f"{entry.tool_name}:{entry.operation_type}:{entry.content[:500]}"
        return hashlib.md5(content_str.encode()).hexdigest()
    
    def _find_similar_knowledge(self, content_hash: str) -> Optional[Dict]:
        """Find similar existing knowledge"""
        # For now, return None - could implement similarity search
        return None
    
    def _increment_usage(self, knowledge_id: int, used_by: str):
        """Track knowledge usage"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO knowledge_usage (knowledge_id, used_by_agent, used_in_context)
                        VALUES (%s, %s, %s)
                    """, (knowledge_id, used_by, "reuse"))
        except:
            pass
    
    def _link_pattern_to_knowledge(self, knowledge_id: int, pattern: KnowledgePattern):
        """Link extracted pattern to knowledge entry"""
        # Save pattern and create relationship
        pattern_entry = AgentKnowledgeEntry(
            agent_name="PatternExtractor",
            tool_name="pattern_extraction",
            operation_type="extract",
            category=pattern.category,
            subcategory="pattern",
            title=pattern.title,
            content=pattern.content,
            metadata=pattern.metadata,
            tags=pattern.tags
        )
        
        pattern_id = self.capture_agent_operation(pattern_entry)
        
        if pattern_id:
            self._create_relationship(knowledge_id, pattern_id, "extracted_pattern")
    
    def _auto_link_related_knowledge(self, knowledge_id: int, entry: AgentKnowledgeEntry):
        """Automatically link related knowledge entries"""
        # Find entries with similar tags or category
        similar = self.query_knowledge({
            'category': entry.category,
            'tags': entry.tags[:3]  # Use first 3 tags
        }, limit=5)
        
        for similar_entry in similar:
            if similar_entry['id'] != knowledge_id:
                self._create_relationship(
                    knowledge_id, 
                    similar_entry['id'],
                    "similar",
                    strength=0.7
                )
    
    def _create_relationship(self, source_id: int, target_id: int, 
                           relationship_type: str, strength: float = 1.0):
        """Create relationship between knowledge entries"""
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        INSERT INTO knowledge_relationships 
                        (source_id, target_id, relationship_type, strength)
                        VALUES (%s, %s, %s, %s)
                        ON CONFLICT DO NOTHING
                    """, (source_id, target_id, relationship_type, strength))
        except:
            pass
    
    def _determine_category(self, tool: str) -> str:
        """Determine category based on tool name"""
        tool_categories = {
            "code": ["code_generator", "code_reviewer", "code_explainer", "test_generator"],
            "sql": ["sql_generator", "sql_optimizer", "sql_explainer"],
            "analytics": ["data_profiling", "nl_to_sql", "pattern_finder"],
            "architecture": ["system_architect", "risk_assessor", "cost_estimator"],
            "research": ["deep_research", "web_research", "report_generation"],
            "error": ["error_decoder", "debug_assistant"]
        }
        
        for category, tools in tool_categories.items():
            if tool in tools:
                return category
        
        return "general"
    
    def _determine_subcategory(self, tool: str, params: Dict) -> str:
        """Determine subcategory based on tool and parameters"""
        # Tool-specific subcategory logic
        if tool == "code_generator":
            lang = params.get('language', 'general').lower()
            return lang
        elif tool == "sql_generator":
            return params.get('dialect', 'standard').lower()
        elif tool == "data_profiling":
            return params.get('analysis_type', 'general')
        
        return "general"
    
    def _generate_title(self, tool: str, params: Dict) -> str:
        """Generate descriptive title for knowledge entry"""
        if tool == "code_generator":
            return params.get('task', 'Code generation')[:100]
        elif tool == "sql_generator":
            return params.get('query', 'SQL query')[:100]
        elif tool == "error_decoder":
            return params.get('error', 'Error analysis')[:100]
        
        return f"{tool} operation"
    
    def _calculate_success_rate(self, results: List[Dict]) -> float:
        """Calculate success rate from results"""
        if not results:
            return 0.0
        
        successful = sum(1 for r in results if r.get('success', False))
        return successful / len(results)

# Global instance for easy access
knowledge_capture = EnhancedKnowledgeCapture()

# Enhanced capture function that integrates with existing system
def capture_knowledge_enhanced(
    tool: str,
    category: str,
    subcategory: str,
    prompt: str,
    response: str,
    metadata: Dict = None,
    agent_name: str = "Unknown",
    success: bool = True,
    performance_metrics: Dict = None
) -> Optional[int]:
    """
    Enhanced knowledge capture with full context
    Backward compatible with existing capture_knowledge calls
    """
    entry = AgentKnowledgeEntry(
        agent_name=agent_name,
        tool_name=tool,
        operation_type="execute",
        category=category,
        subcategory=subcategory,
        title=f"{tool}: {prompt[:100]}",
        content=response,
        input_data={"prompt": prompt},
        output_data={"response": response},
        metadata=metadata or {},
        tags=[tool, category, subcategory],
        success=success,
        performance_metrics=performance_metrics or {}
    )
    
    # Capture in enhanced system
    knowledge_id = knowledge_capture.capture_agent_operation(entry)
    
    # Also capture in original system for compatibility
    if knowledge_id:
        from utils import capture_knowledge as original_capture
        original_capture(tool, "deepseek-r1:1.5b", prompt, response)
    
    return knowledge_id

# Convenience functions for specific capture scenarios
def capture_agent_execution(agent_name: str, goal: str, state: 'AgentState') -> Optional[int]:
    """Capture complete agent execution with state"""
    entry = AgentKnowledgeEntry(
        agent_name=agent_name,
        tool_name="agent_orchestrator",
        operation_type="complete_execution",
        category="pipeline",
        subcategory="execution",
        title=f"Complete execution: {goal[:100]}",
        content=json.dumps(state.to_dict(), indent=2),
        input_data={"goal": goal},
        output_data=state.results,
        metadata={
            "total_steps": len(state.steps),
            "successful_steps": sum(1 for r in state.results.values() if r.get('success')),
            "execution_phase": state.phase,
            "retry_count": state.attempts
        },
        tags=["execution", "complete", agent_name.lower()],
        success=all(r.get('success', False) for r in state.results.values())
    )
    
    return knowledge_capture.capture_agent_operation(entry)

def capture_pipeline_execution(pipeline_name: str, steps: List[Dict], 
                             results: Dict, execution_time: float) -> Optional[int]:
    """Capture visual pipeline execution"""
    entry = AgentKnowledgeEntry(
        agent_name="PipelineBuilder",
        tool_name="visual_pipeline",
        operation_type="pipeline_execution",
        category="pipeline",
        subcategory="workflows",
        title=f"Pipeline: {pipeline_name}",
        content=json.dumps({
            "steps": steps,
            "results": results
        }, indent=2),
        input_data={"pipeline_name": pipeline_name, "steps": steps},
        output_data=results,
        metadata={"execution_time_ms": execution_time},
        tags=["pipeline", "visual", pipeline_name.lower()],
        success=results.get('success', True),
        performance_metrics={
            "execution_time_ms": execution_time,
            "step_count": len(steps)
        }
    )
    
    return knowledge_capture.capture_agent_operation(entry)

# Export main components
__all__ = [
    'EnhancedKnowledgeCapture',
    'AgentKnowledgeEntry',
    'knowledge_capture',
    'capture_knowledge_enhanced',
    'capture_agent_execution',
    'capture_pipeline_execution'
]
