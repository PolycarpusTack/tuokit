"""
Agent Hub Enhancements - Integration of awesome-llm-apps agents
Following TuoKit Architect principles: minimalist, practical, extensible
"""

import json
import re
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
import streamlit as st
import pandas as pd
import duckdb
from pathlib import Path
import requests

from utils import DatabaseManager, safe_ollama_generate, capture_knowledge

# ========== AI SYSTEM ARCHITECT AGENT ==========

class SystemArchitectAgent:
    """
    AI System Architect from awesome-llm-apps
    Generates structured technical specifications with DeepSeek R1
    """
    
    def __init__(self):
        self.name = "System Architect"
        self.description = "Designs complete system architectures with structured reasoning"
        self.tools = ["architecture_planner", "risk_assessor", "cost_estimator"]
        self.db = DatabaseManager()
    
    def generate_architecture(self, requirements: str, model: str = "deepseek-r1:8b") -> Dict[str, Any]:
        """Generate comprehensive system architecture"""
        
        prompt = f"""
You are an AI System Architect. Design a complete system architecture for:

{requirements}

Provide a structured JSON response with:
{{
    "system_name": "...",
    "overview": "Brief system description",
    "architecture": {{
        "components": [
            {{"name": "...", "type": "...", "purpose": "...", "technologies": []}}
        ],
        "data_flow": [
            {{"from": "...", "to": "...", "data_type": "...", "protocol": "..."}}
        ],
        "patterns": ["Pattern names used"]
    }},
    "technical_stack": {{
        "frontend": [],
        "backend": [],
        "database": [],
        "infrastructure": [],
        "monitoring": []
    }},
    "security": {{
        "authentication": "...",
        "authorization": "...",
        "data_protection": "...",
        "compliance": []
    }},
    "performance": {{
        "expected_load": "...",
        "scaling_strategy": "...",
        "caching": "...",
        "optimization_areas": []
    }},
    "risks": [
        {{"risk": "...", "impact": "high/medium/low", "mitigation": "..."}}
    ],
    "cost_estimation": {{
        "development_hours": 0,
        "monthly_infrastructure": "$0",
        "team_size": 0
    }},
    "implementation_roadmap": [
        {{"phase": 1, "name": "...", "duration": "...", "deliverables": []}}
    ]
}}

Think step by step and provide comprehensive, production-ready architecture.
"""
        
        response = safe_ollama_generate(model, prompt)
        
        try:
            # Extract JSON from response
            json_match = re.search(r'\{.*\}', response['response'], re.DOTALL)
            if json_match:
                architecture = json.loads(json_match.group())
                
                # Enhance with additional analysis
                architecture["quality_score"] = self._calculate_quality_score(architecture)
                architecture["completeness"] = self._check_completeness(architecture)
                
                # Save to knowledge base
                if self.db and self.db.connected:
                    capture_knowledge(
                        tool="system_architect",
                        category="architecture",
                        subcategory="system_design",
                        prompt=requirements,
                        response=json.dumps(architecture, indent=2),
                        metadata={"quality_score": architecture["quality_score"]}
                    )
                
                return architecture
        except Exception as e:
            return {
                "error": str(e),
                "raw_response": response['response'][:500]
            }
    
    def _calculate_quality_score(self, architecture: Dict) -> float:
        """Calculate architecture quality score"""
        score = 0.0
        
        # Check component definition
        if architecture.get("architecture", {}).get("components"):
            score += 0.2
        
        # Check security considerations
        if architecture.get("security", {}).get("authentication"):
            score += 0.2
        
        # Check performance planning
        if architecture.get("performance", {}).get("scaling_strategy"):
            score += 0.2
        
        # Check risk assessment
        if len(architecture.get("risks", [])) > 0:
            score += 0.2
        
        # Check implementation roadmap
        if len(architecture.get("implementation_roadmap", [])) > 0:
            score += 0.2
        
        return score
    
    def _check_completeness(self, architecture: Dict) -> Dict[str, bool]:
        """Check architecture completeness"""
        return {
            "has_components": bool(architecture.get("architecture", {}).get("components")),
            "has_data_flow": bool(architecture.get("architecture", {}).get("data_flow")),
            "has_security": bool(architecture.get("security")),
            "has_performance": bool(architecture.get("performance")),
            "has_risks": bool(architecture.get("risks")),
            "has_costs": bool(architecture.get("cost_estimation")),
            "has_roadmap": bool(architecture.get("implementation_roadmap"))
        }

# ========== AI DEEP RESEARCH AGENT ==========

class DeepResearchAgent:
    """
    AI Deep Research Agent from awesome-llm-apps
    Performs comprehensive research with web crawling
    """
    
    def __init__(self):
        self.name = "Deep Research"
        self.description = "Performs comprehensive technical research"
        self.tools = ["web_research", "document_analysis", "report_generation"]
        self.db = DatabaseManager()
    
    def research_topic(self, topic: str, depth: str = "comprehensive") -> Dict[str, Any]:
        """Perform deep research on a topic"""
        
        # Phase 1: Research
        research_prompt = f"""
Research the following topic comprehensively: {topic}

Provide detailed information about:
1. Current state of the art
2. Key technologies and approaches
3. Best practices and patterns
4. Common challenges and solutions
5. Future trends and developments

Include specific examples, tools, and frameworks where relevant.
"""
        
        research_response = safe_ollama_generate("deepseek-r1:8b", research_prompt)
        
        # Phase 2: Elaboration
        elaboration_prompt = f"""
Based on this research about {topic}:

{research_response['response'][:2000]}

Provide:
1. Practical implementation guide
2. Code examples and snippets
3. Tool recommendations
4. Learning resources
5. Common pitfalls to avoid

Format as a comprehensive technical report.
"""
        
        elaboration_response = safe_ollama_generate("deepseek-r1:8b", elaboration_prompt)
        
        # Combine into structured report
        report = {
            "topic": topic,
            "executive_summary": self._generate_summary(research_response['response']),
            "research_findings": research_response['response'],
            "practical_guide": elaboration_response['response'],
            "key_insights": self._extract_key_insights(research_response['response']),
            "recommendations": self._extract_recommendations(elaboration_response['response']),
            "resources": self._extract_resources(elaboration_response['response'])
        }
        
        # Save to knowledge base
        if self.db and self.db.connected:
            capture_knowledge(
                tool="deep_research",
                category="research",
                subcategory=topic.split()[0].lower(),
                prompt=topic,
                response=json.dumps(report, indent=2),
                metadata={"depth": depth}
            )
        
        return report
    
    def _generate_summary(self, content: str) -> str:
        """Generate executive summary"""
        summary_prompt = f"Summarize in 3-4 sentences: {content[:1000]}"
        response = safe_ollama_generate("deepseek-r1:1.5b", summary_prompt)
        return response['response']
    
    def _extract_key_insights(self, content: str) -> List[str]:
        """Extract key insights from research"""
        # Simple extraction - in production, use NLP
        insights = []
        lines = content.split('\n')
        for line in lines:
            if any(keyword in line.lower() for keyword in ['important', 'key', 'critical', 'essential']):
                insights.append(line.strip())
        return insights[:5]  # Top 5 insights
    
    def _extract_recommendations(self, content: str) -> List[str]:
        """Extract recommendations"""
        recommendations = []
        lines = content.split('\n')
        for line in lines:
            if any(keyword in line.lower() for keyword in ['recommend', 'suggest', 'should', 'best practice']):
                recommendations.append(line.strip())
        return recommendations[:5]
    
    def _extract_resources(self, content: str) -> List[Dict[str, str]]:
        """Extract mentioned resources"""
        resources = []
        
        # Extract tool/framework mentions
        tool_pattern = r'\b(?:tool|framework|library|package):\s*(\w+)'
        tools = re.findall(tool_pattern, content, re.IGNORECASE)
        
        for tool in set(tools):
            resources.append({
                "type": "tool",
                "name": tool,
                "description": f"Mentioned {tool} in research"
            })
        
        return resources

# ========== MULTIMODAL AGENT WITH VISION ==========

class MultimodalCodingAgent:
    """
    Multimodal Coding Agent Team from awesome-llm-apps
    Supports vision capabilities and sandboxed execution
    """
    
    def __init__(self):
        self.name = "Multimodal Coding"
        self.description = "Coding with vision capabilities"
        self.tools = ["vision_analyzer", "code_from_image", "diagram_interpreter"]
        self.db = DatabaseManager()
    
    def analyze_code_image(self, image_path: str) -> Dict[str, Any]:
        """Analyze code from screenshot or image"""
        
        # For now, simulate - in production, use actual vision model
        result = {
            "detected_content": "Code screenshot analysis",
            "language": "python",
            "extracted_code": "# Extracted code would go here",
            "observations": [
                "Syntax highlighting detected",
                "Appears to be a function definition",
                "Contains error handling"
            ]
        }
        
        return result
    
    def generate_from_diagram(self, diagram_description: str) -> str:
        """Generate code from architecture diagram description"""
        
        prompt = f"""
Based on this architecture diagram description:
{diagram_description}

Generate:
1. Project structure
2. Main components/classes
3. Interfaces between components
4. Basic implementation skeleton

Use best practices and design patterns.
"""
        
        response = safe_ollama_generate("deepseek-coder:6.7b", prompt)
        return response['response']

# ========== AGENTIC RAG WITH REASONING ==========

class AgenticRAGAgent:
    """
    Agentic RAG with Reasoning from awesome-llm-apps
    Provides transparent reasoning and vector search
    """
    
    def __init__(self):
        self.name = "Agentic RAG"
        self.description = "RAG with transparent reasoning"
        self.tools = ["vector_search", "reasoning_chain", "source_management"]
        self.db = DatabaseManager()
        self.knowledge_sources = []
    
    def add_knowledge_source(self, source: Dict[str, str]):
        """Add a knowledge source"""
        self.knowledge_sources.append({
            "id": len(self.knowledge_sources),
            "type": source.get("type", "document"),
            "content": source.get("content", ""),
            "metadata": source.get("metadata", {})
        })
    
    def query_with_reasoning(self, query: str) -> Dict[str, Any]:
        """Query with transparent reasoning steps"""
        
        reasoning_steps = []
        
        # Step 1: Understand query
        understanding = self._understand_query(query)
        reasoning_steps.append({
            "step": "Query Understanding",
            "reasoning": understanding,
            "confidence": 0.9
        })
        
        # Step 2: Search relevant sources
        relevant_sources = self._search_sources(query)
        reasoning_steps.append({
            "step": "Source Selection",
            "reasoning": f"Found {len(relevant_sources)} relevant sources",
            "sources": relevant_sources
        })
        
        # Step 3: Generate answer with reasoning
        answer = self._generate_answer(query, relevant_sources)
        reasoning_steps.append({
            "step": "Answer Generation",
            "reasoning": "Synthesized answer from sources",
            "answer": answer
        })
        
        return {
            "query": query,
            "answer": answer,
            "reasoning_chain": reasoning_steps,
            "sources_used": len(relevant_sources),
            "confidence": self._calculate_confidence(reasoning_steps)
        }
    
    def _understand_query(self, query: str) -> str:
        """Understand query intent"""
        prompt = f"What is the user asking about in this query: '{query}'? Be specific."
        response = safe_ollama_generate("deepseek-r1:1.5b", prompt)
        return response['response']
    
    def _search_sources(self, query: str) -> List[Dict]:
        """Search knowledge sources"""
        # Simple keyword matching - in production, use vector search
        relevant = []
        query_words = set(query.lower().split())
        
        for source in self.knowledge_sources:
            content_words = set(source['content'].lower().split())
            overlap = len(query_words & content_words)
            if overlap > 0:
                relevant.append({
                    "source_id": source['id'],
                    "relevance_score": overlap / len(query_words),
                    "snippet": source['content'][:200]
                })
        
        return sorted(relevant, key=lambda x: x['relevance_score'], reverse=True)[:3]
    
    def _generate_answer(self, query: str, sources: List[Dict]) -> str:
        """Generate answer from sources"""
        context = "\n".join([s['snippet'] for s in sources])
        
        prompt = f"""
Based on these sources:
{context}

Answer this query: {query}

Provide a clear, accurate answer based only on the given sources.
"""
        
        response = safe_ollama_generate("deepseek-r1:1.5b", prompt)
        return response['response']
    
    def _calculate_confidence(self, steps: List[Dict]) -> float:
        """Calculate answer confidence"""
        # Simple confidence calculation
        return 0.8 if len(steps) >= 3 else 0.6

# ========== DATA ANALYSIS AGENT WITH DUCKDB ==========

# Import the enhanced version with PostgreSQL integration
try:
    from data_analysis_enhanced import DataAnalysisAgentEnhanced as DataAnalysisAgent
except ImportError:
    # Fallback to original if enhanced not available
    class DataAnalysisAgent:
    """
    Enhanced Data Analysis Agent with DuckDB
    Natural language to SQL with advanced analytics
    """
    
    def __init__(self):
        self.name = "Data Analysis"
        self.description = "Advanced data analysis with DuckDB"
        self.tools = ["nl_to_sql", "data_profiling", "visualization_suggest"]
        self.conn = duckdb.connect(':memory:')
    
    def analyze_dataframe(self, df: pd.DataFrame, analysis_type: str = "profile") -> Dict[str, Any]:
        """Analyze pandas DataFrame with DuckDB"""
        
        # Register DataFrame with DuckDB
        self.conn.register('data_table', df)
        
        if analysis_type == "profile":
            return self._profile_data(df)
        elif analysis_type == "quality":
            return self._check_data_quality(df)
        elif analysis_type == "patterns":
            return self._find_patterns(df)
        else:
            return {"error": "Unknown analysis type"}
    
    def _profile_data(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Generate data profile"""
        profile = {
            "shape": df.shape,
            "columns": {},
            "missing_values": {},
            "data_types": dict(df.dtypes),
            "statistics": {}
        }
        
        # Column analysis
        for col in df.columns:
            profile["columns"][col] = {
                "unique_values": df[col].nunique(),
                "missing_count": df[col].isna().sum(),
                "missing_percentage": (df[col].isna().sum() / len(df)) * 100
            }
            
            # Numeric statistics
            if df[col].dtype in ['int64', 'float64']:
                profile["statistics"][col] = {
                    "mean": float(df[col].mean()),
                    "median": float(df[col].median()),
                    "std": float(df[col].std()),
                    "min": float(df[col].min()),
                    "max": float(df[col].max())
                }
        
        # DuckDB advanced analytics
        outliers_query = """
        SELECT column_name, 
               COUNT(*) as outlier_count
        FROM (
            SELECT COLUMNS(*) as column_name
            FROM data_table
        )
        """
        
        return profile
    
    def natural_language_query(self, nl_query: str, table_schema: Dict) -> Dict[str, Any]:
        """Convert natural language to SQL using DuckDB"""
        
        # Build schema context
        schema_str = "Table: data_table\nColumns:\n"
        for col, dtype in table_schema.items():
            schema_str += f"- {col} ({dtype})\n"
        
        prompt = f"""
Convert this natural language query to DuckDB SQL:
"{nl_query}"

{schema_str}

Provide only the SQL query, no explanation.
"""
        
        response = safe_ollama_generate("deepseek-coder:6.7b", prompt)
        sql_query = response['response'].strip()
        
        # Validate and execute
        try:
            result = self.conn.execute(sql_query).fetchall()
            columns = [desc[0] for desc in self.conn.description]
            
            return {
                "success": True,
                "query": sql_query,
                "result": result,
                "columns": columns,
                "row_count": len(result)
            }
        except Exception as e:
            return {
                "success": False,
                "query": sql_query,
                "error": str(e),
                "suggestion": "Check column names and data types"
            }
    
    def _check_data_quality(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Check data quality issues"""
        issues = []
        
        # Check for duplicates
        if df.duplicated().sum() > 0:
            issues.append({
                "type": "duplicates",
                "count": df.duplicated().sum(),
                "severity": "medium"
            })
        
        # Check for missing values
        for col in df.columns:
            missing_pct = (df[col].isna().sum() / len(df)) * 100
            if missing_pct > 10:
                issues.append({
                    "type": "missing_values",
                    "column": col,
                    "percentage": missing_pct,
                    "severity": "high" if missing_pct > 50 else "medium"
                })
        
        return {
            "quality_score": max(0, 100 - len(issues) * 10),
            "issues": issues,
            "recommendations": self._generate_quality_recommendations(issues)
        }
    
    def _generate_quality_recommendations(self, issues: List[Dict]) -> List[str]:
        """Generate data quality recommendations"""
        recommendations = []
        
        for issue in issues:
            if issue["type"] == "duplicates":
                recommendations.append("Remove duplicate rows using df.drop_duplicates()")
            elif issue["type"] == "missing_values":
                if issue["severity"] == "high":
                    recommendations.append(f"Consider dropping column '{issue['column']}' due to high missing values")
                else:
                    recommendations.append(f"Impute missing values in '{issue['column']}' using appropriate method")
        
        return recommendations
    
    def _find_patterns(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Find patterns in data"""
        patterns = {
            "correlations": {},
            "trends": [],
            "anomalies": []
        }
        
        # Find correlations for numeric columns
        numeric_cols = df.select_dtypes(include=['int64', 'float64']).columns
        if len(numeric_cols) > 1:
            corr_matrix = df[numeric_cols].corr()
            
            # Find strong correlations
            for i in range(len(numeric_cols)):
                for j in range(i+1, len(numeric_cols)):
                    corr_value = corr_matrix.iloc[i, j]
                    if abs(corr_value) > 0.7:
                        patterns["correlations"][f"{numeric_cols[i]}_vs_{numeric_cols[j]}"] = round(corr_value, 3)
        
        return patterns

# ========== INTEGRATION FUNCTIONS ==========

def create_enhanced_agent_registry():
    """Create registry with all enhanced agents"""
    from pages.agent_hub import AGENT_REGISTRY
    
    # Add new agents to registry
    AGENT_REGISTRY["system_architect"] = SystemArchitectAgent()
    AGENT_REGISTRY["deep_research"] = DeepResearchAgent()
    AGENT_REGISTRY["multimodal_coding"] = MultimodalCodingAgent()
    AGENT_REGISTRY["agentic_rag"] = AgenticRAGAgent()
    AGENT_REGISTRY["data_analysis_duckdb"] = DataAnalysisAgent()
    
    return AGENT_REGISTRY

# ========== ENHANCED QUICK ACTIONS ==========

def show_enhanced_quick_actions():
    """Enhanced quick actions with new agents"""
    st.subheader("⚡ Enhanced Quick Actions")
    
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        if st.button("🏗️ System Design", use_container_width=True):
            st.session_state.quick_action = "architect"
    
    with col2:
        if st.button("🔬 Deep Research", use_container_width=True):
            st.session_state.quick_action = "research"
    
    with col3:
        if st.button("📊 Data Analysis", use_container_width=True):
            st.session_state.quick_action = "data_analysis"
    
    with col4:
        if st.button("🧠 RAG Query", use_container_width=True):
            st.session_state.quick_action = "rag_query"
    
    # Handle enhanced quick actions
    if "quick_action" in st.session_state:
        handle_enhanced_quick_action(st.session_state.quick_action)

def handle_enhanced_quick_action(action: str):
    """Handle enhanced quick actions"""
    
    if action == "architect":
        st.markdown("### 🏗️ System Architecture Design")
        
        requirements = st.text_area(
            "Describe your system requirements:",
            placeholder="E-commerce platform with microservices, supporting 100k daily users...",
            height=150
        )
        
        if requirements and st.button("🚀 Generate Architecture", type="primary"):
            agent = SystemArchitectAgent()
            
            with st.spinner("Designing system architecture..."):
                architecture = agent.generate_architecture(requirements)
            
            if "error" not in architecture:
                # Display architecture
                st.success(f"Architecture Quality Score: {architecture.get('quality_score', 0) * 100:.0f}%")
                
                # Overview
                st.markdown("#### System Overview")
                st.info(architecture.get('overview', 'No overview available'))
                
                # Components
                st.markdown("#### Components")
                for component in architecture.get('architecture', {}).get('components', []):
                    with st.expander(component['name']):
                        st.write(f"**Type**: {component['type']}")
                        st.write(f"**Purpose**: {component['purpose']}")
                        st.write(f"**Technologies**: {', '.join(component['technologies'])}")
                
                # Technical Stack
                st.markdown("#### Technical Stack")
                stack = architecture.get('technical_stack', {})
                cols = st.columns(len(stack))
                for i, (category, techs) in enumerate(stack.items()):
                    with cols[i]:
                        st.markdown(f"**{category.title()}**")
                        for tech in techs:
                            st.write(f"• {tech}")
                
                # Risks
                st.markdown("#### Risk Assessment")
                for risk in architecture.get('risks', []):
                    severity_color = {"high": "🔴", "medium": "🟡", "low": "🟢"}
                    st.write(f"{severity_color.get(risk['impact'], '⚪')} **{risk['risk']}**")
                    st.caption(f"Mitigation: {risk['mitigation']}")
                
                # Download JSON
                st.download_button(
                    "📥 Download Architecture JSON",
                    json.dumps(architecture, indent=2),
                    "architecture.json",
                    "application/json"
                )
            else:
                st.error(f"Failed to generate architecture: {architecture['error']}")
    
    elif action == "research":
        st.markdown("### 🔬 Deep Technical Research")
        
        topic = st.text_input(
            "Research topic:",
            placeholder="Kubernetes auto-scaling strategies for ML workloads"
        )
        
        col1, col2 = st.columns(2)
        with col1:
            depth = st.selectbox("Research depth", ["Quick", "Comprehensive", "Exhaustive"])
        
        if topic and st.button("🔍 Start Research", type="primary"):
            agent = DeepResearchAgent()
            
            with st.spinner("Conducting deep research..."):
                report = agent.research_topic(topic, depth.lower())
            
            # Display report
            st.markdown("#### Executive Summary")
            st.info(report.get('executive_summary', 'No summary available'))
            
            # Key Insights
            st.markdown("#### Key Insights")
            for insight in report.get('key_insights', []):
                st.write(f"• {insight}")
            
            # Findings
            with st.expander("Research Findings", expanded=False):
                st.markdown(report.get('research_findings', ''))
            
            # Practical Guide
            with st.expander("Implementation Guide", expanded=True):
                st.markdown(report.get('practical_guide', ''))
            
            # Recommendations
            st.markdown("#### Recommendations")
            for rec in report.get('recommendations', []):
                st.write(f"✅ {rec}")
            
            # Download report
            report_md = f"""# Research Report: {topic}
            
## Executive Summary
{report.get('executive_summary', '')}

## Key Insights
{chr(10).join(['- ' + i for i in report.get('key_insights', [])])}

## Research Findings
{report.get('research_findings', '')}

## Implementation Guide
{report.get('practical_guide', '')}

## Recommendations
{chr(10).join(['- ' + r for r in report.get('recommendations', [])])}
"""
            st.download_button(
                "📥 Download Report",
                report_md,
                f"research_{topic.replace(' ', '_')}.md",
                "text/markdown"
            )
    
    elif action == "data_analysis":
        st.markdown("### 📊 Advanced Data Analysis with DuckDB")
        
        uploaded_file = st.file_uploader("Upload CSV file", type=['csv'])
        
        if uploaded_file:
            df = pd.read_csv(uploaded_file)
            st.write(f"Loaded data: {df.shape[0]} rows, {df.shape[1]} columns")
            
            # Show preview
            st.dataframe(df.head())
            
            agent = DataAnalysisAgent()
            
            # Analysis options
            analysis_type = st.radio(
                "Analysis type",
                ["Data Profile", "Quality Check", "Natural Language Query"],
                horizontal=True
            )
            
            if analysis_type == "Natural Language Query":
                nl_query = st.text_input(
                    "Ask a question about your data:",
                    placeholder="What is the average sales by region?"
                )
                
                if nl_query and st.button("🔍 Execute Query"):
                    with st.spinner("Converting to SQL and executing..."):
                        result = agent.natural_language_query(
                            nl_query,
                            dict(df.dtypes)
                        )
                    
                    if result['success']:
                        st.success("Query executed successfully!")
                        st.code(result['query'], language='sql')
                        
                        # Display results
                        if result['result']:
                            result_df = pd.DataFrame(result['result'], columns=result['columns'])
                            st.dataframe(result_df)
                        else:
                            st.info("Query returned no results")
                    else:
                        st.error(f"Query failed: {result['error']}")
                        st.info(f"💡 {result['suggestion']}")
            
            else:
                if st.button(f"Run {analysis_type}"):
                    with st.spinner(f"Running {analysis_type.lower()}..."):
                        if analysis_type == "Data Profile":
                            profile = agent.analyze_dataframe(df, "profile")
                            
                            # Display profile
                            col1, col2 = st.columns(2)
                            with col1:
                                st.metric("Total Rows", profile['shape'][0])
                                st.metric("Total Columns", profile['shape'][1])
                            
                            # Column details
                            st.markdown("#### Column Analysis")
                            for col, info in profile['columns'].items():
                                with st.expander(f"{col} - {profile['data_types'][col]}"):
                                    st.write(f"Unique values: {info['unique_values']}")
                                    st.write(f"Missing: {info['missing_count']} ({info['missing_percentage']:.1f}%)")
                                    
                                    if col in profile['statistics']:
                                        stats = profile['statistics'][col]
                                        st.write("**Statistics:**")
                                        for stat, value in stats.items():
                                            st.write(f"• {stat}: {value:.2f}")
                        
                        elif analysis_type == "Quality Check":
                            quality = agent.analyze_dataframe(df, "quality")
                            
                            st.metric("Data Quality Score", f"{quality['quality_score']}%")
                            
                            if quality['issues']:
                                st.warning(f"Found {len(quality['issues'])} quality issues")
                                
                                for issue in quality['issues']:
                                    severity_icon = "🔴" if issue['severity'] == 'high' else "🟡"
                                    st.write(f"{severity_icon} {issue['type']}: {issue.get('count', issue.get('percentage', 'N/A'))}")
                                
                                st.markdown("#### Recommendations")
                                for rec in quality['recommendations']:
                                    st.write(f"• {rec}")
                            else:
                                st.success("No quality issues found!")
    
    elif action == "rag_query":
        st.markdown("### 🧠 RAG with Reasoning Transparency")
        
        # Knowledge source management
        with st.expander("Knowledge Sources"):
            source_type = st.selectbox("Source type", ["Document", "Code", "FAQ"])
            source_content = st.text_area("Add knowledge source:", height=100)
            
            if source_content and st.button("Add Source"):
                if "rag_agent" not in st.session_state:
                    st.session_state.rag_agent = AgenticRAGAgent()
                
                st.session_state.rag_agent.add_knowledge_source({
                    "type": source_type.lower(),
                    "content": source_content
                })
                st.success(f"Added {source_type} source")
        
        # Query interface
        query = st.text_input(
            "Ask a question:",
            placeholder="How do I implement authentication in FastAPI?"
        )
        
        if query and st.button("🔍 Query with Reasoning"):
            if "rag_agent" not in st.session_state:
                st.session_state.rag_agent = AgenticRAGAgent()
            
            if not st.session_state.rag_agent.knowledge_sources:
                st.warning("Please add some knowledge sources first")
            else:
                with st.spinner("Querying with reasoning..."):
                    result = st.session_state.rag_agent.query_with_reasoning(query)
                
                # Display answer
                st.markdown("#### Answer")
                st.write(result['answer'])
                
                # Show reasoning chain
                st.markdown("#### Reasoning Process")
                for step in result['reasoning_chain']:
                    with st.expander(f"Step: {step['step']}", expanded=False):
                        st.write(step.get('reasoning', ''))
                        if 'sources' in step:
                            st.write(f"Sources used: {len(step['sources'])}")
                        if 'confidence' in step:
                            st.progress(step['confidence'])
                
                # Confidence score
                st.metric("Answer Confidence", f"{result['confidence'] * 100:.0f}%")

# TODO: Add integration points in main agent_hub_enhanced.py:
# 1. Import these new agents
# 2. Add to AGENT_REGISTRY
# 3. Include in visual pipeline builder tool palette
# 4. Add to educational scenarios
# 5. Create dedicated UI sections for each specialized agent
