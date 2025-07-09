"""
Enhanced Data Analysis Agent with PostgreSQL Knowledge Storage
Combines DuckDB's analytical power with PostgreSQL persistence
"""

import json
import duckdb
import pandas as pd
from typing import Dict, List, Any, Optional
from datetime import datetime
import hashlib

from utils import DatabaseManager, safe_ollama_generate, capture_knowledge

class DataAnalysisAgentEnhanced:
    """
    Enhanced Data Analysis Agent that saves insights to PostgreSQL
    Uses DuckDB for fast analytics, PostgreSQL for knowledge persistence
    """
    
    def __init__(self):
        self.name = "Data Analysis Enhanced"
        self.description = "Advanced data analysis with knowledge capture"
        self.tools = ["nl_to_sql", "data_profiling", "visualization_suggest", "insight_extraction"]
        self.conn = duckdb.connect(':memory:')
        self.db = DatabaseManager()  # PostgreSQL connection
        
        # Create analytics results table if not exists
        if self.db and self.db.connected:
            self._create_analytics_tables()
    
    def _create_analytics_tables(self):
        """Create tables for storing analytics results"""
        try:
            # Table for data profiles
            self.db.connection.execute("""
                CREATE TABLE IF NOT EXISTS data_profiles (
                    id SERIAL PRIMARY KEY,
                    profile_name VARCHAR(255),
                    source_file VARCHAR(255),
                    row_count INTEGER,
                    column_count INTEGER,
                    profile_data JSONB,
                    quality_score FLOAT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Table for query results
            self.db.connection.execute("""
                CREATE TABLE IF NOT EXISTS analytics_queries (
                    id SERIAL PRIMARY KEY,
                    natural_language_query TEXT,
                    generated_sql TEXT,
                    result_summary JSONB,
                    row_count INTEGER,
                    execution_time_ms INTEGER,
                    success BOOLEAN,
                    error_message TEXT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Table for discovered insights
            self.db.connection.execute("""
                CREATE TABLE IF NOT EXISTS data_insights (
                    id SERIAL PRIMARY KEY,
                    dataset_name VARCHAR(255),
                    insight_type VARCHAR(50),
                    insight_description TEXT,
                    insight_data JSONB,
                    confidence_score FLOAT,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            # Table for data patterns
            self.db.connection.execute("""
                CREATE TABLE IF NOT EXISTS data_patterns (
                    id SERIAL PRIMARY KEY,
                    dataset_name VARCHAR(255),
                    pattern_type VARCHAR(50),
                    pattern_description TEXT,
                    affected_columns TEXT[],
                    pattern_details JSONB,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            self.db.connection.commit()
            
        except Exception as e:
            print(f"Error creating analytics tables: {e}")
    
    def analyze_dataframe_enhanced(self, df: pd.DataFrame, dataset_name: str, 
                                  analysis_type: str = "comprehensive") -> Dict[str, Any]:
        """
        Analyze DataFrame and save insights to PostgreSQL
        """
        # Register with DuckDB for fast analysis
        self.conn.register('data_table', df)
        
        results = {
            "dataset_name": dataset_name,
            "timestamp": datetime.now().isoformat(),
            "analyses": {}
        }
        
        # 1. Data Profile
        profile = self._profile_data_enhanced(df, dataset_name)
        results["analyses"]["profile"] = profile
        
        # 2. Data Quality Check
        quality = self._check_data_quality_enhanced(df, dataset_name)
        results["analyses"]["quality"] = quality
        
        # 3. Pattern Detection
        patterns = self._find_patterns_enhanced(df, dataset_name)
        results["analyses"]["patterns"] = patterns
        
        # 4. Statistical Insights
        insights = self._extract_insights(df, dataset_name)
        results["analyses"]["insights"] = insights
        
        # 5. Automated Visualizations Suggestions
        viz_suggestions = self._suggest_visualizations(df)
        results["analyses"]["visualizations"] = viz_suggestions
        
        # Save comprehensive analysis to knowledge base
        if self.db and self.db.connected:
            capture_knowledge(
                tool="data_analysis_enhanced",
                category="analytics",
                subcategory=analysis_type,
                prompt=f"Analyze {dataset_name}",
                response=json.dumps(results, indent=2),
                metadata={
                    "dataset_name": dataset_name,
                    "row_count": len(df),
                    "column_count": len(df.columns)
                }
            )
        
        return results
    
    def _profile_data_enhanced(self, df: pd.DataFrame, dataset_name: str) -> Dict[str, Any]:
        """Enhanced data profiling with PostgreSQL storage"""
        profile = {
            "shape": df.shape,
            "memory_usage": df.memory_usage(deep=True).sum() / 1024**2,  # MB
            "columns": {},
            "data_types": dict(df.dtypes.astype(str)),
            "statistics": {}
        }
        
        # Detailed column analysis
        for col in df.columns:
            col_profile = {
                "unique_values": int(df[col].nunique()),
                "missing_count": int(df[col].isna().sum()),
                "missing_percentage": float((df[col].isna().sum() / len(df)) * 100),
                "most_frequent": None,
                "data_type": str(df[col].dtype)
            }
            
            # Get most frequent value
            if df[col].nunique() < 100:  # Only for low cardinality
                value_counts = df[col].value_counts()
                if len(value_counts) > 0:
                    col_profile["most_frequent"] = {
                        "value": str(value_counts.index[0]),
                        "count": int(value_counts.iloc[0]),
                        "percentage": float((value_counts.iloc[0] / len(df)) * 100)
                    }
            
            # Numeric statistics
            if df[col].dtype in ['int64', 'float64']:
                col_profile["statistics"] = {
                    "mean": float(df[col].mean()),
                    "median": float(df[col].median()),
                    "std": float(df[col].std()),
                    "min": float(df[col].min()),
                    "max": float(df[col].max()),
                    "q1": float(df[col].quantile(0.25)),
                    "q3": float(df[col].quantile(0.75))
                }
                
                # Detect outliers using IQR
                Q1 = df[col].quantile(0.25)
                Q3 = df[col].quantile(0.75)
                IQR = Q3 - Q1
                outliers = df[(df[col] < Q1 - 1.5 * IQR) | (df[col] > Q3 + 1.5 * IQR)]
                col_profile["outlier_count"] = len(outliers)
            
            profile["columns"][col] = col_profile
        
        # Calculate data quality score
        quality_score = self._calculate_quality_score(profile)
        profile["quality_score"] = quality_score
        
        # Save to PostgreSQL
        if self.db and self.db.connected:
            try:
                self.db.connection.execute("""
                    INSERT INTO data_profiles 
                    (profile_name, source_file, row_count, column_count, profile_data, quality_score)
                    VALUES (%s, %s, %s, %s, %s, %s)
                """, (
                    f"{dataset_name}_profile",
                    dataset_name,
                    profile["shape"][0],
                    profile["shape"][1],
                    json.dumps(profile),
                    quality_score
                ))
                self.db.connection.commit()
            except Exception as e:
                print(f"Error saving profile: {e}")
        
        return profile
    
    def _check_data_quality_enhanced(self, df: pd.DataFrame, dataset_name: str) -> Dict[str, Any]:
        """Enhanced data quality checks with issue tracking"""
        issues = []
        recommendations = []
        
        # Comprehensive quality checks
        quality_checks = {
            "duplicates": self._check_duplicates(df),
            "missing_values": self._check_missing_values(df),
            "data_types": self._check_data_types(df),
            "outliers": self._check_outliers(df),
            "inconsistencies": self._check_inconsistencies(df)
        }
        
        # Compile issues and recommendations
        for check_type, check_result in quality_checks.items():
            if check_result["issues"]:
                issues.extend(check_result["issues"])
                recommendations.extend(check_result["recommendations"])
        
        quality_report = {
            "quality_score": max(0, 100 - len(issues) * 5),
            "total_issues": len(issues),
            "issues_by_severity": self._categorize_by_severity(issues),
            "issues": issues,
            "recommendations": recommendations,
            "checks_performed": list(quality_checks.keys())
        }
        
        # Save significant issues to PostgreSQL
        if self.db and self.db.connected and issues:
            for issue in issues[:10]:  # Save top 10 issues
                self._save_insight(
                    dataset_name,
                    "quality_issue",
                    issue["description"],
                    {"issue": issue, "severity": issue.get("severity", "medium")},
                    confidence=0.9
                )
        
        return quality_report
    
    def _find_patterns_enhanced(self, df: pd.DataFrame, dataset_name: str) -> Dict[str, Any]:
        """Enhanced pattern detection with PostgreSQL storage"""
        patterns = {
            "correlations": {},
            "trends": [],
            "anomalies": [],
            "relationships": []
        }
        
        # 1. Correlation analysis for numeric columns
        numeric_cols = df.select_dtypes(include=['int64', 'float64']).columns
        if len(numeric_cols) > 1:
            corr_matrix = df[numeric_cols].corr()
            
            # Find strong correlations
            strong_correlations = []
            for i in range(len(numeric_cols)):
                for j in range(i+1, len(numeric_cols)):
                    corr_value = corr_matrix.iloc[i, j]
                    if abs(corr_value) > 0.7:
                        correlation = {
                            "column1": numeric_cols[i],
                            "column2": numeric_cols[j],
                            "correlation": round(corr_value, 3),
                            "strength": "strong" if abs(corr_value) > 0.9 else "moderate"
                        }
                        strong_correlations.append(correlation)
                        
                        # Save to PostgreSQL
                        self._save_pattern(
                            dataset_name,
                            "correlation",
                            f"Strong correlation between {numeric_cols[i]} and {numeric_cols[j]}",
                            [numeric_cols[i], numeric_cols[j]],
                            correlation
                        )
            
            patterns["correlations"] = strong_correlations
        
        # 2. Time series patterns (if date column exists)
        date_cols = df.select_dtypes(include=['datetime64']).columns
        if len(date_cols) > 0:
            patterns["time_series"] = self._analyze_time_patterns(df, date_cols[0])
        
        # 3. Categorical patterns
        categorical_cols = df.select_dtypes(include=['object']).columns
        for col in categorical_cols[:5]:  # Limit to first 5
            value_dist = df[col].value_counts()
            if len(value_dist) > 0:
                # Check for imbalanced distribution
                if value_dist.iloc[0] / len(df) > 0.8:
                    pattern = {
                        "type": "imbalanced_category",
                        "column": col,
                        "dominant_value": str(value_dist.index[0]),
                        "dominance_percentage": float(value_dist.iloc[0] / len(df) * 100)
                    }
                    patterns["anomalies"].append(pattern)
                    
                    self._save_pattern(
                        dataset_name,
                        "imbalance",
                        f"Highly imbalanced distribution in {col}",
                        [col],
                        pattern
                    )
        
        return patterns
    
    def _extract_insights(self, df: pd.DataFrame, dataset_name: str) -> List[Dict[str, Any]]:
        """Extract and save meaningful insights"""
        insights = []
        
        # 1. Summary statistics insights
        numeric_cols = df.select_dtypes(include=['int64', 'float64']).columns
        for col in numeric_cols:
            # High variability
            if df[col].std() > df[col].mean() * 0.5:
                insight = {
                    "type": "high_variability",
                    "column": col,
                    "description": f"{col} shows high variability (CV > 0.5)",
                    "stats": {
                        "mean": float(df[col].mean()),
                        "std": float(df[col].std()),
                        "cv": float(df[col].std() / df[col].mean())
                    }
                }
                insights.append(insight)
                self._save_insight(dataset_name, "statistical", insight["description"], insight, 0.8)
        
        # 2. Missing data insights
        missing_summary = df.isnull().sum()
        if missing_summary.sum() > 0:
            most_missing = missing_summary.nlargest(3)
            for col, missing_count in most_missing.items():
                if missing_count > 0:
                    insight = {
                        "type": "missing_data",
                        "column": col,
                        "description": f"{col} has {missing_count} missing values ({missing_count/len(df)*100:.1f}%)",
                        "recommendation": "Consider imputation or removal"
                    }
                    insights.append(insight)
        
        # 3. Use DuckDB for advanced insights
        try:
            # Find columns with potential date patterns in string columns
            for col in df.select_dtypes(include=['object']).columns[:3]:
                result = self.conn.execute(f"""
                    SELECT COUNT(DISTINCT {col}) as unique_count,
                           COUNT(*) as total_count
                    FROM data_table
                    WHERE {col} IS NOT NULL
                """).fetchone()
                
                if result and result[0] / result[1] < 0.1:  # Low cardinality
                    insight = {
                        "type": "low_cardinality",
                        "column": col,
                        "description": f"{col} has low cardinality - might be a good candidate for encoding",
                        "unique_ratio": result[0] / result[1]
                    }
                    insights.append(insight)
        except:
            pass
        
        return insights[:10]  # Return top 10 insights
    
    def _suggest_visualizations(self, df: pd.DataFrame) -> List[Dict[str, str]]:
        """Suggest appropriate visualizations based on data"""
        suggestions = []
        
        numeric_cols = df.select_dtypes(include=['int64', 'float64']).columns
        categorical_cols = df.select_dtypes(include=['object']).columns
        date_cols = df.select_dtypes(include=['datetime64']).columns
        
        # Histogram for numeric columns
        if len(numeric_cols) > 0:
            suggestions.append({
                "type": "histogram",
                "description": f"Distribution of {numeric_cols[0]}",
                "columns": [numeric_cols[0]]
            })
        
        # Scatter plot for correlations
        if len(numeric_cols) >= 2:
            suggestions.append({
                "type": "scatter",
                "description": f"Relationship between {numeric_cols[0]} and {numeric_cols[1]}",
                "columns": [numeric_cols[0], numeric_cols[1]]
            })
        
        # Bar chart for categorical
        if len(categorical_cols) > 0:
            suggestions.append({
                "type": "bar",
                "description": f"Count by {categorical_cols[0]}",
                "columns": [categorical_cols[0]]
            })
        
        # Time series if date column exists
        if len(date_cols) > 0 and len(numeric_cols) > 0:
            suggestions.append({
                "type": "line",
                "description": f"{numeric_cols[0]} over time",
                "columns": [date_cols[0], numeric_cols[0]]
            })
        
        return suggestions
    
    def natural_language_query_enhanced(self, nl_query: str, table_schema: Dict,
                                      dataset_name: str = "data_table") -> Dict[str, Any]:
        """Enhanced NL to SQL with result storage"""
        start_time = datetime.now()
        
        # Build schema context
        schema_str = f"Table: {dataset_name}\nColumns:\n"
        for col, dtype in table_schema.items():
            schema_str += f"- {col} ({dtype})\n"
        
        prompt = f"""
Convert this natural language query to DuckDB SQL:
"{nl_query}"

{schema_str}

Provide:
1. The SQL query
2. A brief explanation of what the query does
3. Any assumptions made

Format:
SQL: <query>
Explanation: <explanation>
"""
        
        response = safe_ollama_generate("deepseek-coder:6.7b", prompt)
        
        # Extract SQL query
        sql_match = re.search(r'SQL:\s*(.+?)(?:\n|Explanation:|$)', response['response'], re.DOTALL)
        sql_query = sql_match.group(1).strip() if sql_match else response['response'].strip()
        
        # Execute query
        try:
            result = self.conn.execute(sql_query).fetchall()
            columns = [desc[0] for desc in self.conn.description]
            
            execution_time = int((datetime.now() - start_time).total_seconds() * 1000)
            
            # Create result summary
            result_summary = {
                "row_count": len(result),
                "column_count": len(columns),
                "columns": columns,
                "sample_rows": result[:5] if result else [],
                "has_aggregation": any(keyword in sql_query.upper() 
                                     for keyword in ['SUM', 'AVG', 'COUNT', 'MAX', 'MIN']),
                "complexity": self._assess_query_complexity(sql_query)
            }
            
            # Save to PostgreSQL
            if self.db and self.db.connected:
                try:
                    self.db.connection.execute("""
                        INSERT INTO analytics_queries 
                        (natural_language_query, generated_sql, result_summary, 
                         row_count, execution_time_ms, success)
                        VALUES (%s, %s, %s, %s, %s, %s)
                    """, (
                        nl_query,
                        sql_query,
                        json.dumps(result_summary),
                        len(result),
                        execution_time,
                        True
                    ))
                    self.db.connection.commit()
                except Exception as e:
                    print(f"Error saving query result: {e}")
            
            # Also save to knowledge base
            capture_knowledge(
                tool="nl_to_sql",
                category="analytics",
                subcategory="queries",
                prompt=nl_query,
                response=sql_query,
                metadata=result_summary
            )
            
            return {
                "success": True,
                "query": sql_query,
                "result": result,
                "columns": columns,
                "row_count": len(result),
                "execution_time_ms": execution_time,
                "explanation": self._extract_explanation(response['response'])
            }
            
        except Exception as e:
            # Save failed query for learning
            if self.db and self.db.connected:
                try:
                    self.db.connection.execute("""
                        INSERT INTO analytics_queries 
                        (natural_language_query, generated_sql, success, error_message)
                        VALUES (%s, %s, %s, %s)
                    """, (nl_query, sql_query, False, str(e)))
                    self.db.connection.commit()
                except:
                    pass
            
            return {
                "success": False,
                "query": sql_query,
                "error": str(e),
                "suggestion": self._suggest_query_fix(sql_query, str(e))
            }
    
    # Helper methods
    def _calculate_quality_score(self, profile: Dict) -> float:
        """Calculate overall data quality score"""
        score = 100.0
        
        # Deduct for missing values
        for col_data in profile["columns"].values():
            if col_data["missing_percentage"] > 10:
                score -= 5
            elif col_data["missing_percentage"] > 5:
                score -= 2
        
        # Deduct for low cardinality in many columns
        low_cardinality_count = sum(1 for col in profile["columns"].values() 
                                  if col["unique_values"] < 5)
        if low_cardinality_count > len(profile["columns"]) * 0.5:
            score -= 10
        
        return max(0, score)
    
    def _save_pattern(self, dataset_name: str, pattern_type: str, 
                     description: str, columns: List[str], details: Dict):
        """Save discovered pattern to PostgreSQL"""
        if self.db and self.db.connected:
            try:
                self.db.connection.execute("""
                    INSERT INTO data_patterns 
                    (dataset_name, pattern_type, pattern_description, affected_columns, pattern_details)
                    VALUES (%s, %s, %s, %s, %s)
                """, (dataset_name, pattern_type, description, columns, json.dumps(details)))
                self.db.connection.commit()
            except Exception as e:
                print(f"Error saving pattern: {e}")
    
    def _save_insight(self, dataset_name: str, insight_type: str, 
                     description: str, data: Dict, confidence: float):
        """Save insight to PostgreSQL"""
        if self.db and self.db.connected:
            try:
                self.db.connection.execute("""
                    INSERT INTO data_insights 
                    (dataset_name, insight_type, insight_description, insight_data, confidence_score)
                    VALUES (%s, %s, %s, %s, %s)
                """, (dataset_name, insight_type, description, json.dumps(data), confidence))
                self.db.connection.commit()
            except Exception as e:
                print(f"Error saving insight: {e}")
    
    def _check_duplicates(self, df: pd.DataFrame) -> Dict:
        """Check for duplicate rows"""
        issues = []
        recommendations = []
        
        dup_count = df.duplicated().sum()
        if dup_count > 0:
            issues.append({
                "type": "duplicates",
                "severity": "medium",
                "description": f"Found {dup_count} duplicate rows",
                "affected_rows": dup_count
            })
            recommendations.append(f"Remove {dup_count} duplicate rows using df.drop_duplicates()")
        
        return {"issues": issues, "recommendations": recommendations}
    
    def _check_missing_values(self, df: pd.DataFrame) -> Dict:
        """Check for missing values"""
        issues = []
        recommendations = []
        
        for col in df.columns:
            missing_pct = (df[col].isna().sum() / len(df)) * 100
            if missing_pct > 50:
                issues.append({
                    "type": "missing_values",
                    "severity": "high",
                    "description": f"Column '{col}' has {missing_pct:.1f}% missing values",
                    "column": col
                })
                recommendations.append(f"Consider dropping column '{col}' due to high missing values")
            elif missing_pct > 10:
                issues.append({
                    "type": "missing_values",
                    "severity": "medium",
                    "description": f"Column '{col}' has {missing_pct:.1f}% missing values",
                    "column": col
                })
                recommendations.append(f"Impute missing values in '{col}' using appropriate method")
        
        return {"issues": issues, "recommendations": recommendations}
    
    def _check_data_types(self, df: pd.DataFrame) -> Dict:
        """Check for potential data type issues"""
        issues = []
        recommendations = []
        
        # Check for numeric columns stored as strings
        for col in df.select_dtypes(include=['object']).columns:
            try:
                # Try to convert to numeric
                pd.to_numeric(df[col].dropna().iloc[:100])
                issues.append({
                    "type": "incorrect_dtype",
                    "severity": "low",
                    "description": f"Column '{col}' might be numeric but stored as string",
                    "column": col
                })
                recommendations.append(f"Consider converting '{col}' to numeric type")
            except:
                pass
        
        return {"issues": issues, "recommendations": recommendations}
    
    def _check_outliers(self, df: pd.DataFrame) -> Dict:
        """Check for outliers in numeric columns"""
        issues = []
        recommendations = []
        
        numeric_cols = df.select_dtypes(include=['int64', 'float64']).columns
        for col in numeric_cols:
            Q1 = df[col].quantile(0.25)
            Q3 = df[col].quantile(0.75)
            IQR = Q3 - Q1
            outliers = df[(df[col] < Q1 - 1.5 * IQR) | (df[col] > Q3 + 1.5 * IQR)]
            
            if len(outliers) > len(df) * 0.05:  # More than 5% outliers
                issues.append({
                    "type": "outliers",
                    "severity": "medium",
                    "description": f"Column '{col}' has {len(outliers)} outliers ({len(outliers)/len(df)*100:.1f}%)",
                    "column": col
                })
                recommendations.append(f"Investigate outliers in '{col}' - consider capping or transformation")
        
        return {"issues": issues, "recommendations": recommendations}
    
    def _check_inconsistencies(self, df: pd.DataFrame) -> Dict:
        """Check for data inconsistencies"""
        issues = []
        recommendations = []
        
        # Add specific consistency checks based on your needs
        
        return {"issues": issues, "recommendations": recommendations}
    
    def _categorize_by_severity(self, issues: List[Dict]) -> Dict[str, int]:
        """Categorize issues by severity"""
        severity_counts = {"high": 0, "medium": 0, "low": 0}
        for issue in issues:
            severity = issue.get("severity", "medium")
            severity_counts[severity] += 1
        return severity_counts
    
    def _analyze_time_patterns(self, df: pd.DataFrame, date_col: str) -> Dict:
        """Analyze time series patterns"""
        # Simplified time pattern analysis
        return {
            "date_range": {
                "start": str(df[date_col].min()),
                "end": str(df[date_col].max())
            },
            "frequency": "Unknown"  # Could be enhanced
        }
    
    def _assess_query_complexity(self, sql_query: str) -> str:
        """Assess SQL query complexity"""
        query_upper = sql_query.upper()
        
        if "JOIN" in query_upper:
            if query_upper.count("JOIN") > 2:
                return "complex"
            return "moderate"
        elif any(keyword in query_upper for keyword in ["GROUP BY", "HAVING", "UNION"]):
            return "moderate"
        else:
            return "simple"
    
    def _extract_explanation(self, response: str) -> str:
        """Extract explanation from LLM response"""
        explanation_match = re.search(r'Explanation:\s*(.+?)(?:\n|$)', response, re.DOTALL)
        return explanation_match.group(1).strip() if explanation_match else "Query converts natural language to SQL"
    
    def _suggest_query_fix(self, sql_query: str, error: str) -> str:
        """Suggest fix for failed query"""
        if "column" in error.lower() and "not found" in error.lower():
            return "Check column names - they might be case sensitive or misspelled"
        elif "syntax" in error.lower():
            return "Check SQL syntax - DuckDB uses standard SQL syntax"
        else:
            return "Verify table name and column data types"

# Add function to query saved analytics
def query_analytics_history(db: DatabaseManager, query_type: str = "all") -> pd.DataFrame:
    """
    Query saved analytics from PostgreSQL
    
    Args:
        db: DatabaseManager instance
        query_type: 'profiles', 'queries', 'insights', 'patterns', or 'all'
    
    Returns:
        DataFrame with results
    """
    queries = {
        "profiles": """
            SELECT profile_name, source_file, row_count, column_count, 
                   quality_score, created_at
            FROM data_profiles
            ORDER BY created_at DESC
            LIMIT 100
        """,
        "queries": """
            SELECT natural_language_query, generated_sql, row_count,
                   execution_time_ms, success, created_at
            FROM analytics_queries
            ORDER BY created_at DESC
            LIMIT 100
        """,
        "insights": """
            SELECT dataset_name, insight_type, insight_description,
                   confidence_score, created_at
            FROM data_insights
            ORDER BY confidence_score DESC
            LIMIT 100
        """,
        "patterns": """
            SELECT dataset_name, pattern_type, pattern_description,
                   affected_columns, created_at
            FROM data_patterns
            ORDER BY created_at DESC
            LIMIT 100
        """
    }
    
    if query_type in queries:
        result = db.connection.execute(queries[query_type])
        columns = [desc[0] for desc in result.description]
        data = result.fetchall()
        return pd.DataFrame(data, columns=columns)
    else:
        # Return all as a dictionary
        return {
            name: query_analytics_history(db, name)
            for name in queries.keys()
        }

# Update the original agent_hub_enhancements.py import
def get_data_analysis_agent():
    """Get the enhanced data analysis agent"""
    return DataAnalysisAgentEnhanced()
