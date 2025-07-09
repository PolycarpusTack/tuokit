"""
PostgreSQL Training Store for LoRA
Manages Q&A pairs and training data for continuous learning
"""

import json
import logging
import psycopg2
from psycopg2.extras import RealDictCursor
from datetime import datetime
from typing import Dict, List, Optional, Tuple
from pathlib import Path

logger = logging.getLogger(__name__)

class PostgresTrainingStore:
    """
    Manages training data in PostgreSQL for LoRA fine-tuning
    Stores verified Q&A pairs from RAG interactions
    """
    
    def __init__(self, db_url: str):
        """
        Initialize the training store
        
        Args:
            db_url: PostgreSQL connection string
        """
        self.db_url = db_url
        self._init_database()
    
    def _init_database(self):
        """Create tables if they don't exist"""
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        # Main training examples table
        cur.execute("""
            CREATE TABLE IF NOT EXISTS smalltalk_training_examples (
                id SERIAL PRIMARY KEY,
                question TEXT NOT NULL,
                answer TEXT NOT NULL,
                code_context TEXT,
                verified BOOLEAN DEFAULT FALSE,
                quality_score FLOAT DEFAULT 0.5,
                source_file VARCHAR(255),
                pattern_type VARCHAR(50),
                created_at TIMESTAMP DEFAULT NOW(),
                verified_by VARCHAR(100),
                verified_at TIMESTAMP,
                used_in_training BOOLEAN DEFAULT FALSE,
                training_run_id VARCHAR(50),
                metadata JSONB DEFAULT '{}'::jsonb
            );
            
            CREATE INDEX IF NOT EXISTS idx_training_verified 
            ON smalltalk_training_examples(verified, quality_score);
            
            CREATE INDEX IF NOT EXISTS idx_training_pattern 
            ON smalltalk_training_examples(pattern_type);
        """)
        
        # Feedback and corrections table
        cur.execute("""
            CREATE TABLE IF NOT EXISTS training_feedback (
                id SERIAL PRIMARY KEY,
                example_id INTEGER REFERENCES smalltalk_training_examples(id),
                helpful BOOLEAN,
                corrected_answer TEXT,
                feedback_text TEXT,
                created_at TIMESTAMP DEFAULT NOW(),
                created_by VARCHAR(100)
            );
        """)
        
        # Training runs history
        cur.execute("""
            CREATE TABLE IF NOT EXISTS lora_training_runs (
                id SERIAL PRIMARY KEY,
                run_id VARCHAR(50) UNIQUE,
                model_name VARCHAR(100),
                base_model VARCHAR(100),
                num_examples INTEGER,
                num_epochs INTEGER,
                learning_rate FLOAT,
                final_loss FLOAT,
                metrics JSONB,
                started_at TIMESTAMP,
                completed_at TIMESTAMP,
                status VARCHAR(50),
                output_path VARCHAR(255)
            );
        """)
        
        # RAG to training pipeline
        cur.execute("""
            CREATE TABLE IF NOT EXISTS rag_to_training_queue (
                id SERIAL PRIMARY KEY,
                rag_query TEXT NOT NULL,
                rag_answer TEXT NOT NULL,
                rag_sources TEXT[],
                user_feedback_score FLOAT,
                auto_quality_score FLOAT,
                processed BOOLEAN DEFAULT FALSE,
                processed_at TIMESTAMP,
                created_at TIMESTAMP DEFAULT NOW()
            );
        """)
        
        conn.commit()
        cur.close()
        conn.close()
        
        logger.info("PostgreSQL training store initialized")
    
    def add_training_example(self,
                           question: str,
                           answer: str,
                           code_context: Optional[str] = None,
                           source_file: Optional[str] = None,
                           pattern_type: Optional[str] = None,
                           verified: bool = False,
                           quality_score: float = 0.5) -> int:
        """
        Add a new training example
        
        Returns:
            ID of the inserted example
        """
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        cur.execute("""
            INSERT INTO smalltalk_training_examples 
            (question, answer, code_context, source_file, pattern_type, 
             verified, quality_score)
            VALUES (%s, %s, %s, %s, %s, %s, %s)
            RETURNING id
        """, (question, answer, code_context, source_file, pattern_type,
              verified, quality_score))
        
        example_id = cur.fetchone()[0]
        conn.commit()
        cur.close()
        conn.close()
        
        return example_id
    
    def add_from_rag_interaction(self,
                               query: str,
                               answer: str,
                               sources: List[str],
                               feedback_score: float) -> int:
        """
        Add training example from RAG interaction
        
        Args:
            query: User's question
            answer: RAG's answer
            sources: Source files used
            feedback_score: User feedback (0-1)
        
        Returns:
            Example ID if added, None if below threshold
        """
        # Only add if feedback is positive
        if feedback_score < 0.7:
            return None
        
        # Extract pattern type from query
        pattern_type = self._identify_pattern(query)
        
        # Create code context from sources
        code_context = f"Sources: {', '.join(sources[:3])}"
        
        # Calculate quality score
        quality_score = feedback_score * 0.8  # Reduce slightly as it's not verified
        
        return self.add_training_example(
            question=query,
            answer=answer,
            code_context=code_context,
            pattern_type=pattern_type,
            verified=False,  # Needs human verification
            quality_score=quality_score
        )
    
    def verify_example(self,
                      example_id: int,
                      verified_by: str,
                      corrected_answer: Optional[str] = None) -> bool:
        """
        Verify a training example (human review)
        
        Args:
            example_id: ID of example to verify
            verified_by: Who verified it
            corrected_answer: If provided, update the answer
        
        Returns:
            Success boolean
        """
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        if corrected_answer:
            cur.execute("""
                UPDATE smalltalk_training_examples
                SET verified = TRUE,
                    verified_by = %s,
                    verified_at = NOW(),
                    answer = %s,
                    quality_score = GREATEST(quality_score, 0.9)
                WHERE id = %s
            """, (verified_by, corrected_answer, example_id))
        else:
            cur.execute("""
                UPDATE smalltalk_training_examples
                SET verified = TRUE,
                    verified_by = %s,
                    verified_at = NOW(),
                    quality_score = GREATEST(quality_score, 0.85)
                WHERE id = %s
            """, (verified_by, example_id))
        
        success = cur.rowcount > 0
        conn.commit()
        cur.close()
        conn.close()
        
        return success
    
    def get_training_batch(self,
                         min_quality_score: float = 0.7,
                         limit: int = 1000,
                         pattern_types: Optional[List[str]] = None) -> List[Dict]:
        """
        Get a batch of training examples
        
        Args:
            min_quality_score: Minimum quality threshold
            limit: Maximum examples to return
            pattern_types: Filter by pattern types
        
        Returns:
            List of training examples
        """
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor(cursor_factory=RealDictCursor)
        
        query = """
            SELECT 
                id,
                question,
                answer,
                code_context,
                pattern_type,
                source_file,
                quality_score
            FROM smalltalk_training_examples
            WHERE verified = TRUE
            AND quality_score >= %s
            AND NOT used_in_training
        """
        
        params = [min_quality_score]
        
        if pattern_types:
            query += " AND pattern_type = ANY(%s)"
            params.append(pattern_types)
        
        query += " ORDER BY quality_score DESC LIMIT %s"
        params.append(limit)
        
        cur.execute(query, params)
        examples = cur.fetchall()
        
        cur.close()
        conn.close()
        
        return examples
    
    def mark_as_used(self, example_ids: List[int], run_id: str):
        """Mark examples as used in training"""
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        cur.execute("""
            UPDATE smalltalk_training_examples
            SET used_in_training = TRUE,
                training_run_id = %s
            WHERE id = ANY(%s)
        """, (run_id, example_ids))
        
        conn.commit()
        cur.close()
        conn.close()
    
    def record_training_run(self,
                          run_id: str,
                          model_name: str,
                          base_model: str,
                          num_examples: int,
                          num_epochs: int,
                          learning_rate: float,
                          output_path: str) -> int:
        """Record the start of a training run"""
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        cur.execute("""
            INSERT INTO lora_training_runs
            (run_id, model_name, base_model, num_examples, num_epochs,
             learning_rate, started_at, status, output_path)
            VALUES (%s, %s, %s, %s, %s, %s, NOW(), 'running', %s)
            RETURNING id
        """, (run_id, model_name, base_model, num_examples, num_epochs,
              learning_rate, output_path))
        
        training_id = cur.fetchone()[0]
        conn.commit()
        cur.close()
        conn.close()
        
        return training_id
    
    def update_training_run(self,
                          run_id: str,
                          final_loss: float,
                          metrics: Dict,
                          status: str = 'completed'):
        """Update training run with results"""
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor()
        
        cur.execute("""
            UPDATE lora_training_runs
            SET final_loss = %s,
                metrics = %s,
                status = %s,
                completed_at = NOW()
            WHERE run_id = %s
        """, (final_loss, json.dumps(metrics), status, run_id))
        
        conn.commit()
        cur.close()
        conn.close()
    
    def get_pattern_statistics(self) -> List[Dict]:
        """Get statistics on training examples by pattern"""
        conn = psycopg2.connect(self.db_url)
        cur = conn.cursor(cursor_factory=RealDictCursor)
        
        cur.execute("""
            SELECT 
                pattern_type,
                COUNT(*) as total,
                COUNT(*) FILTER (WHERE verified = TRUE) as verified,
                COUNT(*) FILTER (WHERE used_in_training = TRUE) as used,
                AVG(quality_score) as avg_quality
            FROM smalltalk_training_examples
            GROUP BY pattern_type
            ORDER BY total DESC
        """)
        
        stats = cur.fetchall()
        cur.close()
        conn.close()
        
        return stats
    
    def export_for_training(self, output_file: str, format: str = "jsonl") -> int:
        """
        Export training data to file
        
        Args:
            output_file: Path to output file
            format: Format (jsonl, alpaca, openai)
        
        Returns:
            Number of examples exported
        """
        examples = self.get_training_batch(limit=10000)
        
        output_path = Path(output_file)
        count = 0
        
        with open(output_path, 'w') as f:
            for example in examples:
                if format == "jsonl":
                    formatted = {
                        "instruction": example['question'],
                        "input": example['code_context'] or "",
                        "output": example['answer'],
                        "type": example['pattern_type']
                    }
                elif format == "openai":
                    formatted = {
                        "messages": [
                            {
                                "role": "system",
                                "content": "You are an expert Smalltalk developer for MgX."
                            },
                            {
                                "role": "user",
                                "content": example['question']
                            },
                            {
                                "role": "assistant",
                                "content": example['answer']
                            }
                        ]
                    }
                else:  # alpaca format
                    formatted = {
                        "instruction": example['question'],
                        "input": example['code_context'] or "",
                        "output": example['answer']
                    }
                
                f.write(json.dumps(formatted) + '\n')
                count += 1
        
        logger.info(f"Exported {count} examples to {output_path}")
        return count
    
    def _identify_pattern(self, query: str) -> str:
        """Identify pattern type from query"""
        query_lower = query.lower()
        
        patterns = {
            'validation': ['validat', 'check', 'verify'],
            'store': ['store', 'persist', 'database'],
            'rights': ['rights', 'license', 'territory'],
            'schedule': ['schedule', 'program', 'broadcast'],
            'test': ['test', 'spec', 'example'],
            'migration': ['migrat', 'convert', 'transform'],
            'class_definition': ['class', 'subclass', 'create'],
            'method': ['method', 'function', 'implement']
        }
        
        for pattern_type, keywords in patterns.items():
            if any(keyword in query_lower for keyword in keywords):
                return pattern_type
        
        return 'general'
    
    def create_review_interface(self) -> str:
        """
        Create a simple Streamlit interface for reviewing examples
        Returns the code as a string
        """
        interface_code = '''
import streamlit as st
import psycopg2
from psycopg2.extras import RealDictCursor

st.title("🎓 Training Data Review Interface")

# Database connection
@st.cache_resource
def get_db_connection():
    return psycopg2.connect(st.secrets["db_url"])

# Get unverified examples
def get_unverified_examples(limit=10):
    conn = get_db_connection()
    cur = conn.cursor(cursor_factory=RealDictCursor)
    
    cur.execute("""
        SELECT id, question, answer, code_context, pattern_type, quality_score
        FROM smalltalk_training_examples
        WHERE verified = FALSE
        ORDER BY quality_score DESC
        LIMIT %s
    """, (limit,))
    
    examples = cur.fetchall()
    cur.close()
    return examples

# Main interface
examples = get_unverified_examples()

if examples:
    for example in examples:
        with st.expander(f"ID: {example['id']} - {example['pattern_type']}"):
            st.write(f"**Question:** {example['question']}")
            st.write(f"**Current Answer:**")
            st.code(example['answer'], language='smalltalk')
            
            col1, col2 = st.columns(2)
            with col1:
                if st.button(f"✅ Verify", key=f"verify_{example['id']}"):
                    # Verify the example
                    pass
            
            with col2:
                corrected = st.text_area(
                    "Corrected Answer (if needed):",
                    value=example['answer'],
                    key=f"correct_{example['id']}"
                )
                if st.button(f"💾 Save Correction", key=f"save_{example['id']}"):
                    # Save correction
                    pass
else:
    st.info("No unverified examples to review!")

# Statistics
st.sidebar.header("📊 Statistics")
# Add pattern statistics here
'''
        return interface_code


# Convenience functions for integration
def setup_postgres_training_store(db_url: str) -> PostgresTrainingStore:
    """Setup and return a training store instance"""
    return PostgresTrainingStore(db_url)


def migrate_rag_feedback_to_training(db_url: str, min_score: float = 0.8):
    """
    Migrate high-quality RAG interactions to training data
    
    This would be run periodically to capture good Q&A pairs
    """
    store = PostgresTrainingStore(db_url)
    conn = psycopg2.connect(db_url)
    cur = conn.cursor(cursor_factory=RealDictCursor)
    
    # Get high-quality RAG interactions
    # (Assuming you have a rag_feedback table from the RAG system)
    cur.execute("""
        SELECT DISTINCT ON (query)
            query,
            answer,
            sources,
            AVG(feedback_score) as avg_score
        FROM rag_feedback
        WHERE feedback_score >= %s
        GROUP BY query, answer, sources
        HAVING COUNT(*) >= 2  -- At least 2 positive feedbacks
        ORDER BY query, avg_score DESC
    """, (min_score,))
    
    migrated = 0
    for row in cur.fetchall():
        example_id = store.add_from_rag_interaction(
            query=row['query'],
            answer=row['answer'],
            sources=row['sources'] or [],
            feedback_score=row['avg_score']
        )
        if example_id:
            migrated += 1
    
    cur.close()
    conn.close()
    
    logger.info(f"Migrated {migrated} high-quality RAG interactions to training data")
    return migrated


if __name__ == "__main__":
    # Example usage
    import os
    
    db_url = os.getenv("TUOKIT_DB_URL", "postgresql://localhost/tuokit")
    store = PostgresTrainingStore(db_url)
    
    # Add example
    example_id = store.add_training_example(
        question="Create a Store subclass for managing streaming rights",
        answer="""StreamingRights subclass: #NetflixStreamingRights
            instanceVariableNames: 'territory startDate endDate contentId'
            classVariableNames: ''
            poolDictionaries: ''
            category: 'MgX-Rights-Streaming'""",
        pattern_type="class_definition",
        quality_score=0.9
    )
    
    print(f"Added example {example_id}")
    
    # Get statistics
    stats = store.get_pattern_statistics()
    print("\nPattern Statistics:")
    for stat in stats:
        print(f"  {stat['pattern_type']}: {stat['total']} total, "
              f"{stat['verified']} verified, {stat['avg_quality']:.2f} avg quality")