"""
Unified Vector Knowledge System for TuoKit
Designed for both immediate search and future RAG/LoRA/Fine-tuning

Architecture:
1. Immediate: Vector search for knowledge retrieval
2. Future: Training data preparation for LLM fine-tuning
3. Compatible with SmallTalk codebase indexing
"""

import json
import hashlib
from typing import List, Dict, Optional, Tuple, Any
from datetime import datetime
import psycopg2
from psycopg2.extras import RealDictCursor
import logging
from dataclasses import dataclass
from enum import Enum

from utils.ollama import OllamaClient
from utils.database import DatabaseManager

logger = logging.getLogger(__name__)

class ContentType(Enum):
    """Types of content we index"""
    KNOWLEDGE = "knowledge"          # General knowledge entries
    CODE = "code"                   # Code snippets
    SMALLTALK = "smalltalk"         # SmallTalk specific code
    ERROR = "error"                 # Error patterns and solutions
    DOCUMENTATION = "documentation"  # Documentation entries
    CONVERSATION = "conversation"    # Q&A conversations
    
@dataclass
class IndexedContent:
    """Standardized content for indexing"""
    content_type: ContentType
    title: str
    content: str
    metadata: Dict[str, Any]
    source: Optional[str] = None
    language: Optional[str] = None
    tags: Optional[List[str]] = None

class VectorKnowledgeSystem:
    """
    Unified system for vector search and training data preparation
    Designed to support RAG, LoRA fine-tuning, and specialized model training
    """
    
    def __init__(self, embedding_model: str = "nomic-embed-text"):
        self.db = DatabaseManager()
        self.ollama = OllamaClient()
        self.embedding_model = embedding_model
        self.embedding_dim = 768
        
        # Initialize schema
        self._init_schema()
    
    def _init_schema(self):
        """Initialize comprehensive schema for vector knowledge system"""
        with self.db.get_connection() as conn:
            with conn.cursor() as cur:
                # Main content store with embeddings
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS vector_content (
                        id SERIAL PRIMARY KEY,
                        content_hash VARCHAR(64) UNIQUE NOT NULL,
                        content_type VARCHAR(50) NOT NULL,
                        title TEXT NOT NULL,
                        content TEXT NOT NULL,
                        embedding JSONB,
                        metadata JSONB,
                        source TEXT,
                        language VARCHAR(50),
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    );
                    
                    -- Indexes for efficient retrieval
                    CREATE INDEX IF NOT EXISTS idx_vector_content_type 
                    ON vector_content(content_type);
                    
                    CREATE INDEX IF NOT EXISTS idx_vector_content_language 
                    ON vector_content(language);
                    
                    CREATE INDEX IF NOT EXISTS idx_vector_content_created 
                    ON vector_content(created_at DESC);
                    
                    -- Full text search
                    CREATE INDEX IF NOT EXISTS idx_vector_content_search 
                    ON vector_content USING gin(to_tsvector('english', title || ' ' || content));
                """)
                
                # Tags for categorization
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS content_tags (
                        id SERIAL PRIMARY KEY,
                        content_id INTEGER REFERENCES vector_content(id) ON DELETE CASCADE,
                        tag VARCHAR(100) NOT NULL,
                        UNIQUE(content_id, tag)
                    );
                    
                    CREATE INDEX IF NOT EXISTS idx_content_tags_tag 
                    ON content_tags(tag);
                """)
                
                # Training pairs for fine-tuning
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS training_pairs (
                        id SERIAL PRIMARY KEY,
                        content_id INTEGER REFERENCES vector_content(id),
                        instruction TEXT NOT NULL,
                        input TEXT,
                        output TEXT NOT NULL,
                        model_type VARCHAR(50),  -- 'chat', 'completion', 'code'
                        quality_score FLOAT,     -- For filtering training data
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    );
                    
                    CREATE INDEX IF NOT EXISTS idx_training_pairs_model 
                    ON training_pairs(model_type);
                    
                    CREATE INDEX IF NOT EXISTS idx_training_pairs_quality 
                    ON training_pairs(quality_score DESC);
                """)
                
                # SmallTalk specific storage
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS smalltalk_artifacts (
                        id SERIAL PRIMARY KEY,
                        content_id INTEGER REFERENCES vector_content(id),
                        class_name VARCHAR(255),
                        method_name VARCHAR(255),
                        category VARCHAR(255),
                        package VARCHAR(255),
                        dependencies TEXT[],
                        ast_json JSONB,  -- Abstract syntax tree for deep analysis
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    );
                    
                    CREATE INDEX IF NOT EXISTS idx_smalltalk_class 
                    ON smalltalk_artifacts(class_name);
                    
                    CREATE INDEX IF NOT EXISTS idx_smalltalk_package 
                    ON smalltalk_artifacts(package);
                """)
                
                logger.info("Vector knowledge system schema initialized")
    
    def _compute_hash(self, content: str) -> str:
        """Compute content hash for deduplication"""
        return hashlib.sha256(content.encode()).hexdigest()
    
    def index_content(self, content: IndexedContent) -> Optional[int]:
        """
        Index content for both vector search and training data generation
        Returns content_id if successful
        """
        content_hash = self._compute_hash(content.content)
        
        # Get embedding
        full_text = f"{content.title}\n\n{content.content}"[:2000]
        embedding = self._get_embedding(full_text)
        
        try:
            with self.db.get_connection() as conn:
                with conn.cursor() as cur:
                    # Insert or update content
                    cur.execute("""
                        INSERT INTO vector_content 
                        (content_hash, content_type, title, content, embedding, 
                         metadata, source, language)
                        VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                        ON CONFLICT (content_hash) 
                        DO UPDATE SET 
                            title = EXCLUDED.title,
                            embedding = EXCLUDED.embedding,
                            metadata = EXCLUDED.metadata,
                            updated_at = NOW()
                        RETURNING id
                    """, (
                        content_hash,
                        content.content_type.value,
                        content.title,
                        content.content,
                        json.dumps(embedding) if embedding else None,
                        json.dumps(content.metadata),
                        content.source,
                        content.language
                    ))
                    
                    content_id = cur.fetchone()[0]
                    
                    # Add tags
                    if content.tags:
                        for tag in content.tags:
                            cur.execute("""
                                INSERT INTO content_tags (content_id, tag)
                                VALUES (%s, %s)
                                ON CONFLICT (content_id, tag) DO NOTHING
                            """, (content_id, tag))
                    
                    # Generate training pairs if applicable
                    self._generate_training_pairs(cur, content_id, content)
                    
                    # Handle SmallTalk specific indexing
                    if content.content_type == ContentType.SMALLTALK:
                        self._index_smalltalk_artifact(cur, content_id, content)
                    
                    return content_id
                    
        except Exception as e:
            logger.error(f"Failed to index content: {e}")
            return None
    
    def _generate_training_pairs(self, cursor, content_id: int, 
                                content: IndexedContent):
        """Generate training pairs for fine-tuning"""
        pairs = []
        
        if content.content_type == ContentType.CODE:
            # Code explanation pair
            pairs.append({
                "instruction": "Explain this code",
                "input": content.content,
                "output": content.metadata.get("explanation", ""),
                "model_type": "chat"
            })
            
        elif content.content_type == ContentType.SMALLTALK:
            # SmallTalk specific pairs
            pairs.append({
                "instruction": "Convert this SmallTalk code to Python",
                "input": content.content,
                "output": content.metadata.get("python_equivalent", ""),
                "model_type": "code"
            })
            
        elif content.content_type == ContentType.ERROR:
            # Error solution pairs
            pairs.append({
                "instruction": "How to fix this error",
                "input": content.title,
                "output": content.content,
                "model_type": "chat"
            })
        
        # Insert training pairs
        for pair in pairs:
            if pair["output"]:  # Only if we have output
                cursor.execute("""
                    INSERT INTO training_pairs 
                    (content_id, instruction, input, output, model_type)
                    VALUES (%s, %s, %s, %s, %s)
                """, (content_id, pair["instruction"], pair["input"], 
                      pair["output"], pair["model_type"]))
    
    def _index_smalltalk_artifact(self, cursor, content_id: int, 
                                 content: IndexedContent):
        """Index SmallTalk specific metadata"""
        metadata = content.metadata
        cursor.execute("""
            INSERT INTO smalltalk_artifacts
            (content_id, class_name, method_name, category, package, dependencies)
            VALUES (%s, %s, %s, %s, %s, %s)
        """, (
            content_id,
            metadata.get("class_name"),
            metadata.get("method_name"),
            metadata.get("category"),
            metadata.get("package"),
            metadata.get("dependencies", [])
        ))
    
    def _get_embedding(self, text: str) -> Optional[List[float]]:
        """Get embedding using Ollama"""
        try:
            response = self.ollama.embeddings(
                model=self.embedding_model,
                prompt=text
            )
            return response.get('embedding', [])
        except Exception as e:
            logger.error(f"Failed to get embedding: {e}")
            return None
    
    def search(self, query: str, content_types: Optional[List[ContentType]] = None,
              limit: int = 10, min_similarity: float = 0.5) -> List[Dict]:
        """
        Advanced vector search with filtering
        """
        # For now, use text search with pg_trgm
        # TODO: Implement proper vector similarity when pgvector available
        
        sql = """
            SELECT 
                vc.*,
                similarity(vc.title || ' ' || vc.content, %s) as similarity_score,
                array_agg(DISTINCT ct.tag) as tags
            FROM vector_content vc
            LEFT JOIN content_tags ct ON vc.id = ct.content_id
            WHERE 1=1
        """
        params = [query]
        
        if content_types:
            type_values = [ct.value for ct in content_types]
            sql += " AND vc.content_type = ANY(%s)"
            params.append(type_values)
        
        sql += """
            AND similarity(vc.title || ' ' || vc.content, %s) > %s
            GROUP BY vc.id
            ORDER BY similarity_score DESC
            LIMIT %s
        """
        params.extend([query, min_similarity, limit])
        
        results = self.db.execute_query(sql, tuple(params))
        
        # Format results
        formatted_results = []
        for r in results:
            formatted_results.append({
                'id': r['id'],
                'type': r['content_type'],
                'title': r['title'],
                'content': r['content'][:500] + '...' if len(r['content']) > 500 else r['content'],
                'similarity': r['similarity_score'],
                'tags': r['tags'] or [],
                'metadata': r['metadata'],
                'source': r['source']
            })
        
        return formatted_results
    
    def export_training_data(self, model_type: str = "chat", 
                           min_quality: float = 0.0) -> List[Dict]:
        """Export training data for fine-tuning"""
        sql = """
            SELECT instruction, input, output
            FROM training_pairs
            WHERE model_type = %s AND quality_score >= %s
            ORDER BY quality_score DESC
        """
        
        results = self.db.execute_query(sql, (model_type, min_quality))
        
        # Format for different training frameworks
        formatted_data = []
        for r in results:
            if model_type == "chat":
                # Format for chat models (like ChatGPT fine-tuning)
                formatted_data.append({
                    "messages": [
                        {"role": "system", "content": r['instruction']},
                        {"role": "user", "content": r['input'] or ""},
                        {"role": "assistant", "content": r['output']}
                    ]
                })
            else:
                # Format for completion models
                formatted_data.append({
                    "prompt": f"{r['instruction']}\n{r['input'] or ''}\n",
                    "completion": r['output']
                })
        
        return formatted_data
    
    def prepare_rag_context(self, query: str, max_tokens: int = 2000) -> str:
        """Prepare context for RAG (Retrieval Augmented Generation)"""
        # Search for relevant content
        results = self.search(query, limit=5)
        
        context_parts = []
        token_count = 0
        
        for r in results:
            # Simple token estimation (1 token ≈ 4 chars)
            content_tokens = len(r['content']) // 4
            if token_count + content_tokens > max_tokens:
                break
            
            context_parts.append(f"## {r['title']}\n{r['content']}\n")
            token_count += content_tokens
        
        return "\n".join(context_parts)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get system statistics"""
        stats = {}
        
        with self.db.get_connection() as conn:
            with conn.cursor() as cur:
                # Content statistics
                cur.execute("""
                    SELECT content_type, COUNT(*) as count
                    FROM vector_content
                    GROUP BY content_type
                """)
                stats['content_by_type'] = dict(cur.fetchall())
                
                # Training data statistics
                cur.execute("""
                    SELECT model_type, COUNT(*) as count
                    FROM training_pairs
                    GROUP BY model_type
                """)
                stats['training_pairs_by_type'] = dict(cur.fetchall())
                
                # SmallTalk statistics
                cur.execute("""
                    SELECT COUNT(DISTINCT class_name) as classes,
                           COUNT(DISTINCT package) as packages
                    FROM smalltalk_artifacts
                """)
                smalltalk_stats = cur.fetchone()
                stats['smalltalk'] = {
                    'classes': smalltalk_stats[0],
                    'packages': smalltalk_stats[1]
                }
        
        return stats

# Convenience functions for integration
def index_smalltalk_code(class_name: str, method_name: str, 
                        code: str, metadata: Dict) -> bool:
    """Index SmallTalk code for vector search and training"""
    vks = VectorKnowledgeSystem()
    
    content = IndexedContent(
        content_type=ContentType.SMALLTALK,
        title=f"{class_name}>>{method_name}",
        content=code,
        metadata={
            "class_name": class_name,
            "method_name": method_name,
            **metadata
        },
        language="smalltalk",
        tags=[class_name, "smalltalk", metadata.get("category", "")]
    )
    
    return vks.index_content(content) is not None

def prepare_training_dataset(output_file: str = "tuokit_training.jsonl"):
    """Export training dataset for fine-tuning"""
    vks = VectorKnowledgeSystem()
    
    # Export all training data
    chat_data = vks.export_training_data("chat")
    code_data = vks.export_training_data("code")
    
    # Save to JSONL format
    with open(output_file, 'w') as f:
        for item in chat_data + code_data:
            f.write(json.dumps(item) + '\n')
    
    logger.info(f"Exported {len(chat_data + code_data)} training examples to {output_file}")

if __name__ == "__main__":
    # Test the system
    vks = VectorKnowledgeSystem()
    
    # Example: Index some SmallTalk code
    test_content = IndexedContent(
        content_type=ContentType.SMALLTALK,
        title="OrderedCollection>>add:",
        content="""add: newObject
    "Add newObject to the receiver. Answer newObject."
    
    self addLast: newObject.
    ^newObject""",
        metadata={
            "class_name": "OrderedCollection",
            "method_name": "add:",
            "category": "adding",
            "package": "Collections-Sequenceable"
        },
        language="smalltalk",
        tags=["collection", "adding", "smalltalk"]
    )
    
    content_id = vks.index_content(test_content)
    print(f"Indexed content with ID: {content_id}")
    
    # Test search
    results = vks.search("add object to collection", 
                        content_types=[ContentType.SMALLTALK])
    print(f"\nSearch results: {len(results)}")
    for r in results:
        print(f"- {r['title']} (similarity: {r['similarity']:.3f})")
    
    # Show statistics
    stats = vks.get_statistics()
    print(f"\nSystem statistics: {json.dumps(stats, indent=2)}")