"""
DSPy-inspired declarative RAG pipeline
No manual prompt engineering - just declare what you want!
"""

import logging
from typing import List, Dict, Optional, Any, Callable
from dataclasses import dataclass, field
from abc import ABC, abstractmethod

logger = logging.getLogger(__name__)

# DSPy-style signatures
@dataclass
class Signature:
    """Define input/output structure for RAG components"""
    pass

@dataclass
class RAGSignature(Signature):
    """Standard RAG signature"""
    query: str = field(metadata={"desc": "User's question"})
    contexts: List[str] = field(default_factory=list, metadata={"desc": "Retrieved documents"})
    answer: str = field(default="", metadata={"desc": "Generated answer"})

@dataclass
class DebugSignature(Signature):
    """Debugging-specific signature"""
    error_message: str = field(metadata={"desc": "The error to debug"})
    code_context: str = field(metadata={"desc": "Surrounding code"})
    stack_trace: Optional[str] = field(default=None, metadata={"desc": "Stack trace if available"})
    solution: str = field(default="", metadata={"desc": "Debugging solution"})

@dataclass
class CodeGenSignature(Signature):
    """Code generation signature"""
    specification: str = field(metadata={"desc": "What to implement"})
    language: str = field(default="python", metadata={"desc": "Target language"})
    style_examples: List[str] = field(default_factory=list, metadata={"desc": "Code style examples"})
    generated_code: str = field(default="", metadata={"desc": "Generated code"})


# DSPy-style modules
class RAGModule(ABC):
    """Base class for RAG modules"""
    
    def __init__(self, signature: Signature):
        self.signature = signature
        self.metrics = {}
    
    @abstractmethod
    def forward(self, **kwargs) -> Dict[str, Any]:
        """Execute the module"""
        pass
    
    def optimize(self, examples: List[Dict], metric: Callable) -> None:
        """Optimize module parameters based on examples"""
        pass


class Retrieve(RAGModule):
    """Declarative retrieval module"""
    
    def __init__(self, k: int = 5, method: str = "hybrid"):
        super().__init__(RAGSignature())
        self.k = k
        self.method = method
        self.retriever = None  # Set by pipeline
    
    def forward(self, query: str) -> List[Dict]:
        """Retrieve relevant documents"""
        if not self.retriever:
            raise ValueError("Retriever not initialized")
        
        results = self.retriever.search(query, top_k=self.k)
        
        # Track metrics
        self.metrics['retrieved_count'] = len(results)
        self.metrics['avg_score'] = sum(r.get('score', 0) for r in results) / len(results) if results else 0
        
        return results


class Rerank(RAGModule):
    """Declarative reranking module"""
    
    def __init__(self, top_k: int = 3, threshold: float = 0.5):
        super().__init__(RAGSignature())
        self.top_k = top_k
        self.threshold = threshold
    
    def forward(self, query: str, documents: List[Dict]) -> List[Dict]:
        """Rerank documents by relevance"""
        # Filter by threshold
        filtered = [doc for doc in documents if doc.get('score', 0) >= self.threshold]
        
        # Take top k
        reranked = sorted(filtered, key=lambda x: x.get('score', 0), reverse=True)[:self.top_k]
        
        # Track metrics
        self.metrics['filtered_count'] = len(filtered)
        self.metrics['reranked_count'] = len(reranked)
        
        return reranked


class Generate(RAGModule):
    """Declarative generation module"""
    
    def __init__(self, model: str = "deepseek-coder", temperature: float = 0.7):
        super().__init__(RAGSignature())
        self.model = model
        self.temperature = temperature
        self.generator = None  # Set by pipeline
    
    def forward(self, query: str, contexts: List[Dict]) -> str:
        """Generate answer based on contexts"""
        if not self.generator:
            raise ValueError("Generator not initialized")
        
        # Build context string
        context_str = "\n---\n".join([doc['content'] for doc in contexts])
        
        # Generate answer
        response = self.generator.generate_answer(
            query=query,
            context=context_str,
            temperature=self.temperature
        )
        
        # Track metrics
        self.metrics['response_length'] = len(response)
        
        return response


class DeclarativeRAGPipeline:
    """
    DSPy-style declarative RAG pipeline
    Compose modules without manual prompting
    """
    
    def __init__(self, rag_instance):
        self.rag = rag_instance
        self.modules = []
        self.signature = None
        self.metrics_history = []
    
    def add_module(self, module: RAGModule):
        """Add a module to the pipeline"""
        self.modules.append(module)
        
        # Set up module dependencies
        if isinstance(module, Retrieve):
            module.retriever = self.rag
        elif isinstance(module, Generate):
            module.generator = self.rag
        
        return self
    
    def compile(self, signature: Signature = None):
        """Compile the pipeline with a signature"""
        self.signature = signature or RAGSignature()
        
        # Validate module compatibility
        for module in self.modules:
            if not isinstance(module.signature, type(self.signature)):
                logger.warning(f"Module {module.__class__.__name__} signature mismatch")
        
        return self
    
    def forward(self, **inputs) -> Dict[str, Any]:
        """Execute the pipeline"""
        # Initialize with inputs
        state = inputs.copy()
        
        # Execute each module in sequence
        for module in self.modules:
            if isinstance(module, Retrieve):
                state['documents'] = module.forward(query=state.get('query', ''))
            
            elif isinstance(module, Rerank):
                state['documents'] = module.forward(
                    query=state.get('query', ''),
                    documents=state.get('documents', [])
                )
            
            elif isinstance(module, Generate):
                state['answer'] = module.forward(
                    query=state.get('query', ''),
                    contexts=state.get('documents', [])
                )
        
        # Collect metrics
        pipeline_metrics = {
            'modules': [m.__class__.__name__ for m in self.modules],
            'module_metrics': {m.__class__.__name__: m.metrics for m in self.modules}
        }
        self.metrics_history.append(pipeline_metrics)
        
        return state
    
    def optimize(self, examples: List[Dict], metric: Callable = None):
        """
        Optimize pipeline based on examples
        This is where DSPy magic happens - automatic prompt/parameter optimization
        """
        if not metric:
            # Default metric: answer quality
            def default_metric(prediction, example):
                return len(prediction.get('answer', '')) > 0
            metric = default_metric
        
        # Collect baseline performance
        baseline_scores = []
        for example in examples:
            result = self.forward(**example)
            score = metric(result, example)
            baseline_scores.append(score)
        
        baseline_avg = sum(baseline_scores) / len(baseline_scores)
        logger.info(f"Baseline performance: {baseline_avg:.3f}")
        
        # Optimize each module
        for module in self.modules:
            if hasattr(module, 'optimize'):
                module.optimize(examples, metric)
        
        # Test optimized performance
        optimized_scores = []
        for example in examples:
            result = self.forward(**example)
            score = metric(result, example)
            optimized_scores.append(score)
        
        optimized_avg = sum(optimized_scores) / len(optimized_scores)
        improvement = (optimized_avg - baseline_avg) / baseline_avg * 100
        
        logger.info(f"Optimized performance: {optimized_avg:.3f} ({improvement:+.1f}%)")
        
        return {
            'baseline': baseline_avg,
            'optimized': optimized_avg,
            'improvement': improvement
        }


# Pre-built pipelines for common tasks
class SmallTalkRAG(DeclarativeRAGPipeline):
    """Pre-configured pipeline for Smalltalk code questions"""
    
    def __init__(self, rag_instance):
        super().__init__(rag_instance)
        
        # Add modules in order
        self.add_module(Retrieve(k=10, method="hybrid"))
        self.add_module(Rerank(top_k=5, threshold=0.6))
        self.add_module(Generate(model="deepseek-coder", temperature=0.3))
        
        # Compile with appropriate signature
        self.compile(CodeGenSignature())


class DebugRAG(DeclarativeRAGPipeline):
    """Pre-configured pipeline for debugging"""
    
    def __init__(self, rag_instance):
        super().__init__(rag_instance)
        
        # Debugging needs more context
        self.add_module(Retrieve(k=15, method="hybrid"))
        self.add_module(Rerank(top_k=7, threshold=0.5))
        self.add_module(Generate(model="deepseek-coder", temperature=0.5))
        
        self.compile(DebugSignature())


# Advanced: Self-improving module
class AdaptiveRerank(Rerank):
    """Reranking module that learns from feedback"""
    
    def __init__(self, top_k: int = 3):
        super().__init__(top_k=top_k)
        self.score_adjustments = {}  # Learn score adjustments
        self.feedback_history = []
    
    def record_feedback(self, doc_id: str, was_helpful: bool):
        """Record whether a document was helpful"""
        self.feedback_history.append({
            'doc_id': doc_id,
            'helpful': was_helpful
        })
        
        # Update score adjustments
        if doc_id not in self.score_adjustments:
            self.score_adjustments[doc_id] = 0.0
        
        # Simple learning: boost helpful docs, penalize unhelpful
        adjustment = 0.1 if was_helpful else -0.1
        self.score_adjustments[doc_id] += adjustment
    
    def forward(self, query: str, documents: List[Dict]) -> List[Dict]:
        """Rerank with learned adjustments"""
        # Apply learned adjustments
        for doc in documents:
            doc_id = doc.get('source_path', '')
            if doc_id in self.score_adjustments:
                doc['score'] = doc.get('score', 0) + self.score_adjustments[doc_id]
        
        # Continue with normal reranking
        return super().forward(query, documents)
    
    def optimize(self, examples: List[Dict], metric: Callable):
        """Learn optimal threshold from examples"""
        best_threshold = self.threshold
        best_score = 0
        
        # Try different thresholds
        for threshold in [0.3, 0.4, 0.5, 0.6, 0.7]:
            self.threshold = threshold
            scores = []
            
            for example in examples:
                docs = example.get('documents', [])
                reranked = self.forward(example['query'], docs)
                score = metric({'documents': reranked}, example)
                scores.append(score)
            
            avg_score = sum(scores) / len(scores)
            if avg_score > best_score:
                best_score = avg_score
                best_threshold = threshold
        
        self.threshold = best_threshold
        logger.info(f"Optimized threshold: {best_threshold}")


# Example usage
def create_declarative_rag_example():
    """Example of using declarative RAG"""
    return '''
# Example: Declarative RAG pipeline
from toolkits.llm_rag_v2.declarative_rag import SmallTalkRAG, DebugRAG

# Initialize for Smalltalk questions
smalltalk_rag = SmallTalkRAG(rag_instance)

# Just call forward - no prompt engineering needed!
result = smalltalk_rag.forward(
    query="How do I create a Store subclass for streaming rights?",
    language="smalltalk"
)

print(result['answer'])

# Or use debugging pipeline
debug_rag = DebugRAG(rag_instance)
result = debug_rag.forward(
    error_message="MessageNotUnderstood: #streamingRights",
    code_context=my_code
)

# Optimize based on user feedback
training_examples = [
    {
        'query': 'Create Store subclass',
        'expected_answer_contains': 'subclass:'
    },
    # ... more examples
]

debug_rag.optimize(training_examples)
'''