"""
Token counting and management for RAG
Inspired by Anthropic SDK's token tracking
"""

import tiktoken
from typing import Dict, List, Optional, Tuple
import logging
from datetime import datetime

logger = logging.getLogger(__name__)

class TokenManager:
    """
    Manage token counting and limits for RAG operations
    Inspired by Anthropic's usage tracking
    """
    
    def __init__(self, model: str = "deepseek-coder"):
        self.model = model
        self.encoder = self._get_encoder(model)
        
        # Token limits by model (approximate)
        self.model_limits = {
            "deepseek-coder": 16384,
            "deepseek-coder:6.7b": 16384,
            "deepseek-coder:33b": 16384,
            "qwen2.5-coder:32b": 131072,  # 128K
            "yi-coder:9b": 131072,  # 128K
            "phi2": 2048,
            "llama2": 4096,
            "mistral": 32768
        }
        
        # Get limit for current model
        self.context_limit = self._get_model_limit(model)
        self.reserve_tokens = 1000  # Reserve for response
    
    def _get_encoder(self, model: str):
        """Get appropriate tokenizer for the model"""
        try:
            # Try to use the model-specific encoder
            if "gpt" in model.lower():
                return tiktoken.encoding_for_model(model)
            else:
                # Default to cl100k_base for most models
                return tiktoken.get_encoding("cl100k_base")
        except Exception as e:
            logger.warning(f"Could not load encoder for {model}, using default: {e}")
            return tiktoken.get_encoding("cl100k_base")
    
    def _get_model_limit(self, model: str) -> int:
        """Get context limit for model"""
        # Check exact match first
        if model in self.model_limits:
            return self.model_limits[model]
        
        # Check partial match
        for key, limit in self.model_limits.items():
            if key in model or model in key:
                return limit
        
        # Default conservative limit
        return 4096
    
    def count_tokens(self, text: str) -> int:
        """Count tokens in text"""
        try:
            return len(self.encoder.encode(text))
        except Exception as e:
            logger.warning(f"Token counting failed, using estimate: {e}")
            # Fallback: approximate 1 token = 4 characters
            return len(text) // 4
    
    def count_tokens_batch(self, texts: List[str]) -> List[int]:
        """Count tokens for multiple texts efficiently"""
        return [self.count_tokens(text) for text in texts]
    
    def truncate_to_limit(self, text: str, max_tokens: Optional[int] = None) -> str:
        """Truncate text to fit within token limit"""
        limit = max_tokens or (self.context_limit - self.reserve_tokens)
        
        tokens = self.encoder.encode(text)
        if len(tokens) <= limit:
            return text
        
        # Truncate and decode
        truncated_tokens = tokens[:limit]
        return self.encoder.decode(truncated_tokens)
    
    def split_by_tokens(self, text: str, chunk_size: int = 1000, overlap: int = 200) -> List[str]:
        """Split text into token-based chunks with overlap"""
        tokens = self.encoder.encode(text)
        chunks = []
        
        for i in range(0, len(tokens), chunk_size - overlap):
            chunk_tokens = tokens[i:i + chunk_size]
            chunk_text = self.encoder.decode(chunk_tokens)
            chunks.append(chunk_text)
        
        return chunks
    
    def calculate_rag_tokens(self, 
                           query: str,
                           contexts: List[str],
                           system_prompt: Optional[str] = None) -> Dict[str, int]:
        """
        Calculate token usage for a RAG query
        
        Returns:
            Dict with token counts for each component
        """
        counts = {
            'query': self.count_tokens(query),
            'contexts': sum(self.count_tokens(ctx) for ctx in contexts),
            'system': self.count_tokens(system_prompt) if system_prompt else 0,
            'total': 0,
            'available': self.context_limit
        }
        
        counts['total'] = counts['query'] + counts['contexts'] + counts['system']
        counts['remaining'] = counts['available'] - counts['total']
        
        return counts
    
    def optimize_context_selection(self,
                                 query: str,
                                 contexts: List[Dict[str, any]],
                                 system_prompt: Optional[str] = None) -> List[Dict[str, any]]:
        """
        Select contexts that fit within token limit
        
        Args:
            query: User query
            contexts: List of context dicts with 'content' and 'score'
            system_prompt: Optional system prompt
            
        Returns:
            Optimized list of contexts
        """
        # Calculate base tokens
        base_tokens = self.count_tokens(query)
        if system_prompt:
            base_tokens += self.count_tokens(system_prompt)
        
        # Available tokens for contexts
        available_for_contexts = self.context_limit - base_tokens - self.reserve_tokens
        
        # Select contexts greedily by score
        selected = []
        total_context_tokens = 0
        
        # Sort by score (assuming higher is better)
        sorted_contexts = sorted(contexts, key=lambda x: x.get('score', 0), reverse=True)
        
        for ctx in sorted_contexts:
            ctx_tokens = self.count_tokens(ctx['content'])
            
            if total_context_tokens + ctx_tokens <= available_for_contexts:
                selected.append(ctx)
                total_context_tokens += ctx_tokens
            else:
                # Try to fit a truncated version
                remaining_tokens = available_for_contexts - total_context_tokens
                if remaining_tokens > 100:  # Only if we have reasonable space
                    truncated_content = self.truncate_to_limit(ctx['content'], remaining_tokens)
                    ctx_copy = ctx.copy()
                    ctx_copy['content'] = truncated_content
                    ctx_copy['truncated'] = True
                    selected.append(ctx_copy)
                break
        
        return selected
    
    def format_token_usage(self, usage: Dict[str, int]) -> str:
        """Format token usage for display"""
        percentage = (usage['total'] / usage['available'] * 100) if usage['available'] > 0 else 0
        
        return (
            f"Token Usage: {usage['total']:,}/{usage['available']:,} ({percentage:.1f}%)\n"
            f"├─ Query: {usage['query']:,}\n"
            f"├─ Context: {usage['contexts']:,}\n"
            f"├─ System: {usage['system']:,}\n"
            f"└─ Available: {usage['remaining']:,}"
        )


class TokenUsageTracker:
    """Track token usage over time for analytics"""
    
    def __init__(self):
        self.usage_history = []
        self.total_input_tokens = 0
        self.total_output_tokens = 0
    
    def track_query(self, 
                   query: str,
                   input_tokens: int,
                   output_tokens: int,
                   model: str):
        """Track token usage for a query"""
        self.usage_history.append({
            'timestamp': datetime.now(),
            'query': query[:50] + '...' if len(query) > 50 else query,
            'input_tokens': input_tokens,
            'output_tokens': output_tokens,
            'total_tokens': input_tokens + output_tokens,
            'model': model
        })
        
        self.total_input_tokens += input_tokens
        self.total_output_tokens += output_tokens
    
    def get_usage_summary(self) -> Dict:
        """Get usage summary"""
        if not self.usage_history:
            return {
                'total_queries': 0,
                'total_input_tokens': 0,
                'total_output_tokens': 0,
                'total_tokens': 0,
                'average_tokens_per_query': 0
            }
        
        total_tokens = self.total_input_tokens + self.total_output_tokens
        
        return {
            'total_queries': len(self.usage_history),
            'total_input_tokens': self.total_input_tokens,
            'total_output_tokens': self.total_output_tokens,
            'total_tokens': total_tokens,
            'average_tokens_per_query': total_tokens // len(self.usage_history),
            'recent_queries': self.usage_history[-10:]  # Last 10
        }
    
    def estimate_cost(self, input_price_per_1k: float = 0.001, output_price_per_1k: float = 0.002) -> Dict:
        """Estimate cost based on token usage (for cloud models)"""
        input_cost = (self.total_input_tokens / 1000) * input_price_per_1k
        output_cost = (self.total_output_tokens / 1000) * output_price_per_1k
        
        return {
            'input_cost': f"${input_cost:.4f}",
            'output_cost': f"${output_cost:.4f}",
            'total_cost': f"${input_cost + output_cost:.4f}",
            'note': "Estimated for cloud pricing - local models are free!"
        }


# Utility functions for common operations
def create_token_aware_rag_example():
    """Example of token-aware RAG implementation"""
    return '''
# Example: Token-aware RAG search
from toolkits.llm_rag_v2.token_manager import TokenManager

# Initialize token manager
token_mgr = TokenManager(model="deepseek-coder")

# In your RAG search function:
def token_aware_search(query: str, contexts: List[Dict]):
    # Check query tokens
    query_tokens = token_mgr.count_tokens(query)
    if query_tokens > 1000:
        st.warning(f"Query is long ({query_tokens} tokens). Results may be limited.")
    
    # Optimize context selection
    optimized_contexts = token_mgr.optimize_context_selection(
        query=query,
        contexts=contexts,
        system_prompt="You are a helpful assistant."
    )
    
    # Show token usage
    usage = token_mgr.calculate_rag_tokens(
        query=query,
        contexts=[ctx['content'] for ctx in optimized_contexts]
    )
    
    st.sidebar.markdown(token_mgr.format_token_usage(usage))
    
    return optimized_contexts
'''

# Smalltalk-specific tokenization helpers
class SmallTalkTokenizer:
    """Special handling for Smalltalk code tokenization"""
    
    @staticmethod
    def prepare_smalltalk_for_tokenization(code: str) -> str:
        """Prepare Smalltalk code for better tokenization"""
        # Preserve important Smalltalk patterns
        replacements = {
            "ifTrue:": "«IF_TRUE»",
            "ifFalse:": "«IF_FALSE»",
            "whileTrue:": "«WHILE_TRUE»",
            "do:": "«DO»",
            "collect:": "«COLLECT»",
            "select:": "«SELECT»",
            "at:put:": "«AT_PUT»"
        }
        
        prepared = code
        for pattern, replacement in replacements.items():
            prepared = prepared.replace(pattern, replacement)
        
        return prepared
    
    @staticmethod
    def restore_smalltalk_patterns(text: str) -> str:
        """Restore Smalltalk patterns after processing"""
        replacements = {
            "«IF_TRUE»": "ifTrue:",
            "«IF_FALSE»": "ifFalse:",
            "«WHILE_TRUE»": "whileTrue:",
            "«DO»": "do:",
            "«COLLECT»": "collect:",
            "«SELECT»": "select:",
            "«AT_PUT»": "at:put:"
        }
        
        restored = text
        for replacement, pattern in replacements.items():
            restored = restored.replace(replacement, pattern)
        
        return restored