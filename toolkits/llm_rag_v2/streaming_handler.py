"""
Streaming response handler for RAG
Inspired by Anthropic SDK's streaming pattern
"""

import asyncio
import logging
from typing import AsyncGenerator, Dict, Optional, Callable
from datetime import datetime
import ollama

logger = logging.getLogger(__name__)

class StreamingRAGHandler:
    """
    Handle streaming responses from Ollama
    Inspired by Anthropic SDK's event-based streaming
    """
    
    def __init__(self):
        self.buffer = []
        self.total_tokens = 0
        self.start_time = None
        self.callbacks = {
            'on_start': None,
            'on_token': None,
            'on_complete': None,
            'on_error': None
        }
    
    def on(self, event: str, callback: Callable):
        """Register event callbacks like Anthropic SDK"""
        if event in self.callbacks:
            self.callbacks[event] = callback
        return self
    
    async def stream_answer(self, 
                          prompt: str,
                          model: str = "deepseek-coder:6.7b",
                          context: Optional[str] = None) -> AsyncGenerator[Dict, None]:
        """
        Stream answer generation with context
        
        Yields:
            Dict with 'text', 'token_count', 'done' fields
        """
        self.start_time = datetime.now()
        self.buffer = []
        self.total_tokens = 0
        
        # Prepare the full prompt with context
        if context:
            full_prompt = f"""Context:
{context}

Question: {prompt}

Answer:"""
        else:
            full_prompt = prompt
        
        # Call start callback
        if self.callbacks['on_start']:
            self.callbacks['on_start']({'prompt': prompt, 'model': model})
        
        try:
            # Stream from Ollama
            async for chunk in ollama.AsyncClient().generate(
                model=model,
                prompt=full_prompt,
                stream=True
            ):
                text = chunk.get('response', '')
                self.buffer.append(text)
                self.total_tokens += 1
                
                # Yield chunk data
                chunk_data = {
                    'text': text,
                    'token_count': self.total_tokens,
                    'done': chunk.get('done', False),
                    'elapsed': (datetime.now() - self.start_time).total_seconds()
                }
                
                # Call token callback
                if self.callbacks['on_token']:
                    self.callbacks['on_token'](chunk_data)
                
                yield chunk_data
                
                if chunk.get('done', False):
                    # Call complete callback
                    if self.callbacks['on_complete']:
                        self.callbacks['on_complete']({
                            'full_text': ''.join(self.buffer),
                            'total_tokens': self.total_tokens,
                            'duration': (datetime.now() - self.start_time).total_seconds()
                        })
                    break
                    
        except Exception as e:
            logger.error(f"Streaming error: {e}")
            if self.callbacks['on_error']:
                self.callbacks['on_error']({'error': str(e)})
            raise
    
    def get_full_response(self) -> str:
        """Get the complete buffered response"""
        return ''.join(self.buffer)
    
    def get_stats(self) -> Dict:
        """Get streaming statistics"""
        duration = (datetime.now() - self.start_time).total_seconds() if self.start_time else 0
        return {
            'total_tokens': self.total_tokens,
            'duration_seconds': duration,
            'tokens_per_second': self.total_tokens / duration if duration > 0 else 0,
            'response_length': len(self.get_full_response())
        }


class StreamingRAGPipeline:
    """
    Complete streaming RAG pipeline
    Combines retrieval with streaming generation
    """
    
    def __init__(self, rag_instance):
        self.rag = rag_instance
        self.streaming_handler = StreamingRAGHandler()
    
    async def query_stream(self,
                         query: str,
                         top_k: int = 5,
                         query_type: str = "general") -> AsyncGenerator[Dict, None]:
        """
        Perform RAG search and stream the answer
        
        Args:
            query: User's question
            top_k: Number of documents to retrieve
            query_type: Type of query for prompting
            
        Yields:
            Streaming response chunks
        """
        # First, retrieve relevant documents
        search_results = self.rag.search(query, top_k=top_k)
        
        if not search_results:
            yield {
                'text': "I couldn't find relevant information in the knowledge base.",
                'done': True,
                'error': 'no_results'
            }
            return
        
        # Build context from search results
        context_parts = []
        for i, result in enumerate(search_results[:top_k]):
            context_parts.append(f"[Document {i+1}] {result['source_path']}:\n{result['content']}\n")
        
        context = "\n---\n".join(context_parts)
        
        # Get appropriate prompt template
        system_prompt = self._get_prompt_template(query_type)
        
        # Stream the answer
        full_prompt = f"{system_prompt}\n\n{query}"
        
        async for chunk in self.streaming_handler.stream_answer(
            prompt=full_prompt,
            context=context
        ):
            yield chunk
    
    def _get_prompt_template(self, query_type: str) -> str:
        """Get appropriate system prompt"""
        templates = {
            "general": "You are a helpful assistant. Answer based on the provided context.",
            "code_explanation": "You are a code expert. Explain the code clearly based on the context.",
            "debugging": "You are a debugging expert. Help solve the issue based on the context.",
            "implementation": "You are a senior developer. Provide implementation guidance based on the context."
        }
        return templates.get(query_type, templates["general"])
    
    def register_callbacks(self, **callbacks):
        """Register streaming callbacks"""
        for event, callback in callbacks.items():
            self.streaming_handler.on(event, callback)
        return self


# Synchronous wrapper for Streamlit
class StreamingRAGSync:
    """Synchronous wrapper for use in Streamlit"""
    
    def __init__(self, rag_instance):
        self.pipeline = StreamingRAGPipeline(rag_instance)
    
    def query_stream_sync(self, query: str, **kwargs):
        """Synchronous streaming for Streamlit"""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        
        async def _stream():
            async for chunk in self.pipeline.query_stream(query, **kwargs):
                yield chunk
        
        try:
            # Create async generator
            gen = _stream()
            
            # Yield from async generator synchronously
            while True:
                try:
                    chunk = loop.run_until_complete(gen.__anext__())
                    yield chunk
                except StopAsyncIteration:
                    break
        finally:
            loop.close()


# Example usage for Streamlit
def create_streamlit_example():
    """Example code for Streamlit integration"""
    return '''
# In your Streamlit page:
import streamlit as st
from toolkits.llm_rag_v2.streaming_handler import StreamingRAGSync

# Initialize streaming RAG
streaming_rag = StreamingRAGSync(st.session_state.rag_instance)

# Create a container for streaming output
if st.button("Search with Streaming"):
    response_container = st.empty()
    full_response = ""
    token_count = 0
    
    # Stream the response
    for chunk in streaming_rag.query_stream_sync(query, top_k=5):
        if chunk.get('text'):
            full_response += chunk['text']
            token_count = chunk.get('token_count', 0)
            
            # Update the container with formatted response
            response_container.markdown(
                f"{full_response}\n\n"
                f"*Tokens: {token_count} | "
                f"Time: {chunk.get('elapsed', 0):.1f}s*"
            )
        
        if chunk.get('done'):
            st.success("✓ Response complete!")
            break
'''