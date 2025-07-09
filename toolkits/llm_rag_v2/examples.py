"""
Examples of using the enhanced RAG features
Shows integration of all the new components
"""

def basic_usage_example():
    """Basic RAG usage with enhancements"""
    return '''
# Basic Usage with Enhanced Features
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG

# Initialize RAG
rag = TuoKitRAG()

# 1. Token-aware search
results = rag.search("How does crash analyzer work?", top_k=5)
print(f"Found {len(results)} results")

# 2. Generate answer with token tracking
response = rag.generate_answer(
    query="Explain the crash analyzer architecture",
    query_type="code_explanation",
    top_k=5
)

# Show token usage
token_usage = response['token_usage']
print(f"Tokens used: {token_usage['total']}/{token_usage['available']}")
print(f"Answer: {response['answer'][:200]}...")
'''

def streaming_example():
    """Streaming response example"""
    return '''
# Streaming Responses
import streamlit as st
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG

rag = TuoKitRAG()

# Generate streaming answer
response = rag.generate_answer(
    query="How to implement a Smalltalk Store subclass?",
    query_type="implementation",
    stream=True  # Enable streaming
)

# Display streaming response in Streamlit
if response.get('stream'):
    response_container = st.empty()
    full_response = ""
    
    for chunk in response['stream']:
        if chunk.get('text'):
            full_response += chunk['text']
            response_container.markdown(full_response)
        
        if chunk.get('done'):
            st.success("✓ Response complete!")
            break

# Show token usage
st.sidebar.metric("Tokens Used", 
    f"{response['token_usage']['total']}/{response['token_usage']['available']}")
'''

def declarative_pipeline_example():
    """DSPy-style declarative pipeline example"""
    return '''
# Declarative RAG Pipelines (DSPy-style)
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG

rag = TuoKitRAG()

# 1. Use pre-built Smalltalk pipeline
smalltalk_pipeline = rag.create_declarative_pipeline("smalltalk")
result = smalltalk_pipeline.forward(
    query="Create a Store subclass for streaming rights",
    language="smalltalk"
)
print(result['answer'])

# 2. Use debugging pipeline
debug_pipeline = rag.create_declarative_pipeline("debug")
result = debug_pipeline.forward(
    error_message="MessageNotUnderstood: #streamingRights",
    code_context="Store subclass: #StreamingStore..."
)

# 3. Optimize pipeline with examples
training_examples = [
    {
        'query': 'Create Store subclass',
        'expected_contains': 'subclass:'
    },
    {
        'query': 'Debug MessageNotUnderstood',
        'expected_contains': 'doesNotUnderstand:'
    }
]

# Optimize the pipeline
optimization_results = smalltalk_pipeline.optimize(training_examples)
print(f"Performance improved by {optimization_results['improvement']:.1f}%")
'''

def error_handling_example():
    """Error handling example"""
    return '''
# Enhanced Error Handling
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG
from toolkits.llm_rag_v2.error_handling import (
    TokenLimitError, RetrievalError, NoResultsError
)
import streamlit as st

rag = TuoKitRAG()

try:
    # Search with automatic error handling
    results = rag.search("very long query " * 1000)  # Exceeds token limit
    
except TokenLimitError as e:
    st.error(f"❌ {e.user_friendly_message()}")
    
    # Offer to truncate
    if st.button("Truncate and retry"):
        truncated_query = rag.token_manager.truncate_to_limit(query, max_tokens=2048)
        results = rag.search(truncated_query)

except RetrievalError as e:
    st.error(f"❌ {e.user_friendly_message()}")
    
    # Fallback to simpler search
    st.info("Trying alternative search...")
    results = rag.search(query, top_k=3)  # Fewer results

except NoResultsError as e:
    st.warning(f"⚠️ {e.user_friendly_message()}")
    
    # Suggest alternatives
    suggestions = rag.get_query_suggestions(query[:10])
    if suggestions:
        st.info("Try these queries:")
        for suggestion in suggestions:
            if st.button(suggestion):
                results = rag.search(suggestion)
'''

def token_optimization_example():
    """Token optimization example"""
    return '''
# Token Optimization for Long Contexts
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG
import streamlit as st

rag = TuoKitRAG()

# Search for many documents
all_results = rag.search("TuoKit architecture", top_k=20)

# Show token usage before optimization
contexts = [r['content'] for r in all_results]
initial_tokens = rag.token_manager.calculate_rag_tokens(
    query="Explain TuoKit architecture",
    contexts=contexts
)
st.write(f"Initial: {initial_tokens['total']} tokens (exceeds limit!)")

# Optimize context selection
optimized_results = rag.token_manager.optimize_context_selection(
    query="Explain TuoKit architecture",
    contexts=all_results,
    system_prompt="You are a helpful assistant."
)

# Show optimized token usage
optimized_contexts = [r['content'] for r in optimized_results]
final_tokens = rag.token_manager.calculate_rag_tokens(
    query="Explain TuoKit architecture",
    contexts=optimized_contexts
)

st.write(f"Optimized: {final_tokens['total']} tokens")
st.write(f"Selected {len(optimized_results)} out of {len(all_results)} documents")

# Generate answer with optimized contexts
response = rag.generate_answer(
    query="Explain TuoKit architecture",
    top_k=len(optimized_results)
)
'''

def learning_tracker_example():
    """Learning from feedback example"""
    return '''
# Learning from User Feedback
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG
import streamlit as st

rag = TuoKitRAG()

# Search and display results
query = st.text_input("Search query")
if query:
    results = rag.search(query, top_k=5)
    
    for i, result in enumerate(results):
        with st.expander(f"Result {i+1}: {result['source_path']}"):
            st.write(result['content'])
            
            # Feedback buttons
            col1, col2, col3 = st.columns(3)
            
            with col1:
                if st.button("👍 Helpful", key=f"help_{i}"):
                    rag.mark_helpful(result['result_id'])
                    st.success("Thanks for feedback!")
            
            with col2:
                if st.button("👎 Not helpful", key=f"nhelp_{i}"):
                    rag.mark_not_helpful(result['result_id'])
                    st.info("We'll improve!")
            
            with col3:
                if st.button("📋 Copy", key=f"copy_{i}"):
                    rag.track_interaction(result['result_id'], 'copy')
                    st.code(result['content'])
    
    # Show learning stats
    with st.sidebar:
        st.subheader("Learning Stats")
        stats = rag.get_stats()
        learning = stats.get('learning', {})
        
        st.metric("Total Queries", learning.get('total_queries', 0))
        st.metric("Feedback Given", learning.get('total_feedback', 0))
        
        # Popular queries
        popular = rag.get_popular_queries(5)
        if popular:
            st.write("Popular Queries:")
            for q in popular:
                st.write(f"• {q}")
'''

def smalltalk_tokenization_example():
    """Smalltalk-specific tokenization example"""
    return '''
# Smalltalk-Specific Token Handling
from toolkits.llm_rag_v2.token_manager import SmallTalkTokenizer
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG

rag = TuoKitRAG()

# Smalltalk code that might confuse tokenizers
smalltalk_code = """
Store subclass: #StreamingStore
    instanceVariableNames: 'rights platform'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Streaming-Core'!

!StreamingStore methodsFor: 'accessing'!

streamingRights
    "Return streaming rights"
    ^rights ifNil: [rights := Dictionary new]!

at: key put: value
    "Store streaming right"
    self streamingRights at: key put: value!
"""

# Prepare for better tokenization
prepared = SmallTalkTokenizer.prepare_smalltalk_for_tokenization(smalltalk_code)

# Count tokens accurately
token_count = rag.token_manager.count_tokens(prepared)
print(f"Smalltalk code tokens: {token_count}")

# Search with Smalltalk-aware processing
results = rag.search(prepared, top_k=5)

# Restore patterns in results
for result in results:
    result['content'] = SmallTalkTokenizer.restore_smalltalk_patterns(
        result['content']
    )
'''

def complete_integration_example():
    """Complete integration example"""
    return '''
# Complete Enhanced RAG Integration
import streamlit as st
from toolkits.llm_rag_v2.rag_manager import TuoKitRAG
from toolkits.llm_rag_v2.error_handling import handle_rag_errors

# Initialize RAG with all enhancements
@st.cache_resource
def get_rag():
    return TuoKitRAG()

rag = get_rag()

# UI with all features
st.title("Enhanced RAG Knowledge Base")

# Query input with suggestions
query = st.text_input("Ask about TuoKit...")
if query:
    # Show suggestions
    suggestions = rag.get_query_suggestions(query[:10])
    if suggestions:
        st.write("Suggestions:", ", ".join(suggestions))

# Query type selection
query_type = st.selectbox(
    "Query Type",
    ["general", "code_explanation", "debugging", "implementation"]
)

# Advanced options
with st.expander("Advanced Options"):
    top_k = st.slider("Number of results", 1, 20, 5)
    use_streaming = st.checkbox("Enable streaming", value=True)
    show_tokens = st.checkbox("Show token usage", value=True)
    use_declarative = st.checkbox("Use declarative pipeline", value=False)

# Search button
if st.button("Search", type="primary"):
    with st.spinner("Searching..."):
        try:
            if use_declarative:
                # Use declarative pipeline
                pipeline = rag.create_declarative_pipeline(query_type)
                result = pipeline.forward(query=query)
                
                st.write("### Answer")
                st.write(result['answer'])
                
            else:
                # Regular search with streaming
                response = rag.generate_answer(
                    query=query,
                    query_type=query_type,
                    top_k=top_k,
                    stream=use_streaming
                )
                
                if use_streaming and response.get('stream'):
                    # Streaming display
                    response_container = st.empty()
                    full_response = ""
                    
                    for chunk in response['stream']:
                        if chunk.get('text'):
                            full_response += chunk['text']
                            response_container.markdown(full_response)
                else:
                    # Regular display
                    st.write("### Answer")
                    st.write(response['answer'])
                
                # Show token usage
                if show_tokens and 'token_usage' in response:
                    usage = response['token_usage']
                    col1, col2, col3 = st.columns(3)
                    
                    with col1:
                        st.metric("Query Tokens", usage['query'])
                    with col2:
                        st.metric("Context Tokens", usage['contexts'])
                    with col3:
                        st.metric("Total/Limit", 
                            f"{usage['total']}/{usage['available']}")
                    
                    # Token usage bar
                    progress = usage['total'] / usage['available']
                    st.progress(progress)
                
                # Show sources with feedback
                if response.get('sources'):
                    st.write("### Sources")
                    for i, source in enumerate(response['sources']):
                        with st.expander(f"{source['source_path']}"):
                            st.code(source['content'])
                            
                            # Feedback
                            col1, col2 = st.columns(2)
                            with col1:
                                if st.button("👍", key=f"h_{i}"):
                                    rag.mark_helpful(source['result_id'])
                            with col2:
                                if st.button("👎", key=f"nh_{i}"):
                                    rag.mark_not_helpful(source['result_id'])
                
        except Exception as e:
            st.error(f"Error: {str(e)}")

# Sidebar stats
with st.sidebar:
    st.subheader("System Stats")
    stats = rag.get_stats()
    
    st.metric("Indexed Files", stats.get('indexed_files', 0))
    st.metric("Total Queries", stats['learning']['total_queries'])
    
    if st.button("Refresh Stats"):
        st.rerun()
'''

# Export all examples
ALL_EXAMPLES = {
    "basic_usage": basic_usage_example,
    "streaming": streaming_example,
    "declarative_pipeline": declarative_pipeline_example,
    "error_handling": error_handling_example,
    "token_optimization": token_optimization_example,
    "learning_tracker": learning_tracker_example,
    "smalltalk_tokenization": smalltalk_tokenization_example,
    "complete_integration": complete_integration_example
}

if __name__ == "__main__":
    print("Available examples:")
    for name, func in ALL_EXAMPLES.items():
        print(f"- {name}")
    
    print("\nExample usage:")
    print(basic_usage_example())