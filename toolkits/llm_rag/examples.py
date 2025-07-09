"""
TuoKit RAG Usage Examples
Simple examples showing how to use the RAG system
"""
from pathlib import Path
import sys

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent.parent))

from toolkits.llm_rag import TuoKitRAGManager

def example_basic_usage():
    """Basic usage example"""
    print("=== Basic RAG Usage ===\n")
    
    # Initialize RAG manager
    rag = TuoKitRAGManager()
    
    # Get current stats
    stats = rag.get_stats()
    print(f"Current indexed files: {stats.get('indexed_files', 0)}")
    
    # Index a single directory
    print("\nIndexing Python files...")
    results = rag.index_directory(
        "C:/Projects/TuoKit/src",
        pattern="**/*.py"
    )
    print(f"Indexed {results['processed']} files")
    
    # Search for something
    print("\nSearching for 'database'...")
    search_results = rag.search("database connection", top_k=3)
    
    for i, result in enumerate(search_results):
        print(f"\nResult {i+1}:")
        print(f"  Source: {result['source_path']}")
        print(f"  Similarity: {result.get('similarity', 0):.2f}")
        print(f"  Preview: {result['content'][:100]}...")

def example_web_scraping():
    """Web scraping example"""
    print("\n=== Web Scraping Example ===\n")
    
    rag = TuoKitRAGManager()
    
    # Scrape and index documentation
    url = "https://docs.python.org/3/tutorial/"
    print(f"Scraping {url} (this may take a while)...")
    
    results = rag.index_web_docs(url, max_pages=10)
    print(f"Scraped {results['pages_scraped']} pages")
    print(f"Created {results['chunks_created']} chunks")

def example_filtered_search():
    """Filtered search example"""
    print("\n=== Filtered Search Example ===\n")
    
    rag = TuoKitRAGManager()
    
    # Search only in code files
    code_results = rag.search(
        "class definition",
        source_filter="code",
        top_k=5
    )
    
    print(f"Found {len(code_results)} code results")
    
    # Search only in documentation
    doc_results = rag.search(
        "installation guide",
        source_filter="documentation",
        top_k=5
    )
    
    print(f"Found {len(doc_results)} documentation results")

def example_batch_indexing():
    """Batch indexing example"""
    print("\n=== Batch Indexing Example ===\n")
    
    rag = TuoKitRAGManager()
    
    # Define multiple sources
    sources = [
        {
            "path": "C:/Projects/TuoKit/src",
            "pattern": "**/*.py",
            "description": "Python source files"
        },
        {
            "path": "C:/Projects/TuoKit/docs",
            "pattern": "**/*.md",
            "description": "Markdown documentation"
        },
        {
            "path": "C:/Projects/TuoKit/config",
            "pattern": "**/*.{yaml,json}",
            "description": "Configuration files"
        }
    ]
    
    # Index each source
    for source in sources:
        print(f"\nIndexing {source['description']}...")
        results = rag.index_directory(
            source['path'],
            source['pattern']
        )
        print(f"  Processed: {results['processed']} files")
        print(f"  Chunks: {results['chunks_created']}")

def example_similarity_threshold():
    """Example showing similarity threshold filtering"""
    print("\n=== Similarity Threshold Example ===\n")
    
    rag = TuoKitRAGManager()
    
    # Search with all results
    all_results = rag.search("error handling", top_k=20)
    
    # Filter by similarity threshold
    threshold = 0.7
    high_quality_results = [
        r for r in all_results 
        if r.get('similarity', 0) > threshold
    ]
    
    print(f"Total results: {len(all_results)}")
    print(f"High quality results (>{threshold}): {len(high_quality_results)}")

if __name__ == "__main__":
    # Run examples
    print("TuoKit RAG System Examples")
    print("=" * 50)
    
    # Choose which example to run
    examples = {
        "1": ("Basic Usage", example_basic_usage),
        "2": ("Web Scraping", example_web_scraping),
        "3": ("Filtered Search", example_filtered_search),
        "4": ("Batch Indexing", example_batch_indexing),
        "5": ("Similarity Threshold", example_similarity_threshold)
    }
    
    print("\nAvailable examples:")
    for key, (name, _) in examples.items():
        print(f"  {key}. {name}")
    
    choice = input("\nSelect example (1-5) or 'all' for all examples: ")
    
    if choice == 'all':
        for name, func in examples.values():
            func()
    elif choice in examples:
        examples[choice][1]()
    else:
        print("Invalid choice")
