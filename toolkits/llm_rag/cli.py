#!/usr/bin/env python3
"""
TuoKit RAG CLI - Command line interface for batch operations
"""
import click
import sys
from pathlib import Path
import logging

# Add parent directory to path
sys.path.append(str(Path(__file__).parent))

from rag_manager import TuoKitRAGManager

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

@click.group()
def cli():
    """TuoKit RAG System CLI"""
    pass

@cli.command()
@click.option('--config', default='config.yaml', help='Config file path')
def init(config):
    """Initialize the RAG system"""
    click.echo("Initializing TuoKit RAG System...")
    rag = TuoKitRAGManager(config_path=config)
    stats = rag.get_stats()
    click.echo(f"System initialized. Current stats:")
    click.echo(f"  - Indexed files: {stats.get('indexed_files', 0)}")
    click.echo(f"  - Indexed URLs: {stats.get('indexed_urls', 0)}")

@cli.command()
@click.argument('directory')
@click.option('--pattern', default='**/*', help='File pattern to index')
@click.option('--config', default='config.yaml', help='Config file path')
@click.option('--force', is_flag=True, help='Force reindex existing files')
def index_dir(directory, pattern, config, force):
    """Index a directory"""
    click.echo(f"Indexing directory: {directory}")
    click.echo(f"Pattern: {pattern}")
    
    rag = TuoKitRAGManager(config_path=config)
    results = rag.index_directory(directory, pattern, force_reindex=force)
    
    click.echo(f"Results:")
    click.echo(f"  - Files processed: {results['processed']}")
    click.echo(f"  - Chunks created: {results['chunks_created']}")
    if results['errors']:
        click.echo(f"  - Errors: {len(results['errors'])}")

@cli.command()
@click.argument('url')
@click.option('--max-pages', default=50, help='Maximum pages to scrape')
@click.option('--config', default='config.yaml', help='Config file path')
def index_web(url, max_pages, config):
    """Index web documentation"""
    click.echo(f"Indexing web docs from: {url}")
    
    rag = TuoKitRAGManager(config_path=config)
    results = rag.index_web_docs(url, max_pages)
    
    click.echo(f"Results:")
    click.echo(f"  - Pages scraped: {results['pages_scraped']}")
    click.echo(f"  - Chunks created: {results['chunks_created']}")

@cli.command()
@click.argument('query')
@click.option('--top-k', default=5, help='Number of results')
@click.option('--source', help='Filter by source type')
@click.option('--config', default='config.yaml', help='Config file path')
def search(query, top_k, source, config):
    """Search the knowledge base"""
    rag = TuoKitRAGManager(config_path=config)
    results = rag.search(query, top_k=top_k, source_filter=source, use_llm=False)
    
    if results:
        click.echo(f"\nFound {len(results)} results for: '{query}'\n")
        for i, result in enumerate(results):
            click.echo(f"--- Result {i+1} ---")
            click.echo(f"Source: {result['source_type']} - {result['source_path']}")
            click.echo(f"Similarity: {result.get('similarity', 0):.3f}")
            click.echo(f"Content preview:")
            preview = result['content'][:200] + "..." if len(result['content']) > 200 else result['content']
            click.echo(preview)
            click.echo()
    else:
        click.echo("No results found.")

@cli.command()
@click.option('--config', default='config.yaml', help='Config file path')
def stats(config):
    """Show system statistics"""
    rag = TuoKitRAGManager(config_path=config)
    stats = rag.get_stats()
    
    click.echo("\nTuoKit RAG System Statistics:")
    click.echo(f"  - Indexed files: {stats.get('indexed_files', 0)}")
    click.echo(f"  - Indexed URLs: {stats.get('indexed_urls', 0)}")
    
    if 'chunks_by_source' in stats:
        click.echo("\nChunks by source type:")
        for item in stats['chunks_by_source']:
            click.echo(f"  - {item['source_type']}: {item['count']}")

@cli.command()
@click.option('--config', default='config.yaml', help='Config file path')
@click.confirmation_option(prompt='Are you sure you want to clear the index?')
def clear(config):
    """Clear the entire index"""
    rag = TuoKitRAGManager(config_path=config)
    rag.clear_index(confirm=True)
    click.echo("Index cleared successfully.")

@cli.command()
@click.option('--config', default='config.yaml', help='Config file path')
def index_all(config):
    """Index all configured sources"""
    import yaml
    
    # Load config to get sources
    with open(config, 'r') as f:
        cfg = yaml.safe_load(f)
    
    rag = TuoKitRAGManager(config_path=config)
    
    for source in cfg.get('sources', []):
        click.echo(f"\nIndexing {source['name']}...")
        results = rag.index_directory(source['path'], source['pattern'])
        click.echo(f"  - Processed: {results['processed']} files")
        click.echo(f"  - Created: {results['chunks_created']} chunks")

if __name__ == '__main__':
    cli()
