"""
Web Scraper for Online Knowledge Bases
Handles documentation sites, wikis, and API docs
"""
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin, urlparse
from pathlib import Path
import time
import logging
from typing import Set, List, Dict
import json

logger = logging.getLogger(__name__)

class KnowledgeBaseScraper:
    """Scrape and process online documentation"""
    
    def __init__(self, output_dir: str = "C:/Projects/TuoKit/scraped_kb"):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.visited_urls: Set[str] = set()
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'TuoKit-RAG-Bot/1.0'
        })
        
    def scrape_site(self, base_url: str, max_pages: int = 100) -> List[str]:
        """Scrape documentation site"""
        logger.info(f"Starting scrape of {base_url}")
        
        # Parse base URL
        parsed_base = urlparse(base_url)
        self.base_domain = f"{parsed_base.scheme}://{parsed_base.netloc}"
        
        # Create domain-specific folder
        domain_folder = self.output_dir / parsed_base.netloc.replace('.', '_')
        domain_folder.mkdir(exist_ok=True)
        
        # Start crawling
        urls_to_visit = [base_url]
        scraped_files = []
        
        while urls_to_visit and len(self.visited_urls) < max_pages:
            url = urls_to_visit.pop(0)
            
            if url in self.visited_urls:
                continue
                
            try:
                # Scrape page
                content, new_urls = self._scrape_page(url)
                if content:
                    # Save content
                    file_path = self._save_content(url, content, domain_folder)
                    scraped_files.append(file_path)
                    
                    # Add new URLs
                    for new_url in new_urls:
                        if new_url not in self.visited_urls:
                            urls_to_visit.append(new_url)
                
                self.visited_urls.add(url)
                time.sleep(0.5)  # Be respectful
                
            except Exception as e:
                logger.error(f"Error scraping {url}: {e}")
        
        logger.info(f"Scraped {len(scraped_files)} pages")
        return scraped_files
    
    def _scrape_page(self, url: str) -> tuple[str, List[str]]:
        """Scrape a single page"""
        response = self.session.get(url, timeout=10)
        response.raise_for_status()
        
        soup = BeautifulSoup(response.text, 'html.parser')
        
        # Extract main content
        content = self._extract_content(soup)
        
        # Find links
        new_urls = []
        for link in soup.find_all('a', href=True):
            href = link['href']
            full_url = urljoin(url, href)
            
            # Only follow internal links
            if self._is_valid_url(full_url):
                new_urls.append(full_url)
        
        return content, new_urls
    
    def _extract_content(self, soup: BeautifulSoup) -> str:
        """Extract main content from page"""
        # Remove script and style elements
        for script in soup(["script", "style"]):
            script.decompose()
        
        # Try to find main content area
        main_selectors = [
            'main', 'article', '[role="main"]', 
            '.content', '#content', '.documentation'
        ]
        
        content_area = None
        for selector in main_selectors:
            content_area = soup.select_one(selector)
            if content_area:
                break
        
        if not content_area:
            content_area = soup.body
        
        if content_area:
            # Extract text with some structure
            lines = []
            for elem in content_area.find_all(['h1', 'h2', 'h3', 'h4', 'p', 'li', 'pre']):
                if elem.name.startswith('h'):
                    lines.append(f"\n{'#' * int(elem.name[1])} {elem.get_text().strip()}\n")
                elif elem.name == 'pre':
                    lines.append(f"```\n{elem.get_text()}\n```")
                else:
                    text = elem.get_text().strip()
                    if text:
                        lines.append(text)
            
            return '\n\n'.join(lines)
        
        return soup.get_text()
    
    def _is_valid_url(self, url: str) -> bool:
        """Check if URL should be scraped"""
        parsed = urlparse(url)
        
        # Only follow same domain
        if not url.startswith(self.base_domain):
            return False
        
        # Skip non-documentation URLs
        skip_patterns = [
            '/login', '/signup', '/api/', '/download',
            '.pdf', '.zip', '.tar', '.gz'
        ]
        
        for pattern in skip_patterns:
            if pattern in url.lower():
                return False
        
        return True
    
    def _save_content(self, url: str, content: str, output_dir: Path) -> str:
        """Save scraped content to file"""
        # Create filename from URL
        parsed = urlparse(url)
        path_parts = parsed.path.strip('/').split('/')
        
        if not path_parts or path_parts == ['']:
            filename = 'index.md'
        else:
            filename = '_'.join(path_parts) + '.md'
        
        file_path = output_dir / filename
        
        # Add metadata
        metadata = {
            'source_url': url,
            'scraped_at': time.strftime('%Y-%m-%d %H:%M:%S'),
            'title': parsed.path.split('/')[-1] or 'index'
        }
        
        # Write content with metadata
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(f"---\n")
            for key, value in metadata.items():
                f.write(f"{key}: {value}\n")
            f.write(f"---\n\n")
            f.write(content)
        
        return str(file_path)
    
    def scrape_api_docs(self, api_base_url: str) -> List[str]:
        """Special handler for API documentation"""
        # TODO: Implement OpenAPI/Swagger parsing
        logger.info("API docs scraping not yet implemented")
        return []
