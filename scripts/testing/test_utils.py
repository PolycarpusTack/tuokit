"""
Test Utilities for TuoKit
Common helpers, fixtures, and mocks for all tests
"""

import os
import sys
import json
import sqlite3
import tempfile
import shutil
from typing import Dict, List, Any, Optional
from contextlib import contextmanager
from unittest.mock import Mock, MagicMock, patch

# Mock Streamlit for non-UI tests
class MockStreamlit:
    """Mock Streamlit module for testing"""
    
    class session_state:
        _state = {}
        
        @classmethod
        def get(cls, key, default=None):
            return cls._state.get(key, default)
        
        @classmethod
        def set(cls, key, value):
            cls._state[key] = value
        
        def __setattr__(self, key, value):
            self._state[key] = value
        
        def __getattr__(self, key):
            return self._state.get(key)
    
    @staticmethod
    def title(text):
        print(f"Title: {text}")
    
    @staticmethod
    def header(text):
        print(f"Header: {text}")
    
    @staticmethod
    def subheader(text):
        print(f"Subheader: {text}")
    
    @staticmethod
    def write(text):
        print(f"Write: {text}")
    
    @staticmethod
    def markdown(text):
        print(f"Markdown: {text}")
    
    @staticmethod
    def info(text):
        print(f"Info: {text}")
    
    @staticmethod
    def success(text):
        print(f"Success: {text}")
    
    @staticmethod
    def warning(text):
        print(f"Warning: {text}")
    
    @staticmethod
    def error(text):
        print(f"Error: {text}")
    
    @staticmethod
    def button(label, key=None, type="secondary", disabled=False):
        return False
    
    @staticmethod
    def text_input(label, value="", placeholder="", key=None, help=None):
        return value or "test_input"
    
    @staticmethod
    def text_area(label, value="", height=100, placeholder="", key=None, help=None):
        return value or "test_text_area"
    
    @staticmethod
    def selectbox(label, options, index=0, key=None, help=None):
        return options[index] if options else None
    
    @staticmethod
    def multiselect(label, options, default=None, key=None):
        return default or []
    
    @staticmethod
    def checkbox(label, value=False, key=None, help=None):
        return value
    
    @staticmethod
    def toggle(label, value=False, key=None, help=None):
        return value
    
    @staticmethod
    def slider(label, min_value=0, max_value=100, value=50, key=None):
        return value
    
    @staticmethod
    def number_input(label, min_value=None, max_value=None, value=0, key=None):
        return value
    
    @staticmethod
    def columns(spec):
        return [MockStreamlit() for _ in range(len(spec) if isinstance(spec, list) else spec)]
    
    @staticmethod
    def tabs(labels):
        return [MockStreamlit() for _ in labels]
    
    @staticmethod
    def expander(label, expanded=False):
        return MockStreamlit()
    
    @staticmethod
    def container():
        return MockStreamlit()
    
    @staticmethod
    def sidebar():
        return MockStreamlit()
    
    @staticmethod
    def empty():
        return MockStreamlit()
    
    @staticmethod
    def progress(value):
        return MockStreamlit()
    
    @staticmethod
    def spinner(text):
        return MockStreamlit()
    
    @staticmethod
    def metric(label, value, delta=None):
        print(f"Metric: {label}={value}")
    
    @staticmethod
    def dataframe(data):
        print(f"Dataframe: {len(data)} rows")
    
    @staticmethod
    def code(code, language=None):
        print(f"Code ({language}): {code[:50]}...")
    
    @staticmethod
    def download_button(label, data, file_name, mime="text/plain"):
        return False
    
    @staticmethod
    def file_uploader(label, type=None, accept_multiple_files=False):
        return None
    
    def __enter__(self):
        return self
    
    def __exit__(self, *args):
        pass


class MockOllama:
    """Mock Ollama client for testing"""
    
    def __init__(self, mock_responses=None):
        self.mock_responses = mock_responses or {}
        self.call_history = []
    
    def list(self):
        """Mock list models"""
        self.call_history.append(('list', {}))
        return {
            'models': [
                {'name': 'deepseek-coder:6.7b'},
                {'name': 'llama2:latest'},
                {'name': 'test-model:latest'}
            ]
        }
    
    def generate(self, model=None, prompt=None, system=None, **kwargs):
        """Mock generate response"""
        self.call_history.append(('generate', {
            'model': model,
            'prompt': prompt,
            'system': system,
            **kwargs
        }))
        
        # Return mock response based on prompt content
        if prompt and 'sql' in prompt.lower():
            return {'response': 'SELECT * FROM users WHERE active = true;'}
        elif prompt and 'explain' in prompt.lower():
            return {'response': 'This query selects all active users from the database.'}
        elif prompt and 'optimize' in prompt.lower():
            return {'response': 'SELECT id, name FROM users WHERE active = true;'}
        else:
            return {'response': self.mock_responses.get('default', 'Mock response for testing')}
    
    def pull(self, model):
        """Mock pull model"""
        self.call_history.append(('pull', {'model': model}))
        return {'status': 'success'}


class TestDatabase:
    """Test database helper for isolated testing"""
    
    def __init__(self, db_path=':memory:'):
        self.db_path = db_path
        self.conn = None
    
    def __enter__(self):
        self.setup()
        return self
    
    def __exit__(self, *args):
        self.teardown()
    
    def setup(self):
        """Set up test database with schema"""
        self.conn = sqlite3.connect(self.db_path)
        cursor = self.conn.cursor()
        
        # Create test tables
        cursor.executescript("""
            CREATE TABLE IF NOT EXISTS queries (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                tool TEXT NOT NULL,
                model TEXT NOT NULL,
                prompt TEXT NOT NULL,
                response TEXT,
                metadata TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
            
            CREATE TABLE IF NOT EXISTS knowledge_units (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                query_id INTEGER,
                title TEXT NOT NULL,
                content TEXT NOT NULL,
                category TEXT,
                tags TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (query_id) REFERENCES queries (id)
            );
            
            CREATE TABLE IF NOT EXISTS tools (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT UNIQUE NOT NULL,
                category TEXT NOT NULL,
                description TEXT,
                usage_count INTEGER DEFAULT 0,
                last_used TIMESTAMP,
                config TEXT
            );
            
            CREATE TABLE IF NOT EXISTS test_results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                test_file TEXT NOT NULL,
                passed INTEGER DEFAULT 0,
                failed INTEGER DEFAULT 0,
                errors INTEGER DEFAULT 0,
                duration REAL,
                output TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
        """)
        
        self.conn.commit()
    
    def teardown(self):
        """Clean up test database"""
        if self.conn:
            self.conn.close()
        
        if self.db_path != ':memory:' and os.path.exists(self.db_path):
            os.remove(self.db_path)
    
    def insert_test_data(self):
        """Insert sample test data"""
        cursor = self.conn.cursor()
        
        # Insert test queries
        test_queries = [
            ('sql_generator', 'deepseek-coder:6.7b', 'Find all users', 'SELECT * FROM users;', '{}'),
            ('sql_optimizer', 'deepseek-coder:6.7b', 'Optimize query', 'SELECT id FROM users;', '{}'),
            ('agent', 'llama2:latest', 'Test prompt', 'Test response', '{}')
        ]
        
        cursor.executemany(
            "INSERT INTO queries (tool, model, prompt, response, metadata) VALUES (?, ?, ?, ?, ?)",
            test_queries
        )
        
        # Insert test knowledge units
        test_knowledge = [
            (1, 'SQL Basics', 'Content about SQL basics', 'SQL', 'sql,basics'),
            (2, 'Query Optimization', 'Content about optimization', 'Performance', 'sql,performance')
        ]
        
        cursor.executemany(
            "INSERT INTO knowledge_units (query_id, title, content, category, tags) VALUES (?, ?, ?, ?, ?)",
            test_knowledge
        )
        
        self.conn.commit()


@contextmanager
def mock_streamlit():
    """Context manager to mock Streamlit"""
    original_streamlit = sys.modules.get('streamlit')
    sys.modules['streamlit'] = MockStreamlit
    
    try:
        yield MockStreamlit
    finally:
        if original_streamlit:
            sys.modules['streamlit'] = original_streamlit
        else:
            del sys.modules['streamlit']


@contextmanager
def mock_ollama(mock_responses=None):
    """Context manager to mock Ollama"""
    mock_client = MockOllama(mock_responses)
    
    with patch('ollama.Client', return_value=mock_client):
        with patch('ollama.list', mock_client.list):
            with patch('ollama.generate', mock_client.generate):
                yield mock_client


@contextmanager
def temp_directory():
    """Context manager for temporary directory"""
    temp_dir = tempfile.mkdtemp()
    try:
        yield temp_dir
    finally:
        shutil.rmtree(temp_dir)


@contextmanager
def temp_env_vars(**kwargs):
    """Context manager to temporarily set environment variables"""
    original_vars = {}
    
    for key, value in kwargs.items():
        original_vars[key] = os.environ.get(key)
        os.environ[key] = str(value)
    
    try:
        yield
    finally:
        for key, value in original_vars.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value


class TestFixtures:
    """Common test fixtures and data"""
    
    @staticmethod
    def get_test_sql_schema():
        """Get test SQL schema"""
        return """
        customers (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            email TEXT UNIQUE,
            created_at TIMESTAMP
        )
        
        orders (
            id INTEGER PRIMARY KEY,
            customer_id INTEGER REFERENCES customers(id),
            amount DECIMAL(10,2),
            order_date DATE,
            status TEXT
        )
        
        products (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            price DECIMAL(10,2),
            stock INTEGER DEFAULT 0,
            category TEXT
        )
        """
    
    @staticmethod
    def get_test_queries():
        """Get test SQL queries"""
        return [
            "Find top 5 customers by total order amount",
            "Show products with low stock (less than 10)",
            "Calculate monthly revenue for 2024",
            "Find orders with status 'pending' from last week",
            "Get customer order history with product details"
        ]
    
    @staticmethod
    def get_test_code_samples():
        """Get test code samples for various tools"""
        return {
            'python': """
def calculate_fibonacci(n):
    if n <= 1:
        return n
    return calculate_fibonacci(n-1) + calculate_fibonacci(n-2)
            """,
            'sql': """
SELECT c.name, COUNT(o.id) as order_count, SUM(o.amount) as total_spent
FROM customers c
LEFT JOIN orders o ON c.id = o.customer_id
GROUP BY c.id, c.name
ORDER BY total_spent DESC
LIMIT 10;
            """,
            'ruby': """
class User < ApplicationRecord
  validates :email, presence: true, uniqueness: true
  has_many :posts
  
  def full_name
    "#{first_name} #{last_name}".strip
  end
end
            """
        }
    
    @staticmethod
    def get_mock_file_content():
        """Get mock file content for testing"""
        return {
            'log': """
2024-01-15 10:30:45 ERROR: Database connection failed
2024-01-15 10:30:46 INFO: Retrying connection...
2024-01-15 10:30:47 ERROR: Connection timeout after 3 attempts
2024-01-15 10:30:48 FATAL: Application shutting down
            """,
            'json': """
{
    "name": "test_project",
    "version": "1.0.0",
    "dependencies": {
        "streamlit": "^1.28.0",
        "pandas": "^2.0.0"
    }
}
            """,
            'csv': """
name,email,age,city
John Doe,john@example.com,30,New York
Jane Smith,jane@example.com,25,Los Angeles
Bob Johnson,bob@example.com,35,Chicago
            """
        }


def assert_streamlit_output(mock_st, expected_calls):
    """Helper to assert Streamlit method calls"""
    # This would need to be implemented based on how we track calls
    pass


def create_test_file(path, content):
    """Create a test file with content"""
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, 'w') as f:
        f.write(content)


def cleanup_test_files(*paths):
    """Clean up test files"""
    for path in paths:
        if os.path.exists(path):
            if os.path.isdir(path):
                shutil.rmtree(path)
            else:
                os.remove(path)