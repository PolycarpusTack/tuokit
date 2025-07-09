#!/usr/bin/env python3
"""Test pgvector availability and create a simple vector search implementation"""

import os
import sys
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

import psycopg2
from utils.database import DB_CONFIG

def check_pgvector():
    """Check if pgvector is available"""
    try:
        conn = psycopg2.connect(**DB_CONFIG)
        cur = conn.cursor()
        
        # Check if pgvector extension exists
        cur.execute("SELECT * FROM pg_extension WHERE extname = 'vector'")
        has_pgvector = cur.fetchone() is not None
        print(f'✓ pgvector installed: {has_pgvector}')
        
        # Check available extensions
        cur.execute("SELECT name FROM pg_available_extensions WHERE name LIKE '%vector%' OR name LIKE '%embed%' OR name = 'pg_trgm'")
        extensions = cur.fetchall()
        print(f'✓ Vector-related extensions available: {[ext[0] for ext in extensions]}')
        
        # Check if pg_trgm is installed (for text similarity)
        cur.execute("SELECT * FROM pg_extension WHERE extname = 'pg_trgm'")
        has_trgm = cur.fetchone() is not None
        print(f'✓ pg_trgm installed: {has_trgm}')
        
        cur.close()
        conn.close()
        
        return has_pgvector, has_trgm
        
    except Exception as e:
        print(f'✗ Error connecting to database: {e}')
        return False, False

if __name__ == "__main__":
    print("Checking PostgreSQL vector capabilities...")
    check_pgvector()