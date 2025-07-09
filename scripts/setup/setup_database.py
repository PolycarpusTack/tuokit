#!/usr/bin/env python3
"""
Unified Database Setup for TuoKit
Run this to initialize the database with all required tables
"""

import sqlite3
import os
from pathlib import Path

def setup_database(db_path="tuokit.db"):
    """Create all required tables for TuoKit"""
    
    print(f"Setting up database: {db_path}")
    
    # Create absolute path from current directory
    if not os.path.isabs(db_path):
        db_path = os.path.abspath(db_path)
    
    with sqlite3.connect(db_path) as conn:
        # Enable foreign keys
        conn.execute("PRAGMA foreign_keys = ON")
        
        # Queries table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS queries (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                tool TEXT NOT NULL,
                model TEXT NOT NULL,
                prompt TEXT NOT NULL,
                ai_response TEXT NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Knowledge base table  
        conn.execute("""
            CREATE TABLE IF NOT EXISTS knowledge_base (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                query_id INTEGER,
                title TEXT NOT NULL,
                content TEXT NOT NULL,
                category TEXT,
                metadata TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (query_id) REFERENCES queries(id)
            )
        """)
        
        # System logs table
        conn.execute("""
            CREATE TABLE IF NOT EXISTS system_logs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                level TEXT NOT NULL,
                message TEXT NOT NULL,
                context TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        # Create indexes for performance
        conn.execute("CREATE INDEX IF NOT EXISTS idx_queries_tool ON queries(tool)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_queries_created ON queries(created_at)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_kb_category ON knowledge_base(category)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_kb_created ON knowledge_base(created_at)")
        conn.execute("CREATE INDEX IF NOT EXISTS idx_logs_level ON system_logs(level)")
        
        # Verify tables
        cursor = conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table'")
        tables = cursor.fetchall()
        
        print("\nCreated tables:")
        for table in tables:
            print(f"  [OK] {table[0]}")
            
    print("\n[SUCCESS] Database setup complete!")

if __name__ == "__main__":
    # Go back to project root if we're in scripts/setup
    if os.path.basename(os.getcwd()) == "setup":
        os.chdir("../..")
    
    setup_database()
