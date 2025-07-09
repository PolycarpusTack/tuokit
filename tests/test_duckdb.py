"""
Quick test to verify DuckDB is installed and working
"""

try:
    import duckdb
    print("✅ DuckDB imported successfully!")
    print(f"   Version: {duckdb.__version__}")
    
    # Test basic functionality
    conn = duckdb.connect(':memory:')
    
    # Create test data
    conn.execute("""
        CREATE TABLE test AS 
        SELECT * FROM (VALUES 
            ('North', 1000),
            ('South', 1500),
            ('East', 1200),
            ('West', 1800)
        ) AS t(region, sales)
    """)
    
    # Run a query
    result = conn.execute("SELECT region, sales FROM test ORDER BY sales DESC").fetchall()
    
    print("\n✅ DuckDB query test successful!")
    print("   Top regions by sales:")
    for region, sales in result:
        print(f"   - {region}: ${sales}")
    
    # Test pandas integration
    try:
        import pandas as pd
        
        # Create DataFrame
        df = pd.DataFrame({
            'product': ['A', 'B', 'C', 'D'],
            'price': [10.5, 20.0, 15.5, 30.0]
        })
        
        # Register with DuckDB
        conn.register('products', df)
        
        # Query the DataFrame
        avg_price = conn.execute("SELECT AVG(price) as avg_price FROM products").fetchone()[0]
        
        print(f"\n✅ Pandas integration working!")
        print(f"   Average price: ${avg_price:.2f}")
        
    except ImportError:
        print("\n⚠️  Pandas not installed, skipping DataFrame test")
    
    print("\n" + "="*50)
    print("DuckDB is ready to use in TuoKit!")
    print("="*50)
    
except ImportError:
    print("❌ DuckDB not installed!")
    print("   Please run: pip install duckdb")
except Exception as e:
    print(f"❌ Error testing DuckDB: {e}")
