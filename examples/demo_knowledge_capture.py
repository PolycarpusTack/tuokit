"""
Demo: Knowledge Capture in Action
Shows how all agent operations are automatically captured
"""

import sys
sys.path.append('C:/Projects/Tuokit')

from utils import DatabaseManager
from manage_knowledge_db import create_knowledge_tables, show_knowledge_stats

# Import the wrapper to auto-capture
from knowledge_capture_wrapper import auto_wrap_all_agents

def demo_knowledge_capture():
    """Demonstrate automatic knowledge capture"""
    
    print("=" * 60)
    print("TuoKit Knowledge Capture Demo")
    print("=" * 60)
    
    # 1. Setup
    print("\n1. Setting up knowledge tables...")
    db = DatabaseManager()
    if not db or not db.connected:
        print("❌ Database not connected. Please check your .env file")
        return
    
    create_knowledge_tables(db)
    
    # 2. Auto-wrap agents
    print("\n2. Auto-wrapping agents with knowledge capture...")
    wrapped = auto_wrap_all_agents()
    
    if wrapped == 0:
        print("⚠️  No agents could be wrapped. Demonstrating manual capture...")
        demo_manual_capture(db)
    else:
        print(f"✅ {wrapped} agents wrapped successfully!")
        
        # 3. Use agents normally - everything is captured!
        print("\n3. Using agents (all operations automatically captured)...")
        
        try:
            # System Architecture
            from agent_hub_enhancements import SystemArchitectAgent
            print("\n📐 Designing system architecture...")
            architect = SystemArchitectAgent()
            architecture = architect.generate_architecture(
                "Real-time chat application with 10k concurrent users"
            )
            if "error" not in architecture:
                print(f"   ✅ Architecture designed (Quality: {architecture.get('quality_score', 0)*100:.0f}%)")
                print(f"   ✅ Knowledge captured automatically!")
        except Exception as e:
            print(f"   ❌ Architecture error: {e}")
        
        try:
            # Deep Research
            from agent_hub_enhancements import DeepResearchAgent
            print("\n🔬 Researching technical topic...")
            researcher = DeepResearchAgent()
            report = researcher.research_topic(
                "WebAssembly performance optimization techniques"
            )
            print(f"   ✅ Research completed with {len(report.get('key_insights', []))} insights")
            print(f"   ✅ Knowledge captured automatically!")
        except Exception as e:
            print(f"   ❌ Research error: {e}")
        
        try:
            # Data Analysis
            import pandas as pd
            import numpy as np
            from data_analysis_enhanced import DataAnalysisAgentEnhanced
            
            print("\n📊 Analyzing sample data...")
            
            # Create sample data
            df = pd.DataFrame({
                'date': pd.date_range('2024-01-01', periods=100),
                'sales': np.random.normal(1000, 200, 100),
                'region': np.random.choice(['North', 'South', 'East', 'West'], 100)
            })
            
            analyst = DataAnalysisAgentEnhanced()
            results = analyst.analyze_dataframe_enhanced(df, "demo_sales_data")
            
            print(f"   ✅ Analysis completed")
            print(f"   ✅ Data profile captured")
            print(f"   ✅ {len(results['analyses'].get('insights', []))} insights captured")
            
            # Natural language query
            nl_result = analyst.natural_language_query_enhanced(
                "What's the average sales by region?",
                dict(df.dtypes),
                "demo_sales_data"
            )
            
            if nl_result['success']:
                print(f"   ✅ Natural language query captured")
                print(f"   Generated SQL: {nl_result['query']}")
                
        except Exception as e:
            print(f"   ❌ Analysis error: {e}")
    
    # 4. Show captured knowledge
    print("\n4. Viewing captured knowledge...")
    show_knowledge_stats(db)
    
    # 5. Search example
    print("\n5. Searching knowledge base...")
    try:
        from knowledge_capture_enhanced import knowledge_capture
        
        # Search for architecture-related knowledge
        results = knowledge_capture.query_knowledge({
            "category": "architecture"
        }, limit=3)
        
        if results:
            print(f"\n🔍 Found {len(results)} architecture entries:")
            for entry in results:
                print(f"   - {entry['title'][:60]}... (Confidence: {entry['confidence_score']:.2f})")
        
        # Search for high-confidence insights
        high_conf = knowledge_capture.query_knowledge({
            "min_confidence": 0.8
        }, limit=5)
        
        if high_conf:
            print(f"\n🎯 High-confidence knowledge ({len(high_conf)} entries)")
            
    except Exception as e:
        print(f"Search error: {e}")
    
    db.close()
    
    print("\n" + "=" * 60)
    print("Demo Complete!")
    print("=" * 60)
    print("\nKey Takeaways:")
    print("✅ All agent operations are automatically captured")
    print("✅ Failed operations captured for learning")
    print("✅ Knowledge is searchable and categorized")
    print("✅ Performance metrics tracked automatically")
    print("✅ Relationships between knowledge preserved")
    
    print("\nNext steps:")
    print("1. Use agents normally - everything is captured!")
    print("2. Run 'python manage_knowledge_db.py stats' to see growth")
    print("3. Search past solutions when facing similar problems")
    print("4. Export valuable knowledge for sharing")

def demo_manual_capture(db):
    """Demo manual knowledge capture if auto-wrap fails"""
    
    print("\n📝 Demonstrating manual knowledge capture...")
    
    try:
        from knowledge_capture_enhanced import capture_knowledge_enhanced
        
        # Example 1: Capture a code generation
        capture_knowledge_enhanced(
            tool="code_generator",
            category="code",
            subcategory="python",
            prompt="Create a function to validate email addresses",
            response="""
def validate_email(email):
    import re
    pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
    return re.match(pattern, email) is not None
""",
            metadata={"language": "python", "complexity": "simple"},
            agent_name="CodeAgent",
            success=True
        )
        print("   ✅ Code generation captured")
        
        # Example 2: Capture an error solution
        capture_knowledge_enhanced(
            tool="error_decoder",
            category="error",
            subcategory="python",
            prompt="TypeError: 'NoneType' object is not subscriptable",
            response="This error occurs when trying to access an index or key on a None value. Check if the variable is None before accessing.",
            metadata={"error_type": "TypeError", "severity": "high"},
            agent_name="ErrorAgent",
            success=True,
            performance_metrics={"resolution_time": 0.5}
        )
        print("   ✅ Error solution captured")
        
        # Example 3: Capture a SQL query
        capture_knowledge_enhanced(
            tool="sql_generator",
            category="sql",
            subcategory="analytics",
            prompt="Get top 5 products by sales",
            response="SELECT product_name, SUM(sales) as total_sales FROM products GROUP BY product_name ORDER BY total_sales DESC LIMIT 5",
            metadata={"dialect": "postgresql", "complexity": "medium"},
            agent_name="SQLAgent",
            success=True
        )
        print("   ✅ SQL query captured")
        
        print("\n✅ Manual capture examples completed!")
        
    except Exception as e:
        print(f"   ❌ Manual capture error: {e}")

if __name__ == "__main__":
    demo_knowledge_capture()
