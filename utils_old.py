import subprocess
import psycopg2
import os
import platform
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Database configuration
DB_CONFIG = {
    "dbname": os.getenv("DB_NAME", "ollama_knowledge"),
    "user": os.getenv("DB_USER", "ollama_user"),
    "password": os.getenv("DB_PASSWORD", "secure_password"),
    "host": os.getenv("DB_HOST", "localhost")
}

class OllamaManager:
    @staticmethod
    def get_status():
        """Check Ollama service status"""
        try:
            result = subprocess.run(["ollama", "list", "--json"], 
                                  capture_output=True, text=True, timeout=5)
            models = result.stdout.splitlines() if result.stdout else []
            return {
                "running": True,
                "model_count": len(models),
                "error": None
            }
        except Exception as e:
            return {                "running": False,
                "model_count": 0,
                "error": str(e)
            }

class DatabaseManager:
    def __init__(self):
        try:
            self.conn = psycopg2.connect(**DB_CONFIG)
            self.conn.autocommit = True
            self.connected = True
        except psycopg2.Error as e:
            print(f"Database connection failed: {e}")
            self.conn = None
            self.connected = False
    
    def get_recent_queries(self, limit=5):
        """Get recent queries from database"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    SELECT id, tool, user_prompt, created_at 
                    FROM queries 
                    ORDER BY created_at DESC 
                    LIMIT %s
                """, (limit,))
                return cur.fetchall()
        except psycopg2.Error as e:
            print(f"Database error: {e}")
            return []    
    def get_knowledge_count(self):
        """Get total knowledge units"""
        if not self.connected or not self.conn:
            return 0
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT COUNT(*) FROM knowledge_units")
                return cur.fetchone()[0]
        except psycopg2.Error:
            return 0
    
    def log_query(self, tool: str, model: str, prompt: str, response: str):
        """Log a query to the database"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO queries (tool, model, user_prompt, ai_response)
                    VALUES (%s, %s, %s, %s)
                    RETURNING id
                """, (tool, model, prompt, response))
                return cur.fetchone()[0]
        except psycopg2.Error as e:
            print(f"Error logging query: {e}")
            return None
    
    def get_query_by_id(self, query_id: int):
        """Retrieve full query by ID"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT * FROM queries WHERE id = %s", (query_id,))
                return cur.fetchone()
        except psycopg2.Error as e:
            print(f"Error retrieving query: {e}")
            return None
    
    def save_knowledge_unit(self, query_id: int, title: str, content: str, category: str):
        """Save extracted knowledge to database"""
        if not self.connected or not self.conn:
            return False
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    INSERT INTO knowledge_units 
                    (query_id, title, content, category)
                    VALUES (%s, %s, %s, %s)
                """, (query_id, title, content, category))
                return True
        except psycopg2.Error as e:
            print(f"Error saving knowledge: {e}")
            return False
    
    def get_knowledge_categories(self):
        """Get distinct knowledge categories"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT DISTINCT category FROM knowledge_units ORDER BY category")
                return [row[0] for row in cur.fetchall()]
        except psycopg2.Error as e:
            print(f"Error getting categories: {e}")
            return []
    
    def get_knowledge_by_id(self, k_id: int):
        """Get knowledge unit by ID"""
        if not self.connected or not self.conn:
            return None
        try:
            with self.conn.cursor() as cur:
                cur.execute("""
                    SELECT k.*, q.tool, q.model, q.created_at 
                    FROM knowledge_units k
                    JOIN queries q ON k.query_id = q.id
                    WHERE k.id = %s
                """, (k_id,))
                return cur.fetchone()
        except psycopg2.Error as e:
            print(f"Error getting knowledge: {e}")
            return None
    
    def get_knowledge_categories(self):
        """Get distinct knowledge categories"""
        if not self.connected or not self.conn:
            return []
        try:
            with self.conn.cursor() as cur:
                cur.execute("SELECT DISTINCT category FROM knowledge_units ORDER BY category")
                return [row[0] for row in cur.fetchall()]
        except psycopg2.Error as e:
            print(f"Error getting categories: {e}")
            return []

def get_system_stats():
    """Get basic system resource usage"""
    try:
        import platform
        if platform.system() == "Windows":
            # Windows-specific commands
            # Get CPU usage
            cpu_cmd = "wmic cpu get loadpercentage /value"
            cpu_result = subprocess.check_output(cpu_cmd, shell=True).decode().strip()
            cpu = [line.split('=')[1] for line in cpu_result.split('\n') if 'LoadPercentage=' in line][0]
            
            # Get memory usage
            mem_cmd = 'wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value'
            mem_result = subprocess.check_output(mem_cmd, shell=True).decode().strip()
            mem_lines = mem_result.split('\n')
            total_mem = int([line.split('=')[1] for line in mem_lines if 'TotalVisibleMemorySize=' in line][0])
            free_mem = int([line.split('=')[1] for line in mem_lines if 'FreePhysicalMemory=' in line][0])
            mem_percent = ((total_mem - free_mem) / total_mem) * 100
            mem = f"{mem_percent:.1f}%"
        else:
            # Linux/Mac compatible
            cpu = subprocess.check_output("top -bn1 | grep 'Cpu(s)' | awk '{print $2}'", shell=True).decode().strip()
            mem = subprocess.check_output("free -m | awk 'NR==2{printf \"%.1f%%\", $3*100/$2}'", shell=True).decode().strip()
        
        return {
            "cpu": f"{cpu}%",
            "memory": mem
        }
    except Exception as e:
        return {"cpu": "N/A", "memory": "N/A"}

def safe_ollama_generate(model: str, prompt: str) -> dict:
    """Safely call Ollama generate with error handling"""
    try:
        import ollama
        response = ollama.generate(model=model, prompt=prompt)
        return response
    except Exception as e:
        return {"response": f"Error calling Ollama: {str(e)}", "error": True}

def get_contextual_help(tool: str, context: str = "") -> str:
    """Fetch help documentation based on current context"""
    help_db = {
        "code_explainer": {
            "default": "Explain code functionality in bullet points. Add comments like '# Focus on security' for targeted analysis.",
            "security": "Analyzing for security vulnerabilities. Check for SQL injection, XSS, authentication issues."
        },
        "code_debugger": {
            "default": "Debug errors by providing the full error message and problematic code.",
            "performance": "Include profiling data or slow operation details for performance debugging."
        },
        "code_generator": {
            "default": "Generate code by describing what you need. Be specific about requirements.",
            "api": "For API clients, specify authentication method, endpoints, and error handling needs."
        },
        "doc_summary": {
            "default": "Generate 3-5 key point summaries. Use '// Technical summary' for jargon-heavy docs.",
            "technical": "Creating technical summary with preservation of key terminology and specifications."
        },
        "doc_qa": {
            "default": "Ask specific questions about the document content.",
            "research": "For research papers, ask about methodology, findings, limitations."
        }
    }
    
    # Try to fetch from knowledge base first
    db = None
    try:
        db = DatabaseManager()
        if db.connected:
            with db.conn.cursor() as cur:
                cur.execute("""
                    SELECT content FROM knowledge_units 
                    WHERE category = 'Documentation' 
                    AND title ILIKE %s
                    LIMIT 1
                """, (f"%{tool}%",))
                if result := cur.fetchone():
                    return result[0]
    except:
        pass
    
    # Fallback to built-in help
    tool_help = help_db.get(tool, {})
    if context and context in tool_help:
        return tool_help[context]
    return tool_help.get("default", "Use this tool to process your content with AI assistance.")