# 🧠 TuoKit - AI Developer Portal

TuoKit is a practical, minimalist AI-powered developer toolkit built with Streamlit. Following the "TuoKit Architect" philosophy of building fast, smart, and exactly what's needed.

## 🚀 Quick Start

### Prerequisites
- Python 3.8+
- PostgreSQL (optional, for knowledge persistence)
- Ollama installed and running

### Installation

1. **Create virtual environment:**
```bash
cd C:/Projects/Tuokit
python -m venv tuokit-env

# Windows
tuokit-env\Scripts\activate

# Linux/Mac
source tuokit-env/bin/activate
```

2. **Install dependencies:**
```bash
pip install -r requirements.txt
```

3. **Configure environment:**
Copy the example environment file and edit with your credentials:
```bash
copy .env.example .env  # Windows
cp .env.example .env    # Linux/Mac
```

Edit the `.env` file with your database credentials:
```env
DB_NAME=ollama_knowledge
DB_USER=ollama_user
DB_PASSWORD=your_secure_password
DB_HOST=localhost
```
4. **Database Setup (Optional but Recommended):**

For full functionality including query history and knowledge library:

```bash
# Option 1: Run the setup script
python setup_database.py

# Option 2: Use the SQL file
psql -U ollama_user -d ollama_knowledge -f setup_database.sql
```

See [DATABASE_SETUP.md](DATABASE_SETUP.md) for detailed instructions.

**Note:** If you skip database setup, TuoKit will still work but without persistence features.

Or manually create the database:
```sql
-- Create database
CREATE DATABASE ollama_knowledge;

-- Create user
CREATE USER ollama_user WITH PASSWORD 'your_secure_password';

-- Grant privileges
GRANT ALL PRIVILEGES ON DATABASE ollama_knowledge TO ollama_user;

-- Create tables (run these in the ollama_knowledge database)
CREATE TABLE queries (
    id SERIAL PRIMARY KEY,
    tool VARCHAR(100),
    model VARCHAR(100),
    user_prompt TEXT,
    ai_response TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE knowledge_units (
    id SERIAL PRIMARY KEY,
    query_id INTEGER REFERENCES queries(id),
    title VARCHAR(255),
    content TEXT,
    category VARCHAR(100),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### 🏃 Running TuoKit

1. **Test Ollama Connection (Recommended):**
```bash
python test_ollama.py
```

2. **Start TuoKit:**
```bash
streamlit run app.py
```

Or use the quick start script:
```bash
# Windows
start_tuokit.bat

# Linux/Mac
./start_tuokit.sh
```
The dashboard will open at `http://localhost:8501`

## 🎯 Features

### Current
- **System Monitoring**: Real-time Ollama status and resource usage
- **Model Management**: Switch between AI models on the fly
- **Activity Dashboard**: Track recent queries and knowledge growth
- **Navigation Hub**: Quick access to all TuoKit tools
- **Code Tools**: 
  - Code Explanation with algorithm analysis
  - Intelligent debugging with error diagnosis
  - Multi-language code generation
  - Knowledge base integration
- **Document Tools**:
  - PDF and text file processing
  - Context-aware Q&A
  - Intelligent summarization
  - Structured knowledge extraction
  - Download processed outputs
- **SQL Generator**:
  - Natural language to SQL conversion
  - PostgreSQL and Oracle dialect support
  - Performance optimization suggestions
  - SQL dialect translation (Oracle ↔ PostgreSQL)
  - Security vulnerability scanning
  - Schema-aware query generation
  - Stored procedure generation
  - SQL pattern knowledge capture
  - Optional: Live database connectivity (see docs/SQL_ENTERPRISE_SETUP.md)
- **SQL Optimizer**:
  - Query performance analysis
  - Execution plan visualization
  - Index recommendations with validation
  - AI-generated query alternatives
  - Anti-pattern detection
  - Functional equivalence checking
  - Professional validation framework
  - Safety checks and warnings
- **SQL Pipeline**:
  - Guided 4-step workflow (Describe → Generate → Optimize → Understand)
  - Natural language to optimized SQL
  - Automatic query optimization
  - Plain English explanations
  - Interactive testing with sample data
  - Built-in learning resources
  - Visual progress tracking
- **Knowledge Library**:
  - Full-text search and filtering
  - One-click copy and reuse
  - Export to multiple formats
  - Knowledge analytics dashboard
- **Help System**:
  - Integrated documentation
  - Tool-specific tutorials
  - Troubleshooting guides
  - Searchable FAQ

### Roadmap
- Advanced AI model management
- Team collaboration features
- API integration for external tools

## 🏗️ Architecture

TuoKit follows minimalist principles:
- Single-file implementations where possible
- Streamlit for rapid UI development
- PostgreSQL for optional persistence
- Modular design for easy extension

## 📁 Project Structure
```
tuokit/
├── app.py                    # Main dashboard
├── utils.py                  # Core utilities
├── requirements.txt          # Dependencies
├── .env                      # Configuration (create from .env.example)
├── .env.example              # Example configuration
├── database_setup.sql        # Database initialization
├── database_migration_v0.4.sql # Migration for v0.4
├── sample_knowledge_data.sql # Sample data for testing
├── test_ollama.py            # Ollama connection test
├── test_pdf.py               # PDF library test
├── test_document.txt         # Sample document for testing
├── pages/                    # Tool pages
│   ├── code_tools.py         # Code explanation, debugging, generation
│   ├── doc_tools.py          # Document analysis
│   └── knowledge_lib.py      # Knowledge management
├── docs/                     # Documentation
│   ├── document_tools_guide.md
│   ├── knowledge_library_guide.md
│   └── quick_reference.md
├── start_tuokit.bat          # Windows quick start
├── start_tuokit.sh           # Linux/Mac quick start
├── CHANGELOG.md              # Version history
└── README.md                 # This file
```

## 🔧 Troubleshooting

### Database Connection Issues
- TuoKit works without a database (limited features)
- Check PostgreSQL is running: `pg_isready`
- Verify credentials in `.env` file

### Ollama Not Detected
- Ensure Ollama is installed: `ollama --version`
- Start Ollama service: `ollama serve`
- Check models: `ollama list`
- Run the test script: `python test_ollama.py`

### Code Tools Not Working
- Verify Ollama has appropriate models: `ollama pull deepseek-coder:6.7b`
- Check Ollama is running: `ollama serve`
- Test with: `python test_ollama.py`

### System Stats Not Showing (Windows)
- Requires admin privileges for some metrics
- Falls back to "N/A" if permissions insufficient

## 🛠️ Development Setup

### Security and Code Quality Tools

```bash
# Python security scanning with Bandit
pip install bandit
bandit -r . -f json -o security_report.json

# For JavaScript/Frontend development (if needed)
npm install -g eslint
```

See [Development Setup Guide](docs/development_setup.md) for detailed instructions.

## 🤝 Contributing

Follow the TuoKit Architect principles:
1. Build only what's needed
2. Prefer simplicity over complexity
3. Add comprehensive error handling
4. Document with usage examples
5. Test with actual Ollama models
6. Run security scans before committing

## 📝 License

MIT License - Build freely!

---
*TuoKit v0.1 - Built with the TuoKit Architect philosophy*# tuokit
