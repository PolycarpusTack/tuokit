# TuoKit Deployment Guide

## Table of Contents
1. [Quick Start](#quick-start)
2. [Prerequisites](#prerequisites)
3. [Installation Methods](#installation-methods)
4. [Database Setup](#database-setup)
5. [Ollama Configuration](#ollama-configuration)
6. [Platform-Specific Instructions](#platform-specific-instructions)
7. [Production Deployment](#production-deployment)
8. [Troubleshooting](#troubleshooting)
9. [Maintenance](#maintenance)

## Quick Start

### One-Line Installation
```bash
# Clone and setup
git clone https://github.com/yourusername/tuokit.git && cd tuokit && python tuokit_launcher.py --setup
```

### Quick Launch
```bash
# After setup
python tuokit_launcher.py --quick
```

## Prerequisites

### System Requirements
- **Python**: 3.8 or higher
- **RAM**: 8GB minimum (16GB recommended)
- **Disk Space**: 10GB minimum
- **OS**: Windows 10+, macOS 10.15+, Linux (Ubuntu 20.04+)

### Required Software
1. **Python 3.8+**
   ```bash
   # Check version
   python --version
   ```

2. **Git**
   ```bash
   # Check installation
   git --version
   ```

3. **PostgreSQL** (recommended) or SQLite
   ```bash
   # PostgreSQL check
   psql --version
   ```

4. **Ollama**
   ```bash
   # Install Ollama
   curl -fsSL https://ollama.ai/install.sh | sh
   ```

## Installation Methods

### Method 1: Using the Launcher (Recommended)

1. **Clone Repository**
   ```bash
   git clone https://github.com/yourusername/tuokit.git
   cd tuokit
   ```

2. **Run Launcher**
   ```bash
   python tuokit_launcher.py
   ```

3. **Select Option 2: Full Setup Wizard**
   - Creates virtual environment
   - Installs dependencies
   - Configures database
   - Checks Ollama
   - Runs diagnostics

### Method 2: Manual Installation

1. **Clone Repository**
   ```bash
   git clone https://github.com/yourusername/tuokit.git
   cd tuokit
   ```

2. **Create Virtual Environment**
   ```bash
   python -m venv tuokit-env
   
   # Activate on Windows
   tuokit-env\Scripts\activate
   
   # Activate on Linux/Mac
   source tuokit-env/bin/activate
   ```

3. **Install Dependencies**
   ```bash
   pip install -r requirements.txt
   ```

4. **Configure Environment**
   ```bash
   cp .env.example .env
   # Edit .env with your settings
   ```

5. **Setup Database**
   ```bash
   python setup_manager.py --interactive
   ```

6. **Launch Application**
   ```bash
   streamlit run tuokit_unified.py
   ```

### Method 3: Docker Installation

1. **Create Dockerfile**
   ```dockerfile
   FROM python:3.9-slim
   
   WORKDIR /app
   
   # Install system dependencies
   RUN apt-get update && apt-get install -y \
       postgresql-client \
       && rm -rf /var/lib/apt/lists/*
   
   # Copy requirements
   COPY requirements.txt .
   RUN pip install --no-cache-dir -r requirements.txt
   
   # Copy application
   COPY . .
   
   # Expose port
   EXPOSE 8501
   
   # Run app
   CMD ["streamlit", "run", "tuokit_unified.py", "--server.port=8501", "--server.address=0.0.0.0"]
   ```

2. **Build and Run**
   ```bash
   docker build -t tuokit .
   docker run -p 8501:8501 -v $(pwd)/.env:/app/.env tuokit
   ```

## Database Setup

### PostgreSQL Setup

1. **Install PostgreSQL**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install postgresql postgresql-contrib
   
   # macOS
   brew install postgresql
   
   # Windows
   # Download from https://www.postgresql.org/download/windows/
   ```

2. **Create Database and User**
   ```sql
   -- Connect as postgres user
   sudo -u postgres psql
   
   -- Create database and user
   CREATE DATABASE tuokit_knowledge;
   CREATE USER tuokit_user WITH PASSWORD 'your_secure_password';
   GRANT ALL PRIVILEGES ON DATABASE tuokit_knowledge TO tuokit_user;
   \q
   ```

3. **Configure .env**
   ```bash
   TUOKIT_DB_TYPE=postgresql
   TUOKIT_PG_HOST=localhost
   TUOKIT_PG_PORT=5432
   TUOKIT_PG_DB=tuokit_knowledge
   TUOKIT_PG_USER=tuokit_user
   TUOKIT_PG_PASSWORD=your_secure_password
   ```

4. **Run Migrations**
   ```bash
   python migration_manager.py --init
   python migration_manager.py --migrate
   ```

### SQLite Setup (Simple)

1. **Configure .env**
   ```bash
   TUOKIT_DB_TYPE=sqlite
   TUOKIT_SQLITE_PATH=tuokit.db
   ```

2. **Initialize Database**
   ```bash
   python setup_manager.py --init-db
   ```

### MySQL Setup

1. **Install MySQL**
   ```bash
   # Ubuntu/Debian
   sudo apt-get install mysql-server
   
   # macOS
   brew install mysql
   ```

2. **Create Database**
   ```sql
   CREATE DATABASE tuokit_knowledge;
   CREATE USER 'tuokit_user'@'localhost' IDENTIFIED BY 'your_secure_password';
   GRANT ALL PRIVILEGES ON tuokit_knowledge.* TO 'tuokit_user'@'localhost';
   FLUSH PRIVILEGES;
   ```

3. **Configure .env**
   ```bash
   TUOKIT_DB_TYPE=mysql
   TUOKIT_MYSQL_HOST=localhost
   TUOKIT_MYSQL_PORT=3306
   TUOKIT_MYSQL_DB=tuokit_knowledge
   TUOKIT_MYSQL_USER=tuokit_user
   TUOKIT_MYSQL_PASSWORD=your_secure_password
   ```

## Ollama Configuration

### Installation

1. **Linux/macOS**
   ```bash
   curl -fsSL https://ollama.ai/install.sh | sh
   ```

2. **Windows**
   - Download from https://ollama.ai/download/windows

### Model Setup

1. **Start Ollama**
   ```bash
   ollama serve
   ```

2. **Pull Recommended Models**
   ```bash
   # Primary model
   ollama pull deepseek-r1:latest
   
   # Alternative models
   ollama pull deepseek-coder:6.7b
   ollama pull qwen2.5-coder:latest
   ollama pull llama3.2:latest
   ```

3. **Configure in .env**
   ```bash
   TUOKIT_OLLAMA_HOST=http://localhost:11434
   ```

### WSL-Specific Configuration

For WSL users, Ollama typically runs on Windows host:

1. **Auto-detect Host**
   ```bash
   python diagnostic_toolkit.py --cli
   # Will auto-detect and update .env
   ```

2. **Manual Configuration**
   ```bash
   # Get Windows host IP
   cat /etc/resolv.conf | grep nameserver
   
   # Update .env
   TUOKIT_OLLAMA_HOST=http://[WINDOWS_IP]:11434
   ```

## Platform-Specific Instructions

### Windows

1. **Install Prerequisites**
   - Python from python.org
   - Git from git-scm.com
   - PostgreSQL from postgresql.org
   - Ollama from ollama.ai

2. **Path Configuration**
   ```powershell
   # Add Python to PATH if needed
   $env:Path += ";C:\Python39;C:\Python39\Scripts"
   ```

3. **Run Launcher**
   ```powershell
   python tuokit_launcher.py
   ```

### macOS

1. **Install Homebrew**
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Install Dependencies**
   ```bash
   brew install python@3.9 postgresql ollama
   ```

3. **Start Services**
   ```bash
   brew services start postgresql
   ollama serve &
   ```

### Linux (Ubuntu/Debian)

1. **Update System**
   ```bash
   sudo apt update && sudo apt upgrade
   ```

2. **Install Dependencies**
   ```bash
   sudo apt install python3.9 python3-pip postgresql postgresql-contrib
   ```

3. **Install Ollama**
   ```bash
   curl -fsSL https://ollama.ai/install.sh | sh
   ```

### WSL (Windows Subsystem for Linux)

1. **Install WSL2**
   ```powershell
   wsl --install
   ```

2. **Setup in WSL**
   ```bash
   # In WSL terminal
   cd /mnt/c/Projects
   git clone https://github.com/yourusername/tuokit.git
   cd tuokit
   python3 tuokit_launcher.py
   ```

3. **Configure for Windows Ollama**
   - Run Ollama on Windows host
   - Use diagnostic toolkit to auto-detect

## Production Deployment

### Using Systemd (Linux)

1. **Create Service File**
   ```ini
   # /etc/systemd/system/tuokit.service
   [Unit]
   Description=TuoKit AI Toolkit
   After=network.target postgresql.service
   
   [Service]
   Type=simple
   User=tuokit
   WorkingDirectory=/opt/tuokit
   Environment="PATH=/opt/tuokit/tuokit-env/bin"
   ExecStart=/opt/tuokit/tuokit-env/bin/streamlit run tuokit_unified.py --server.port=8501 --server.address=0.0.0.0
   Restart=always
   
   [Install]
   WantedBy=multi-user.target
   ```

2. **Enable and Start**
   ```bash
   sudo systemctl enable tuokit
   sudo systemctl start tuokit
   ```

### Using Docker Compose

1. **Create docker-compose.yml**
   ```yaml
   version: '3.8'
   
   services:
     tuokit:
       build: .
       ports:
         - "8501:8501"
       environment:
         - TUOKIT_DB_TYPE=postgresql
         - TUOKIT_PG_HOST=db
         - TUOKIT_PG_PORT=5432
         - TUOKIT_PG_DB=tuokit_knowledge
         - TUOKIT_PG_USER=tuokit_user
         - TUOKIT_PG_PASSWORD=secure_password
         - TUOKIT_OLLAMA_HOST=http://ollama:11434
       depends_on:
         - db
         - ollama
       volumes:
         - ./data:/app/data
     
     db:
       image: postgres:13
       environment:
         - POSTGRES_DB=tuokit_knowledge
         - POSTGRES_USER=tuokit_user
         - POSTGRES_PASSWORD=secure_password
       volumes:
         - postgres_data:/var/lib/postgresql/data
     
     ollama:
       image: ollama/ollama
       ports:
         - "11434:11434"
       volumes:
         - ollama_data:/root/.ollama
   
   volumes:
     postgres_data:
     ollama_data:
   ```

2. **Deploy**
   ```bash
   docker-compose up -d
   ```

### Nginx Reverse Proxy

1. **Install Nginx**
   ```bash
   sudo apt install nginx
   ```

2. **Configure Site**
   ```nginx
   # /etc/nginx/sites-available/tuokit
   server {
       listen 80;
       server_name tuokit.yourdomain.com;
       
       location / {
           proxy_pass http://localhost:8501;
           proxy_http_version 1.1;
           proxy_set_header Upgrade $http_upgrade;
           proxy_set_header Connection "upgrade";
           proxy_set_header Host $host;
           proxy_set_header X-Real-IP $remote_addr;
           proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
           proxy_set_header X-Forwarded-Proto $scheme;
           proxy_read_timeout 86400;
       }
   }
   ```

3. **Enable Site**
   ```bash
   sudo ln -s /etc/nginx/sites-available/tuokit /etc/nginx/sites-enabled/
   sudo nginx -t
   sudo systemctl reload nginx
   ```

## Troubleshooting

### Common Issues

#### 1. Ollama Connection Failed
```bash
# Check if Ollama is running
curl http://localhost:11434/api/tags

# Run diagnostic
python diagnostic_toolkit.py --cli

# Manual fix in .env
TUOKIT_OLLAMA_HOST=http://localhost:11434
```

#### 2. Database Connection Error
```bash
# Check PostgreSQL status
sudo systemctl status postgresql

# Test connection
psql -h localhost -U tuokit_user -d tuokit_knowledge

# Verify .env settings
cat .env | grep TUOKIT_
```

#### 3. Import Errors
```bash
# Ensure virtual environment is activated
which python  # Should show venv path

# Reinstall requirements
pip install -r requirements.txt --force-reinstall
```

#### 4. Streamlit Issues
```bash
# Clear cache
streamlit cache clear

# Run with debug
streamlit run tuokit_unified.py --logger.level=debug
```

### Diagnostic Commands

```bash
# Full system diagnostic
python diagnostic_toolkit.py --cli

# Check specific component
python tuokit_launcher.py --diagnose

# Migration status
python migration_manager.py --status

# Verify integrity
python migration_manager.py --verify
```

## Maintenance

### Regular Tasks

#### 1. Database Backup
```bash
# Create backup
python migration_manager.py --backup

# Automated backup (cron)
0 2 * * * cd /opt/tuokit && python migration_manager.py --backup
```

#### 2. Update Models
```bash
# Pull latest models
ollama pull deepseek-r1:latest
ollama pull qwen2.5-coder:latest
```

#### 3. Clean Logs
```bash
# Clear old logs
find logs/ -name "*.log" -mtime +30 -delete

# Clear Streamlit cache
streamlit cache clear
```

#### 4. Monitor Disk Usage
```bash
# Check database size
psql -U tuokit_user -d tuokit_knowledge -c "SELECT pg_size_pretty(pg_database_size('tuokit_knowledge'));"

# Check Ollama models
du -sh ~/.ollama/models/
```

### Upgrade Process

1. **Backup Current Installation**
   ```bash
   tar -czf tuokit-backup-$(date +%Y%m%d).tar.gz --exclude=tuokit-env .
   ```

2. **Pull Updates**
   ```bash
   git pull origin main
   ```

3. **Update Dependencies**
   ```bash
   pip install -r requirements.txt --upgrade
   ```

4. **Run Migrations**
   ```bash
   python migration_manager.py --migrate
   ```

5. **Restart Services**
   ```bash
   sudo systemctl restart tuokit
   ```

### Performance Tuning

#### PostgreSQL Optimization
```sql
-- Increase shared buffers
ALTER SYSTEM SET shared_buffers = '256MB';

-- Optimize for SSD
ALTER SYSTEM SET random_page_cost = 1.1;

-- Reload configuration
SELECT pg_reload_conf();
```

#### Streamlit Configuration
```toml
# .streamlit/config.toml
[server]
maxUploadSize = 200
maxMessageSize = 200

[browser]
gatherUsageStats = false

[theme]
base = "dark"
```

## Security Considerations

### 1. Environment Variables
- Never commit .env to version control
- Use strong passwords
- Rotate credentials regularly

### 2. Network Security
- Use HTTPS in production
- Configure firewall rules
- Limit database access

### 3. Access Control
- Implement authentication if needed
- Use environment-specific configs
- Monitor access logs

## Support and Resources

### Documentation
- System Architecture: SYSTEM_ARCHITECTURE.md
- Component Reference: COMPONENT_REFERENCE.md
- API Documentation: API_REFERENCE.md

### Community
- GitHub Issues: Report bugs and request features
- Discord: Join the community chat
- Wiki: Detailed guides and tutorials

### Getting Help
1. Run diagnostics first
2. Check troubleshooting section
3. Search existing issues
4. Create detailed bug report