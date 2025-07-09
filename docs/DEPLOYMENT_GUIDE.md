# 🚀 TuoKit Deployment Guide: Local Client + Remote Server

## Architecture Overview

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   Local PC 1    │     │   Server/Cloud   │     │   Local PC 2    │
│                 │     │                  │     │                 │
│  TuoKit Client  │────▶│  Ollama Server   │◀────│  TuoKit Client  │
│  (Streamlit UI) │     │  (Deep Learning) │     │  (Streamlit UI) │
│                 │     │                  │     │                 │
│                 │────▶│  PostgreSQL DB   │◀────│                 │
└─────────────────┘     │  (Knowledge Base)│     └─────────────────┘
                        └──────────────────┘
```

## 🖥️ Server Setup (One-time)

### 1. Install Ollama on Server

```bash
# On Ubuntu/Debian server
curl -fsSL https://ollama.com/install.sh | sh

# Configure Ollama to listen on all interfaces
sudo systemctl edit ollama

# Add these lines:
[Service]
Environment="OLLAMA_HOST=0.0.0.0"
```

### 2. Install PostgreSQL on Server

```bash
# Install PostgreSQL
sudo apt update
sudo apt install postgresql postgresql-contrib

# Create TuoKit database and user
sudo -u postgres psql

CREATE DATABASE tuokit_knowledge;
CREATE USER tuokit_user WITH ENCRYPTED PASSWORD 'your_secure_password';
GRANT ALL PRIVILEGES ON DATABASE tuokit_knowledge TO tuokit_user;

# Configure PostgreSQL to accept remote connections
sudo nano /etc/postgresql/14/main/postgresql.conf
# Set: listen_addresses = '*'

sudo nano /etc/postgresql/14/main/pg_hba.conf
# Add: host all all 0.0.0.0/0 md5

# Restart PostgreSQL
sudo systemctl restart postgresql
```

### 3. Download Models on Server

```bash
# Pull the models you need
ollama pull deepseek-r1
ollama pull llama3.2
ollama pull codellama
```

### 4. Configure Firewall

```bash
# Open necessary ports
sudo ufw allow 11434/tcp  # Ollama
sudo ufw allow 5432/tcp   # PostgreSQL
```

## 💻 Local Client Setup (Each User)

### 1. Clone TuoKit Repository

```bash
git clone https://github.com/your-org/tuokit.git
cd tuokit
```

### 2. Install Python Dependencies

```bash
# Create virtual environment
python -m venv venv

# Activate it
# Windows:
venv\Scripts\activate
# Linux/Mac:
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
```

### 3. Configure Connection

```bash
# Run the setup wizard
streamlit run setup_local.py
```

Or manually create `.env` file:

```bash
cp .env.example .env
nano .env  # Edit with your server details
```

### 4. Initialize Database (First User Only)

```python
# Run once to create tables
python -c "from utils.database_client import TuoKitDB; TuoKitDB().initialize_schema()"
```

### 5. Launch TuoKit

```bash
# Windows
start_tuokit.bat

# Linux/Mac
./start_tuokit.sh

# Or directly
streamlit run main.py
```

## 🔒 Security Considerations

### For Production Deployment:

1. **Use VPN or Private Network**
   ```bash
   # Consider using Tailscale or WireGuard
   curl -fsSL https://tailscale.com/install.sh | sh
   ```

2. **Enable SSL/TLS for PostgreSQL**
   ```bash
   # Generate certificates
   sudo -u postgres openssl req -new -x509 -days 365 -nodes -text \
     -out server.crt -keyout server.key -subj "/CN=postgres"
   ```

3. **Add Authentication to Ollama**
   ```nginx
   # Use nginx as reverse proxy with auth
   server {
       listen 11434 ssl;
       location / {
           auth_basic "Ollama Server";
           auth_basic_user_file /etc/nginx/.htpasswd;
           proxy_pass http://localhost:11434;
       }
   }
   ```

4. **Network Isolation**
   - Place servers in private subnet
   - Use jump host for access
   - Implement IP whitelisting

## 🎯 Usage Patterns

### Solo Developer
- Run everything locally (Ollama + PostgreSQL + TuoKit)
- Use `.env` with all localhost settings

### Small Team
- Shared server with Ollama + PostgreSQL
- Each developer runs TuoKit client locally
- Shared knowledge base grows with team usage

### Enterprise
- Dedicated GPU server for Ollama
- PostgreSQL cluster with replication
- Load balancer for multiple Ollama instances
- Central file storage (NFS/S3)

## 📊 Performance Tips

### Ollama Server Optimization
```bash
# Allocate more GPU memory
export OLLAMA_GPU_MEMORY=24  # GB

# Enable multiple model loading
export OLLAMA_NUM_PARALLEL=4
```

### PostgreSQL Tuning
```sql
-- Adjust for your server's RAM
ALTER SYSTEM SET shared_buffers = '4GB';
ALTER SYSTEM SET effective_cache_size = '12GB';
ALTER SYSTEM SET work_mem = '128MB';
```

### Network Optimization
- Use gigabit ethernet minimum
- Consider dedicated VLAN for TuoKit traffic
- Enable jumbo frames if supported

## 🐛 Troubleshooting

### Connection Issues
```python
# Test script for debugging
import requests
import psycopg2

# Test Ollama
try:
    r = requests.get("http://your-server:11434/api/tags")
    print(f"Ollama: {r.status_code}")
except Exception as e:
    print(f"Ollama Error: {e}")

# Test PostgreSQL
try:
    conn = psycopg2.connect(
        host="your-server",
        database="tuokit_knowledge",
        user="tuokit_user",
        password="your_password"
    )
    print("PostgreSQL: Connected")
    conn.close()
except Exception as e:
    print(f"PostgreSQL Error: {e}")
```

### Common Fixes

1. **"Connection refused"**
   - Check firewall rules
   - Verify services are running
   - Confirm bind addresses

2. **"Model not found"**
   - Pull models on server first
   - Check model names match exactly

3. **"SSL error"**
   - Set `TUOKIT_SSL=false` for testing
   - Install proper certificates for production

## 🚀 Advanced Configurations

### Multi-Region Setup
```yaml
# docker-compose.yml for containerized deployment
version: '3.8'
services:
  ollama:
    image: ollama/ollama:latest
    ports:
      - "11434:11434"
    volumes:
      - ollama_models:/root/.ollama
    deploy:
      resources:
        reservations:
          devices:
            - driver: nvidia
              count: all
              capabilities: [gpu]
  
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: tuokit_knowledge
      POSTGRES_USER: tuokit_user
      POSTGRES_PASSWORD: ${DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"
```

### High Availability
- Use pgpool-II for PostgreSQL load balancing
- Deploy multiple Ollama instances behind HAProxy
- Implement Redis for session caching

This architecture provides:
- ✅ Centralized AI processing (expensive GPUs on server only)
- ✅ Shared knowledge base (team learning)
- ✅ Fast local UI response
- ✅ Scalable to many users
- ✅ Cost-effective (one GPU serves many)
