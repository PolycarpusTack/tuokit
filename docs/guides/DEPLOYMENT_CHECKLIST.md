# 🚀 TuoKit Deployment Checklist  
**Follow this step-by-step guide to launch your AI development suite:**

---

## 🔧 Pre-Deployment Setup  

### 1. Environment Preparation
```bash
# Create virtual environment
python -m venv tuo-env
source tuo-env/bin/activate  # Linux/Mac
.\tuo-env\Scripts\activate   # Windows
```

### 2. Install Dependencies
```bash
pip install -r requirements.txt
```

### 3. PostgreSQL Configuration
```bash
# Create database and user
sudo -u postgres psql -c "CREATE DATABASE ollama_knowledge;"
sudo -u postgres psql -c "CREATE USER ollama_user WITH PASSWORD 'your_secure_password';"
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE ollama_knowledge TO ollama_user;"

# Initialize schema
psql -U ollama_user -d ollama_knowledge -f database_setup.sql
```

### 4. Environment Variables
Create `.env` file:
```env
DB_NAME=ollama_knowledge
DB_USER=ollama_user
DB_PASSWORD=your_secure_password
DB_HOST=localhost
```

---

## 🖥️ Application Launch  

### 1. Start Ollama Service
```bash
ollama serve &
```

### 2. Download Models
```bash
ollama pull deepseek-coder:6.7b
ollama pull deepseek-r1:6.7b
```

### 3. Launch TuoKit
```bash
streamlit run app.py
```

### 4. Access Portal
Open: http://localhost:8501

---

## 🔒 Production Considerations  

### 1. Reverse Proxy Setup (Nginx)
```nginx
server {
    listen 80;
    server_name tuo.example.com;
    
    location / {
        proxy_pass http://localhost:8501;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }
}
```

### 2. Systemd Service
Create `/etc/systemd/system/tuokit.service`:
```ini
[Unit]
Description=TuoKit AI Development Suite
After=network.target postgresql.service

[Service]
User=your_user
WorkingDirectory=/path/to/tuokit
Environment="PATH=/path/to/tuo-env/bin"
ExecStart=/path/to/tuo-env/bin/streamlit run app.py
Restart=always

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl enable tuokit
sudo systemctl start tuokit
```

### 3. Database Backups
Add to crontab:
```bash
0 3 * * * pg_dump -U ollama_user -d ollama_knowledge > /backups/tuo_$(date +\%F).sql
```

---

## 📈 Post-Deployment

### Usage Analytics
```sql
-- Top used tools
SELECT tool, COUNT(*) FROM queries 
GROUP BY tool ORDER BY count DESC;

-- Most valuable knowledge
SELECT category, COUNT(*) FROM knowledge_units 
WHERE verified = true GROUP BY category;

-- Daily activity
SELECT DATE(created_at), COUNT(*) FROM queries 
GROUP BY DATE(created_at) ORDER BY date DESC LIMIT 30;
```

### Health Checks
```bash
# Check services
systemctl status tuokit
systemctl status postgresql
ollama list

# Check logs
journalctl -u tuokit -f
```

---

## 🎉 Launch Celebration!  
Your complete TuoKit suite includes:  
✅ Private AI development environment  
✅ Knowledge retention system  
✅ Team demonstration package  
✅ Production deployment guide  

**Final Command to Start Your Journey:**  
```bash
streamlit run app.py
```

---
Deployment Date: _______________
Deployed By: ___________________
Version: 1.1.0
Notes: _________________________