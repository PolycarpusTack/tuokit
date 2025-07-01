# Enterprise SQL Generator - Installation Guide

## Overview
The enhanced SQL Generator works in two modes:
1. **Basic Mode** (default) - All AI features without database connectivity
2. **Enterprise Mode** - Full features with live database connections

## Basic Installation (Already Complete)
The basic SQL Generator is ready to use with your current setup:
- Natural language to SQL
- SQL optimization
- Dialect translation
- Security scanning
- Knowledge base integration

## Enterprise Installation (Optional)

### 1. Install SQLAlchemy (For Database Connectivity)
```bash
pip install sqlalchemy==2.0.30
```

This enables:
- Live database connections
- Schema discovery
- Query execution preview
- Real-time validation

### 2. Install Oracle Support (Optional)
Only needed if you work with Oracle databases:

#### Step 1: Install cx_Oracle
```bash
pip install cx_Oracle==8.3.0
```

#### Step 2: Install Oracle Instant Client
**Windows:**
1. Download from: https://www.oracle.com/database/technologies/instant-client/downloads.html
2. Extract to `C:\oracle\instantclient`
3. Add to PATH: `C:\oracle\instantclient`

**Linux:**
```bash
# Download Basic Light Package
wget https://download.oracle.com/otn_software/linux/instantclient/instantclient-basiclite-linuxx64.zip
unzip instantclient-*.zip
export LD_LIBRARY_PATH=$PWD/instantclient_*:$LD_LIBRARY_PATH
```

**macOS:**
```bash
# Using Homebrew
brew tap InstantClientTap/instantclient
brew install instantclient-basic
```

### 3. Test Your Installation
```bash
python test_sql_enterprise.py
```

## Feature Availability Matrix

| Feature | Basic Mode | Enterprise Mode |
|---------|------------|-----------------|
| Natural Language to SQL | ✅ | ✅ |
| SQL Optimization | ✅ | ✅ Enhanced |
| Dialect Translation | ✅ | ✅ |
| Security Scanning | ✅ | ✅ |
| Schema Discovery | ❌ | ✅ |
| Query Execution | ❌ | ✅ |
| Live Validation | ❌ | ✅ |
| EXPLAIN Plans | ❌ | ✅ |

## Security Considerations

### Connection Security
- Credentials are never stored persistently
- Connections timeout after 30 minutes
- Use read-only database users when possible
- Enable SSL/TLS for production databases

### Query Execution Safety
- Automatic row limits (50 rows)
- Dangerous operations blocked (DROP, TRUNCATE, etc.)
- Parameterized queries enforced
- Audit logging available

## Environment Variables (Optional)
For production deployments, use environment variables:

```bash
# .env file
SQL_GEN_MAX_ROWS=100
SQL_GEN_TIMEOUT=30
SQL_GEN_ALLOW_EXEC=true
SQL_GEN_AUDIT_LOG=true
```

## Troubleshooting

### SQLAlchemy Issues
```
Error: No module named 'sqlalchemy'
Solution: pip install sqlalchemy
```

### Oracle Connection Issues
```
Error: DPI-1047: Cannot locate a 64-bit Oracle Client library
Solution: Install Oracle Instant Client and set LD_LIBRARY_PATH
```

### PostgreSQL Connection Issues
```
Error: psycopg2.OperationalError
Solution: Verify PostgreSQL is running and credentials are correct
```

## Docker Deployment (Advanced)
```dockerfile
FROM python:3.9-slim

# Install Oracle Instant Client
RUN apt-get update && apt-get install -y wget unzip
RUN wget https://download.oracle.com/otn_software/linux/instantclient/instantclient-basiclite-linuxx64.zip
RUN unzip instantclient-*.zip -d /opt/oracle
ENV LD_LIBRARY_PATH=/opt/oracle/instantclient_*:$LD_LIBRARY_PATH

# Install Python dependencies
COPY requirements.txt .
RUN pip install -r requirements.txt
RUN pip install sqlalchemy cx_Oracle

# Copy application
COPY . /app
WORKDIR /app

CMD ["streamlit", "run", "app.py"]
```

## Support
- Basic features work without any additional setup
- Enterprise features are completely optional
- Choose what you need based on your use case
- All features maintain TuoKit's privacy-first approach
