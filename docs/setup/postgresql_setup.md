# PostgreSQL Setup for TuoKit

## Quick Start

### 1. Install PostgreSQL

**Windows:**
```bash
# Download from https://www.postgresql.org/download/windows/
# Or use Chocolatey:
choco install postgresql
```

**macOS:**
```bash
brew install postgresql
brew services start postgresql
```

**Linux:**
```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
sudo systemctl start postgresql
```

### 2. Create Database and User

```bash
# Access PostgreSQL as superuser
sudo -u postgres psql

# Create database and user
CREATE DATABASE tuokit_knowledge;
CREATE USER tuokit_user WITH PASSWORD 'your_secure_password';
GRANT ALL PRIVILEGES ON DATABASE tuokit_knowledge TO tuokit_user;

# Exit
\q
```

### 3. Configure .env File

Copy `.env.example` to `.env` and update:
```env
TUOKIT_PG_HOST=localhost
TUOKIT_PG_PORT=5432
TUOKIT_PG_DB=tuokit_knowledge
TUOKIT_PG_USER=tuokit_user
TUOKIT_PG_PASSWORD=your_secure_password
```

### 4. Test Connection

```bash
python scripts/migration/test_database.py
```

### 5. Run Migrations

```bash
# Initial schema
python scripts/setup/setup_database.py

# Knowledge Capture System
python scripts/migration/create_knowledge_capture_tables.py
```

## Troubleshooting

**Connection Refused:**
- Ensure PostgreSQL is running: `pg_isready`
- Check if it's listening on the right port: `netstat -an | grep 5432`

**Authentication Failed:**
- Verify credentials in .env match database
- Check pg_hba.conf allows local connections

**Database Does Not Exist:**
- Create it manually using psql as shown above