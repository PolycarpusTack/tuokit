# TuoKit Database Setup Guide

## Quick Start

TuoKit uses PostgreSQL to store queries, knowledge units, and various tool-specific data. We provide a unified setup script that handles everything.

### 1. Install PostgreSQL

**Windows:**
```bash
# Download from https://www.postgresql.org/download/windows/
# Or use chocolatey:
choco install postgresql
```

**macOS:**
```bash
brew install postgresql
brew services start postgresql
```

**Linux:**
```bash
sudo apt-get install postgresql postgresql-contrib
sudo systemctl start postgresql
```

### 2. Configure Environment

Copy the example environment file:
```bash
cp .env.example .env
```

Edit `.env` and update with your settings:
```env
# PostgreSQL Database Configuration
TUOKIT_PG_HOST="localhost"         # Or your remote server IP
TUOKIT_PG_PORT="5432"
TUOKIT_PG_DB="tuokit_knowledge"
TUOKIT_PG_USER="tuokit_user"
TUOKIT_PG_PASSWORD="your_secure_password"

# Optional: Admin credentials for automatic setup
PG_ADMIN_USER="postgres"
PG_ADMIN_PASSWORD="your_postgres_password"
```

### 3. Run Unified Setup

The easiest way - run our unified setup script:

```bash
# Basic setup (creates database, user, and all tables)
python setup_unified_database.py

# With sample data for testing
python setup_unified_database.py --with-samples

# If database/user already exist
python setup_unified_database.py --skip-admin
```

### 4. Manual Setup (Alternative)

If you prefer manual setup or the script fails:

```bash
# Connect as PostgreSQL superuser
sudo -u postgres psql

# Run the unified SQL script
\i unified_database_setup.sql

# Or run it from command line
psql -U postgres -f unified_database_setup.sql
```

## Database Schema Overview

The unified setup creates these main tables:

### Core Tables
- `queries` - All AI tool queries and responses
- `knowledge_units` - Organized knowledge entries
- `knowledge_collections` - Collections for grouping knowledge

### Agent System Tables
- `agent_executions` - Agent run history
- `agent_metrics` - Performance metrics
- `agent_collaborations` - Multi-agent interactions
- `pipelines` - Lite agent pipeline workflows
- `pipeline_templates` - Pre-built workflows

### Ruby Development Tables
- `performance_findings` - Code performance issues
- `test_cases` - Generated test cases
- `graphql_apis` - GraphQL schemas
- `concurrency_patterns` - Ruby concurrency patterns
- `pattern_matching_examples` - Ruby 3+ examples
- `memory_patterns` - Memory optimization patterns
- `katas` - Programming challenges
- `view_components` - Rails ViewComponents
- `rails_upgrades` - Upgrade plans
- `c_extensions` - C extension implementations

## Troubleshooting

### "relation 'queries' does not exist"
Run the setup script: `python setup_unified_database.py`

### "password authentication failed"
1. Check your `.env` file has correct credentials
2. Make sure the password matches what's in PostgreSQL
3. Try resetting the password:
   ```sql
   ALTER USER tuokit_user WITH PASSWORD 'new_password';
   ```

### "could not connect to server"
1. Check PostgreSQL is running:
   - Windows: Check Services for "postgresql"
   - macOS: `brew services list`
   - Linux: `sudo systemctl status postgresql`

2. Check connection settings in `.env`:
   - Is the host correct? (localhost or IP)
   - Is the port correct? (default: 5432)

### "permission denied"
Grant permissions to the user:
```sql
GRANT ALL PRIVILEGES ON DATABASE tuokit_knowledge TO tuokit_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO tuokit_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO tuokit_user;
```

### Want to reset everything?
```bash
# Connect as superuser
sudo -u postgres psql

# Drop everything
DROP DATABASE IF EXISTS tuokit_knowledge;
DROP USER IF EXISTS tuokit_user;

# Run setup again
python setup_unified_database.py --with-samples
```

## Using TuoKit Without Database

TuoKit can run without PostgreSQL, but you'll lose:
- Query history
- Knowledge library
- Agent execution tracking
- Usage analytics

To run without database, the tools will still work but nothing will be persisted.

## Remote Database Setup

If using a remote PostgreSQL server:

1. Update `.env` with remote settings:
   ```env
   TUOKIT_PG_HOST="192.168.1.100"  # Your server IP
   TUOKIT_PG_PORT="5432"
   ```

2. Ensure PostgreSQL allows remote connections:
   - Edit `postgresql.conf`: `listen_addresses = '*'`
   - Edit `pg_hba.conf`: Add your client IP range
   - Restart PostgreSQL

3. Run setup from your local machine:
   ```bash
   python setup_unified_database.py
   ```

## Backup and Restore

### Backup
```bash
pg_dump -U tuokit_user -h localhost tuokit_knowledge > tuokit_backup_$(date +%Y%m%d).sql
```

### Restore
```bash
psql -U tuokit_user -h localhost tuokit_knowledge < tuokit_backup_20250102.sql
```

## Migration History

The unified setup includes all migrations:
- v0.4: Added verified column
- Agents: Full agent system support
- Lite Agents: Pipeline workflows
- Knowledge Graph: Concept relationships
- Ruby Tools: Performance and testing
- Advanced Ruby: Pattern matching, Ractors, GraphQL
- Professional Ruby: Memory optimization, Katas, ViewComponents

Each migration is applied in order and is idempotent (safe to run multiple times).