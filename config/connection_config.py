"""
TuoKit Connection Configuration
Supports both local and remote deployments
"""
import os
from dataclasses import dataclass
from typing import Optional

@dataclass
class TuoKitConfig:
    """Configuration for TuoKit connections"""
    # Ollama Configuration
    ollama_host: str = os.getenv("TUOKIT_OLLAMA_HOST", "http://localhost:11434")
    ollama_api_key: Optional[str] = os.getenv("TUOKIT_OLLAMA_API_KEY", None)
    
    # PostgreSQL Configuration
    postgres_host: str = os.getenv("TUOKIT_PG_HOST", "localhost")
    postgres_port: int = int(os.getenv("TUOKIT_PG_PORT", "5432"))
    postgres_db: str = os.getenv("TUOKIT_PG_DB", "tuokit_knowledge")
    postgres_user: str = os.getenv("TUOKIT_PG_USER", "tuokit_user")
    postgres_password: str = os.getenv("TUOKIT_PG_PASSWORD", "")
    
    # Optional: Shared Storage
    shared_storage_path: str = os.getenv("TUOKIT_STORAGE", "./data")
    
    # Security
    enable_ssl: bool = os.getenv("TUOKIT_SSL", "true").lower() == "true"
    verify_certificates: bool = os.getenv("TUOKIT_VERIFY_SSL", "true").lower() == "true"
    
    @property
    def postgres_url(self) -> str:
        """Generate PostgreSQL connection URL"""
        return f"postgresql://{self.postgres_user}:{self.postgres_password}@{self.postgres_host}:{self.postgres_port}/{self.postgres_db}"
    
    @property
    def ollama_url(self) -> str:
        """Generate Ollama API URL"""
        return f"{self.ollama_host}/api"

# Global config instance
config = TuoKitConfig()
