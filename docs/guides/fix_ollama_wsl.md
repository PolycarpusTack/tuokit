# Fixing Ollama Connection in WSL

## The Issue
Ollama is running on Windows but not accessible from WSL2. This is a common networking issue.

## Solution Options

### Option 1: Run Ollama with Network Binding (Recommended)
On Windows (PowerShell), stop Ollama and restart it with:
```powershell
# Stop existing Ollama
taskkill /F /IM ollama.exe

# Start Ollama binding to all interfaces
ollama serve --host 0.0.0.0
```

### Option 2: Use Windows Terminal/PowerShell
If you prefer, run TuoKit directly in Windows instead of WSL:
```powershell
cd C:\Projects\Tuokit
python -m venv venv
.\venv\Scripts\activate
pip install -r requirements.txt
streamlit run app.py
```

### Option 3: Port Forwarding (Advanced)
Add Windows Firewall rule and port forwarding:
```powershell
# Run as Administrator
netsh interface portproxy add v4tov4 listenport=11434 listenaddress=0.0.0.0 connectport=11434 connectaddress=127.0.0.1
New-NetFirewallRule -DisplayName "Ollama WSL" -Direction Inbound -Protocol TCP -LocalPort 11434 -Action Allow
```

## Quick Test
After applying any solution above, test from WSL:
```bash
# Get Windows host IP
export WINDOWS_HOST=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}')

# Test connection
curl http://$WINDOWS_HOST:11434/api/tags

# If successful, update .env
echo "TUOKIT_OLLAMA_HOST=http://$WINDOWS_HOST:11434" >> .env
```

## Alternative: Install Ollama in WSL
If the above doesn't work, install Ollama directly in WSL:
```bash
curl -fsSL https://ollama.com/install.sh | sh
ollama serve &
ollama pull deepseek-r1:latest
```

Then update .env:
```
TUOKIT_OLLAMA_HOST=http://localhost:11434
```