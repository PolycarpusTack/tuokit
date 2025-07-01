"""
System utilities for TuoKit
Platform-independent system information gathering
"""

import subprocess
import platform
from typing import Dict, Optional
import psutil  # Add to requirements.txt if not present

def get_system_stats() -> Dict[str, str]:
    """Get basic system resource usage - cross-platform"""
    try:
        # Try using psutil first (most reliable cross-platform)
        try:
            import psutil
            cpu_percent = psutil.cpu_percent(interval=1)
            memory = psutil.virtual_memory()
            
            return {
                "cpu": f"{cpu_percent:.1f}%",
                "memory": f"{memory.percent:.1f}%",
                "memory_used": f"{memory.used / (1024**3):.1f}GB",
                "memory_total": f"{memory.total / (1024**3):.1f}GB"
            }
        except ImportError:
            # Fallback to platform-specific commands
            return _get_system_stats_native()
            
    except Exception as e:
        return {
            "cpu": "N/A",
            "memory": "N/A",
            "error": str(e)
        }

def _get_system_stats_native() -> Dict[str, str]:
    """Platform-specific system stats as fallback"""
    system = platform.system()
    
    try:
        if system == "Windows":
            # Windows-specific commands
            cpu_cmd = "wmic cpu get loadpercentage /value"
            cpu_result = subprocess.check_output(cpu_cmd, shell=True).decode().strip()
            cpu_lines = [line for line in cpu_result.split('\n') if 'LoadPercentage=' in line]
            cpu = cpu_lines[0].split('=')[1] if cpu_lines else "N/A"
            
            # Get memory usage
            mem_cmd = 'wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value'
            mem_result = subprocess.check_output(mem_cmd, shell=True).decode().strip()
            mem_lines = mem_result.split('\n')
            
            total_mem = 0
            free_mem = 0
            for line in mem_lines:
                if 'TotalVisibleMemorySize=' in line:
                    total_mem = int(line.split('=')[1])
                elif 'FreePhysicalMemory=' in line:
                    free_mem = int(line.split('=')[1])
                    
            if total_mem > 0:
                mem_percent = ((total_mem - free_mem) / total_mem) * 100
                mem = f"{mem_percent:.1f}%"
            else:
                mem = "N/A"
                
        elif system == "Darwin":  # macOS
            # CPU usage
            cpu_cmd = "top -l 1 | grep 'CPU usage' | awk '{print $3}'"
            cpu = subprocess.check_output(cpu_cmd, shell=True).decode().strip()
            
            # Memory usage
            mem_cmd = "vm_stat | grep 'Pages active' | awk '{print $3}'"
            active_pages = int(subprocess.check_output(mem_cmd, shell=True).decode().strip().replace('.', ''))
            page_size = 4096  # bytes
            active_gb = (active_pages * page_size) / (1024**3)
            mem = f"{active_gb:.1f}GB active"
            
        else:  # Linux
            # CPU usage
            cpu_cmd = "top -bn1 | grep 'Cpu(s)' | awk '{print $2}'"
            cpu = subprocess.check_output(cpu_cmd, shell=True).decode().strip()
            
            # Memory usage
            mem_cmd = "free -m | awk 'NR==2{printf \"%.1f%%\", $3*100/$2}'"
            mem = subprocess.check_output(mem_cmd, shell=True).decode().strip()
            
        return {"cpu": f"{cpu}", "memory": mem}
        
    except Exception:
        return {"cpu": "N/A", "memory": "N/A"}

def get_platform_info() -> Dict[str, str]:
    """Get detailed platform information"""
    return {
        "system": platform.system(),
        "release": platform.release(),
        "version": platform.version(),
        "machine": platform.machine(),
        "processor": platform.processor(),
        "python_version": platform.python_version()
    }
