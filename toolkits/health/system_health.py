"""
System Health Dashboard for TuoKit
Monitors overall system resources and health
"""

import streamlit as st
import psutil
import platform
from datetime import datetime
from typing import Dict, Any, List
import os

from .config import RESOURCE_THRESHOLDS

class SystemHealthDashboard:
    """System resource monitoring and diagnostics"""
    
    def __init__(self):
        self.thresholds = RESOURCE_THRESHOLDS
    
    def get_quick_status(self) -> Dict[str, Any]:
        """Get quick system status"""
        cpu = psutil.cpu_percent(interval=1)
        memory = psutil.virtual_memory()
        
        status = {
            'healthy': True,
            'cpu': cpu,
            'memory': memory.percent,
            'issues': []
        }
        
        # Check thresholds
        if cpu > self.thresholds['cpu_critical']:
            status['healthy'] = False
            status['issues'].append(f"Critical CPU usage: {cpu}%")
        elif cpu > self.thresholds['cpu_warning']:
            status['issues'].append(f"High CPU usage: {cpu}%")
        
        if memory.percent > self.thresholds['memory_critical']:
            status['healthy'] = False
            status['issues'].append(f"Critical memory usage: {memory.percent}%")
        elif memory.percent > self.thresholds['memory_warning']:
            status['issues'].append(f"High memory usage: {memory.percent}%")
        
        return status
    
    def get_detailed_status(self) -> Dict[str, Any]:
        """Get detailed system status"""
        # Basic system info
        status = {
            'platform': {
                'system': platform.system(),
                'release': platform.release(),
                'version': platform.version(),
                'machine': platform.machine(),
                'processor': platform.processor(),
                'python_version': platform.python_version()
            }
        }
        
        # CPU info
        cpu_count = psutil.cpu_count()
        cpu_freq = psutil.cpu_freq()
        status['cpu'] = {
            'count': cpu_count,
            'physical_cores': psutil.cpu_count(logical=False),
            'current_percent': psutil.cpu_percent(interval=1),
            'per_cpu': psutil.cpu_percent(interval=1, percpu=True),
            'frequency': {
                'current': cpu_freq.current if cpu_freq else 0,
                'min': cpu_freq.min if cpu_freq else 0,
                'max': cpu_freq.max if cpu_freq else 0
            }
        }
        
        # Memory info
        memory = psutil.virtual_memory()
        swap = psutil.swap_memory()
        status['memory'] = {
            'total': memory.total,
            'available': memory.available,
            'used': memory.used,
            'percent': memory.percent,
            'swap_total': swap.total,
            'swap_used': swap.used,
            'swap_percent': swap.percent
        }
        
        # Disk info
        disks = []
        for partition in psutil.disk_partitions():
            try:
                usage = psutil.disk_usage(partition.mountpoint)
                disks.append({
                    'device': partition.device,
                    'mountpoint': partition.mountpoint,
                    'fstype': partition.fstype,
                    'total': usage.total,
                    'used': usage.used,
                    'free': usage.free,
                    'percent': usage.percent
                })
            except:
                pass
        status['disks'] = disks
        
        # Network info
        net_io = psutil.net_io_counters()
        status['network'] = {
            'bytes_sent': net_io.bytes_sent,
            'bytes_recv': net_io.bytes_recv,
            'packets_sent': net_io.packets_sent,
            'packets_recv': net_io.packets_recv,
            'errin': net_io.errin,
            'errout': net_io.errout
        }
        
        # Process info
        status['processes'] = {
            'total': len(psutil.pids()),
            'top_cpu': self._get_top_processes('cpu'),
            'top_memory': self._get_top_processes('memory')
        }
        
        return status
    
    def run_diagnostics(self) -> Dict[str, Dict[str, Any]]:
        """Run system diagnostics"""
        diagnostics = {}
        
        # Test 1: CPU usage
        cpu = psutil.cpu_percent(interval=1)
        if cpu < self.thresholds['cpu_warning']:
            diagnostics['cpu'] = {
                'passed': True,
                'message': f'CPU usage normal ({cpu}%)'
            }
        else:
            diagnostics['cpu'] = {
                'passed': False,
                'message': f'High CPU usage ({cpu}%)'
            }
        
        # Test 2: Memory usage
        memory = psutil.virtual_memory()
        if memory.percent < self.thresholds['memory_warning']:
            diagnostics['memory'] = {
                'passed': True,
                'message': f'Memory usage normal ({memory.percent:.1f}%)'
            }
        else:
            diagnostics['memory'] = {
                'passed': False,
                'message': f'High memory usage ({memory.percent:.1f}%)'
            }
        
        # Test 3: Disk space
        disk_ok = True
        for partition in psutil.disk_partitions():
            try:
                usage = psutil.disk_usage(partition.mountpoint)
                if usage.percent > self.thresholds['disk_critical']:
                    disk_ok = False
                    break
            except:
                pass
        
        if disk_ok:
            diagnostics['disk'] = {
                'passed': True,
                'message': 'Disk space adequate'
            }
        else:
            diagnostics['disk'] = {
                'passed': False,
                'message': f'Low disk space on {partition.mountpoint} ({usage.percent}%)'
            }
        
        # Test 4: Python environment
        try:
            import numpy
            import pandas
            diagnostics['python_env'] = {
                'passed': True,
                'message': 'Core packages available'
            }
        except ImportError as e:
            diagnostics['python_env'] = {
                'passed': False,
                'message': f'Missing package: {str(e)}'
            }
        
        return diagnostics
    
    def _get_top_processes(self, sort_by='cpu', limit=5) -> List[Dict]:
        """Get top processes by CPU or memory"""
        processes = []
        
        for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
            try:
                processes.append({
                    'pid': proc.info['pid'],
                    'name': proc.info['name'],
                    'cpu': proc.info['cpu_percent'],
                    'memory': proc.info['memory_percent']
                })
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
        
        # Sort by specified metric
        if sort_by == 'cpu':
            processes.sort(key=lambda x: x['cpu'], reverse=True)
        else:
            processes.sort(key=lambda x: x['memory'], reverse=True)
        
        return processes[:limit]
    
    def run(self):
        """Run the system health interface"""
        st.subheader("🖥️ System Health Dashboard")
        
        # System overview
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            cpu = psutil.cpu_percent(interval=1)
            color = "normal" if cpu < self.thresholds['cpu_warning'] else "inverse"
            st.metric("CPU Usage", f"{cpu}%", delta_color=color)
        
        with col2:
            memory = psutil.virtual_memory()
            color = "normal" if memory.percent < self.thresholds['memory_warning'] else "inverse"
            st.metric("Memory Usage", f"{memory.percent:.1f}%", delta_color=color)
        
        with col3:
            disk = psutil.disk_usage('/')
            color = "normal" if disk.percent < self.thresholds['disk_warning'] else "inverse"
            st.metric("Disk Usage", f"{disk.percent:.1f}%", delta_color=color)
        
        with col4:
            st.metric("Processes", len(psutil.pids()))
        
        # Detailed metrics
        tab1, tab2, tab3, tab4 = st.tabs(["CPU", "Memory", "Disk", "Processes"])
        
        with tab1:
            self._show_cpu_details()
        
        with tab2:
            self._show_memory_details()
        
        with tab3:
            self._show_disk_details()
        
        with tab4:
            self._show_process_details()
        
        # System info
        with st.expander("🔧 System Information", expanded=False):
            detailed = self.get_detailed_status()
            platform_info = detailed['platform']
            
            col1, col2 = st.columns(2)
            with col1:
                st.info(f"**OS**: {platform_info['system']} {platform_info['release']}")
                st.info(f"**Machine**: {platform_info['machine']}")
                st.info(f"**Python**: {platform_info['python_version']}")
            
            with col2:
                cpu_info = detailed['cpu']
                st.info(f"**CPU Cores**: {cpu_info['count']} ({cpu_info['physical_cores']} physical)")
                if cpu_info['frequency']['current']:
                    st.info(f"**CPU Frequency**: {cpu_info['frequency']['current']:.0f} MHz")
                
                net_info = detailed['network']
                sent_mb = net_info['bytes_sent'] / (1024 * 1024)
                recv_mb = net_info['bytes_recv'] / (1024 * 1024)
                st.info(f"**Network**: ↑{sent_mb:.1f}MB ↓{recv_mb:.1f}MB")
    
    def _show_cpu_details(self):
        """Show detailed CPU information"""
        st.markdown("### CPU Details")
        
        # Per-CPU usage
        cpu_percents = psutil.cpu_percent(interval=1, percpu=True)
        
        col_count = min(4, len(cpu_percents))
        cols = st.columns(col_count)
        
        for i, cpu_percent in enumerate(cpu_percents):
            col = cols[i % col_count]
            with col:
                st.metric(f"Core {i}", f"{cpu_percent}%")
        
        # CPU frequency
        cpu_freq = psutil.cpu_freq()
        if cpu_freq:
            st.divider()
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Current Frequency", f"{cpu_freq.current:.0f} MHz")
            with col2:
                st.metric("Min Frequency", f"{cpu_freq.min:.0f} MHz")
            with col3:
                st.metric("Max Frequency", f"{cpu_freq.max:.0f} MHz")
    
    def _show_memory_details(self):
        """Show detailed memory information"""
        st.markdown("### Memory Details")
        
        # Virtual memory
        memory = psutil.virtual_memory()
        
        col1, col2, col3, col4 = st.columns(4)
        with col1:
            st.metric("Total", self._format_bytes(memory.total))
        with col2:
            st.metric("Used", self._format_bytes(memory.used))
        with col3:
            st.metric("Available", self._format_bytes(memory.available))
        with col4:
            st.metric("Cached", self._format_bytes(memory.cached if hasattr(memory, 'cached') else 0))
        
        # Memory bar
        st.progress(memory.percent / 100)
        
        # Swap memory
        swap = psutil.swap_memory()
        if swap.total > 0:
            st.divider()
            st.markdown("**Swap Memory**")
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Total", self._format_bytes(swap.total))
            with col2:
                st.metric("Used", self._format_bytes(swap.used))
            with col3:
                st.metric("Percent", f"{swap.percent}%")
    
    def _show_disk_details(self):
        """Show detailed disk information"""
        st.markdown("### Disk Details")
        
        for partition in psutil.disk_partitions():
            try:
                usage = psutil.disk_usage(partition.mountpoint)
                
                with st.expander(f"📁 {partition.device} ({partition.mountpoint})", expanded=True):
                    col1, col2, col3, col4 = st.columns(4)
                    
                    with col1:
                        st.metric("Total", self._format_bytes(usage.total))
                    with col2:
                        st.metric("Used", self._format_bytes(usage.used))
                    with col3:
                        st.metric("Free", self._format_bytes(usage.free))
                    with col4:
                        st.metric("Type", partition.fstype)
                    
                    # Usage bar
                    st.progress(usage.percent / 100)
                    
            except PermissionError:
                st.warning(f"No permission to access {partition.mountpoint}")
    
    def _show_process_details(self):
        """Show process information"""
        st.markdown("### Process Details")
        
        # Top CPU processes
        st.markdown("**Top CPU Consumers**")
        top_cpu = self._get_top_processes('cpu')
        
        for proc in top_cpu:
            col1, col2, col3 = st.columns([3, 1, 1])
            with col1:
                st.caption(f"{proc['name']} (PID: {proc['pid']})")
            with col2:
                st.caption(f"CPU: {proc['cpu']:.1f}%")
            with col3:
                st.caption(f"Memory: {proc['memory']:.1f}%")
        
        st.divider()
        
        # Top Memory processes
        st.markdown("**Top Memory Consumers**")
        top_memory = self._get_top_processes('memory')
        
        for proc in top_memory:
            col1, col2, col3 = st.columns([3, 1, 1])
            with col1:
                st.caption(f"{proc['name']} (PID: {proc['pid']})")
            with col2:
                st.caption(f"CPU: {proc['cpu']:.1f}%")
            with col3:
                st.caption(f"Memory: {proc['memory']:.1f}%")
    
    def _format_bytes(self, bytes_value: int) -> str:
        """Format bytes to human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB', 'TB']:
            if bytes_value < 1024.0:
                return f"{bytes_value:.1f} {unit}"
            bytes_value /= 1024.0
        return f"{bytes_value:.1f} PB"