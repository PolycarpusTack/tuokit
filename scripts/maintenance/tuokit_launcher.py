#!/usr/bin/env python3
"""
TuoKit Universal Launcher
Comprehensive launcher for setup, diagnostics, and running TuoKit
"""

import os
import sys
import subprocess
import platform
import shutil
import time
import argparse
from pathlib import Path
from datetime import datetime
import json
import webbrowser

# ANSI color codes for terminal output
class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    CYAN = '\033[96m'
    GREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

# Launcher configuration
LAUNCHER_CONFIG = {
    "app_name": "TuoKit",
    "version": "3.0",
    "default_port": 8501,
    "python_min_version": (3, 8),
    "venv_name": "tuokit-env",
    "requirements_file": "requirements.txt",
    "main_app": "tuokit_unified.py",
    "legacy_apps": {
        "classic": "app.py",
        "modern": "app_modern.py",
        "mockup": "tuokit_mockup.py",
        "distributed": "main.py"
    }
}

class TuoKitLauncher:
    def __init__(self):
        self.platform = platform.system()
        self.is_windows = self.platform == "Windows"
        self.is_wsl = 'microsoft-standard' in platform.uname().release.lower()
        self.python_cmd = sys.executable
        self.venv_path = Path(LAUNCHER_CONFIG["venv_name"])
        self.streamlit_cmd = None
        self.pip_cmd = None
        
    def print_banner(self):
        """Print TuoKit banner"""
        banner = f"""
{Colors.CYAN}╔══════════════════════════════════════════════════════════════╗
║                                                              ║
║  {Colors.BOLD}████████╗██╗   ██╗ ██████╗ ██╗  ██╗██╗████████╗{Colors.CYAN}          ║
║  {Colors.BOLD}╚══██╔══╝██║   ██║██╔═══██╗██║ ██╔╝██║╚══██╔══╝{Colors.CYAN}          ║
║  {Colors.BOLD}   ██║   ██║   ██║██║   ██║█████╔╝ ██║   ██║{Colors.CYAN}             ║
║  {Colors.BOLD}   ██║   ██║   ██║██║   ██║██╔═██╗ ██║   ██║{Colors.CYAN}             ║
║  {Colors.BOLD}   ██║   ╚██████╔╝╚██████╔╝██║  ██╗██║   ██║{Colors.CYAN}             ║
║  {Colors.BOLD}   ╚═╝    ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝   ╚═╝{Colors.CYAN}             ║
║                                                              ║
║  {Colors.GREEN}Universal AI Toolkit for Developers v{LAUNCHER_CONFIG['version']}{Colors.CYAN}                   ║
╚══════════════════════════════════════════════════════════════╝{Colors.ENDC}
"""
        print(banner)
    
    def check_python_version(self):
        """Check if Python version meets requirements"""
        current = sys.version_info[:2]
        required = LAUNCHER_CONFIG["python_min_version"]
        
        if current < required:
            print(f"{Colors.FAIL}❌ Python {required[0]}.{required[1]}+ required, found {current[0]}.{current[1]}{Colors.ENDC}")
            return False
        
        print(f"{Colors.GREEN}✅ Python {current[0]}.{current[1]} detected{Colors.ENDC}")
        return True
    
    def check_venv(self):
        """Check if virtual environment exists"""
        if self.venv_path.exists():
            print(f"{Colors.GREEN}✅ Virtual environment found{Colors.ENDC}")
            return True
        else:
            print(f"{Colors.WARNING}⚠️  Virtual environment not found{Colors.ENDC}")
            return False
    
    def create_venv(self):
        """Create virtual environment"""
        print(f"{Colors.BLUE}📦 Creating virtual environment...{Colors.ENDC}")
        try:
            subprocess.run([self.python_cmd, "-m", "venv", str(self.venv_path)], check=True)
            print(f"{Colors.GREEN}✅ Virtual environment created{Colors.ENDC}")
            return True
        except subprocess.CalledProcessError as e:
            print(f"{Colors.FAIL}❌ Failed to create virtual environment: {e}{Colors.ENDC}")
            return False
    
    def get_venv_python(self):
        """Get path to Python in virtual environment"""
        if self.is_windows:
            return self.venv_path / "Scripts" / "python.exe"
        else:
            return self.venv_path / "bin" / "python"
    
    def get_venv_pip(self):
        """Get path to pip in virtual environment"""
        if self.is_windows:
            return self.venv_path / "Scripts" / "pip.exe"
        else:
            return self.venv_path / "bin" / "pip"
    
    def get_venv_streamlit(self):
        """Get path to streamlit in virtual environment"""
        if self.is_windows:
            return self.venv_path / "Scripts" / "streamlit.exe"
        else:
            return self.venv_path / "bin" / "streamlit"
    
    def activate_venv_message(self):
        """Show how to activate virtual environment"""
        print(f"\n{Colors.CYAN}To activate the virtual environment manually:{Colors.ENDC}")
        if self.is_windows:
            print(f"  {LAUNCHER_CONFIG['venv_name']}\\Scripts\\activate.bat")
        else:
            print(f"  source {LAUNCHER_CONFIG['venv_name']}/bin/activate")
    
    def install_requirements(self):
        """Install required packages"""
        print(f"{Colors.BLUE}📦 Installing requirements...{Colors.ENDC}")
        
        pip_cmd = str(self.get_venv_pip())
        
        # First upgrade pip
        print(f"{Colors.BLUE}   Upgrading pip...{Colors.ENDC}")
        try:
            subprocess.run([pip_cmd, "install", "--upgrade", "pip"], check=True, capture_output=True)
        except:
            pass  # Ignore pip upgrade errors
        
        # Install requirements
        if Path(LAUNCHER_CONFIG["requirements_file"]).exists():
            try:
                print(f"{Colors.BLUE}   Installing from requirements.txt...{Colors.ENDC}")
                subprocess.run([pip_cmd, "install", "-r", LAUNCHER_CONFIG["requirements_file"]], check=True)
                print(f"{Colors.GREEN}✅ Requirements installed{Colors.ENDC}")
                return True
            except subprocess.CalledProcessError as e:
                print(f"{Colors.FAIL}❌ Failed to install requirements: {e}{Colors.ENDC}")
                return False
        else:
            # Install essential packages
            print(f"{Colors.BLUE}   Installing essential packages...{Colors.ENDC}")
            essential = [
                "streamlit>=1.28.0",
                "pandas>=1.5.3",
                "psycopg2-binary>=2.9.9",
                "python-dotenv>=1.0.0",
                "requests>=2.31.0",
                "ollama>=0.1.7",
                "plotly>=5.17.0"
            ]
            
            for package in essential:
                try:
                    subprocess.run([pip_cmd, "install", package], check=True, capture_output=True)
                    print(f"{Colors.GREEN}   ✅ {package}{Colors.ENDC}")
                except:
                    print(f"{Colors.WARNING}   ⚠️  {package} failed{Colors.ENDC}")
            
            return True
    
    def check_ollama(self):
        """Check if Ollama is installed and running"""
        print(f"{Colors.BLUE}🤖 Checking Ollama...{Colors.ENDC}")
        
        # Check if ollama command exists
        ollama_installed = shutil.which("ollama") is not None
        
        if not ollama_installed:
            print(f"{Colors.WARNING}⚠️  Ollama not installed{Colors.ENDC}")
            print(f"   Install from: https://ollama.ai")
            return False
        
        # Check if ollama is running
        try:
            import requests
            response = requests.get("http://localhost:11434/api/tags", timeout=2)
            if response.status_code == 200:
                models = response.json().get('models', [])
                print(f"{Colors.GREEN}✅ Ollama running with {len(models)} models{Colors.ENDC}")
                return True
            else:
                print(f"{Colors.WARNING}⚠️  Ollama installed but not running{Colors.ENDC}")
                print(f"   Start with: ollama serve")
                return False
        except:
            print(f"{Colors.WARNING}⚠️  Ollama installed but not running{Colors.ENDC}")
            print(f"   Start with: ollama serve")
            return False
    
    def check_database(self):
        """Check database configuration"""
        print(f"{Colors.BLUE}🗄️  Checking database...{Colors.ENDC}")
        
        # Check for .env file
        if not Path(".env").exists():
            print(f"{Colors.WARNING}⚠️  No .env file found{Colors.ENDC}")
            print(f"   Run setup wizard or copy .env.example")
            return False
        
        # Try to load and check database settings
        try:
            from dotenv import load_dotenv
            load_dotenv()
            
            db_type = os.getenv('TUOKIT_DB_TYPE', 'postgresql')
            
            if db_type == 'postgresql':
                host = os.getenv('TUOKIT_PG_HOST', 'localhost')
                port = os.getenv('TUOKIT_PG_PORT', '5432')
                db = os.getenv('TUOKIT_PG_DB', 'tuokit_knowledge')
                print(f"{Colors.GREEN}✅ PostgreSQL configured: {host}:{port}/{db}{Colors.ENDC}")
            elif db_type == 'sqlite':
                db_path = os.getenv('TUOKIT_SQLITE_PATH', 'tuokit.db')
                if Path(db_path).exists():
                    print(f"{Colors.GREEN}✅ SQLite database found: {db_path}{Colors.ENDC}")
                else:
                    print(f"{Colors.WARNING}⚠️  SQLite database not found: {db_path}{Colors.ENDC}")
                    
            return True
        except Exception as e:
            print(f"{Colors.FAIL}❌ Database check failed: {e}{Colors.ENDC}")
            return False
    
    def run_diagnostics(self):
        """Run diagnostic toolkit"""
        print(f"\n{Colors.BLUE}🔧 Running diagnostics...{Colors.ENDC}")
        
        python_cmd = str(self.get_venv_python())
        
        try:
            subprocess.run([python_cmd, "diagnostic_toolkit.py", "--cli"], check=True)
            return True
        except subprocess.CalledProcessError:
            print(f"{Colors.WARNING}⚠️  Diagnostics reported issues{Colors.ENDC}")
            return False
        except FileNotFoundError:
            print(f"{Colors.WARNING}⚠️  Diagnostic toolkit not found{Colors.ENDC}")
            return False
    
    def run_setup(self):
        """Run setup wizard"""
        print(f"\n{Colors.BLUE}🛠️  Running setup wizard...{Colors.ENDC}")
        
        python_cmd = str(self.get_venv_python())
        
        # Check which setup tool is available
        if Path("setup_manager.py").exists():
            subprocess.run([python_cmd, "setup_manager.py", "--interactive"])
        elif Path("setup_local.py").exists():
            streamlit_cmd = str(self.get_venv_streamlit())
            subprocess.run([streamlit_cmd, "run", "setup_local.py"])
        else:
            print(f"{Colors.WARNING}⚠️  No setup tool found{Colors.ENDC}")
    
    def run_migration(self):
        """Run migration tool"""
        print(f"\n{Colors.BLUE}🔄 Running migration tool...{Colors.ENDC}")
        
        python_cmd = str(self.get_venv_python())
        
        if Path("migration_manager.py").exists():
            subprocess.run([python_cmd, "migration_manager.py", "--status"])
        else:
            print(f"{Colors.WARNING}⚠️  Migration tool not found{Colors.ENDC}")
    
    def launch_app(self, app_choice="unified", port=None, open_browser=True):
        """Launch TuoKit application"""
        # Determine which app to launch
        if app_choice == "unified":
            app_file = LAUNCHER_CONFIG["main_app"]
        elif app_choice in LAUNCHER_CONFIG["legacy_apps"]:
            app_file = LAUNCHER_CONFIG["legacy_apps"][app_choice]
        else:
            print(f"{Colors.FAIL}❌ Unknown app choice: {app_choice}{Colors.ENDC}")
            return False
        
        if not Path(app_file).exists():
            print(f"{Colors.FAIL}❌ App file not found: {app_file}{Colors.ENDC}")
            return False
        
        # Use specified port or default
        if port is None:
            port = LAUNCHER_CONFIG["default_port"]
        
        print(f"\n{Colors.GREEN}🚀 Launching TuoKit ({app_choice})...{Colors.ENDC}")
        print(f"{Colors.CYAN}   URL: http://localhost:{port}{Colors.ENDC}")
        print(f"{Colors.CYAN}   Press Ctrl+C to stop{Colors.ENDC}\n")
        
        streamlit_cmd = str(self.get_venv_streamlit())
        
        # Build command
        cmd = [
            streamlit_cmd, "run", app_file,
            "--server.port", str(port),
            "--server.headless", "true"
        ]
        
        if not open_browser:
            cmd.extend(["--server.open_browser", "false"])
        
        try:
            # Launch streamlit
            process = subprocess.Popen(cmd)
            
            # Wait a bit for server to start
            time.sleep(3)
            
            # Open browser if requested
            if open_browser:
                webbrowser.open(f"http://localhost:{port}")
            
            # Wait for process
            process.wait()
            
        except KeyboardInterrupt:
            print(f"\n{Colors.CYAN}Shutting down TuoKit...{Colors.ENDC}")
            process.terminate()
            process.wait()
            print(f"{Colors.GREEN}✅ TuoKit stopped{Colors.ENDC}")
        except Exception as e:
            print(f"{Colors.FAIL}❌ Error launching TuoKit: {e}{Colors.ENDC}")
            return False
        
        return True
    
    def quick_start(self):
        """Quick start - check essentials and launch"""
        print(f"{Colors.HEADER}⚡ Quick Start Mode{Colors.ENDC}\n")
        
        # Essential checks
        checks = [
            ("Python version", self.check_python_version()),
            ("Virtual environment", self.check_venv()),
            ("Ollama", self.check_ollama()),
            ("Database", self.check_database())
        ]
        
        all_good = all(check[1] for check in checks)
        
        if all_good:
            print(f"\n{Colors.GREEN}✅ All checks passed!{Colors.ENDC}")
            self.launch_app()
        else:
            print(f"\n{Colors.WARNING}⚠️  Some checks failed. Run full setup? (y/n){Colors.ENDC}")
            if input().lower() == 'y':
                self.full_setup()
    
    def full_setup(self):
        """Full setup process"""
        print(f"{Colors.HEADER}🛠️  Full Setup Mode{Colors.ENDC}\n")
        
        # Step 1: Python check
        if not self.check_python_version():
            return
        
        # Step 2: Virtual environment
        if not self.check_venv():
            if not self.create_venv():
                return
        
        # Step 3: Install requirements
        if not self.install_requirements():
            print(f"{Colors.WARNING}⚠️  Some packages failed to install{Colors.ENDC}")
        
        # Step 4: Database setup
        print(f"\n{Colors.CYAN}Configure database? (y/n){Colors.ENDC}")
        if input().lower() == 'y':
            self.run_setup()
        
        # Step 5: Check Ollama
        self.check_ollama()
        
        # Step 6: Run diagnostics
        print(f"\n{Colors.CYAN}Run diagnostics? (y/n){Colors.ENDC}")
        if input().lower() == 'y':
            self.run_diagnostics()
        
        print(f"\n{Colors.GREEN}✅ Setup complete!{Colors.ENDC}")
        print(f"\n{Colors.CYAN}Launch TuoKit now? (y/n){Colors.ENDC}")
        if input().lower() == 'y':
            self.launch_app()
    
    def interactive_menu(self):
        """Interactive menu mode"""
        while True:
            print(f"\n{Colors.HEADER}🎮 TuoKit Launcher Menu{Colors.ENDC}")
            print(f"\n{Colors.CYAN}Quick Actions:{Colors.ENDC}")
            print("  1. 🚀 Quick Start (launch with defaults)")
            print("  2. 🛠️  Full Setup Wizard")
            print("  3. 🔧 Run Diagnostics")
            
            print(f"\n{Colors.CYAN}Launch Options:{Colors.ENDC}")
            print("  4. 🚀 Launch Unified App (recommended)")
            print("  5. 📱 Launch Classic App")
            print("  6. 🎨 Launch Modern UI")
            print("  7. 🧪 Launch Mockup")
            
            print(f"\n{Colors.CYAN}Management:{Colors.ENDC}")
            print("  8. 📦 Install/Update Requirements")
            print("  9. 🔄 Run Migrations")
            print("  10. ⚙️  Configure Database")
            
            print(f"\n{Colors.CYAN}System:{Colors.ENDC}")
            print("  11. ℹ️  Show System Info")
            print("  12. 📖 Open Documentation")
            print("  0. ❌ Exit")
            
            choice = input(f"\n{Colors.BOLD}Enter choice (0-12): {Colors.ENDC}")
            
            if choice == '0':
                print(f"{Colors.GREEN}Goodbye!{Colors.ENDC}")
                break
            elif choice == '1':
                self.quick_start()
            elif choice == '2':
                self.full_setup()
            elif choice == '3':
                self.run_diagnostics()
            elif choice == '4':
                self.launch_app("unified")
            elif choice == '5':
                self.launch_app("classic")
            elif choice == '6':
                self.launch_app("modern")
            elif choice == '7':
                self.launch_app("mockup")
            elif choice == '8':
                self.install_requirements()
            elif choice == '9':
                self.run_migration()
            elif choice == '10':
                self.run_setup()
            elif choice == '11':
                self.show_system_info()
            elif choice == '12':
                webbrowser.open("https://github.com/yourusername/tuokit/wiki")
            else:
                print(f"{Colors.WARNING}Invalid choice{Colors.ENDC}")
    
    def show_system_info(self):
        """Show system information"""
        print(f"\n{Colors.HEADER}📊 System Information{Colors.ENDC}")
        print(f"\n{Colors.CYAN}Platform:{Colors.ENDC}")
        print(f"  OS: {platform.system()} {platform.release()}")
        print(f"  Machine: {platform.machine()}")
        print(f"  Python: {sys.version}")
        print(f"  WSL: {'Yes' if self.is_wsl else 'No'}")
        
        print(f"\n{Colors.CYAN}TuoKit:{Colors.ENDC}")
        print(f"  Version: {LAUNCHER_CONFIG['version']}")
        print(f"  Main App: {LAUNCHER_CONFIG['main_app']}")
        print(f"  Venv: {self.venv_path}")
        
        # Check installed packages
        if self.check_venv():
            print(f"\n{Colors.CYAN}Key Packages:{Colors.ENDC}")
            pip_cmd = str(self.get_venv_pip())
            try:
                result = subprocess.run([pip_cmd, "list", "--format=json"], 
                                      capture_output=True, text=True)
                if result.returncode == 0:
                    packages = json.loads(result.stdout)
                    key_packages = ['streamlit', 'pandas', 'ollama', 'psycopg2-binary']
                    for pkg in packages:
                        if pkg['name'] in key_packages:
                            print(f"  {pkg['name']}: {pkg['version']}")
            except:
                pass

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="TuoKit Universal Launcher",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python tuokit_launcher.py              # Interactive menu
  python tuokit_launcher.py --quick      # Quick start
  python tuokit_launcher.py --setup      # Full setup
  python tuokit_launcher.py --launch     # Launch directly
  python tuokit_launcher.py --diagnose   # Run diagnostics
  
Launch specific versions:
  python tuokit_launcher.py --launch --app classic
  python tuokit_launcher.py --launch --app modern
  python tuokit_launcher.py --launch --port 8502
        """
    )
    
    parser.add_argument('--quick', action='store_true', help='Quick start mode')
    parser.add_argument('--setup', action='store_true', help='Run full setup')
    parser.add_argument('--launch', action='store_true', help='Launch TuoKit directly')
    parser.add_argument('--diagnose', action='store_true', help='Run diagnostics')
    parser.add_argument('--app', choices=['unified', 'classic', 'modern', 'mockup'], 
                       default='unified', help='App version to launch')
    parser.add_argument('--port', type=int, help='Port to run on')
    parser.add_argument('--no-browser', action='store_true', help='Don\'t open browser')
    
    args = parser.parse_args()
    
    # Create launcher instance
    launcher = TuoKitLauncher()
    
    # Always show banner
    launcher.print_banner()
    
    # Handle command line arguments
    if args.quick:
        launcher.quick_start()
    elif args.setup:
        launcher.full_setup()
    elif args.launch:
        launcher.launch_app(args.app, args.port, not args.no_browser)
    elif args.diagnose:
        launcher.run_diagnostics()
    else:
        # No arguments - show interactive menu
        launcher.interactive_menu()

if __name__ == "__main__":
    main()