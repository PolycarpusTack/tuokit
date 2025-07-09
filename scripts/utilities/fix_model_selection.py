"""
Fix for TuoKit model selection issue
Ensures dropdown shows actual Ollama models and tools use the selected model
"""

import os
import sys

def fix_app_py():
    """Fix the main app.py to use dynamic model detection"""
    print("Fixing app.py...")
    
    with open("app.py", "r", encoding="utf-8") as f:
        content = f.read()
    
    # Replace the hardcoded model selection with dynamic detection
    old_code = '''    # Model selection
    st.subheader("AI Engine")
    model_options = ["deepseek-coder:6.7b", "deepseek-r1:6.7b", "deepseek-r1:1.5b"]
    st.selectbox("Active Model", model_options, key="selected_model")'''
    
    new_code = '''    # Model selection
    st.subheader("AI Engine")
    # Get available models from Ollama
    model_options = get_available_models()
    if model_options:
        # If selected model not in available models, use first available
        if st.session_state.selected_model not in model_options:
            st.session_state.selected_model = model_options[0]
        st.selectbox("Active Model", model_options, key="selected_model")
    else:
        st.warning("No Ollama models found. Please check Ollama status.")
        # Fallback to defaults
        model_options = ["deepseek-coder:6.7b", "deepseek-r1:6.7b", "deepseek-r1:1.5b"]
        st.selectbox("Active Model", model_options, key="selected_model")'''
    
    if old_code in content:
        content = content.replace(old_code, new_code)
        
        with open("app.py", "w", encoding="utf-8") as f:
            f.write(content)
        print("[OK] Fixed app.py model selection")
    else:
        print("[!] Could not find exact code to replace in app.py")

def fix_crash_analyzer():
    """Fix crash_analyzer.py to use selected model"""
    print("\nFixing crash_analyzer.py...")
    
    with open("pages/crash_analyzer.py", "r", encoding="utf-8") as f:
        content = f.read()
    
    # Replace hardcoded model with session state model
    replacements = [
        ('model="deepseek-r1:latest"', 'model=st.session_state.get("selected_model", "deepseek-r1:latest")'),
        ('model="deepseek-coder:latest"', 'model=st.session_state.get("selected_model", "deepseek-coder:6.7b")'),
    ]
    
    modified = False
    for old, new in replacements:
        if old in content:
            content = content.replace(old, new)
            modified = True
    
    if modified:
        with open("pages/crash_analyzer.py", "w", encoding="utf-8") as f:
            f.write(content)
        print("[OK] Fixed crash_analyzer.py to use selected model")
    else:
        print("[!] No hardcoded models found in crash_analyzer.py")

def fix_all_tools():
    """Fix all tools to use session state model"""
    print("\nChecking all tools for hardcoded models...")
    
    tools_fixed = 0
    for root, dirs, files in os.walk("pages"):
        for file in files:
            if file.endswith(".py"):
                filepath = os.path.join(root, file)
                
                try:
                    with open(filepath, "r", encoding="utf-8") as f:
                        content = f.read()
                    
                    original_content = content
                    
                    # Common patterns to fix
                    replacements = [
                        # Pattern 1: safe_ollama_generate with hardcoded model
                        ('safe_ollama_generate(\n            model="deepseek-r1:latest"',
                         'safe_ollama_generate(\n            model=st.session_state.get("selected_model", "deepseek-r1:latest")'),
                        
                        # Pattern 2: inline model specification
                        ('model="deepseek-r1:latest"', 
                         'model=st.session_state.get("selected_model", "deepseek-r1:latest")'),
                        
                        ('model="deepseek-coder:latest"',
                         'model=st.session_state.get("selected_model", "deepseek-coder:6.7b")'),
                        
                        ('model="deepseek-coder:6.7b"',
                         'model=st.session_state.get("selected_model", "deepseek-coder:6.7b")'),
                        
                        ('model="deepseek-r1:6.7b"',
                         'model=st.session_state.get("selected_model", "deepseek-r1:6.7b")'),
                        
                        ('model="deepseek-r1:1.5b"',
                         'model=st.session_state.get("selected_model", "deepseek-r1:1.5b")'),
                    ]
                    
                    for old, new in replacements:
                        content = content.replace(old, new)
                    
                    if content != original_content:
                        with open(filepath, "w", encoding="utf-8") as f:
                            f.write(content)
                        tools_fixed += 1
                        print(f"  [OK] Fixed {file}")
                        
                except Exception as e:
                    print(f"  [ERROR] Error processing {file}: {e}")
    
    print(f"\n[INFO] Fixed {tools_fixed} tool files")

def main():
    """Run all fixes"""
    print("[TUOKIT] TuoKit Model Selection Fix")
    print("=" * 50)
    
    # Change to project directory
    os.chdir("C:/Projects/Tuokit")
    
    # Run fixes
    fix_app_py()
    fix_crash_analyzer()
    fix_all_tools()
    
    print("\n[DONE] Fix complete!")
    print("\n[!] Next steps:")
    print("1. Restart TuoKit (Ctrl+C and run start_tuokit.bat again)")
    print("2. The model dropdown should now show your running Ollama models")
    print("3. All tools will use the selected model from the dropdown")

if __name__ == "__main__":
    main()
