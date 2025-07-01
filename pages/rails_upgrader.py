# pages/rails_upgrader.py
import streamlit as st
from utils import DatabaseManager, safe_ollama_generate

def generate_upgrade_path(from_ver, to_ver, project_details):
    """Generate Rails upgrade roadmap"""
    return safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Upgrade from Rails {from_ver} to {to_ver} | Project: {project_details}",
        system=(
            "Provide detailed roadmap:\n"
            "1. Breaking Changes: Key differences\n"
            "2. Gem Compatibility: Required updates\n"
            "3. Deprecation Guide: Changes needed\n"
            "4. Performance Considerations\n"
            "5. Recommended Tools: dual-boot, appraisal\n"
            "Include estimated effort level"
        )
    )['response']

def analyze_gemfile(gemfile_content):
    """Analyze Gemfile for upgrade compatibility"""
    return safe_ollama_generate(
        model="deepseek-r1:latest",
        prompt=f"Analyze Gemfile for Rails upgrade compatibility:\n{gemfile_content}",
        system="Identify gems that may cause issues during upgrade and suggest alternatives"
    )['response']

def generate_upgrade_script(from_ver, to_ver):
    """Generate upgrade automation script"""
    return safe_ollama_generate(
        model="deepseek-coder:latest",
        prompt=f"Generate Rails upgrade script from {from_ver} to {to_ver}",
        system="Create bash/rake script to automate common upgrade tasks"
    )['response']

def show():
    st.title("🆙 Rails Upgrade Advisor")
    st.caption("Plan and execute seamless Rails version upgrades")
    
    # Version selection
    col1, col2 = st.columns(2)
    with col1:
        from_ver = st.selectbox("Current Rails Version", 
                               ["5.0", "5.1", "5.2", "6.0", "6.1", "7.0", "7.1"],
                               index=3)  # Default to 6.0
    with col2:
        # Filter target versions to be greater than current
        all_versions = ["5.0", "5.1", "5.2", "6.0", "6.1", "7.0", "7.1", "7.2"]
        available_targets = [v for v in all_versions if v > from_ver]
        to_ver = st.selectbox("Target Rails Version", available_targets)
    
    # Project details
    project_size = st.radio("Project Size", 
                           ["Small (<10k LOC)", "Medium (10-50k LOC)", "Large (>50k LOC)"])
    
    critical_gems = st.text_area("Critical Gems (one per line)", 
                                "devise\nsidekiq\npg\nredis\nrspec-rails",
                                height=100)
    
    # Optional Gemfile analysis
    analyze_gemfile_opt = st.checkbox("Analyze Gemfile")
    if analyze_gemfile_opt:
        gemfile_content = st.text_area("Paste Gemfile Content", 
                                      height=200,
                                      placeholder="source 'https://rubygems.org'\ngem 'rails', '~> 6.0'")
    
    # Database and deployment info
    with st.sidebar:
        st.subheader("Project Configuration")
        database = st.selectbox("Database", ["PostgreSQL", "MySQL", "SQLite"])
        deployment = st.selectbox("Deployment", ["Heroku", "AWS", "Docker", "Traditional"])
        api_only = st.toggle("API-only Application", False)
        uses_webpacker = st.toggle("Uses Webpacker/Shakapacker", True)
        has_engines = st.toggle("Has Rails Engines", False)
        
        st.subheader("Testing Framework")
        test_framework = st.radio("Test Suite", ["RSpec", "Minitest", "Both"])
        ci_platform = st.selectbox("CI Platform", ["GitHub Actions", "CircleCI", "Jenkins", "GitLab CI"])
    
    if st.button("Generate Upgrade Plan", type="primary"):
        with st.spinner("Analyzing upgrade path..."):
            project_details = {
                "size": project_size,
                "gems": critical_gems.split('\n'),
                "database": database,
                "deployment": deployment,
                "api_only": api_only,
                "test_framework": test_framework
            }
            
            plan = generate_upgrade_path(from_ver, to_ver, project_details)
            
            # Display results in tabs
            tab1, tab2, tab3, tab4, tab5 = st.tabs(["Upgrade Plan", "Gem Analysis", "Scripts", "Timeline", "Resources"])
            
            with tab1:
                st.subheader(f"Rails {from_ver} → {to_ver} Upgrade Plan")
                st.markdown(plan)
                
                # Download plan
                st.download_button("Download Upgrade Plan", plan, f"rails_{from_ver}_to_{to_ver}_upgrade.md")
                
            with tab2:
                st.subheader("Gem Compatibility Analysis")
                if analyze_gemfile_opt and gemfile_content:
                    gem_analysis = analyze_gemfile(gemfile_content)
                    st.markdown(gem_analysis)
                else:
                    # General gem compatibility info
                    gem_compat = safe_ollama_generate(
                        model="deepseek-r1:latest",
                        prompt=f"Common gem compatibility issues upgrading Rails {from_ver} to {to_ver}",
                        system="List gems that commonly cause issues and their solutions"
                    )['response']
                    st.markdown(gem_compat)
                
            with tab3:
                st.subheader("Automation Scripts")
                
                # Upgrade script
                script = generate_upgrade_script(from_ver, to_ver)
                st.code(script, language="bash")
                st.download_button("Download Script", script, "upgrade_rails.sh")
                
                # Dual boot setup
                st.subheader("Dual Boot Configuration")
                dual_boot = """# Gemfile
eval_gemfile "Gemfile.common"

if ENV['RAILS_VERSION'] == 'next'
  gem 'rails', '~> """ + to_ver + """'
else
  gem 'rails', '~> """ + from_ver + """'
end

# Test with: RAILS_VERSION=next bundle install"""
                st.code(dual_boot, language="ruby")
                
            with tab4:
                st.subheader("Upgrade Timeline")
                
                # Estimate effort
                effort_multiplier = {"Small (<10k LOC)": 1, "Medium (10-50k LOC)": 2, "Large (>50k LOC)": 3}[project_size]
                version_gap = float(to_ver) - float(from_ver)
                base_days = int(version_gap * 10 * effort_multiplier)
                
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("Estimated Duration", f"{base_days}-{base_days*2} days")
                with col2:
                    st.metric("Risk Level", "Medium" if version_gap < 1.0 else "High")
                with col3:
                    st.metric("Recommended Team Size", f"{1 + int(effort_multiplier/2)} developers")
                
                # Upgrade phases
                st.subheader("Recommended Phases")
                phases = safe_ollama_generate(
                    model="deepseek-r1:latest",
                    prompt=f"Break down Rails {from_ver} to {to_ver} upgrade into phases",
                    system="Create 4-6 phases with clear milestones"
                )['response']
                st.markdown(phases)
                
            with tab5:
                st.subheader("Upgrade Resources")
                
                # Version-specific changes
                with st.expander(f"Rails {to_ver} Key Changes", expanded=True):
                    changes = safe_ollama_generate(
                        model="deepseek-r1:latest",
                        prompt=f"List key changes and new features in Rails {to_ver}",
                        system="Focus on breaking changes and new capabilities"
                    )['response']
                    st.markdown(changes)
                
                # Upgrade tools
                with st.expander("🧰 Essential Upgrade Tools", expanded=True):
                    st.markdown(f"""
                    **Pre-upgrade Checklist:**
                    ```bash
                    # Update to latest patch version first
                    bundle update rails --patch
                    
                    # Run upgrade check
                    bundle exec rails app:update
                    
                    # Check for deprecations
                    grep -r "DEPRECATION WARNING" log/
                    ```
                    
                    **Useful Gems:**
                    - `rails_upgrade_checklist` - Interactive upgrade guide
                    - `next_rails` - Dual boot for gradual migration
                    - `strong_migrations` - Catch unsafe migrations
                    - `bundle-audit` - Security vulnerability check
                    
                    **Testing Strategy:**
                    1. Set up dual CI builds
                    2. Fix deprecation warnings
                    3. Update gems incrementally
                    4. Run full test suite frequently
                    """)
                
                # External resources
                st.subheader("📚 Official Resources")
                col1, col2 = st.columns(2)
                with col1:
                    st.link_button("Rails Upgrade Guide", 
                                 f"https://guides.rubyonrails.org/upgrading_ruby_on_rails.html")
                    st.link_button("Rails {to_ver} Release Notes", 
                                 f"https://guides.rubyonrails.org/{to_ver.replace('.', '_')}_release_notes.html")
                with col2:
                    st.link_button("RailsDiff", 
                                 f"https://railsdiff.org/{from_ver}/{to_ver}")
                    st.link_button("GoRails Upgrade Guides", 
                                 "https://gorails.com/episodes/tagged/Upgrades")
            
            # Common pitfalls
            with st.expander("⚠️ Common Upgrade Pitfalls"):
                st.markdown("""
                **Avoid These Mistakes:**
                1. ❌ Upgrading multiple major versions at once
                2. ❌ Not reading deprecation warnings
                3. ❌ Skipping test coverage improvements
                4. ❌ Ignoring gem compatibility
                5. ❌ Not using version control branches
                
                **Best Practices:**
                1. ✅ Upgrade one minor version at a time
                2. ✅ Fix all deprecation warnings first
                3. ✅ Increase test coverage before starting
                4. ✅ Use dual boot for gradual migration
                5. ✅ Document all changes made
                """)
            
            # Save to knowledge base
            if st.button("📝 Save Upgrade Plan"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="rails_upgrade",
                        model="deepseek-r1:latest",
                        prompt=f"{from_ver}→{to_ver} | {project_details}",
                        response=plan,
                        metadata={
                            "tags": ["rails", "upgrade"],
                            "from_version": from_ver,
                            "to_version": to_ver,
                            "project_size": project_size
                        }
                    )
                    if query_id:
                        st.success("Plan saved to knowledge library!")
                else:
                    st.error("Could not connect to database")

if __name__ == "__main__":
    show()
