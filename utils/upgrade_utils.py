# utils/upgrade_utils.py
"""
Rails upgrade utilities and version management
"""
from typing import Dict, List, Tuple

class RailsUpgrader:
    """Rails upgrade analysis and planning utilities"""
    
    # Major version changes database
    VERSION_CHANGES = {
        ("5.0", "5.1"): {
            "breaking": ["form_with default to remote", "encrypted secrets"],
            "deprecated": ["redirect_to :back", "alias_method_chain"],
            "new_features": ["yarn support", "system tests", "encrypted secrets"]
        },
        ("5.1", "5.2"): {
            "breaking": ["bootsnap default", "credentials.yml.enc"],
            "deprecated": ["secrets.yml", "config.secret_token"],
            "new_features": ["Active Storage", "credentials", "CSP DSL"]
        },
        ("5.2", "6.0"): {
            "breaking": ["Webpacker default", "Zeitwerk autoloader"],
            "deprecated": ["update_attributes", "update_attributes!"],
            "new_features": ["Action Mailbox", "Action Text", "parallel testing"]
        },
        ("6.0", "6.1"): {
            "breaking": ["strict_loading", "legacy_connection_handling"],
            "deprecated": ["update_attributes", "where.not chaining"],
            "new_features": ["strict_loading", "delegated types", "destroy_async"]
        },
        ("6.1", "7.0"): {
            "breaking": ["Zeitwerk only", "Ruby 2.7+ required"],
            "deprecated": ["button_to GET", "legacy autoloader"],
            "new_features": ["encrypted attributes", "async queries", "load_async"]
        },
        ("7.0", "7.1"): {
            "breaking": ["Ruby 3.0+ required", "Trilogy adapter"],
            "deprecated": ["unsafe_raw_sql", "ActiveRecord.legacy_connection_handling"],
            "new_features": ["composite primary keys", "async queries", "normalizes"]
        }
    }
    
    @staticmethod
    def version_changes(from_ver: str, to_ver: str) -> Dict[str, List[str]]:
        """Get changes between Rails versions"""
        key = (from_ver, to_ver)
        
        # Direct upgrade path
        if key in RailsUpgrader.VERSION_CHANGES:
            return RailsUpgrader.VERSION_CHANGES[key]
        
        # Multi-version upgrade (aggregate changes)
        changes = {"breaking": [], "deprecated": [], "new_features": []}
        current = from_ver
        
        for (start, end), change_set in RailsUpgrader.VERSION_CHANGES.items():
            if start >= from_ver and end <= to_ver:
                for category in changes:
                    changes[category].extend(change_set.get(category, []))
        
        return changes
    
    @staticmethod
    def estimate_effort(from_ver: str, to_ver: str, project_size: str) -> Dict[str, any]:
        """Estimate upgrade effort in developer-days"""
        size_multiplier = {
            "Small (<10k LOC)": 1.0,
            "Medium (10-50k LOC)": 2.5,
            "Large (>50k LOC)": 5.0
        }
        
        base_effort = {
            "5.0": 5, "5.1": 5, "5.2": 5,
            "6.0": 10, "6.1": 8,
            "7.0": 15, "7.1": 10, "7.2": 8
        }
        
        # Calculate version gap
        from_major = float(from_ver)
        to_major = float(to_ver)
        version_gap = to_major - from_major
        
        # Base effort calculation
        effort_days = base_effort.get(to_ver, 10) * size_multiplier.get(project_size, 2.0)
        
        # Add complexity for larger jumps
        if version_gap > 1.0:
            effort_days *= (1 + version_gap * 0.3)
        
        return {
            "min_days": int(effort_days * 0.8),
            "max_days": int(effort_days * 1.5),
            "recommended_team": max(1, int(effort_days / 20)),
            "risk_level": "High" if version_gap > 1.0 else "Medium"
        }
    
    @staticmethod
    def gem_compatibility_check(gem_list: List[str], target_version: str) -> Dict[str, str]:
        """Check common gem compatibility issues"""
        compatibility_issues = {
            "7.0": {
                "paperclip": "Use Active Storage instead",
                "friendly_id": "Update to 5.4+",
                "devise": "Update to 4.8+",
                "rspec-rails": "Update to 5.0+",
                "factory_bot_rails": "Update to 6.2+"
            },
            "6.1": {
                "sprockets": "Update to 4.0+",
                "sass-rails": "Update to 6.0+",
                "webpacker": "Update to 5.0+",
                "bootsnap": "Update to 1.7+"
            }
        }
        
        issues = {}
        target_issues = compatibility_issues.get(target_version, {})
        
        for gem in gem_list:
            gem_name = gem.strip().lower()
            if gem_name in target_issues:
                issues[gem_name] = target_issues[gem_name]
        
        return issues
    
    @staticmethod
    def generate_upgrade_checklist(from_ver: str, to_ver: str) -> List[str]:
        """Generate upgrade checklist"""
        checklist = [
            "✅ Back up database and codebase",
            "✅ Update Ruby to minimum required version",
            "✅ Update bundler to latest version",
            f"✅ Create new branch: rails-upgrade-{to_ver}",
            "✅ Update Gemfile: gem 'rails', '~> " + to_ver + "'",
            "✅ Run: bundle update rails",
            "✅ Run: rails app:update",
            "✅ Review and merge configuration changes",
            "✅ Fix deprecation warnings in logs",
            "✅ Update gems with compatibility issues",
            "✅ Run full test suite",
            "✅ Update JavaScript dependencies if needed",
            "✅ Test in staging environment",
            "✅ Update deployment configurations",
            "✅ Plan database migrations if needed",
            "✅ Document all changes made"
        ]
        
        # Add version-specific items
        if to_ver >= "6.0":
            checklist.insert(5, "✅ Configure Zeitwerk autoloader")
        if to_ver >= "7.0":
            checklist.insert(6, "✅ Update to importmap or jsbundling")
        
        return checklist


class UpgradeAutomation:
    """Automation helpers for Rails upgrades"""
    
    @staticmethod
    def dual_boot_gemfile() -> str:
        """Generate dual boot Gemfile template"""
        return """# Gemfile
source 'https://rubygems.org'
git_source(:github) { |repo| "https://github.com/#{repo}.git" }

if ENV['RAILS_VERSION'] == 'next'
  gem 'rails', '~> 7.1.0'
else
  gem 'rails', '~> 7.0.0'
end

# Conditional gem versions
if ENV['RAILS_VERSION'] == 'next'
  gem 'devise', '~> 4.9'
else
  gem 'devise', '~> 4.8'
end

# Shared gems
eval_gemfile 'Gemfile.common'
"""
    
    @staticmethod
    def deprecation_tracking_initializer() -> str:
        """Generate deprecation tracking code"""
        return """# config/initializers/deprecation_tracking.rb
if Rails.env.development? || Rails.env.test?
  # Log deprecations to separate file
  deprecation_log = File.join(Rails.root, 'log', 'deprecations.log')
  Rails.application.configure do
    config.active_support.deprecation = [:log, :stderr]
  end
  
  # Track unique deprecations
  ActiveSupport::Deprecation.behavior = lambda do |message, callstack|
    File.open(deprecation_log, 'a') do |f|
      f.puts "=" * 80
      f.puts Time.current
      f.puts message
      f.puts callstack.first(5).join("\n")
    end
  end
end
"""
