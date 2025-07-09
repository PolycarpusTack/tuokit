"""
RSpec Test Generator for TuoKit
Generates comprehensive RSpec tests for Ruby/Rails code
"""

import streamlit as st
from utils.model_manager import ModelManager

# Page configuration
st.set_page_config(
    page_title="Rspec Generator - TuoKit",
    page_icon="🚀",
    layout="wide"
)

# Initialize session state
from utils.ollama import OllamaToolBase
from utils.database import DatabaseManager
import re

class RSpecTestGenerator(OllamaToolBase):
    """RSpec test generation tool"""
    
    def __init__(self):
        super().__init__(
            tool_name="rspec_generator",
            default_model=ModelManager.get_default_model()
        )
        
        self.test_types = {
            "Model": {
                "description": "Unit tests for ActiveRecord models",
                "focus": "validations, associations, scopes, methods"
            },
            "Controller": {
                "description": "Controller specs (legacy style)",
                "focus": "actions, params, responses, filters"
            },
            "Request": {
                "description": "Integration tests for API endpoints",
                "focus": "HTTP requests, responses, side effects"
            },
            "System": {
                "description": "End-to-end browser tests",
                "focus": "user interactions, JavaScript, full stack"
            },
            "Helper": {
                "description": "Tests for view helpers",
                "focus": "helper methods, formatting, view logic"
            },
            "Service": {
                "description": "Tests for service objects",
                "focus": "business logic, external APIs, complex operations"
            },
            "Job": {
                "description": "Tests for background jobs",
                "focus": "job execution, arguments, side effects"
            },
            "Mailer": {
                "description": "Tests for ActionMailer",
                "focus": "email content, recipients, attachments"
            }
        }
    
    def generate_specs(self, code: str, spec_type: str,
                      use_factories: bool = True, use_shoulda: bool = True,
                      coverage_level: str = "comprehensive") -> dict:
        """Generate RSpec tests for Ruby/Rails code"""
        
        test_info = self.test_types.get(spec_type, {})
        
        enhancements = []
        if use_factories:
            enhancements.append("Use FactoryBot for test data")
        if use_shoulda and spec_type == "Model":
            enhancements.append("Use Shoulda Matchers for validations/associations")
        
        prompt = f"""Generate {coverage_level} RSpec {spec_type} tests for this Ruby/Rails code:

```ruby
{code}
```

Test Type: {spec_type}
Focus Areas: {test_info.get('focus', 'all aspects')}
{chr(10).join(enhancements) if enhancements else ''}

Create comprehensive tests covering:
1. Happy path scenarios - normal expected behavior
2. Edge cases - boundary conditions, empty inputs
3. Error conditions - invalid inputs, exceptions
4. Security considerations - authorization, injection
5. Performance considerations - N+1 queries, caching

Use RSpec best practices:
- Descriptive test names
- Proper test structure (describe/context/it)
- DRY with shared examples
- Appropriate use of let/let!/before blocks
- Clear expectations with proper matchers

Include comments explaining complex test scenarios."""
        
        result = self.generate_with_logging(
            prompt=prompt,
            temperature=0.1,
            system="You are an RSpec testing expert. Generate thorough, well-structured tests."
        )
        
        return {
            "specs": result["response"],
            "error": result["error"]
        }
    
    def extract_class_name(self, code: str) -> str:
        """Extract class or module name from Ruby code"""
        match = re.search(r'class\s+(\w+)', code)
        if match:
            return match.group(1)
        
        match = re.search(r'module\s+(\w+)', code)
        if match:
            return match.group(1)
        
        match = re.search(r'def\s+(\w+)', code)
        if match:
            return match.group(1).capitalize()
        
        return "Subject"
    
    def estimate_coverage(self, code: str, specs: str) -> dict:
        """Estimate test coverage based on code and specs"""
        # Simple heuristic - in production this would be more sophisticated
        code_methods = len(re.findall(r'def\s+\w+', code))
        spec_tests = len(re.findall(r'it\s+[\'"]', specs))
        
        if code_methods > 0:
            coverage = min(100, (spec_tests / code_methods) * 100)
        else:
            coverage = 100 if spec_tests > 0 else 0
        
        return {
            "methods": code_methods,
            "tests": spec_tests,
            "coverage": round(coverage, 1)
        }

def render_sidebar_config(generator):
    """Render sidebar configuration options"""
    with st.sidebar:
        st.subheader("⚙️ Test Configuration")
        
        # Test helpers
        use_factories = st.toggle(
            "Use FactoryBot",
            value=True,
            help="Generate factories for test data"
        )
        
        use_shoulda = st.toggle(
            "Use Shoulda Matchers",
            value=True,
            help="Use shoulda-matchers for concise tests"
        )
        
        use_faker = st.toggle(
            "Use Faker",
            value=True,
            help="Generate realistic test data"
        )
        
        st.divider()
        
        # Coverage level
        coverage_level = st.select_slider(
            "Coverage Level",
            options=["basic", "standard", "comprehensive", "exhaustive"],
            value="comprehensive",
            help="How thorough should the tests be"
        )
        
        st.divider()
        
        # Test patterns
        st.subheader("📋 Test Patterns")
        
        include_shared = st.checkbox(
            "Include Shared Examples",
            value=True,
            help="DRY tests with shared behaviors"
        )
        
        include_contexts = st.checkbox(
            "Use Contexts",
            value=True,
            help="Group related tests"
        )
        
        st.divider()
        st.caption("💡 **Tip**: Good tests are documentation")
    
    return {
        "use_factories": use_factories,
        "use_shoulda": use_shoulda,
        "use_faker": use_faker,
        "coverage_level": coverage_level,
        "include_shared": include_shared,
        "include_contexts": include_contexts
    }

def render_test_results(generator, result, spec_type, code, config):
    """Render test generation results"""
    st.success("✅ Tests generated successfully!")
    
    # Extract class name and estimate coverage
    class_name = generator.extract_class_name(code)
    coverage_info = generator.estimate_coverage(code, result["specs"])
    
    # Display metrics
    col1, col2, col3, col4 = st.columns(4)
    with col1:
        st.metric("Test Type", spec_type)
    with col2:
        st.metric("Test Cases", coverage_info["tests"])
    with col3:
        st.metric("Methods Found", coverage_info["methods"])
    with col4:
        st.metric("Est. Coverage", f"{coverage_info['coverage']}%")
    
    # Display in tabs
    tabs = st.tabs([
        "🧪 Test Code",
        "📊 Coverage Report",
        "📚 Testing Guide",
        "🛠️ Test Helpers",
        "💾 Save"
    ])
    
    with tabs[0]:
        render_test_code_tab(result, class_name, spec_type)
    
    with tabs[1]:
        render_coverage_report_tab(coverage_info)
    
    with tabs[2]:
        render_testing_guide_tab()
    
    with tabs[3]:
        render_test_helpers_tab()
    
    with tabs[4]:
        render_save_tab(generator, result, class_name, spec_type, code, coverage_info, config)
    
    return class_name, coverage_info

def render_test_code_tab(result, class_name, spec_type):
    """Render the test code tab"""
    st.code(result["specs"], language="ruby")
    
    # Download button
    spec_filename = f"{class_name.lower()}_spec.rb"
    st.download_button(
        "📥 Download Spec File",
        data=result["specs"],
        file_name=spec_filename,
        mime="text/plain"
    )
    
    # File location info
    spec_path = {
        "Model": f"spec/models/{spec_filename}",
        "Controller": f"spec/controllers/{spec_filename}",
        "Request": f"spec/requests/{spec_filename}",
        "System": f"spec/system/{spec_filename}",
        "Helper": f"spec/helpers/{spec_filename}",
        "Service": f"spec/services/{spec_filename}",
        "Job": f"spec/jobs/{spec_filename}",
        "Mailer": f"spec/mailers/{spec_filename}"
    }
    
    st.info(f"""
    **Save this file to:**
    ```
    {spec_path.get(spec_type, f'spec/{spec_filename}')}
    ```
    """)

def render_coverage_report_tab(coverage_info):
    """Render the coverage report tab"""
    st.subheader("📊 Test Coverage Analysis")
    
    # Coverage visualization
    coverage_color = "green" if coverage_info["coverage"] >= 80 else "orange" if coverage_info["coverage"] >= 60 else "red"
    
    st.markdown(f"""
    ### Estimated Coverage: <span style='color: {coverage_color}'>{coverage_info['coverage']}%</span>
    
    - **Methods in code:** {coverage_info['methods']}
    - **Test cases generated:** {coverage_info['tests']}
    - **Average tests per method:** {coverage_info['tests'] / max(coverage_info['methods'], 1):.1f}
    """, unsafe_allow_html=True)
    
    # Coverage recommendations
    if coverage_info["coverage"] < 80:
        st.warning("""
        **Recommendations to improve coverage:**
        - Add edge case tests
        - Test error conditions
        - Add integration tests
        - Test all conditional branches
        """)
    else:
        st.success("Good coverage! Consider adding performance tests.")
    
    # Running coverage
    st.divider()
    st.subheader("🏃 Running Coverage Reports")
    
    st.code("""# Add to Gemfile
group :test do
  gem 'simplecov', require: false
end

# Add to spec_helper.rb
require 'simplecov'
SimpleCov.start 'rails' do
  add_filter '/spec/'
  add_filter '/config/'
  coverage_dir 'coverage'
end

# Run with coverage
COVERAGE=true bundle exec rspec

# View report
open coverage/index.html""", language="ruby")

def render_testing_guide_tab():
    """Render the testing guide tab"""
    st.subheader("📚 RSpec Testing Guide")
    
    guide_tabs = st.tabs(["Structure", "Matchers", "Best Practices", "Anti-patterns"])
    
    with guide_tabs[0]:
        st.markdown("""
        ### Test Structure
        
        ```ruby
        RSpec.describe User, type: :model do
          # Test setup
          let(:user) { build(:user) }
          
          # Group related tests
          describe '#full_name' do
            context 'when both names present' do
              it 'returns full name' do
                expect(user.full_name).to eq('John Doe')
              end
            end
            
            context 'when last name missing' do
              it 'returns first name only' do
                user.last_name = nil
                expect(user.full_name).to eq('John')
              end
            end
          end
        end
        ```
        
        **Key Elements:**
        - `describe` - Group tests by class/method
        - `context` - Group by condition
        - `it` - Individual test case
        - `let` - Lazy-loaded test data
        """)
    
    with guide_tabs[1]:
        st.markdown("""
        ### Common Matchers
        
        **Equality**
        ```ruby
        expect(actual).to eq(expected)       # ==
        expect(actual).to be(expected)       # equal?
        expect(actual).to match(/pattern/)   # =~
        ```
        
        **Truthiness**
        ```ruby
        expect(actual).to be_truthy
        expect(actual).to be_falsey
        expect(actual).to be_nil
        ```
        
        **Collections**
        ```ruby
        expect(array).to include(item)
        expect(array).to match_array([1, 2, 3])
        expect(hash).to have_key(:key)
        ```
        
        **Errors**
        ```ruby
        expect { code }.to raise_error(ErrorClass)
        expect { code }.to change { Model.count }.by(1)
        ```
        """)
    
    with guide_tabs[2]:
        st.markdown("""
        ### Best Practices
        
        **1. One Assertion Per Test**
        ```ruby
        # Good
        it 'creates a user' do
          expect(User.count).to eq(1)
        end
        
        it 'sets the email' do
          expect(user.email).to eq('test@example.com')
        end
        ```
        
        **2. Use Contexts**
        ```ruby
        context 'when user is admin' do
          let(:user) { create(:user, :admin) }
          # admin-specific tests
        end
        ```
        
        **3. DRY with Shared Examples**
        ```ruby
        shared_examples 'a timestamped model' do
          it { is_expected.to have_db_column(:created_at) }
          it { is_expected.to have_db_column(:updated_at) }
        end
        ```
        """)
    
    with guide_tabs[3]:
        st.markdown("""
        ### Testing Anti-patterns
        
        **❌ Avoid:**
        - Testing implementation details
        - Overmocking
        - Brittle tests tied to UI
        - Testing framework code
        - Not testing edge cases
        
        **✅ Instead:**
        - Test behavior, not implementation
        - Use real objects when possible
        - Test through public interfaces
        - Focus on your code
        - Cover happy path + edge cases
        """)
    
    # External resources
    st.divider()
    col1, col2, col3 = st.columns(3)
    with col1:
        st.link_button(
            "Better Specs",
            "https://www.betterspecs.org/",
            use_container_width=True
        )
    with col2:
        st.link_button(
            "RSpec Documentation",
            "https://rspec.info/",
            use_container_width=True
        )
    with col3:
        st.link_button(
            "Testing Rails",
            "https://guides.rubyonrails.org/testing.html",
            use_container_width=True
        )

def render_test_helpers_tab():
    """Render the test helpers tab"""
    st.subheader("🛠️ Test Helper Setup")
    
    helper_tabs = st.tabs(["FactoryBot", "Shoulda", "Database Cleaner", "Helpers"])
    
    with helper_tabs[0]:
        st.markdown("### FactoryBot Setup")
        st.code("""# spec/factories/users.rb
FactoryBot.define do
  factory :user do
    sequence(:email) { |n| "user#{n}@example.com" }
    first_name { Faker::Name.first_name }
    last_name { Faker::Name.last_name }
    age { rand(18..65) }
    
    trait :admin do
      admin { true }
    end
    
    trait :with_posts do
      transient do
        posts_count { 5 }
      end
      
      after(:create) do |user, evaluator|
        create_list(:post, evaluator.posts_count, user: user)
      end
    end
  end
end

# Usage in tests
let(:user) { create(:user) }
let(:admin) { create(:user, :admin) }
let(:author) { create(:user, :with_posts, posts_count: 10) }""", language="ruby")
    
    with helper_tabs[1]:
        st.markdown("### Shoulda Matchers Setup")
        st.code("""# spec/rails_helper.rb
Shoulda::Matchers.configure do |config|
  config.integrate do |with|
    with.test_framework :rspec
    with.library :rails
  end
end

# Usage in model specs
RSpec.describe User, type: :model do
  # Validations
  it { should validate_presence_of(:email) }
  it { should validate_uniqueness_of(:email) }
  
  # Associations
  it { should have_many(:posts) }
  it { should belong_to(:company).optional }
  
  # Database
  it { should have_db_column(:email).of_type(:string) }
  it { should have_db_index(:email).unique }
end""", language="ruby")
    
    with helper_tabs[2]:
        st.markdown("### Database Cleaner Setup")
        st.code("""# spec/rails_helper.rb
RSpec.configure do |config|
  config.before(:suite) do
    DatabaseCleaner.strategy = :transaction
    DatabaseCleaner.clean_with(:truncation)
  end

  config.around(:each) do |example|
    DatabaseCleaner.cleaning do
      example.run
    end
  end
  
  # For system specs
  config.before(:each, type: :system) do
    driven_by :selenium_chrome_headless
    DatabaseCleaner.strategy = :truncation
  end
end""", language="ruby")
    
    with helper_tabs[3]:
        st.markdown("### Custom Test Helpers")
        st.code("""# spec/support/request_helpers.rb
module RequestHelpers
  def json_response
    JSON.parse(response.body, symbolize_names: true)
  end
  
  def auth_headers(user)
    { 'Authorization' => "Bearer #{user.auth_token}" }
  end
end

RSpec.configure do |config|
  config.include RequestHelpers, type: :request
end

# spec/support/capybara_helpers.rb
module CapybaraHelpers
  def sign_in(user)
    visit login_path
    fill_in 'Email', with: user.email
    fill_in 'Password', with: 'password'
    click_button 'Sign In'
  end
end""", language="ruby")

def render_save_tab(generator, result, class_name, spec_type, code, coverage_info, config):
    """Render the save tab"""
    st.subheader("💾 Save Test Suite")
    
    title = st.text_input(
        "Title",
        value=f"RSpec Tests: {class_name} {spec_type}"
    )
    
    project = st.text_input(
        "Project Name",
        placeholder="MyRailsApp"
    )
    
    notes = st.text_area(
        "Testing Notes",
        placeholder="Add notes about these tests..."
    )
    
    tags = st.text_input(
        "Tags",
        value=f"rspec, testing, {spec_type.lower()}, {class_name.lower()}"
    )
    
    if st.button("💾 Save Tests", type="primary"):
        if generator.db.connected:
            # Compile content
            content = f"""## RSpec {spec_type} Tests: {class_name}

## Test Configuration
- Type: {spec_type}
- Coverage Level: {config['coverage_level']}
- FactoryBot: {'Yes' if config['use_factories'] else 'No'}
- Shoulda Matchers: {'Yes' if config['use_shoulda'] else 'No'}

## Original Code
```ruby
{code}
```

## Generated Tests
```ruby
{result['specs']}
```

## Coverage Analysis
- Methods: {coverage_info['methods']}
- Test Cases: {coverage_info['tests']}
- Estimated Coverage: {coverage_info['coverage']}%

## Project: {project}
## Notes: {notes}"""
            
            metadata = {
                "class_name": class_name,
                "spec_type": spec_type,
                "coverage_level": config['coverage_level'],
                "use_factories": config['use_factories'],
                "use_shoulda": config['use_shoulda'],
                "coverage_info": coverage_info
            }
            
            query_id = generator.db.log_query(
                tool="rspec_generator",
                model=generator.default_model,
                prompt=f"Generate {spec_type} tests for {class_name}",
                response=result["specs"],
                metadata=metadata
            )
            
            if query_id and title:
                success = generator.db.save_knowledge_unit(
                    query_id=query_id,
                    title=title,
                    content=content,
                    category="RSpec Tests",
                    tags=[tag.strip() for tag in tags.split(",")]
                )
                if success:
                    st.success("✅ Tests saved to library!")
                    st.balloons()
        else:
            st.warning("Database not connected")

def show():
    """Main page display function"""
    st.title("🧪 RSpec Test Generator")
    st.markdown("Generate comprehensive RSpec tests for your Ruby/Rails code")
    
    # Initialize generator
    generator = RSpecTestGenerator()
    
    # Render sidebar configuration
    config = render_sidebar_config(generator)
    
    # Main content
    code = st.text_area(
        "Paste Ruby/Rails Code to Test",
        height=300,
        placeholder="""Example:
class User < ApplicationRecord
  validates :email, presence: true, uniqueness: true
  validates :age, numericality: { greater_than_or_equal_to: 18 }
  
  has_many :posts
  
  def full_name
    "#{first_name} #{last_name}".strip
  end
  
  def adult?
    age >= 18
  end
end""",
        help="Paste the code you want to test"
    )
    
    # Test type selection
    col1, col2 = st.columns([3, 1])
    
    with col1:
        spec_type = st.selectbox(
            "Test Type",
            list(generator.test_types.keys()),
            help="Select the appropriate test type for your code"
        )
    
    with col2:
        # Show test type info
        if spec_type in generator.test_types:
            st.info(generator.test_types[spec_type]["description"])
    
    # Quick examples based on test type
    if spec_type == "Model":
        st.markdown("**Model Test Focus:** Validations, associations, scopes, instance methods, callbacks")
    elif spec_type == "Request":
        st.markdown("**Request Test Focus:** HTTP verbs, status codes, response bodies, headers, authentication")
    elif spec_type == "System":
        st.markdown("**System Test Focus:** User flows, JavaScript interactions, form submissions, navigation")
    
    # Generate button
    if st.button("🧪 Generate Tests", type="primary", disabled=not code.strip()):
        with st.spinner("Creating comprehensive test suite..."):
            result = generator.generate_specs(
                code,
                spec_type,
                use_factories=config["use_factories"],
                use_shoulda=config["use_shoulda"],
                coverage_level=config["coverage_level"]
            )
            
            if not result["error"]:
                render_test_results(generator, result, spec_type, code, config)
            else:
                st.error("Test generation failed. Please check your code and try again.")

# Entry point for Streamlit
if __name__ == "__main__":
    show()
