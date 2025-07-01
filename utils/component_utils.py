# utils/component_utils.py
"""
ViewComponent generation and management utilities
"""
import re
from typing import Dict, List, Optional

class ComponentBuilder:
    """ViewComponent builder utilities"""
    
    @staticmethod
    def generate_component_structure(name: str, features: List[str]) -> Dict[str, str]:
        """Generate component file structure"""
        component_name = f"{name.title().replace(' ', '')}Component"
        
        structure = {
            "ruby_class": f"""class {component_name} < ViewComponent::Base
  {'renders_one :header' if 'Slots' in features else ''}
  {'renders_one :footer' if 'Slots' in features else ''}
  {'renders_many :items' if 'Slots' in features else ''}
  
  def initialize(
    {'variant: :primary,' if 'Variants' in features else ''}
    {'loading: false,' if 'Loading States' in features else ''}
    **options
  )
    {'@variant = variant' if 'Variants' in features else ''}
    {'@loading = loading' if 'Loading States' in features else ''}
    @options = options
  end
  
  private
  
  {'def variant_classes
    variants[@variant]
  end
  
  def variants
    {
      primary: "btn-primary",
      secondary: "btn-secondary",
      danger: "btn-danger"
    }
  end' if 'Variants' in features else ''}
end""",
            
            "template": f"""<div class="{component_name.lower()} {{{{ component_classes }}}}" {{{{ stimulus_attributes }}}}>
  {'{{% if header %}}
  <div class="component-header">
    {{{{ header }}}}
  </div>
  {{% end %}}' if 'Slots' in features else ''}
  
  <div class="component-body">
    {{{{ content }}}}
  </div>
  
  {'{{% if footer %}}
  <div class="component-footer">
    {{{{ footer }}}}
  </div>
  {{% end %}}' if 'Slots' in features else ''}
</div>""",
            
            "stimulus": f"""import {{ Controller }} from "@hotwired/stimulus"

export default class extends Controller {{
  {'static targets = ["content", "loader"]' if 'Loading States' in features else ''}
  
  connect() {{
    console.log("{component_name} connected")
  }}
  
  {'async load() {
    this.showLoader()
    try {
      const response = await fetch(this.data.get("url"))
      const html = await response.text()
      this.contentTarget.innerHTML = html
    } finally {
      this.hideLoader()
    }
  }
  
  showLoader() {
    this.loaderTarget.classList.remove("hidden")
  }
  
  hideLoader() {
    this.loaderTarget.classList.add("hidden")
  }' if 'Loading States' in features else ''}
}}""",
            
            "preview": f"""class {component_name}Preview < ViewComponent::Preview
  # Default preview
  def default
    render {component_name}.new
  end
  
  {'# With slots
  def with_slots
    render ' + component_name + '.new do |component|
      component.header { "Header Content" }
      component.footer { "Footer Content" }
      "Main Content"
    end
  end' if 'Slots' in features else ''}
  
  {'# Variants
  def variants
    render ' + component_name + '.new(variant: :primary)
  end' if 'Variants' in features else ''}
  
  {'# Loading state
  def loading
    render ' + component_name + '.new(loading: true)
  end' if 'Loading States' in features else ''}
end"""
        }
        
        return structure
    
    @staticmethod
    def generate_tests(component_name: str, features: List[str]) -> str:
        """Generate RSpec tests for component"""
        return f"""require "rails_helper"

RSpec.describe {component_name}, type: :component do
  subject(:component) {{ described_class.new(**options) }}
  let(:options) {{ {{}} }}
  
  it "renders successfully" do
    expect {{ render_inline(component) }}.not_to raise_error
  end
  
  it "has correct CSS classes" do
    render_inline(component)
    expect(page).to have_css(".{component_name.lower()}")
  end
  
  {'describe "variants" do
    %i[primary secondary danger].each do |variant|
      context "when variant is #{variant}" do
        let(:options) { { variant: variant } }
        
        it "applies variant classes" do
          render_inline(component)
          expect(page).to have_css(".btn-#{variant}")
        end
      end
    end
  end' if 'Variants' in features else ''}
  
  {'describe "slots" do
    it "renders header slot" do
      render_inline(component) do |c|
        c.header { "Test Header" }
      end
      
      expect(page).to have_content("Test Header")
      expect(page).to have_css(".component-header")
    end
  end' if 'Slots' in features else ''}
  
  {'describe "accessibility" do
    it "has proper ARIA attributes" do
      render_inline(component)
      expect(page).to have_css("[role]")
    end
    
    it "has proper heading hierarchy" do
      render_inline(component)
      # Add specific heading tests
    end
  end' if 'I18n Support' in features else ''}
end"""
    
    @staticmethod
    def accessibility_checklist(component_type: str) -> List[str]:
        """Get accessibility checklist for component type"""
        base_checklist = [
            "Semantic HTML elements",
            "ARIA labels where needed",
            "Keyboard navigation support",
            "Focus indicators",
            "Color contrast (4.5:1 minimum)"
        ]
        
        component_specific = {
            "form": [
                "Label associations",
                "Error announcements",
                "Required field indicators",
                "Fieldset/legend for groups"
            ],
            "navigation": [
                "Landmark roles",
                "Current page indication",
                "Skip links",
                "Consistent ordering"
            ],
            "modal": [
                "Focus trap",
                "Escape key handling",
                "Background interaction disabled",
                "Announcement on open"
            ],
            "button": [
                "Disabled state handling",
                "Loading state announcement",
                "Button vs link semantics"
            ]
        }
        
        base_checklist.extend(component_specific.get(component_type, []))
        return base_checklist
    
    @staticmethod
    def stimulus_patterns() -> Dict[str, str]:
        """Common Stimulus patterns for components"""
        return {
            "debounce": """debounce(func, wait) {
    let timeout
    return function executedFunction(...args) {
      const later = () => {
        clearTimeout(timeout)
        func(...args)
      }
      clearTimeout(timeout)
      timeout = setTimeout(later, wait)
    }
  }""",
            
            "fetch_with_loading": """async fetchData() {
    this.element.classList.add('loading')
    try {
      const response = await fetch(this.data.get('url'))
      const data = await response.json()
      this.updateContent(data)
    } catch (error) {
      this.showError(error.message)
    } finally {
      this.element.classList.remove('loading')
    }
  }""",
            
            "auto_submit": """connect() {
    this.element.addEventListener('change', this.submit.bind(this))
  }
  
  submit() {
    this.element.closest('form').requestSubmit()
  }""",
            
            "infinite_scroll": """initialize() {
    this.page = 1
    this.loading = false
  }
  
  connect() {
    this.observer = new IntersectionObserver(
      entries => this.handleIntersection(entries),
      { threshold: 0.1 }
    )
    this.observer.observe(this.data.get('target'))
  }"""
        }


class ComponentPatterns:
    """Common ViewComponent patterns and best practices"""
    
    @staticmethod
    def slot_patterns() -> Dict[str, str]:
        """Common slot patterns"""
        return {
            "conditional_slots": """renders_one :header, lambda { |classes: nil|
    content_tag :div, class: ["header", classes] do
      yield
    end
  }""",
            
            "polymorphic_slots": """renders_many :items, types: {
    text: TextItemComponent,
    image: ImageItemComponent,
    video: VideoItemComponent
  }""",
            
            "slot_predicates": """def header?
    header.present?
  end
  
  def show_header?
    header? && !collapsed?
  end"""
        }
    
    @staticmethod
    def variant_patterns() -> Dict[str, str]:
        """Common variant patterns"""
        return {
            "size_variants": """{
    small: "text-sm px-2 py-1",
    medium: "text-base px-4 py-2",
    large: "text-lg px-6 py-3"
  }""",
            
            "state_variants": """{
    default: "bg-white border-gray-300",
    hover: "hover:bg-gray-50",
    active: "bg-gray-100",
    disabled: "opacity-50 cursor-not-allowed"
  }""",
            
            "theme_variants": """{
    light: "bg-white text-gray-900",
    dark: "bg-gray-900 text-white",
    auto: "bg-white text-gray-900 dark:bg-gray-900 dark:text-white"
  }"""
        }
