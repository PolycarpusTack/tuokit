"""
SmallTalk & Rails Tools Integration Example
Demonstrates how the tools work together for a complete workflow
"""

import streamlit as st

def show_integration_example():
    st.title("🔗 SmallTalk & Rails Integration Workflow")
    st.markdown("""
    This example demonstrates a typical workflow using TuoKit's SmallTalk and Rails tools together.
    
    ## Scenario: Porting a SmallTalk Application to Rails
    
    ### Step 1: Understanding SmallTalk Code
    
    First, use the **SmallTalk Explainer** to understand your legacy code:
    
    ```smalltalk
    "SmallTalk Order Management System"
    Object subclass: #Order
        instanceVariableNames: 'orderNumber items customer totalAmount'
        classVariableNames: ''
        poolDictionaries: ''
        
    Order >> calculateTotal
        "Calculate total amount for the order"
        totalAmount := items inject: 0 into: [:sum :item | 
            sum + (item price * item quantity)]
    ```
    
    The explainer will help you understand:
    - Object structure and inheritance
    - Message passing patterns
    - Collection operations
    
    ### Step 2: Convert Core Logic to Ruby
    
    Use the **SmallTalk ↔ Ruby Converter** to translate business logic:
    
    **SmallTalk Input:**
    ```smalltalk
    items inject: 0 into: [:sum :item | 
        sum + (item price * item quantity)]
    ```
    
    **Ruby Output:**
    ```ruby
    items.reduce(0) do |sum, item|
      sum + (item.price * item.quantity)
    end
    ```
    
    ### Step 3: Generate Rails Scaffold
    
    Use the **Rails Scaffold Generator** with the understood structure:
    
    **Input:** `Order order_number:string customer:references total_amount:decimal status:string`
    
    **Generated Files:**
    - `app/models/order.rb` - Model with validations
    - `app/controllers/orders_controller.rb` - RESTful controller
    - `app/views/orders/` - All necessary views
    - `db/migrate/xxx_create_orders.rb` - Database migration
    
    ### Step 4: Debug Integration Issues
    
    When errors occur during porting, use the **Rails Debugger**:
    
    **Common Porting Error:**
    ```
    NoMethodError: undefined method `inject' for #<ActiveRecord::Relation>
    ```
    
    **Debugger Solution:**
    - Explains ActiveRecord vs SmallTalk collections
    - Suggests using `.to_a.inject` or Rails-native `.sum`
    - Provides idiomatic Rails alternatives
    
    ### Step 5: Build Helper Methods
    
    Use the **SmallTalk Snippet Finder** to find patterns for:
    - Collection operations
    - Date/Time handling
    - File I/O operations
    
    Then convert them to Ruby helpers for your Rails app.
    
    ## Complete Workflow Benefits
    
    1. **Understanding**: Explainer helps grasp SmallTalk concepts
    2. **Translation**: Converter maintains business logic integrity
    3. **Structure**: Scaffold generator creates Rails foundation
    4. **Problem Solving**: Debugger handles integration issues
    5. **Pattern Library**: Snippet finder provides reusable solutions
    
    ## Example Integration Code
    
    Here's how a complete SmallTalk class might look after Rails conversion:
    
    ```ruby
    # app/models/order.rb
    class Order < ApplicationRecord
      belongs_to :customer
      has_many :order_items, dependent: :destroy
      
      # Converted from SmallTalk calculateTotal method
      def calculate_total
        self.total_amount = order_items.sum { |item| item.price * item.quantity }
      end
      
      # Converted from SmallTalk validation
      validates :order_number, presence: true, uniqueness: true
      
      # Rails callback replacing SmallTalk initialization
      before_save :calculate_total
    end
    ```
    
    ## Best Practices for SmallTalk → Rails Migration
    
    1. **Preserve Business Logic**: Focus on functionality, not syntax
    2. **Use Rails Conventions**: Leverage ActiveRecord patterns
    3. **Test Thoroughly**: Create specs for converted methods
    4. **Document Paradigm Shifts**: Note where Rails differs from SmallTalk
    5. **Incremental Migration**: Port one module at a time
    
    ## Tool Synergy
    
    The tools work together to provide:
    - **Analysis** → **Conversion** → **Implementation** → **Debugging** → **Optimization**
    
    Each tool feeds into the next, creating a complete development pipeline for SmallTalk to Rails migration projects.
    """)
    
    # Interactive demo section
    st.divider()
    st.subheader("🎯 Try It Yourself")
    
    workflow_step = st.selectbox(
        "Select a workflow step to explore:",
        [
            "1. Analyze SmallTalk Code",
            "2. Convert to Ruby",
            "3. Generate Rails Scaffold",
            "4. Debug Issues",
            "5. Find Snippets"
        ]
    )
    
    if "1." in workflow_step:
        if st.button("Go to SmallTalk Explainer"):
            st.switch_page("pages/smalltalk_explainer.py")
    elif "2." in workflow_step:
        if st.button("Go to Code Converter"):
            st.switch_page("pages/smalltalk_ruby_converter.py")
    elif "3." in workflow_step:
        if st.button("Go to Rails Scaffold Generator"):
            st.switch_page("pages/rails_scaffold.py")
    elif "4." in workflow_step:
        if st.button("Go to Rails Debugger"):
            st.switch_page("pages/rails_debugger.py")
    elif "5." in workflow_step:
        if st.button("Go to Snippet Finder"):
            st.switch_page("pages/smalltalk_snippets.py")

if __name__ == "__main__":
    show_integration_example()
