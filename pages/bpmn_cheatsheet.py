import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation
import random

# Page configuration
st.set_page_config(
    page_title="BPMN 2.0 Training Path - TuoKit",
    page_icon="🔄",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="bpmn_cheatsheet")

# Define all functions first
def show_beginner_path():
    """Beginner learning path"""
    st.markdown("### 🌱 Beginner Path: BPMN Fundamentals")
    
    # Week 1
    with st.expander("📅 Week 1: Introduction to Business Process Modeling", expanded=True):
        st.markdown("""
        **Learning Objectives:**
        - Understand what business processes are
        - Learn why process modeling is important
        - Introduction to BPMN 2.0 standard
        
        **Topics:**
        1. **What is a Business Process?**
           - Definition and characteristics
           - Process vs. project vs. case
           - Value chains and process hierarchies
        
        2. **Why Model Business Processes?**
           - Documentation and standardization
           - Process improvement and optimization
           - Communication and training
           - Automation preparation
        
        3. **Introduction to BPMN 2.0**
           - History and evolution
           - BPMN vs. other notations (flowcharts, UML, etc.)
           - Key principles and goals
        
        **Practice:**
        - Identify 5 business processes in your organization
        - Describe one process in plain text (narrative form)
        - List stakeholders for that process
        """)
        
        if st.button("✅ Mark Week 1 Complete", key="week1"):
            st.session_state.bpmn_progress['basics'] = min(25, st.session_state.bpmn_progress['basics'] + 25)
            st.success("Week 1 completed! Great start!")
    
    # Week 2
    with st.expander("📅 Week 2: Basic BPMN Elements"):
        st.markdown("""
        **Learning Objectives:**
        - Master the core BPMN elements
        - Create your first simple process diagram
        - Understand sequence flows
        
        **Core Elements:**
        
        1. **Events** (Circles)
           - **Start Event** (thin circle): Where process begins
           - **End Event** (thick circle): Where process ends
           - **Intermediate Event** (double circle): Something that happens during the process
        
        2. **Activities** (Rounded Rectangles)
           - **Task**: Atomic unit of work
           - **Sub-Process**: Compound activity (can be expanded)
           - Task Types:
             - User Task: Human performs the task
             - Service Task: System/automated task
             - Manual Task: Non-IT human task
        
        3. **Gateways** (Diamonds)
           - **Exclusive (XOR)**: One path only
           - **Parallel (AND)**: All paths simultaneously
           - **Inclusive (OR)**: One or more paths
        
        4. **Sequence Flows** (Solid Arrows)
           - Connect elements
           - Show order of execution
           - Can have conditions
        
        **Practice Exercise:**
        Create a simple "Order Coffee" process:
        1. Start → Select Coffee Type → Pay → Prepare Coffee → Deliver Coffee → End
        2. Add an exclusive gateway for payment method (cash/card)
        3. Add error handling for payment failure
        """)
        
        if st.button("✅ Mark Week 2 Complete", key="week2"):
            st.session_state.bpmn_progress['basics'] = min(50, st.session_state.bpmn_progress['basics'] + 25)
            st.success("Week 2 completed! You know the basics now!")
    
    # Week 3
    with st.expander("📅 Week 3: Pools, Lanes, and Data"):
        st.markdown("""
        **Learning Objectives:**
        - Understand organizational modeling with pools and lanes
        - Learn about data objects and data flow
        - Model multi-participant processes
        
        **Topics:**
        
        1. **Pools and Lanes**
           - **Pool**: Represents a participant (organization, role, system)
           - **Lane**: Sub-partition within a pool (department, role, person)
           - Black box pools for external participants
        
        2. **Message Flows** (Dashed Arrows)
           - Communication between pools
           - Cannot cross lane boundaries
           - Always between different pools
        
        3. **Data Elements**
           - **Data Object**: Information required/produced
           - **Data Store**: Persistent data storage
           - **Data Input/Output**: Data requirements for activities
        
        4. **Artifacts**
           - **Text Annotation**: Comments and notes
           - **Group**: Visual grouping of elements
        
        **Practice Exercise:**
        Model an "Online Purchase" process with:
        - Customer Pool (Browse → Select → Order → Pay)
        - Online Shop Pool (lanes: Website, Payment System, Warehouse)
        - Show message flows between customer and shop
        - Include order data object
        """)
        
        if st.button("✅ Mark Week 3 Complete", key="week3"):
            st.session_state.bpmn_progress['basics'] = min(75, st.session_state.bpmn_progress['basics'] + 25)
            st.success("Week 3 completed! You understand collaboration!")
    
    # Week 4
    with st.expander("📅 Week 4: Your First Complete Process"):
        st.markdown("""
        **Learning Objectives:**
        - Combine all learned elements
        - Create a complete, realistic process model
        - Validate and improve your model
        
        **Checklist for Complete Process:**
        - [ ] Clear start and end events
        - [ ] All activities have clear names (verb + noun)
        - [ ] Gateways have clear conditions
        - [ ] Pools/lanes show clear responsibilities
        - [ ] Data flow is complete
        - [ ] Exception handling included
        - [ ] Model is readable and not cluttered
        
        **Final Project:**
        Model a "Employee Onboarding" process including:
        1. HR receives new hire information
        2. IT prepares equipment and accounts
        3. Manager prepares workspace
        4. Employee completes paperwork
        5. Training schedule created
        6. First day activities
        
        **Validation Questions:**
        - Can someone unfamiliar with the process understand it?
        - Are all possible paths covered?
        - Is the happy path clear?
        - Are exceptions handled?
        """)
        
        if st.button("✅ Mark Week 4 Complete", key="week4"):
            st.session_state.bpmn_progress['basics'] = 100
            st.success("🎉 Beginner path completed! Ready for intermediate level!")

def show_intermediate_path():
    """Intermediate learning path"""
    st.markdown("### 🌿 Intermediate Path: Advanced BPMN Concepts")
    
    with st.expander("📅 Week 5-6: Event Types and Patterns", expanded=True):
        st.markdown("""
        **Advanced Event Types:**
        
        1. **Timer Events** ⏰
           - Start Timer: Process starts at specific time/interval
           - Intermediate Timer: Wait for duration/until date
           - Boundary Timer: Timeout for activities
        
        2. **Message Events** ✉️
           - Start Message: Process triggered by message
           - Intermediate Message: Wait for/send message
           - Boundary Message: Interrupt activity on message
        
        3. **Error Events** ⚠️
           - End Error: Throw error and end
           - Boundary Error: Catch errors from activities
           - Error propagation in sub-processes
        
        4. **Signal Events** 📡
           - Broadcast to multiple processes
           - Start/Intermediate/End signal events
           - Signal vs. Message differences
        
        5. **Conditional Events** 🔄
           - Start when condition becomes true
           - Monitor business conditions
        
        6. **Escalation Events** 📈
           - Non-critical issues requiring attention
           - Can be interrupting or non-interrupting
        
        **Event Patterns:**
        - Event-based Gateway (wait for multiple events)
        - Boundary events (interrupting vs. non-interrupting)
        - Event sub-processes
        """)
    
    with st.expander("📅 Week 7-8: Sub-Processes and Transactions"):
        st.markdown("""
        **Sub-Process Types:**
        
        1. **Embedded Sub-Process**
           - Defined within parent process
           - Can be collapsed/expanded
           - Shares process context
        
        2. **Call Activity**
           - References external process
           - Reusable across models
           - Independent context
        
        3. **Event Sub-Process**
           - Triggered by events
           - Runs within process context
           - Can be interrupting or non-interrupting
        
        4. **Transaction Sub-Process**
           - All-or-nothing execution
           - Compensation handling
           - Cancel events
        
        5. **Ad-hoc Sub-Process**
           - Activities without sequence
           - Completion condition
           - User-determined order
        
        **Loop Types:**
        - Standard Loop (while condition)
        - Multi-Instance Sequential
        - Multi-Instance Parallel
        """)
    
    with st.expander("📅 Week 9-10: Compensation and Error Handling"):
        st.markdown("""
        **Advanced Error Handling:**
        
        1. **Compensation Activities**
           - Undo completed activities
           - Compensation handlers
           - Compensation throw/catch events
        
        2. **Transaction Patterns**
           - ACID properties in BPMN
           - Cancel events and handlers
           - Transaction boundaries
        
        3. **Exception Handling Strategies**
           - Retry patterns
           - Fallback paths
           - Escalation hierarchies
           - Dead letter queues
        
        4. **Best Practices:**
           - Always model the unhappy path
           - Use boundary events for timeouts
           - Implement compensation for critical activities
           - Clear error messages and logging
        """)

def show_advanced_path():
    """Advanced learning path"""
    st.markdown("### 🌳 Advanced Path: Enterprise BPMN")
    
    with st.expander("📅 Week 11-12: Process Orchestration", expanded=True):
        st.markdown("""
        **Orchestration Concepts:**
        
        1. **Process Choreography**
           - Multi-party collaboration
           - Choreography diagrams
           - Public vs. private processes
        
        2. **Process Correlation**
           - Instance identification
           - Correlation keys
           - Message correlation patterns
        
        3. **Long-Running Processes**
           - State persistence
           - Human task management
           - Process versioning
        
        4. **Process Monitoring**
           - KPIs and metrics
           - SLA management
           - Process mining preparation
        """)
    
    with st.expander("📅 Week 13-14: BPMN Execution"):
        st.markdown("""
        **Execution Semantics:**
        
        1. **Token Concept**
           - Token flow simulation
           - Token multiplication at gateways
           - Dead path elimination
        
        2. **Process Engine Concepts**
           - Process deployment
           - Instance creation
           - State management
           - History and audit trails
        
        3. **Integration Patterns**
           - Service task implementation
           - External task pattern
           - REST/SOAP integration
           - Message queuing
        
        4. **Human Task Management**
           - Task assignment
           - Delegation and escalation
           - Form integration
           - Worklist concepts
        """)

def show_expert_path():
    """Expert learning path"""
    st.markdown("### 🏆 Expert Path: BPMN Mastery")
    
    with st.expander("📅 Advanced Topics", expanded=True):
        st.markdown("""
        **Expert-Level Concepts:**
        
        1. **Process Optimization**
           - Simulation and analysis
           - Bottleneck identification
           - Resource optimization
           - Cost analysis
        
        2. **BPMN Extensions**
           - Custom attributes
           - Extension elements
           - Domain-specific extensions
        
        3. **Method and Style**
           - Hierarchical modeling
           - Process architecture
           - Naming conventions
           - Model layout best practices
        
        4. **Tool Expertise**
           - BPMN-XML format
           - Model interchange
           - Round-trip engineering
           - API automation
        """)

def show_learning_path():
    """Display progressive BPMN learning path"""
    
    st.markdown("## 🎓 BPMN 2.0 Progressive Learning Path")
    
    # Learning Path Selector
    level = st.radio(
        "Select Your Learning Level:",
        ["🌱 Beginner", "🌿 Intermediate", "🌳 Advanced", "🏆 Expert"],
        horizontal=True
    )
    
    if level == "🌱 Beginner":
        show_beginner_path()
    elif level == "🌿 Intermediate":
        show_intermediate_path()
    elif level == "🌳 Advanced":
        show_advanced_path()
    else:
        show_expert_path()

def show_quick_reference():
    """Display BPMN quick reference guide"""
    
    st.markdown("## 🎯 BPMN 2.0 Quick Reference")
    
    # Element Categories
    element_category = st.selectbox(
        "Select Element Category:",
        ["Flow Objects", "Connecting Objects", "Swimlanes", "Artifacts", "Event Types", "Gateway Types", "Activity Markers"]
    )
    
    if element_category == "Flow Objects":
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.markdown("""
            ### Events
            - **Start Event** ○
              - None Start
              - Message Start ✉
              - Timer Start ⏰
              - Conditional Start ⬟
              - Signal Start 📡
              - Multiple Start ⬡
            """)
        
        with col2:
            st.markdown("""
            ### Activities
            - **Task** ▢
              - User Task 👤
              - Service Task ⚙
              - Script Task 📄
              - Business Rule Task 📊
              - Manual Task ✋
              - Send Task ➤
              - Receive Task ◄
            """)
        
        with col3:
            st.markdown("""
            ### Gateways
            - **Exclusive** ◇ (XOR)
            - **Parallel** ⬩ (AND)
            - **Inclusive** ⬡ (OR)
            - **Event-Based** ⬢
            - **Complex** ✱
            """)
    
    elif element_category == "Event Types":
        # Comprehensive event type matrix
        st.markdown("""
        ### Event Type Matrix
        
        | Event Type | Start | Intermediate | End | Boundary |
        |------------|-------|--------------|-----|----------|
        | None | ○ | - | ● | - |
        | Message | ✉○ | ✉◎ | ✉● | ✉▣ |
        | Timer | ⏰○ | ⏰◎ | - | ⏰▣ |
        | Error | - | - | ⚠● | ⚠▣ |
        | Escalation | 🔺○ | 🔺◎ | 🔺● | 🔺▣ |
        | Cancel | - | - | ❌● | ❌▣ |
        | Compensation | - | ↩◎ | ↩● | ↩▣ |
        | Conditional | 📋○ | 📋◎ | - | 📋▣ |
        | Link | - | 🔗◎ | 🔗● | - |
        | Signal | 📡○ | 📡◎ | 📡● | 📡▣ |
        | Terminate | - | - | ⊗● | - |
        | Multiple | ⬡○ | ⬡◎ | ⬡● | ⬡▣ |
        
        **Legend:**
        - ○ = Start Event (thin circle)
        - ◎ = Intermediate Event (double circle)
        - ● = End Event (thick circle)
        - ▣ = Boundary Event (attached to activity)
        """)
    
    elif element_category == "Activity Markers":
        st.markdown("""
        ### Activity Markers and Decorators
        
        **Task Types:**
        - 👤 User Task (human involvement)
        - ⚙ Service Task (automated)
        - 📄 Script Task (script execution)
        - ✋ Manual Task (non-system)
        - 📊 Business Rule Task
        - ➤ Send Task
        - ◄ Receive Task
        
        **Loop Markers:**
        - ↻ Standard Loop
        - ||| Multi-Instance Parallel
        - = Multi-Instance Sequential
        
        **Other Markers:**
        - ⊕ Sub-Process (collapsed)
        - ⊞ Ad Hoc Sub-Process
        - ≈ Compensation Activity
        - ◈ Transaction Sub-Process
        """)
    
    # BPMN Patterns Section
    st.markdown("---")
    st.markdown("### 🎨 Common BPMN Patterns")
    
    pattern = st.selectbox(
        "Select Pattern:",
        ["Approval Pattern", "Retry Pattern", "Timeout Pattern", "Escalation Pattern", "Parallel Processing"]
    )
    
    if pattern == "Approval Pattern":
        st.code("""
        Start → Submit Request → XOR Gateway
                                    ├─[Approved]→ Process Request → End
                                    └─[Rejected]→ Notify Rejection → End
        """)
    
    elif pattern == "Retry Pattern":
        st.code("""
        Start → Try Operation → XOR Gateway
                     ↑              ├─[Success]→ End
                     └──[Retry]────┘[Failure]
                                    └─[Max Retries]→ Error End
        """)
    
    elif pattern == "Timeout Pattern":
        st.code("""
        Start → Activity with Timer Boundary Event
                    ├─[Normal Flow]→ Continue → End
                    └─[Timer Expires]→ Timeout Handling → End
        """)

def show_practical_examples():
    """Show practical BPMN examples"""
    
    st.markdown("## 🔧 Practical BPMN Examples")
    
    example_type = st.selectbox(
        "Choose Example Category:",
        ["Order Management", "HR Processes", "IT Service Management", "Financial Processes", "Manufacturing"]
    )
    
    if example_type == "Order Management":
        st.markdown("### 📦 Order-to-Cash Process")
        
        with st.expander("Complete Order Process"):
            st.markdown("""
            **Process Overview:**
            End-to-end order fulfillment with payment processing and delivery.
            
            **Main Flow:**
            1. **Customer Places Order** (Message Start Event)
               - Receive order details
               - Validate order information
            
            2. **Check Inventory** (Service Task)
               - Query inventory system
               - Reserve items
            
            3. **Inventory Decision** (Exclusive Gateway)
               - If available → Process Payment
               - If not available → Check with Supplier
            
            4. **Process Payment** (Sub-Process)
               - Validate payment method
               - Charge customer
               - Handle payment failures
            
            5. **Fulfill Order** (Parallel Gateway)
               - Pick items (User Task)
               - Pack items (User Task)
               - Generate shipping label (Service Task)
            
            6. **Ship Order** (Send Task)
               - Hand over to carrier
               - Send tracking information
            
            7. **Close Order** (End Event)
            
            **Exception Handling:**
            - Payment timeout (Timer Boundary)
            - Out of stock (Error Boundary)
            - Customer cancellation (Message Boundary)
            
            **Key Patterns Used:**
            - Compensation for payment reversal
            - Parallel processing for fulfillment
            - Event-based gateway for payment confirmation
            """)
        
        # Code example
        st.code("""
# BPMN XML snippet for Order Process
<process id="OrderProcess" name="Order to Cash">
  <startEvent id="OrderReceived" name="Order Received">
    <messageEventDefinition messageRef="OrderMessage"/>
  </startEvent>
  
  <serviceTask id="CheckInventory" name="Check Inventory">
    <incoming>flow1</incoming>
    <outgoing>flow2</outgoing>
  </serviceTask>
  
  <exclusiveGateway id="InventoryDecision" name="In Stock?">
    <incoming>flow2</incoming>
    <outgoing>flow3</outgoing>
    <outgoing>flow4</outgoing>
  </exclusiveGateway>
  
  <subProcess id="ProcessPayment" name="Process Payment">
    <incoming>flow3</incoming>
    <outgoing>flow5</outgoing>
    <!-- Payment sub-process details -->
  </subProcess>
</process>
        """, language="xml")
    
    elif example_type == "HR Processes":
        st.markdown("### 👥 Employee Onboarding Process")
        
        with st.expander("Onboarding Workflow"):
            st.markdown("""
            **Process Participants:**
            - HR Department
            - IT Department
            - Direct Manager
            - New Employee
            
            **Process Flow:**
            
            1. **Pre-Boarding** (1 week before start)
               - Send welcome email (Send Task)
               - Prepare workspace (User Task - Manager)
               - Order equipment (User Task - IT)
               - Create accounts (Service Task - IT)
            
            2. **First Day** (Event Sub-Process)
               - Welcome meeting (User Task - HR)
               - IT setup (User Task - IT)
               - Team introduction (User Task - Manager)
               - Complete paperwork (User Task - Employee)
            
            3. **First Week**
               - Training schedule (User Task - HR)
               - System access verification (User Task - IT)
               - Initial assignments (User Task - Manager)
            
            4. **30-Day Check-in** (Timer Event)
               - Performance discussion
               - Feedback collection
               - Adjustment planning
            
            **Automation Opportunities:**
            - Account creation via API
            - Training enrollment system
            - Document generation
            - Reminder notifications
            """)

def show_cmmn_introduction():
    """Show CMMN introduction"""
    
    st.markdown("## 🎨 Introduction to CMMN (Case Management Model and Notation)")
    
    st.info("CMMN complements BPMN by handling unpredictable, knowledge-intensive work")
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("""
        ### When to Use CMMN vs BPMN
        
        **Use BPMN when:**
        - Process is predictable
        - Steps follow defined sequence
        - Automation is primary goal
        - Repeatable workflows
        
        **Use CMMN when:**
        - Work is knowledge-intensive
        - Sequence is unpredictable
        - Decisions made at runtime
        - Case-by-case variations
        """)
    
    with col2:
        st.markdown("""
        ### CMMN Core Concepts
        
        **Case File:**
        - Central information container
        - Documents and data
        - Evolves during case
        
        **Plan Items:**
        - Tasks (similar to BPMN)
        - Stages (phases of work)
        - Milestones (achievements)
        - Event Listeners
        """)
    
    st.markdown("### CMMN Elements")
    
    with st.expander("CMMN Shapes and Symbols"):
        st.markdown("""
        **Basic Elements:**
        
        1. **Case Plan Model** (Folder shape)
           - Container for all case elements
           - Defines case boundaries
        
        2. **Stage** (Rectangle with angled corners)
           - Groups related plan items
           - Can be nested
           - May have entry/exit criteria
        
        3. **Task Types:**
           - **Human Task** (Rectangle with person icon)
           - **Process Task** (Rectangle with chevron) - calls BPMN process
           - **Case Task** (Rectangle with folder) - calls another case
           - **Decision Task** (Rectangle with table) - calls DMN decision
        
        4. **Milestone** (Rounded rectangle)
           - Represents achievement/goal
           - No work, just recognition
        
        5. **Event Listener** (Double circle)
           - Waits for events
           - Can trigger plan items
        
        6. **Sentries** (Diamond on border)
           - Entry criteria (white diamond)
           - Exit criteria (black diamond)
           - Define when items become available/complete
        """)
    
    # CMMN Example
    st.markdown("### Example: Insurance Claim Case")
    st.code("""
    Case: Insurance Claim
    ├─ Stage: Initial Assessment
    │   ├─ Collect Documents (Human Task) [Entry: Claim Received]
    │   ├─ Verify Coverage (Process Task)
    │   └─ Milestone: Initial Review Complete
    ├─ Stage: Investigation [Entry: Documents Complete]
    │   ├─ Investigate Claim (Human Task)
    │   ├─ Request Expert Opinion (Human Task) [Manual Activation]
    │   └─ Milestone: Investigation Complete
    └─ Stage: Resolution
        ├─ Make Decision (Decision Task)
        ├─ Process Payment (Process Task) [Entry: Approved]
        └─ Send Denial Letter (Human Task) [Entry: Denied]
    """)

def show_dmn_introduction():
    """Show DMN introduction"""
    
    st.markdown("## 🧮 Introduction to DMN (Decision Model and Notation)")
    
    st.info("DMN separates business decisions from process flow, making them reusable and maintainable")
    
    # DMN Overview
    st.markdown("""
    ### What is DMN?
    
    DMN provides a standard for modeling and executing business decisions. It consists of:
    
    1. **Decision Requirements Diagram (DRD)** - Shows decision dependencies
    2. **Decision Tables** - Define decision logic
    3. **FEEL Expression Language** - Formal expression language
    4. **Boxed Expressions** - Other decision logic representations
    """)
    
    # Decision Table Example
    st.markdown("### Decision Table Structure")
    
    with st.expander("Credit Score Decision Example"):
        st.markdown("""
        **Decision: Determine Loan Approval**
        
        | Credit Score | Income | Debt Ratio | Employment | **Approval** | **Interest Rate** |
        |--------------|--------|------------|------------|--------------|-------------------|
        | >= 750 | > 50000 | < 30% | > 2 years | Approved | 3.5% |
        | >= 700 | > 75000 | < 40% | > 1 year | Approved | 4.5% |
        | >= 650 | > 100000 | < 25% | > 3 years | Approved | 5.5% |
        | >= 600 | > 50000 | < 20% | > 5 years | Review | 6.5% |
        | < 600 | - | - | - | Denied | - |
        
        **Hit Policy: First (F)**
        - Rules evaluated top to bottom
        - First matching rule applies
        """)
    
    # DMN Integration
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("""
        ### DMN in BPMN
        
        **Business Rule Task:**
        ```
        Process Order → [BR] Check Discount → Apply Pricing
        ```
        
        The Business Rule Task calls a DMN decision table to determine applicable discounts based on:
        - Customer type
        - Order volume  
        - Season
        - Payment terms
        """)
    
    with col2:
        st.markdown("""
        ### DMN in CMMN
        
        **Decision Task:**
        ```
        Case: Loan Application
        └─ [D] Determine Eligibility
        ```
        
        The Decision Task evaluates multiple criteria using DMN to decide loan eligibility and terms.
        """)
    
    # FEEL Examples
    st.markdown("### FEEL Expression Examples")
    st.code("""
    // Simple comparisons
    age >= 18
    income > 50000 and creditScore >= 700
    
    // String operations
    contains(customerType, "Premium")
    upper case(country) = "USA"
    
    // Date operations
    now() - birthDate > duration("P18Y")
    applicationDate + duration("P30D")
    
    // Lists
    status in ["Active", "Premium", "VIP"]
    sum(orderItems.price) > 1000
    
    // Conditionals
    if creditScore >= 750 then "Excellent" 
    else if creditScore >= 700 then "Good"
    else "Fair"
    """, language="javascript")

def show_best_practices():
    """Show BPMN best practices"""
    
    st.markdown("## 💡 BPMN Best Practices")
    
    practice_area = st.selectbox(
        "Select Practice Area:",
        ["Modeling Guidelines", "Naming Conventions", "Layout and Style", "Common Mistakes", "Tool Tips"]
    )
    
    if practice_area == "Modeling Guidelines":
        st.markdown("""
        ### 📐 Modeling Guidelines
        
        **1. Start Simple**
        - Begin with happy path
        - Add exceptions incrementally
        - Validate with stakeholders often
        
        **2. Model at the Right Level**
        - Strategic level: High-level, few activities
        - Operational level: Detailed, executable
        - Don't mix levels in one diagram
        
        **3. Decomposition Strategy**
        - 5-15 activities per diagram
        - Use sub-processes for complexity
        - Create process hierarchy
        
        **4. Explicit Over Implicit**
        - Model all paths explicitly
        - Show error handling
        - Document assumptions
        
        **5. Business Language**
        - Use business terms, not technical
        - Avoid system-specific terminology
        - Focus on "what" not "how"
        """)
    
    elif practice_area == "Naming Conventions":
        st.markdown("""
        ### 📝 Naming Conventions
        
        **Activities (Tasks):**
        - Verb + Noun format
        - ✅ "Validate Order"
        - ❌ "Validation"
        - ✅ "Send Invoice to Customer"
        - ❌ "Invoice"
        
        **Events:**
        - Past tense for things that happened
        - ✅ "Order Received"
        - ✅ "Payment Confirmed"
        - Question form for conditions
        - ✅ "Is Customer Premium?"
        
        **Gateways:**
        - Question format for exclusive
        - ✅ "Payment Method?"
        - ✅ "Approved?"
        - State what happens for parallel
        - ✅ "Process All"
        
        **Pools/Lanes:**
        - Organization/Department/Role name
        - ✅ "Sales Department"
        - ✅ "Customer"
        - ❌ "Pool1"
        
        **Data Objects:**
        - Noun representing the data
        - ✅ "Invoice"
        - ✅ "Customer Record"
        - State when relevant
        - ✅ "Invoice [Paid]"
        """)
    
    elif practice_area == "Layout and Style":
        st.markdown("""
        ### 🎨 Layout and Style Guide
        
        **Flow Direction:**
        - Left to right (preferred)
        - Top to bottom (alternative)
        - Consistent throughout model
        
        **Element Alignment:**
        - Align elements on grid
        - Equal spacing between elements
        - Straight sequence flows
        
        **Crossing Lines:**
        - Minimize crossings
        - Use link events if needed
        - Bridge symbol for unavoidable crosses
        
        **Color Usage:**
        - Use sparingly and consistently
        - Red for errors/exceptions
        - Green for success/happy path
        - Blue for information
        
        **Text and Annotations:**
        - Keep labels concise
        - Use text annotations for clarification
        - Don't clutter the diagram
        
        **Pool/Lane Sizing:**
        - Size lanes based on content
        - Keep pools compact
        - Leave whitespace for readability
        """)
    
    elif practice_area == "Common Mistakes":
        st.markdown("""
        ### ⚠️ Common BPMN Mistakes to Avoid
        
        **1. Incorrect Gateway Usage**
        ❌ Using XOR when AND is needed
        ❌ Missing merge gateways
        ❌ Conditions on AND gateways
        
        **2. Event Misuse**
        ❌ Multiple start events in a pool
        ❌ Timer events in wrong positions
        ❌ Message flows within same pool
        
        **3. Data Flow Errors**
        ❌ Sequence flow carrying data
        ❌ Data associations crossing pools
        ❌ Missing data objects for key information
        
        **4. Scope Confusion**
        ❌ Mixing process levels
        ❌ Technical implementation details
        ❌ System architecture in business process
        
        **5. Over-Complexity**
        ❌ Too many elements in one diagram
        ❌ Unnecessary gateways
        ❌ Modeling every exception
        
        **6. Pool/Lane Violations**
        ❌ Sequence flow between pools
        ❌ Activities outside lanes
        ❌ Empty pools or lanes
        """)
    
    # Quick Tips Section
    st.markdown("---")
    st.markdown("### 🚀 Quick Tips")
    
    tips = [
        "Always start with a single start event per pool",
        "End events are not optional - every path must end",
        "Use sub-processes to hide complexity",
        "Message flows only between different pools",
        "Validate your model by 'token simulation'",
        "Keep diagrams at consistent abstraction level",
        "Use meaningful IDs for automation",
        "Document assumptions in text annotations",
        "Review with stakeholders frequently",
        "Consider both happy path and exceptions"
    ]
    
    # Random tip display
    if st.button("💡 Get Random Tip"):
        st.info(random.choice(tips))
    
    # Resources Section
    st.markdown("---")
    st.markdown("### 📚 Additional Resources")
    
    with st.expander("Recommended Reading"):
        st.markdown("""
        **Books:**
        - "Real-Life BPMN" by Freund & Rücker
        - "BPMN Method and Style" by Bruce Silver
        - "Fundamentals of Business Process Management" by Dumas et al.
        
        **Specifications:**
        - [BPMN 2.0 Specification](https://www.omg.org/spec/BPMN/2.0/)
        - [CMMN 1.1 Specification](https://www.omg.org/spec/CMMN/)
        - [DMN 1.3 Specification](https://www.omg.org/spec/DMN/)
        
        **Tools:**
        - Camunda Modeler (free, open source)
        - Signavio Process Manager
        - Bizagi Modeler
        - Draw.io / Diagrams.net
        
        **Communities:**
        - BPMN.io Forum
        - Camunda Community
        - BPM+ Community
        """)

# Header Section
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🔄 BPMN 2.0 Complete Training Path
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Master Business Process Model and Notation with CMMN and DMN
    </p>
</div>
""", unsafe_allow_html=True)

# Training Progress Tracker
if 'bpmn_progress' not in st.session_state:
    st.session_state.bpmn_progress = {
        'basics': 0,
        'intermediate': 0,
        'advanced': 0,
        'practice': 0
    }

# Progress Overview
col1, col2, col3, col4 = st.columns(4)
with col1:
    st.metric("Basics", f"{st.session_state.bpmn_progress['basics']}%", 
              delta=f"+{st.session_state.bpmn_progress['basics']}%" if st.session_state.bpmn_progress['basics'] > 0 else None)
with col2:
    st.metric("Intermediate", f"{st.session_state.bpmn_progress['intermediate']}%",
              delta=f"+{st.session_state.bpmn_progress['intermediate']}%" if st.session_state.bpmn_progress['intermediate'] > 0 else None)
with col3:
    st.metric("Advanced", f"{st.session_state.bpmn_progress['advanced']}%",
              delta=f"+{st.session_state.bpmn_progress['advanced']}%" if st.session_state.bpmn_progress['advanced'] > 0 else None)
with col4:
    st.metric("Practice", f"{st.session_state.bpmn_progress['practice']}%",
              delta=f"+{st.session_state.bpmn_progress['practice']}%" if st.session_state.bpmn_progress['practice'] > 0 else None)

# Main Navigation Tabs
tab1, tab2, tab3, tab4, tab5, tab6 = st.tabs([
    "📚 Learning Path", 
    "🎯 Quick Reference", 
    "🔧 Practical Examples",
    "🎨 CMMN Introduction",
    "🧮 DMN Introduction",
    "💡 Best Practices"
])

with tab1:
    show_learning_path()

with tab2:
    show_quick_reference()

with tab3:
    show_practical_examples()

with tab4:
    show_cmmn_introduction()

with tab5:
    show_dmn_introduction()

with tab6:
    show_best_practices()