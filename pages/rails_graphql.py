# pages/rails_graphql.py
import streamlit as st
from utils import DatabaseManager, safe_ollama_generate

def build_graphql_api(resource, operations):
    """Generate GraphQL API implementation"""
    return safe_ollama_generate(
        model="deepseek-coder:latest",
        prompt=f"Create GraphQL API for {resource} with operations: {', '.join(operations)}",
        system=(
            "Implement complete solution using graphql-ruby gem:\n"
            "- Type definitions\n"
            "- Query/Mutation resolvers\n"
            "- N+1 prevention (BatchLoader)\n"
            "- Authentication\n"
            "- Error handling\n"
            "Include tests and example queries"
        )
    )['response']

def generate_example_query(resource, query_type):
    """Generate example GraphQL query"""
    return safe_ollama_generate(
        model="deepseek-coder:latest",
        prompt=f"Generate GraphQL {query_type} for {resource}",
        system="Create sample query with nested fields, variables, and fragments"
    )['response']

def show():
    st.title("🚀 Rails GraphQL API Builder")
    st.caption("Create production-ready GraphQL APIs for Rails applications")
    
    # Resource definition
    resource = st.text_input("Resource Name", "Post")
    fields = st.text_area("Resource Fields", 
                         "title: String!\ncontent: String!\nauthor: User!\ncomments: [Comment!]!",
                         height=150)
    
    # Operations
    operations = st.multiselect("Supported Operations", 
                              ["Query", "Mutation", "Subscription"],
                              default=["Query", "Mutation"])
    
    # Advanced options
    with st.sidebar:
        st.subheader("API Options")
        auth_method = st.selectbox("Authentication", ["None", "JWT", "Devise", "API Key"])
        pagination = st.radio("Pagination", ["Cursor", "Offset", "Relay"])
        enable_tracing = st.toggle("Enable Tracing", True)
        rate_limiting = st.toggle("Add Rate Limiting", False)
        enable_caching = st.toggle("Enable Caching", True)
        
        # Additional features
        st.subheader("Additional Features")
        features = st.multiselect("Include Features",
                                ["File Uploads", "Subscriptions", "Introspection", 
                                 "Field-level Auth", "Query Complexity"],
                                default=["Introspection"])
    
    if st.button("Generate API", type="primary"):
        with st.spinner("Building GraphQL schema..."):
            # Build comprehensive description
            description = f"{resource} with fields: {fields}"
            config = {
                "operations": operations,
                "auth": auth_method,
                "pagination": pagination,
                "features": features
            }
            
            full_prompt = f"{description} | Config: {config}"
            code = build_graphql_api(full_prompt, operations)
            
            # Display results
            st.subheader("GraphQL Implementation")
            
            # Code tabs
            tab1, tab2, tab3, tab4 = st.tabs(["Schema", "Example Queries", "Tests", "Setup"])
            
            with tab1:
                st.code(code, language="ruby")
                st.download_button("Download Schema", code, "graphql_schema.rb")
            
            with tab2:
                # Generate example queries
                if "Query" in operations:
                    st.subheader("Query Example")
                    query_example = generate_example_query(resource, "query")
                    st.code(query_example, language="graphql")
                
                if "Mutation" in operations:
                    st.subheader("Mutation Example")
                    mutation_example = generate_example_query(resource, "mutation")
                    st.code(mutation_example, language="graphql")
                
                if "Subscription" in operations:
                    st.subheader("Subscription Example")
                    subscription_example = generate_example_query(resource, "subscription")
                    st.code(subscription_example, language="graphql")
            
            with tab3:
                test_code = safe_ollama_generate(
                    model="deepseek-coder:latest",
                    prompt=f"Generate RSpec tests for GraphQL {resource} API",
                    system="Include query, mutation, and error case tests"
                )['response']
                st.code(test_code, language="ruby")
                st.download_button("Download Tests", test_code, "graphql_spec.rb")
            
            with tab4:
                st.markdown("""
                ### Setup Instructions
                
                1. **Add to Gemfile:**
                ```ruby
                gem 'graphql'
                gem 'batch-loader'
                gem 'graphiql-rails', group: :development
                ```
                
                2. **Install and generate:**
                ```bash
                bundle install
                rails generate graphql:install
                ```
                
                3. **Mount GraphiQL in routes:**
                ```ruby
                if Rails.env.development?
                  mount GraphiQL::Rails::Engine, at: "/graphiql", graphql_path: "/graphql"
                end
                ```
                
                4. **Add the generated schema files to your project**
                """)
            
            # GraphQL concepts
            with st.expander("📚 GraphQL Best Practices", expanded=True):
                col1, col2 = st.columns(2)
                
                with col1:
                    st.markdown("""
                    **Schema Design:**
                    - Use clear, consistent naming
                    - Implement proper error handling
                    - Version your API thoughtfully
                    - Document all fields
                    
                    **Performance:**
                    - Prevent N+1 with BatchLoader
                    - Implement query complexity limits
                    - Use caching strategically
                    - Monitor with tracing
                    """)
                
                with col2:
                    st.markdown("""
                    **Security:**
                    - Authenticate at resolver level
                    - Authorize field access
                    - Rate limit by complexity
                    - Disable introspection in production
                    
                    **Testing:**
                    - Test resolvers in isolation
                    - Verify error responses
                    - Check authorization rules
                    - Performance test queries
                    """)
                
                st.link_button("graphql-ruby Guides", "https://graphql-ruby.org/")
            
            # Save to knowledge base
            if st.button("💾 Save to Project"):
                db = DatabaseManager()
                if db.connected:
                    query_id = db.log_query(
                        tool="graphql_api",
                        model="deepseek-coder:latest",
                        prompt=full_prompt,
                        response=code,
                        metadata={
                            "tags": ["rails", "graphql", "api"],
                            "resource": resource,
                            "operations": operations
                        }
                    )
                    if query_id:
                        st.success("API saved to knowledge library!")
                else:
                    st.error("Could not connect to database")
    
    # Common patterns
    with st.expander("🎯 Common GraphQL Patterns"):
        st.markdown("""
        **Relay-style Connections:**
        ```ruby
        field :posts, Types::PostType.connection_type do
          argument :first, Int, required: false
          argument :after, String, required: false
        end
        ```
        
        **Field-level Authorization:**
        ```ruby
        field :sensitive_data, String, null: true do
          authorize :admin?
        end
        ```
        
        **Batch Loading:**
        ```ruby
        def author
          BatchLoader::GraphQL.for(object.author_id).batch do |ids, loader|
            User.where(id: ids).each { |user| loader.call(user.id, user) }
          end
        end
        ```
        """)

if __name__ == "__main__":
    show()
