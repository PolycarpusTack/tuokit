# utils/graphql_utils.py
"""
GraphQL API generation utilities for Rails
"""
import re
from typing import Dict, List, Optional

class GraphQLHelper:
    """GraphQL schema and query utilities"""
    
    @staticmethod
    def extract_types(code: str) -> List[str]:
        """Extract GraphQL types from generated code"""
        types = re.findall(r'class (\w+Type) <', code)
        return list(set(types))
    
    @staticmethod
    def validate_query(query: str) -> Dict[str, any]:
        """Simple GraphQL query validation"""
        issues = []
        
        # Check for query/mutation/subscription keyword
        if not re.search(r'^\s*(query|mutation|subscription)', query, re.MULTILINE):
            issues.append("Missing operation type (query/mutation/subscription)")
        
        # Check for balanced braces
        if query.count('{') != query.count('}'):
            issues.append("Unbalanced braces")
        
        # Check for balanced parentheses
        if query.count('(') != query.count(')'):
            issues.append("Unbalanced parentheses")
        
        # Check for field selection
        if not re.search(r'{\s*\w+', query):
            issues.append("No fields selected")
        
        return {
            "valid": len(issues) == 0,
            "issues": issues
        }
    
    @staticmethod
    def parse_schema_definition(schema_text: str) -> Dict[str, List[str]]:
        """Parse GraphQL schema to extract types and fields"""
        result = {
            "types": [],
            "queries": [],
            "mutations": [],
            "fields": {}
        }
        
        # Extract type definitions
        type_matches = re.finditer(r'type (\w+) {\s*([^}]+)\s*}', schema_text)
        for match in type_matches:
            type_name = match.group(1)
            fields_text = match.group(2)
            
            result["types"].append(type_name)
            fields = re.findall(r'(\w+):\s*([^\n]+)', fields_text)
            result["fields"][type_name] = [(f[0], f[1].strip()) for f in fields]
        
        # Extract queries
        query_match = re.search(r'type Query {\s*([^}]+)\s*}', schema_text)
        if query_match:
            queries = re.findall(r'(\w+)(?:\([^)]*\))?:\s*([^\n]+)', query_match.group(1))
            result["queries"] = [q[0] for q in queries]
        
        # Extract mutations
        mutation_match = re.search(r'type Mutation {\s*([^}]+)\s*}', schema_text)
        if mutation_match:
            mutations = re.findall(r'(\w+)(?:\([^)]*\))?:\s*([^\n]+)', mutation_match.group(1))
            result["mutations"] = [m[0] for m in mutations]
        
        return result
    
    @staticmethod
    def generate_resolver_structure(resource: str, operation: str) -> str:
        """Generate resolver method structure"""
        templates = {
            "query": f"""
def {resource.lower()}
  {resource}.find(args[:id])
end

def {resource.lower()}s
  scope = {resource}.all
  scope = scope.where(filter_params) if args[:filter]
  scope.page(args[:page]).per(args[:per_page])
end""",
            
            "mutation": f"""
def create_{resource.lower()}
  {resource.lower()} = {resource}.new(args[:input])
  if {resource.lower()}.save
    {{ {resource.lower()}: {resource.lower()}, errors: [] }}
  else
    {{ {resource.lower()}: nil, errors: {resource.lower()}.errors.full_messages }}
  end
end

def update_{resource.lower()}
  {resource.lower()} = {resource}.find(args[:id])
  if {resource.lower()}.update(args[:input])
    {{ {resource.lower()}: {resource.lower()}, errors: [] }}
  else
    {{ {resource.lower()}: nil, errors: {resource.lower()}.errors.full_messages }}
  end
end""",
            
            "subscription": f"""
def {resource.lower()}_created
  # Subscription implementation
end"""
        }
        
        return templates.get(operation.lower(), "# Custom resolver needed")
    
    @staticmethod
    def suggest_optimizations(schema: str) -> List[Dict[str, str]]:
        """Suggest GraphQL performance optimizations"""
        suggestions = []
        
        # Check for potential N+1 queries
        if re.search(r'has_many|belongs_to|has_one', schema):
            suggestions.append({
                "issue": "Potential N+1 queries with associations",
                "solution": "Use BatchLoader or dataloader-pattern",
                "example": "BatchLoader::GraphQL.for(object.user_id).batch..."
            })
        
        # Check for missing pagination
        if re.search(r':\s*\[\w+\]', schema) and not re.search(r'first:|last:|after:|before:', schema):
            suggestions.append({
                "issue": "List fields without pagination",
                "solution": "Add connection-based pagination",
                "example": "field :items, Types::ItemType.connection_type"
            })
        
        # Check for missing field authorization
        if not re.search(r'authorize\s*:', schema):
            suggestions.append({
                "issue": "No field-level authorization detected",
                "solution": "Add authorization checks to sensitive fields",
                "example": "field :sensitive_data, String, authorize: :admin?"
            })
        
        return suggestions
