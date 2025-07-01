"""
TuoKit Content Validation Module
Simple accuracy checking without over-engineering
Following TuoKit Architect: Practical validation that works today
"""

import re
from typing import Dict, List, Optional
from utils import safe_ollama_generate

class SimpleContentValidator:
    """
    Basic content validation - no multi-model consensus needed initially
    Focus on practical checks that prevent obvious errors
    """
    
    def __init__(self):
        self.common_errors = {
            # Pattern: Explanation
            r'\b(\d{4})\s+BC\b': 'Check historical dates',
            r'\b(\d+)\s*%': 'Verify percentage claims',
            r'\$[\d,]+': 'Confirm monetary values',
            r'\b\d+\s*(million|billion|trillion)\b': 'Verify large numbers',
            r'Chapter\s+\d+|Section\s+\d+': 'Check document references',
        }
    
    def quick_accuracy_check(self, generated_content: str, source_content: str) -> Dict:
        """
        Fast validation without multiple model calls
        Returns potential issues for user review
        """
        issues = []
        
        # 1. Check for hallucinated references
        doc_refs = re.findall(r'(Chapter|Section|Page|Figure)\s+\d+', generated_content)
        for ref in doc_refs:
            if ref not in source_content:
                issues.append({
                    'type': 'possible_hallucination',
                    'content': ref,
                    'suggestion': 'Reference not found in source'
                })
        
        # 2. Verify factual claims are grounded
        for pattern, description in self.common_errors.items():
            matches = re.findall(pattern, generated_content, re.IGNORECASE)
            for match in matches:
                # Simple check: is this claim in the source?
                if str(match) not in source_content:
                    issues.append({
                        'type': 'unverified_claim',
                        'content': match,
                        'suggestion': description
                    })
        
        # 3. Calculate simple accuracy score
        total_sentences = len(generated_content.split('.'))
        issue_count = len(issues)
        accuracy_score = max(0, 10 - (issue_count * 2))  # Simple scoring
        
        return {
            'accuracy_score': accuracy_score,
            'issues': issues[:5],  # Limit to top 5 issues
            'total_issues': len(issues),
            'confidence': 'high' if accuracy_score >= 8 else 'medium' if accuracy_score >= 6 else 'low'
        }
    
    def extract_key_claims(self, content: str) -> List[str]:
        """
        Extract factual claims that should be verified
        Simple implementation - no NLP libraries needed
        """
        claims = []
        
        # Look for sentences with factual indicators
        factual_patterns = [
            r'.*\bis\s+\w+',  # "X is Y" statements
            r'.*\bwas\s+\w+',  # Historical facts
            r'.*\bequals?\s+\d+',  # Mathematical statements
            r'.*\bconsists?\s+of',  # Composition statements
            r'.*\bdefin\w+\s+as',  # Definitions
        ]
        
        sentences = content.split('.')
        for sentence in sentences:
            for pattern in factual_patterns:
                if re.match(pattern, sentence.strip(), re.IGNORECASE):
                    claims.append(sentence.strip())
                    break
        
        return claims[:10]  # Return top 10 claims
    
    def source_correlation_check(self, claim: str, source_chunks: List[str]) -> float:
        """
        Simple keyword-based correlation check
        No embeddings needed for MVP
        """
        # Extract key terms from claim
        claim_words = set(re.findall(r'\b\w{4,}\b', claim.lower()))
        
        if not claim_words:
            return 0.0
        
        best_score = 0.0
        for chunk in source_chunks:
            chunk_words = set(re.findall(r'\b\w{4,}\b', chunk.lower()))
            
            # Simple Jaccard similarity
            intersection = len(claim_words & chunk_words)
            union = len(claim_words | chunk_words)
            
            score = intersection / union if union > 0 else 0
            best_score = max(best_score, score)
        
        return best_score


def validate_with_ai(content: str, source_preview: str) -> Optional[Dict]:
    """
    Single AI validation call - practical approach
    """
    prompt = f"""Review this generated study content for accuracy.

Source Material (first 1000 chars):
{source_preview[:1000]}

Generated Content:
{content}

Identify any potential inaccuracies or claims not supported by the source.
Format: List each issue on a new line starting with "- "
If no issues found, respond with "No accuracy issues detected."
"""
    
    try:
        response = safe_ollama_generate(
            prompt=prompt,
            model="deepseek-r1:1.5b",  # Use smaller model for speed
            system_prompt="You are a fact-checker reviewing educational content for accuracy."
        )
        
        if response and "No accuracy issues" not in response:
            issues = [
                line.strip('- ').strip() 
                for line in response.split('\n') 
                if line.strip().startswith('-')
            ]
            return {
                'ai_validation': True,
                'issues': issues[:5]  # Limit issues
            }
        
        return {'ai_validation': True, 'issues': []}
        
    except Exception as e:
        print(f"AI validation error: {e}")
        return None


# TODO: Add citation extraction for academic sources
# TODO: Implement fact database for common knowledge verification
# TODO: Add support for mathematical formula validation
# TODO: Create accuracy improvement suggestions
