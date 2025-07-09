"""
Smalltalk Training Data Preparation for LLM Fine-Tuning
Extracts patterns from your MgX Smalltalk codebase for training
"""

import re
import json
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from collections import defaultdict

logger = logging.getLogger(__name__)

class SmalltalkTrainingExtractor:
    """
    Extracts Smalltalk code patterns for LLM fine-tuning
    Specializes in MgX/WHATS'On codebase patterns
    """
    
    def __init__(self):
        self.class_pattern = re.compile(r"(\w+)\s+subclass:\s+#(\w+)")
        self.method_pattern = re.compile(r"^(\w+)>>(\w+)", re.MULTILINE)
        self.category_pattern = re.compile(r"category:\s+'([^']+)'")
        self.comment_pattern = re.compile(r'"([^"]*)"')
        
        # MgX-specific patterns
        self.mgx_patterns = {
            'store': re.compile(r'Store\s+subclass:'),
            'rights': re.compile(r'Rights|License|Territory'),
            'schedule': re.compile(r'Schedule|Program|Broadcast'),
            'validator': re.compile(r'validate|Validator|Validation'),
            'manager': re.compile(r'Manager|Controller|Handler')
        }
    
    def extract_training_data(self, 
                            directory: Path,
                            output_file: str = "smalltalk_training.jsonl") -> Dict:
        """
        Extract training examples from Smalltalk files
        
        Returns dict with statistics and saves training data to file
        """
        directory = Path(directory)
        training_examples = []
        stats = defaultdict(int)
        
        # Find all Smalltalk files
        st_files = list(directory.rglob("*.st"))
        logger.info(f"Found {len(st_files)} Smalltalk files")
        
        for st_file in st_files:
            try:
                content = st_file.read_text(encoding='utf-8', errors='ignore')
                
                # Extract different types of examples
                class_examples = self._extract_class_examples(content, st_file)
                method_examples = self._extract_method_examples(content, st_file)
                pattern_examples = self._extract_pattern_examples(content, st_file)
                
                training_examples.extend(class_examples)
                training_examples.extend(method_examples)
                training_examples.extend(pattern_examples)
                
                stats['files_processed'] += 1
                stats['classes_found'] += len(class_examples)
                stats['methods_found'] += len(method_examples)
                stats['patterns_found'] += len(pattern_examples)
                
            except Exception as e:
                logger.error(f"Error processing {st_file}: {e}")
                stats['errors'] += 1
        
        # Save training data
        output_path = Path(output_file)
        with open(output_path, 'w') as f:
            for example in training_examples:
                f.write(json.dumps(example) + '\n')
        
        stats['total_examples'] = len(training_examples)
        stats['output_file'] = str(output_path)
        
        return dict(stats)
    
    def _extract_class_examples(self, content: str, file_path: Path) -> List[Dict]:
        """Extract class definition examples"""
        examples = []
        
        # Find class definitions
        for match in self.class_pattern.finditer(content):
            superclass = match.group(1)
            classname = match.group(2)
            
            # Extract the full class definition
            class_start = match.start()
            class_end = content.find('\n\n', class_start)
            if class_end == -1:
                class_end = len(content)
            
            class_definition = content[class_start:class_end].strip()
            
            # Create training example
            example = {
                "instruction": f"Create a Smalltalk class that inherits from {superclass} for the MgX system",
                "input": f"The class should be named {classname} and follow MgX patterns",
                "output": class_definition,
                "type": "class_definition",
                "source": str(file_path.name)
            }
            
            examples.append(example)
            
            # Also create pattern-based example
            if any(pattern.search(classname) for pattern in self.mgx_patterns.values()):
                pattern_example = {
                    "instruction": f"Show me how MgX implements {self._identify_pattern(classname)}",
                    "input": f"I need to understand the {classname} implementation",
                    "output": class_definition,
                    "type": "pattern_example",
                    "source": str(file_path.name)
                }
                examples.append(pattern_example)
        
        return examples
    
    def _extract_method_examples(self, content: str, file_path: Path) -> List[Dict]:
        """Extract method implementation examples"""
        examples = []
        
        # Split by method definitions
        methods = re.split(r'\n![\w\s]+methodsFor:', content)
        
        for method_section in methods[1:]:  # Skip first split
            # Extract category
            category_match = re.search(r"'([^']+)'!", method_section)
            category = category_match.group(1) if category_match else "uncategorized"
            
            # Extract individual methods
            method_impls = re.split(r'\n\n(?=\w+)', method_section)
            
            for method_impl in method_impls:
                if not method_impl.strip() or method_impl.strip() == '!':
                    continue
                
                # Get method name
                first_line = method_impl.strip().split('\n')[0]
                method_name = first_line.split()[0] if first_line else "unknown"
                
                # Create training examples
                example = {
                    "instruction": f"Write a Smalltalk method for {category}",
                    "input": f"Implement {method_name} following MgX conventions",
                    "output": method_impl.strip(),
                    "type": "method_implementation",
                    "category": category,
                    "source": str(file_path.name)
                }
                examples.append(example)
                
                # Create task-specific examples
                if 'validat' in method_name.lower():
                    validation_example = {
                        "instruction": "Write a validation method in MgX style",
                        "input": f"Create a method like {method_name} for validation",
                        "output": method_impl.strip(),
                        "type": "validation_pattern",
                        "source": str(file_path.name)
                    }
                    examples.append(validation_example)
        
        return examples
    
    def _extract_pattern_examples(self, content: str, file_path: Path) -> List[Dict]:
        """Extract MgX-specific pattern examples"""
        examples = []
        
        # Look for specific patterns
        for pattern_name, pattern_regex in self.mgx_patterns.items():
            if pattern_regex.search(content):
                # Extract relevant section
                matches = list(pattern_regex.finditer(content))
                
                for match in matches[:3]:  # Limit to 3 examples per pattern
                    # Get surrounding context
                    start = max(0, match.start() - 200)
                    end = min(len(content), match.end() + 500)
                    context = content[start:end]
                    
                    example = {
                        "instruction": f"Show me how MgX implements {pattern_name} patterns",
                        "input": f"I need an example of {pattern_name} in the MgX system",
                        "output": context,
                        "type": f"{pattern_name}_pattern",
                        "source": str(file_path.name)
                    }
                    examples.append(example)
        
        return examples
    
    def _identify_pattern(self, name: str) -> str:
        """Identify which MgX pattern a name belongs to"""
        name_lower = name.lower()
        for pattern_name, pattern_regex in self.mgx_patterns.items():
            if pattern_regex.search(name):
                return pattern_name
        return "component"
    
    def create_fine_tuning_dataset(self,
                                 training_file: str,
                                 output_format: str = "openai") -> Path:
        """
        Convert training data to format for fine-tuning
        
        Formats:
        - openai: For OpenAI fine-tuning
        - alpaca: For local Alpaca-style training
        - conversations: For conversation-style training
        """
        input_path = Path(training_file)
        output_path = input_path.with_suffix(f'.{output_format}.jsonl')
        
        with open(input_path, 'r') as infile, open(output_path, 'w') as outfile:
            for line in infile:
                example = json.loads(line)
                
                if output_format == "openai":
                    formatted = {
                        "messages": [
                            {"role": "system", "content": "You are an expert Smalltalk developer specializing in the MgX/WHATS'On broadcasting system."},
                            {"role": "user", "content": f"{example['instruction']}\n{example['input']}"},
                            {"role": "assistant", "content": example['output']}
                        ]
                    }
                
                elif output_format == "alpaca":
                    formatted = {
                        "instruction": example['instruction'],
                        "input": example['input'],
                        "output": example['output']
                    }
                
                elif output_format == "conversations":
                    formatted = {
                        "conversations": [
                            {"from": "system", "value": "You are an expert in MgX Smalltalk development."},
                            {"from": "human", "value": f"{example['instruction']}\n{example['input']}"},
                            {"from": "assistant", "value": example['output']}
                        ]
                    }
                
                outfile.write(json.dumps(formatted) + '\n')
        
        return output_path
    
    def create_validation_set(self, 
                            training_file: str,
                            split_ratio: float = 0.9) -> Tuple[Path, Path]:
        """Split training data into train/validation sets"""
        import random
        
        # Load all examples
        examples = []
        with open(training_file, 'r') as f:
            for line in f:
                examples.append(json.loads(line))
        
        # Shuffle and split
        random.shuffle(examples)
        split_point = int(len(examples) * split_ratio)
        
        train_examples = examples[:split_point]
        val_examples = examples[split_point:]
        
        # Save splits
        train_path = Path(training_file).with_suffix('.train.jsonl')
        val_path = Path(training_file).with_suffix('.val.jsonl')
        
        with open(train_path, 'w') as f:
            for example in train_examples:
                f.write(json.dumps(example) + '\n')
        
        with open(val_path, 'w') as f:
            for example in val_examples:
                f.write(json.dumps(example) + '\n')
        
        return train_path, val_path


def prepare_mgx_training_data(whatson_dir: str = "C:/whatson") -> Dict:
    """
    Main function to prepare MgX Smalltalk training data
    """
    extractor = SmalltalkTrainingExtractor()
    
    logger.info(f"Extracting training data from {whatson_dir}")
    
    # Extract examples
    stats = extractor.extract_training_data(
        Path(whatson_dir),
        "mgx_smalltalk_training.jsonl"
    )
    
    # Create different formats
    logger.info("Creating fine-tuning formats...")
    openai_file = extractor.create_fine_tuning_dataset(
        "mgx_smalltalk_training.jsonl",
        "openai"
    )
    
    alpaca_file = extractor.create_fine_tuning_dataset(
        "mgx_smalltalk_training.jsonl",
        "alpaca"
    )
    
    # Create train/val split
    train_file, val_file = extractor.create_validation_set(
        "mgx_smalltalk_training.jsonl"
    )
    
    stats['formats_created'] = {
        'openai': str(openai_file),
        'alpaca': str(alpaca_file),
        'train_split': str(train_file),
        'validation_split': str(val_file)
    }
    
    return stats


if __name__ == "__main__":
    # Example usage
    logging.basicConfig(level=logging.INFO)
    
    # Prepare training data
    stats = prepare_mgx_training_data()
    
    print("\nTraining Data Preparation Complete!")
    print(f"Total examples: {stats['total_examples']}")
    print(f"Classes found: {stats['classes_found']}")
    print(f"Methods found: {stats['methods_found']}")
    print(f"Patterns found: {stats['patterns_found']}")
    print(f"\nOutput files created:")
    for format_name, file_path in stats['formats_created'].items():
        print(f"  {format_name}: {file_path}")