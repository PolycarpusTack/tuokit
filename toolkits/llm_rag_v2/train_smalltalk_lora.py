"""
Local LoRA Training Script for Smalltalk
Trains a LoRA adapter on your MgX/WHATS'On Smalltalk patterns
Designed for complete privacy - no data leaves your machine
"""

import os
import json
import torch
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional
import psycopg2
from psycopg2.extras import RealDictCursor

# Training libraries (install with: pip install transformers peft datasets accelerate)
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    TrainingArguments,
    Trainer,
    DataCollatorForLanguageModeling
)
from peft import (
    LoraConfig,
    get_peft_model,
    prepare_model_for_kbit_training,
    TaskType
)
from datasets import Dataset
import wandb  # Optional: for tracking experiments locally

logger = logging.getLogger(__name__)

class SmallTalkLoRATrainer:
    """
    Local LoRA trainer for Smalltalk code generation
    Optimized for MgX/WHATS'On patterns
    """
    
    def __init__(self, 
                 base_model: str = "deepseek-ai/deepseek-coder-6.7b-instruct",
                 output_dir: str = "./mgx-smalltalk-lora"):
        """
        Initialize the trainer
        
        Args:
            base_model: HuggingFace model ID or local path
            output_dir: Where to save the LoRA adapter
        """
        self.base_model_name = base_model
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        
        # LoRA configuration optimized for code
        self.lora_config = LoraConfig(
            r=16,  # LoRA rank - higher = more capacity but slower
            lora_alpha=32,  # LoRA scaling parameter
            target_modules=[  # Which layers to adapt
                "q_proj",
                "k_proj", 
                "v_proj",
                "o_proj",
                "gate_proj",
                "up_proj",
                "down_proj"
            ],
            lora_dropout=0.1,
            bias="none",
            task_type=TaskType.CAUSAL_LM,
        )
        
        self.model = None
        self.tokenizer = None
        self.training_data = []
        
    def load_base_model(self, load_in_8bit: bool = True):
        """
        Load the base model with memory optimization
        
        Args:
            load_in_8bit: Use 8-bit quantization to save memory
        """
        logger.info(f"Loading base model: {self.base_model_name}")
        
        # Load tokenizer
        self.tokenizer = AutoTokenizer.from_pretrained(self.base_model_name)
        self.tokenizer.pad_token = self.tokenizer.eos_token
        
        # Load model with quantization for memory efficiency
        if load_in_8bit:
            self.model = AutoModelForCausalLM.from_pretrained(
                self.base_model_name,
                load_in_8bit=True,
                device_map="auto",
                trust_remote_code=True
            )
            self.model = prepare_model_for_kbit_training(self.model)
        else:
            self.model = AutoModelForCausalLM.from_pretrained(
                self.base_model_name,
                device_map="auto",
                trust_remote_code=True,
                torch_dtype=torch.float16
            )
        
        # Apply LoRA
        self.model = get_peft_model(self.model, self.lora_config)
        self.model.print_trainable_parameters()
        
    def load_training_data_from_file(self, file_path: str):
        """Load training data from JSONL file"""
        logger.info(f"Loading training data from {file_path}")
        
        with open(file_path, 'r') as f:
            for line in f:
                example = json.loads(line)
                self.training_data.append(self._format_example(example))
        
        logger.info(f"Loaded {len(self.training_data)} training examples")
        
    def load_training_data_from_postgres(self, 
                                       db_url: str,
                                       min_quality_score: float = 0.7):
        """
        Load verified Q&A pairs from PostgreSQL
        
        Args:
            db_url: PostgreSQL connection string
            min_quality_score: Minimum quality score for inclusion
        """
        logger.info("Loading training data from PostgreSQL")
        
        conn = psycopg2.connect(db_url)
        cur = conn.cursor(cursor_factory=RealDictCursor)
        
        # Get verified examples
        cur.execute("""
            SELECT 
                question,
                answer,
                code_context,
                pattern_type,
                source_file
            FROM smalltalk_training_examples
            WHERE verified = TRUE
            AND quality_score >= %s
            AND NOT used_in_training
            ORDER BY quality_score DESC
        """, (min_quality_score,))
        
        for row in cur.fetchall():
            example = {
                "instruction": row['question'],
                "input": row['code_context'] or "",
                "output": row['answer'],
                "type": row['pattern_type'],
                "source": row['source_file']
            }
            self.training_data.append(self._format_example(example))
        
        # Mark as used
        cur.execute("""
            UPDATE smalltalk_training_examples
            SET used_in_training = TRUE
            WHERE verified = TRUE
            AND quality_score >= %s
        """, (min_quality_score,))
        
        conn.commit()
        cur.close()
        conn.close()
        
        logger.info(f"Loaded {len(self.training_data)} examples from PostgreSQL")
        
    def _format_example(self, example: Dict) -> Dict:
        """Format example for training"""
        # Create a conversation format
        if example.get('input'):
            user_content = f"{example['instruction']}\n\nContext:\n{example['input']}"
        else:
            user_content = example['instruction']
        
        messages = [
            {
                "role": "system",
                "content": "You are an expert Smalltalk developer specializing in the MgX/WHATS'On broadcasting system. Generate code following MgX patterns and conventions."
            },
            {
                "role": "user",
                "content": user_content
            },
            {
                "role": "assistant",
                "content": example['output']
            }
        ]
        
        # Format for the model
        text = self.tokenizer.apply_chat_template(
            messages,
            tokenize=False,
            add_generation_prompt=False
        )
        
        return {"text": text}
    
    def prepare_dataset(self, validation_split: float = 0.1):
        """
        Prepare training and validation datasets
        
        Args:
            validation_split: Fraction of data to use for validation
        """
        # Create dataset
        dataset = Dataset.from_list(self.training_data)
        
        # Tokenize
        def tokenize_function(examples):
            return self.tokenizer(
                examples["text"],
                truncation=True,
                padding="max_length",
                max_length=2048,  # Adjust based on your GPU memory
            )
        
        tokenized_dataset = dataset.map(
            tokenize_function,
            batched=True,
            remove_columns=["text"]
        )
        
        # Split into train/validation
        split_dataset = tokenized_dataset.train_test_split(
            test_size=validation_split,
            seed=42
        )
        
        return split_dataset["train"], split_dataset["test"]
    
    def train(self,
              num_epochs: int = 3,
              batch_size: int = 4,
              learning_rate: float = 2e-4,
              warmup_steps: int = 100,
              logging_steps: int = 10,
              save_steps: int = 500,
              eval_steps: int = 500,
              gradient_accumulation_steps: int = 4,
              use_wandb: bool = False):
        """
        Train the LoRA adapter
        
        Args:
            num_epochs: Number of training epochs
            batch_size: Batch size (reduce if OOM)
            learning_rate: Learning rate
            warmup_steps: Number of warmup steps
            logging_steps: Log every N steps
            save_steps: Save checkpoint every N steps
            eval_steps: Evaluate every N steps
            gradient_accumulation_steps: Accumulate gradients
            use_wandb: Use Weights & Biases for tracking
        """
        if not self.model:
            raise ValueError("Model not loaded. Call load_base_model() first.")
        
        if not self.training_data:
            raise ValueError("No training data loaded.")
        
        # Prepare datasets
        train_dataset, eval_dataset = self.prepare_dataset()
        
        # Setup training arguments
        training_args = TrainingArguments(
            output_dir=self.output_dir,
            num_train_epochs=num_epochs,
            per_device_train_batch_size=batch_size,
            per_device_eval_batch_size=batch_size,
            gradient_accumulation_steps=gradient_accumulation_steps,
            warmup_steps=warmup_steps,
            logging_steps=logging_steps,
            save_steps=save_steps,
            eval_steps=eval_steps,
            evaluation_strategy="steps",
            save_strategy="steps",
            learning_rate=learning_rate,
            fp16=True,  # Use mixed precision
            optim="adamw_torch",
            logging_dir=self.output_dir / "logs",
            load_best_model_at_end=True,
            metric_for_best_model="eval_loss",
            greater_is_better=False,
            report_to="wandb" if use_wandb else "none",
            run_name=f"mgx-smalltalk-{datetime.now().strftime('%Y%m%d-%H%M%S')}",
        )
        
        # Data collator
        data_collator = DataCollatorForLanguageModeling(
            tokenizer=self.tokenizer,
            mlm=False,  # Causal LM, not masked LM
        )
        
        # Create trainer
        trainer = Trainer(
            model=self.model,
            args=training_args,
            train_dataset=train_dataset,
            eval_dataset=eval_dataset,
            data_collator=data_collator,
        )
        
        # Train!
        logger.info("Starting training...")
        trainer.train()
        
        # Save final model
        logger.info("Saving final model...")
        trainer.save_model(self.output_dir / "final")
        self.tokenizer.save_pretrained(self.output_dir / "final")
        
        return trainer.state.log_history
    
    def test_generation(self, prompts: List[str]):
        """Test the trained model with sample prompts"""
        if not self.model:
            raise ValueError("Model not loaded.")
        
        self.model.eval()
        
        results = []
        for prompt in prompts:
            # Format as conversation
            messages = [
                {
                    "role": "system",
                    "content": "You are an expert Smalltalk developer specializing in the MgX/WHATS'On broadcasting system."
                },
                {
                    "role": "user",
                    "content": prompt
                }
            ]
            
            # Tokenize
            text = self.tokenizer.apply_chat_template(
                messages,
                tokenize=False,
                add_generation_prompt=True
            )
            inputs = self.tokenizer(text, return_tensors="pt").to(self.model.device)
            
            # Generate
            with torch.no_grad():
                outputs = self.model.generate(
                    **inputs,
                    max_new_tokens=512,
                    temperature=0.7,
                    do_sample=True,
                    top_p=0.95,
                    pad_token_id=self.tokenizer.eos_token_id
                )
            
            # Decode
            response = self.tokenizer.decode(outputs[0], skip_special_tokens=True)
            # Extract only the assistant's response
            response = response.split("assistant")[-1].strip()
            
            results.append({
                "prompt": prompt,
                "response": response
            })
            
        return results
    
    def create_ollama_modelfile(self):
        """Create Modelfile for Ollama integration"""
        modelfile_content = f"""# MgX Smalltalk LoRA Model
FROM {self.base_model_name}

# Apply LoRA adapter
ADAPTER {self.output_dir}/final

# System prompt
SYSTEM You are an expert Smalltalk developer specializing in the MgX/WHATS'On broadcasting system. You have deep knowledge of:
- MgX architecture and patterns
- Store subclasses and persistence
- Rights management and validation
- Schedule conflict resolution
- Broadcasting domain concepts

Generate code following MgX conventions and patterns.

# Parameters
PARAMETER temperature 0.7
PARAMETER top_p 0.95
PARAMETER top_k 40
PARAMETER repeat_penalty 1.1

# Template
TEMPLATE \"\"\"{{ if .System }}<|im_start|>system
{{ .System }}<|im_end|>
{{ end }}{{ if .Prompt }}<|im_start|>user
{{ .Prompt }}<|im_end|>
{{ end }}<|im_start|>assistant
\"\"\"
"""
        
        modelfile_path = self.output_dir / "Modelfile"
        with open(modelfile_path, 'w') as f:
            f.write(modelfile_content)
        
        logger.info(f"Created Ollama Modelfile at {modelfile_path}")
        logger.info("To use with Ollama: ollama create mgx-smalltalk -f Modelfile")
        
        return modelfile_path


def main():
    """Main training script"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Train LoRA for Smalltalk code generation")
    parser.add_argument("--base-model", default="deepseek-ai/deepseek-coder-6.7b-instruct",
                       help="Base model to use")
    parser.add_argument("--data-file", default="mgx_smalltalk_training.jsonl",
                       help="Training data file")
    parser.add_argument("--postgres-url", help="PostgreSQL URL for loading training data")
    parser.add_argument("--output-dir", default="./mgx-smalltalk-lora",
                       help="Output directory for LoRA adapter")
    parser.add_argument("--num-epochs", type=int, default=3,
                       help="Number of training epochs")
    parser.add_argument("--batch-size", type=int, default=4,
                       help="Batch size")
    parser.add_argument("--learning-rate", type=float, default=2e-4,
                       help="Learning rate")
    parser.add_argument("--use-wandb", action="store_true",
                       help="Use Weights & Biases for tracking")
    parser.add_argument("--test", action="store_true",
                       help="Test the model after training")
    
    args = parser.parse_args()
    
    # Setup logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Initialize trainer
    trainer = SmallTalkLoRATrainer(
        base_model=args.base_model,
        output_dir=args.output_dir
    )
    
    # Load model
    trainer.load_base_model()
    
    # Load training data
    if args.postgres_url:
        trainer.load_training_data_from_postgres(args.postgres_url)
    else:
        trainer.load_training_data_from_file(args.data_file)
    
    # Train
    history = trainer.train(
        num_epochs=args.num_epochs,
        batch_size=args.batch_size,
        learning_rate=args.learning_rate,
        use_wandb=args.use_wandb
    )
    
    # Test if requested
    if args.test:
        test_prompts = [
            "Create a Store subclass for managing Netflix streaming rights",
            "Write a validation method for checking broadcast territory conflicts",
            "Implement a method to calculate rights expiration dates",
            "Create a test case for the MgXScheduleManager class"
        ]
        
        results = trainer.test_generation(test_prompts)
        
        print("\n=== Test Results ===")
        for result in results:
            print(f"\nPrompt: {result['prompt']}")
            print(f"Response: {result['response'][:500]}...")
    
    # Create Ollama integration
    trainer.create_ollama_modelfile()
    
    print(f"\n✅ Training complete! Model saved to {args.output_dir}")
    print(f"📊 Final loss: {history[-1]['loss']:.4f}")
    print(f"\n🚀 To use with Ollama:")
    print(f"   cd {args.output_dir}")
    print(f"   ollama create mgx-smalltalk -f Modelfile")
    print(f"   ollama run mgx-smalltalk")


if __name__ == "__main__":
    main()