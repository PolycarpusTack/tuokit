# 🎓 Side LLM Learning Project

## Project Overview
A personal exploration project to understand LLMs from the ground up, focusing on practical insights that could eventually benefit TuoKit while satisfying intellectual curiosity.

---

## EPIC 1: Tiny Smalltalk Language Model
*Build a small, focused LLM trained exclusively on Smalltalk code*

### Quality Gates:
- [ ] Runs on consumer GPU (RTX 3060 or better)
- [ ] Generates valid Smalltalk syntax
- [ ] Under 100M parameters
- [ ] Training completes in <24 hours

### TASK 1.1: Data Collection & Preparation
**Goal:** Create high-quality Smalltalk training dataset

#### SUBTASK 1.1.1: Collect Smalltalk repositories
```
Create data_collection/smalltalk_scraper.py:
- Scrape GitHub Smalltalk repos
- Download Pharo packages
- Extract GNU Smalltalk code
- Collect VisualWorks examples

Sources:
- GitHub API (search language:smalltalk)
- SqueakSource
- SmalltalkHub
- Public Pharo images

Target: 100MB+ of clean Smalltalk code

Quality checks:
- Valid Smalltalk syntax only
- Remove duplicates
- Filter quality code
- Preserve project structure
```

#### SUBTASK 1.1.2: Create Smalltalk tokenizer
```
Create tokenizer/smalltalk_tokenizer.py:
- Handle Smalltalk-specific syntax
- Preserve message sends
- Special tokens for blocks
- Method syntax awareness

Special considerations:
- Binary messages (#+, #-, etc.)
- Keyword messages (at:put:)
- Block syntax [:x | x + 1]
- Class definitions

Quality checks:
- Roundtrip perfectly
- Efficient encoding
- Preserves semantics
- Handles edge cases
```

#### SUBTASK 1.1.3: Prepare training data
```
Create data_prep/prepare_dataset.py:
- Clean code formatting
- Create train/val/test splits
- Generate instruction pairs
- Add special tokens

Dataset formats:
1. Next token prediction
2. Method completion
3. Class generation
4. Bug fixing pairs

Quality checks:
- Balanced dataset
- No data leakage
- Consistent formatting
- Meaningful splits
```

### TASK 1.2: Model Architecture Design
**Goal:** Design efficient architecture for code generation

#### SUBTASK 1.2.1: Implement baby transformer
```
Create model/tiny_transformer.py:
- 6-12 layers maximum
- Efficient attention
- RoPE or ALiBi positions
- Flash attention if available

Architecture decisions:
- Hidden size: 512-768
- Heads: 8-12
- FFN multiplier: 2.5
- Vocabulary: ~10K tokens

Quality checks:
- Fits in 8GB VRAM
- Forward pass <100ms
- Gradient checkpointing works
- ONNX exportable
```

#### SUBTASK 1.2.2: Code-aware modifications
```
Enhance model with:
- Syntax-aware attention
- Indentation encoding
- Method boundary detection
- Message send understanding

Code-specific features:
- Bracket matching layer
- Identifier consistency
- Type-aware embeddings
- Semantic role encoding

Quality checks:
- Improves code metrics
- No huge overhead
- Ablation tested
- Clear benefits
```

### TASK 1.3: Training Pipeline
**Goal:** Efficient training on consumer hardware

#### SUBTASK 1.3.1: Training script
```
Create training/train_smalltalk.py:
- Mixed precision training
- Gradient accumulation
- Learning rate scheduling
- Checkpoint management

Optimization targets:
- 1 epoch in <2 hours
- Stable training
- Memory efficient
- Resume capability

Quality checks:
- Uses all GPU memory
- No OOM errors
- Smooth loss curves
- Reproducible
```

#### SUBTASK 1.3.2: Evaluation metrics
```
Create evaluation/code_metrics.py:
- Syntax validity rate
- Method completion accuracy
- Class coherence score
- Message send correctness

Smalltalk-specific metrics:
- Valid message patterns
- Block syntax correctness
- Class/method structure
- Semantic consistency

Quality checks:
- Fast evaluation
- Meaningful metrics
- Correlates with quality
- Easy to interpret
```

---

## EPIC 2: Custom Tokenizer Research
*Deep dive into tokenization optimized for specific domains*

### Quality Gates:
- [ ] Better compression than GPT tokenizers
- [ ] Handles domain-specific patterns
- [ ] Fast encoding/decoding
- [ ] Transferable insights

### TASK 2.1: Multi-Modal Tokenizer
**Goal:** Build tokenizer for code + docs + tests

#### SUBTASK 2.1.1: Analyze token patterns
```
Create analysis/token_patterns.py:
- Frequency analysis by context
- Cross-language patterns
- Documentation patterns
- Test assertion patterns

Analyze:
- Variable naming patterns
- Comment structures
- Doc string formats
- Test vocabularies

Quality checks:
- Comprehensive analysis
- Statistical significance
- Visual outputs
- Clear insights
```

#### SUBTASK 2.1.2: Build hybrid tokenizer
```
Create tokenizer/hybrid_tokenizer.py:
- BPE for general text
- Word-level for keywords
- Char-level for identifiers
- Special handling for patterns

Features:
- Context-aware switching
- Compact encoding
- Semantic preservation
- Fast implementation

Quality checks:
- Better compression
- Preserves meaning
- Fast processing
- Memory efficient
```

### TASK 2.2: Domain Adaptation
**Goal:** Create tokenizers for specific TuoKit domains

#### SUBTASK 2.2.1: Rails tokenizer
```
Create tokenizer/rails_tokenizer.py:
- Rails conventions
- ERB template handling
- Route patterns
- Migration syntax

Special tokens:
- Rails methods
- HTTP verbs
- Database operations
- View helpers

Quality checks:
- Handles all Rails code
- Efficient encoding
- Semantic grouping
- Easy to extend
```

#### SUBTASK 2.2.2: Error log tokenizer
```
Create tokenizer/error_tokenizer.py:
- Stack trace patterns
- Error message structure
- File path handling
- Line number encoding

Optimizations:
- Compress repeated patterns
- Preserve error hierarchy
- Efficient path encoding
- Semantic error grouping

Quality checks:
- 50% better compression
- Preserves all info
- Fast processing
- Pattern learning
```

---

## EPIC 3: Attention Mechanism Visualization
*Build tools to understand what models "see"*

### Quality Gates:
- [ ] Real-time visualization
- [ ] Interactive exploration
- [ ] Clear insights gained
- [ ] Reusable components

### TASK 3.1: Attention Visualizer
**Goal:** See how models process code

#### SUBTASK 3.1.1: Basic attention viewer
```
Create viz/attention_viewer.py:
- Layer-by-layer view
- Head-specific patterns
- Token relationship graph
- Interactive selection

Features:
- Streamlit interface
- Real-time updates
- Pattern highlighting
- Export capabilities

Quality checks:
- Smooth interaction
- Clear visualizations
- Meaningful insights
- Educational value
```

#### SUBTASK 3.1.2: Code-aware visualization
```
Enhance with code understanding:
- Syntax highlighting integration
- AST overlay
- Semantic role coloring
- Message flow tracking

Smalltalk-specific:
- Message send paths
- Block scope visualization
- Class hierarchy overlay
- Method relationship

Quality checks:
- Enhances understanding
- No visual clutter
- Fast rendering
- Intuitive interface
```

### TASK 3.2: Pattern Discovery
**Goal:** Automatically find what models learn

#### SUBTASK 3.2.1: Attention pattern mining
```
Create analysis/pattern_miner.py:
- Cluster similar patterns
- Identify common motifs
- Track pattern evolution
- Correlate with performance

Patterns to find:
- Syntax matching
- Variable tracking
- Method boundaries
- Error patterns

Quality checks:
- Finds known patterns
- Discovers new ones
- Statistically sound
- Actionable insights
```

---

## EPIC 4: Model Compression Experiments
*Make models smaller while maintaining quality*

### Quality Gates:
- [ ] 10x smaller models
- [ ] <10% quality loss
- [ ] Real-time inference
- [ ] Practical techniques

### TASK 4.1: Distillation Pipeline
**Goal:** Transfer knowledge to tiny models

#### SUBTASK 4.1.1: Teacher-student setup
```
Create distillation/distill_pipeline.py:
- Teacher model wrapper
- Student architecture
- Distillation loss
- Training pipeline

Techniques:
- Soft label distillation
- Feature matching
- Attention transfer
- Progressive shrinking

Quality checks:
- Effective transfer
- Stable training
- Quality metrics
- Speed improvement
```

#### SUBTASK 4.1.2: Extreme compression
```
Push limits of compression:
- 1M parameter models
- Quantization aware
- Pruning strategies
- Mobile deployment ready

Target metrics:
- 100x smaller
- 50x faster
- <20% quality loss
- Real device testing

Quality checks:
- Actually deployable
- Maintains core ability
- Fast inference
- Small memory footprint
```

### TASK 4.2: Specialized Mini-Models
**Goal:** Tiny models for specific tasks

#### SUBTASK 4.2.1: Single-task models
```
Create specialized models:
- Syntax checker (500K params)
- Method namer (1M params)
- Error classifier (2M params)
- Comment generator (5M params)

Quality checks:
- Better than rules
- Instant inference
- High accuracy
- Easy integration
```

---

## EPIC 5: Fine-Tuning Research
*Efficient adaptation of base models*

### Quality Gates:
- [ ] Fine-tune in <1 hour
- [ ] Minimal data requirements
- [ ] No catastrophic forgetting
- [ ] Clear improvement metrics

### TASK 5.1: LoRA Implementation
**Goal:** Implement efficient fine-tuning

#### SUBTASK 5.1.1: Basic LoRA
```
Create finetuning/lora.py:
- LoRA layer implementation
- Rank selection logic
- Merge functionality
- Memory efficient training

Features:
- Multiple rank support
- Adaptive rank selection
- Quantized backbone
- Fast switching

Quality checks:
- Matches paper results
- Memory efficient
- Fast training
- Easy to use
```

#### SUBTASK 5.1.2: Code-aware LoRA
```
Enhance for code tasks:
- Syntax-aware adapters
- Task-specific ranks
- Multi-task support
- Incremental learning

Quality checks:
- Better than standard
- Preserves abilities
- Fast adaptation
- Clear benefits
```

---

## Learning Experiments Schedule

### Month 1: Foundation
- Week 1-2: Data collection and tokenizer
- Week 3-4: Basic model training

### Month 2: Understanding  
- Week 1-2: Attention visualization
- Week 3-4: Pattern discovery

### Month 3: Optimization
- Week 1-2: Model compression
- Week 3-4: Fine-tuning research

## Tools & Resources

### Required Tools:
- PyTorch 2.0+
- Transformers library
- Weights & Biases (free tier)
- CUDA toolkit
- 24GB+ RAM

### Compute Resources:
- Local: RTX 3060+ (12GB VRAM)
- Cloud: Colab Pro ($10/month)
- Alternative: vast.ai ($0.50/hour)

### Learning Resources:
- Karpathy's nanoGPT
- HuggingFace course
- Papers with Code
- Local LLM communities

## Success Metrics

### Technical Goals:
- Working Smalltalk model
- Novel tokenizer insights  
- Attention understanding
- Compression techniques
- Fine-tuning expertise

### Knowledge Goals:
- Deep understanding of transformers
- Practical training experience
- Optimization techniques
- Deployment strategies
- Research methodology

## Potential TuoKit Benefits

### Immediate Applications:
1. Better understanding of model limitations
2. Custom tokenizers for tools
3. Model behavior insights
4. Optimization techniques

### Future Possibilities:
1. Tiny specialized models for TuoKit
2. Custom fine-tuning pipelines
3. Better prompt engineering
4. Model debugging tools

### Knowledge Transfer:
1. Blog posts about findings
2. TuoKit integration guides
3. Community tools
4. Educational content

---

## Project Philosophy

### Core Principles:
- **Learn by building**: Every concept implemented
- **Start small**: Tiny models, clear goals
- **Document everything**: Future reference
- **Share findings**: Community benefit
- **Practical focus**: Applicable insights

### What This Is NOT:
- Not trying to beat GPT-4
- Not building production models
- Not competing with giants
- Not purely theoretical

### What This IS:
- Deep learning journey
- Practical experimentation
- Knowledge building
- Skill development
- Fun exploration

---

**Timeline: 3 months of evenings/weekends**

*Remember: The goal is learning, not launching. Every failed experiment teaches something valuable!*