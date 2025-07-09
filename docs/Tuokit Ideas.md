Here are some quick-win applications you can build with Ollama + DeepSeekCoder that deliver immediate value while showcasing its capabilities:

### 1. **Code Explainer Tool**

```bash
echo "Explain this code:
def factorial(n):
    return 1 if n <= 1 else n * factorial(n-1)" | ollama run deepseek-coder
```

**Use Case**: Paste any code snippet to get plain-English explanations. Perfect for learning or documenting legacy code.

---

### 2. **API Client Generator**

```python
# Prompt: "Generate a Python API client for a REST service with endpoints:
# GET /users, POST /users, GET /users/{id}"
echo "Your prompt here" | ollama run deepseek-coder > api_client.py
```

**Result**: Instant production-ready API clients. Customize by adding "Make it async" or "Add error handling".

---

### 3. **SQL Query Generator**

```sql
/* Prompt: "Write a PostgreSQL query to find:
Top 5 customers by total purchase value in 2023" */
echo "Your prompt here" | ollama run deepseek-coder
```

**Output**: Complex SQL queries from natural language descriptions. Works for JOINs, window functions, etc.

---

### 4. **Data Format Converter**

```bash
# Convert JSON to YAML
echo 'Convert this to YAML: {"name": "Alice", "age": 30, "hobbies": ["hiking"]}' | ollama run deepseek-coder

# CSV to JSON conversion
echo 'Convert CSV to JSON: name,age\nAlice,30\nBob,25' | ollama run deepseek-coder
```

**Perfect for**: Quick data transformations during development.

---

### 5. **Regex Generator**

```python
# Prompt: "Create a Python regex to validate:
# Email addresses and ISO8601 dates"
echo "Your prompt here" | ollama run deepseek-coder
```

**Output**: Battle-tested regular expressions with explanation comments.

---

### 6. **Bug Fixer**

```python
# Paste broken code
echo "Fix this buggy function:
def sum_list(nums):
    total = 0
    for i in range(len(nums)):
        total =+ nums[i]
    return total" | ollama run deepseek-coder
```

**Saves time**: Gets fixes with explanations of the error.

---

### 7. **Documentation Generator**

```bash
# Generate docstrings for Python code
echo "Add Google-style docstrings to:
def calculate_tax(income, rate=0.15):
    return income * rate" | ollama run deepseek-coder
```

**Output**: Ready-to-use documentation with parameter descriptions and examples.

---

### 8. \*\*CLI Tool Generator

```python
# Prompt: "Create a Python CLI tool that counts files by extension in a directory"
echo "Your prompt here" | ollama run deepseek-coder > file_counter.py
```

**Run it**:

```bash
python file_counter.py ~/projects
```

---

### Pro Tips:

1. **Chain Prompts** for complex tasks:
   ```bash
   echo "First generate a data model for an e-commerce cart, then implement it in Python with SQLAlchemy" | ollama run deepseek-coder
   ```
2. **Add Constraints** for better results:
   > "Make it memory efficient..."  
   > "Optimize for large inputs..."  
   > "Use zero external dependencies..."
3. **Experiment with Temperature**:
   ```bash
   OLLAMA_NUM_CTX=4096 ollama run deepseek-coder --temperature 0.3
   ```
   (Lower = more deterministic, Higher = more creative)

### Example Workflow

```bash
# 1. Generate microservice boilerplate
echo "Create a FastAPI endpoint for /weather that accepts GET requests with location parameter and returns mock JSON data" | ollama run deepseek-coder > weather_api.py

# 2. Run it
uvicorn weather_api:app --reload
```

These examples work right out of the terminal and solve real development pain points. The model excels at code generation/explanation tasks under 100 lines of code. For best results, provide clear constraints and examples in your prompts!
