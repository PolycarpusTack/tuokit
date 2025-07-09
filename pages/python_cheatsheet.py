"""
TuoKit - Python Cheat Sheet
Comprehensive Python reference for developers, analysts, and support staff
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="Python Cheat Sheet - TuoKit",
    page_icon="🐍",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="python_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        🐍 Python Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        Python syntax, patterns, and best practices for all skill levels
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search Python topics...", placeholder="Try: list comprehension, decorators, pandas, debugging...")

# Create role-based tabs
role_tab1, role_tab2, role_tab3, role_tab4 = st.tabs([
    "👨‍💻 Developer", 
    "📊 Analyst", 
    "🛠️ Support", 
    "📚 Reference"
])

# Helper function
def show_code_example(title, description, code, show=True):
    if show:
        st.markdown(f"### {title}")
        st.caption(description)
        st.code(code, language="python")
        if st.button(f"📋 Copy", key=f"copy_{hash(title)}"):
            st.toast(f"Copied {title} to clipboard!")
        st.markdown("---")

def matches_search(text, query):
    if not query:
        return True
    return query.lower() in text.lower()

# Developer Tab
with role_tab1:
    st.header("👨‍💻 Developer Reference")
    
    dev_tabs = st.tabs([
        "Data Structures", "Functions", "Classes", "Advanced", "Async", "Best Practices"
    ])
    
    with dev_tabs[0]:  # Data Structures
        st.subheader("📦 Data Structures & Collections")
        
        col1, col2 = st.columns(2)
        
        with col1:
            show_code_example(
                "Lists & List Comprehensions",
                "Creating and manipulating lists efficiently",
                """# List creation
numbers = [1, 2, 3, 4, 5]
mixed = [1, "hello", 3.14, True, None]
nested = [[1, 2], [3, 4], [5, 6]]

# List comprehensions
squares = [x**2 for x in range(10)]
evens = [x for x in range(20) if x % 2 == 0]
matrix = [[i*j for j in range(5)] for i in range(5)]

# With conditions
result = [x if x > 0 else 0 for x in numbers]

# Nested comprehension
flattened = [item for sublist in nested for item in sublist]

# List operations
numbers.append(6)           # Add to end
numbers.extend([7, 8, 9])   # Add multiple
numbers.insert(0, 0)        # Insert at index
removed = numbers.pop()     # Remove and return last
numbers.remove(3)           # Remove first occurrence
numbers.reverse()           # Reverse in place
numbers.sort()              # Sort in place

# Slicing
first_three = numbers[:3]   # [0, 1, 2]
last_three = numbers[-3:]   # Last 3 elements
every_other = numbers[::2]  # Every 2nd element
reversed_list = numbers[::-1]  # Reverse copy

# Advanced techniques
# Enumerate with start index
for i, val in enumerate(numbers, start=1):
    print(f"{i}: {val}")

# Zip multiple lists
names = ["Alice", "Bob", "Charlie"]
ages = [25, 30, 35]
for name, age in zip(names, ages):
    print(f"{name} is {age} years old")""",
                matches_search("list comprehension slice enumerate zip", search_query)
            )
            
        with col2:
            show_code_example(
                "Dictionaries & Sets",
                "Key-value pairs and unique collections",
                """# Dictionary creation
person = {"name": "Alice", "age": 30, "city": "NYC"}
person = dict(name="Alice", age=30, city="NYC")

# Dictionary comprehension
squares = {x: x**2 for x in range(10)}
filtered = {k: v for k, v in person.items() if v != "NYC"}

# Dictionary operations
person["email"] = "alice@example.com"  # Add/update
age = person.get("age", 0)             # Safe get with default
person.setdefault("country", "USA")    # Set if missing
removed = person.pop("city", None)     # Remove with default

# Merge dictionaries (Python 3.9+)
defaults = {"color": "blue", "size": "M"}
custom = {"size": "L", "price": 29.99}
merged = defaults | custom  # {'color': 'blue', 'size': 'L', 'price': 29.99}

# Dictionary methods
keys = list(person.keys())
values = list(person.values())
items = list(person.items())

# Sets
unique_numbers = {1, 2, 3, 4, 5}
from_list = set([1, 2, 2, 3, 3, 4])  # {1, 2, 3, 4}

# Set operations
a = {1, 2, 3, 4}
b = {3, 4, 5, 6}
union = a | b           # {1, 2, 3, 4, 5, 6}
intersection = a & b    # {3, 4}
difference = a - b      # {1, 2}
symmetric_diff = a ^ b  # {1, 2, 5, 6}

# Set comprehension
evens = {x for x in range(20) if x % 2 == 0}

# Frozen set (immutable)
constants = frozenset([1, 2, 3])

# defaultdict for automatic initialization
from collections import defaultdict
dd = defaultdict(list)
dd['key'].append('value')  # No KeyError

# Counter for counting
from collections import Counter
counts = Counter(['a', 'b', 'a', 'c', 'b', 'a'])
print(counts)  # Counter({'a': 3, 'b': 2, 'c': 1})""",
                matches_search("dictionary dict set comprehension defaultdict counter", search_query)
            )
            
    with dev_tabs[1]:  # Functions
        st.subheader("🔧 Functions & Functional Programming")
        
        show_code_example(
            "Function Features & Patterns",
            "Advanced function concepts and patterns",
            """# Function basics with type hints
def greet(name: str, greeting: str = "Hello") -> str:
    \"\"\"Greet a person with a custom greeting.
    
    Args:
        name: Person's name
        greeting: Greeting to use (default: "Hello")
    
    Returns:
        Formatted greeting string
    \"\"\"
    return f"{greeting}, {name}!"

# *args and **kwargs
def flexible_function(*args, **kwargs):
    print(f"Positional args: {args}")
    print(f"Keyword args: {kwargs}")
    
flexible_function(1, 2, 3, name="Alice", age=30)

# Unpacking arguments
def add(a, b, c):
    return a + b + c

numbers = [1, 2, 3]
result = add(*numbers)  # Unpacking list

params = {'a': 1, 'b': 2, 'c': 3}
result = add(**params)  # Unpacking dict

# Lambda functions
square = lambda x: x**2
add = lambda x, y: x + y
is_even = lambda x: x % 2 == 0

# Using lambdas with built-ins
numbers = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, numbers))
evens = list(filter(lambda x: x % 2 == 0, numbers))
from functools import reduce
total = reduce(lambda x, y: x + y, numbers)

# Decorators
def timer(func):
    import time
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} took {end - start:.4f} seconds")
        return result
    return wrapper

@timer
def slow_function():
    import time
    time.sleep(1)
    return "Done"

# Decorator with arguments
def retry(max_attempts=3):
    def decorator(func):
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    print(f"Attempt {attempt + 1} failed: {e}")
            return None
        return wrapper
    return decorator

@retry(max_attempts=5)
def unstable_api_call():
    # Simulated API call
    pass

# Closures
def make_multiplier(n):
    def multiplier(x):
        return x * n
    return multiplier

times_two = make_multiplier(2)
times_three = make_multiplier(3)

# Partial functions
from functools import partial
import os
join_with_slash = partial(os.path.join, "/")""",
            matches_search("function lambda decorator args kwargs closure", search_query)
        )
        
    with dev_tabs[2]:  # Classes
        st.subheader("🏗️ Classes & OOP")
        
        show_code_example(
            "Modern Python Classes",
            "Classes with best practices and patterns",
            """# Basic class with type hints
class Person:
    \"\"\"Represents a person with name and age.\"\"\"
    
    def __init__(self, name: str, age: int):
        self.name = name
        self.age = age
    
    def __repr__(self) -> str:
        return f"Person(name='{self.name}', age={self.age})"
    
    def __str__(self) -> str:
        return f"{self.name} ({self.age} years old)"
    
    def birthday(self) -> None:
        \"\"\"Increment age by 1.\"\"\"
        self.age += 1

# Dataclasses (Python 3.7+)
from dataclasses import dataclass, field
from typing import List, Optional

@dataclass
class Employee:
    name: str
    employee_id: int
    department: str = "General"
    skills: List[str] = field(default_factory=list)
    manager: Optional['Employee'] = None
    
    def add_skill(self, skill: str) -> None:
        if skill not in self.skills:
            self.skills.append(skill)

# Property decorators
class Temperature:
    def __init__(self, celsius: float = 0.0):
        self._celsius = celsius
    
    @property
    def celsius(self) -> float:
        return self._celsius
    
    @celsius.setter
    def celsius(self, value: float) -> None:
        if value < -273.15:
            raise ValueError("Temperature below absolute zero is not possible")
        self._celsius = value
    
    @property
    def fahrenheit(self) -> float:
        return self._celsius * 9/5 + 32
    
    @fahrenheit.setter
    def fahrenheit(self, value: float) -> None:
        self.celsius = (value - 32) * 5/9

# Class and static methods
class MathUtils:
    @staticmethod
    def add(x: float, y: float) -> float:
        return x + y
    
    @classmethod
    def from_string(cls, string_input: str):
        # Factory method
        return cls(*map(float, string_input.split(',')))

# Inheritance and abstract classes
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass
    
    @abstractmethod
    def perimeter(self) -> float:
        pass

class Rectangle(Shape):
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height
    
    def area(self) -> float:
        return self.width * self.height
    
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

# Context managers
class FileManager:
    def __init__(self, filename: str, mode: str):
        self.filename = filename
        self.mode = mode
        self.file = None
    
    def __enter__(self):
        self.file = open(self.filename, self.mode)
        return self.file
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.file:
            self.file.close()

# Using context manager
with FileManager('test.txt', 'w') as f:
    f.write('Hello, World!')""",
            matches_search("class dataclass property inheritance abstract context", search_query)
        )
        
    with dev_tabs[3]:  # Advanced
        st.subheader("🚀 Advanced Python Features")
        
        show_code_example(
            "Generators & Iterators",
            "Memory-efficient iteration patterns",
            """# Generator function
def fibonacci(n):
    \"\"\"Generate Fibonacci numbers up to n.\"\"\"
    a, b = 0, 1
    while a < n:
        yield a
        a, b = b, a + b

# Using generator
for num in fibonacci(100):
    print(num, end=' ')

# Generator expression
squares_gen = (x**2 for x in range(10))
sum_of_squares = sum(x**2 for x in range(10))

# Custom iterator
class Counter:
    def __init__(self, start, end):
        self.current = start
        self.end = end
    
    def __iter__(self):
        return self
    
    def __next__(self):
        if self.current < self.end:
            self.current += 1
            return self.current - 1
        raise StopIteration

# Infinite generator
def infinite_sequence():
    num = 0
    while True:
        yield num
        num += 1

# Using itertools
import itertools

# Useful itertools functions
# Chain multiple iterables
combined = itertools.chain([1, 2], [3, 4], [5, 6])

# Cycle through an iterable indefinitely
cycled = itertools.cycle(['A', 'B', 'C'])

# Combinations and permutations
items = ['A', 'B', 'C']
combos = list(itertools.combinations(items, 2))  # [('A', 'B'), ('A', 'C'), ('B', 'C')]
perms = list(itertools.permutations(items, 2))   # All ordered pairs

# Group by
data = [('A', 1), ('B', 2), ('A', 3), ('B', 4)]
grouped = itertools.groupby(data, key=lambda x: x[0])
for key, group in grouped:
    print(f"{key}: {list(group)}")

# Generator delegation (yield from)
def sub_generator():
    yield 1
    yield 2

def main_generator():
    yield from sub_generator()
    yield 3
    yield 4

# Send values to generator
def echo_generator():
    while True:
        value = yield
        print(f"Received: {value}")

gen = echo_generator()
next(gen)  # Prime the generator
gen.send("Hello")
gen.send("World")""",
            matches_search("generator yield iterator itertools", search_query)
        )

# Analyst Tab
with role_tab2:
    st.header("📊 Analyst Toolkit")
    
    analyst_tabs = st.tabs([
        "Data Processing", "Pandas", "Visualization", "File I/O", "Reporting"
    ])
    
    with analyst_tabs[0]:  # Data Processing
        st.subheader("🔄 Data Processing Fundamentals")
        
        show_code_example(
            "Working with CSV and JSON",
            "Read, process, and write common data formats",
            """import csv
import json
from pathlib import Path

# Reading CSV
with open('data.csv', 'r', encoding='utf-8') as file:
    # As list of lists
    reader = csv.reader(file)
    headers = next(reader)  # Skip header row
    data = list(reader)
    
    # As list of dictionaries
    file.seek(0)  # Reset file pointer
    dict_reader = csv.DictReader(file)
    records = list(dict_reader)

# Writing CSV
data = [
    ['Name', 'Age', 'City'],
    ['Alice', 30, 'New York'],
    ['Bob', 25, 'San Francisco']
]

with open('output.csv', 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerows(data)

# CSV with custom delimiter
with open('output.tsv', 'w', newline='') as file:
    writer = csv.writer(file, delimiter='\\t')
    writer.writerows(data)

# Working with JSON
# Read JSON
with open('data.json', 'r') as file:
    data = json.load(file)

# Write JSON
output_data = {
    'name': 'Analysis Results',
    'date': '2024-01-15',
    'values': [1, 2, 3, 4, 5]
}

with open('output.json', 'w') as file:
    json.dump(output_data, file, indent=2)

# JSON string conversion
json_string = json.dumps(output_data)
parsed = json.loads(json_string)

# Working with nested data
def flatten_json(nested_json, parent_key='', sep='_'):
    \"\"\"Flatten nested JSON structure.\"\"\"
    items = []
    for k, v in nested_json.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, dict):
            items.extend(flatten_json(v, new_key, sep=sep).items())
        else:
            items.append((new_key, v))
    return dict(items)

# Example usage
nested = {
    'user': {
        'name': 'Alice',
        'address': {
            'city': 'NYC',
            'zip': '10001'
        }
    }
}
flat = flatten_json(nested)
# {'user_name': 'Alice', 'user_address_city': 'NYC', 'user_address_zip': '10001'}""",
            matches_search("CSV JSON file read write data", search_query)
        )
        
    with analyst_tabs[1]:  # Pandas
        st.subheader("🐼 Pandas Data Analysis")
        
        col1, col2 = st.columns(2)
        
        with col1:
            show_code_example(
                "Pandas Basics",
                "Essential DataFrame operations",
                """import pandas as pd
import numpy as np

# Create DataFrame
df = pd.DataFrame({
    'date': pd.date_range('2024-01-01', periods=100),
    'product': np.random.choice(['A', 'B', 'C'], 100),
    'sales': np.random.randint(10, 100, 100),
    'price': np.random.uniform(10, 50, 100).round(2)
})

# Basic info
print(df.info())        # Data types and memory
print(df.describe())    # Statistical summary
print(df.head(10))     # First 10 rows
print(df.shape)        # (rows, columns)

# Column operations
df['revenue'] = df['sales'] * df['price']
df['month'] = df['date'].dt.month
df['weekday'] = df['date'].dt.day_name()

# Filtering
high_sales = df[df['sales'] > 50]
product_a = df[df['product'] == 'A']
complex_filter = df[(df['sales'] > 30) & (df['product'].isin(['A', 'B']))]

# Sorting
df_sorted = df.sort_values('revenue', ascending=False)
df_multi_sort = df.sort_values(['product', 'revenue'], ascending=[True, False])

# Grouping and aggregation
summary = df.groupby('product').agg({
    'sales': ['sum', 'mean', 'count'],
    'revenue': ['sum', 'mean'],
    'price': ['min', 'max', 'mean']
})

# Pivot tables
pivot = df.pivot_table(
    values='revenue',
    index='month',
    columns='product',
    aggfunc='sum',
    fill_value=0
)

# Rolling calculations
df['sales_ma7'] = df['sales'].rolling(window=7).mean()
df['sales_cumsum'] = df['sales'].cumsum()

# Handling missing data
df_filled = df.fillna(0)
df_dropped = df.dropna()
df_interpolated = df.interpolate()""",
                matches_search("pandas DataFrame groupby pivot filter", search_query)
            )
            
        with col2:
            show_code_example(
                "Advanced Pandas",
                "Complex data transformations",
                """# Merging DataFrames
customers = pd.DataFrame({
    'customer_id': [1, 2, 3, 4],
    'name': ['Alice', 'Bob', 'Charlie', 'David'],
    'segment': ['Premium', 'Regular', 'Premium', 'Regular']
})

orders = pd.DataFrame({
    'order_id': [101, 102, 103, 104],
    'customer_id': [1, 2, 1, 3],
    'amount': [100, 200, 150, 300]
})

# Different join types
merged = pd.merge(customers, orders, on='customer_id', how='left')
inner = pd.merge(customers, orders, on='customer_id', how='inner')

# Apply functions
df['category'] = df['sales'].apply(
    lambda x: 'High' if x > 70 else 'Medium' if x > 40 else 'Low'
)

# Apply to multiple columns
df[['sales_rank', 'price_rank']] = df[['sales', 'price']].apply(
    lambda x: x.rank(pct=True)
)

# Window functions
df['sales_rank_by_product'] = df.groupby('product')['sales'].rank()
df['sales_pct_of_product'] = df.groupby('product')['sales'].transform(
    lambda x: x / x.sum()
)

# Time series resampling
daily_sales = df.set_index('date').resample('D')['revenue'].sum()
weekly_sales = df.set_index('date').resample('W')['revenue'].agg(['sum', 'mean', 'count'])

# String operations
df['product_lower'] = df['product'].str.lower()
df['product_code'] = df['product'].str.extract(r'([A-Z])')

# Categorical data
df['product_cat'] = pd.Categorical(df['product'], categories=['A', 'B', 'C'], ordered=True)

# Multi-index operations
multi_df = df.set_index(['product', 'month'])
product_a_data = multi_df.loc['A']
jan_data = multi_df.xs(1, level='month')

# Export results
df.to_csv('analysis_results.csv', index=False)
df.to_excel('analysis_results.xlsx', sheet_name='Sales', index=False)
df.to_json('analysis_results.json', orient='records')""",
                matches_search("pandas merge apply transform resample", search_query)
            )
            
    with analyst_tabs[2]:  # Visualization
        st.subheader("📈 Data Visualization")
        
        show_code_example(
            "Matplotlib & Seaborn Basics",
            "Creating informative visualizations",
            """import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np

# Set style
plt.style.use('seaborn-v0_8-darkgrid')
sns.set_palette("husl")

# Sample data
df = pd.DataFrame({
    'month': pd.date_range('2024-01', periods=12, freq='M'),
    'sales': np.random.randint(100, 500, 12),
    'costs': np.random.randint(50, 300, 12),
    'category': np.random.choice(['A', 'B', 'C'], 12)
})

# Basic line plot
fig, ax = plt.subplots(figsize=(10, 6))
ax.plot(df['month'], df['sales'], marker='o', label='Sales')
ax.plot(df['month'], df['costs'], marker='s', label='Costs')
ax.set_xlabel('Month')
ax.set_ylabel('Amount ($)')
ax.set_title('Monthly Sales vs Costs')
ax.legend()
ax.grid(True, alpha=0.3)
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# Bar plot with error bars
fig, ax = plt.subplots(figsize=(10, 6))
categories = df.groupby('category')['sales'].agg(['mean', 'std'])
ax.bar(categories.index, categories['mean'], yerr=categories['std'], capsize=5)
ax.set_xlabel('Category')
ax.set_ylabel('Average Sales')
ax.set_title('Sales by Category')
plt.show()

# Seaborn plots
# Distribution plot
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
sns.histplot(data=df, x='sales', kde=True, ax=ax1)
sns.boxplot(data=df, x='category', y='sales', ax=ax2)
plt.tight_layout()
plt.show()

# Correlation heatmap
numeric_df = df.select_dtypes(include=[np.number])
corr = numeric_df.corr()
fig, ax = plt.subplots(figsize=(8, 6))
sns.heatmap(corr, annot=True, cmap='coolwarm', center=0, ax=ax)
ax.set_title('Correlation Matrix')
plt.show()

# Subplots
fig, axes = plt.subplots(2, 2, figsize=(12, 10))
fig.suptitle('Sales Analysis Dashboard', fontsize=16)

# Plot 1: Time series
axes[0, 0].plot(df['month'], df['sales'])
axes[0, 0].set_title('Sales Over Time')
axes[0, 0].set_xlabel('Month')
axes[0, 0].set_ylabel('Sales')

# Plot 2: Scatter
axes[0, 1].scatter(df['costs'], df['sales'], c=df.index, cmap='viridis')
axes[0, 1].set_title('Sales vs Costs')
axes[0, 1].set_xlabel('Costs')
axes[0, 1].set_ylabel('Sales')

# Plot 3: Pie chart
category_sales = df.groupby('category')['sales'].sum()
axes[1, 0].pie(category_sales, labels=category_sales.index, autopct='%1.1f%%')
axes[1, 0].set_title('Sales by Category')

# Plot 4: Bar chart
monthly_profit = df['sales'] - df['costs']
axes[1, 1].bar(range(len(monthly_profit)), monthly_profit, 
               color=['green' if x > 0 else 'red' for x in monthly_profit])
axes[1, 1].set_title('Monthly Profit/Loss')
axes[1, 1].set_xlabel('Month')
axes[1, 1].set_ylabel('Profit')

plt.tight_layout()
plt.savefig('dashboard.png', dpi=300, bbox_inches='tight')
plt.show()""",
            matches_search("matplotlib seaborn plot visualization chart", search_query)
        )

# Support Tab
with role_tab3:
    st.header("🛠️ Support Toolkit")
    
    support_tabs = st.tabs([
        "Debugging", "Error Handling", "System Info", "Troubleshooting"
    ])
    
    with support_tabs[0]:  # Debugging
        st.subheader("🐛 Debugging Techniques")
        
        show_code_example(
            "Debugging Tools & Methods",
            "Find and fix issues in Python code",
            """# Print debugging with f-strings
def calculate_discount(price, discount_percent):
    print(f"Debug: price={price}, discount_percent={discount_percent}")
    discount = price * (discount_percent / 100)
    print(f"Debug: calculated discount={discount}")
    final_price = price - discount
    print(f"Debug: final_price={final_price}")
    return final_price

# Using Python debugger (pdb)
import pdb

def problematic_function(data):
    result = []
    for item in data:
        # Set breakpoint
        pdb.set_trace()  # or use breakpoint() in Python 3.7+
        processed = item * 2
        result.append(processed)
    return result

# Logging instead of print
import logging

# Configure logging
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('debug.log'),
        logging.StreamHandler()
    ]
)

logger = logging.getLogger(__name__)

def process_data(data):
    logger.info(f"Processing {len(data)} items")
    try:
        for i, item in enumerate(data):
            logger.debug(f"Processing item {i}: {item}")
            # Process item
            if item < 0:
                logger.warning(f"Negative value found: {item}")
        logger.info("Processing complete")
    except Exception as e:
        logger.error(f"Error processing data: {e}", exc_info=True)
        raise

# Assertions for debugging
def divide(a, b):
    assert b != 0, f"Division by zero: {a}/{b}"
    assert isinstance(a, (int, float)), f"Invalid type for a: {type(a)}"
    assert isinstance(b, (int, float)), f"Invalid type for b: {type(b)}"
    return a / b

# Debugging decorators
def debug_args(func):
    \"\"\"Print function arguments for debugging.\"\"\"
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        print(f"  args: {args}")
        print(f"  kwargs: {kwargs}")
        result = func(*args, **kwargs)
        print(f"  returned: {result}")
        return result
    return wrapper

@debug_args
def add(a, b):
    return a + b

# Timing code execution
import time
from contextlib import contextmanager

@contextmanager
def timer(name):
    start = time.time()
    print(f"[{name}] Starting...")
    try:
        yield
    finally:
        end = time.time()
        print(f"[{name}] Finished in {end - start:.4f} seconds")

# Usage
with timer("Data processing"):
    # Long running operation
    time.sleep(1)""",
            matches_search("debug pdb logging assert timer", search_query)
        )
        
    with support_tabs[1]:  # Error Handling
        st.subheader("⚠️ Error Handling")
        
        show_code_example(
            "Exception Handling Patterns",
            "Properly handle and log errors",
            """# Basic exception handling
def safe_divide(a, b):
    try:
        result = a / b
        return result
    except ZeroDivisionError:
        print(f"Error: Cannot divide {a} by zero")
        return None
    except TypeError as e:
        print(f"Type error: {e}")
        return None
    finally:
        print("Division operation completed")

# Multiple exception types
def process_file(filename):
    try:
        with open(filename, 'r') as f:
            data = f.read()
            return json.loads(data)
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        return None
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in file - {e}")
        return None
    except PermissionError:
        print(f"Error: No permission to read '{filename}'")
        return None
    except Exception as e:
        # Catch-all for unexpected errors
        print(f"Unexpected error: {type(e).__name__}: {e}")
        return None

# Custom exceptions
class ValidationError(Exception):
    \"\"\"Raised when validation fails.\"\"\"
    pass

class APIError(Exception):
    \"\"\"Raised when API calls fail.\"\"\"
    def __init__(self, status_code, message):
        self.status_code = status_code
        self.message = message
        super().__init__(f"API Error {status_code}: {message}")

# Using custom exceptions
def validate_email(email):
    if '@' not in email:
        raise ValidationError(f"Invalid email format: {email}")
    if not email.endswith(('.com', '.org', '.net')):
        raise ValidationError(f"Unsupported email domain: {email}")
    return True

# Context managers for resource cleanup
class DatabaseConnection:
    def __enter__(self):
        print("Opening database connection")
        self.conn = self._connect()
        return self.conn
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Closing database connection")
        if exc_type:
            print(f"Error occurred: {exc_val}")
            self.conn.rollback()
        else:
            self.conn.commit()
        self.conn.close()
        return False  # Don't suppress exceptions

# Retry decorator with exponential backoff
import time
from functools import wraps

def retry(max_attempts=3, delay=1, backoff=2, exceptions=(Exception,)):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            attempt = 0
            current_delay = delay
            
            while attempt < max_attempts:
                try:
                    return func(*args, **kwargs)
                except exceptions as e:
                    attempt += 1
                    if attempt >= max_attempts:
                        raise
                    print(f"Attempt {attempt} failed: {e}")
                    print(f"Retrying in {current_delay} seconds...")
                    time.sleep(current_delay)
                    current_delay *= backoff
            
        return wrapper
    return decorator

@retry(max_attempts=3, exceptions=(ConnectionError, TimeoutError))
def unreliable_api_call():
    # Simulated API call
    import random
    if random.random() < 0.7:
        raise ConnectionError("API unavailable")
    return "Success!"

# Error logging
import logging
import traceback

logger = logging.getLogger(__name__)

def safe_operation():
    try:
        # Risky operation
        result = risky_function()
        return result
    except Exception as e:
        # Log full traceback
        logger.error(f"Operation failed: {e}")
        logger.error(traceback.format_exc())
        # Re-raise or return safe value
        return None""",
            matches_search("exception error try except finally custom", search_query)
        )
        
    with support_tabs[2]:  # System Info
        st.subheader("💻 System Information")
        
        show_code_example(
            "Getting System Information",
            "Check Python environment and system details",
            """import sys
import os
import platform
import pkg_resources
import psutil  # pip install psutil
import socket
from datetime import datetime

# Python information
print(f"Python version: {sys.version}")
print(f"Python executable: {sys.executable}")
print(f"Python path: {sys.path}")
print(f"Platform: {sys.platform}")

# Detailed platform info
print(f"\\nSystem Information:")
print(f"OS: {platform.system()} {platform.release()}")
print(f"Machine: {platform.machine()}")
print(f"Processor: {platform.processor()}")
print(f"Python implementation: {platform.python_implementation()}")
print(f"Python version: {platform.python_version()}")

# Environment variables
print(f"\\nEnvironment Variables:")
print(f"HOME: {os.environ.get('HOME', 'Not set')}")
print(f"PATH: {os.environ.get('PATH', 'Not set')}")
print(f"PYTHONPATH: {os.environ.get('PYTHONPATH', 'Not set')}")
print(f"Virtual env: {os.environ.get('VIRTUAL_ENV', 'Not in venv')}")

# Installed packages
print(f"\\nInstalled Packages:")
installed_packages = [d for d in pkg_resources.working_set]
for package in sorted(installed_packages, key=lambda x: x.key):
    print(f"  {package.key}: {package.version}")

# System resources (requires psutil)
print(f"\\nSystem Resources:")
print(f"CPU count: {psutil.cpu_count()} (logical), {psutil.cpu_count(logical=False)} (physical)")
print(f"CPU usage: {psutil.cpu_percent(interval=1)}%")
print(f"Memory: {psutil.virtual_memory().percent}% used")
print(f"Disk usage: {psutil.disk_usage('/').percent}% used")

# Network information
hostname = socket.gethostname()
ip_address = socket.gethostbyname(hostname)
print(f"\\nNetwork:")
print(f"Hostname: {hostname}")
print(f"IP Address: {ip_address}")

# Current process info
process = psutil.Process()
print(f"\\nCurrent Process:")
print(f"PID: {process.pid}")
print(f"Memory usage: {process.memory_info().rss / 1024 / 1024:.2f} MB")
print(f"CPU usage: {process.cpu_percent()}%")
print(f"Started: {datetime.fromtimestamp(process.create_time())}")

# Check module availability
def check_module(module_name):
    try:
        __import__(module_name)
        return True
    except ImportError:
        return False

print(f"\\nModule Availability:")
modules_to_check = ['numpy', 'pandas', 'matplotlib', 'requests', 'django', 'flask']
for module in modules_to_check:
    status = "✓" if check_module(module) else "✗"
    print(f"  {status} {module}")

# Python path and import debugging
def find_module_location(module_name):
    try:
        module = __import__(module_name)
        if hasattr(module, '__file__'):
            return module.__file__
        else:
            return "Built-in module"
    except ImportError:
        return "Module not found"

print(f"\\nModule Locations:")
for module in ['os', 'json', 'requests', 'numpy']:
    print(f"  {module}: {find_module_location(module)}")""",
            matches_search("system info platform environment psutil", search_query)
        )

# Reference Tab
with role_tab4:
    st.header("📚 Quick Reference")
    
    ref_tabs = st.tabs(["Syntax", "Built-ins", "String Formatting", "File Paths", "Type Hints"])
    
    with ref_tabs[0]:  # Syntax
        col1, col2 = st.columns(2)
        
        with col1:
            st.subheader("🔤 Python Syntax")
            st.code("""
# Variable assignment
x = 5
y, z = 10, 20
a = b = c = 0

# Augmented assignment
x += 1   # x = x + 1
x -= 1   # x = x - 1
x *= 2   # x = x * 2
x /= 2   # x = x / 2
x //= 2  # x = x // 2 (floor division)
x %= 2   # x = x % 2
x **= 2  # x = x ** 2

# Comparison operators
==  # Equal
!=  # Not equal
<   # Less than
>   # Greater than
<=  # Less than or equal
>=  # Greater than or equal

# Logical operators
and  # Both must be True
or   # At least one must be True
not  # Negation

# Identity operators
is      # Same object
is not  # Different objects

# Membership operators
in      # Contained in
not in  # Not contained in

# Conditional (ternary)
result = value_if_true if condition else value_if_false

# Walrus operator (Python 3.8+)
if (n := len(data)) > 10:
    print(f"List is too long ({n} elements)")
""", language="python")
            
        with col2:
            st.subheader("🔁 Control Flow")
            st.code("""
# If statements
if condition:
    # code
elif another_condition:
    # code
else:
    # code

# For loops
for item in iterable:
    # code
    
for i in range(10):
    # 0 to 9
    
for i in range(5, 10):
    # 5 to 9
    
for i in range(0, 10, 2):
    # 0, 2, 4, 6, 8

# While loops
while condition:
    # code
    if break_condition:
        break
    if skip_condition:
        continue

# Match statement (Python 3.10+)
match value:
    case 1:
        print("One")
    case 2 | 3:
        print("Two or three")
    case [x, y]:
        print(f"List with {x} and {y}")
    case {"name": name}:
        print(f"Dict with name: {name}")
    case _:
        print("Default case")

# Exception handling
try:
    risky_code()
except SpecificError as e:
    handle_error(e)
except (Error1, Error2):
    handle_multiple()
else:
    # Runs if no exception
    success_code()
finally:
    # Always runs
    cleanup()
""", language="python")
            
    with ref_tabs[1]:  # Built-ins
        col1, col2, col3 = st.columns(3)
        
        with col1:
            st.subheader("📦 Built-in Functions")
            st.code("""
# Type conversion
int("123")      # 123
float("3.14")   # 3.14
str(123)        # "123"
bool(1)         # True
list("abc")     # ['a','b','c']
tuple([1,2,3])  # (1, 2, 3)
set([1,1,2,3])  # {1, 2, 3}
dict([('a',1)]) # {'a': 1}

# Math functions
abs(-5)         # 5
round(3.7)      # 4
round(3.14, 1)  # 3.1
min(1, 2, 3)    # 1
max(1, 2, 3)    # 3
sum([1, 2, 3])  # 6
pow(2, 3)       # 8

# Sequence functions
len([1, 2, 3])  # 3
sorted([3,1,2]) # [1, 2, 3]
reversed([1,2]) # iterator
enumerate(['a','b'])
zip([1,2], ['a','b'])

# Type checking
type(5)         # <class 'int'>
isinstance(5, int)  # True
callable(print) # True
""", language="python")
            
        with col2:
            st.subheader("🔧 Useful Built-ins")
            st.code("""
# Input/Output
print("Hello", end="")
input("Enter: ")

# Object functions
id(object)      # Memory address
hash(object)    # Hash value
dir(object)     # Attributes
vars(object)    # __dict__
help(object)    # Documentation

# Functional
map(func, iterable)
filter(func, iterable)
all([True, True])   # True
any([False, True])  # True

# Iteration
next(iterator)
iter(sequence)
range(start, stop, step)

# Evaluation
eval("2 + 2")   # 4 (careful!)
exec("x = 5")   # Execute code

# Object creation
object()
property(fget, fset, fdel)
classmethod(func)
staticmethod(func)

# Memory
del variable
globals()
locals()
""", language="python")
            
        with col3:
            st.subheader("🎯 One-liners")
            st.code("""
# List operations
flat = [x for sub in nested for x in sub]
unique = list(set(items))
chunks = [lst[i:i+n] for i in range(0, len(lst), n)]

# Dict operations
inverted = {v: k for k, v in d.items()}
merged = {**dict1, **dict2}
filtered = {k: v for k, v in d.items() if v > 0}

# String operations
words = text.split()
joined = ' '.join(words)
reversed_str = text[::-1]

# File operations
text = open('file.txt').read()
lines = [line.strip() for line in open('file.txt')]

# Conditional assignment
result = x if condition else y
status = "pass" if score >= 60 else "fail"

# Quick calculations
average = sum(numbers) / len(numbers)
factorial = 1 if n == 0 else n * factorial(n-1)

# Boolean checks
has_items = bool(my_list)
all_positive = all(x > 0 for x in numbers)
any_match = any(item == target for item in items)
""", language="python")
            
    with ref_tabs[2]:  # String Formatting
        st.subheader("📝 String Formatting")
        
        col1, col2 = st.columns(2)
        
        with col1:
            st.code("""
# f-strings (Python 3.6+) - RECOMMENDED
name = "Alice"
age = 30
pi = 3.14159

f"Hello, {name}!"
f"{name} is {age} years old"
f"Pi: {pi:.2f}"  # 3.14
f"Binary: {42:b}"  # 101010
f"Hex: {255:x}"    # ff
f"Percent: {0.25:.0%}"  # 25%
f"Padded: {5:03d}"  # 005
f"Center: {name:^10}"  # '  Alice   '
f"Debug: {name=}, {age=}"  # name='Alice', age=30

# Multiline f-strings
message = f"""
Name: {name}
Age: {age}
Next year: {age + 1}
"""

# Format with expressions
f"{2 + 2}"  # "4"
f"{[1,2,3]}"  # "[1, 2, 3]"
f"{name.upper()}"  # "ALICE"

# Alignment
f"{name:<10}"  # 'Alice     ' (left)
f"{name:>10}"  # '     Alice' (right)
f"{name:^10}"  # '  Alice   ' (center)
f"{name:*^10}" # '**Alice***' (fill)
""", language="python")
            
        with col2:
            st.code("""
# .format() method
"Hello, {}!".format(name)
"{0} is {1} years old".format(name, age)
"{name} is {age}".format(name=name, age=age)

# Format specifications
"{:.2f}".format(pi)      # 3.14
"{:,}".format(1000000)   # 1,000,000
"{:.2e}".format(1000000) # 1.00e+06
"{:10.2f}".format(pi)    # '      3.14'

# Old style % formatting
"Hello, %s!" % name
"%s is %d years old" % (name, age)
"Pi: %.2f" % pi

# Template strings
from string import Template
t = Template("$name is $age years old")
t.substitute(name=name, age=age)

# Number formatting
format(1234.5, ',.2f')  # '1,234.50'
format(42, 'b')         # '101010'
format(255, 'x')        # 'ff'

# Datetime formatting
from datetime import datetime
now = datetime.now()
now.strftime("%Y-%m-%d %H:%M:%S")
now.strftime("%B %d, %Y")  # January 15, 2024
""", language="python")
            
    with ref_tabs[3]:  # File Paths
        st.subheader("📁 File Path Handling")
        st.code("""
from pathlib import Path
import os

# Pathlib (recommended for new code)
# Create paths
current_dir = Path.cwd()
home_dir = Path.home()
file_path = Path("folder/subfolder/file.txt")

# Join paths
data_dir = Path("data")
file_path = data_dir / "input" / "file.csv"

# Path properties
file_path.name          # 'file.csv'
file_path.stem         # 'file'
file_path.suffix       # '.csv'
file_path.parent       # Path('data/input')
file_path.parts        # ('data', 'input', 'file.csv')

# Path operations
file_path.exists()     # Check if exists
file_path.is_file()    # Is it a file?
file_path.is_dir()     # Is it a directory?
file_path.mkdir(parents=True, exist_ok=True)  # Create directory

# Iterate directory
for file in Path("data").iterdir():
    if file.is_file():
        print(file)

# Glob patterns
for csv_file in Path("data").glob("*.csv"):
    print(csv_file)

for py_file in Path(".").rglob("*.py"):  # Recursive
    print(py_file)

# Read/write with pathlib
text = Path("file.txt").read_text()
Path("file.txt").write_text("content")

# os.path (older but still common)
os.path.join("folder", "file.txt")
os.path.dirname("/path/to/file.txt")  # '/path/to'
os.path.basename("/path/to/file.txt") # 'file.txt'
os.path.splitext("file.txt")  # ('file', '.txt')
os.path.exists("file.txt")
os.path.isfile("file.txt")
os.path.isdir("folder")
os.path.abspath("file.txt")  # Absolute path
os.path.expanduser("~/file.txt")  # Expand ~
""", language="python")
        
    with ref_tabs[4]:  # Type Hints
        st.subheader("🏷️ Type Hints")
        st.code("""
from typing import List, Dict, Optional, Union, Tuple, Any, Callable
from typing import TypeVar, Generic, Protocol, Literal, TypedDict
from collections.abc import Sequence, Mapping, Iterable

# Basic type hints
name: str = "Alice"
age: int = 30
height: float = 5.6
is_student: bool = False

# Collections
numbers: List[int] = [1, 2, 3]
names: list[str] = ["Alice", "Bob"]  # Python 3.9+
scores: Dict[str, int] = {"Alice": 95, "Bob": 87}
coordinates: Tuple[float, float] = (10.5, 20.3)
unique_ids: set[int] = {1, 2, 3}

# Optional and Union
middle_name: Optional[str] = None  # str or None
middle_name: str | None = None     # Python 3.10+
value: Union[int, float] = 3.14
value: int | float = 3.14          # Python 3.10+

# Function type hints
def greet(name: str, formal: bool = False) -> str:
    return f"Hello, {name}!"

def process(data: List[int]) -> Dict[str, Any]:
    return {"sum": sum(data), "count": len(data)}

# Callable
operation: Callable[[int, int], int] = lambda x, y: x + y

# Type variables
T = TypeVar('T')
def first_item(items: List[T]) -> T:
    return items[0]

# TypedDict
class Person(TypedDict):
    name: str
    age: int
    email: Optional[str]

# Protocol (structural subtyping)
class Drawable(Protocol):
    def draw(self) -> None: ...

# Literal types
Mode = Literal["read", "write", "append"]
def open_file(name: str, mode: Mode) -> None:
    pass

# Generic class
class Stack(Generic[T]):
    def __init__(self) -> None:
        self._items: List[T] = []
    
    def push(self, item: T) -> None:
        self._items.append(item)
    
    def pop(self) -> T:
        return self._items.pop()
""", language="python")

# Tips and Tricks
st.markdown("---")
st.header("💡 Python Tips & Tricks")

tips_col1, tips_col2 = st.columns(2)

with tips_col1:
    st.markdown("""
    **Performance Tips**
    - Use `list.append()` instead of `list + [item]`
    - Use `set` for membership testing (`in` operator)
    - Use generators for large datasets
    - Cache expensive computations with `@functools.lru_cache`
    - Use `collections.deque` for queues
    - Profile with `cProfile` before optimizing
    
    **Common Pitfalls**
    - Mutable default arguments: Use `None` and check
    - Modifying list while iterating: Use copy or list comp
    - Late binding in closures: Use default parameters
    - Integer division changed in Python 3: Use `//`
    - Don't use `eval()` with user input
    - Remember `is` vs `==` difference
    """)

with tips_col2:
    st.markdown("""
    **Best Practices**
    - Follow PEP 8 style guide
    - Use meaningful variable names
    - Write docstrings for functions/classes
    - Use type hints for clarity
    - Handle exceptions specifically
    - Use context managers for resources
    - Test edge cases
    - Keep functions small and focused
    
    **Useful Modules**
    - `itertools` - Efficient iteration
    - `collections` - Specialized containers
    - `functools` - Functional programming
    - `pathlib` - Modern path handling
    - `dataclasses` - Boilerplate reduction
    - `contextlib` - Context manager utilities
    """)

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use the search box to quickly find Python syntax, patterns, and examples!")