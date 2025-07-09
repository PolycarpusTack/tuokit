"""
TuoKit - XML Cheat Sheet
Comprehensive XML reference for developers, analysts, and support staff
"""

import streamlit as st
from utils import apply_modern_theme
from utils.sidebar_nav import render_sidebar_navigation

# Page configuration
st.set_page_config(
    page_title="XML Cheat Sheet - TuoKit",
    page_icon="📄",
    layout="wide"
)

# Apply theme and navigation
apply_modern_theme()
render_sidebar_navigation(current_page="xml_cheatsheet")

# Header
st.markdown("""
<div style="text-align: center; padding: 2rem 0;">
    <h1 class="gradient-text" style="font-size: 3rem; margin-bottom: 1rem;">
        📄 XML Cheat Sheet
    </h1>
    <p style="font-size: 1.2rem; color: #9e9e9e;">
        XML syntax, XPath, XSLT, and best practices for all roles
    </p>
</div>
""", unsafe_allow_html=True)

# Search functionality
search_query = st.text_input("🔍 Search XML topics...", placeholder="Try: XPath, namespace, CDATA, validation...")

# Create role-based tabs
role_tab1, role_tab2, role_tab3, role_tab4 = st.tabs([
    "👨‍💻 Developer", 
    "📊 Analyst", 
    "🛠️ Support", 
    "📚 Reference"
])

# Helper function
def show_xml_example(title, description, code, language="xml", show=True):
    if show:
        st.markdown(f"### {title}")
        st.caption(description)
        st.code(code, language=language)
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
        "Structure", "Namespaces", "XPath", "XSLT", "Validation", "Parsing"
    ])
    
    with dev_tabs[0]:  # Structure
        st.subheader("📋 XML Structure & Syntax")
        
        show_xml_example(
            "Complete XML Document Structure",
            "All components of a well-formed XML document",
            """<?xml version="1.0" encoding="UTF-8"?>
<!-- This is a comment -->
<!DOCTYPE root [
    <!ENTITY company "TechCorp">
    <!ELEMENT root (header, body)>
]>
<root xmlns="http://example.com/default" 
      xmlns:app="http://example.com/app"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://example.com/default schema.xsd">
    
    <!-- Processing instruction -->
    <?xml-stylesheet type="text/xsl" href="style.xsl"?>
    
    <header version="1.0" timestamp="2024-01-15T10:30:00Z">
        <title>Document Title</title>
        <author id="123">John Doe</author>
    </header>
    
    <body>
        <!-- Using entity reference -->
        <company>&company;</company>
        
        <!-- CDATA section for literal text -->
        <code><![CDATA[
            if (x < 10 && y > 5) {
                console.log("Condition met");
            }
        ]]></code>
        
        <!-- Namespaced element -->
        <app:config>
            <app:setting name="debug">true</app:setting>
        </app:config>
        
        <!-- Self-closing element -->
        <status complete="true" />
    </body>
</root>""",
            matches_search("XML structure document namespace CDATA", search_query)
        )
        
        show_xml_example(
            "Special Characters & Entities",
            "How to handle special characters in XML",
            """<!-- Predefined XML entities -->
<text>
    <!-- Less than: < -->
    <comparison>5 &lt; 10</comparison>
    
    <!-- Greater than: > -->
    <comparison>10 &gt; 5</comparison>
    
    <!-- Ampersand: & -->
    <company>Smith &amp; Sons</company>
    
    <!-- Apostrophe: ' -->
    <message>It&apos;s working</message>
    
    <!-- Quote: " -->
    <quote>He said &quot;Hello&quot;</quote>
</text>

<!-- Custom entity definition -->
<!DOCTYPE root [
    <!ENTITY copyright "© 2024 TechCorp. All rights reserved.">
    <!ENTITY reg "®">
    <!ENTITY trade "™">
]>
<root>
    <footer>&copyright;</footer>
    <product>SuperApp&trade;</product>
    <brand>MegaCorp&reg;</brand>
</root>

<!-- Unicode characters -->
<international>
    <currency>€100</currency>  <!-- Euro sign -->
    <emoji>😀</emoji>         <!-- Emoji -->
    <chinese>你好</chinese>    <!-- Chinese characters -->
</international>""",
            matches_search("special characters entities escape", search_query)
        )
        
    with dev_tabs[1]:  # Namespaces
        st.subheader("🏷️ XML Namespaces")
        
        show_xml_example(
            "Namespace Declaration & Usage",
            "Avoiding element name conflicts with namespaces",
            """<!-- Default namespace -->
<catalog xmlns="http://example.com/catalog">
    <product>
        <name>Laptop</name>
        <price>999.99</price>
    </product>
</catalog>

<!-- Multiple namespaces with prefixes -->
<store:catalog xmlns:store="http://example.com/store"
               xmlns:inv="http://example.com/inventory"
               xmlns:fin="http://example.com/finance">
    <store:product id="123">
        <store:name>Laptop</store:name>
        <inv:quantity>50</inv:quantity>
        <inv:location>Warehouse A</inv:location>
        <fin:price currency="USD">999.99</fin:price>
        <fin:cost>750.00</fin:cost>
    </store:product>
</store:catalog>

<!-- Inline namespace declaration -->
<root>
    <normalElement>Content</normalElement>
    <special:element xmlns:special="http://example.com/special">
        Special content
    </special:element>
</root>

<!-- Namespace with schema validation -->
<order xmlns="http://example.com/orders"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://example.com/orders orders.xsd">
    <orderId>12345</orderId>
    <orderDate>2024-01-15</orderDate>
</order>""",
            matches_search("namespace xmlns prefix schema", search_query)
        )
        
    with dev_tabs[2]:  # XPath
        st.subheader("🔍 XPath Expressions")
        
        col1, col2 = st.columns(2)
        
        with col1:
            show_xml_example(
                "XPath Axes & Navigation",
                "Navigate XML document structure",
                """<!-- Sample XML for XPath examples -->
<library>
    <section name="Fiction">
        <book id="1" year="2020">
            <title>The Great Adventure</title>
            <author>Jane Smith</author>
            <price>29.99</price>
        </book>
        <book id="2" year="2021">
            <title>Mystery Island</title>
            <author>John Doe</author>
            <price>24.99</price>
        </book>
    </section>
    <section name="Technical">
        <book id="3" year="2022">
            <title>XML Mastery</title>
            <author>Tech Expert</author>
            <price>49.99</price>
        </book>
    </section>
</library>

<!-- XPath Examples -->
// Select all book elements anywhere
//book

// Select books in Fiction section
//section[@name='Fiction']/book

// Select book titles over $25
//book[price > 25]/title

// Select the first book in each section
//section/book[1]

// Select all attributes
//@*

// Select by position
//book[position() = 2]
//book[last()]

// Text content
//book/title/text()

// Parent navigation
//author/..

// Following siblings
//book[@id='1']/following-sibling::book

// Ancestors
//price/ancestor::section""",
                matches_search("XPath axes navigation position", search_query)
            )
            
        with col2:
            show_xml_example(
                "XPath Functions",
                "Built-in XPath functions",
                """<!-- String functions -->
// Books with 'Great' in title
//book[contains(title, 'Great')]

// Case-insensitive search
//book[contains(translate(title, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'great')]

// String length
//book[string-length(title) > 10]

// Starts with
//book[starts-with(author, 'John')]

// Substring
//book[substring(title, 1, 3) = 'The']

<!-- Numeric functions -->
// Sum of all prices
sum(//book/price)

// Count books
count(//book)

// Average price
sum(//book/price) div count(//book)

// Round price
//book[round(price) = 30]

<!-- Boolean functions -->
// Books with no author
//book[not(author)]

// Books from 2021 or 2022
//book[@year = 2021 or @year = 2022]

<!-- Node functions -->
// Element name
//*/name()

// Local name (without namespace)
//*/local-name()

// Namespace URI
//*/namespace-uri()""",
                "xpath",
                matches_search("XPath functions string numeric", search_query)
            )
            
    with dev_tabs[3]:  # XSLT
        st.subheader("🔄 XSLT Transformations")
        
        show_xml_example(
            "Basic XSLT Template",
            "Transform XML to HTML",
            """<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    
    <!-- Output configuration -->
    <xsl:output method="html" indent="yes" encoding="UTF-8"/>
    
    <!-- Root template -->
    <xsl:template match="/">
        <html>
            <head>
                <title>Product Catalog</title>
                <style>
                    table { border-collapse: collapse; width: 100%; }
                    th, td { border: 1px solid #ddd; padding: 8px; }
                    th { background-color: #f2f2f2; }
                </style>
            </head>
            <body>
                <h1>Product Catalog</h1>
                <xsl:apply-templates select="catalog/product"/>
            </body>
        </html>
    </xsl:template>
    
    <!-- Product template -->
    <xsl:template match="product">
        <div class="product">
            <h2><xsl:value-of select="name"/></h2>
            <p>Price: $<xsl:value-of select="price"/></p>
            <xsl:if test="@featured = 'true'">
                <span class="featured">★ Featured Product</span>
            </xsl:if>
        </div>
    </xsl:template>
    
    <!-- Table transformation -->
    <xsl:template match="products">
        <table>
            <tr>
                <th>ID</th>
                <th>Name</th>
                <th>Price</th>
                <th>Stock</th>
            </tr>
            <xsl:for-each select="product">
                <xsl:sort select="price" data-type="number" order="ascending"/>
                <tr>
                    <td><xsl:value-of select="@id"/></td>
                    <td><xsl:value-of select="name"/></td>
                    <td>$<xsl:value-of select="format-number(price, '#,##0.00')"/></td>
                    <td>
                        <xsl:choose>
                            <xsl:when test="stock > 10">In Stock</xsl:when>
                            <xsl:when test="stock > 0">Low Stock</xsl:when>
                            <xsl:otherwise>Out of Stock</xsl:otherwise>
                        </xsl:choose>
                    </td>
                </tr>
            </xsl:for-each>
        </table>
    </xsl:template>
</xsl:stylesheet>""",
            "xsl",
            matches_search("XSLT transform template stylesheet", search_query)
        )

# Analyst Tab
with role_tab2:
    st.header("📊 Analyst Toolkit")
    
    analyst_tabs = st.tabs([
        "Data Extraction", "XML to CSV", "Reports", "Analysis"
    ])
    
    with analyst_tabs[0]:  # Data Extraction
        st.subheader("📥 Extracting Data from XML")
        
        show_xml_example(
            "XPath for Data Extraction",
            "Common patterns for extracting data",
            """<!-- Sample sales data XML -->
<sales>
    <region name="North">
        <quarter period="Q1-2024">
            <product category="Electronics">
                <name>Laptop</name>
                <units>150</units>
                <revenue>149850</revenue>
            </product>
            <product category="Electronics">
                <name>Phone</name>
                <units>320</units>
                <revenue>223680</revenue>
            </product>
        </quarter>
    </region>
    <region name="South">
        <quarter period="Q1-2024">
            <product category="Electronics">
                <name>Laptop</name>
                <units>120</units>
                <revenue>119880</revenue>
            </product>
        </quarter>
    </region>
</sales>

<!-- Data extraction queries -->
// Total revenue across all regions
sum(//revenue)

// Revenue by region
//region[@name='North']//revenue/sum(.)

// Product count by category
count(//product[@category='Electronics'])

// All unique product names
//product/name[not(. = preceding::name)]

// Quarterly comparisons
//quarter[@period='Q1-2024']//revenue/sum(.)

// Top performing products (requires XSLT/programming)
//product[revenue > 200000]/name""",
            matches_search("extract data XPath sum count", search_query)
        )
        
    with analyst_tabs[1]:  # XML to CSV
        st.subheader("📊 Converting XML to CSV")
        
        col1, col2 = st.columns(2)
        
        with col1:
            show_xml_example(
                "Python XML to CSV",
                "Convert XML data to CSV format",
                """import xml.etree.ElementTree as ET
import csv

# Parse XML file
tree = ET.parse('sales_data.xml')
root = tree.getroot()

# Open CSV file for writing
with open('sales_report.csv', 'w', newline='', encoding='utf-8') as csvfile:
    writer = csv.writer(csvfile)
    
    # Write headers
    writer.writerow(['Region', 'Quarter', 'Product', 'Category', 'Units', 'Revenue'])
    
    # Extract data
    for region in root.findall('.//region'):
        region_name = region.get('name')
        
        for quarter in region.findall('.//quarter'):
            quarter_period = quarter.get('period')
            
            for product in quarter.findall('.//product'):
                writer.writerow([
                    region_name,
                    quarter_period,
                    product.find('name').text,
                    product.get('category'),
                    product.find('units').text,
                    product.find('revenue').text
                ])

# Advanced: Handle missing data
for product in root.findall('.//product'):
    writer.writerow([
        product.find('name').text if product.find('name') is not None else '',
        product.find('units').text if product.find('units') is not None else '0',
        product.find('revenue').text if product.find('revenue') is not None else '0'
    ])""",
                "python",
                matches_search("XML CSV convert Python", search_query)
            )
            
        with col2:
            show_xml_example(
                "XSLT to CSV Transform",
                "Generate CSV using XSLT",
                """<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    
    <xsl:output method="text" encoding="UTF-8"/>
    
    <!-- CSV header -->
    <xsl:template match="/">
        <xsl:text>Region,Quarter,Product,Units,Revenue&#10;</xsl:text>
        <xsl:apply-templates select="//product"/>
    </xsl:template>
    
    <!-- Product rows -->
    <xsl:template match="product">
        <!-- Region -->
        <xsl:value-of select="ancestor::region/@name"/>
        <xsl:text>,</xsl:text>
        
        <!-- Quarter -->
        <xsl:value-of select="ancestor::quarter/@period"/>
        <xsl:text>,</xsl:text>
        
        <!-- Product name (escape commas) -->
        <xsl:call-template name="escape-csv">
            <xsl:with-param name="value" select="name"/>
        </xsl:call-template>
        <xsl:text>,</xsl:text>
        
        <!-- Units and Revenue -->
        <xsl:value-of select="units"/>
        <xsl:text>,</xsl:text>
        <xsl:value-of select="revenue"/>
        <xsl:text>&#10;</xsl:text>
    </xsl:template>
    
    <!-- Escape CSV values -->
    <xsl:template name="escape-csv">
        <xsl:param name="value"/>
        <xsl:choose>
            <xsl:when test="contains($value, ',') or contains($value, '&quot;') or contains($value, '&#10;')">
                <xsl:text>"</xsl:text>
                <xsl:value-of select="translate($value, '&quot;', '&quot;&quot;')"/>
                <xsl:text>"</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$value"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
</xsl:stylesheet>""",
                "xsl",
                matches_search("XSLT CSV transform", search_query)
            )
            
    with analyst_tabs[2]:  # Reports
        st.subheader("📈 XML Report Generation")
        
        show_xml_example(
            "Aggregating XML Data",
            "Summarize and analyze XML data",
            """<!-- Using XPath 2.0 / XQuery for aggregation -->
<!-- Group by category -->
for $category in distinct-values(//product/@category)
return 
    <category name="{$category}">
        <total-units>{sum(//product[@category=$category]/units)}</total-units>
        <total-revenue>{sum(//product[@category=$category]/revenue)}</total-revenue>
        <product-count>{count(//product[@category=$category])}</product-count>
    </category>

<!-- Python aggregation example -->
import xml.etree.ElementTree as ET
from collections import defaultdict

tree = ET.parse('sales_data.xml')
root = tree.getroot()

# Aggregate by region
region_totals = defaultdict(lambda: {'units': 0, 'revenue': 0})

for region in root.findall('.//region'):
    region_name = region.get('name')
    for product in region.findall('.//product'):
        region_totals[region_name]['units'] += int(product.find('units').text)
        region_totals[region_name]['revenue'] += float(product.find('revenue').text)

# Generate summary report
print("Regional Sales Summary")
print("-" * 40)
for region, totals in region_totals.items():
    print(f"{region}:")
    print(f"  Total Units: {totals['units']:,}")
    print(f"  Total Revenue: ${totals['revenue']:,.2f}")
    print(f"  Avg per Unit: ${totals['revenue']/totals['units']:.2f}")

# Time-based analysis
from datetime import datetime

quarters = {}
for quarter in root.findall('.//quarter'):
    period = quarter.get('period')
    revenue = sum(float(p.find('revenue').text) for p in quarter.findall('.//product'))
    quarters[period] = revenue

# Quarter-over-quarter growth
sorted_quarters = sorted(quarters.items())
for i in range(1, len(sorted_quarters)):
    current = sorted_quarters[i]
    previous = sorted_quarters[i-1]
    growth = ((current[1] - previous[1]) / previous[1]) * 100
    print(f"{current[0]} vs {previous[0]}: {growth:+.1f}%")""",
            "python",
            matches_search("aggregate summary report analysis", search_query)
        )

# Support Tab
with role_tab3:
    st.header("🛠️ Support Toolkit")
    
    support_tabs = st.tabs([
        "Validation", "Common Errors", "Debugging", "Tools"
    ])
    
    with support_tabs[0]:  # Validation
        st.subheader("✅ XML Validation")
        
        show_xml_example(
            "Command-Line Validation",
            "Validate XML files using command-line tools",
            """# Using xmllint (Linux/Mac)
# Check well-formedness
xmllint --noout file.xml

# Validate against DTD
xmllint --valid --noout file.xml

# Validate against XSD Schema
xmllint --schema schema.xsd file.xml --noout

# Format/pretty-print XML
xmllint --format ugly.xml > pretty.xml

# Using Python
import xml.etree.ElementTree as ET

def validate_xml(xml_file):
    try:
        tree = ET.parse(xml_file)
        print(f"✓ {xml_file} is well-formed")
        return True
    except ET.ParseError as e:
        print(f"✗ {xml_file} is not well-formed")
        print(f"  Error: {e}")
        return False

# Advanced validation with lxml
from lxml import etree

def validate_with_schema(xml_file, schema_file):
    # Parse schema
    with open(schema_file, 'r') as f:
        schema_doc = etree.parse(f)
        schema = etree.XMLSchema(schema_doc)
    
    # Parse and validate XML
    with open(xml_file, 'r') as f:
        xml_doc = etree.parse(f)
        
    if schema.validate(xml_doc):
        print("✓ Valid according to schema")
    else:
        print("✗ Schema validation failed:")
        print(schema.error_log)""",
            "bash",
            matches_search("validate xmllint schema XSD", search_query)
        )
        
    with support_tabs[1]:  # Common Errors
        st.subheader("❌ Common XML Errors")
        
        # Error reference table
        st.markdown("""
        ### Error Quick Reference
        
        | Error Message | Cause | Solution |
        |--------------|-------|----------|
        | **"Premature end of data"** | Missing closing tag | Check all tags are properly closed |
        | **"Invalid character"** | Encoding issue | Ensure UTF-8 encoding, check special chars |
        | **"Element not expected"** | Schema validation error | Element not allowed in current context |
        | **"Entity not defined"** | Unknown entity reference | Define entity in DTD or use numeric reference |
        | **"Attribute redefined"** | Duplicate attribute | Remove duplicate attribute |
        | **"No root element"** | Empty or invalid file | Ensure file has single root element |
        | **"Namespace not declared"** | Missing xmlns | Declare namespace before use |
        """)
        
        show_xml_example(
            "Common Mistakes & Fixes",
            "Frequently encountered XML errors and solutions",
            """<!-- WRONG: Unclosed tag -->
<product>
    <name>Laptop
    <price>999.99</price>
</product>

<!-- CORRECT: Properly closed -->
<product>
    <name>Laptop</name>
    <price>999.99</price>
</product>

<!-- WRONG: Multiple root elements -->
<product1>...</product1>
<product2>...</product2>

<!-- CORRECT: Single root element -->
<products>
    <product>...</product>
    <product>...</product>
</products>

<!-- WRONG: Unescaped special characters -->
<comparison>5 < 10 & 10 > 5</comparison>

<!-- CORRECT: Properly escaped -->
<comparison>5 &lt; 10 &amp; 10 &gt; 5</comparison>

<!-- WRONG: Invalid attribute names -->
<element 2nd-attr="value" my attr="value">

<!-- CORRECT: Valid attribute names -->
<element second-attr="value" my_attr="value">

<!-- WRONG: Incorrect namespace usage -->
<doc>
    <app:config>value</app:config>  <!-- 'app' not declared -->
</doc>

<!-- CORRECT: Namespace declared -->
<doc xmlns:app="http://example.com/app">
    <app:config>value</app:config>
</doc>""",
            matches_search("error mistake wrong fix", search_query)
        )
        
    with support_tabs[2]:  # Debugging
        st.subheader("🔧 XML Debugging")
        
        show_xml_example(
            "Debugging Tools & Techniques",
            "Find and fix XML issues",
            """# Python XML debugging script
import xml.etree.ElementTree as ET
import sys

def debug_xml(filename):
    print(f"Debugging {filename}...")
    print("-" * 50)
    
    try:
        # Try to parse
        tree = ET.parse(filename)
        root = tree.getroot()
        print("✓ XML is well-formed")
        
        # Show structure
        print(f"\\nRoot element: <{root.tag}>")
        print(f"Root attributes: {root.attrib}")
        
        # Count elements
        all_tags = [elem.tag for elem in root.iter()]
        from collections import Counter
        tag_counts = Counter(all_tags)
        
        print("\\nElement counts:")
        for tag, count in tag_counts.most_common():
            print(f"  {tag}: {count}")
            
        # Check for namespaces
        namespaces = set()
        for elem in root.iter():
            if '}' in elem.tag:
                ns = elem.tag.split('}')[0][1:]
                namespaces.add(ns)
        
        if namespaces:
            print("\\nNamespaces found:")
            for ns in namespaces:
                print(f"  {ns}")
                
        # Find empty elements
        empty = []
        for elem in root.iter():
            if elem.text is None or elem.text.strip() == '':
                if len(elem) == 0:  # No children
                    empty.append(elem.tag)
        
        if empty:
            print("\\nEmpty elements:")
            for tag in set(empty):
                print(f"  {tag}")
                
    except ET.ParseError as e:
        print(f"✗ Parse error: {e}")
        print(f"\\nError location: Line {e.position[0]}, Column {e.position[1]}")
        
        # Try to show context
        try:
            with open(filename, 'r') as f:
                lines = f.readlines()
                error_line = e.position[0] - 1
                
                print("\\nContext:")
                start = max(0, error_line - 2)
                end = min(len(lines), error_line + 3)
                
                for i in range(start, end):
                    marker = ">>>" if i == error_line else "   "
                    print(f"{marker} {i+1}: {lines[i].rstrip()}")
                    
        except:
            pass

# Usage
debug_xml('problematic.xml')""",
            "python",
            matches_search("debug troubleshoot parse error", search_query)
        )

# Reference Tab
with role_tab4:
    st.header("📚 Quick Reference")
    
    col1, col2, col3 = st.columns(3)
    
    with col1:
        st.subheader("🏷️ XML Syntax")
        st.code("""
<!-- Declaration -->
<?xml version="1.0" encoding="UTF-8"?>

<!-- Elements -->
<element>Content</element>
<element attribute="value"/>
<empty-element/>

<!-- Attributes -->
<elem id="123" class="main" enabled="true">

<!-- Comments -->
<!-- This is a comment -->

<!-- CDATA -->
<![CDATA[Literal text with <, >, &]]>

<!-- Entities -->
&lt;    <!-- < -->
&gt;    <!-- > -->
&amp;   <!-- & -->
&apos;  <!-- ' -->
&quot;  <!-- " -->

<!-- Processing Instructions -->
<?target instructions?>

<!-- DOCTYPE -->
<!DOCTYPE root SYSTEM "dtd-file.dtd">
<!DOCTYPE root [
    <!ENTITY name "value">
]>
""", language="xml")
    
    with col2:
        st.subheader("🔍 XPath Quick Ref")
        st.code("""
AXES:
/           Root or child
//          Descendant
.           Current node
..          Parent
@           Attribute
*           Any element
@*          Any attribute
text()      Text content
node()      Any node

PREDICATES:
[1]         First element
[last()]    Last element
[@id='x']   Attribute equals
[price>10]  Value comparison

OPERATORS:
=, !=       Equality
<, >, <=, >= Comparison
+, -, *, div Math
and, or     Logic
|           Union

FUNCTIONS:
count()     Node count
sum()       Sum values
contains()  String contains
starts-with() String start
string-length() Length
position()  Current position
""", language="text")
    
    with col3:
        st.subheader("🛠️ Useful Commands")
        st.code("""
# Format XML (xmllint)
xmllint --format input.xml

# Validate
xmllint --noout file.xml
xmllint --schema schema.xsd file.xml

# XPath query
xmllint --xpath "//book/title" file.xml

# XSLT transform
xsltproc style.xsl input.xml > output.html

# Python one-liners
# Parse and print root
python -c "import xml.etree.ElementTree as ET; print(ET.parse('file.xml').getroot().tag)"

# Count elements
python -c "import xml.etree.ElementTree as ET; print(len(ET.parse('file.xml').findall('.//element')))"

# Extract text
python -c "import xml.etree.ElementTree as ET; [print(e.text) for e in ET.parse('file.xml').findall('.//title')]"

# Convert to JSON
python -c "import xmltodict, json, sys; print(json.dumps(xmltodict.parse(sys.stdin.read())))" < file.xml
""", language="bash")

# Best Practices
st.markdown("---")
st.header("💡 XML Best Practices")

bp_col1, bp_col2 = st.columns(2)

with bp_col1:
    st.markdown("""
    **Document Structure**
    - ✅ Always include XML declaration
    - ✅ Use UTF-8 encoding
    - ✅ One root element per document
    - ✅ Close all tags properly
    - ✅ Use meaningful element names
    - ✅ Be consistent with naming conventions
    
    **When to Use Attributes vs Elements**
    - **Attributes**: Metadata, IDs, simple values
    - **Elements**: Complex data, multiple values, future extensibility
    
    ```xml
    <!-- Good: Metadata as attributes -->
    <book id="123" lang="en">
        <title>XML Guide</title>
        <description>Long text...</description>
    </book>
    ```
    """)

with bp_col2:
    st.markdown("""
    **Performance & Optimization**
    - ✅ Use streaming parsers for large files (SAX)
    - ✅ Avoid deeply nested structures
    - ✅ Consider binary XML for performance
    - ✅ Use compression for storage/transfer
    - ✅ Index frequently queried paths
    
    **Security Considerations**
    - ⚠️ Disable external entity processing (XXE prevention)
    - ⚠️ Validate all input XML
    - ⚠️ Use schemas for strict validation
    - ⚠️ Limit document size
    - ⚠️ Sanitize before displaying
    
    ```python
    # Safe parsing (prevent XXE)
    parser = ET.XMLParser(resolve_entities=False)
    tree = ET.parse('file.xml', parser)
    ```
    """)

# XML vs JSON comparison
st.markdown("---")
st.header("📊 XML vs JSON")

st.markdown("""
| Feature | XML | JSON |
|---------|-----|------|
| **Human Readable** | Yes (with formatting) | Yes |
| **Schema Support** | XSD, DTD | JSON Schema |
| **Namespaces** | Yes | No |
| **Attributes** | Yes | No (use nested objects) |
| **Comments** | Yes | No (unofficial) |
| **Data Types** | Text (schema defines types) | String, Number, Boolean, null |
| **Arrays** | Repeated elements | Native arrays |
| **Size** | Larger (tags) | Smaller |
| **Parse Speed** | Slower | Faster |
| **Use Cases** | Config files, documents, SOAP | REST APIs, web apps |
""")

# Footer
st.markdown("---")
st.caption("💡 **Pro Tip**: Use the search box to quickly find XML syntax, XPath expressions, and troubleshooting tips!")