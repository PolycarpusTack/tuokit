import streamlit as st
from utils import DatabaseManager
import json
import re
from datetime import datetime

# Page configuration
st.set_page_config(
    page_title="TuoKit - Knowledge Library",
    page_icon="📚",
    layout="wide"
)

def format_knowledge(content, category):
    """Format knowledge content based on its type"""
    if category in ["Code Snippet", "Algorithm", "Utility Function", "Error Solution"]:
        return st.code(content, language="python")
    elif category == "JSON" or "knowledge" in category.lower():
        try:
            parsed = json.loads(content)
            return st.json(parsed)
        except:
            return st.code(content)
    else:
        return st.markdown(content)

def search_knowledge(db, search_term="", category=None):
    """Search knowledge base with filters"""
    if not db or not db.connected:
        return []
    
    try:
        with db.conn.cursor() as cur:            if category and category != "All":
                cur.execute("""
                    SELECT k.id, k.title, k.content, k.category, 
                           k.created_at as k_created_at,
                           q.created_at as q_created_at, q.tool, q.model
                    FROM knowledge_units k
                    JOIN queries q ON k.query_id = q.id
                    WHERE (k.title ILIKE %s OR k.content ILIKE %s)
                    AND k.category = %s
                    ORDER BY k.created_at DESC
                """, (f'%{search_term}%', f'%{search_term}%', category))
            else:
                cur.execute("""
                    SELECT k.id, k.title, k.content, k.category,
                           k.created_at as k_created_at,
                           q.created_at as q_created_at, q.tool, q.model
                    FROM knowledge_units k
                    JOIN queries q ON k.query_id = q.id
                    WHERE k.title ILIKE %s OR k.content ILIKE %s
                    ORDER BY k.created_at DESC
                """, (f'%{search_term}%', f'%{search_term}%'))
            return cur.fetchall()
    except Exception as e:
        st.error(f"Search error: {e}")
        return []

# Initialize session state for database
if "db" not in st.session_state:
    try:
        st.session_state.db = DatabaseManager()
    except Exception as e:        st.error(f"Database connection failed: {e}")
        st.session_state.db = None

# Main content
st.title("📚 Knowledge Library")
st.caption("Browse and search your AI-generated knowledge base")

if not st.session_state.db:
    st.warning("Database connection required for Knowledge Library")
    st.info("Please configure your database connection in the .env file")
    if st.button("← Back to Dashboard"):
        st.switch_page("app.py")
    st.stop()

# Search and filter controls
col1, col2, col3 = st.columns([3, 1, 1])
with col1:
    search_term = st.text_input("🔍 Search knowledge base", 
                               placeholder="Search by title or content...")
with col2:
    categories = ["All", "Code Snippet", "Algorithm", "Error Solution", 
                 "Utility Function", "Document Summary", "Research Findings", 
                 "Meeting Notes", "Technical Documentation"]
    category = st.selectbox("Category", categories)
with col3:
    sort_order = st.selectbox("Sort by", ["Newest", "Oldest", "Title"])

# Display results
results = search_knowledge(st.session_state.db, search_term, 
                         category if category != "All" else None)

if not results:
    st.info("No knowledge units found matching your criteria")else:
    # Results counter and stats
    st.caption(f"Found {len(results)} knowledge units")
    
    # Sort results if needed
    if sort_order == "Oldest":
        results.reverse()
    elif sort_order == "Title":
        results.sort(key=lambda x: x[1])
    
    # Knowledge display
    for idx, (k_id, title, content, k_category, k_created_at, 
              q_created_at, tool, model) in enumerate(results):
        
        # Create expander with category emoji
        emoji = {"Code Snippet": "💻", "Document Summary": "📄", 
                "Error Solution": "🐛", "Algorithm": "🔢",
                "Research Findings": "🔬", "Meeting Notes": "📝",
                "Technical Documentation": "📚", "Utility Function": "🔧"
                }.get(k_category, "📌")
        
        with st.expander(f"{emoji} {title}"):
            # Metadata row
            col1, col2 = st.columns([4, 1])
            with col1:
                st.caption(f"Created: {k_created_at.strftime('%Y-%m-%d %H:%M')} | "
                          f"Tool: {tool} | Model: {model}")
            with col2:
                st.caption(f"Category: {k_category}")
            
            # Display content with type-appropriate formatting
            format_knowledge(content, k_category)            
            # Action buttons
            col1, col2, col3, col4 = st.columns(4)
            with col1:
                if st.button("📋 Copy", key=f"copy_{idx}", use_container_width=True):
                    st.session_state.clipboard = content
                    st.success("Copied to clipboard!")
            with col2:
                if st.button("✏️ Edit", key=f"edit_{idx}", use_container_width=True):
                    st.session_state.edit_mode = k_id
                    st.session_state.edit_title = title
                    st.session_state.edit_content = content
                    st.rerun()
            with col3:
                if st.button("📤 Export", key=f"export_{idx}", use_container_width=True):
                    st.download_button(
                        label="Download",
                        data=content,
                        file_name=f"{title.replace(' ', '_')}.txt",
                        mime="text/plain",
                        key=f"download_{idx}"
                    )
            with col4:
                if st.button("🗑️ Delete", key=f"delete_{idx}", use_container_width=True):
                    if st.session_state.get(f"confirm_delete_{idx}", False):
                        try:
                            with st.session_state.db.conn.cursor() as cur:
                                cur.execute("DELETE FROM knowledge_units WHERE id = %s", (k_id,))
                            st.success("Knowledge unit deleted")
                            st.rerun()
                        except Exception as e:
                            st.error(f"Error deleting: {e}")
                    else:
                        st.session_state[f"confirm_delete_{idx}"] = True
                        st.warning("Click delete again to confirm")
# Edit mode
if "edit_mode" in st.session_state and st.session_state.edit_mode:
    st.divider()
    st.subheader("✏️ Edit Knowledge Unit")
    
    edited_title = st.text_input("Title", value=st.session_state.edit_title)
    edited_content = st.text_area("Content", value=st.session_state.edit_content, height=300)
    
    col1, col2 = st.columns(2)
    with col1:
        if st.button("💾 Save Changes", use_container_width=True):
            try:
                with st.session_state.db.conn.cursor() as cur:
                    cur.execute("""
                        UPDATE knowledge_units 
                        SET title = %s, content = %s 
                        WHERE id = %s
                    """, (edited_title, edited_content, st.session_state.edit_mode))
                st.success("Knowledge unit updated!")
                del st.session_state.edit_mode
                del st.session_state.edit_title
                del st.session_state.edit_content
                st.rerun()
            except Exception as e:
                st.error(f"Error updating: {e}")
    
    with col2:
        if st.button("❌ Cancel", use_container_width=True):
            del st.session_state.edit_mode
            del st.session_state.edit_title
            del st.session_state.edit_content
            st.rerun()
# Knowledge statistics
st.divider()
st.subheader("📊 Knowledge Base Statistics")

try:
    with st.session_state.db.conn.cursor() as cur:
        # Total knowledge units
        cur.execute("SELECT COUNT(*) FROM knowledge_units")
        total_units = cur.fetchone()[0]
        
        # Knowledge by category
        cur.execute("""
            SELECT category, COUNT(*) 
            FROM knowledge_units 
            GROUP BY category 
            ORDER BY COUNT(*) DESC
        """)
        category_stats = cur.fetchall()
        
        # Recent activity
        cur.execute("""
            SELECT DATE(created_at) as date, COUNT(*) 
            FROM knowledge_units 
            WHERE created_at > CURRENT_DATE - INTERVAL '7 days'
            GROUP BY DATE(created_at)
            ORDER BY date DESC
        """)
        recent_activity = cur.fetchall()
    
    col1, col2, col3 = st.columns(3)
    with col1:
        st.metric("Total Knowledge Units", total_units)
    with col2:
        st.metric("Categories", len(category_stats))
    with col3:
        weekly_total = sum(count for _, count in recent_activity)
        st.metric("Added This Week", weekly_total)    
    # Category breakdown
    if category_stats:
        st.caption("Knowledge by Category:")
        for cat, count in category_stats[:5]:  # Show top 5
            st.progress(count / total_units, text=f"{cat}: {count}")
            
except Exception as e:
    st.error(f"Error loading statistics: {e}")

# Export all knowledge
st.divider()
if st.button("📥 Export All Knowledge", use_container_width=True):
    try:
        with st.session_state.db.conn.cursor() as cur:
            cur.execute("""
                SELECT k.title, k.content, k.category, k.created_at,
                       q.tool, q.model
                FROM knowledge_units k
                JOIN queries q ON k.query_id = q.id
                ORDER BY k.created_at DESC
            """)
            all_knowledge = cur.fetchall()
        
        # Create export content
        export_content = "# TuoKit Knowledge Export\n\n"
        for title, content, category, created_at, tool, model in all_knowledge:
            export_content += f"## {title}\n"
            export_content += f"**Category:** {category}\n"
            export_content += f"**Created:** {created_at}\n"
            export_content += f"**Tool:** {tool} | **Model:** {model}\n\n"
            export_content += f"```\n{content}\n```\n\n---\n\n"
        
        st.download_button(
            label="Download Knowledge Export",
            data=export_content,
            file_name=f"tuokit_knowledge_{datetime.now().strftime('%Y%m%d_%H%M%S')}.md",
            mime="text/markdown"
        )
    except Exception as e:
        st.error(f"Error exporting: {e}")

# Back to dashboard
st.divider()
col1, col2 = st.columns([1, 1])
with col1:
    if st.button("← Back to Dashboard", use_container_width=True):
        st.switch_page("app.py")
with col2:
    if st.button("❓ Help", use_container_width=True):
        st.switch_page("pages/help_guide.py")