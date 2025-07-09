"""
Centralized Ollama Model Management for TuoKit
Provides dynamic model selection and fallback handling
"""

import streamlit as st
from typing import List, Optional, Dict, Any
from utils.ollama import get_available_models, check_model_availability
import logging

logger = logging.getLogger(__name__)

class ModelManager:
    """Centralized model management for all TuoKit tools"""
    
    @staticmethod
    def get_preferred_model(available_models: List[str], 
                          preferred_keywords: List[str] = None) -> str:
        """
        Get the best available model based on preferences
        
        Args:
            available_models: List of available models
            preferred_keywords: Keywords to prefer (default: common good models)
            
        Returns:
            Best available model or first model in list
        """
        if not available_models:
            return None
            
        if preferred_keywords is None:
            # Default preferences based on quality and speed
            preferred_keywords = ["deepseek-r1", "deepseek-coder", "llama", "mistral", "gpt"]
        
        # Try to find a preferred model
        for keyword in preferred_keywords:
            for model in available_models:
                if keyword in model.lower():
                    return model
        
        # Return first available if no preference matches
        return available_models[0]
    
    @staticmethod
    def get_default_model() -> str:
        """Get the default model with dynamic fallback"""
        # Check session state first
        if "selected_model" in st.session_state and st.session_state.selected_model:
            return st.session_state.selected_model
        
        # Get available models
        available_models = get_available_models()
        
        if not available_models:
            # Return a sensible default that user should install
            logger.warning("No Ollama models available")
            return "deepseek-r1:1.5b"  # Lightweight default suggestion
        
        # Get best available model
        best_model = ModelManager.get_preferred_model(available_models)
        
        # Store in session state
        st.session_state.selected_model = best_model
        return best_model
    
    @staticmethod
    def render_model_selector(key: str = "main_model_selector",
                            help_text: str = None,
                            preferred_keywords: List[str] = None) -> Optional[str]:
        """
        Render a dynamic model selector UI component
        
        Args:
            key: Unique key for the selectbox
            help_text: Custom help text
            preferred_keywords: Keywords for preferred models
            
        Returns:
            Selected model name or None
        """
        available_models = get_available_models()
        
        if not available_models:
            st.warning("⚠️ No Ollama models found. Please install models first.")
            st.code("ollama pull deepseek-r1:1.5b", language="bash")
            
            col1, col2 = st.columns([3, 1])
            with col1:
                st.selectbox(
                    "Select Model",
                    ["No models installed"],
                    disabled=True,
                    key=key,
                    help="Install models in Ollama first"
                )
            with col2:
                if st.button("🔄 Refresh", key=f"{key}_refresh"):
                    st.rerun()
            
            return None
        
        # Get current selection or best default
        current_model = st.session_state.get("selected_model")
        if not current_model or current_model not in available_models:
            current_model = ModelManager.get_preferred_model(available_models, preferred_keywords)
        
        # Find index of current model
        try:
            default_index = available_models.index(current_model)
        except ValueError:
            default_index = 0
        
        # Model selection
        col1, col2, col3 = st.columns([3, 1, 1])
        
        with col1:
            selected_model = st.selectbox(
                "Select Model",
                available_models,
                index=default_index,
                key=key,
                help=help_text or "Choose the AI model to use"
            )
        
        with col2:
            if selected_model:
                status = check_model_availability(selected_model)
                status_icon = {
                    "ready": "🟢",
                    "needs_pull": "🟡",
                    "pull_failed": "🔴"
                }.get(status["status"], "⚪")
                st.metric("Status", status_icon)
        
        with col3:
            if st.button("🔄 Refresh", key=f"{key}_refresh_btn"):
                st.rerun()
        
        # Update session state
        if selected_model:
            st.session_state.selected_model = selected_model
        
        return selected_model
    
    @staticmethod
    def get_model_for_task(task_type: str = "general") -> str:
        """
        Get appropriate model for specific task type
        
        Args:
            task_type: Type of task (general, code, analysis, etc.)
            
        Returns:
            Best available model for the task
        """
        available_models = get_available_models()
        
        if not available_models:
            return ModelManager.get_default_model()
        
        # Task-specific preferences
        task_preferences = {
            "code": ["deepseek-coder", "codellama", "starcoder", "deepseek-r1"],
            "analysis": ["deepseek-r1", "llama", "mixtral", "mistral"],
            "sql": ["deepseek-coder", "sqlcoder", "deepseek-r1"],
            "explanation": ["llama", "mistral", "deepseek-r1"],
            "general": ["deepseek-r1", "llama", "mistral", "gpt"]
        }
        
        preferences = task_preferences.get(task_type, task_preferences["general"])
        return ModelManager.get_preferred_model(available_models, preferences)
    
    @staticmethod
    def ensure_model_available(model_name: str) -> bool:
        """
        Ensure a model is available, with user-friendly error handling
        
        Args:
            model_name: Model to check
            
        Returns:
            True if model is available/can be used
        """
        status = check_model_availability(model_name)
        
        if status["available"]:
            return True
        
        if status["can_attempt_pull"]:
            st.info(f"📥 Model '{model_name}' will be downloaded on first use.")
            return True
        
        # Model failed before
        st.error(f"❌ Model '{model_name}' is not available.")
        
        # Suggest alternatives
        available_models = get_available_models()
        if available_models:
            alternative = ModelManager.get_preferred_model(available_models)
            st.info(f"💡 Try using '{alternative}' instead.")
            
            if st.button(f"Use {alternative}", key="use_alternative"):
                st.session_state.selected_model = alternative
                st.rerun()
        
        return False


# Convenience functions for backward compatibility
def get_default_model() -> str:
    """Get the default model with dynamic fallback"""
    return ModelManager.get_default_model()


def render_model_selector(**kwargs) -> Optional[str]:
    """Render model selector UI"""
    return ModelManager.render_model_selector(**kwargs)