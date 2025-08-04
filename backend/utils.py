# ============================================================================
# utils.py
# ============================================================================
"""
Shared utility functions for data processing and conversion.
"""

import pandas as pd

def clean_value(value):
    """Clean entry values and convert to lowercase strings"""
    if value is None or pd.isna(value): 
        return None
    return str(value).strip().lower()

def safe_float(value):
    """Convert value to float safely, handle NaN and None"""
    if pd.isna(value) or value is None:
        return None
    try:
        str_value = str(value).replace(',', '')
        return float(str_value)
    except (ValueError, TypeError):
        return None

def safe_int(value):
    """Convert value to int safely, handle NaN and None"""
    if pd.isna(value) or value is None:
        return None
    try:
        return int(float(value))
    except (ValueError, TypeError):
        return None