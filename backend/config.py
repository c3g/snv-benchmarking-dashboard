# ============================================================================
# config.py
# ============================================================================
"""
Environment configuration and path management for SNV Benchmarking Dashboard.

Main components:
- Container vs local environment detection
- Path configuration for data and database files
- Environment constants and settings
- Utility functions (ie: get happy file path)
"""

import os
import logging

#logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s: %(message)s',
    datefmt='%H:%M:%S' 
)
logger = logging.getLogger(__name__)

# Container detection 
IS_CONTAINER = os.path.exists("/app") and os.path.exists("/app/backend")

if IS_CONTAINER:
    # Container: everything in mounted /data volume
    PROJECT_ROOT = "/app"
    DATA_FOLDER = "/data"
    
    # Check if files are in happy_files subdirectory
    happy_files_path = "/data/happy_files"
    metadata_in_happy_files = os.path.join(happy_files_path, "000_benchmark_dashboard_default_metadata.csv")
    
    if os.path.exists(metadata_in_happy_files):
        DATA_FOLDER = happy_files_path
    
else:
    # Local development
    PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'happy_files')

# File paths
METADATA_CSV_PATH = os.path.join(DATA_FOLDER, '000_benchmark_dashboard_default_metadata.csv')
DATABASE_PATH = os.path.join(DATA_FOLDER if IS_CONTAINER else os.path.join(PROJECT_ROOT, 'data'), 'benchmarking.db')

# Constants
METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'

def get_data_file_path(filename):
    return os.path.join(DATA_FOLDER, filename)

# Ensure directories exist for local development
if not IS_CONTAINER:
    os.makedirs(DATA_FOLDER, exist_ok=True)
    os.makedirs(os.path.dirname(DATABASE_PATH), exist_ok=True)

# Debug info
logger.info(f"Container mode: {IS_CONTAINER}")
logger.info(f"DATA_FOLDER: {DATA_FOLDER}")
logger.info(f"METADATA_CSV exists: {os.path.exists(METADATA_CSV_PATH)}")