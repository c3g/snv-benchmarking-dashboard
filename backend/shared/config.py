# ============================================================================
# config.py
# ============================================================================
"""
Environment configuration and path management.

Main components:
- Container vs local environment detection
- Path configuration for data and database files
- Environment constants and settings
- Utility functions (get_snv_file_path, get_sv_file_path)

TO DO: RENAME THE 000_benchmark_dashboard_default_metadata.csv to make it SNV-specific.
"""

import os
import logging

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
    # config.py is at backend/shared/config.py — need 3 dirnams to reach project root
    PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'happy_files')

# ============================================================================
# SNV PATHS (unchanged from original)
# ============================================================================

METADATA_CSV_PATH = os.path.join(DATA_FOLDER, '000_benchmark_dashboard_default_metadata.csv')
DATABASE_PATH = os.path.join(DATA_FOLDER if IS_CONTAINER else os.path.join(PROJECT_ROOT, 'data'), 'benchmarking.db')

METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'
DELETED_CSV_PATH = os.path.join(DATA_FOLDER, '000_deleted.csv')

# Alias used by snv/ modules (DATA_FOLDER and SNV_DATA_FOLDER are the same thing)
SNV_DATA_FOLDER = DATA_FOLDER

# SNV backup CSV paths (used by snv/csv_backup.py)
SNV_METADATA_CSV_PATH = os.path.join(SNV_DATA_FOLDER, '000_benchmark_dashboard_default_metadata.csv')
SNV_DELETED_CSV_PATH = DELETED_CSV_PATH


def get_data_file_path(filename):
    """Original SNV helper — OPTIONAL"""
    return os.path.join(DATA_FOLDER, filename)


def get_snv_file_path(filename):
    """SNV file path helper."""
    return os.path.join(SNV_DATA_FOLDER, filename)

# ============================================================================
# SV PATHS (same pattern as SNV)
# ============================================================================

if IS_CONTAINER:
    SV_DATA_FOLDER = "/data/sv_files"
else:
    SV_DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'sv_files')

SV_METADATA_CSV_PATH = os.path.join(SV_DATA_FOLDER, '000_sv_benchmark_metadata.csv')
SV_DELETED_CSV_PATH = os.path.join(SV_DATA_FOLDER, '000_sv_deleted.csv')


def get_sv_file_path(filename):
    """SV file path helper."""
    return os.path.join(SV_DATA_FOLDER, filename)

# ============================================================================
# DIRECTORY CREATION (local dev only)
# ============================================================================

if not IS_CONTAINER:
    os.makedirs(DATA_FOLDER, exist_ok=True)
    os.makedirs(SV_DATA_FOLDER, exist_ok=True)
    os.makedirs(os.path.dirname(DATABASE_PATH), exist_ok=True)

# Debug info
logger.info(f"Container mode: {IS_CONTAINER}")
logger.info(f"DATA_FOLDER (SNV): {DATA_FOLDER}")
logger.info(f"SV_DATA_FOLDER: {SV_DATA_FOLDER}")
logger.info(f"METADATA_CSV exists: {os.path.exists(METADATA_CSV_PATH)}")