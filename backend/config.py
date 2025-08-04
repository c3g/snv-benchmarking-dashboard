import os
import logging

#logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s - %(levelname)s: %(message)s',
    datefmt='%H:%M:%S' 
)
logger = logging.getLogger(__name__)

# ============================================================================
# PATHS
# ============================================================================

# Container detection 
IS_CONTAINER = os.path.exists("/app") and os.path.exists("/app/backend")

if IS_CONTAINER:
    PROJECT_ROOT = "/app"
    DATA_FOLDER = "/data"
    
    # Check for happy_files subdirectory
    if os.path.exists("/data/happy_files/000_benchmark_dashboard_default_metadata.csv"):
        DATA_FOLDER = "/data/happy_files"
        
else: # local deployment (for testing)
    PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'happy_files')

# Paths
METADATA_CSV_PATH = os.path.join(DATA_FOLDER, "000_benchmark_dashboard_default_metadata.csv")
DATABASE_PATH = os.path.join(DATA_FOLDER if IS_CONTAINER else os.path.join(PROJECT_ROOT, 'data'), "benchmarking.db")

# ============================================================================
# VALIDATION & SETUP
# ============================================================================

def setup_and_validate():
    """Setup directories and validate access"""
    
    # Create directories
    try:
        os.makedirs(DATA_FOLDER, exist_ok=True)
        os.makedirs(os.path.dirname(DATABASE_PATH), exist_ok=True)
        logger.info(f"Directories created: {DATA_FOLDER}")
    except Exception as e:
        logger.error(f"Failed to create directories: {e}")
        return False
    
    # Validate directory access
    if not os.path.exists(DATA_FOLDER):
        logger.error(f"DATA_FOLDER does not exist: {DATA_FOLDER}")
        return False
    
    if not os.access(DATA_FOLDER, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to DATA_FOLDER: {DATA_FOLDER}")
        return False
    
    # Validate database directory access
    db_dir = os.path.dirname(DATABASE_PATH)
    if not os.path.exists(db_dir):
        logger.error(f"Database directory does not exist: {db_dir}")
        return False
        
    if not os.access(db_dir, os.R_OK | os.W_OK):
        logger.error(f"Cannot read/write to database directory: {db_dir}")
        return False
    
    # Check metadata CSV access
    if os.path.exists(METADATA_CSV_PATH):
        if os.access(METADATA_CSV_PATH, os.R_OK):
            logger.info(f"Metadata CSV accessible: {METADATA_CSV_PATH}")
        else:
            logger.warning(f"Metadata CSV exists but not readable: {METADATA_CSV_PATH}")
    else:
        logger.info("Metadata CSV not found")
    
    logger.info("--- Configuration validation successful ---")
    return True

# ============================================================================
# FUNCTIONS & CONSTANTS
# ============================================================================

def get_data_file_path(filename):
    return os.path.join(DATA_FOLDER, filename)

METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'

# ============================================================================
# AUTO-SETUP
# ============================================================================

config_valid = setup_and_validate()

# Debug info
logger.info(f"Container mode: {IS_CONTAINER}")
logger.info(f"DATA_FOLDER: {DATA_FOLDER}") 
logger.info(f"DATABASE_PATH: {DATABASE_PATH}")
