import os

# ============================================================================
# PROJECT PATHS
# ============================================================================

# Get the absolute path to the project root (two levels up from this file)
# PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__))) ---------------- Local path
# PROJECT_ROOT = "/app"  ---------------------------------------------------------------- Container path

# Folder containing all data files
# DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'happy_files') -------------------- Local path
# DATA_FOLDER = os.getenv('BENCHMARKING_DATA_LOCATION', '/data') -------------------- Container path

#------------------------------------
import os

# Simple container detection
IS_CONTAINER = os.path.exists("/app") and os.path.exists("/app/backend")

if IS_CONTAINER:
    # Container: everything in mounted /data volume
    PROJECT_ROOT = "/app"
    DATA_FOLDER = "/data"
    
    # MINIMAL FIX: Check if files are in happy_files subdirectory
    if os.path.exists("/data/happy_files"):
        DATA_FOLDER = "/data/happy_files"
    
    METADATA_CSV_PATH = os.path.join(DATA_FOLDER, "000_benchmark_dashboard_default_metadata.csv")
    DATABASE_PATH = os.path.join(DATA_FOLDER, "benchmarking.db")
else:
    # Local development
    PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    DATA_FOLDER = os.path.join(PROJECT_ROOT, 'data', 'happy_files')
    METADATA_CSV_PATH = os.path.join(DATA_FOLDER, '000_benchmark_dashboard_default_metadata.csv')
    DATABASE_PATH = os.path.join(PROJECT_ROOT, 'data', 'benchmarking.db')

# Ensure directories exist locally
if not IS_CONTAINER:
    os.makedirs(DATA_FOLDER, exist_ok=True)
    os.makedirs(os.path.dirname(DATABASE_PATH), exist_ok=True)

def get_data_file_path(filename):
    return os.path.join(DATA_FOLDER, filename)

# Debug info
print(f"Container mode: {IS_CONTAINER}")
print(f"DATA_FOLDER: {DATA_FOLDER}")
print(f"METADATA_CSV exists: {os.path.exists(METADATA_CSV_PATH)}")

# The key fix: METADATA_CSV_FILENAME
METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'