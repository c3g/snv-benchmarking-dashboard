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
# temp 
# Detect if running in container vs local environment
if os.path.exists("/app") and os.path.exists("/app/backend"):
    # Container environment
    PROJECT_ROOT = "/app"
else:
    # Local development environment
    PROJECT_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Folder containing all data files
DATA_FOLDER = os.getenv('BENCHMARKING_DATA_LOCATION', 
                       os.path.join(PROJECT_ROOT, 'data', 'happy_files') if PROJECT_ROOT != "/app" 
                       else '/data')

#-----------------------------------

os.makedirs(DATA_FOLDER, exist_ok=True) # ensure data directory exists 

# Metadata CSV filename
METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'

# Full path to metadata CSV file (in data folder, not happy_files)
METADATA_CSV_PATH = os.path.join(PROJECT_ROOT,'data', 'happy_files', METADATA_CSV_FILENAME)

# Database filename
DATABASE_PATH = os.path.join(PROJECT_ROOT, 'data', 'benchmarking.db')

# Function to get path to any file in data folder
def get_data_file_path(filename):
    """Get full path to a file in the data folder"""
    return os.path.join(PROJECT_ROOT, 'data','happy_files', filename)