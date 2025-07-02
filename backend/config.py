import os

# ============================================================================
# PROJECT PATHS (temporary) 
# ============================================================================

# Folder containing all data files
DATA_FOLDER = 'happy_files'

# Metadata CSV filename  
METADATA_CSV_FILENAME = '000_benchmark_dashboard_default_metadata.csv'

# Database filename
DATABASE_PATH = 'benchmarking.db'


# Full path to metadata CSV file
METADATA_CSV_PATH = os.path.join(DATA_FOLDER, METADATA_CSV_FILENAME)

# Function to get path to any file in data folder
def get_data_file_path(filename):
    """Get full path to a file in the data folder"""
    return os.path.join(DATA_FOLDER, filename)