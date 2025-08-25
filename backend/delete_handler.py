# ============================================================================
# delete_handler.py 
# ============================================================================
"""
Experiment deletion and database cleanup for SNV Benchmarking Dashboard.

Main components:
- Experiment removal from CSV metadata
- Associated file cleanup 
- Database rebuilding with backup/restore
"""

import pandas as pd
import os
import shutil
import logging
from config import METADATA_CSV_PATH, DATA_FOLDER
from database import drop_all_data
from populate_metadata import populate_database_from_csv

logger = logging.getLogger(__name__)

# Single backup file path
BACKUP_CSV_PATH = f"{METADATA_CSV_PATH}.backup"

def delete_experiment(experiment_id):
    """
    Delete experiment by removing from CSV and rebuilding database
    """
    try:
        logger.info(f"Starting deletion of experiment {experiment_id}")
        
        # 1. Create backup (overwrites previous backup)
        backup_csv()
        
        # 2. Load current CSV
        if not os.path.exists(METADATA_CSV_PATH):
            return {"success": False, "error": "Metadata CSV not found"}
            
        df = pd.read_csv(METADATA_CSV_PATH)
        
        # 3. Find and remove the experiment
        if experiment_id not in df['ID'].values:
            return {"success": False, "error": f"Experiment {experiment_id} not found"}
        
        target_row = df[df['ID'] == experiment_id].iloc[0]
        happy_file = target_row['file_name'] if 'file_name' in target_row and pd.notna(target_row['file_name']) else None
        
        # 4. Remove from dataframe (no reindexing)
        df = df[df['ID'] != experiment_id].copy()
        
        # 5. Save updated CSV
        df.to_csv(METADATA_CSV_PATH, index=False)
        logger.info(f"Updated metadata CSV, removed experiment {experiment_id}")
        
        # 6. Delete happy file if it exists
        if happy_file:
            happy_file_path = os.path.join(DATA_FOLDER, happy_file)
            if os.path.exists(happy_file_path):
                os.remove(happy_file_path)
                logger.info(f"Deleted file: {happy_file}")
        
        # 7. Rebuild database from CSV
        logger.info("Rebuilding database...")
        drop_all_data()
        success = populate_database_from_csv()
        
        if success:
            logger.info(f"Successfully deleted experiment {experiment_id}")
            # Success - can optionally clean up backup
            # cleanup_backup()  # Uncomment if you want to remove backup after success
            return {"success": True, "message": f"Experiment {experiment_id} deleted and database rebuilt"}
        else:
            # Restore backup if database rebuild failed
            restore_csv_backup()
            return {"success": False, "error": "Database rebuild failed, changes reverted"}
            
    except Exception as e:
        logger.error(f"Delete failed for experiment {experiment_id}: {e}")
        restore_csv_backup()
        return {"success": False, "error": f"Delete failed: {str(e)}"}

def backup_csv():
    """Create a backup metadata file"""
    if os.path.exists(METADATA_CSV_PATH):
        shutil.copy2(METADATA_CSV_PATH, BACKUP_CSV_PATH)
        logger.info(f"CSV backed up to: {BACKUP_CSV_PATH}")

def restore_csv_backup():
    """Restore from the backup file"""
    try:
        if os.path.exists(BACKUP_CSV_PATH):
            shutil.copy2(BACKUP_CSV_PATH, METADATA_CSV_PATH)
            logger.info("CSV backup restored")
        else:
            logger.warning("No backup file found to restore")
    except Exception as e:
        logger.error(f"Failed to restore backup: {e}")

def cleanup_backup():
    """Remove backup file after successful operation-- if needed"""
    try:
        if os.path.exists(BACKUP_CSV_PATH):
            os.remove(BACKUP_CSV_PATH)
            logger.info("Backup file cleaned up")
    except Exception as e:
        logger.warning(f"Could not cleanup backup file: {e}")
