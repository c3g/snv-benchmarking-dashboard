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
from datetime import datetime
from sqlalchemy import text
from config import METADATA_CSV_PATH, DATA_FOLDER
from database import get_db_session, force_reconnect, drop_all_data
from populate_metadata import populate_database_from_csv

logger = logging.getLogger(__name__)

BACKUP_CSV_PATH = f"{METADATA_CSV_PATH}.backup"

def create_archive_dir():
    """Create deleted folder if it doesn't exist"""
    deleted_folder = os.path.join(DATA_FOLDER, 'deleted')
    os.makedirs(deleted_folder, exist_ok=True)
    return deleted_folder

def save_metadata():
    """Move current metadata CSV to deleted folder with timestamp"""
    if os.path.exists(METADATA_CSV_PATH):
        deleted_folder = create_archive_dir()
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        archived_name = f"metadata_backup_{timestamp}.csv"
        archived_path = os.path.join(deleted_folder, archived_name)
        shutil.copy2(METADATA_CSV_PATH, archived_path)
        logger.info(f"Metadata archived to: {archived_name}")

def backup_metadata():
    """Create a backup metadata file"""
    if os.path.exists(METADATA_CSV_PATH):
        shutil.copy2(METADATA_CSV_PATH, BACKUP_CSV_PATH)
        logger.info(f"CSV backed up to: {BACKUP_CSV_PATH}")

def restore_metadata():
    """Restore from the backup file"""
    try:
        if os.path.exists(BACKUP_CSV_PATH):
            shutil.copy2(BACKUP_CSV_PATH, METADATA_CSV_PATH)
            logger.info("CSV backup restored")
        else:
            logger.warning("No backup file found to restore")
    except Exception as e:
        logger.error(f"Failed to restore backup: {e}")

def archive_happy_file(happy_file):
    """Move happy file to deleted folder"""
    if not happy_file:
        return
        
    happy_file_path = os.path.join(DATA_FOLDER, happy_file)
    if not os.path.exists(happy_file_path):
        logger.info(f"Happy file not found for archiving: {happy_file}")
        return
        
    # Create deleted folder
    deleted_folder = os.path.join(DATA_FOLDER, "deleted_experiments")
    os.makedirs(deleted_folder, exist_ok=True)
    
    # Move file with timestamp to avoid conflicts
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    name_parts = os.path.splitext(happy_file)
    archived_name = f"{name_parts[0]}_{timestamp}{name_parts[1]}"
    deleted_file_path = os.path.join(deleted_folder, archived_name)
    
    shutil.move(happy_file_path, deleted_file_path)
    logger.info(f"Archived file: {happy_file} -> {archived_name}")

def delete_experiment(experiment_id):
    """Delete experiment by removing from CSV and rebuilding database"""
    try:
        logger.info(f"Starting deletion of experiment {experiment_id}")
        
        # 1. Create backup
        backup_metadata()
        
        # 2. Load current CSV
        if not os.path.exists(METADATA_CSV_PATH):
            return {"success": False, "error": "Metadata CSV not found"}
            
        df = pd.read_csv(METADATA_CSV_PATH)
        
        # 3. Find and remove the experiment
        if experiment_id not in df['ID'].values:
            return {"success": False, "error": f"Experiment {experiment_id} not found"}
        
        target_row = df[df['ID'] == experiment_id].iloc[0]
        happy_file = target_row['file_name'] if 'file_name' in target_row and pd.notna(target_row['file_name']) else None
        
        # Archive current metadata CSV
        save_metadata()
        
        # 4. Remove from dataframe
        df = df[df['ID'] != experiment_id].copy()
        
        # 5. Save updated CSV
        df.to_csv(METADATA_CSV_PATH, index=False)
        logger.info(f"Updated metadata CSV, removed experiment {experiment_id}")
        
        # 6. Move happy file to deleted folder
        if happy_file:
            archive_happy_file(happy_file)
        
        # 7. Rebuild database from CSV
        logger.info("Rebuilding database...")
        drop_all_data()
        success = populate_database_from_csv()
        
        if success:
            logger.info(f"Successfully deleted experiment {experiment_id}")
            return {"success": True, "message": f"Experiment {experiment_id} deleted and database rebuilt"}
        else:
            restore_metadata()
            return {"success": False, "error": "Database rebuild failed, changes reverted"}
            
    except Exception as e:
        logger.error(f"Delete failed for experiment {experiment_id}: {e}")
        restore_metadata()
        return {"success": False, "error": f"Delete failed: {str(e)}"}

def delete_multiple_experiments(experiment_ids):
    """Delete multiple experiments in one operation"""
    try:
        logger.info(f"Deleting {len(experiment_ids)} experiments: {experiment_ids}")
        
        # 1. Create backup
        backup_metadata()
        
        # 2. Load CSV
        if not os.path.exists(METADATA_CSV_PATH):
            return {"success": False, "error": "Metadata CSV not found"}
            
        df = pd.read_csv(METADATA_CSV_PATH)
        
        # 3. Check all experiments exist
        missing = [eid for eid in experiment_ids if eid not in df['ID'].values]
        if missing:
            return {"success": False, "error": f"Experiments not found: {missing}"}
        
        # 4. Get files to archive before removal
        target_rows = df[df['ID'].isin(experiment_ids)]
        happy_files = target_rows['file_name'].dropna().tolist()
        experiment_names = target_rows['name'].tolist()
        
        # Archive current metadata CSV
        save_metadata()
        
        # 5. Remove from CSV and save
        df = df[~df['ID'].isin(experiment_ids)].copy()
        df.to_csv(METADATA_CSV_PATH, index=False)
        logger.info(f"Updated CSV - removed {len(experiment_ids)} experiments")
        
        # 6. Archive files
        for happy_file in happy_files:
            archive_happy_file(happy_file)
        
        # 7. Rebuild database
        logger.info("Rebuilding database...")
        success = populate_database_from_csv()
        
        if success:
            logger.info(f"Successfully deleted {len(experiment_ids)} experiments")
            return {"success": True, "message": f"Deleted {len(experiment_ids)} experiments successfully"}
        else:
            restore_metadata()
            return {"success": False, "error": "Database rebuild failed - changes reverted"}
            
    except Exception as e:
        logger.error(f"Multiple delete failed: {e}")
        restore_metadata()
        return {"success": False, "error": f"Delete failed: {str(e)}"}