import datetime
import pandas as pd
import os
import shutil
from config import METADATA_CSV_PATH, DATA_FOLDER
from database import drop_all_data
from populate_metadata import populate_database_from_csv

def delete_experiment(experiment_id):
    """
    Delete experiment by removing from CSV and rebuilding database
    """
    try:
        # 1. Backup CSV
        backup_csv()
        
        # 2. Load current CSV
        df = pd.read_csv(METADATA_CSV_PATH)
        
        # 3. Find and remove the experiment
        if experiment_id not in df['ID'].values:
            return {"success": False, "error": f"Experiment {experiment_id} not found"}
        
        target_row = df[df['ID'] == experiment_id].iloc[0]
        happy_file = target_row['file_name'] if 'file_name' in target_row else None
        
        # 4. Remove from dataframe
        df = df[df['ID'] != experiment_id].copy()
        
        # 5. Reindex IDs (shift all higher IDs down)
        df.loc[df['ID'] > experiment_id, 'ID'] = df.loc[df['ID'] > experiment_id, 'ID'] - 1
        
        # 6. Save updated CSV
        df.to_csv(METADATA_CSV_PATH, index=False)
        
        # 7. Delete happy file
        if happy_file:
            happy_file_path = os.path.join(DATA_FOLDER, happy_file)
            if os.path.exists(happy_file_path):
                os.remove(happy_file_path)
        
        # 8. Rebuild database from CSV
        drop_all_data()
        success = populate_database_from_csv()
        
        if success:
            return {"success": True, "message": f"Experiment {experiment_id} deleted and database rebuilt"}
        else:
            # Restore backup if failed
            restore_csv_backup()
            return {"success": False, "error": "Database rebuild failed, changes reverted"}
            
    except Exception as e:
        restore_csv_backup()
        return {"success": False, "error": f"Delete failed: {str(e)}"}

def backup_csv():
    """Just one backup file - overwrite previous backup"""
    if os.path.exists(METADATA_CSV_PATH):
        date_str = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_path = f"{METADATA_CSV_PATH}.backup_{date_str}"
        shutil.copy2(METADATA_CSV_PATH, backup_path)

def restore_csv_backup():
    """Restore the backup"""
    date_str = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup_path = f"{METADATA_CSV_PATH}.backup_{date_str}"
    if os.path.exists(backup_path):
        shutil.copy2(backup_path, METADATA_CSV_PATH)