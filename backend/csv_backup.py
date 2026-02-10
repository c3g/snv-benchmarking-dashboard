# ============================================================================
# csv_backup.py
# ============================================================================
"""
CSV backup for SNV Benchmarking Dashboard.

Maintains metadata CSV as backup reference (database is source of truth).
Deleted experiments are archived to 000_deleted.csv in DATA_FOLDER.
"""

import pandas as pd
import os
import logging
from datetime import datetime
from config import METADATA_CSV_PATH, DATA_FOLDER, DELETED_CSV_PATH

logger = logging.getLogger(__name__)

# Main backup CSV columns
CSV_COLUMNS = [
    'ID', 'name', 'technology', 'target', 'platform_name', 'platform_type',
    'platform_version', 'chemistry_name', 'caller_name', 'caller_type',
    'caller_version', 'caller_model', 'aligner_name', 'aligner_version',
    'truth_set_name', 'truth_set_sample', 'truth_set_version', 'truth_set_reference',
    'variant_type', 'variant_size', 'variant_origin', 'is_phased',
    'benchmark_tool_name', 'benchmark_tool_version', 'mean_coverage',
    'read_length', 'mean_insert_size', 'mean_read_length', 'file_name',
    'file_path', 'created_at', 'is_public', 'owner_username'
]

# Deleted archive has additional columns for audit trail
DELETED_CSV_COLUMNS = CSV_COLUMNS + ['deleted_at', 'deleted_by']


def ensure_backup_exists():
    """Create backup CSV with headers if it doesn't exist"""
    if not os.path.exists(METADATA_CSV_PATH):
        df = pd.DataFrame(columns=CSV_COLUMNS)
        df.to_csv(METADATA_CSV_PATH, index=False)
        logger.info(f"Created new backup CSV: {METADATA_CSV_PATH}")


def ensure_deleted_csv_exists():
    """Create deleted archive CSV with headers if it doesn't exist"""
    if not os.path.exists(DELETED_CSV_PATH):
        df = pd.DataFrame(columns=DELETED_CSV_COLUMNS)
        df.to_csv(DELETED_CSV_PATH, index=False)
        logger.info(f"Created deleted archive CSV: {DELETED_CSV_PATH}")


def add_to_backup(experiment_id, metadata, filename=None):
    """
    Add experiment to CSV backup after successful upload.
    
    Args:
        experiment_id: Database ID of the experiment
        metadata: Dictionary with experiment metadata
        filename: Name of the happy file
        
    Returns:
        dict: Result with success status
    """
    try:
        ensure_backup_exists()
        
        df = pd.read_csv(METADATA_CSV_PATH)
        
        # Remove existing entry if present (for updates)
        if experiment_id in df['ID'].values:
            logger.info(f"Updating existing backup entry for experiment {experiment_id}")
            df = df[df['ID'] != experiment_id]
        
        new_row = {
            'ID': experiment_id,
            'name': metadata.get('exp_name', ''),
            'technology': metadata.get('technology', ''),
            'target': metadata.get('target', 'WGS'),
            'platform_name': metadata.get('platform_name', ''),
            'platform_type': metadata.get('platform_type', ''),
            'platform_version': metadata.get('platform_version', ''),
            'chemistry_name': metadata.get('chemistry_name', ''),
            'caller_name': metadata.get('caller_name', ''),
            'caller_type': metadata.get('caller_type', ''),
            'caller_version': metadata.get('caller_version', ''),
            'caller_model': metadata.get('caller_model', ''),
            'aligner_name': metadata.get('aligner_name', ''),
            'aligner_version': metadata.get('aligner_version', ''),
            'truth_set_name': metadata.get('truth_set_name', ''),
            'truth_set_sample': metadata.get('truth_set_sample', 'HG002'),
            'truth_set_version': metadata.get('truth_set_version', ''),
            'truth_set_reference': metadata.get('truth_set_reference', 'GRCh38'),
            'variant_type': metadata.get('variant_type', 'SNP+INDEL'),
            'variant_size': metadata.get('variant_size', 'small'),
            'variant_origin': metadata.get('variant_origin', 'Germline'),
            'is_phased': metadata.get('is_phased', False),
            'benchmark_tool_name': metadata.get('benchmark_tool_name', 'hap.py'),
            'benchmark_tool_version': metadata.get('benchmark_tool_version', ''),
            'mean_coverage': metadata.get('mean_coverage', ''),
            'read_length': metadata.get('read_length', ''),
            'mean_insert_size': metadata.get('mean_insert_size', ''),
            'mean_read_length': metadata.get('mean_read_length', ''),
            'file_name': filename or '',
            'file_path': '',
            'created_at': metadata.get('created_at', datetime.now().strftime('%Y-%m-%d')),
            'is_public': metadata.get('is_public', True),
            'owner_username': metadata.get('owner_username', '')
        }
        
        new_df = pd.DataFrame([new_row])
        df = pd.concat([df, new_df], ignore_index=True)
        df = df.sort_values('ID').reset_index(drop=True)
        
        df.to_csv(METADATA_CSV_PATH, index=False)
        logger.info(f"Added experiment {experiment_id} to backup CSV")
        
        return {"success": True, "message": f"Backup updated for experiment {experiment_id}"}
        
    except Exception as e:
        logger.error(f"Failed to add experiment {experiment_id} to backup: {e}")
        return {"success": False, "error": str(e)}


def get_filename_from_backup(experiment_id):
    """
    Get the happy file filename for an experiment from backup CSV.
    
    Args:
        experiment_id: Experiment ID
        
    Returns:
        str: Filename or None if not found
    """
    try:
        if not os.path.exists(METADATA_CSV_PATH):
            return None
        
        df = pd.read_csv(METADATA_CSV_PATH)
        
        if experiment_id not in df['ID'].values:
            return None
        
        row = df[df['ID'] == experiment_id].iloc[0]
        filename = row.get('file_name')
        
        if pd.isna(filename) or str(filename).strip() == '':
            return None
        
        return str(filename).strip()
        
    except Exception as e:
        logger.warning(f"Could not get filename from backup for {experiment_id}: {e}")
        return None


def remove_from_backup(experiment_id, deleted_by=None):
    """
    Remove experiment from CSV backup and archive to 000_deleted.csv.
    
    Args:
        experiment_id: ID of experiment to remove
        deleted_by: Username who performed deletion (for audit)
        
    Returns:
        dict: Result with success status
    """
    try:
        if not os.path.exists(METADATA_CSV_PATH):
            return {"success": True, "message": "No backup file exists"}
        
        df = pd.read_csv(METADATA_CSV_PATH)
        
        if experiment_id not in df['ID'].values:
            return {"success": True, "message": "Experiment not in backup"}
        
        # Archive before removing
        deleted_row = df[df['ID'] == experiment_id]
        _archive_deleted_row(deleted_row, deleted_by)
        
        # Remove from main backup
        df = df[df['ID'] != experiment_id]
        df.to_csv(METADATA_CSV_PATH, index=False)
        
        logger.info(f"Removed experiment {experiment_id} from backup CSV")
        return {"success": True, "message": f"Removed experiment {experiment_id} from backup"}
        
    except Exception as e:
        logger.error(f"Failed to remove experiment {experiment_id} from backup: {e}")
        return {"success": False, "error": str(e)}


def _archive_deleted_row(row_df, deleted_by=None):
    """
    Archive deleted experiment row to 000_deleted.csv in DATA_FOLDER.
    
    Args:
        row_df: DataFrame row of deleted experiment
        deleted_by: Username who performed deletion
    """
    try:
        ensure_deleted_csv_exists()
        
        row_df = row_df.copy()
        row_df['deleted_at'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        row_df['deleted_by'] = deleted_by or 'unknown'
        
        if os.path.exists(DELETED_CSV_PATH):
            existing = pd.read_csv(DELETED_CSV_PATH)
            combined = pd.concat([existing, row_df], ignore_index=True)
            combined.to_csv(DELETED_CSV_PATH, index=False)
        else:
            row_df.to_csv(DELETED_CSV_PATH, index=False)
        
        exp_id = row_df['ID'].iloc[0] if len(row_df) > 0 else 'unknown'
        logger.info(f"Archived experiment {exp_id} to 000_deleted.csv")
        
    except Exception as e:
        logger.warning(f"Failed to archive deleted row: {e}")


def update_visibility_in_backup(experiment_id, is_public):
    """
    Update visibility field in backup CSV.
    Called when admin toggles experiment visibility.
    
    Args:
        experiment_id: Experiment ID
        is_public: New visibility status
        
    Returns:
        dict: Result with success status
    """
    try:
        if not os.path.exists(METADATA_CSV_PATH):
            return {"success": False, "error": "Backup CSV does not exist"}
        
        df = pd.read_csv(METADATA_CSV_PATH)
        
        if experiment_id not in df['ID'].values:
            return {"success": False, "error": f"Experiment {experiment_id} not in backup"}
        
        df.loc[df['ID'] == experiment_id, 'is_public'] = is_public
        df.to_csv(METADATA_CSV_PATH, index=False)
        
        logger.info(f"Updated visibility for experiment {experiment_id} in backup")
        return {"success": True}
        
    except Exception as e:
        logger.error(f"Failed to update visibility in backup: {e}")
        return {"success": False, "error": str(e)}


def get_deleted_experiments():
    """
    Get all deleted experiments from archive.
    
    Returns:
        pandas.DataFrame: Deleted experiments or empty DataFrame
    """
    try:
        if not os.path.exists(DELETED_CSV_PATH):
            return pd.DataFrame(columns=DELETED_CSV_COLUMNS)
        
        return pd.read_csv(DELETED_CSV_PATH)
        
    except Exception as e:
        logger.error(f"Failed to read deleted experiments: {e}")
        return pd.DataFrame(columns=DELETED_CSV_COLUMNS)