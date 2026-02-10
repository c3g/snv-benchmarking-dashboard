# ============================================================================
# delete_handler.py
# ============================================================================
"""
Experiment deletion for SNV Benchmarking Dashboard.

Authorization:
- Admins can delete any experiment
- Users can delete only their own private experiments
- Public experiments without owner (legacy) require admin

Operation Order:
1. Verify experiment exists and user has permission
2. Delete from database and commit
3. Remove from CSV backup (archives to 000_deleted.csv)
4. Archive happy file to deleted/ folder
"""

import os
import shutil
import logging
from datetime import datetime
from config import DATA_FOLDER
from database import get_db_session
from models import Experiment, BenchmarkResult, OverallResult

logger = logging.getLogger(__name__)


# ============================================================================
# AUTHORIZATION
# ============================================================================

def can_delete_experiment(experiment, user_id, is_admin):
    """
    Check if user has permission to delete experiment.
    
    Rules:
    - Admins can delete any experiment
    - Users can delete their own experiments (owner_id matches)
    - Legacy public experiments (owner_id=NULL) require admin
    
    Args:
        experiment: Experiment object from database
        user_id: Current user's database ID
        is_admin: Whether current user is admin
        
    Returns:
        tuple: (allowed: bool, reason: str)
    """
    if is_admin:
        return True, "Admin access"
    
    if experiment.owner_id is None:
        return False, "Legacy experiments require admin to delete"
    
    if user_id is None:
        return False, "Authentication required"
    
    if experiment.owner_id == user_id:
        return True, "Owner access"
    
    return False, "Not authorized to delete this experiment"


# ============================================================================
# FILE ARCHIVING
# ============================================================================

def _create_archive_dir():
    """Create deleted folder if it doesn't exist"""
    deleted_folder = os.path.join(DATA_FOLDER, 'deleted')
    os.makedirs(deleted_folder, exist_ok=True)
    return deleted_folder


def _find_happy_file(experiment_id, hint_filename=None):
    """
    Find happy file for experiment.
    
    Args:
        experiment_id: Experiment ID
        hint_filename: Known filename from CSV backup (preferred)
        
    Returns:
        str: Full path to file or None
    """
    # Try hint filename first
    if hint_filename:
        hint_path = os.path.join(DATA_FOLDER, hint_filename)
        if os.path.exists(hint_path):
            return hint_path
    
    # Fall back to ID-based pattern matching
    try:
        for filename in os.listdir(DATA_FOLDER):
            if not filename.endswith('.csv') or filename.startswith('000_'):
                continue
            
            # Check various ID padding formats
            id_prefixes = [
                f"{experiment_id:03d}_"
            ]
            
            for prefix in id_prefixes:
                if filename.startswith(prefix):
                    return os.path.join(DATA_FOLDER, filename)
                    
    except Exception as e:
        logger.error(f"Error searching for happy file: {e}")
    
    return None


def _archive_happy_file(experiment_id, hint_filename=None):
    """
    Move experiment's happy file to deleted folder.
    Called AFTER successful database deletion.
    
    Args:
        experiment_id: ID of deleted experiment
        hint_filename: Known filename from CSV backup
    
    Returns:
        str: Archived filename or None
    """
    try:
        source_path = _find_happy_file(experiment_id, hint_filename)
        
        if not source_path:
            logger.warning(f"No happy file found for experiment {experiment_id}")
            return None
        
        filename = os.path.basename(source_path)
        deleted_folder = _create_archive_dir()
        
        archived_name = filename
        dest_path = os.path.join(deleted_folder, archived_name)
        
        shutil.move(source_path, dest_path)
        logger.info(f"Archived file: {filename} -> {archived_name}")
        return archived_name
        
    except Exception as e:
        logger.error(f"Failed to archive file for experiment {experiment_id}: {e}")
        return None


# ============================================================================
# DATABASE DELETION
# ============================================================================

def _delete_from_database(experiment_id, session):
    """
    Delete experiment and related records from database.
    Order: BenchmarkResult -> OverallResult -> Experiment (foreign key constraints)
    
    Returns:
        dict: Deletion counts
    """
    benchmark_count = session.query(BenchmarkResult).filter(
        BenchmarkResult.experiment_id == experiment_id
    ).delete()
    
    overall_count = session.query(OverallResult).filter(
        OverallResult.experiment_id == experiment_id
    ).delete()
    
    exp_count = session.query(Experiment).filter(
        Experiment.id == experiment_id
    ).delete()
    
    return {
        "benchmark_results": benchmark_count,
        "overall_results": overall_count,
        "experiments": exp_count
    }


# ============================================================================
# MAIN DELETE FUNCTIONS
# ============================================================================

def delete_experiment(experiment_id, user_id=None, username=None, is_admin=False):
    """
    Delete experiment with ownership verification.
    
    Authorization:
    - Admins can delete any experiment
    - Users can delete only their own experiments
    
    Operation Order (transaction safe):
    1. Verify experiment exists
    2. Check delete permission
    3. Delete from database and commit
    4. Remove from CSV backup (archives metadata to 000_deleted.csv)
    5. Archive happy file to deleted/ folder
    
    Args:
        experiment_id: ID of experiment to delete
        user_id: Current user's database ID (for ownership check)
        username: Username for logging
        is_admin: Admin status
        
    Returns:
        dict: Result with success status and message
    """
    exp_name = None
    hint_filename = None
    
    try:
        logger.info(f"Delete requested by {username} (user_id={user_id}, admin={is_admin}) "
                   f"for experiment {experiment_id}")
        
        with get_db_session() as session:
            # Step 1: Verify experiment exists
            experiment = session.query(Experiment).filter(
                Experiment.id == experiment_id
            ).first()
            
            if not experiment:
                return {"success": False, "error": f"Experiment {experiment_id} not found"}
            
            exp_name = experiment.name
            
            # Step 2: Check authorization
            allowed, reason = can_delete_experiment(experiment, user_id, is_admin)
            if not allowed:
                logger.warning(f"Delete denied for {username}: {reason}")
                return {"success": False, "error": reason, "unauthorized": True}
            
            logger.info(f"Deleting: {exp_name} (ID: {experiment_id}) - {reason}")
            
            # Step 3: Delete from database
            counts = _delete_from_database(experiment_id, session)
            session.commit()
            
            logger.info(f"DB deletion complete - {counts['benchmark_results']} benchmark results, "
                       f"{counts['overall_results']} overall results")
        
        # Step 4: Remove from CSV backup (outside DB transaction)
        # This also archives metadata to 000_deleted.csv
        try:
            from csv_backup import remove_from_backup, get_filename_from_backup
            hint_filename = get_filename_from_backup(experiment_id)
            remove_from_backup(experiment_id, deleted_by=username)
        except Exception as e:
            logger.warning(f"CSV backup removal failed (non-critical): {e}")
        
        # Step 5: Archive happy file (after all DB operations succeed)
        archived_file = _archive_happy_file(experiment_id, hint_filename)
        
        return {
            "success": True,
            "message": f"Deleted '{exp_name}' (ID: {experiment_id})",
            "archived_file": archived_file,
            "deleted_counts": counts
        }
        
    except Exception as e:
        logger.error(f"Delete failed for experiment {experiment_id}: {e}")
        import traceback
        logger.error(traceback.format_exc())
        return {"success": False, "error": f"Delete failed: {str(e)}"}
