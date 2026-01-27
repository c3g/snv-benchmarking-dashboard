# ============================================================================
# upload_handler.py
# ============================================================================
"""
Direct-to-database upload handler for SNV Benchmarking Dashboard.

Key features:
- Writes directly to database (CSV is backup only)
- Ownership tracking via owner_id
- Visibility control via is_public flag

Visibility Rules:
- Admins can upload public or private experiments
- Regular users can only upload private experiments
- Private experiments visible only to owner + admins

"""

import json
import pandas as pd
import os
import shutil
import tempfile
import logging
from datetime import datetime
from pathlib import Path

from config import DATA_FOLDER
from direct_db_population import create_experiment_direct
from database import get_db_session
from models import Experiment
from sqlalchemy import func

logger = logging.getLogger(__name__)

# ============================================================================
# VALIATION CONSTANTS
# ============================================================================

# Required fields for upload validation
REQUIRED_METADATA = [
    'exp_name', 'technology', 'platform_name', 'platform_type',
    'caller_name', 'caller_version', 'caller_type', 'mean_coverage', 'truth_set_name'
]

# Required columns in hap.py CSV
REQUIRED_COLS = ['Type', 'Subtype', 'Subset', 'METRIC.Recall', 'METRIC.Precision', 'METRIC.F1_Score']

# import enum mappings for validation
from enum_mappings import VALID_TECHNOLOGIES, VALID_CALLERS 
# ============================================================================
# FILE VALIDATION
# ============================================================================

def validate_happy_file(file_path):
    """
    Validate uploaded hap.py CSV file format and content.
    
    Args:
        file_path: Path to uploaded CSV file
        
    Returns:
        tuple: (is_valid: bool, message: str)
    """
    try:
        if not os.path.exists(file_path):
            logger.error(f"File not found: {file_path}")
            return False, "File not found"
            
        df = pd.read_csv(file_path)
        
        # Check required columns
        missing = [col for col in REQUIRED_COLS if col not in df.columns]
        if missing:
            return False, f"Missing columns: {', '.join(missing)}"
        
        # Check for data
        if len(df) == 0:
            return False, "File is empty"
            
        # Check for expected variant types
        found_types = df['Type'].unique()
        if not any(t in ['SNP', 'INDEL'] for t in found_types):
            return False, f"No SNP/INDEL data found. Found: {list(found_types)}"
        
        logger.info(f"File validation successful: {len(df)} rows found")
        return True, f"Valid hap.py file with {len(df)} rows"
        
    except Exception as e:
        logger.error(f"File validation failed for {file_path}: {str(e)}")
        return False, f"Error reading file: {str(e)}"

def validate_metadata(metadata):
    """
    Validate required metadata fields and enum values.
    
    Args:
        metadata: Dictionary from upload form
        
    Returns:
        tuple: (is_valid: bool, message: str)
    """
    # Check required fields
    for field in REQUIRED_METADATA:
        value = metadata.get(field, "")
        if not value or str(value).strip() == "":
            logger.error(f"Metadata validation failed: missing required field '{field}'")
            return False, f"Required field '{field}' is missing"
    
    # Validate technology enum
    if metadata['technology'].upper() not in VALID_TECHNOLOGIES:
        logger.error(f"Metadata validation failed: invalid technology '{metadata['technology']}'")
        return False, f"Invalid technology: {metadata['technology']}"
    
    # Validate caller enum
    if metadata['caller_name'].upper() not in VALID_CALLERS:
        logger.error(f"Metadata validation failed: invalid caller '{metadata['caller_name']}'")
        return False, f"Invalid caller: {metadata['caller_name']}"
    
    logger.info("Metadata validation successful")
    return True, "Metadata is valid"

# ============================================================================
# FILENAME GENERATION
# ============================================================================

def generate_filename(metadata, experiment_id, is_public):
    """
    Generate standardized filename for hap.py file.
    
    Format: {ID}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv
    
    Args:
        metadata: Experiment metadata dict
        experiment_id: Assigned experiment ID
        is_public: Whether experiment is public (affects ID padding)
        
    Returns:
        str: Generated filename
    """
    def strip_value(value): # to remove spaces from the filename 
        if value is None or pd.isna(value): 
            return ""
        return str(value).strip()
    
    try:
        # Extract sample name (first part before underscore)
        sample = strip_value(str(metadata.get('exp_name', '')).split('_')[0])
        
        # Clean all metadata components
        technology = strip_value(metadata.get('technology', '')).lower().replace(" ", "")
        platform = strip_value(metadata.get('platform_name', '')).lower().replace(" ", "")
        caller = strip_value(metadata.get('caller_name', '')).lower().replace(" ", "")
        truthset = strip_value(metadata.get('truth_set_name', '')).lower().replace(" ", "")
        
        # Build filename - 3-digit padding for all public and private records
        filename = f"{experiment_id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv"
        return filename
    
    except Exception as e:
        # Fallback: timestamp-based filename
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        sample = metadata.get('exp_name', 'unknown').split('_')[0]
        logger.warning(f"Filename generation failed, using timestamp fallback: {e}")
        return f"{experiment_id}_{timestamp}_{sample}.csv"

# ============================================================================
# METADATA PREPARATION
# ============================================================================

def prepare_metadata_dict(metadata):
    """
    Convert upload form metadata to database-ready format.
    
    Args:
        metadata: Raw metadata from upload form
        
    Returns:
        dict: Cleaned and formatted metadata for database insertion
    """
    def safe_float(value):
        try:
            return float(value) if value and str(value).strip() else None
        except:
            return None
    
    def safe_bool(value):
        if isinstance(value, bool):
            return value
        return str(value).lower() == 'true'
    
    def safe_upper(value):
        if not value or str(value).strip() == '':
            return ''
        return str(value).strip().upper()
    
    # Determine visibility
    is_public = metadata.get('experiment_visibility', 'private') == 'public'
    
    return {
        # Basic info
        'exp_name': metadata['exp_name'],
        'description': metadata.get('description', ''),
        
        # Sequencing technology
        'technology': safe_upper(metadata['technology']),
        'target': safe_upper(metadata.get('target', 'wgs')),
        'platform_name': metadata['platform_name'],
        'platform_type': safe_upper(metadata.get('platform_type', '')),
        'platform_version': metadata.get('platform_version', ''),
        
        # Chemistry
        'chemistry_name': metadata.get('chemistry_name', ''),
        'chemistry_version': metadata.get('chemistry_version', ''),
        
        # Variant caller
        'caller_name': safe_upper(metadata['caller_name']),
        'caller_type': safe_upper(metadata.get('caller_type', '')),
        'caller_version': metadata.get('caller_version', ''),
        'caller_model': metadata.get('caller_model', ''),
        
        # Aligner
        'aligner_name': metadata.get('aligner_name', ''),
        'aligner_version': metadata.get('aligner_version', ''),
        
        # Truth set
        'truth_set_name': safe_upper(metadata.get('truth_set_name', '')),
        'truth_set_sample': safe_upper(metadata.get('truth_set_sample', 'hg002')),
        'truth_set_version': metadata.get('truth_set_version', ''),
        'truth_set_reference': safe_upper(metadata.get('truth_set_reference', '')),
        
        # Variant info
        'variant_type': safe_upper(metadata.get('variant_type', 'snp+indel')),
        'variant_size': safe_upper(metadata.get('variant_size', '')),
        'variant_origin': safe_upper(metadata.get('variant_origin', '')),
        'is_phased': safe_bool(metadata.get('is_phased', 'false')),
        
        # Benchmark tool
        'benchmark_tool_name': safe_upper(metadata.get('benchmark_tool_name', 'hap.py')),
        'benchmark_tool_version': metadata.get('benchmark_tool_version', ''),
        
        # Quality metrics
        'mean_coverage': safe_float(metadata.get('mean_coverage')),
        'read_length': safe_float(metadata.get('read_length')),
        'mean_insert_size': safe_float(metadata.get('mean_insert_size')),
        'mean_read_length': metadata.get('mean_read_length', ''),
        
        # Timestamps
        'created_at': datetime.now().strftime('%Y-%m-%d'),
        
        # Ownership and visibility
        'is_public': is_public,
        'owner_username': metadata.get('owner_username'),
        'owner_id': metadata.get('owner_id'),
    }

# ============================================================================
# MAIN UPLOAD PROCESSING
# ============================================================================

def process_upload_direct(temp_file_path, metadata_json_string):
    """
    Process upload directly to database.
    
    Workflow:
    1. Parse and validate metadata
    2. Validate hap.py file
    3. Determine visibility and generate appropriate ID
    4. Generate filename and save file
    5. Create experiment in database
    6. Parse and load hap.py results
    
    Args:
        temp_file_path: Path to uploaded temp file
        metadata_json_string: JSON metadata from form
        
    Returns:
        dict: {"success": bool, "message": str, "filename": str, "experiment_id": int}
    """
    temp_work_dir = None
    
    try:
        logger.info("Starting direct database upload...")
        
        # Parse metadata JSON
        try:
            metadata = json.loads(metadata_json_string)
        except json.JSONDecodeError as e:
            return {"success": False, "message": f"Invalid JSON: {str(e)}", "filename": None}
        
        logger.info(f"Processing experiment: {metadata.get('exp_name', 'Unknown')}")
        
        # STEP 1: Validate file
        is_valid_file, file_msg = validate_happy_file(temp_file_path)
        if not is_valid_file:
            return {"success": False, "message": f"File validation failed: {file_msg}", "filename": None}
        
        # STEP 2: Validate metadata
        is_valid_meta, meta_msg = validate_metadata(metadata)
        if not is_valid_meta:
            return {"success": False, "message": f"Metadata validation failed: {meta_msg}", "filename": None}
        
        # STEP 3: Prepare metadata and determine visibility
        db_metadata = prepare_metadata_dict(metadata)
        is_public = db_metadata['is_public']
        
        # Validate owner_id for private uploads
        if not is_public and not db_metadata.get('owner_id'):
            logger.warning("Private upload without owner_id - this shouldn't happen")
            # Could reject here, but let's allow it with just username tracking
        
        logger.info(f"Upload visibility: {'PUBLIC' if is_public else 'PRIVATE'}")
        
        # STEP 4: database auto-generates ID
        experiment_id = None  # assigned by database
        logger.info(f"Visibility: {'PUBLIC' if is_public else 'PRIVATE'} (ID will be auto-assigned)")
        
        # STEP 5: Setup working directory
        temp_work_dir = tempfile.mkdtemp(prefix="upload_")
        logger.debug(f"Working in {temp_work_dir}")
        
        # STEP 6: Copy file temporarily (filename generated after DB assigns ID)
        temp_filename = f"temp_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
        temp_file_copy = os.path.join(temp_work_dir, temp_filename)
        shutil.copy2(temp_file_path, temp_file_copy)
        logger.debug("File copied to working directory")
        
        # STEP 7: Create experiment in database (ID auto-assigned)
        logger.debug("Creating experiment in database...")
        db_result = create_experiment_direct(
            metadata=db_metadata,
            experiment_id=None,  # Let DB auto-generate
            filename=None  # Will add file after we get ID
        )
        
        if not db_result["success"]:
            return {
                "success": False,
                "message": f"Database creation failed: {db_result['message']}",
                "filename": None
            }
        
        # Get the auto-assigned ID
        experiment_id = db_result["experiment_id"]
        logger.info(f"Database assigned experiment ID: {experiment_id}")
        
        # STEP 8: Generate final filename with real ID and save file
        filename = generate_filename(db_metadata, experiment_id, is_public)
        os.makedirs(DATA_FOLDER, exist_ok=True)
        final_file_path = os.path.join(DATA_FOLDER, filename)
        shutil.copy2(temp_file_copy, final_file_path)
        logger.info(f"File saved to: {final_file_path}")
        
# STEP 9: Parse hap.py results into database
        from happy_parser import parse_happy_csv
        from database import get_db_session
        with get_db_session() as session:
            parse_result = parse_happy_csv(filename, experiment_id, session)
            if not parse_result["success"]:
                logger.warning(f"Failed to parse hap.py: {parse_result.get('error')}")
        
        # STEP 10: Add to CSV backup
        try:
            from csv_backup import add_to_backup
            add_to_backup(experiment_id, db_metadata, filename)
        except Exception as e:
            logger.warning(f"CSV backup failed (non-critical): {e}")
        
        # Success
        visibility_label = "public" if is_public else "private"
        success_message = f"Upload successful! {visibility_label.capitalize()} experiment ID: {experiment_id}"
        logger.info(success_message)
        
        return {
            "success": True, 
            "message": success_message,
            "filename": filename,
            "experiment_id": experiment_id,
            "is_public": is_public
        }
        
    except Exception as e:
        error_msg = f"Upload failed: {str(e)}"
        logger.error(error_msg)
        import traceback
        logger.error(traceback.format_exc())
        return {"success": False, "message": error_msg, "filename": None}
        
    finally:
        # Cleanup working directory
        if temp_work_dir and os.path.exists(temp_work_dir):
            try:
                shutil.rmtree(temp_work_dir)
                logger.debug("Cleanup: Working directory removed")
            except:
                logger.warning("Could not cleanup working directory")

# ============================================================================
# R/PYTHON INTERFACE
# ============================================================================

def upload_experiment_v2(file_path, metadata_json, username=None, is_admin=False):
    """
    Upload experiment to database.
    
    Access control:
    - Admins can upload public or private experiments
    - Regular users can only upload private experiments
    
    Args:
        file_path: Path to uploaded file
        metadata_json: JSON string with experiment metadata
        username: Username for logging
        is_admin: Admin status from OIDC
    
    Returns:
        dict: Result with success status, message, filename, and experiment_id
    """
    logger.info(f"Upload requested by: {username} (admin: {is_admin})")
    
    # Parse metadata to check visibility
    try:
        metadata = json.loads(metadata_json)
        is_public = metadata.get('experiment_visibility', 'private') == 'public'
        
        # Non-admins can only upload private experiments
        if is_public and not is_admin:
            logger.warning(f"Non-admin {username} attempted public upload - converting to private")
            metadata['experiment_visibility'] = 'private'
            metadata_json = json.dumps(metadata)
            
    except json.JSONDecodeError:
        pass  # Will fail in process_upload_direct with proper error
    
    return process_upload_direct(file_path, metadata_json)