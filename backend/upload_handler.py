# ============================================================================
# upload_handler_v2.py
# ============================================================================
"""
Direct-to-database upload handler for SNV Benchmarking Dashboard.

Key changes from v1:
- Writes directly to database (bypasses CSV)
- CSV is now optional/deprecated
- Database is the source of truth
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
from authorization import require_admin

logger = logging.getLogger(__name__)

REQUIRED_METADATA = ['exp_name', 'technology', 'platform_name', 'platform_type', 'caller_name', 'caller_version', 'caller_type', 'mean_coverage', 'truth_set_name']
REQUIRED_COLS = ['Type', 'Subtype', 'Subset', 'METRIC.Recall', 'METRIC.Precision', 'METRIC.F1_Score']

# ============================================================================
# FILE VALIDATION
# ============================================================================

def validate_happy_file(file_path):
    """Validate uploaded hap.py CSV file"""
    try:
        if not os.path.exists(file_path):
            logger.error(f"File not found: {file_path}")
            return False, "File not found"
            
        df = pd.read_csv(file_path)
        
        missing = [col for col in REQUIRED_COLS if col not in df.columns]
        if missing:
            return False, f"Missing columns: {', '.join(missing)}"
        
        if len(df) == 0:
            return False, "File is empty"
            
        found_types = df['Type'].unique()
        if not any(t in ['SNP', 'INDEL'] for t in found_types):
            return False, f"No SNP/INDEL data found. Found: {list(found_types)}"
        
        logger.info(f"File validation successful: {len(df)} rows found")
        return True, f"Valid hap.py file with {len(df)} rows"
        
    except Exception as e:
        logger.error(f"File validation failed for {file_path}: {str(e)}")
        return False, f"Error reading file: {str(e)}"

def validate_metadata(metadata):
    """Validate required metadata fields"""
    for field in REQUIRED_METADATA:
        value = metadata.get(field, "")
        if not value or str(value).strip() == "":
            logger.error(f"Metadata validation failed: missing required field '{field}'")
            return False, f"Required field '{field}' is missing"
    
    if metadata['technology'].upper() not in ['ILLUMINA', 'PACBIO', 'ONT', 'MGI', '10X']:
        logger.error(f"Metadata validation failed: invalid technology '{metadata['technology']}'")
        return False, f"Invalid technology: {metadata['technology']}"
        
    if metadata['caller_name'].upper() not in ['DEEPVARIANT', 'GATK', 'CLAIR3', 'DRAGEN', 
                                                'GATK3', 'GATK4', 'LONGRANGER', 'MEGABOLT', 
                                                'NANOCALLER', 'PARABRICK', 'PEPPER']:
        logger.error(f"Metadata validation failed: invalid caller '{metadata['caller_name']}'")
        return False, f"Invalid caller: {metadata['caller_name']}"
    
    logger.info("Metadata validation successful")
    return True, "Metadata is valid"

# ============================================================================
# FILE PROCESSING
# ============================================================================

def get_next_experiment_id():
    """Get next experiment ID from database"""
    from database import get_db_session
    from models import Experiment
    from sqlalchemy import func
    
    try:
        with get_db_session() as session:
            max_id = session.query(func.max(Experiment.id)).scalar()
            next_id = (max_id or 0) + 1
            logger.info(f"Next experiment ID: {next_id}")
            return next_id
    except Exception as e:
        logger.error(f"Error getting next ID from database: {e}")
        return 1

def generate_filename(metadata, experiment_id):
    """Generate standardized filename"""
    def strip_value(value):
        if value is None or pd.isna(value): 
            return ""
        return str(value).strip()
    
    try:
        sample = strip_value(str(metadata.get('exp_name', '')).split('_')[0])
        technology = strip_value(metadata.get('technology', '')).lower()
        platform = strip_value(metadata.get('platform_name', '')).lower()
        caller = strip_value(metadata.get('caller_name', '')).lower()
        truthset = strip_value(metadata.get('truth_set_name', '')).lower()
        
        filename = f"{experiment_id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv"
        
        return filename
        
    except Exception as e:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        sample = metadata['exp_name'].split('_')[0]
        logger.warning(f"Filename generation failed, using timestamp fallback: {e}")
        return f"{timestamp}_{sample}_{metadata['technology']}_{metadata['caller_name']}.csv"

def prepare_metadata_dict(metadata):
    """Convert upload form metadata to database-ready format"""
    
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
    
    return {
        'exp_name': metadata['exp_name'],
        'description': metadata.get('description', ''),
        'technology': safe_upper(metadata['technology']),
        'target': safe_upper(metadata.get('target', 'wgs')),
        'platform_name': metadata['platform_name'],
        'platform_type': safe_upper(metadata.get('platform_type', '')),
        'platform_version': metadata.get('platform_version', ''),
        'chemistry_name': metadata.get('chemistry_name', ''),
        'caller_name': safe_upper(metadata['caller_name']),
        'caller_type': safe_upper(metadata.get('caller_type', '')),
        'caller_version': metadata.get('caller_version', ''),
        'caller_model': metadata.get('caller_model', ''),
        'aligner_name': metadata.get('aligner_name', ''),
        'aligner_version': metadata.get('aligner_version', ''),
        'truth_set_name': safe_upper(metadata.get('truth_set_name', '')),
        'truth_set_sample': safe_upper(metadata.get('truth_set_sample', 'hg002')),
        'truth_set_version': metadata.get('truth_set_version', ''),
        'truth_set_reference': safe_upper(metadata.get('truth_set_reference', '')),
        'variant_type': safe_upper(metadata.get('variant_type', 'snp+indel')),
        'variant_size': safe_upper(metadata.get('variant_size', '')),
        'variant_origin': safe_upper(metadata.get('variant_origin', '')),
        'is_phased': safe_bool(metadata.get('is_phased', 'false')),
        'benchmark_tool_name': safe_upper(metadata.get('benchmark_tool_name', 'hap.py')),
        'benchmark_tool_version': metadata.get('benchmark_tool_version', ''),
        'mean_coverage': safe_float(metadata.get('mean_coverage')),
        'read_length': safe_float(metadata.get('read_length')),
        'mean_insert_size': safe_float(metadata.get('mean_insert_size')),
        'mean_read_length': metadata.get('mean_read_length', ''),
        'created_at': datetime.now().strftime('%Y-%m-%d'),
        'is_public': metadata.get('experiment_visibility', 'public') == 'public', # public vs private
        'owner_username': metadata.get('owner_username'),  # From R session
        'owner_id': metadata.get('owner_id'),              # From R session
    }

# ============================================================================
# MAIN UPLOAD PROCESSING
# ============================================================================

def process_upload_direct(temp_file_path, metadata_json_string):
    """
    Direct-to-database upload processing.
    
    Workflow:
    1. Validate file and metadata
    2. Generate filename and get experiment ID from database
    3. Save file to DATA_FOLDER
    4. Create experiment directly in database
    5. Parse and load hap.py results
    
    Args:
        temp_file_path: Path to uploaded temp file
        metadata_json_string: JSON metadata from form
        
    Returns:
        dict: {"success": bool, "message": str, "filename": str}
    """
    
    temp_work_dir = None
    
    try:
        logger.info("Starting direct database upload...")
        
        try:
            metadata = json.loads(metadata_json_string)
        except json.JSONDecodeError as e:
            return {"success": False, "message": f"Invalid JSON: {str(e)}", "filename": None}
        
        logger.info(f"Processing experiment: {metadata.get('exp_name', 'Unknown')}")
        
        # STEP 1: Validation
        is_valid_file, file_msg = validate_happy_file(temp_file_path)
        if not is_valid_file:
            return {"success": False, "message": f"File validation failed: {file_msg}", "filename": None}
        
        is_valid_meta, meta_msg = validate_metadata(metadata)
        if not is_valid_meta:
            return {"success": False, "message": f"Metadata validation failed: {meta_msg}", "filename": None}
        
        # STEP 2: Get experiment ID from database
        experiment_id = get_next_experiment_id()
        logger.info(f"Assigned experiment ID: {experiment_id}")
        
        # STEP 3: Setup working directory
        temp_work_dir = tempfile.mkdtemp(prefix="upload_")
        logger.debug(f"Working in {temp_work_dir}")
        
        # STEP 4: Generate filename
        filename = generate_filename(metadata, experiment_id)
        temp_file_copy = os.path.join(temp_work_dir, filename)
        
        shutil.copy2(temp_file_path, temp_file_copy)
        logger.debug("File copied to working directory")
        
        # STEP 5: Save file to final location
        os.makedirs(DATA_FOLDER, exist_ok=True)
        final_file_path = os.path.join(DATA_FOLDER, filename)
        shutil.move(temp_file_copy, final_file_path)
        logger.info(f"File saved to: {final_file_path}")
        
        # STEP 6: Prepare metadata for database
        db_metadata = prepare_metadata_dict(metadata)
        
        # STEP 7: Create experiment directly in database
        logger.debug("Creating experiment in database...")
        db_result = create_experiment_direct(
            metadata=db_metadata,
            experiment_id=experiment_id,
            filename=filename
        )
        
        if not db_result["success"]:
            os.remove(final_file_path)
            return {
                "success": False,
                "message": f"Database creation failed: {db_result['message']}",
                "filename": None
            }
        
        success_message = f"Upload successful! File: {filename}. {db_result['message']}"
        logger.info(success_message)
        
        return {
            "success": True, 
            "message": success_message,
            "filename": filename,
            "experiment_id": experiment_id
        }
        
    except Exception as e:
        error_msg = f"Upload failed: {str(e)}"
        logger.error(error_msg)
        return {"success": False, "message": error_msg, "filename": None}
        
    finally:
        if temp_work_dir and os.path.exists(temp_work_dir):
            try:
                shutil.rmtree(temp_work_dir)
                logger.debug("Cleanup: Working directory removed")
            except:
                logger.warning("Could not cleanup working directory")

# ============================================================================
# PYTHON/R INTERFACE
# ============================================================================

def upload_experiment_v2(file_path, metadata_json, username=None, is_admin=False):
    """
    Upload experiment directly to database - ADMIN ONLY
    
    Args:
        file_path: Path to uploaded file
        metadata_json: JSON string with experiment metadata
        username: Username for logging
        is_admin: Admin status (required)
    
    Returns:
        dict: Result with success status, message, and filename
    """
    logger.info(f"Upload requested by: {username}")
    return process_upload_direct(file_path, metadata_json)