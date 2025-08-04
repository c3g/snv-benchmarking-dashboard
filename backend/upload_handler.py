# ============================================================================
# upload_handler.py
# ============================================================================
"""
File upload processing and validation for SNV Benchmarking Dashboard.

Main components:
- Hap.py CSV file validation
- Metadata validation and processing
- Safe filename generation and file handling
- Database integration after upload
- Comprehensive error handling and cleanup
"""

import json
import pandas as pd
import os
import shutil
import tempfile
import logging
from datetime import datetime
from pathlib import Path

from config import DATA_FOLDER, METADATA_CSV_PATH
from populate_metadata import populate_database_from_csv

logger = logging.getLogger(__name__)

# required metadata fields for upload
REQUIRED_METADATA = ['exp_name', 'technology', 'platform_name', 'caller_name', 'truth_set_name']

# ============================================================================
# FILE VALIDATION
# ============================================================================

def validate_happy_file(file_path):
    """
    Validate uploaded hap.py CSV file format and content.
    
    Checks for file existence, required columns, and expected data types.
    Ensures the file contains SNP/INDEL variant data before processing.
    
    Args:
        file_path (str): Path to uploaded hap.py CSV file
        
    Returns:
        tuple: (is_valid: bool, message: str) - validation result and description
    """
    try:
        if not os.path.exists(file_path):
            return False, "File not found"
            
        df = pd.read_csv(file_path)
        
        # Essential columns only
        required_cols = ['Type', 'Subtype', 'Subset', 'Filter', 
                        'METRIC.Recall', 'METRIC.Precision', 'METRIC.F1_Score']
        
        missing = [col for col in required_cols if col not in df.columns]
        if missing:
            return False, f"Missing columns: {', '.join(missing)}"
        
        # Check for data
        if len(df) == 0:
            return False, "File is empty"
            
        # Check for expected variant types
        found_types = df['Type'].unique()
        if not any(t in ['SNP', 'INDEL'] for t in found_types):
            return False, f"No SNP/INDEL data found. Found: {list(found_types)}"
        
        return True, f"Valid hap.py file with {len(df)} rows"
        
    except Exception as e:
        return False, f"Error reading file: {str(e)}"

def validate_metadata(metadata):
    """
    Validate required metadata fields and enum values.
    
    Ensures all required fields are present and technology/caller values
    are valid before attempting database insertion.
    
    Args:
        metadata (dict): Metadata dictionary from upload form
        
    Returns:
        tuple: (is_valid: bool, message: str) - validation result and description
    """
    
    for field in REQUIRED_METADATA:
        value = metadata.get(field, "")
        if not value or str(value).strip() == "":
            return False, f"Required field '{field}' is missing"
    
    # Validate enums
    if metadata['technology'].upper() not in ['ILLUMINA', 'PACBIO', 'ONT', 'MGI']:
        return False, f"Invalid technology: {metadata['technology']}"
        
    if metadata['caller_name'].upper() not in ['DEEPVARIANT', 'GATK', 'CLAIR3']:
        return False, f"Invalid caller: {metadata['caller_name']}"
    
    return True, "Metadata is valid"

# ============================================================================
# FILE PROCESSING
# ============================================================================

def get_next_experiment_id():
    """
    Get the next available experiment ID by counting existing entries.
    """
    try:
        if os.path.exists(METADATA_CSV_PATH):
            existing_df = pd.read_csv(METADATA_CSV_PATH)
            return len(existing_df) + 1
        else:
            return 1
    except Exception as e:
        logger.error(f"Error getting next ID: {e}")
        return 1  # Fallback to 1

def generate_filename(metadata, experiment_id):
    """
    Generate standardized filename using experiment metadata.
    
    Creates filename in format: {experiment_id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv
    Falls back to timestamp-based naming if metadata processing fails.
    
    Args:
        metadata (dict): Experiment metadata
        experiment_id (int): Assigned experiment ID
        
    Returns:
        str: Generated filename
    """
    
    def strip_value(value):
        if value is None or pd.isna(value): 
            return ""
        return str(value).strip()
    
    try:
        # Extract sample name (first part before underscore)
        sample = strip_value(str(metadata.get('exp_name', '')).split('_')[0])
        
        # Clean all metadata components
        technology = strip_value(metadata.get('technology', '')).lower()
        platform = strip_value(metadata.get('platform_name', '')).lower()
        caller = strip_value(metadata.get('caller_name', '')).lower()
        truthset = strip_value(metadata.get('truth_set_name', '')).lower()
        
        # Build filename with 3 zero padding
        filename = f"{experiment_id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv"
        
        return filename
        
    except Exception as e:
        # generate filename with timestamp if anything fails
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        sample = metadata['exp_name'].split('_')[0]
        logger.warning(f"Filename generation failed, using timestamp fallback: {e}")
        return f"{timestamp}_{sample}_{metadata['technology']}_{metadata['caller_name']}.csv"

def create_metadata_entry(metadata, filename, experiment_id):
    """
    Create metadata row for CSV file from upload form data.
    
    Converts upload form metadata into the format expected by the metadata CSV,
    with proper type conversion and default values.
    
    Args:
        metadata (dict): Raw metadata from upload form
        filename (str): Generated filename for the hap.py file
        experiment_id (int): Assigned experiment ID
        
    Returns:
        dict: Formatted metadata entry for CSV insertion
    """
    
    # Helper function for safe conversion
    def safe_float(value):
        try:
            return float(value) if value and str(value).strip() else None
        except:
            return None
    
    def safe_bool(value):
        return str(value).lower() == 'true'
    
    return {
        'ID': experiment_id,
        'name': metadata['exp_name'],
        'technology': metadata['technology'],
        'target': metadata.get('target', 'wgs'),
        'platform_name': metadata['platform_name'],
        'platform_type': metadata.get('platform_type', ''),
        'platform_version': metadata.get('platform_version', ''),
        'chemistry_name': metadata.get('chemistry_name', ''),
        'caller_name': metadata['caller_name'],
        'caller_type': metadata.get('caller_type', ''),
        'caller_version': metadata.get('caller_version', ''),
        'caller_model': metadata.get('caller_model', ''),
        'aligner_name': metadata.get('aligner_name', ''),
        'aligner_version': metadata.get('aligner_version', ''),
        'truth_set_name': metadata.get('truth_set_name', ''),
        'truth_set_sample': metadata.get('truth_set_sample', 'hg002'),
        'truth_set_version': metadata.get('truth_set_version', ''),
        'truth_set_reference': metadata.get('truth_set_reference', ''),
        'variant_type': metadata.get('variant_type', 'snp+indel'),
        'variant_size': metadata.get('variant_size', ''),
        'variant_origin': metadata.get('variant_origin', ''),
        'is_phased': safe_bool(metadata.get('is_phased', 'false')),
        'benchmark_tool_name': metadata.get('benchmark_tool_name', 'hap.py'),
        'benchmark_tool_version': metadata.get('benchmark_tool_version', ''),
        'mean_coverage': safe_float(metadata.get('mean_coverage')),
        'read_length': safe_float(metadata.get('read_length')),
        'mean_insert_size': safe_float(metadata.get('mean_insert_size')),
        'mean_read_length': metadata.get('mean_read_length', ''),
        'file_name': filename,
        'file_path': None,
        'upload_date': datetime.now().strftime('%Y-%m-%d'),
    }

# ============================================================================
# MAIN UPLOAD PROCESSING
# ============================================================================

def process_upload(temp_file_path, metadata_json_string):
    """
    Complete upload processing workflow with validation, file handling, and database integration.
    
    Handles the upload process:
    1. Validate file and metadata
    2. Generate filename and setup working directory
    3. Copy files to final location
    4. Update metadata CSV
    5. Populate database with new experiment
    6. Cleanup temporary files
    
    Args:
        temp_file_path (str): Path to uploaded temporary file
        metadata_json_string (str): JSON string containing experiment metadata
        
    Returns:
        dict: Result with success status, message, and filename
              {"success": bool, "message": str, "filename": str or None}
    """
    
    temp_work_dir = None
    
    try:
        logger.info("Starting upload process...")
        
        # Parse metadata
        try:
            metadata = json.loads(metadata_json_string)
        except json.JSONDecodeError as e:
            return {"success": False, "message": f"Invalid JSON: {str(e)}", "filename": None}
        
        logger.info(f"Processing experiment: {metadata.get('exp_name', 'Unknown')}")
        
        # STEP 1: Validation
        logger.debug("Validating file...")
        is_valid_file, file_msg = validate_happy_file(temp_file_path)
        if not is_valid_file:
            return {"success": False, "message": f"File validation failed: {file_msg}", "filename": None}
        
        logger.debug("Validating metadata...")
        is_valid_meta, meta_msg = validate_metadata(metadata)
        if not is_valid_meta:
            return {"success": False, "message": f"Metadata validation failed: {meta_msg}", "filename": None}
        
        # STEP 2: Get experiment ID
        experiment_id = get_next_experiment_id()
        logger.info(f"Assigned experiment ID: {experiment_id}")
        
        # STEP 3: Setup working directory
        temp_work_dir = tempfile.mkdtemp(prefix="upload_")
        logger.debug(f"Working in {temp_work_dir}")
        
        # STEP 4: Generate filename and prepare data
        filename = generate_filename(metadata, experiment_id)
        temp_file_copy = os.path.join(temp_work_dir, filename)
        
        # Copy file to working directory first
        shutil.copy2(temp_file_path, temp_file_copy)
        logger.debug("File copied to working directory")
        
        # STEP 5: Prepare metadata update
        metadata_entry = create_metadata_entry(metadata, filename, experiment_id)
        
        # Read existing metadata or create new
        if os.path.exists(METADATA_CSV_PATH):
            existing_df = pd.read_csv(METADATA_CSV_PATH)
        else:
            existing_df = pd.DataFrame(columns=[
                'ID', 'name', 'technology', 'target', 'platform_name', 'platform_type',
                'platform_version', 'chemistry_name', 'caller_name', 'caller_type',
                'caller_version', 'caller_model', 'aligner_name', 'aligner_version',
                'truth_set_name', 'truth_set_sample', 'truth_set_version',
                'truth_set_reference', 'variant_type', 'variant_size', 'variant_origin',
                'is_phased', 'benchmark_tool_name', 'benchmark_tool_version',
                'mean_coverage', 'read_length', 'mean_insert_size', 'mean_read_length',
                'file_name', 'file_path', 'upload_date'
            ])
        
        # Add new entry
        updated_df = pd.concat([existing_df, pd.DataFrame([metadata_entry])], ignore_index=True)
        
        # STEP 6: Commit files
        logger.debug("Adding/editing files...")
        os.makedirs(DATA_FOLDER, exist_ok=True)
        
        # Move file to final location
        final_file_path = os.path.join(DATA_FOLDER, filename)
        shutil.move(temp_file_copy, final_file_path)
        
        # Update metadata CSV
        updated_df.to_csv(METADATA_CSV_PATH, index=False)
        
        logger.info("Files added/edited successfully")
        
        # STEP 7: Update database
        logger.debug("Updating database...")
        try:
            db_success = populate_database_from_csv()
            if db_success:
                logger.info("Database updated successfully")
                db_status = "Database updated"
            else:
                logger.warning("Database update failed - but files are saved")
                db_status = "Database update failed (files saved)"
        except Exception as e:
            logger.error(f"Database update error: {e}")
            db_status = f"Database error: {str(e)}"
        
        success_message = f"Upload successful! File: {filename}. {db_status}"
        logger.info(success_message)
        
        return {
            "success": True, 
            "message": success_message,
            "filename": filename
        }
        
    except Exception as e:
        error_msg = f"Upload failed: {str(e)}"
        logger.error(error_msg)
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
# PYTHON/R INTERFACE FUNCTION
# ============================================================================

def upload_experiment(file_path, metadata_json):
    """
    Simple interface for R Shiny integration.
    
    Args:
        file_path (str): Path to uploaded file
        metadata_json (str): JSON string containing experiment metadata
        
    Returns:
        dict: Upload result with success status and message
    """
    return process_upload(file_path, metadata_json)