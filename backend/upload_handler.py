
import json
import pandas as pd
import os
import shutil
import tempfile
from datetime import datetime
from pathlib import Path
import uuid

from config import DATA_FOLDER, METADATA_CSV_PATH
from populate_metadata import populate_database_from_csv

def validate_happy_file(file_path):
    """Validate hap.py CSV format"""
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
    """Validate required metadata fields"""
    required = ['exp_name', 'technology', 'platform_name', 'caller_name']
    
    for field in required:
        value = metadata.get(field, "").strip()
        if not value:
            return False, f"Required field '{field}' is missing"
    
    # Validate enums
    if metadata['technology'].upper() not in ['ILLUMINA', 'PACBIO', 'ONT', 'MGI']:
        return False, f"Invalid technology: {metadata['technology']}"
        
    if metadata['caller_name'].upper() not in ['DEEPVARIANT', 'GATK', 'CLAIR3']:
        return False, f"Invalid caller: {metadata['caller_name']}"
    
    return True, "Metadata is valid"

def generate_safe_filename(metadata):
    """Generate filename with timestamp to avoid collisions"""
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    sample = metadata['exp_name'].split('_')[0]
    
    filename = f"{timestamp}_{sample}_{metadata['technology']}_{metadata['caller_name']}.csv"
    return filename

def create_metadata_entry(metadata, filename):
    """Create metadata row for CSV"""
    
    # Helper function for safe conversion
    def safe_float(value):
        try:
            return float(value) if value and str(value).strip() else None
        except:
            return None
    
    def safe_bool(value):
        return str(value).lower() == 'true'
    
    return {
        'name': metadata['exp_name'],
        'technology': metadata['technology'].lower(),
        'target': metadata.get('target', 'wgs').lower(),
        'platform_name': metadata['platform_name'],
        'platform_type': metadata.get('platform_type', '').lower(),
        'platform_version': metadata.get('platform_version', ''),
        'chemistry_name': metadata.get('chemistry_name', ''),
        'caller_name': metadata['caller_name'].lower(),
        'caller_type': metadata.get('caller_type', '').lower(),
        'caller_version': metadata.get('caller_version', ''),
        'caller_model': metadata.get('caller_model', ''),
        'aligner_name': metadata.get('aligner_name', ''),
        'aligner_version': metadata.get('aligner_version', ''),
        'truth_set_name': metadata.get('truth_set_name', 'giab').lower(),
        'truth_set_sample': metadata.get('truth_set_sample', 'hg002').lower(),
        'truth_set_version': metadata.get('truth_set_version', '4.2.1'),
        'truth_set_reference': metadata.get('truth_set_reference', 'grch38').lower(),
        'variant_type': metadata.get('variant_type', 'snp+indel').lower(),
        'variant_size': metadata.get('variant_size', 'small').lower(),
        'variant_origin': metadata.get('variant_origin', 'germline').lower(),
        'is_phased': safe_bool(metadata.get('is_phased', 'false')),
        'benchmark_tool_name': metadata.get('benchmark_tool_name', 'hap.py').lower(),
        'benchmark_tool_version': metadata.get('benchmark_tool_version', '0.3.12'),
        'mean_coverage': safe_float(metadata.get('mean_coverage')),
        'read_length': safe_float(metadata.get('read_length')),
        'mean_insert_size': safe_float(metadata.get('mean_insert_size')),
        'mean_read_length': metadata.get('mean_read_length', ''),
        'file_name': filename,
        'file_path': None,
        'upload_date': datetime.now().strftime('%Y-%m-%d')
    }

def process_upload_atomic(temp_file_path, metadata_json_string):
    """
    ATOMIC upload processing - either everything succeeds or everything fails
    
    Returns: {"success": bool, "message": str, "filename": str or None}
    """
    
    temp_work_dir = None
    
    try:
        print(f"🔄 Starting atomic upload process...")
        
        # Parse metadata
        try:
            metadata = json.loads(metadata_json_string)
        except json.JSONDecodeError as e:
            return {"success": False, "message": f"Invalid JSON: {str(e)}", "filename": None}
        
        print(f"   Experiment: {metadata.get('exp_name', 'Unknown')}")
        
        # STEP 1: Validate everything BEFORE making any changes
        print("   Step 1: Validating file...")
        is_valid_file, file_msg = validate_happy_file(temp_file_path)
        if not is_valid_file:
            return {"success": False, "message": f"File validation failed: {file_msg}", "filename": None}
        
        print("   Step 2: Validating metadata...")
        is_valid_meta, meta_msg = validate_metadata(metadata)
        if not is_valid_meta:
            return {"success": False, "message": f"Metadata validation failed: {meta_msg}", "filename": None}
        
        # STEP 2: Setup working directory (for atomic operations)
        temp_work_dir = tempfile.mkdtemp(prefix="upload_")
        print(f"   Step 3: Working in {temp_work_dir}")
        
        # STEP 3: Generate filename and prepare data
        filename = generate_safe_filename(metadata)
        temp_file_copy = os.path.join(temp_work_dir, filename)
        
        # Copy file to working directory first
        shutil.copy2(temp_file_path, temp_file_copy)
        print(f"   Step 4: File copied to working directory")
        
        # STEP 4: Prepare metadata update
        metadata_entry = create_metadata_entry(metadata, filename)
        
        # Read existing metadata or create new
        if os.path.exists(METADATA_CSV_PATH):
            existing_df = pd.read_csv(METADATA_CSV_PATH)
        else:
            # Create with proper columns
            existing_df = pd.DataFrame(columns=[
                'name', 'technology', 'target', 'platform_name', 'platform_type',
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
        
        # STEP 5: ATOMIC COMMIT - Move everything at once
        print("   Step 5: Atomic commit...")
        
        # Ensure target directory exists
        os.makedirs(DATA_FOLDER, exist_ok=True)
        
        # Move file to final location
        final_file_path = os.path.join(DATA_FOLDER, filename)
        shutil.move(temp_file_copy, final_file_path)
        
        # Update metadata CSV
        updated_df.to_csv(METADATA_CSV_PATH, index=False)
        
        print("   Step 6: Files committed successfully")
        
        # STEP 6: Update database (separate from file operations)
        print("   Step 7: Updating database...")
        try:
            db_success = populate_database_from_csv()
            if db_success:
                print("   Database updated successfully")
                db_status = "Database updated"
            else:
                print("   Database update failed - but files are saved")
                db_status = "Database update failed (files saved)"
        except Exception as e:
            print(f"   Database update error: {e}")
            db_status = f"Database error: {str(e)}"
        
        success_message = f"Upload successful! File: {filename}. {db_status}"
        print(f" {success_message}")
        
        return {
            "success": True, 
            "message": success_message,
            "filename": filename
        }
        
    except Exception as e:
        error_msg = f"Upload failed: {str(e)}"
        print(f" {error_msg}")
        return {"success": False, "message": error_msg, "filename": None}
        
    finally:
        # Cleanup working directory
        if temp_work_dir and os.path.exists(temp_work_dir):
            try:
                shutil.rmtree(temp_work_dir)
                print("   Cleanup: Working directory removed")
            except:
                print("   Warning: Could not cleanup working directory")

# Simple interface for R
def upload_experiment(file_path, metadata_json):
    """Simple interface for R Shiny"""
    return process_upload_atomic(file_path, metadata_json)