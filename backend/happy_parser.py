# ============================================================================
# happy_parser.py
# ============================================================================
"""
Hap.py CSV output parsing and database storage for SNV Benchmarking Dashboard.

Main components:
- Hap.py CSV file validation and reading
- Performance metrics extraction and conversion
- Database storage for both detailed and overall results
- Duplicate prevention and error handling
"""

import os
import logging
import pandas as pd
from models import RegionType, BenchmarkResult, OverallResult
from config import get_data_file_path
from utils import safe_float, safe_int

logger = logging.getLogger(__name__)

# ============================================================================
# FILE VALIDATION
# ============================================================================

def validate_happy_file(file_path):
    """Validate hap.py CSV file exists and is readable"""
    if not os.path.exists(file_path):
        logger.error(f"Hap.py file not found: {file_path}")
        return False
    
    if not os.access(file_path, os.R_OK):
        logger.error(f"Cannot read hap.py file: {file_path}")
        return False
    
    if os.path.getsize(file_path) == 0:
        logger.error(f"Hap.py file is empty: {file_path}")
        return False
    
    return True

def validate_happy_data(df):
    """Validate hap.py CSV has required columns and data"""
    required_columns = ['Type', 'Subtype', 'Subset', 'Filter', 'METRIC.Recall', 'METRIC.Precision', 'METRIC.F1_Score']
    
    missing_columns = [col for col in required_columns if col not in df.columns]
    if missing_columns:
        logger.error(f"Missing required columns: {missing_columns}")
        return False
    
    if len(df) == 0:
        logger.error("Hap.py file contains no data")
        return False
    
    return True

# ============================================================================
# MAIN PARSING FUNCTION
# ============================================================================

def parse_happy_csv(happy_file_name, experiment_id, session):
    """
    Parse hap.py CSV output file and store performance metrics in database.
    
    Processes hap.py benchmarking results by:
    1. Checking for existing results (skip if already processed)
    2. Validating file existence and format
    3. Filtering for specific rows (Subtype='*', Filter='ALL')
    4. Converting hap.py regions to database enums
    5. Storing stratified results in BenchmarkResult table
    6. Storing summary results in OverallResult table (for 'All Regions' only)
    
    Args:
        happy_file_name (str): Filename of hap.py CSV (e.g., '001_HG002_Illumina_DeepVariant.csv')
        experiment_id (int): Database ID of the experiment these results belong to
        session: Active SQLAlchemy session for database operations
        
    Returns:
        dict: Success/failure status with message
            {"success": True, "message": "Added X results..."}
            {"success": False, "error": "File not found"}
            {"success": True, "message": "Results already exist", "skipped": True}
    """
    
    happy_file_path = get_data_file_path(happy_file_name)
    
    logger.debug(f"Parsing hap.py file: {happy_file_name} for experiment {experiment_id}")
    
    try:
        # Check for existing results
        existing_benchmark = session.query(BenchmarkResult).filter_by(experiment_id=experiment_id).first()
        existing_overall = session.query(OverallResult).filter_by(experiment_id=experiment_id).first()
        
        if existing_benchmark or existing_overall:
            logger.info(f"Results already exist for experiment {experiment_id}")
            return {"success": True, "message": f"Results already exist for experiment {experiment_id}", "skipped": True}
        
        # File validation
        if not validate_happy_file(happy_file_path):
            return {"success": False, "error": f"File validation failed for {happy_file_path}"}
        
        # Read CSV file
        try:
            df = pd.read_csv(happy_file_path)
            logger.debug(f"Read {len(df)} rows from {happy_file_name}")
        except Exception as e:
            logger.error(f"Failed to read CSV file {happy_file_path}: {e}")
            return {"success": False, "error": f"Failed to read CSV: {e}"}
        
        # Data validation
        if not validate_happy_data(df):
            return {"success": False, "error": "Invalid hap.py data format"}

        # Filter for specific rows (Subtype='*', Filter='ALL')
        filtered_df = df[(df['Subtype'] == '*') & (df['Filter'] == 'ALL')]
   
        if len(filtered_df) == 0:
            logger.warning(f"No matching rows found in {happy_file_path}")
            return {"success": False, "error": "No matching rows found (Subtype='*', Filter='ALL')"}
       
        logger.debug(f"Found {len(filtered_df)} filtered rows for processing")
       
        results_added = 0
        overall_results_added = 0

        # Process each row and create database records
        for _, row in filtered_df.iterrows():
            # Convert hap.py region string to enum
            region_enum = RegionType.from_string(row['Subset'])
            if region_enum is None:
                logger.warning(f"Unknown region '{row['Subset']}' - skipping")
                continue
           
            # Create BenchmarkResult for all regions
            result = BenchmarkResult(
                experiment_id=experiment_id,
                
                # Main identifiers
                variant_type=row['Type'],
                subtype=row['Subtype'].replace('*', 'ALL_SUBTYPES'),
                subset=region_enum,
                filter_type=row['Filter'],
                
                # Performance metrics
                metric_recall=safe_float(row.get('METRIC.Recall')),
                metric_precision=safe_float(row.get('METRIC.Precision')),
                metric_f1_score=safe_float(row.get('METRIC.F1_Score')),
                
                # Subset information
                subset_size=safe_float(row.get('Subset.Size')),
                subset_is_conf_size=safe_float(row.get('Subset.IS_CONF.Size')),
                
                # Truth set totals
                truth_total=safe_int(row.get('TRUTH.TOTAL')),
                truth_total_het=safe_int(row.get('TRUTH.TOTAL.het')),
                truth_total_homalt=safe_int(row.get('TRUTH.TOTAL.homalt')),
                
                # Truth set true positives
                truth_tp=safe_int(row.get('TRUTH.TP')),
                truth_tp_het=safe_int(row.get('TRUTH.TP.het')),
                truth_tp_homalt=safe_int(row.get('TRUTH.TP.homalt')),
                
                # Truth set false negatives
                truth_fn=safe_int(row.get('TRUTH.FN')),
                truth_fn_het=safe_int(row.get('TRUTH.FN.het')),
                truth_fn_homalt=safe_int(row.get('TRUTH.FN.homalt')),
                
                # Query totals
                query_total=safe_int(row.get('QUERY.TOTAL')),
                query_total_het=safe_int(row.get('QUERY.TOTAL.het')),
                query_total_homalt=safe_int(row.get('QUERY.TOTAL.homalt')),
                
                # Query true positives
                query_tp=safe_int(row.get('QUERY.TP')),
                query_tp_het=safe_int(row.get('QUERY.TP.het')),
                query_tp_homalt=safe_int(row.get('QUERY.TP.homalt')),
                
                # Query false positives
                query_fp=safe_int(row.get('QUERY.FP')),
                query_fp_het=safe_int(row.get('QUERY.FP.het')),
                query_fp_homalt=safe_int(row.get('QUERY.FP.homalt')),
                
                # Query unknown
                query_unk=safe_int(row.get('QUERY.UNK')),
                query_unk_het=safe_int(row.get('QUERY.UNK.het')),
                query_unk_homalt=safe_int(row.get('QUERY.UNK.homalt'))
            )
           
            session.add(result)
            results_added += 1

            # Store in overall table (ALL subset only for quick access)
            if region_enum == RegionType.ALL:
                overall_result = OverallResult(
                    experiment_id=experiment_id,
                    variant_type=row['Type'],
                    metric_recall=safe_float(row.get('METRIC.Recall')),
                    metric_precision=safe_float(row.get('METRIC.Precision')),
                    metric_f1_score=safe_float(row.get('METRIC.F1_Score')),
                    truth_total=safe_int(row.get('TRUTH.TOTAL')),
                    truth_tp=safe_int(row.get('TRUTH.TP')),
                    truth_fn=safe_int(row.get('TRUTH.FN')),
                    query_total=safe_int(row.get('QUERY.TOTAL')),
                    query_tp=safe_int(row.get('QUERY.TP')),
                    query_fp=safe_int(row.get('QUERY.FP'))
                )
               
                session.add(overall_result)
                overall_results_added += 1

        success_message = f"Added {results_added} results and {overall_results_added} overall results for experiment {experiment_id}"
        logger.debug(success_message)
        return {"success": True, "message": success_message}
       
    except Exception as e:
        error_message = f"Error parsing {happy_file_path}: {e}"
        logger.error(error_message)
        return {"success": False, "error": str(e)}