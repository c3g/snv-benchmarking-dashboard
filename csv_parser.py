# simple_csv_processor.py
# Simple version for processing CSVs when names match between metadata and files

import pandas as pd
from database import get_db_session
from models import Experiment, BenchmarkResult
from config import DATA_FOLDER, METADATA_CSV_PATH, get_data_file_path



def safe_float(value):
    """Convert value to float, handle"""
    if pd.isna(value) or value is None:
        return None
    try:
        return float(value)
    except:
        return None

def safe_int(value):
    """Convert value to int safely"""
    if pd.isna(value) or value is None:
        return None
    try:
        return int(float(value))
    except:
        return None

def parse_happy_csv(happy_file_name, experiment_id):
    """
    Parse one hap.py CSV file and store results in database
    
    Args:
        happy_file_name: path to hap.py CSV file
        
    Returns:

    """
    happy_file_path = get_data_file_path(happy_file_name)
    
    try:
        # Read the CSV file
        df = pd.read_csv(happy_file_path)
        print(f"  _____ Found {len(df)} rows")

        # Filter for the specific rows --------------- Can be edited later
        filtered_df = df[
            (df['Subtype'] == '*') &
            (df['Subset'] == '*') &
            (df['Filter'] == 'ALL')
        ]
    
        if len(filtered_df) == 0:
                print(f"Warning: No matching rows found in {happy_file_path}")
                return []
        print(f"  _____ Found {len(filtered_df)} rows")
        
        results = [] # python list to store result objects 

        # Store specific columns from the csv to the BenchmarkResult table in the database
        for _, row in filtered_df.iterrows():
            # Create BenchmarkResult object
            result = BenchmarkResult(
                experiment_id=experiment_id,
                
                # Core identifiers
                variant_type=row['Type'],
                subtype=row['Subtype'],
                subset=row['Subset'],
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
            
            results.append(result)
            
        print(f"Extracted {len(results)} results from {(happy_file_path)}")
        return results
        
        # need to add these results to DB results table
    except Exception as e:
        print(f"Error parsing {happy_file_path}: {e}")
        return []



    except Exception as e:
        return {"success": False, "error": str(e)}

#------------- TEST 
print(parse_happy_csv('001_HG002_Illumina_NovaseqX_DeepVariant_GIAB.csv',1))








