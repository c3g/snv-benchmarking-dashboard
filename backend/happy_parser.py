
# happy_parser.py
# Parses hap.py CSV output files and stores performance metrics for each experiemtn recrod

import pandas as pd
from models import RegionType, BenchmarkResult,OverallResult
from config import get_data_file_path

def safe_float(value):
    """Convert value to float, handle NaN"""
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

def parse_happy_csv(happy_file_name, experiment_id, session):
    """
    Parse one hap.py CSV file and store results in database
    """
    happy_file_path = get_data_file_path(happy_file_name)
    
    try:
        # Read the CSV file
        df = pd.read_csv(happy_file_path)
        print(f"  Found {len(df)} rows")

        # Filter for specific rows
        filtered_df = df[
            (df['Subtype'] == '*') &
            (df['Filter'] == 'ALL') 
        ]
    
        if len(filtered_df) == 0:
            print(f"Warning: No matching rows found in {happy_file_path}")
            return {"success": False, "error": "No matching rows found"}
        
        print(f"  Found {len(filtered_df)} filtered rows")
        
        results_added = 0
        overall_results_added = 0

        # Store specific columns from the csv to the BenchmarkResult table
        for _, row in filtered_df.iterrows():

            # Convert hap.py region string to enum
            region_enum = RegionType.from_string(row['Subset'])
            if region_enum is None:
                print(f"  Warning: Unknown region '{row['Subset']}' - skipping")
                continue
            
            # 1. Create BenchmarkResult for all regions
            result = BenchmarkResult(
                experiment_id=experiment_id, # links the result to the experiment ID. 
                
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

            # 2. Store in overall table (ALL subset only)
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

        return {"success": True, "message": f"Added {results_added} results and {overall_results_added} overall results for experiment {experiment_id}"}
        
    except Exception as e:
        print(f"Error parsing {happy_file_path}: {e}")
        import traceback
        traceback.print_exc()
        return {"success": False, "error": str(e)}