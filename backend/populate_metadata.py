# ============================================================================
# populate_metadata.py
# ============================================================================
"""
<<<<<<THIS IS ONLY USED FOR INITIAL MIGRATION FROM CSV AND BACK_UP>>>>>>

Metadata parsing and database population for SNV Benchmarking Dashboard.

Main components:
- CSV metadata loading and validation
- Data cleaning and enum mapping
- Database record creation (with duplicate prevention)
- Experiment linking and hap.py results integration
"""

import os
import logging
import pandas as pd
from database import get_db_session
from models import *
from config import METADATA_CSV_PATH
from happy_parser import parse_happy_csv
from utils import clean_value, safe_float
from datetime import datetime

logger = logging.getLogger(__name__)

# Keep original case for display columns (not converted to lowercase)
DISPLAY_COLUMNS = ['name', 'description', 'platform_name', 'chemistry_name','caller_model','aligner_name','file_name']

# ============================================================================
# CSV LOADING AND VALIDATION
# ============================================================================

def load_csv_metadata(file_path=METADATA_CSV_PATH):
    """Load and validate CSV metadata file"""
    
    # File existence and access checks
    if not os.path.exists(file_path):
        logger.error(f"Metadata CSV file not found: {file_path}")
        return None
    
    if not os.access(file_path, os.R_OK):
        logger.error(f"Cannot read metadata CSV file: {file_path}")
        return None
    
    if os.path.getsize(file_path) == 0:
        logger.error("Metadata CSV file is empty")
        return None
    
    try:
        metadata_df = pd.read_csv(file_path)
        logger.info(f"Successfully loaded {len(metadata_df)} rows from metadata CSV")
        return metadata_df
        
    except Exception as e:
        logger.error(f"Error loading metadata CSV: {e}")
        return None

# ============================================================================
# DATA CLEANING AND CONVERSION
# ============================================================================

def clean_dataframe_strings(df):
    """
    Clean string columns to lowercase for enum matching, while preserving display columns.
    
    prepares CSV data for database insertion by normalizing most string fields to lowercase,
    but keeps certain columns in their original case for better display in the UI (DISPLAY_COLUMNS).
    
    Args:
        df (pandas.DataFrame): Raw CSV data loaded from metadata file
        
    Returns:
        pandas.DataFrame: Cleaned dataframe with normalized strings

    """
    cleaned_df = df.copy()
    
    string_columns = cleaned_df.select_dtypes(include=['object']).columns
    for col in string_columns:
        if col not in DISPLAY_COLUMNS:
            cleaned_df[col] = cleaned_df[col].apply(clean_value)
    
    return cleaned_df

# ============================================================================
# ENUM MAPPING
# ============================================================================

# Dictionary for converting cleaned strings to enums
ENUM_MAPPINGS = {
    'technology': {
        'illumina': SeqTechName.ILLUMINA,
        'mgi': SeqTechName.MGI,
        'ont': SeqTechName.ONT,
        'pacbio': SeqTechName.PACBIO,
        '10x': SeqTechName.TENX,             
        '10x genomics': SeqTechName.TENX,     
    },
    'target': {
        'wgs': SeqTechTarget.WGS,
        'wes': SeqTechTarget.WES,
    },
    'platform_type': {
        'srs': SeqTechPlatformType.SRS,
        'lrs': SeqTechPlatformType.LRS,
        'synthetic': SeqTechPlatformType.SYNTHETIC, 
    },
    'caller_name': {
        'deepvariant': CallerName.DEEPVARIANT,
        'gatk3': CallerName.GATK3,     
        'gatk4': CallerName.GATK4,         
        'clair3': CallerName.CLAIR3,
        'dragen': CallerName.DRAGEN,
        'longranger': CallerName.LONGRANGER, 
        'megabolt': CallerName.MEGABOLT,     
        'nanocaller': CallerName.NANOCALLER, 
        'parabrick': CallerName.PARABRICK,     
        'pepper': CallerName.PEPPER,          
    },
    'caller_type': {
        'ml': CallerType.ML,
        'traditional': CallerType.TRADITIONAL,
    },
    'truth_set_name': {
        'giab': TruthSetName.GIAB,
        'cmrg': TruthSetName.CMRG,
        't2t': TruthSetName.T2T,
    },
    'truth_set_reference': {
        'grch37': TruthSetReference.GRCH37,
        'grch38': TruthSetReference.GRCH38,
    },
    'truth_set_sample': {
        'hg001': TruthSetSample.HG001,
        'hg002': TruthSetSample.HG002,
        'hg003': TruthSetSample.HG003,
        'hg004': TruthSetSample.HG004,
        'hcc1395': TruthSetSample.HCC1395,
    },
    'variant_origin': {
        'germline': VariantOrigin.GERMLINE,
        'somatic': VariantOrigin.SOMATIC,
    },
    'variant_size': {
        'small': VariantSize.SMALL,
        'large': VariantSize.LARGE,
    },
    'variant_type': {
        'snp': VariantType.SNP,
        'indel': VariantType.INDEL,
        'ins': VariantType.INS,
        'del': VariantType.DEL,
        'snp+indel': VariantType.SNPINDEL,
    },
    'benchmark_tool_name': {
        'hap.py': BenchmarkToolName.HAPPY,
        'vcfdist': BenchmarkToolName.VCFDIST,
        'truvari': BenchmarkToolName.TRUVARI,
    }
}

def map_enum(field_name, value):
    """Map string values to enum types with case-insensitive matching"""
    
    if pd.isna(value) or value == '' or value is None:
        return None
    
    # Clean the value
    value_clean = str(value).strip().lower()
    
    # Use the ENUM_MAPPINGS dictionary that already exists
    if field_name not in ENUM_MAPPINGS:
        logger.warning(f"Unknown enum field: {field_name}")
        return None
    
    field_mappings = ENUM_MAPPINGS[field_name]
    
    if value_clean not in field_mappings:
        logger.warning(f"Unknown {field_name} value: '{value}' (cleaned: '{value_clean}')")
        logger.info(f"Valid options: {list(field_mappings.keys())}")
        return None
    
    return field_mappings[value_clean]

def map_boolean(value):
    """Convert string to boolean"""
    return clean_value(value) == 'true'

# ============================================================================
# RECORD CREATION FUNCTIONS
# ============================================================================

def add_record_if_not_exists(session, model_class, filter_fields, all_fields, record_name):
    """
    Generic function to create a database record only if it doesn't already exist.
    Prevents duplicate records by checking for existing entries first.
    
    Args:
        session: Active SQLAlchemy database session for queries and commits
        model_class: SQLAlchemy model class to create (e.g., SequencingTechnology, VariantCaller)
        filter_fields (dict): Fields to check for existing records (e.g., {'name': 'ILLUMINA', 'platform': 'NovaSeq'})
        all_fields (dict): Complete field set for creating new record (includes filter_fields + additional fields)
        record_name (str): Human-readable description for logging (e.g., "sequencing tech", "variant caller")
        
    Returns:
        SQLAlchemy object: Either the existing record (if found) or newly created record
    """
    try:
        existing = session.query(model_class).filter_by(**filter_fields).first()
            
        if not existing:
            new_record = model_class(**all_fields)
            session.add(new_record)
            session.flush()
            logger.debug(f"Added {record_name}")
            return new_record
        else:
            logger.debug(f"Already exists {record_name}")
            return existing
                
    except Exception as e:
        logger.error(f"Error adding {record_name}: {e}")
        session.rollback()
        raise
    

def add_sequencing_tech(session, row):
    """Create SequencingTechnology record from CSV row"""
    tech_enum = map_enum('technology', row['technology'])
    target_enum = map_enum('target', row['target'])
    type_enum = map_enum('platform_type', row['platform_type'])
    
    filter_fields = {
        'technology': tech_enum,
        'platform_name': row['platform_name'],
    }
    all_fields = {
        **filter_fields,
        'target': target_enum,
        'platform_type': type_enum,
        'platform_version': row['platform_version']
    }
    
    return add_record_if_not_exists(session, SequencingTechnology, filter_fields, all_fields, "sequencing tech")

def add_variant_caller(session, row):
    """Create VariantCaller record from CSV row"""
    name_enum = map_enum('caller_name', row['caller_name'])
    type_enum = map_enum('caller_type', row['caller_type'])
    
    filter_fields = {
        'name': name_enum,
        'version': row['caller_version']
    }
    all_fields = {
        **filter_fields,
        'type': type_enum,
        'model': row.get('caller_model', None)
    }
    return add_record_if_not_exists(session, VariantCaller, filter_fields, all_fields, "variant caller")

def add_aligner(session, row):
    """Create Aligner record from CSV row"""
    # Skip if no aligner name provided
    if not row.get('aligner_name') or pd.isna(row.get('aligner_name')) or str(row.get('aligner_name')).strip() == '':
        return None
        
    filter_fields = {
        'name': row['aligner_name'],
        'version': row['aligner_version']
    }
    return add_record_if_not_exists(session, Aligner, filter_fields, filter_fields, "aligner")

def add_truth_set(session, row):
    """Create TruthSet record from CSV row"""
    name_enum = map_enum('truth_set_name', row['truth_set_name'])
    reference_enum = map_enum('truth_set_reference', row['truth_set_reference'])
    sample_enum = map_enum('truth_set_sample', row['truth_set_sample'])
    
    filter_fields = {
        'name': name_enum,
        'version': row['truth_set_version'],
        'sample': sample_enum,
        'reference': reference_enum
    }
    return add_record_if_not_exists(session, TruthSet, filter_fields, filter_fields, "truth set")

def add_benchmark_tool(session, row):
    """Create BenchmarkTool record from CSV row"""
    tool_enum = map_enum('benchmark_tool_name', row['benchmark_tool_name'])
    
    filter_fields = {
        'name': tool_enum,
        'version': row['benchmark_tool_version']
    }
    return add_record_if_not_exists(session, BenchmarkTool, filter_fields, filter_fields, "benchmark tool")

def add_variant(session, row):
    """Create Variant record from CSV row"""
    type_enum = map_enum('variant_type', row['variant_type'])
    size_enum = map_enum('variant_size', row['variant_size'])
    origin_enum = map_enum('variant_origin', row['variant_origin'])
    phased_bool = map_boolean(row['is_phased'])
    
    filter_fields = {
        'type': type_enum,
        'size': size_enum,
        'origin': origin_enum,
        'is_phased': phased_bool
    }
    return add_record_if_not_exists(session, Variant, filter_fields, filter_fields, "variant")

def add_chemistry(session, row):
    """Create Chemistry record from CSV row"""
    # Skip if no chemistry name provided
    if not row.get('chemistry_name') or pd.isna(row.get('chemistry_name')) or str(row.get('chemistry_name')).strip() == '':
        return None
        
    tech_enum = map_enum('technology', row['technology'])
    
    filter_fields = {
        'name': row['chemistry_name'],
        'sequencing_technology': tech_enum,
        'sequencing_platform': row['platform_name'],
        'version': row.get('chemistry_version', None)
    }
    return add_record_if_not_exists(session, Chemistry, filter_fields, filter_fields, "chemistry")

def add_quality_control(session, row):
    """Create QualityControl record from CSV row"""
    all_fields = {
        'mean_coverage': safe_float(row.get('mean_coverage', None)),
        'read_length': safe_float(row.get('read_length', None)),
        'mean_read_length': safe_float(row.get('mean_read_length', None)),
        'mean_insert_size': safe_float(row.get('mean_insert_size', None))
    }
    
    # Skip if all QC fields are None/empty
    if all(value is None for value in all_fields.values()):
        return None
    
    filter_fields = all_fields.copy()
    return add_record_if_not_exists(session, QualityControl, filter_fields, all_fields, "quality control")

def add_experiment(session, row, metadata_objects):
    """Create Experiment record linking all metadata"""
    experiment_name = row['name']
    description = row.get('description', f"Benchmarking experiment for {experiment_name}")
    csv_id = row.get('ID')  # GET CSV ID
    
    # Parse created date from CSV
    created_at = None
    if 'created_at' in row and pd.notna(row['created_at']) and str(row['created_at']).strip():
        try:
            created_at = pd.to_datetime(row['created_at']).date()
        except Exception as e:
            logger.warning(f"Could not parse created_at for experiment {csv_id}: {e}")
            created_at = None
    
    # Check if experiment already exists BY ID
    existing_experiment = session.query(Experiment).filter_by(id=csv_id).first()
    
    if existing_experiment:
        logger.warning(f"Experiment ID {csv_id} already exists: {experiment_name}")
        return existing_experiment
    
    try:
        new_experiment = Experiment(
            id=csv_id,  # SET ID FROM CSV
            name=experiment_name,
            description=description,
            created_at = created_at,
            sequencing_technology_id=metadata_objects['seq_tech'].id if metadata_objects['seq_tech'] else None,
            variant_caller_id=metadata_objects['caller'].id if metadata_objects['caller'] else None,
            aligner_id=metadata_objects['aligner'].id if metadata_objects['aligner'] else None,
            truth_set_id=metadata_objects['truth_set'].id if metadata_objects['truth_set'] else None,
            benchmark_tool_id=metadata_objects['benchmark_tool'].id if metadata_objects['benchmark_tool'] else None,
            variant_id=metadata_objects['variant'].id if metadata_objects['variant'] else None,
            chemistry_id=metadata_objects['chemistry'].id if metadata_objects['chemistry'] else None,
            quality_control_metrics_id=metadata_objects['qc'].id if metadata_objects['qc'] else None
        )
        
        session.add(new_experiment)
        session.flush()
        logger.info(f"Created experiment: {experiment_name} (ID: {new_experiment.id})")
        return new_experiment
                    
    except Exception as e:
        logger.error(f"Error adding experiment {experiment_name}: {e}")
        return None

# ============================================================================
# MAIN POPULATION FUNCTION
# ============================================================================

def populate_database_from_csv(file_path=METADATA_CSV_PATH):
    """Main function to populate database from CSV metadata and hap.py files"""
    logger.info("Starting database population from CSV")
    
    # Load and validate CSV
    raw_df = load_csv_metadata(file_path)
    if raw_df is None:
        logger.error("Failed to load metadata CSV")
        return False
    
    # Clean and process data
    raw_df = raw_df.dropna(subset=['name'])
    metadata_df = clean_dataframe_strings(raw_df)
    logger.info(f"Processing {len(metadata_df)} experiments")

    success_count = 0
    
    try:
        with get_db_session() as session:
            for index, row in metadata_df.iterrows():
                try:
                    logger.info(f"Processing experiment {index + 1}: {row.get('name', 'Unknown')}")
                                        
                    # Create all metadata records
                    metadata_objects = {
                        'seq_tech': add_sequencing_tech(session, row),
                        'caller': add_variant_caller(session, row),
                        'aligner': add_aligner(session, row),
                        'truth_set': add_truth_set(session, row),
                        'benchmark_tool': add_benchmark_tool(session, row),
                        'variant': add_variant(session, row),
                        'chemistry': add_chemistry(session, row),
                        'qc': add_quality_control(session, row)
                    }

                    # Create experiment
                    experiment = add_experiment(session, row, metadata_objects)
                    if not experiment:
                        logger.warning(f"Failed to create experiment for {row['name']}")
                        continue
                    
                    # Parse hap.py results if available
                    file_name = row.get('file_name')
                    if file_name and not pd.isna(file_name):
                        result = parse_happy_csv(file_name, experiment.id, session)
                        if result["success"]:
                            logger.info(f"Loaded results: {result['message']}")
                        else:
                            logger.warning(f"Failed to load results: {result.get('error')}")
                    
                    success_count += 1
                    logger.info(f"Completed setup for experiment {row.get('ID', index+1)}")
                    
                except Exception as e:
                    logger.error(f"Failed to process experiment {row.get('name', 'Unknown')}: {e}")
                    continue
            
        logger.info(f"Database population completed: {success_count}/{len(metadata_df)} experiments processed")
        return True
        
    except Exception as e:
        logger.error(f"Database population failed: {e}")
        return False
