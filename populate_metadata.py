"""
populate_metadata.py

Parses experiment metadata from CSV files and populates the normalized database tables.
Handles data validation, enum mapping, and prevents duplicate records.

Main workflow:
1. Load CSV file containing experiment metadata
2. Clean and validate each data value
3. Map string values to database enums
4. Create database records (skipping duplicates)
5. Link all metadata through the Experiment table
"""

import pandas as pd
from database import get_db_session
from models import *
from config import METADATA_CSV_PATH
from happy_parser import parse_happy_csv

# Path to metadata CSV file
metadata_CSV_file_path = METADATA_CSV_PATH

#Loads CSV file and returns pandas metadata dataframe
def load_csv_metadata(file_path):
    try:
        metadata_df = pd.read_csv(file_path)
        return metadata_df
    except Exception as e:
        print(f"Error loading CSV: {e}")
        return None

# ============================================================================
# CLEAN AND CONVERT ENTRY VALUE
# ============================================================================

def clean_value(value):
    """Cleans entry values and converts them to lowercase Strings, used by enum mappers"""
    if value is None or pd.isna(value): 
        return None
    return str(value).strip().lower()

def clean_dataframe_strings(df):
    """
    Clean all string columns in the DataFrame to lowercase.
    Leaves numeric columns unchanged.
    """
    cleaned_df = df.copy()
    
    # Columns to exclude from cleaning (keep original case for display)
    exclude_columns = ['name', 'description', 'platform_name', 'chemistry']

    # Clean only string columns
    string_columns = cleaned_df.select_dtypes(include=['object']).columns
    for col in string_columns:
            if col not in exclude_columns:
                cleaned_df[col] = cleaned_df[col].apply(clean_value)
    
    return cleaned_df

# Converts strings to floats while handling commas
def safe_float(value):
    if value is None or pd.isna(value):
        return None
    try:
        str_value = str(value).replace(',', '')
        return float(str_value)
    except (ValueError, TypeError):
        return None
    
# ============================================================================
# METADATA ENUM MAPPERS (Case Insensitive)
# ============================================================================
# Dictionary mapping CSV string values to corresponding database enum values

ENUM_MAPPINGS = {
    'technology': {
        'illumina': SeqTechName.ILLUMINA,
        'mgi': SeqTechName.MGI,
        'ont': SeqTechName.ONT,
        'pacbio': SeqTechName.PACBIO,
    },
    'target': {
        'wgs': SeqTechTarget.WGS,
        'wes': SeqTechTarget.WES,
    },
    'platform_type': {
        'srs': SeqTechPlatformType.SRS,
        'lrs': SeqTechPlatformType.LRS
    },
    'caller_name': {
        'deepvariant': CallerName.DEEPVARIANT,
        'gatk': CallerName.GATK,
        'clair3': CallerName.CLAIR3,
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


def map_enum(enum_type, value):
    """Convert CSV string value to corresponding database enum."""
    return ENUM_MAPPINGS.get(enum_type, {}).get(clean_value(value))

# Booleans (i.e. Is_Phased)
def map_boolean(value):
    """Convert string to boolean"""
    if clean_value(value) == 'true':
        return True
    else:
        return False

# ============================================================================
# GENERIC METADATA ADDING FUNCTION
# ============================================================================

def add_record_if_not_exists(model_class, filter_fields, all_fields, record_name):
    """
    Generic function to add database record only if it doesn't already exist.
    Prevents duplicate records and works with any SQLAlchemy model.
    
    Args:
        model_class: SQLAlchemy model class (e.g., SequencingTechnology)
        filter_fields (dict): Fields to check for existing records
        all_fields (dict): Complete field set for new record creation
        record_name (str): Description for logging purposes
        
    Returns:
        SQLAlchemy object: New or existing record instance
    """
    try:
        with get_db_session() as session:
            # Check if record already exists by starting query on model table
            existing = session.query(model_class).filter_by(**filter_fields).first()
            
            if not existing:
                # creates a new instance of the model and stages it for insertion
                new_record = model_class(**all_fields)
                session.add(new_record)
                print(f"Added {record_name}: {' '.join(str(v) for v in filter_fields.values() if v)}")
                return new_record
            else:
                print(f"Already exists {record_name}: {' '.join(str(v) for v in filter_fields.values() if v)}")
                return existing
                
    except Exception as e:
        print(f"Error adding {record_name}: {e}")
        return None

# ============================================================================
# METADATA ADDERS
# ============================================================================

def add_sequencing_tech(row):
    """
    Create SequencingTechnology record from CSV row. (if not already exist)
    """
    # Map fields to enums
    tech_enum = map_enum('technology', row['technology'])
    target_enum = map_enum('target', row['target'])
    type_enum = map_enum('platform_type', row['platform_type'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'technology': tech_enum,
        'platform_name': row['platform_name'],
    
    }
    # Complete record data
    all_fields = {
        **filter_fields,
        'target': target_enum,
        'platform_type': type_enum,
        'platform_version': row['platform_version']
    }
    
    return add_record_if_not_exists(SequencingTechnology, filter_fields, all_fields, "sequencing tech")

def add_variant_caller(row):
    """
    Create VariantCaller record from CSV row.
    """
    # Map fields to enums
    name_enum = map_enum('caller_name', row['caller_name'])
    type_enum = map_enum('caller_type', row['caller_type'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'name': name_enum,
        'version': row['caller_version']
    }

    # Complete record data
    all_fields = {
        **filter_fields,
        'type': type_enum,
        'model': row.get('caller_model', None)
    }
    return add_record_if_not_exists(VariantCaller, filter_fields, all_fields, "variant caller")

def add_aligner(row):
    """
    Create Aligner record from CSV row.
    """
    # Fields used to check for duplicates
    filter_fields = {
        'name': row['aligner_name'],
        'version': row['aligner_version']
    }
    
    return add_record_if_not_exists(Aligner, filter_fields, filter_fields, "aligner")

def add_truth_set(row):
    """
    Create TruthSet record from CSV row.
    """
    # Map fields to enums
    name_enum = map_enum('truth_set_name', row['truth_set_name'])
    reference_enum = map_enum('truth_set_reference', row['truth_set_reference'])
    sample_enum = map_enum('truth_set_sample', row['truth_set_sample'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'name': name_enum,
        'version': row['truth_set_version'],
        'sample': sample_enum,
        'reference': reference_enum
    }
    
    return add_record_if_not_exists(TruthSet, filter_fields, filter_fields, "truth set")

def add_benchmark_tool(row):
    """
    Create BenchmarkTool record from CSV row.
    """
    # Map fields to enums
    tool_enum = map_enum('benchmark_tool_name', row['benchmark_tool_name'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'name': tool_enum,
        'version': row['benchmark_tool_version']
    }
    
    return add_record_if_not_exists(BenchmarkTool, filter_fields, filter_fields, "benchmark tool")

def add_variant(row):
    """
    Create Variant record from CSV row.
    """
    # Map fields to enums
    type_enum = map_enum('variant_type', row['variant_type'])
    size_enum = map_enum('variant_size', row['variant_size'])
    origin_enum = map_enum('variant_origin', row['variant_origin'])
    phased_bool = map_boolean(row['is_phased'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'type': type_enum,
        'size': size_enum,
        'origin': origin_enum,
        'is_phased': phased_bool
    }
    
    return add_record_if_not_exists(Variant, filter_fields, filter_fields, "variant")

def add_chemistry(row):
    """
    Create Chemistry record from CSV row.
    """
    # Map fields to enums
    tech_enum = map_enum('technology', row['technology'])
    
    # Fields used to check for duplicates
    filter_fields = {
        'name': row['chemistry_name'],
        'sequencing_technology': tech_enum,
        'sequencing_platform': row['platform_name'],
        'version': row.get('chemistry_version', None)
    }
    
    return add_record_if_not_exists(Chemistry, filter_fields, filter_fields, "chemistry")

def add_quality_control(row):
    """
    Create Quality Control Metrics from CSV row.
    """
    all_fields = {
        'mean_coverage': safe_float(row.get('mean_coverage', None)),
        'read_length': safe_float(row.get('read_length', None)),
        'mean_read_length': safe_float(row.get('mean_read_length', None)),
        'mean_insert_size': safe_float(row.get('mean_insert_size', None))
    }
    
    # For QC metrics, we create unique entries each time (no filtering)
    try:
        with get_db_session() as session:
            new_qc = QualityControl(**all_fields)
            session.add(new_qc)
            print(f"Added QC metrics: coverage={row.get('mean_coverage')}, read_length={row.get('read_length')}")
            return new_qc
    except Exception as e:
        print(f"Error adding quality control: {e}")
        return None

def add_experiment(row):
    """
    Create Experiment record linking all metadata via foreign keys.
    """
    
    experiment_name = row['name']
    description = row.get('description', f"Benchmarking experiment for {experiment_name}")
    
    try:
        with get_db_session() as session:

            # Find all related records by querying each reference table
            seq_tech = session.query(SequencingTechnology).filter_by(
                technology=map_enum('technology', row['technology']),
                platform_name=row['platform_name'],
                platform_type=map_enum('platform_type', row['platform_type'])
            ).first()
            
            variant_caller = session.query(VariantCaller).filter_by(
                name=map_enum('caller_name', row['caller_name']),
                version=row['caller_version']
            ).first()
            
            aligner = session.query(Aligner).filter_by(
                name=row['aligner_name'],
                version=row['aligner_version']
            ).first()
            
            truth_set = session.query(TruthSet).filter_by(
                name=map_enum('truth_set_name', row['truth_set_name']),
                sample=map_enum('truth_set_sample', row['truth_set_sample']),
                version=row['truth_set_version']
            ).first()
            
            benchmark_tool = session.query(BenchmarkTool).filter_by(
                name=map_enum('benchmark_tool_name', row['benchmark_tool_name']),
                version=row['benchmark_tool_version']
            ).first()
            
            variant = session.query(Variant).filter_by(
                type=map_enum('variant_type', row['variant_type']),
                size=map_enum('variant_size', row['variant_size']),
                origin=map_enum('variant_origin', row['variant_origin']),
                is_phased=map_boolean(row['is_phased'])
            ).first()
            
            chemistry = session.query(Chemistry).filter_by(
                name=row['chemistry_name'],
                sequencing_technology=map_enum('technology', row['technology']),
                sequencing_platform=row['platform_name']
            ).first()
            
            quality_control = session.query(QualityControl).filter_by(
                mean_coverage=safe_float(row.get('mean_coverage')),
                read_length=safe_float(row.get('read_length')),
                mean_read_length=safe_float(row.get('mean_read_length')),
                mean_insert_size=safe_float(row.get('mean_insert_size'))
            ).first()
            
            # Create experiment with foreign key references
            new_experiment = Experiment(
                name=experiment_name,
                description=description,
                sequencing_technology_id=seq_tech.id if seq_tech else None,
                variant_caller_id=variant_caller.id if variant_caller else None,
                aligner_id=aligner.id if aligner else None,
                truth_set_id=truth_set.id if truth_set else None,
                benchmark_tool_id=benchmark_tool.id if benchmark_tool else None,
                variant_id=variant.id if variant else None,
                chemistry_id=chemistry.id if chemistry else None,
                quality_control_metrics_id=quality_control.id if quality_control else None
            )
            
            session.add(new_experiment)
            print(f"Added experiment: {experiment_name}")
                     
    except Exception as e:
        print(f"Error adding experiment {experiment_name}: {e}")


# ============================================================================
# POPULATE METADATA
# ============================================================================

def populate_database_from_csv(file_path=metadata_CSV_file_path):
    """
    Main function to populate the entire database from CSV metadata AND hap.py results
    Processes each row and calls all population functions in order.
    
    Args:
        file_path (str): Path to metadata CSV file
        
    Returns:
        bool: True if successful, False if errors occurred
    """

    # Load CSV file
    raw_df = load_csv_metadata(file_path)
    if raw_df is None:
        return False
    
    # Clean the entire DataFrame
    metadata_df = clean_dataframe_strings(raw_df)
    
    print(f"Loaded CSV with {len(metadata_df)} rows")
    print(f"Columns: {list(metadata_df.columns)}")
    
    # Process each row
    for index, row in metadata_df.iterrows():
        print(f"\nProcessing row {index + 1}: {row.get('name', 'Unknown')}")
        
        # Add data to each table
        add_sequencing_tech(row)
        add_variant_caller(row)
        add_aligner(row)
        add_truth_set(row)
        add_benchmark_tool(row)
        add_variant(row)
        add_chemistry(row)
        add_quality_control(row)
        add_experiment(row)
        
        experiment_id = row['ID']
        file_name = row['file_name']
        
        if file_name and not pd.isna(file_name):
            result = parse_happy_csv(file_name, experiment_id)
            if result["success"]:
                print(f"Loaded results: {result['message']}")
            else:
                print(f"Failed to load results: {result.get('error')}")
        
        print(f"✅ Complete setup for: {row['name']}\n")
    return True