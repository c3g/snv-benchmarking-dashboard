# ============================================================================
# direct_db_population.py
# ============================================================================
"""
Direct database population from metadata dictionaries.
Bypasses CSV file - creates database entries directly from upload form data.
"""

import logging
from datetime import datetime
from database import get_db_session
from models import *
from utils import clean_value, safe_float

logger = logging.getLogger(__name__)

# ============================================================================
# ENUM MAPPING
# ============================================================================

ENUM_MAPPINGS = {
    'technology': {
        'illumina': SeqTechName.ILLUMINA,
        'mgi': SeqTechName.MGI,
        'ont': SeqTechName.ONT,
        'pacbio': SeqTechName.PACBIO,
        '10x': SeqTechName.TENX,
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
        'gatk': CallerName.GATK,
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
    """Map string values to enum types"""
    if not value or value == '' or value is None:
        return None
    
    value_clean = str(value).strip().lower()
    
    if field_name not in ENUM_MAPPINGS:
        logger.warning(f"Unknown enum field: {field_name}")
        return None
    
    field_mappings = ENUM_MAPPINGS[field_name]
    
    if value_clean not in field_mappings:
        logger.warning(f"Unknown {field_name} value: '{value}'")
        return None
    
    return field_mappings[value_clean]

def map_boolean(value):
    """Convert string/bool to boolean"""
    if isinstance(value, bool):
        return value
    return str(value).lower() == 'true'

# ============================================================================
# RECORD CREATION
# ============================================================================

def get_or_create_record(session, model_class, filter_fields, all_fields=None):
    """
    Get existing record or create new one.
    
    Args:
        session: Database session
        model_class: SQLAlchemy model
        filter_fields: Fields to check for existing record
        all_fields: All fields for new record (defaults to filter_fields)
    
    Returns:
        Database record (existing or new)
    """
    if all_fields is None:
        all_fields = filter_fields
    
    existing = session.query(model_class).filter_by(**filter_fields).first()
    
    if existing:
        return existing
    
    new_record = model_class(**all_fields)
    session.add(new_record)
    session.flush()
    return new_record

def create_sequencing_tech(session, metadata):
    """Create SequencingTechnology record from metadata dict"""
    tech_enum = map_enum('technology', metadata.get('technology'))
    target_enum = map_enum('target', metadata.get('target', 'wgs'))
    platform_type_enum = map_enum('platform_type', metadata.get('platform_type'))
    
    if not tech_enum:
        return None
    
    filter_fields = {
        'technology': tech_enum,
        'target': target_enum,
        'platform_name': metadata.get('platform_name', ''),
        'platform_type': platform_type_enum
    }
    
    all_fields = {
        **filter_fields,
        'platform_version': metadata.get('platform_version')
    }
    
    return get_or_create_record(session, SequencingTechnology, filter_fields, all_fields)

def create_variant_caller(session, metadata):
    """Create VariantCaller record from metadata dict"""
    name_enum = map_enum('caller_name', metadata.get('caller_name'))
    type_enum = map_enum('caller_type', metadata.get('caller_type'))
    
    if not name_enum:
        return None
    
    filter_fields = {
        'name': name_enum,
        'version': metadata.get('caller_version', ''),
        'type': type_enum
    }
    
    all_fields = {
        **filter_fields,
        'model': metadata.get('caller_model')
    }
    
    return get_or_create_record(session, VariantCaller, filter_fields, all_fields)

def create_aligner(session, metadata):
    """Create Aligner record from metadata dict"""
    aligner_name = metadata.get('aligner_name')
    
    if not aligner_name or str(aligner_name).strip() == '':
        return None
    
    filter_fields = {
        'name': aligner_name,
        'version': metadata.get('aligner_version', '')
    }
    
    return get_or_create_record(session, Aligner, filter_fields)

def create_truth_set(session, metadata):
    """Create TruthSet record from metadata dict"""
    name_enum = map_enum('truth_set_name', metadata.get('truth_set_name'))
    reference_enum = map_enum('truth_set_reference', metadata.get('truth_set_reference'))
    sample_enum = map_enum('truth_set_sample', metadata.get('truth_set_sample', 'hg002'))
    
    if not name_enum:
        return None
    
    filter_fields = {
        'name': name_enum,
        'version': metadata.get('truth_set_version', ''),
        'sample': sample_enum,
        'reference': reference_enum
    }
    
    return get_or_create_record(session, TruthSet, filter_fields)

def create_benchmark_tool(session, metadata):
    """Create BenchmarkTool record from metadata dict"""
    tool_enum = map_enum('benchmark_tool_name', metadata.get('benchmark_tool_name', 'hap.py'))
    
    if not tool_enum:
        return None
    
    filter_fields = {
        'name': tool_enum,
        'version': metadata.get('benchmark_tool_version', '')
    }
    
    return get_or_create_record(session, BenchmarkTool, filter_fields)

def create_variant(session, metadata):
    """Create Variant record from metadata dict"""
    type_enum = map_enum('variant_type', metadata.get('variant_type', 'snp+indel'))
    size_enum = map_enum('variant_size', metadata.get('variant_size'))
    origin_enum = map_enum('variant_origin', metadata.get('variant_origin'))
    phased_bool = map_boolean(metadata.get('is_phased', False))
    
    filter_fields = {
        'type': type_enum,
        'size': size_enum,
        'origin': origin_enum,
        'is_phased': phased_bool
    }
    
    return get_or_create_record(session, Variant, filter_fields)

def create_chemistry(session, metadata):
    """Create Chemistry record from metadata dict"""
    chemistry_name = metadata.get('chemistry_name')
    
    if not chemistry_name or str(chemistry_name).strip() == '':
        return None
    
    tech_enum = map_enum('technology', metadata.get('technology'))
    
    filter_fields = {
        'name': chemistry_name,
        'sequencing_technology': tech_enum,
        'sequencing_platform': metadata.get('platform_name', ''),
        'version': metadata.get('chemistry_version')
    }
    
    return get_or_create_record(session, Chemistry, filter_fields)

def create_quality_control(session, metadata):
    """Create QualityControl record from metadata dict"""
    all_fields = {
        'mean_coverage': safe_float(metadata.get('mean_coverage')),
        'read_length': safe_float(metadata.get('read_length')),
        'mean_read_length': safe_float(metadata.get('mean_read_length')),
        'mean_insert_size': safe_float(metadata.get('mean_insert_size'))
    }
    
    if all(value is None for value in all_fields.values()):
        return None
    
    return get_or_create_record(session, QualityControl, all_fields)

# ============================================================================
# MAIN EXPERIMENT CREATION
# ============================================================================

def create_experiment_direct(metadata, experiment_id=None, filename=None):
    """
    Create experiment directly in database from metadata dict.
    This bypasses the CSV file completely.
    
    Args:
        metadata: Dictionary with experiment metadata from upload form
        experiment_id: Optional ID (will auto-generate if None)
        filename: Hap.py filename for this experiment
    
    Returns:
        dict: {"success": bool, "experiment_id": int, "message": str}
    """
    try:
        with get_db_session() as session:
            
            if experiment_id is None:
                max_id_result = session.query(func.max(Experiment.id)).scalar()
                experiment_id = (max_id_result or 0) + 1
                logger.info(f"Auto-generated experiment ID: {experiment_id}")
            
            existing = session.query(Experiment).filter_by(id=experiment_id).first()
            if existing:
                return {
                    "success": False,
                    "experiment_id": None,
                    "message": f"Experiment ID {experiment_id} already exists"
                }
            
            logger.info(f"Creating metadata objects for experiment: {metadata.get('exp_name', 'Unknown')}")
            
            seq_tech = create_sequencing_tech(session, metadata)
            caller = create_variant_caller(session, metadata)
            aligner = create_aligner(session, metadata)
            truth_set = create_truth_set(session, metadata)
            benchmark_tool = create_benchmark_tool(session, metadata)
            variant = create_variant(session, metadata)
            chemistry = create_chemistry(session, metadata)
            qc = create_quality_control(session, metadata)
            
            created_at = metadata.get('created_at')
            if created_at and isinstance(created_at, str):
                try:
                    created_at = datetime.strptime(created_at, '%Y-%m-%d')
                except:
                    created_at = datetime.now()
            elif not created_at:
                created_at = datetime.now()
            
            experiment = Experiment(
                id=experiment_id,
                name=metadata.get('exp_name', 'Unnamed Experiment'),
                description=metadata.get('description', f"Experiment {metadata.get('exp_name', '')}"),
                created_at=created_at,
                
                owner_id=metadata.get('owner_id') if isinstance(metadata.get('owner_id'), int) else None,  # User database ID----------------------------------------                 
                is_public=metadata.get('is_public', True),                   # Visibility flag
                created_by_username=metadata.get('owner_username'),          # Username backup
        

                sequencing_technology_id=seq_tech.id if seq_tech else None,
                variant_caller_id=caller.id if caller else None,
                aligner_id=aligner.id if aligner else None,
                truth_set_id=truth_set.id if truth_set else None,
                benchmark_tool_id=benchmark_tool.id if benchmark_tool else None,
                variant_id=variant.id if variant else None,
                chemistry_id=chemistry.id if chemistry else None,
                quality_control_metrics_id=qc.id if qc else None
            )
            
            session.add(experiment)
            session.flush()
            
            if filename:
                from happy_parser import parse_happy_csv
                result = parse_happy_csv(filename, experiment.id, session)
                
                if result["success"]:
                    logger.info(f"Loaded hap.py results: {result['message']}")
                else:
                    logger.warning(f"Failed to load hap.py results: {result.get('error')}")
            
            logger.info(f"Successfully created experiment ID {experiment_id}: {experiment.name}")
            
            return {
                "success": True,
                "experiment_id": experiment_id,
                "message": f"Experiment created successfully (ID: {experiment_id})"
            }
            
    except Exception as e:
        logger.error(f"Failed to create experiment: {e}")
        return {
            "success": False,
            "experiment_id": None,
            "message": f"Database error: {str(e)}"
        }