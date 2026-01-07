# ============================================================================
# db_interface.py
# ============================================================================
"""
Database interface and query functions for SNV Benchmarking Dashboard frontend (R) to access the data.

Main components:
- Experiment overview and metadata retrieval
- Performance data queries
- Stratified analysis data retrieval  
- Technology and caller-based filtering
- JSON parameter handling for R Shiny integration
"""

import pandas as pd
import json
import logging
from sqlalchemy.orm import joinedload
from database import get_db_session
from models import *
from sqlalchemy import or_

logger = logging.getLogger(__name__)

# ============================================================================
# db_interface.py
# ============================================================================
def parse_experiment_ids(experiment_ids_param):
    """
    Parse experiment IDs from various input formats (JSON string, list, or single value) and returns a list.
    """
    if experiment_ids_param is None:
        return []
        
    try:
        if isinstance(experiment_ids_param, str):
            experiment_ids = json.loads(experiment_ids_param)
            if not isinstance(experiment_ids, list):
                experiment_ids = [experiment_ids]  # Convert single value to list
        elif isinstance(experiment_ids_param, list):
            experiment_ids = experiment_ids_param
        else:
            # Single value (int/float)
            experiment_ids = [experiment_ids_param]
            
        return experiment_ids
        
    except json.JSONDecodeError as e:
        logger.error(f"Error parsing experiment_ids JSON: {e}")
        return []
    except Exception as e:
        logger.error(f"Error processing experiment_ids: {e}")
        return []

# ============================================================================
# EXPERIMENT OVERVIEW AND METADATA
# ============================================================================

def get_experiments_overview(filters=None, experiment_ids_param=None, current_user_id=None, is_admin=False):
    """
    Get basic experiment information for dashboard overview table.
    
    Retrieves experiments key metadata for the main dashboard table. 
    Supports filtering by technology/caller and specific experiment ID selection.
    
    Args:
        filters (dict): Optional filters like {'technology': 'ILLUMINA', 'caller': 'DEEPVARIANT'}
        experiment_ids_param (str/list): JSON string or list of specific experiment IDs (picked in frontend)
        
    Returns:
        pandas.DataFrame: Experiment overview data with columns: id, name, technology, 
                         platform_name, caller, caller_version, chemistry, truth_set, sample, created_at
    """
    # Parse JSON IDs
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    # Get metadata from database
    try:
        with get_db_session() as session:
            # Base query with necessary joins
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.truth_set), 
                joinedload(Experiment.chemistry),
                joinedload(Experiment.owner)  # Include owner info

            )
            
            # ACCESS CONTROL FILTER
            if not is_admin:
                if current_user_id is not None:
                    # Logged-in non-admin: show public experiments OR their own private ones
                    query = query.filter(
                        or_(
                            Experiment.is_public == True,
                            Experiment.owner_id == current_user_id
                        )
                    )
                else:
                    # Not logged in: show only public experiments
                    query = query.filter(Experiment.is_public == True)
                    

            # If specific experiment IDs provided, filter by them first
            if experiment_ids and len(experiment_ids) > 0:
                query = query.filter(Experiment.id.in_(experiment_ids))
            
            # Apply additional filters if provided
            if filters:
                if 'technology' in filters:
                    try:
                        tech_enum = SeqTechName(filters['technology'].upper())
                        query = query.join(SequencingTechnology).filter(
                            SequencingTechnology.technology == tech_enum
                        )
                    except ValueError:
                        logger.error(f"Invalid technology filter: {filters['technology']}")
                        return pd.DataFrame()
                
                if 'caller' in filters:
                    try:
                        caller_enum = CallerName(filters['caller'].upper())
                        query = query.join(VariantCaller).filter(
                            VariantCaller.name == caller_enum
                        )
                    except ValueError:
                        logger.error(f"Invalid caller filter: {filters['caller']}")
                        return pd.DataFrame()
            
            experiments = query.all()
            
            # Extract essential data
            data = []
            for exp in experiments:
                data.append({ 
                    'id': exp.id,
                    'name': exp.name,
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else "N/A",
                    'platform_name': exp.sequencing_technology.platform_name if (exp.sequencing_technology and exp.sequencing_technology.platform_name) else "N/A",
                    'caller': exp.variant_caller.name.value if exp.variant_caller else "N/A",
                    'caller_version': exp.variant_caller.version if (exp.variant_caller and exp.variant_caller.version) else "N/A",
                    'chemistry': exp.chemistry.name if (exp.chemistry and exp.chemistry.name) else "N/A",
                    'truth_set': exp.truth_set.name.value if exp.truth_set else "N/A",
                    'sample': exp.truth_set.sample.value if exp.truth_set else "N/A",
                    'created_at': exp.created_at.strftime('%Y-%m-%d') if exp.created_at else "N/A",
                    'is_public': exp.is_public if exp.is_public is not None else True,
                    'owner_username': exp.created_by_username if exp.created_by_username else ''

                    
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_experiments_overview: {e}")
        return pd.DataFrame()

def get_experiment_metadata(experiment_ids_param):
    """
    Get complete metadata for specific experiments.
    
    Retrieves detailed metadata for selected experiments including all related
    tables (technology, caller, aligner, truth set, etc.). 
    Used in Tab 1 (row expansion)
    
    Args:
        experiment_ids_param (str/list): JSON string or list of experiment IDs to get metadata for
        
    Returns:
        pandas.DataFrame: Complete metadata for selected experiments with all available fields
    """
    # Parse JSON IDs
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    # check if list is empty
    if not experiment_ids or len(experiment_ids) == 0:
        logger.warning("No experiment IDs provided to get_experiment_metadata")
        return pd.DataFrame()
    
    # Get full metadata   
    try:
        with get_db_session() as session:
            # Complete query of all tables with metadata joins
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.aligner),
                joinedload(Experiment.truth_set),
                joinedload(Experiment.benchmark_tool),
                joinedload(Experiment.variant),
                joinedload(Experiment.chemistry),
                joinedload(Experiment.quality_control)
            ).filter(Experiment.id.in_(experiment_ids))
            
            experiments = query.all()
            
            # Extract complete metadata
            data = []
            for exp in experiments:
                data.append({
                    # Basic info
                    'id': exp.id,
                    'name': exp.name,
                    'description': exp.description,
                    'created_at': exp.created_at.isoformat() if exp.created_at else None,
                    
                    # Sequencing Technology
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else None,
                    'target': exp.sequencing_technology.target.value if exp.sequencing_technology else None,
                    'platform_name': exp.sequencing_technology.platform_name if exp.sequencing_technology else None,
                    'platform_type': exp.sequencing_technology.platform_type.value if exp.sequencing_technology else None,
                    'platform_version': exp.sequencing_technology.platform_version if exp.sequencing_technology else None,
                    
                    # Variant Caller
                    'caller': exp.variant_caller.name.value if exp.variant_caller else None,
                    'caller_type': exp.variant_caller.type.value if exp.variant_caller else None,
                    'caller_version': exp.variant_caller.version if exp.variant_caller else None,
                    'caller_model': exp.variant_caller.model if exp.variant_caller else None,
                    
                    # Aligner
                    'aligner_name': exp.aligner.name if exp.aligner else None,
                    'aligner_version': exp.aligner.version if exp.aligner else None,
                    
                    # Truth Set
                    'truth_set_name': exp.truth_set.name.value if exp.truth_set else None,
                    'truth_set_sample': exp.truth_set.sample.value if exp.truth_set else None,
                    'truth_set_version': exp.truth_set.version if exp.truth_set else None,
                    'truth_set_reference': exp.truth_set.reference.value if exp.truth_set else None,
                    
                    # Benchmark Tool
                    'benchmark_tool_name': exp.benchmark_tool.name.value if exp.benchmark_tool else None,
                    'benchmark_tool_version': exp.benchmark_tool.version if exp.benchmark_tool else None,
                    
                    # Variant Info
                    'variant_type': exp.variant.type.value if exp.variant else None,
                    'variant_origin': exp.variant.origin.value if exp.variant else None,
                    'variant_size': exp.variant.size.value if exp.variant else None,
                    'is_phased': exp.variant.is_phased if exp.variant else None,
                    
                    # Quality Control Metrics
                    'mean_coverage': float(exp.quality_control.mean_coverage) if (exp.quality_control and exp.quality_control.mean_coverage is not None) else None,
                    'read_length': float(exp.quality_control.read_length) if (exp.quality_control and exp.quality_control.read_length is not None) else None,
                    'mean_read_length': float(exp.quality_control.mean_read_length) if (exp.quality_control and exp.quality_control.mean_read_length is not None) else None,
                    'mean_insert_size': float(exp.quality_control.mean_insert_size) if (exp.quality_control and exp.quality_control.mean_insert_size is not None) else None,
                    
                    # Chemistry
                    'chemistry_name': exp.chemistry.name if exp.chemistry else None,
                    'chemistry_version': exp.chemistry.version if exp.chemistry else None,
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_experiment_metadata: {e}")
        return pd.DataFrame()

# ============================================================================
# PERFORMANCE DATA QUERIES
# ============================================================================

def get_experiments_with_performance(experiment_ids_param, variant_types=['SNP', 'INDEL']):
    """
    Get performance data combined with metadata for selected experiments.
    
    Joins performance results from OverallResult table with complete experiment metadata. 
    Used for performance tables and visualizations in the dashboard (Tabs 2 and 3).
    
    Args:
        experiment_ids_param (str/list): JSON string or list of experiment IDs
        variant_types (list): List of variant types to include (default: ['SNP', 'INDEL'])
        
    Returns:
        pandas.DataFrame: Combined performance metrics and metadata for each experiment/variant type
    """
    # Parse JSON IDs
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    if not experiment_ids:
        logger.warning("No valid experiment IDs provided to get_experiments_with_performance")
        return pd.DataFrame()

    try:
        with get_db_session() as session:
            query = session.query(
                # Performance metrics 
                OverallResult.experiment_id,
                OverallResult.variant_type,
                OverallResult.metric_recall.label('recall'),
                OverallResult.metric_precision.label('precision'),
                OverallResult.metric_f1_score.label('f1_score'),
                OverallResult.truth_total,
                OverallResult.truth_tp,
                OverallResult.truth_fn,
                OverallResult.query_total,
                OverallResult.query_tp,
                OverallResult.query_fp,
                
                # Basic experiment info
                Experiment.name.label('experiment_name'),
                Experiment.description,
                Experiment.created_at,
                
                # Sequencing Technology info
                SequencingTechnology.technology.label('technology'),
                SequencingTechnology.target,
                SequencingTechnology.platform_name,
                SequencingTechnology.platform_type,
                SequencingTechnology.platform_version,
                
                # Variant Caller info
                VariantCaller.name.label('caller'),
                VariantCaller.type.label('caller_type'),
                VariantCaller.version.label('caller_version'),
                VariantCaller.model.label('caller_model'),
                
                # Aligner info
                Aligner.name.label('aligner_name'),
                Aligner.version.label('aligner_version'),
                
                # Truth Set info
                TruthSet.name.label('truth_set_name'),
                TruthSet.sample.label('truth_set_sample'),
                TruthSet.version.label('truth_set_version'),
                TruthSet.reference.label('truth_set_reference'),
                
                # Benchmark Tool info
                BenchmarkTool.name.label('benchmark_tool_name'),
                BenchmarkTool.version.label('benchmark_tool_version'),
                
                # Variant info
                Variant.type.label('variant_type_detail'),
                Variant.origin.label('variant_origin'),
                Variant.size.label('variant_size'),
                Variant.is_phased,
                
                # Quality Control metrics
                QualityControl.mean_coverage,
                QualityControl.read_length,
                QualityControl.mean_read_length,
                QualityControl.mean_insert_size,
                
                # Chemistry info
                Chemistry.name.label('chemistry_name'),
                Chemistry.version.label('chemistry_version')
                
            ).select_from(OverallResult).join(
                # Required joins
                Experiment, OverallResult.experiment_id == Experiment.id
            ).join(
                SequencingTechnology, Experiment.sequencing_technology_id == SequencingTechnology.id
            ).join(
                VariantCaller, Experiment.variant_caller_id == VariantCaller.id
            ).outerjoin(
                # Optional joins (outerjoin to avoid missing data)
                Aligner, Experiment.aligner_id == Aligner.id
            ).outerjoin(
                TruthSet, Experiment.truth_set_id == TruthSet.id
            ).outerjoin(
                BenchmarkTool, Experiment.benchmark_tool_id == BenchmarkTool.id
            ).outerjoin(
                Variant, Experiment.variant_id == Variant.id
            ).outerjoin(
                QualityControl, Experiment.quality_control_metrics_id == QualityControl.id
            ).outerjoin(
                Chemistry, Experiment.chemistry_id == Chemistry.id
            ).filter(
                OverallResult.experiment_id.in_(experiment_ids),
                OverallResult.variant_type.in_(variant_types)
            )
            
            results = query.all()
            
            if not results:
                logger.info(f"No performance data found for experiment IDs: {experiment_ids}")
                return pd.DataFrame()
            
            # Convert to DataFrame 
            data = []
            for result in results:
                data.append({
                    # Performance metrics
                    'experiment_id': result.experiment_id,
                    'experiment_name': result.experiment_name,
                    'variant_type': result.variant_type,
                    'recall': result.recall,
                    'precision': result.precision,
                    'f1_score': result.f1_score,
                    'truth_total': result.truth_total,
                    'truth_tp': result.truth_tp,
                    'truth_fn': result.truth_fn,
                    'query_total': result.query_total,
                    'query_tp': result.query_tp,
                    'query_fp': result.query_fp,
                    
                    # Basic info
                    'description': result.description,
                    'created_at': result.created_at.isoformat() if result.created_at else None,
                    
                    # Technology info
                    'technology': result.technology.value if result.technology else 'Unknown',
                    'target': result.target.value if result.target else None,
                    'platform_name': result.platform_name,
                    'platform_type': result.platform_type.value if result.platform_type else None,
                    'platform_version': result.platform_version,
                    
                    # Caller info
                    'caller': result.caller.value if result.caller else 'Unknown',
                    'caller_type': result.caller_type.value if result.caller_type else None,
                    'caller_version': result.caller_version,
                    'caller_model': result.caller_model,
                    
                    # Aligner info
                    'aligner_name': result.aligner_name,
                    'aligner_version': result.aligner_version,
                    
                    # Truth Set info
                    'truth_set_name': result.truth_set_name.value if result.truth_set_name else None,
                    'truth_set_sample': result.truth_set_sample.value if result.truth_set_sample else None,
                    'truth_set_version': result.truth_set_version,
                    'truth_set_reference': result.truth_set_reference.value if result.truth_set_reference else None,
                    
                    # Benchmark Tool info
                    'benchmark_tool_name': result.benchmark_tool_name.value if result.benchmark_tool_name else None,
                    'benchmark_tool_version': result.benchmark_tool_version,
                    
                    # Variant info
                    'variant_type_detail': result.variant_type_detail.value if result.variant_type_detail else None,
                    'variant_origin': result.variant_origin.value if result.variant_origin else None,
                    'variant_size': result.variant_size.value if result.variant_size else None,
                    'is_phased': result.is_phased,
                    
                    # Quality Control metrics
                    'mean_coverage': float(result.mean_coverage) if result.mean_coverage is not None else None,
                    'read_length': float(result.read_length) if result.read_length is not None else None,
                    'mean_read_length': float(result.mean_read_length) if result.mean_read_length is not None else None,
                    'mean_insert_size': float(result.mean_insert_size) if result.mean_insert_size is not None else None,
                    
                    # Chemistry info
                    'chemistry_name': result.chemistry_name,
                    'chemistry_version': result.chemistry_version
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_experiments_with_performance: {e}")
        import traceback
        traceback.print_exc()
        return pd.DataFrame()

def get_stratified_performance_by_regions(experiment_ids_param, variant_types=['SNP', 'INDEL'], regions=None):
    """
    Get stratified performance results filtered by specific genomic regions.
    
    Retrieves detailed performance metrics across different genomic regions
    (easy/difficult, GC content, functional regions, etc.) for stratified analysis (Tab 4)
    Only loads data for user-selected regions.
    
    Args:
        experiment_ids_param (str/list): JSON string or list of experiment IDs
        variant_types (list): List of variant types to include (default: ['SNP', 'INDEL'])  
        regions (list): List of region names to filter by (e.g., ['All Regions', 'Easy Regions'])
        
    Returns:
        pandas.DataFrame: Stratified performance data with experiment metadata
    """
    
    # Parse JSON IDs
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    try:
        with get_db_session() as session:
            query = session.query(BenchmarkResult).options(
                joinedload(BenchmarkResult.experiment).joinedload(Experiment.sequencing_technology),
                joinedload(BenchmarkResult.experiment).joinedload(Experiment.variant_caller),
                joinedload(BenchmarkResult.experiment).joinedload(Experiment.chemistry)
            ).filter(
                BenchmarkResult.experiment_id.in_(experiment_ids),
                BenchmarkResult.variant_type.in_(variant_types)
            )

            # Filter by regions if specified
            if regions and len(regions) > 0:
                region_enums = []
                for region_name in regions:
                    # Try display name first, then fall back to original string
                    region_enum = RegionType.from_display_name(region_name) or RegionType.from_string(region_name)
                    if region_enum:
                        region_enums.append(region_enum)
                
                if region_enums:
                    query = query.filter(BenchmarkResult.subset.in_(region_enums))
                else:
                    logger.warning(f"No valid regions found for: {regions}")
                    return pd.DataFrame()
            
            results = query.all()
            
            # Convert to dataframe with metadata included
            data = []
            for result in results:
                data.append({
                    'experiment_id': result.experiment_id,
                    'experiment_name': result.experiment.name if result.experiment else None,
                    'variant_type': result.variant_type,
                    'technology': result.experiment.sequencing_technology.technology.value if (result.experiment and result.experiment.sequencing_technology) else 'Unknown',
                    'caller': result.experiment.variant_caller.name.value if (result.experiment and result.experiment.variant_caller) else 'Unknown',
                    'caller_version': result.experiment.variant_caller.version if result.experiment.variant_caller else None,
                    'platform_name': result.experiment.sequencing_technology.platform_name if result.experiment.sequencing_technology else None,
    
                    'subset': result.subset.value,
                    'filter_type': result.filter_type,
                    'chemistry_name': result.experiment.chemistry.name if (result.experiment and result.experiment.chemistry and result.experiment.chemistry.name) else None,
                    
                    # Performance metrics
                    'recall': result.metric_recall,
                    'precision': result.metric_precision,
                    'f1_score': result.metric_f1_score,
                    
                    # Essential counts
                    'truth_total': result.truth_total,
                    'truth_tp': result.truth_tp,
                    'truth_fn': result.truth_fn,
                    'query_total': result.query_total,
                    'query_tp': result.query_tp,
                    'query_fp': result.query_fp,
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_stratified_performance_by_regions: {e}")
        import traceback
        traceback.print_exc()
        return pd.DataFrame()

# ============================================================================
# FILTERING FUNCTIONS
# ============================================================================

def get_experiments_by_technology(technology):
    """
    Get experiment IDs that match a specific sequencing technology.
    
    Args:
        technology (str): Technology name - "ILLUMINA", "PACBIO", "ONT", or "MGI"
        
    Returns:
        list: List of experiment IDs matching the technology
    """
    try:
        with get_db_session() as session:
            # Convert string to enum
            try:
                tech_enum = SeqTechName(technology.strip().upper())
            except ValueError:
                logger.error(f"Invalid technology: {technology}")
                logger.info(f"Valid options: {[t.value for t in SeqTechName]}")
                return []
            
            # Query joining Experiment with Sequencing technologies
            query = session.query(Experiment.id).join(
                SequencingTechnology
            ).filter(
                SequencingTechnology.technology == tech_enum
            )
            
            # Extract IDs
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        logger.error(f"Error getting experiments by technology: {e}")
        return []

def get_experiments_by_caller(caller):
    """
    Get experiment IDs that match a specific variant caller.
    
    Args:
        caller (str): Caller name - "DEEPVARIANT", "GATK", or "CLAIR3"
        
    Returns:
        list: List of experiment IDs matching the caller
    """
    try:
        with get_db_session() as session:
            # Convert string to enum
            try:
                caller_enum = CallerName(caller.strip().upper())
            except ValueError:
                logger.error(f"Invalid caller: {caller}")
                logger.info(f"Valid options: {[c.value for c in CallerName]}")
                return []
            
            # Query joining Experiment with variant caller
            query = session.query(Experiment.id).join(
                VariantCaller
            ).filter(
                VariantCaller.name == caller_enum
            )
            
            # Extract IDs
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        logger.error(f"Error getting experiments by caller: {e}")
        return []

# ============================================================================
# INDIVIDUAL EXPERIMENT LOOKUPS
# ============================================================================

def get_technology(experiment_id):
    """
    Get sequencing technology name for a specific experiment.
    
    Args:
        experiment_id (int): Single experiment ID
        
    Returns:
        str or None: Technology name (e.g., "ILLUMINA", "PACBIO") or None if not found
    """
    try:
        with get_db_session() as session:
            # Query experiment with technology join
            experiment = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology)
            ).filter(Experiment.id == experiment_id).first()
            
            if experiment and experiment.sequencing_technology and experiment.sequencing_technology.technology:
                return experiment.sequencing_technology.technology.value
            else:
                return None
            
    except Exception as e:
        logger.error(f"Error getting technology for experiment {experiment_id}: {e}")
        return None

def get_caller(experiment_id):
    """
    Get variant caller name for a specific experiment.
    
    Args:
        experiment_id (int): Single experiment ID
        
    Returns:
        str or None: Caller name (e.g., "DEEPVARIANT", "GATK") or None if not found
    """
    try:
        with get_db_session() as session:
            # Query experiment with caller join
            experiment = session.query(Experiment).options(
                joinedload(Experiment.variant_caller)
            ).filter(Experiment.id == experiment_id).first()
            
            if experiment and experiment.variant_caller and experiment.variant_caller.name:
                return experiment.variant_caller.name.value
            else:
                return None
            
    except Exception as e:
        logger.error(f"Error getting caller for experiment {experiment_id}: {e}")
        return None

# ============================================================================
# DYNAMIC DROPDOWN OPTIONS
# ============================================================================

def get_distinct_technologies():
    """
    Get all unique technology names from database.
    
    Returns:
        list: Sorted list of technology names (e.g., ["ILLUMINA", "ONT", "PACBIO"])
    """
    try:
        with get_db_session() as session:
            results = session.query(
                SequencingTechnology.technology
            ).distinct().all()
            
            technologies = [r[0].value for r in results if r[0] is not None]
            return sorted(set(technologies))
            
    except Exception as e:
        logger.error(f"Error getting distinct technologies: {e}")
        return []

def get_distinct_callers():
    """
    Get all unique caller names from database.
    
    Returns:
        list: Sorted list of caller names (e.g., ["CLAIR3", "DEEPVARIANT", "GATK"])
    """
    try:
        with get_db_session() as session:
            results = session.query(
                VariantCaller.name
            ).distinct().all()
            
            callers = [r[0].value for r in results if r[0] is not None]
            return sorted(set(callers))
            
    except Exception as e:
        logger.error(f"Error getting distinct callers: {e}")
        return []

def get_platforms_for_technology(technology):
    """
    Get distinct platform names for a given technology.
    
    Args:
        technology (str): Technology name (e.g., "ILLUMINA")
        
    Returns:
        list: Sorted list of platform names (e.g., ["HiSeq", "NovaSeq X"])
    """
    try:
        with get_db_session() as session:
            tech_enum = SeqTechName(technology.upper())
            
            results = session.query(
                SequencingTechnology.platform_name
            ).filter(
                SequencingTechnology.technology == tech_enum
            ).distinct().all()
            
            platforms = [r[0] for r in results if r[0] is not None and r[0] != ""]
            return sorted(set(platforms))
            
    except ValueError:
        logger.error(f"Invalid technology: {technology}")
        return []
    except Exception as e:
        logger.error(f"Error getting platforms for technology {technology}: {e}")
        return []

def get_versions_for_caller(caller):
    """
    Get distinct versions for a given caller.
    
    Args:
        caller (str): Caller name (e.g., "DEEPVARIANT")
        
    Returns:
        list: Sorted list of versions (e.g., ["1.5", "1.6.1"])
    """
    try:
        with get_db_session() as session:
            caller_enum = CallerName(caller.upper())
            
            results = session.query(
                VariantCaller.version
            ).filter(
                VariantCaller.name == caller_enum
            ).distinct().all()
            
            versions = [r[0] for r in results if r[0] is not None and r[0] != ""]
            return sorted(set(versions))
            
    except ValueError:
        logger.error(f"Invalid caller: {caller}")
        return []
    except Exception as e:
        logger.error(f"Error getting versions for caller {caller}: {e}")
        return []

def get_chemistries_for_technology(technology):
    """
    Get distinct chemistry names for a given technology.
    
    Args:
        technology (str): Technology name (e.g., "PACBIO")
        
    Returns:
        list: Sorted list of chemistry names (e.g., ["SPRQ", "Sequel II"])
    """
    try:
        with get_db_session() as session:
            tech_enum = SeqTechName(technology.upper())
            
            results = session.query(
                Chemistry.name
            ).filter(
                Chemistry.sequencing_technology == tech_enum
            ).distinct().all()
            
            chemistries = [r[0] for r in results if r[0] is not None and r[0] != ""]
            return sorted(set(chemistries))
            
    except ValueError:
        logger.error(f"Invalid technology: {technology}")
        return []
    except Exception as e:
        logger.error(f"Error getting chemistries for technology {technology}: {e}")
        return []

def get_experiments_filtered(technology=None, platform=None, caller=None, version=None):
    """
    Get experiment IDs matching the specified filters.
    
    Args:
        technology (str): Technology name filter (optional)
        platform (str): Platform name filter (optional)
        caller (str): Caller name filter (optional)
        version (str): Caller version filter (optional)
        
    Returns:
        list: List of matching experiment IDs
    """
    try:
        with get_db_session() as session:
            query = session.query(Experiment.id)
            
            if technology:
                tech_enum = SeqTechName(technology.upper())
                query = query.join(SequencingTechnology).filter(
                    SequencingTechnology.technology == tech_enum
                )
                
                if platform:
                    query = query.filter(SequencingTechnology.platform_name == platform)
            
            if caller:
                caller_enum = CallerName(caller.upper())
                query = query.join(VariantCaller).filter(
                    VariantCaller.name == caller_enum
                )
                
                if version:
                    query = query.filter(VariantCaller.version == version)
            
            results = query.all()
            return [r[0] for r in results]
            
    except ValueError as e:
        logger.error(f"Invalid enum value: {e}")
        return []
    except Exception as e:
        logger.error(f"Error in get_experiments_filtered: {e}")
        return []