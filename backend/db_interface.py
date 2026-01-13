# ============================================================================
# db_interface.py
# ============================================================================
"""
Database interface and query functions for SNV Benchmarking Dashboard.

Main components:
- Experiment overview and metadata retrieval
- Performance data queries
- Stratified analysis data retrieval  
- Visibility filtering (public/private experiments)
- Technology and caller-based filtering
- JSON parameter handling for R Shiny integration

Visibility Rules:
- Public experiments (is_public=True): visible to everyone
- Private experiments (is_public=False): visible only to owner + admins
- Legacy experiments (owner_id=NULL): treated as public
"""

import pandas as pd
import json
import logging
from sqlalchemy.orm import joinedload
from sqlalchemy import or_
from database import get_db_session
from models import *

logger = logging.getLogger(__name__)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

def parse_experiment_ids(experiment_ids_param):
    """
    Parse experiment IDs from various input formats.
    
    Args:
        experiment_ids_param: JSON string, list, or single value
        
    Returns:
        list: List of experiment IDs
    """
    if experiment_ids_param is None:
        return []
        
    try:
        if isinstance(experiment_ids_param, str):
            experiment_ids = json.loads(experiment_ids_param)
            if not isinstance(experiment_ids, list):
                experiment_ids = [experiment_ids]
        elif isinstance(experiment_ids_param, list):
            experiment_ids = experiment_ids_param
        else:
            experiment_ids = [experiment_ids_param]
            
        return experiment_ids
        
    except json.JSONDecodeError as e:
        logger.error(f"Error parsing experiment_ids JSON: {e}")
        return []
    except Exception as e:
        logger.error(f"Error processing experiment_ids: {e}")
        return []

def apply_visibility_filter(query, user_id=None, is_admin=False):
    """
    Apply visibility filtering to experiment query.
    
    Visibility Rules:
    - Admins see everything
    - Authenticated users see: public + their own private
    - Anonymous users see: public only
    - Legacy data (owner_id=NULL) is treated as public
    
    Args:
        query: SQLAlchemy query object
        user_id: Current user's database ID (None if anonymous)
        is_admin: Whether current user is admin
        
    Returns:
        query: Filtered query object
    """
    if is_admin:
        # Admins see everything
        return query
    elif user_id:
        # Authenticated: public + own private + legacy (owner_id=NULL)
        return query.filter(
            or_(
                Experiment.is_public == True,
                Experiment.owner_id == user_id,
                Experiment.owner_id.is_(None)  # Legacy public data
            )
        )
    else:
        # Anonymous: public only + legacy
        return query.filter(
            or_(
                Experiment.is_public == True,
                Experiment.owner_id.is_(None)
            )
        )

# ============================================================================
# EXPERIMENT OVERVIEW AND METADATA
# ============================================================================

def get_experiments_overview(filters=None, experiment_ids_param=None, user_id=None, is_admin=False):
    """
    Get basic experiment information for dashboard overview table.
    
    Retrieves experiment metadata for the main dashboard table with visibility filtering.
    
    Args:
        filters (dict): Optional filters like {'technology': 'ILLUMINA', 'caller': 'DEEPVARIANT'}
        experiment_ids_param (str/list): JSON string or list of specific experiment IDs
        user_id (int): Current user's database ID for visibility filtering
        is_admin (bool): Whether current user is admin
        
    Returns:
        pandas.DataFrame: Experiment overview with visibility info
    """
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    try:
        with get_db_session() as session:
            # Base query with joins
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.truth_set), 
                joinedload(Experiment.chemistry),
                joinedload(Experiment.owner)
            )
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            # Filter by specific IDs if provided
            if experiment_ids and len(experiment_ids) > 0:
                query = query.filter(Experiment.id.in_(experiment_ids))
            
            # Apply additional filters
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
            
            # Build result data
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
                    # Visibility info
                    'is_public': exp.is_public if exp.is_public is not None else True,
                    'owner_id': exp.owner_id,
                    'owner_username': exp.owner.username if exp.owner else None,
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_experiments_overview: {e}")
        import traceback
        traceback.print_exc()
        return pd.DataFrame()

def get_experiment_metadata(experiment_ids_param, user_id=None, is_admin=False):
    """
    Get complete metadata for specific experiments.
    
    Args:
        experiment_ids_param (str/list): JSON string or list of experiment IDs
        user_id (int): Current user's database ID for visibility filtering
        is_admin (bool): Whether current user is admin
        
    Returns:
        pandas.DataFrame: Complete metadata for selected experiments
    """
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    if not experiment_ids or len(experiment_ids) == 0:
        logger.warning("No experiment IDs provided to get_experiment_metadata")
        return pd.DataFrame()
    
    try:
        with get_db_session() as session:
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.aligner),
                joinedload(Experiment.truth_set),
                joinedload(Experiment.benchmark_tool),
                joinedload(Experiment.variant),
                joinedload(Experiment.chemistry),
                joinedload(Experiment.quality_control),
                joinedload(Experiment.owner)
            ).filter(Experiment.id.in_(experiment_ids))
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            experiments = query.all()
            
            data = []
            for exp in experiments:
                data.append({
                    # Basic info
                    'id': exp.id,
                    'name': exp.name,
                    'description': exp.description,
                    'created_at': exp.created_at.isoformat() if exp.created_at else None,
                    
                    # Visibility
                    'is_public': exp.is_public if exp.is_public is not None else True,
                    'owner_id': exp.owner_id,
                    'owner_username': exp.owner.username if exp.owner else None,
                    
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

def get_experiments_filtered(technology=None, platform=None, caller=None, version=None, user_id=None, is_admin=False):
    """
    Get experiment IDs matching technology, platform, caller, and version criteria.
    
    Args:
        technology: Technology name (optional)
        platform: Platform name (optional)
        caller: Caller name (optional)
        version: Caller version (optional)
        user_id: Current user's database ID for visibility filtering
        is_admin: Whether current user is admin
        
    Returns:
        list: Experiment IDs matching all specified criteria
    """
    try:
        with get_db_session() as session:
            query = session.query(Experiment.id)
            
            # Join tables as needed
            if technology or platform:
                query = query.join(SequencingTechnology)
            if caller or version:
                query = query.join(VariantCaller)
            
            # Apply filters
            if technology:
                tech_enum = SeqTechName(technology.upper())
                query = query.filter(SequencingTechnology.technology == tech_enum)
            if platform:
                query = query.filter(SequencingTechnology.platform_name == platform)
            if caller:
                caller_enum = CallerName(caller.upper())
                query = query.filter(VariantCaller.name == caller_enum)
            if version:
                query = query.filter(VariantCaller.version == version)
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        logger.error(f"Error in get_experiments_filtered: {e}")
        return []
def get_platforms_for_technology(technology, user_id=None, is_admin=False):
    """Get platforms available for a specific technology (visible to user)."""
    try:
        with get_db_session() as session:
            tech_enum = SeqTechName(technology.upper())
            query = session.query(SequencingTechnology.platform_name).join(Experiment).filter(
                SequencingTechnology.technology == tech_enum,
                SequencingTechnology.platform_name.isnot(None)
            ).distinct()
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            result = query.all()
            return [row[0] for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting platforms for {technology}: {e}")
        return []

def get_versions_for_caller(caller, user_id=None, is_admin=False):
    """Get versions available for a specific caller (visible to user)."""
    try:
        with get_db_session() as session:
            caller_enum = CallerName(caller.upper())
            query = session.query(VariantCaller.version).join(Experiment).filter(
                VariantCaller.name == caller_enum,
                VariantCaller.version.isnot(None)
            ).distinct()
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            result = query.all()
            return [row[0] for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting versions for {caller}: {e}")
        return []
# ============================================================================
# PERFORMANCE DATA QUERIES
# ============================================================================

def get_experiments_with_performance(experiment_ids_param, variant_types=['SNP', 'INDEL']):
    """
    Get performance data combined with metadata for selected experiments.
    """
    experiment_ids = parse_experiment_ids(experiment_ids_param)

    if not experiment_ids or len(experiment_ids) == 0:
        logger.warning("No experiment IDs provided to get_experiments_with_performance")
        return pd.DataFrame()
    
    try:
        with get_db_session() as session:
            query = session.query(
                Experiment.id.label('experiment_id'),
                Experiment.name.label('experiment_name'),
                Experiment.is_public,
                Experiment.owner_id,
                SequencingTechnology.technology,
                SequencingTechnology.platform_name,
                SequencingTechnology.platform_type,
                SequencingTechnology.target,
                VariantCaller.name.label('caller'),
                VariantCaller.version.label('caller_version'),
                VariantCaller.type.label('caller_type'),
                VariantCaller.model.label('caller_model'),
                TruthSet.name.label('truth_set'),
                TruthSet.sample.label('truth_set_sample'),
                TruthSet.version.label('truth_set_version'),
                TruthSet.reference.label('truth_set_reference'),
                BenchmarkTool.name.label('benchmark_tool_name'),
                BenchmarkTool.version.label('benchmark_tool_version'),
                Variant.type.label('variant_type_detail'),
                Variant.origin.label('variant_origin'),
                Variant.size.label('variant_size'),
                Variant.is_phased,
                QualityControl.mean_coverage,
                QualityControl.read_length,
                QualityControl.mean_read_length,
                QualityControl.mean_insert_size,
                Chemistry.name.label('chemistry_name'),
                Chemistry.version.label('chemistry_version'),
                OverallResult.variant_type,
                OverallResult.metric_recall.label('recall'),
                OverallResult.metric_precision.label('precision'),
                OverallResult.metric_f1_score.label('f1_score'),
                OverallResult.truth_total,
                OverallResult.truth_tp,
                OverallResult.truth_fn,
                OverallResult.query_total,
                OverallResult.query_tp,
                OverallResult.query_fp
            ).select_from(Experiment).outerjoin(
                SequencingTechnology, Experiment.sequencing_technology_id == SequencingTechnology.id
            ).outerjoin(
                VariantCaller, Experiment.variant_caller_id == VariantCaller.id
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
            ).outerjoin(
                OverallResult, Experiment.id == OverallResult.experiment_id
            ).filter(
                Experiment.id.in_(experiment_ids),
                OverallResult.variant_type.in_(variant_types)
            ).order_by(Experiment.id, OverallResult.variant_type)
            
            results = query.all()
            
            data = []
            for row in results:
                data.append({
                    'experiment_id': row.experiment_id,
                    'experiment_name': row.experiment_name,
                    'is_public': row.is_public,
                    'owner_id': row.owner_id,
                    'technology': row.technology.value if row.technology else None,
                    'platform_name': row.platform_name,
                    'platform_type': row.platform_type.value if row.platform_type else None,
                    'target': row.target.value if row.target else None,
                    'caller': row.caller.value if row.caller else None,
                    'caller_version': row.caller_version,
                    'caller_type': row.caller_type.value if row.caller_type else None,
                    'caller_model': row.caller_model,
                    'truth_set': row.truth_set.value if row.truth_set else None,
                    'truth_set_sample': row.truth_set_sample.value if row.truth_set_sample else None,
                    'truth_set_version': row.truth_set_version,
                    'truth_set_reference': row.truth_set_reference.value if row.truth_set_reference else None,
                    'benchmark_tool_name': row.benchmark_tool_name.value if row.benchmark_tool_name else None,
                    'benchmark_tool_version': row.benchmark_tool_version,
                    'variant_type_detail': row.variant_type_detail.value if row.variant_type_detail else None,
                    'variant_origin': row.variant_origin.value if row.variant_origin else None,
                    'variant_size': row.variant_size.value if row.variant_size else None,
                    'is_phased': row.is_phased,
                    'mean_coverage': float(row.mean_coverage) if row.mean_coverage is not None else None,
                    'read_length': float(row.read_length) if row.read_length is not None else None,
                    'mean_read_length': float(row.mean_read_length) if row.mean_read_length is not None else None,
                    'mean_insert_size': float(row.mean_insert_size) if row.mean_insert_size is not None else None,
                    'chemistry_name': row.chemistry_name,
                    'chemistry_version': row.chemistry_version,
                    'variant_type': row.variant_type,
                    'recall': row.recall,
                    'precision': row.precision,
                    'f1_score': row.f1_score,
                    'truth_total': row.truth_total,
                    'truth_tp': row.truth_tp,
                    'truth_fn': row.truth_fn,
                    'query_total': row.query_total,
                    'query_tp': row.query_tp,
                    'query_fp': row.query_fp
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
    
    Note: Visibility filtering should be applied BEFORE calling this function.
    
    Args:
        experiment_ids_param (str/list): JSON string or list of experiment IDs
        variant_types (list): List of variant types to include
        regions (list): List of region names to filter by
        
    Returns:
        pandas.DataFrame: Stratified performance data
    """
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
                    region_enum = RegionType.from_display_name(region_name) or RegionType.from_string(region_name)
                    if region_enum:
                        region_enums.append(region_enum)
                
                if region_enums:
                    query = query.filter(BenchmarkResult.subset.in_(region_enums))
                else:
                    logger.warning(f"No valid regions found for: {regions}")
                    return pd.DataFrame()
            
            results = query.all()
            
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
                    'chemistry_name': result.experiment.chemistry.name if (result.experiment and result.experiment.chemistry) else None,
                    'recall': result.metric_recall,
                    'precision': result.metric_precision,
                    'f1_score': result.metric_f1_score,
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
# TECHNOLOGY AND CALLER FILTERING
# ============================================================================

def get_experiments_by_technology(technology, user_id=None, is_admin=False):
    """
    Get experiment IDs matching a specific sequencing technology.
    
    Args:
        technology (str): Technology name (e.g., "ILLUMINA", "PACBIO")
        user_id (int): Current user's database ID for visibility filtering
        is_admin (bool): Whether current user is admin
        
    Returns:
        list: List of visible experiment IDs matching the technology
    """
    try:
        with get_db_session() as session:
            try:
                tech_enum = SeqTechName(technology.strip().upper())
            except ValueError:
                logger.error(f"Invalid technology: {technology}")
                return []
            
            query = session.query(Experiment.id).join(
                SequencingTechnology
            ).filter(
                SequencingTechnology.technology == tech_enum
            )
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        logger.error(f"Error getting experiments by technology: {e}")
        return []

def get_experiments_by_caller(caller, user_id=None, is_admin=False):
    """
    Get experiment IDs matching a specific variant caller.
    
    Args:
        caller (str): Caller name (e.g., "DEEPVARIANT", "GATK")
        user_id (int): Current user's database ID for visibility filtering
        is_admin (bool): Whether current user is admin
        
    Returns:
        list: List of visible experiment IDs matching the caller
    """
    try:
        with get_db_session() as session:
            try:
                caller_enum = CallerName(caller.strip().upper())
            except ValueError:
                logger.error(f"Invalid caller: {caller}")
                return []
            
            query = session.query(Experiment.id).join(
                VariantCaller
            ).filter(
                VariantCaller.name == caller_enum
            )
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        logger.error(f"Error getting experiments by caller: {e}")
        return []

# ============================================================================
# INDIVIDUAL EXPERIMENT LOOKUPS
# ============================================================================

def get_technology(experiment_id):
    """Get sequencing technology name for a specific experiment."""
    try:
        with get_db_session() as session:
            experiment = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology)
            ).filter(Experiment.id == experiment_id).first()
            
            if experiment and experiment.sequencing_technology:
                return experiment.sequencing_technology.technology.value
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
        str or None: Caller name (e.g., "DEEPVARIANT", "GATK3") or None if not found
    """
    try:
        with get_db_session() as session:
            experiment = session.query(Experiment).options(
                joinedload(Experiment.variant_caller)
            ).filter(Experiment.id == experiment_id).first()
            
            if experiment and experiment.variant_caller:
                return experiment.variant_caller.name.value
            return None
            
    except Exception as e:
        logger.error(f"Error getting caller for experiment {experiment_id}: {e}")
        return None

def get_experiment_owner(experiment_id):
    """
    Get owner information for an experiment.
    
    Args:
        experiment_id: Experiment ID
        
    Returns:
        dict: Owner info or None
    """
    try:
        with get_db_session() as session:
            experiment = session.query(Experiment).options(
                joinedload(Experiment.owner)
            ).filter(Experiment.id == experiment_id).first()
            
            if experiment:
                return {
                    'owner_id': experiment.owner_id,
                    'owner_username': experiment.owner.username if experiment.owner else None,
                    'is_public': experiment.is_public
                }
            return None
            
    except Exception as e:
        logger.error(f"Error getting owner for experiment {experiment_id}: {e}")
        return None

def can_user_access_experiment(experiment_id, user_id=None, is_admin=False):
    """
    Check if user can access a specific experiment.
    
    Args:
        experiment_id: Experiment ID to check
        user_id: Current user's database ID
        is_admin: Whether current user is admin
        
    Returns:
        bool: True if user can access the experiment
    """
    try:
        with get_db_session() as session:
            experiment = session.query(Experiment).filter(
                Experiment.id == experiment_id
            ).first()
            
            if not experiment:
                return False
            
            # Admins can access everything
            if is_admin:
                return True
            
            # Public experiments accessible to all
            if experiment.is_public or experiment.owner_id is None:
                return True
            
            # Private experiments only accessible to owner
            if user_id and experiment.owner_id == user_id:
                return True
            
            return False
            
    except Exception as e:
        logger.error(f"Error checking access for experiment {experiment_id}: {e}")
        return False

# ============================================================================
# DROPDOWN OPTIONS
# ============================================================================

def get_distinct_technologies():
    """Get list of distinct technology names in database."""
    try:
        with get_db_session() as session:
            result = session.query(SequencingTechnology.technology).distinct().all()
            return [row[0].value for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting distinct technologies: {e}")
        return []

def get_distinct_callers():
    """
    Get all unique caller names from database.
    
    Returns:
        list: Sorted list of caller names (e.g., ["CLAIR3", "DEEPVARIANT", "GATK3"])
    """
    try:
        with get_db_session() as session:
            result = session.query(VariantCaller.name).distinct().all()
            return [row[0].value for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting distinct callers: {e}")
        return []

def get_distinct_truth_sets():
    """Get list of distinct truth set names in database."""
    try:
        with get_db_session() as session:
            result = session.query(TruthSet.name).distinct().all()
            return [row[0].value for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting distinct truth sets: {e}")
        return []

def get_platforms_by_technology(technology):
    """Get platforms available for a specific technology."""
    try:
        with get_db_session() as session:
            tech_enum = SeqTechName(technology.upper())
            result = session.query(SequencingTechnology.platform_name).filter(
                SequencingTechnology.technology == tech_enum,
                SequencingTechnology.platform_name.isnot(None)
            ).distinct().all()
            return [row[0] for row in result if row[0]]
    except Exception as e:
        logger.error(f"Error getting platforms for {technology}: {e}")
        return []

# ============================================================================
# HIERARCHICAL DATA FOR ADVANCED COMPARISONS
# ============================================================================

def get_technology_hierarchy(user_id=None, is_admin=False):
    """
    Get hierarchical technology data for advanced comparison UI.
    
    Returns:
        dict: Nested structure {technology: {platform: [experiment_ids]}}
    """
    try:
        with get_db_session() as session:
            query = session.query(
                Experiment.id,
                SequencingTechnology.technology,
                SequencingTechnology.platform_name
            ).join(SequencingTechnology)
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            results = query.all()
            
            hierarchy = {}
            for exp_id, tech, platform in results:
                tech_name = tech.value if tech else "Unknown"
                platform_name = platform or "Unknown"
                
                if tech_name not in hierarchy:
                    hierarchy[tech_name] = {}
                if platform_name not in hierarchy[tech_name]:
                    hierarchy[tech_name][platform_name] = []
                    
                hierarchy[tech_name][platform_name].append(exp_id)
            
            return hierarchy
            
    except Exception as e:
        logger.error(f"Error getting technology hierarchy: {e}")
        return {}

def get_caller_hierarchy(user_id=None, is_admin=False):
    """
    Get hierarchical caller data for advanced comparison UI.
    
    Returns:
        dict: Nested structure {caller: {version: [experiment_ids]}}
    """
    try:
        with get_db_session() as session:
            query = session.query(
                Experiment.id,
                VariantCaller.name,
                VariantCaller.version
            ).join(VariantCaller)
            
            # Apply visibility filter
            query = apply_visibility_filter(query, user_id, is_admin)
            
            results = query.all()
            
            hierarchy = {}
            for exp_id, caller, version in results:
                caller_name = caller.value if caller else "Unknown"
                version_str = version or "Unknown"
                
                if caller_name not in hierarchy:
                    hierarchy[caller_name] = {}
                if version_str not in hierarchy[caller_name]:
                    hierarchy[caller_name][version_str] = []
                    
                hierarchy[caller_name][version_str].append(exp_id)
            
            return hierarchy
            
    except Exception as e:
        logger.error(f"Error getting caller hierarchy: {e}")
        return {}

# ============================================================================
# USER'S EXPERIMENTS
# ============================================================================

def get_user_experiments(user_id):
    """
    Get all experiments owned by a specific user.
    
    Args:
        user_id: User's database ID
        
    Returns:
        pandas.DataFrame: User's experiments
    """
    if not user_id:
        return pd.DataFrame()
    
    try:
        with get_db_session() as session:
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller)
            ).filter(Experiment.owner_id == user_id)
            
            experiments = query.all()
            
            data = []
            for exp in experiments:
                data.append({
                    'id': exp.id,
                    'name': exp.name,
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else "N/A",
                    'caller': exp.variant_caller.name.value if exp.variant_caller else "N/A",
                    'is_public': exp.is_public,
                    'created_at': exp.created_at.strftime('%Y-%m-%d') if exp.created_at else "N/A"
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error getting user experiments: {e}")
        return pd.DataFrame()
    