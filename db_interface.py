# db_interface.py
"""
Core SQLAlchemy interface for Benchmarking Dashboard - translator between Rshiny and the DB
Provides functions for R Shiny dashboard (with Reticulate)

Main Functions:
1. get_experiments_overview() - Light overview of all experiments
2. get_experiment_metadata(experiment_ids) - Detailed metadata for selected experiments  
3. get_performance_results(experiment_ids, variant_types) - Performance data for selected experiments
4. get_experiments_by_filter() - filtering to get experiment IDs/info
"""

import pandas as pd
from sqlalchemy.orm import joinedload
from database import get_db_session
from models import *

# ========================================================================
# 1. EXPERIMENTS OVERVIEW
# ========================================================================

def get_experiments_overview(filters=None):
    """
    Get basic experiment information for dashboard overview.
    returns essential info
    
    Args:
        filters (dict): Optional filters like {'technology': 'ILLUMINA', 'caller': 'DEEPVARIANT'}
    """
    try:
        with get_db_session() as session:
            # Lightweight query with minimal necessary joins
            query = session.query(Experiment).options(
                joinedload(Experiment.sequencing_technology),
                joinedload(Experiment.variant_caller),
                joinedload(Experiment.truth_set), 
                joinedload(Experiment.chemistry)
            )
            
            # Apply filters if provided
            if filters:
                if 'technology' in filters:
                    tech_enum = SeqTechName(filters['technology'])
                    query = query.join(SequencingTechnology).filter(
                        SequencingTechnology.technology == tech_enum
                    )
                
                if 'caller' in filters:
                    caller_enum = CallerName(filters['caller'])
                    query = query.join(VariantCaller).filter(
                        VariantCaller.name == caller_enum
                    )
            
            experiments = query.all()
            
            # Extract essential data
            data = []
            for exp in experiments:
                data.append({ 
                    'id': exp.id,
                    'name': exp.name,
                    'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else None,
                    'platform': exp.sequencing_technology.platform_name if exp.sequencing_technology else None,
                    'caller': exp.variant_caller.name.value if exp.variant_caller else None,
                    'caller_version': exp.variant_caller.version if exp.variant_caller else None,
                    'chemistry': exp.chemistry.name if exp.chemistry else None,
                    'truth_set': exp.truth_set.name.value if exp.truth_set else None,
                    'sample': exp.truth_set.sample.value if exp.truth_set else None,
                    'created_at': exp.created_at.isoformat() if exp.created_at else None
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        print(f"Error in get_experiments_overview: {e}")
        return pd.DataFrame()
# ========================================================================
# 2. DETAILED EXPERIMENT METADATA
# ========================================================================
def get_experiment_metadata(experiment_ids):
    """
    Get complete metadata for specific experiments.
    Use after user selects experiments from overview or filtering.
    
    Args:
        experiment_ids (list): List of experiment IDs to get metadata for
        
    Returns:
        pandas.DataFrame: Complete metadata for selected experiments
    """
    # If experiment ID does not exist
    if not experiment_ids:
        return pd.DataFrame()
        
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
                    'caller_name': exp.variant_caller.name.value if exp.variant_caller else None,
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
        print(f"Error in get_experiment_metadata: {e}")
        return pd.DataFrame()

# ========================================================================
# 3. DETAILED EXPERIMENT PERFORMANCE RESULTS
# ======================================================================== 
def get_performance_results(experiment_ids, variant_types=['SNP', 'INDEL']):
    """
    Get benchmark performance results for specific experiments.
    
    Args:
        experiment_ids (list): List of experiment IDs
        variant_types (list): Variant types to include ['SNP', 'INDEL', 'SNP+INDEL']
        
    Returns:
        pandas.DataFrame: Performance metrics and counts
    """
    if not experiment_ids:
        return pd.DataFrame()
        
    try:
        with get_db_session() as session:
            query = session.query(BenchmarkResult).options(
                joinedload(BenchmarkResult.experiment)
            ).filter(
                BenchmarkResult.experiment_id.in_(experiment_ids),
                BenchmarkResult.variant_type.in_(variant_types),
            )
            
            results = query.all()
            
            data = []
            for result in results:
                data.append({
                    # Identifiers
                    'experiment_id': result.experiment_id,
                    'experiment_name': result.experiment.name if result.experiment else None,
                    'variant_type': result.variant_type,
                    'technology': result.experiment.sequencing_technology.technology.value if (result.experiment and result.experiment.sequencing_technology) else 'Unknown',
                    'caller': result.experiment.variant_caller.name.value if (result.experiment and result.experiment.variant_caller) else 'Unknown',
                    'subset': result.subset,
                    'filter_type': result.filter_type,
                    
                    # Core Performance Metrics
                    'recall': result.metric_recall,
                    'precision': result.metric_precision,
                    'f1_score': result.metric_f1_score,
                    
                    # Subset Information  
                    'subset_size': result.subset_size,
                    'subset_is_conf_size': result.subset_is_conf_size,
                    
                    # Truth Set Counts
                    'truth_total': result.truth_total,
                    'truth_total_het': result.truth_total_het,
                    'truth_total_homalt': result.truth_total_homalt,
                    'truth_tp': result.truth_tp,
                    'truth_tp_het': result.truth_tp_het,
                    'truth_tp_homalt': result.truth_tp_homalt,
                    'truth_fn': result.truth_fn,
                    'truth_fn_het': result.truth_fn_het,
                    'truth_fn_homalt': result.truth_fn_homalt,
                    
                    # Query Counts
                    'query_total': result.query_total,
                    'query_total_het': result.query_total_het,
                    'query_total_homalt': result.query_total_homalt,
                    'query_tp': result.query_tp,
                    'query_tp_het': result.query_tp_het,
                    'query_tp_homalt': result.query_tp_homalt,
                    'query_fp': result.query_fp,
                    'query_fp_het': result.query_fp_het,
                    'query_fp_homalt': result.query_fp_homalt,
                    'query_unk': result.query_unk,
                    'query_unk_het': result.query_unk_het,
                    'query_unk_homalt': result.query_unk_homalt
                })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        print(f"Error in get_performance_results: {e}")
        return pd.DataFrame()

# ========================================================================
# 4. FILTER EXPERIMENTS BY SEQUNCING TECHNOLGY
# ========================================================================

def get_experiments_by_technology(technology):
    """
    Get experiment IDs for a specific sequencing technology.
    
    Args:
        technology (str): Technology name - "Illumina", "PacBio", "ONT", or "MGI"
        
    Returns:
        list: List of experiment IDs matching the technology
    """
    try:
        with get_db_session() as session:
            # Convert string to enum
            try:
                tech_enum = SeqTechName(technology.strip().upper())
            except ValueError:
                print(f"Invalid technology: {technology}")
                print(f"Valid options: {[t.value for t in SeqTechName]}")
                return []
            
            # Query joining Experiment with Sequencing technologies
            query = session.query(Experiment.id).join(
                SequencingTechnology
            ).filter(
                SequencingTechnology.technology == tech_enum # Filter for chosen sequencing tech
            )
            
            # Extract IDs
            result = query.all()
            return [row[0] for row in result] #returns ID
            
    except Exception as e:
        print(f"Error getting experiments by technology: {e}")
        return []

# ========================================================================
# 4. FILTER EXPERIMENTS BY VARIANT CALLER
# ========================================================================
def get_experiments_by_caller(caller):
    """
    Get experiment IDs for a specific variant caller.
    
    Args:
        caller (str): Caller name - "DeepVariant", "GATK", or "Clair3"
        
    Returns:
        list: List of experiment IDs matching the caller
    """
    try:
        with get_db_session() as session:
            # Convert string to enum
            try:
                caller_enum = CallerName(caller.strip().upper())
            except ValueError:
                print(f"Invalid caller: {caller}")
                print(f"Valid options: {[c.value for c in CallerName]}")
                return []
            
            # Query joining Experiment with varaint caller
            query = session.query(Experiment.id).join(
                VariantCaller
            ).filter(
                VariantCaller.name == caller_enum # Filters for chosen callers
            )
            
            # Extract IDs
            result = query.all()
            return [row[0] for row in result]
            
    except Exception as e:
        print(f"Error getting experiments by caller: {e}")
        return []
    

# ========================================================================
# 5. GET TECHNOLOGY FROM ID
# ========================================================================

def get_technology(experiment_id):
    """
    Get sequencing technology name for a specific experiment.
    
    Args:
        experiment_id (int): Single experiment ID
        
    Returns:
        str or None: Technology name (e.g., "Illumina", "PacBio", "ONT", "MGI") or None if not found
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
        print(f"Error getting technology for experiment {experiment_id}: {e}")
        return None

# ========================================================================
# 5. GET CALLER FROM ID
# ========================================================================
def get_caller(experiment_id):
    """
    Get variant caller name for a specific experiment.
    
    Args:
        experiment_id (int): Single experiment ID
        
    Returns:
        str or None: Caller name (e.g., "DeepVariant", "GATK", "Clair3") or None if not found
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
        print(f"Error getting caller for experiment {experiment_id}: {e}")
        return None