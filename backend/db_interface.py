# ============================================================================
# db_interface.py
# ============================================================================
"""
Database interface with unified functions for public and user data.
Each function can return either public data (user_id=None) or user data (user_id=provided).
"""

import pandas as pd
import logging
from typing import Dict, Any, List, Optional, Union
from sqlalchemy.orm import joinedload

from database import get_db_session
from models import Experiment, BenchmarkResult, OverallResult 
from user_models import UserExperiment, UserBenchmarkResult, UserOverallResult, UserSequencingTechnology, UserVariantCaller 
import json

logger = logging.getLogger(__name__)

# ============================================================================
# UNIFIED EXPERIMENT OVERVIEW
# ============================================================================

def get_experiments_overview(filters=None, experiment_ids_param=None, user_id=None):
    """
    Unified function to get experiments:
    - If user_id=None: Returns ONLY public experiments (for main dashboard)
    - If user_id=123: Returns ONLY that user's private experiments (for "My Experiments" tab)
    
    Args:
        filters: Existing filter logic (currently unused, kept for compatibility)
        experiment_ids_param: JSON string of experiment IDs to filter
        user_id: If None=public experiments, if provided=that user's experiments only
    
    Returns:
        DataFrame with either public OR user experiments
    """
    try:
        # Parse experiment IDs if provided
        experiment_ids = []
        if experiment_ids_param:
            try:
                experiment_ids = json.loads(experiment_ids_param)
            except:
                experiment_ids = []
        
        with get_db_session() as session:
            data = []
            
            if user_id is None:
                # PUBLIC EXPERIMENTS ONLY
                # Filter for numeric IDs only (public experiments)
                if experiment_ids:
                    public_ids = [int(id_val) for id_val in experiment_ids 
                                 if isinstance(id_val, (int, str)) and str(id_val).isdigit()]
                else:
                    public_ids = []
                
                public_query = session.query(Experiment).options(
                    joinedload(Experiment.sequencing_technology),
                    joinedload(Experiment.variant_caller),
                    joinedload(Experiment.chemistry),
                    joinedload(Experiment.truth_set),
                    joinedload(Experiment.quality_control),
                    joinedload(Experiment.aligner),
                    joinedload(Experiment.variant)
                )
                
                if public_ids:
                    public_query = public_query.filter(Experiment.id.in_(public_ids))
                
                public_experiments = public_query.all()
                
                # Convert public experiments to standard format
                for exp in public_experiments:
                    data.append({
                        'id': exp.id, 
                        'source': 'public',
                        'name': exp.name or f"Experiment {exp.id}",
                        'description': getattr(exp, 'description', ''),
                        'technology': exp.sequencing_technology.technology.value if exp.sequencing_technology else "N/A",
                        'target': exp.sequencing_technology.target.value if (exp.sequencing_technology and exp.sequencing_technology.target) else "N/A",
                        'platform_name': exp.sequencing_technology.platform_name if (exp.sequencing_technology and exp.sequencing_technology.platform_name) else "N/A",
                        'platform_type': exp.sequencing_technology.platform_type.value if (exp.sequencing_technology and exp.sequencing_technology.platform_type) else "N/A",
                        'caller_name': exp.variant_caller.name.value if exp.variant_caller else "N/A",
                        'caller_type': exp.variant_caller.type.value if (exp.variant_caller and exp.variant_caller.type) else "N/A",
                        'caller_version': exp.variant_caller.version if (exp.variant_caller and exp.variant_caller.version) else "N/A",
                        'aligner_name': exp.aligner.name if exp.aligner else "N/A",
                        'aligner_version': exp.aligner.version if (exp.aligner and exp.aligner.version) else "N/A",
                        'chemistry_name': exp.chemistry.name if (exp.chemistry and exp.chemistry.name) else "N/A",
                        'truth_set_name': exp.truth_set.name.value if exp.truth_set else "N/A",
                        'truth_set_sample': exp.truth_set.sample.value if exp.truth_set else "N/A",
                        'truth_set_version': exp.truth_set.version if (exp.truth_set and exp.truth_set.version) else "N/A",
                        'variant_type': exp.variant.type.value if exp.variant else "N/A",
                        'variant_origin': exp.variant.origin.value if exp.variant else "N/A",
                        'is_phased': exp.variant.is_phased if exp.variant else False,
                        'mean_coverage': exp.quality_control.mean_coverage if exp.quality_control else None,
                        'read_length': exp.quality_control.read_length if exp.quality_control else None,
                        'mean_read_length': exp.quality_control.mean_read_length if exp.quality_control else None,
                        'mean_insert_size': exp.quality_control.mean_insert_size if exp.quality_control else None,
                        'created_at': exp.created_at.strftime('%Y-%m-%d') if hasattr(exp, 'created_at') and exp.created_at else "N/A"
                    })
            
            else:
                # USER EXPERIMENTS ONLY
                # Validate user exists first
                from user_models import User
                user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
                if not user:
                    logger.warning(f"User {user_id} not found or inactive")
                    return pd.DataFrame()
                
                # Filter for user experiment IDs only
                if experiment_ids:
                    user_exp_ids = [int(id_str.replace('user_', '')) for id_str in experiment_ids 
                                   if isinstance(id_str, str) and id_str.startswith('user_')]
                else:
                    user_exp_ids = []
                
                user_query = session.query(UserExperiment).options(
                    joinedload(UserExperiment.sequencing_technology),
                    joinedload(UserExperiment.variant_caller),
                    joinedload(UserExperiment.chemistry),
                    joinedload(UserExperiment.truth_set),
                    joinedload(UserExperiment.quality_control),
                    joinedload(UserExperiment.aligner),
                    joinedload(UserExperiment.variant)
                ).filter(UserExperiment.user_id == user_id)
                
                if user_exp_ids:
                    user_query = user_query.filter(UserExperiment.id.in_(user_exp_ids))
                
                user_experiments = user_query.all()
                
                # Convert user experiments to standard format
                for exp in user_experiments:
                    data.append({
                        'id': f"user_{exp.id}",
                        'source': 'user',
                        'user_id': exp.user_id,
                        'name': exp.name or f"My Experiment {exp.id}",
                        'description': exp.description or '',
                        'technology': exp.sequencing_technology.technology if exp.sequencing_technology else "N/A",
                        'target': exp.sequencing_technology.target if (exp.sequencing_technology and exp.sequencing_technology.target) else "N/A",
                        'platform_name': exp.sequencing_technology.platform_name if (exp.sequencing_technology and exp.sequencing_technology.platform_name) else "N/A",
                        'platform_type': exp.sequencing_technology.platform_type if (exp.sequencing_technology and exp.sequencing_technology.platform_type) else "N/A",
                        'caller_name': exp.variant_caller.name if exp.variant_caller else "N/A",
                        'caller_type': exp.variant_caller.type if (exp.variant_caller and exp.variant_caller.type) else "N/A",
                        'caller_version': exp.variant_caller.version if (exp.variant_caller and exp.variant_caller.version) else "N/A",
                        'aligner_name': exp.aligner.name if exp.aligner else "N/A",
                        'aligner_version': exp.aligner.version if (exp.aligner and exp.aligner.version) else "N/A",
                        'chemistry_name': exp.chemistry.name if (exp.chemistry and exp.chemistry.name) else "N/A",
                        'truth_set_name': exp.truth_set.name if exp.truth_set else "N/A",
                        'truth_set_sample': exp.truth_set.sample if exp.truth_set else "N/A",
                        'truth_set_version': exp.truth_set.version if (exp.truth_set and exp.truth_set.version) else "N/A",
                        'variant_type': exp.variant.type if exp.variant else "N/A",
                        'variant_origin': exp.variant.origin if exp.variant else "N/A",
                        'is_phased': exp.variant.is_phased if exp.variant else False,
                        'mean_coverage': exp.quality_control.mean_coverage if exp.quality_control else None,
                        'read_length': exp.quality_control.read_length if exp.quality_control else None,
                        'mean_read_length': exp.quality_control.mean_read_length if exp.quality_control else None,
                        'mean_insert_size': exp.quality_control.mean_insert_size if exp.quality_control else None,
                        'created_at': exp.created_at.strftime('%Y-%m-%d') if exp.created_at else "N/A"
                    })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_experiments_overview (user_id={user_id}): {e}")
        return pd.DataFrame()

# ============================================================================
# RESULTS QUERIES
# ============================================================================

def get_overall_results(experiment_ids_param=None, user_id=None):
    """
    Function to get overall results:
    - If user_id=None: Returns ONLY public experiment results
    - If user_id=123: Returns ONLY that user's experiment results
    """
    try:
        # Parse experiment IDs
        experiment_ids = []
        if experiment_ids_param:
            try:
                experiment_ids = json.loads(experiment_ids_param)
            except:
                experiment_ids = []
        
        with get_db_session() as session:
            data = []
            
            if user_id is None:
                # PUBLIC RESULTS ONLY
                if experiment_ids:
                    public_ids = [int(id_val) for id_val in experiment_ids 
                                 if isinstance(id_val, (int, str)) and str(id_val).isdigit()]
                else:
                    public_ids = []
                
                public_query = session.query(OverallResult).join(Experiment)
                if public_ids:
                    public_query = public_query.filter(Experiment.id.in_(public_ids))
                
                public_results = public_query.all()
                
                for result in public_results:
                    data.append({
                        'id': result.experiment_id, 
                        'source': 'public',
                        'experiment_id': result.experiment_id,
                        'variant_type': result.variant_type,
                        'metric_recall': result.metric_recall,
                        'metric_precision': result.metric_precision,
                        'metric_f1_score': result.metric_f1_score,
                        'truth_total': result.truth_total,
                        'truth_tp': result.truth_tp,
                        'truth_fn': result.truth_fn,
                        'query_total': result.query_total,
                        'query_tp': result.query_tp,
                        'query_fp': result.query_fp
                    })
            
            else:
                # USER RESULTS ONLY
                # Validate user exists
                from user_models import User
                user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
                if not user:
                    logger.warning(f"User {user_id} not found or inactive")
                    return pd.DataFrame()
                
                if experiment_ids:
                    user_exp_ids = [int(id_str.replace('user_', '')) for id_str in experiment_ids 
                                   if isinstance(id_str, str) and id_str.startswith('user_')]
                else:
                    user_exp_ids = []
                
                user_query = session.query(UserOverallResult).join(UserExperiment).filter(
                    UserExperiment.user_id == user_id
                )
                
                if user_exp_ids:
                    user_query = user_query.filter(UserExperiment.id.in_(user_exp_ids))
                
                user_results = user_query.all()
                
                for result in user_results:
                    data.append({
                        'id': f"user_{result.experiment_id}",
                        'source': 'user',
                        'experiment_id': result.experiment_id,
                        'variant_type': result.variant_type,
                        'metric_recall': result.metric_recall,
                        'metric_precision': result.metric_precision,
                        'metric_f1_score': result.metric_f1_score,
                        'truth_total': result.truth_total,
                        'truth_tp': result.truth_tp,
                        'truth_fn': result.truth_fn,
                        'query_total': result.query_total,
                        'query_tp': result.query_tp,
                        'query_fp': result.query_fp
                    })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_overall_results (user_id={user_id}): {e}")
        return pd.DataFrame()

def get_benchmark_results(experiment_ids_param=None, region_filter=None, user_id=None):
    """
    Unified function to get detailed benchmark results:
    - If user_id=None: Returns ONLY public experiment results
    - If user_id=123: Returns ONLY that user's experiment results
    """
    try:
        # Parse experiment IDs
        experiment_ids = []
        if experiment_ids_param:
            try:
                experiment_ids = json.loads(experiment_ids_param)
            except:
                experiment_ids = []
        
        with get_db_session() as session:
            data = []
            
            if user_id is None:
                # PUBLIC RESULTS ONLY
                if experiment_ids:
                    public_ids = [int(id_val) for id_val in experiment_ids 
                                 if isinstance(id_val, (int, str)) and str(id_val).isdigit()]
                else:
                    public_ids = []
                
                public_query = session.query(BenchmarkResult).join(Experiment)
                if public_ids:
                    public_query = public_query.filter(Experiment.id.in_(public_ids))
                if region_filter:
                    public_query = public_query.filter(BenchmarkResult.subset == region_filter)
                
                public_results = public_query.all()
                
                for result in public_results:
                    data.append({
                        'id': result.experiment_id,  # Keep natural numeric ID
                        'source': 'public',
                        'experiment_id': result.experiment_id,
                        'variant_type': result.variant_type,
                        'subset': result.subset.value if hasattr(result.subset, 'value') else str(result.subset),
                        'metric_recall': result.metric_recall,
                        'metric_precision': result.metric_precision,
                        'metric_f1_score': result.metric_f1_score,
                        'subset_size': result.subset_size,
                        'truth_total': result.truth_total,
                        'truth_tp': result.truth_tp,
                        'truth_fn': result.truth_fn,
                        'query_total': result.query_total,
                        'query_tp': result.query_tp,
                        'query_fp': result.query_fp
                    })
            
            else:
                # USER RESULTS ONLY
                # Validate user exists
                from user_models import User
                user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
                if not user:
                    logger.warning(f"User {user_id} not found or inactive")
                    return pd.DataFrame()
                
                if experiment_ids:
                    user_exp_ids = [int(id_str.replace('user_', '')) for id_str in experiment_ids 
                                   if isinstance(id_str, str) and id_str.startswith('user_')]
                else:
                    user_exp_ids = []
                
                user_query = session.query(UserBenchmarkResult).join(UserExperiment).filter(
                    UserExperiment.user_id == user_id
                )
                
                if user_exp_ids:
                    user_query = user_query.filter(UserExperiment.id.in_(user_exp_ids))
                if region_filter:
                    user_query = user_query.filter(UserBenchmarkResult.subset == region_filter)
                
                user_results = user_query.all()
                
                for result in user_results:
                    data.append({
                        'id': f"user_{result.experiment_id}",
                        'source': 'user',
                        'experiment_id': result.experiment_id,
                        'variant_type': result.variant_type,
                        'subset': result.subset,
                        'metric_recall': result.metric_recall,
                        'metric_precision': result.metric_precision,
                        'metric_f1_score': result.metric_f1_score,
                        'subset_size': result.subset_size,
                        'truth_total': result.truth_total,
                        'truth_tp': result.truth_tp,
                        'truth_fn': result.truth_fn,
                        'query_total': result.query_total,
                        'query_tp': result.query_tp,
                        'query_fp': result.query_fp
                    })
            
            return pd.DataFrame(data)
            
    except Exception as e:
        logger.error(f"Error in get_benchmark_results (user_id={user_id}): {e}")
        return pd.DataFrame()

# ============================================================================
# UNIFIED FILTERING FUNCTIONS
# ============================================================================

def get_experiments_by_technology(technology, user_id=None):
    """
    Unified function to get experiments by technology:
    - If user_id=None: Returns ONLY public experiment IDs
    - If user_id=123: Returns ONLY that user's experiment IDs
    """
    try:
        with get_db_session() as session:
            experiment_ids = []
            
            if user_id is None:
                # PUBLIC EXPERIMENTS ONLY
                public_experiments = session.query(Experiment).join(
                    Experiment.sequencing_technology
                ).filter_by(technology=technology).all()
                
                for exp in public_experiments:
                    experiment_ids.append(f"public_{exp.id}")
            
            else:
                # USER EXPERIMENTS ONLY
                # Validate user exists
                from user_models import User
                user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
                if not user:
                    logger.warning(f"User {user_id} not found or inactive")
                    return []
                
                user_experiments = session.query(UserExperiment).join(
                    UserExperiment.sequencing_technology
                ).filter(
                    UserExperiment.user_id == user_id,
                    UserSequencingTechnology.technology == technology
                ).all()
                
                for exp in user_experiments:
                    experiment_ids.append(f"user_{exp.id}")
            
            return experiment_ids
            
    except Exception as e:
        logger.error(f"Error getting experiments by technology (user_id={user_id}): {e}")
        return []

def get_experiments_by_caller(caller, user_id=None):
    """
    Unified function to get experiments by caller:
    - If user_id=None: Returns ONLY public experiment IDs
    - If user_id=123: Returns ONLY that user's experiment IDs
    """
    try:
        with get_db_session() as session:
            experiment_ids = []
            
            if user_id is None:
                # PUBLIC EXPERIMENTS ONLY
                public_experiments = session.query(Experiment).join(
                    Experiment.variant_caller
                ).filter_by(name=caller).all()
                
                for exp in public_experiments:
                    experiment_ids.append(f"public_{exp.id}")
            
            else:
                # USER EXPERIMENTS ONLY
                # Validate user exists
                from user_models import User
                user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
                if not user:
                    logger.warning(f"User {user_id} not found or inactive")
                    return []
                
                user_experiments = session.query(UserExperiment).join(
                    UserExperiment.variant_caller
                ).filter(
                    UserExperiment.user_id == user_id,
                    UserVariantCaller.name == caller
                ).all()
                
                for exp in user_experiments:
                    experiment_ids.append(f"user_{exp.id}")
            
            return experiment_ids
            
    except Exception as e:
        logger.error(f"Error getting experiments by caller (user_id={user_id}): {e}")
        return []

# ============================================================================
# UNIFIED EXPERIMENT DETAILS
# ============================================================================

def get_experiment_details(experiment_id_str, user_id=None):
    """
    Unified function to get experiment details:
    - For public experiments: user_id can be None or any value
    - For user experiments: user_id must match the experiment owner
    """
    try:
        with get_db_session() as session:
            if experiment_id_str.startswith('public_'):
                # PUBLIC EXPERIMENT
                exp_id = int(experiment_id_str.replace('public_', ''))
                experiment = session.query(Experiment).options(
                    joinedload(Experiment.sequencing_technology),
                    joinedload(Experiment.variant_caller),
                    joinedload(Experiment.aligner),
                    joinedload(Experiment.truth_set),
                    joinedload(Experiment.benchmark_tool),
                    joinedload(Experiment.variant),
                    joinedload(Experiment.quality_control),
                    joinedload(Experiment.chemistry)
                ).filter(Experiment.id == exp_id).first()
                
                if not experiment:
                    return None
                
                return {
                    'id': experiment_id_str,
                    'source': 'public',
                    'name': experiment.name,
                    'description': getattr(experiment, 'description', ''),
                    'technology': experiment.sequencing_technology.technology.value if experiment.sequencing_technology else None,
                    'platform_name': experiment.sequencing_technology.platform_name if experiment.sequencing_technology else None,
                    'caller_name': experiment.variant_caller.name.value if experiment.variant_caller else None,
                    'caller_version': experiment.variant_caller.version if experiment.variant_caller else None,
                    'truth_set_name': experiment.truth_set.name.value if experiment.truth_set else None,
                    'truth_set_sample': experiment.truth_set.sample.value if experiment.truth_set else None,
                    'mean_coverage': experiment.quality_control.mean_coverage if experiment.quality_control else None,
                    'created_at': experiment.created_at.isoformat() if hasattr(experiment, 'created_at') and experiment.created_at else None
                }
                
            elif experiment_id_str.startswith('user_'):
                # USER EXPERIMENT - requires user_id and ownership verification
                if user_id is None:
                    logger.warning("Cannot access user experiment without user_id")
                    return None
                
                exp_id = int(experiment_id_str.replace('user_', ''))
                experiment = session.query(UserExperiment).options(
                    joinedload(UserExperiment.sequencing_technology),
                    joinedload(UserExperiment.variant_caller),
                    joinedload(UserExperiment.aligner),
                    joinedload(UserExperiment.truth_set),
                    joinedload(UserExperiment.benchmark_tool),
                    joinedload(UserExperiment.variant),
                    joinedload(UserExperiment.quality_control),
                    joinedload(UserExperiment.chemistry)
                ).filter(
                    UserExperiment.id == exp_id,
                    UserExperiment.user_id == user_id
                ).first()
                
                if not experiment:
                    logger.warning(f"User experiment {exp_id} not found or access denied for user {user_id}")
                    return None
                
                return {
                    'id': experiment_id_str,
                    'source': 'user',
                    'name': experiment.name,
                    'description': experiment.description or '',
                    'technology': experiment.sequencing_technology.technology if experiment.sequencing_technology else None,
                    'platform_name': experiment.sequencing_technology.platform_name if experiment.sequencing_technology else None,
                    'caller_name': experiment.variant_caller.name if experiment.variant_caller else None,
                    'caller_version': experiment.variant_caller.version if experiment.variant_caller else None,
                    'truth_set_name': experiment.truth_set.name if experiment.truth_set else None,
                    'truth_set_sample': experiment.truth_set.sample if experiment.truth_set else None,
                    'mean_coverage': experiment.quality_control.mean_coverage if experiment.quality_control else None,
                    'created_at': experiment.created_at.isoformat() if experiment.created_at else None
                }
            
            return None
            
    except Exception as e:
        logger.error(f"Error getting experiment details for {experiment_id_str} (user_id={user_id}): {e}")
        return None