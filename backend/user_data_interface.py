# ============================================================================
# user_data_interface.py
# ============================================================================
"""
User-specific data management for SNV Benchmarking Dashboard.
Handles user experiment uploads, queries, and data processing.
"""

import os
import pandas as pd
import logging
from datetime import datetime
from typing import Dict, Any, List, Optional
from sqlalchemy.exc import IntegrityError
from sqlalchemy.orm import joinedload

from database import get_db_session
from user_models import (
    User, UserExperiment, UserBenchmarkResult, UserOverallResult,
    UserSequencingTechnology, UserVariantCaller, UserAligner, 
    UserTruthSet, UserBenchmarkTool, UserVariant, UserQualityControl, UserChemistry
)

logger = logging.getLogger(__name__)

# ============================================================================
# USER EXPERIMENT QUERIES
# ============================================================================

def get_user_experiments(user_id: int) -> Dict[str, Any]:
    """
    Get all experiments for a specific user with metadata
    
    Args:
        user_id: User ID to query experiments for
    
    Returns:
        Dict with success status and DataFrame of user experiments
    """
    try:
        with get_db_session() as session:
            experiments = session.query(UserExperiment).options(
                joinedload(UserExperiment.sequencing_technology),
                joinedload(UserExperiment.variant_caller),
                joinedload(UserExperiment.truth_set),
                joinedload(UserExperiment.chemistry),
                joinedload(UserExperiment.quality_control)
            ).filter(UserExperiment.user_id == user_id).order_by(
                UserExperiment.created_at.desc()
            ).all()
            
            data = []
            for exp in experiments:
                data.append({
                    'id': exp.id,
                    'name': exp.name,
                    'description': exp.description or "",
                    'technology': exp.sequencing_technology.technology if exp.sequencing_technology else "N/A",
                    'platform_name': exp.sequencing_technology.platform_name if exp.sequencing_technology else "N/A",
                    'caller_name': exp.variant_caller.name if exp.variant_caller else "N/A",
                    'caller_version': exp.variant_caller.version if exp.variant_caller else "N/A",
                    'truth_set_name': exp.truth_set.name if exp.truth_set else "N/A",
                    'truth_set_sample': exp.truth_set.sample if exp.truth_set else "N/A",
                    'chemistry_name': exp.chemistry.name if exp.chemistry else "N/A",
                    'mean_coverage': exp.quality_control.mean_coverage if exp.quality_control else None,
                    'original_filename': exp.original_filename,
                    'file_size': exp.file_size,
                    'created_at': exp.created_at.strftime('%Y-%m-%d %H:%M') if exp.created_at else "N/A",
                    'updated_at': exp.updated_at.strftime('%Y-%m-%d %H:%M') if exp.updated_at else "N/A"
                })
            
            return {"success": True, "data": pd.DataFrame(data)}
            
    except Exception as e:
        logger.error(f"Error getting user experiments for user {user_id}: {e}")
        return {"success": False, "message": str(e), "data": pd.DataFrame()}

def get_user_experiment_details(user_id: int, experiment_id: int) -> Dict[str, Any]:
    """Get detailed information about a specific user experiment"""
    try:
        with get_db_session() as session:
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
                UserExperiment.id == experiment_id,
                UserExperiment.user_id == user_id
            ).first()
            
            if not experiment:
                return {"success": False, "message": "Experiment not found or access denied"}
            
            # Build detailed experiment data
            exp_data = {
                'id': experiment.id,
                'name': experiment.name,
                'description': experiment.description,
                'created_at': experiment.created_at.isoformat() if experiment.created_at else None,
                'updated_at': experiment.updated_at.isoformat() if experiment.updated_at else None,
                'original_filename': experiment.original_filename,
                'file_size': experiment.file_size,
                
                # Technology details
                'technology': experiment.sequencing_technology.technology if experiment.sequencing_technology else None,
                'platform_name': experiment.sequencing_technology.platform_name if experiment.sequencing_technology else None,
                'platform_type': experiment.sequencing_technology.platform_type if experiment.sequencing_technology else None,
                'platform_version': experiment.sequencing_technology.platform_version if experiment.sequencing_technology else None,
                
                # Caller details
                'caller_name': experiment.variant_caller.name if experiment.variant_caller else None,
                'caller_type': experiment.variant_caller.type if experiment.variant_caller else None,
                'caller_version': experiment.variant_caller.version if experiment.variant_caller else None,
                'caller_model': experiment.variant_caller.model if experiment.variant_caller else None,
                
                # Other metadata
                'aligner_name': experiment.aligner.name if experiment.aligner else None,
                'aligner_version': experiment.aligner.version if experiment.aligner else None,
                'truth_set_name': experiment.truth_set.name if experiment.truth_set else None,
                'truth_set_sample': experiment.truth_set.sample if experiment.truth_set else None,
                'truth_set_version': experiment.truth_set.version if experiment.truth_set else None,
                'benchmark_tool_name': experiment.benchmark_tool.name if experiment.benchmark_tool else None,
                'benchmark_tool_version': experiment.benchmark_tool.version if experiment.benchmark_tool else None,
                
                # Quality metrics
                'mean_coverage': experiment.quality_control.mean_coverage if experiment.quality_control else None,
                'read_length': experiment.quality_control.read_length if experiment.quality_control else None,
                'mean_read_length': experiment.quality_control.mean_read_length if experiment.quality_control else None,
                'mean_insert_size': experiment.quality_control.mean_insert_size if experiment.quality_control else None,
                
                # Chemistry
                'chemistry_name': experiment.chemistry.name if experiment.chemistry else None,
                'chemistry_version': experiment.chemistry.version if experiment.chemistry else None
            }
            
            return {"success": True, "experiment": exp_data}
            
    except Exception as e:
        logger.error(f"Error getting experiment details {experiment_id} for user {user_id}: {e}")
        return {"success": False, "message": str(e)}

# ============================================================================
# USER EXPERIMENT UPLOAD
# ============================================================================

def upload_user_experiment(user_id: int, file_path: str, experiment_name: str, 
                          metadata: Dict[str, Any] = None, description: str = "") -> Dict[str, Any]:
    """
    Upload and process user experiment data from hap.py CSV file
    
    Args:
        user_id: User ID uploading the experiment
        file_path: Path to the CSV file
        experiment_name: Name for the experiment
        metadata: Dictionary of experiment metadata
        description: Optional description
    
    Returns:
        Dict with success status and experiment details
    """
    try:
        # Validate user exists
        with get_db_session() as session:
            user = session.query(User).filter(User.id == user_id, User.is_active == True).first()
            if not user:
                return {"success": False, "message": "User not found or inactive"}
        
        # Validate file exists
        if not os.path.exists(file_path):
            return {"success": False, "message": "File not found"}
        
        # Read and validate CSV file
        try:
            df = pd.read_csv(file_path)
            file_size = os.path.getsize(file_path)
            original_filename = os.path.basename(file_path)
        except Exception as e:
            return {"success": False, "message": f"Failed to read CSV file: {str(e)}"}
        
        # Validate hap.py format
        required_columns = ['Type', 'METRIC.Recall', 'METRIC.Precision', 'METRIC.F1_Score']
        missing_columns = [col for col in required_columns if col not in df.columns]
        if missing_columns:
            return {"success": False, "message": f"Missing required columns: {missing_columns}"}
        
        # Process the upload
        with get_db_session() as session:
            # Create metadata records
            metadata = metadata or {}
            
            # Create sequencing technology
            seq_tech = None
            if metadata.get('technology'):
                seq_tech = UserSequencingTechnology(
                    technology=metadata.get('technology', ''),
                    platform_name=metadata.get('platform_name', ''),
                    platform_type=metadata.get('platform_type', ''),
                    platform_version=metadata.get('platform_version', '')
                )
                session.add(seq_tech)
                session.flush()
            
            # Create variant caller
            variant_caller = None
            if metadata.get('caller_name'):
                variant_caller = UserVariantCaller(
                    name=metadata.get('caller_name', ''),
                    type=metadata.get('caller_type', ''),
                    version=metadata.get('caller_version', ''),
                    model=metadata.get('caller_model', '')
                )
                session.add(variant_caller)
                session.flush()
            
            # Create aligner
            aligner = None
            if metadata.get('aligner_name'):
                aligner = UserAligner(
                    name=metadata.get('aligner_name', ''),
                    version=metadata.get('aligner_version', '')
                )
                session.add(aligner)
                session.flush()
            
            # Create truth set
            truth_set = None
            if metadata.get('truth_set_name'):
                truth_set = UserTruthSet(
                    name=metadata.get('truth_set_name', ''),
                    version=metadata.get('truth_set_version', ''),
                    reference=metadata.get('truth_set_reference', ''),
                    sample=metadata.get('truth_set_sample', '')
                )
                session.add(truth_set)
                session.flush()
            
            # Create benchmark tool
            benchmark_tool = None
            if metadata.get('benchmark_tool_name'):
                benchmark_tool = UserBenchmarkTool(
                    name=metadata.get('benchmark_tool_name', ''),
                    version=metadata.get('benchmark_tool_version', '')
                )
                session.add(benchmark_tool)
                session.flush()
            
            # Create quality control
            quality_control = None
            if any(metadata.get(k) for k in ['mean_coverage', 'read_length', 'mean_read_length']):
                quality_control = UserQualityControl(
                    mean_coverage=metadata.get('mean_coverage'),
                    read_length=metadata.get('read_length'),
                    mean_read_length=metadata.get('mean_read_length'),
                    mean_insert_size=metadata.get('mean_insert_size'),
                    read_quality=metadata.get('read_quality'),
                    max_aligned_read=metadata.get('max_aligned_read')
                )
                session.add(quality_control)
                session.flush()
            
            # Create chemistry
            chemistry = None
            if metadata.get('chemistry_name'):
                chemistry = UserChemistry(
                    name=metadata.get('chemistry_name', ''),
                    version=metadata.get('chemistry_version', ''),
                    sequencing_technology=metadata.get('technology', ''),
                    sequencing_platform=metadata.get('platform_name', '')
                )
                session.add(chemistry)
                session.flush()
            
            # Create experiment record
            experiment = UserExperiment(
                user_id=user_id,
                name=experiment_name.strip(),
                description=description.strip() if description else "",
                original_filename=original_filename,
                file_size=file_size,
                created_at=datetime.now(),
                sequencing_technology_id=seq_tech.id if seq_tech else None,
                variant_caller_id=variant_caller.id if variant_caller else None,
                aligner_id=aligner.id if aligner else None,
                truth_set_id=truth_set.id if truth_set else None,
                benchmark_tool_id=benchmark_tool.id if benchmark_tool else None,
                quality_control_metrics_id=quality_control.id if quality_control else None,
                chemistry_id=chemistry.id if chemistry else None
            )
            
            session.add(experiment)
            session.flush()  # Get experiment ID
            
            # Process CSV data into results tables
            processed_rows = 0
            overall_rows = 0
            detail_rows = 0
            
            for _, row in df.iterrows():
                try:
                    # Process overall results (Type = '*')
                    if row.get('Type') == '*':
                        overall_result = UserOverallResult(
                            experiment_id=experiment.id,
                            variant_type=row.get('Subtype', 'SNV'),
                            metric_recall=float(row.get('METRIC.Recall', 0)) if pd.notna(row.get('METRIC.Recall')) else None,
                            metric_precision=float(row.get('METRIC.Precision', 0)) if pd.notna(row.get('METRIC.Precision')) else None,
                            metric_f1_score=float(row.get('METRIC.F1_Score', 0)) if pd.notna(row.get('METRIC.F1_Score')) else None,
                            truth_total=int(row.get('TRUTH.TOTAL', 0)) if pd.notna(row.get('TRUTH.TOTAL')) else None,
                            truth_tp=int(row.get('TRUTH.TP', 0)) if pd.notna(row.get('TRUTH.TP')) else None,
                            truth_fn=int(row.get('TRUTH.FN', 0)) if pd.notna(row.get('TRUTH.FN')) else None,
                            query_total=int(row.get('QUERY.TOTAL', 0)) if pd.notna(row.get('QUERY.TOTAL')) else None,
                            query_tp=int(row.get('QUERY.TP', 0)) if pd.notna(row.get('QUERY.TP')) else None,
                            query_fp=int(row.get('QUERY.FP', 0)) if pd.notna(row.get('QUERY.FP')) else None
                        )
                        session.add(overall_result)
                        overall_rows += 1
                    
                    # Process detailed results for other regions
                    else:
                        benchmark_result = UserBenchmarkResult(
                            experiment_id=experiment.id,
                            variant_type=row.get('Subtype', 'SNV'),
                            subset=row.get('Subset', ''),
                            metric_recall=float(row.get('METRIC.Recall', 0)) if pd.notna(row.get('METRIC.Recall')) else None,
                            metric_precision=float(row.get('METRIC.Precision', 0)) if pd.notna(row.get('METRIC.Precision')) else None,
                            metric_f1_score=float(row.get('METRIC.F1_Score', 0)) if pd.notna(row.get('METRIC.F1_Score')) else None,
                            truth_total=int(row.get('TRUTH.TOTAL', 0)) if pd.notna(row.get('TRUTH.TOTAL')) else None,
                            truth_tp=int(row.get('TRUTH.TP', 0)) if pd.notna(row.get('TRUTH.TP')) else None,
                            truth_fn=int(row.get('TRUTH.FN', 0)) if pd.notna(row.get('TRUTH.FN')) else None,
                            query_total=int(row.get('QUERY.TOTAL', 0)) if pd.notna(row.get('QUERY.TOTAL')) else None,
                            query_tp=int(row.get('QUERY.TP', 0)) if pd.notna(row.get('QUERY.TP')) else None,
                            query_fp=int(row.get('QUERY.FP', 0)) if pd.notna(row.get('QUERY.FP')) else None
                        )
                        session.add(benchmark_result)
                        detail_rows += 1
                    
                    processed_rows += 1
                    
                except (ValueError, TypeError) as e:
                    logger.warning(f"Skipping invalid row in upload: {e}")
                    continue
            
            if processed_rows == 0:
                session.rollback()
                return {"success": False, "message": "No valid data rows found in file"}
            
            logger.info(f"User {user_id} uploaded experiment '{experiment_name}' with {processed_rows} rows ({overall_rows} overall, {detail_rows} detailed)")
            
            return {
                "success": True,
                "message": f"Successfully uploaded experiment with {processed_rows} data points",
                "experiment_id": experiment.id,
                "stats": {
                    "total_rows": processed_rows,
                    "overall_rows": overall_rows,
                    "detail_rows": detail_rows
                }
            }
            
    except Exception as e:
        logger.error(f"Error uploading user experiment: {e}")
        return {"success": False, "message": f"Upload failed: {str(e)}"}

# ============================================================================
# USER EXPERIMENT MANAGEMENT
# ============================================================================

def delete_user_experiment(user_id: int, experiment_id: int) -> Dict[str, Any]:
    """Delete user experiment with permission verification"""
    try:
        with get_db_session() as session:
            # Verify experiment belongs to user
            experiment = session.query(UserExperiment).filter(
                UserExperiment.id == experiment_id,
                UserExperiment.user_id == user_id
            ).first()
            
            if not experiment:
                return {"success": False, "message": "Experiment not found or access denied"}
            
            experiment_name = experiment.name
            
            # Delete experiment (cascade will handle related records)
            session.delete(experiment)
            
            logger.info(f"User {user_id} deleted experiment '{experiment_name}' (ID: {experiment_id})")
            return {"success": True, "message": f"Experiment '{experiment_name}' deleted successfully"}
            
    except Exception as e:
        logger.error(f"Error deleting user experiment {experiment_id}: {e}")
        return {"success": False, "message": f"Failed to delete experiment: {str(e)}"}

def update_user_experiment(user_id: int, experiment_id: int, updates: Dict[str, Any]) -> Dict[str, Any]:
    """Update user experiment metadata"""
    try:
        with get_db_session() as session:
            # Verify experiment belongs to user
            experiment = session.query(UserExperiment).filter(
                UserExperiment.id == experiment_id,
                UserExperiment.user_id == user_id
            ).first()
            
            if not experiment:
                return {"success": False, "message": "Experiment not found or access denied"}
            
            # Update allowed fields
            if 'name' in updates:
                experiment.name = updates['name'].strip()
            if 'description' in updates:
                experiment.description = updates['description'].strip()
            
            experiment.updated_at = datetime.now()
            
            logger.info(f"User {user_id} updated experiment {experiment_id}")
            return {"success": True, "message": "Experiment updated successfully"}
            
    except Exception as e:
        logger.error(f"Error updating user experiment {experiment_id}: {e}")
        return {"success": False, "message": f"Failed to update experiment: {str(e)}"}

# ============================================================================
# USER RESULTS QUERIES
# ============================================================================

def get_user_overall_results(user_id: int, experiment_ids: List[int] = None) -> Dict[str, Any]:
    """Get overall results for user experiments"""
    try:
        with get_db_session() as session:
            # Build query
            query = session.query(UserOverallResult).join(UserExperiment).filter(
                UserExperiment.user_id == user_id
            )
            
            if experiment_ids:
                query = query.filter(UserExperiment.id.in_(experiment_ids))
            
            results = query.all()
            
            data = []
            for result in results:
                data.append({
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
            
            return {"success": True, "data": pd.DataFrame(data)}
            
    except Exception as e:
        logger.error(f"Error getting user overall results for user {user_id}: {e}")
        return {"success": False, "message": str(e), "data": pd.DataFrame()}

def get_user_benchmark_results(user_id: int, experiment_ids: List[int] = None, 
                              region_filter: str = None) -> Dict[str, Any]:
    """Get detailed benchmark results for user experiments"""
    try:
        with get_db_session() as session:
            # Build query
            query = session.query(UserBenchmarkResult).join(UserExperiment).filter(
                UserExperiment.user_id == user_id
            )
            
            if experiment_ids:
                query = query.filter(UserExperiment.id.in_(experiment_ids))
            
            if region_filter:
                query = query.filter(UserBenchmarkResult.subset == region_filter)
            
            results = query.all()
            
            data = []
            for result in results:
                data.append({
                    'experiment_id': result.experiment_id,
                    'variant_type': result.variant_type,
                    'subset': result.subset,
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
            
            return {"success": True, "data": pd.DataFrame(data)}
            
    except Exception as e:
        logger.error(f"Error getting user benchmark results for user {user_id}: {e}")
        return {"success": False, "message": str(e), "data": pd.DataFrame()}

# ============================================================================
# ADMIN FUNCTIONS FOR USER DATA
# ============================================================================

def admin_get_all_user_experiments(admin_user_id: int) -> Dict[str, Any]:
    """Get all user experiments (admin only)"""
    try:
        from auth import is_admin
        if not is_admin(admin_user_id):
            return {"success": False, "message": "Admin privileges required"}
        
        with get_db_session() as session:
            experiments = session.query(UserExperiment).options(
                joinedload(UserExperiment.user)
            ).order_by(UserExperiment.created_at.desc()).all()
            
            data = []
            for exp in experiments:
                data.append({
                    'id': exp.id,
                    'user_id': exp.user_id,
                    'username': exp.user.username,
                    'name': exp.name,
                    'description': exp.description,
                    'file_size': exp.file_size,
                    'created_at': exp.created_at.strftime('%Y-%m-%d %H:%M') if exp.created_at else "N/A"
                })
            
            return {"success": True, "data": pd.DataFrame(data)}
            
    except Exception as e:
        logger.error(f"Error getting all user experiments: {e}")
        return {"success": False, "message": str(e), "data": pd.DataFrame()}